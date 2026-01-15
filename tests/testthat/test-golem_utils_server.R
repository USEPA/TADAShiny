test_that("not_in works", {
  expect_true(1 %not_in% 2:10)
  expect_false(1 %not_in% 1:10)
})

test_that("not_null works", {
  expect_true(not_null(1))
  expect_false(not_null(NULL))
})

test_that("not_na works", {
  expect_true(not_na(1))
  expect_false(not_na(NA))
})

test_that("drop_nulls works", {
  expect_equal(
    drop_nulls(
      list(x = NULL, y = 2)
    ),
    list(y = 2)
  )
})

test_that("%||% works", {
  expect_equal(
    NULL %||% 1,
    1
  )
  expect_equal(
    2 %||% 1,
    2
  )
})

test_that("%|NA|% works", {
  expect_equal(
    NA %|NA|% 1,
    1
  )
  expect_equal(
    2 %|NA|% 1,
    2
  )
})

test_that("rv and rvtl work", {
  expect_true(
    inherits(rv, "function")
  )
  expect_true(
    inherits(rvtl, "function")
  )
})

test_that("options() usage is safe and consistent", {
  # Collect R source files in a typical golem app structure
  r_files <- c(
    list.files("R", pattern = "\\.R$", full.names = TRUE, recursive = TRUE),
    list.files("inst", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  )
  r_files <- unique(r_files[file.exists(r_files)])

  expect_true(length(r_files) > 0, info = "No R files found to scan for options() usage.")

  # Helper: read file lines
  read_file_lines <- function(f) {
    tryCatch(readLines(f, warn = FALSE), error = function(e) character(0))
  }

  # Helper: find options() calls via parse, capturing srcref and code
  find_options_calls <- function(file) {
    calls <- list()
    exprs <- tryCatch(parse(file, keep.source = TRUE), error = function(e) NULL)
    if (is.null(exprs)) {
      return(calls)
    }

    # Recursive walk through expressions looking for calls to options
    walk <- function(e) {
      if (is.call(e)) {
        fn <- as.character(e[[1]])
        if (identical(fn, "options")) {
          sr <- attr(e, "srcref")
          line <- if (!is.null(sr)) sr[1] else NA_integer_
          calls[[length(calls) + 1]] <<- list(call = e, line = line)
        }
        # walk arguments
        for (i in seq_along(e)) {
          walk(e[[i]])
        }
      } else if (is.expression(e) || is.list(e)) {
        for (i in seq_along(e)) walk(e[[i]])
      }
    }
    walk(exprs)
    calls
  }

  # Collect issues
  issues <- list()

  # Scan each file
  for (f in r_files) {
    lines <- read_file_lines(f)
    calls <- find_options_calls(f)

    # Heuristic scan for nearby restoration or withr use
    has_withr_local_options <- any(grepl("withr::local_options", lines, fixed = TRUE))

    # Rule A: options() with unnamed argument (e.g., options(old_warn)) -> BAD
    for (c in calls) {
      call <- c$call
      line <- c$line
      args <- as.list(call)[-1]
      arg_names <- names(as.list(call))[-1]

      # If any argument is unnamed and not a list() call, flag it
      unnamed_idx <- which(is.na(arg_names) | arg_names == "")
      if (length(unnamed_idx) > 0) {
        bad <- TRUE
        # Allow options(list(...)) pattern
        for (i in unnamed_idx) {
          ai <- args[[i]]
          if (is.call(ai) && identical(as.character(ai[[1]]), "list")) {
            bad <- FALSE
          }
        }
        if (bad) {
          snippet <- if (!is.na(line) && line <= length(lines)) lines[line] else deparse(call)
          issues[[length(issues) + 1]] <- sprintf(
            "%s:%s: options() called with unnamed argument (e.g., options(old_warn)): %s",
            f, ifelse(is.na(line), "?", line), snippet
          )
        }
      }
    }

    # Rule B: discourage options(\"warn\") used to retrieve warn (prefer getOption(\"warn\"))
    # (This is a soft rule but often signals later misuse.)
    warn_get_lines <- grep("options\\(\\s*\"warn\"\\s*\\)", lines)
    for (ln in warn_get_lines) {
      issues[[length(issues) + 1]] <- sprintf(
        "%s:%s: Found options(\"warn\"). Prefer getOption(\"warn\") to retrieve the warn value. Line: %s",
        f, ln, trimws(lines[ln])
      )
    }

    # Rule C: options(warn = 2) with no nearby restoration (on.exit/options(warn=old_warn)) or withr::local_options
    # Heuristic: look +/- 10 lines for withr::local_options or on.exit(options(warn = ...))
    warn_set_lines <- grep("options\\s*\\(\\s*warn\\s*=", lines)
    for (ln in warn_set_lines) {
      window_lo <- max(1, ln - 10)
      window_hi <- min(length(lines), ln + 10)
      window <- lines[window_lo:window_hi]

      nearby_withr <- any(grepl("withr::local_options", window, fixed = TRUE))
      nearby_onexit_restore <- any(grepl("on\\.exit\\s*\\(\\s*options\\s*\\(\\s*warn\\s*=", window))

      if (!nearby_withr && !nearby_onexit_restore && !has_withr_local_options) {
        issues[[length(issues) + 1]] <- sprintf(
          "%s:%s: options(warn = ...) without nearby restoration or withr::local_options. Add withr::local_options(list(warn = ...)) or on.exit(options(warn = old_warn), add = TRUE). Line: %s",
          f, ln, trimws(lines[ln])
        )
      }
    }

    # Bonus Rule D: interactive() guard followed by options(warn=...) can lead to inconsistent behavior in Shiny
    # (Shiny often runs non-interactively; warn might not be set/restored)
    interactive_guard_lines <- grep("if\\s*\\(\\s*interactive\\s*\\(\\s*\\)\\s*\\)", lines)
    if (length(interactive_guard_lines) > 0 && length(warn_set_lines) > 0) {
      issues[[length(issues) + 1]] <- sprintf(
        "%s: Found options(warn=...) in a file that uses interactive() guards. In Shiny, interactive() is often FALSE, leading to inconsistent warn behavior.",
        f
      )
    }
  }

  # Fail with a readable summary if any issues were found
  if (length(issues) > 0) {
    msg <- paste0(
      "Unsafe or inconsistent options() usage detected:\n",
      paste0(" - ", issues, collapse = "\n"),
      "\n\nRemediation suggestions:\n",
      " - Use withr::local_options(list(warn = 2)) to set warn temporarily and restore automatically.\n",
      " - If you need manual control, use:\n",
      "     old_warn <- getOption(\"warn\"); options(warn = 2); on.exit(options(warn = old_warn), add = TRUE)\n",
      " - Do not call options(old_warn): options() requires named arguments.\n",
      " - Prefer getOption(\"warn\") over options(\"warn\") when reading the warn value.\n",
      " - Avoid interactive() guards for warn logic in Shiny; ensure consistent behavior in non-interactive sessions.\n"
    )
    fail(msg)
  } else {
    succeed("All options() usage appears safe and consistent.")
  }
})
