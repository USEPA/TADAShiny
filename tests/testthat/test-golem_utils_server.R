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
  expect_equal(drop_nulls(list(x = NULL, y = 2)), list(y = 2))
})

test_that("%||% works", {
  expect_equal(NULL %||% 1, 1)
  expect_equal(2 %||% 1, 2)
})

test_that("%|NA|% works", {
  expect_equal(NA %|NA|% 1, 1)
  expect_equal(2 %|NA|% 1, 2)
})

test_that("rv and rvtl work", {
  expect_true(inherits(rv, "function"))
  expect_true(inherits(rvtl, "function"))
})

test_that("options() usage is safe and consistent", {
  # Resolve the package root
  root <- tryCatch(rprojroot::find_package_root_file(), error = function(e) {
    normalizePath(
      file.path(testthat::test_path(), "..", ".."),
      mustWork = FALSE
    )
  })

  # Directories to scan
  dirs <- c(
    file.path(root, "R"),
    file.path(root, "inst"),
    file.path(root, "inst", "app"),
    file.path(root, "inst", "dev"),
    file.path(root, "dev")
  )
  dirs <- dirs[dir.exists(dirs)]

  # Collect R files (flatten to character, clean up, then filter existing)
  r_files <- unlist(
    lapply(dirs, function(d) {
      list.files(d, pattern = "\\.[Rr]$", full.names = TRUE, recursive = TRUE)
    }),
    use.names = FALSE
  )

  # Ensure character and clean
  r_files <- as.character(r_files)
  r_files <- r_files[!is.na(r_files) & nzchar(r_files)]
  r_files <- unique(r_files)
  r_files <- r_files[file.exists(r_files)]

  # If nothing to scan, skip rather than fail
  skip_if(length(r_files) == 0, "No R files found to scan for options() usage.")

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

    walk <- function(e) {
      if (is.call(e)) {
        fn <- as.character(e[[1]])
        if (identical(fn, "options")) {
          sr <- attr(e, "srcref")
          line <- if (!is.null(sr)) sr[1] else NA_integer_
          calls[[length(calls) + 1]] <<- list(call = e, line = line)
        }
        for (i in seq_along(e)) {
          walk(e[[i]])
        }
      } else if (is.expression(e) || is.list(e)) {
        for (i in seq_along(e)) {
          walk(e[[i]])
        }
      }
    }
    walk(exprs)
    calls
  }

  issues <- list()

  for (f in r_files) {
    lines <- read_file_lines(f)
    calls <- find_options_calls(f)
    has_withr_local_options <- any(grepl(
      "withr::local_options",
      lines,
      fixed = TRUE
    ))

    # Rule A: unnamed arguments to options()
    for (c in calls) {
      call <- c$call
      line <- c$line
      args <- as.list(call)[-1]
      arg_names <- names(as.list(call))[-1]

      unnamed_idx <- which(is.na(arg_names) | arg_names == "")
      if (length(unnamed_idx) > 0) {
        bad <- TRUE
        for (i in unnamed_idx) {
          ai <- args[[i]]
          if (is.call(ai) && identical(as.character(ai[[1]]), "list")) {
            bad <- FALSE
          }
        }
        if (bad) {
          snippet <- if (!is.na(line) && line <= length(lines)) {
            lines[line]
          } else {
            deparse(call)
          }
          issues[[length(issues) + 1]] <- sprintf(
            "%s:%s: options() called with unnamed argument (e.g., options(old_warn)): %s",
            f,
            ifelse(is.na(line), "?", line),
            snippet
          )
        }
      }
    }

    # Rule B: discourage options("warn") for getting warn
    warn_get_lines <- grep("options\\(\\s*\"warn\"\\s*\\)", lines)
    for (ln in warn_get_lines) {
      issues[[length(issues) + 1]] <- sprintf(
        "%s:%s: Found options(\"warn\"). Prefer getOption(\"warn\"). Line: %s",
        f,
        ln,
        trimws(lines[ln])
      )
    }

    # Rule C: options(warn = ...) without restoration or withr::local_options
    warn_set_lines <- grep("options\\s*\\(\\s*warn\\s*=", lines)
    for (ln in warn_set_lines) {
      window_lo <- max(1, ln - 10)
      window_hi <- min(length(lines), ln + 10)
      window <- lines[window_lo:window_hi]

      nearby_withr <- any(grepl("withr::local_options", window, fixed = TRUE))
      nearby_onexit_restore <- any(grepl(
        "on\\.exit\\s*\\(\\s*options\\s*\\(\\s*warn\\s*=",
        window
      ))

      if (!nearby_withr && !nearby_onexit_restore && !has_withr_local_options) {
        issues[[length(issues) + 1]] <- sprintf(
          "%s:%s: options(warn = ...) without restoration or withr::local_options. Add withr::local_options(list(warn = ...)) or on.exit(options(warn = old_warn), add = TRUE). Line: %s",
          f,
          ln,
          trimws(lines[ln])
        )
      }
    }

    # Rule D: interactive() guard + options(warn=...) warning
    interactive_guard_lines <- grep(
      "if\\s*\\(\\s*interactive\\s*\\(\\s*\\)\\s*\\)",
      lines
    )
    if (length(interactive_guard_lines) > 0 && length(warn_set_lines) > 0) {
      issues[[length(issues) + 1]] <- sprintf(
        "%s: Found options(warn=...) in a file that uses interactive() guards. In Shiny, interactive() is often FALSE, leading to inconsistent warn behavior.",
        f
      )
    }
  }

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

# Pre-load shiny and muffle warning related to environment/version mismatch
local({
  withCallingHandlers(library(shiny), warning = function(w) {
    msg <- conditionMessage(w)
    if (grepl("package 'shiny' was built under R version", msg)) {
      invokeRestart("muffleWarning")
    }
  })
})
