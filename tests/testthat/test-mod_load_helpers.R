testthat::test_that(".tadas_offline responds to environment variable", {
  withr::with_envvar(c(TADAS_OFFLINE = "true"), {
    testthat::expect_true(.tadas_offline())
  })
  withr::with_envvar(c(TADAS_OFFLINE = ""), {
    testthat::expect_false(.tadas_offline())
  })
})

testthat::test_that(".safe_fetch_county handles malformed input by returning fallback with expected columns", {
  # Simulate .safe_req_string returning something malformed that causes data.table::fread to fail
  # We can stub .safe_req_string to return some broken content and ensure fallback path works.
  mockery::stub(.safe_fetch_county, ".safe_req_string", function(u) "not,a,county\n1,2")
  df <- .safe_fetch_county("ignored")
  testthat::expect_true(is.data.frame(df))
  # In the code, the fallback when fread fails returns a data.frame with STUSAB etc.
  testthat::expect_true(all(c("STUSAB", "STATE", "COUNTY", "COUNTY_NAME", "COUNTY_ID") %in% names(df)))
})

testthat::test_that(".safe_fetch_csv_column returns values when given CSV text (via temp file + file://)", {
  # Create a temp CSV and serve it through a file:// URL recognized by data.table::fread
  tmpf <- tempfile(fileext = ".csv")
  write.csv(data.frame(ID = c("a","b","a"), Other = 1:3), tmpf, row.names = FALSE)
  # Use file path directly — the helper uses .safe_req_string which normally performs an HTTP request.
  # We'll stub .safe_req_string to return the CSV file contents as a string.
  csv_text <- paste(readLines(tmpf), collapse = "\n")
  mockery::stub(.safe_fetch_csv_column, ".safe_req_string", function(u) csv_text)
  res <- .safe_fetch_csv_column("ignored", "ID", default = character())
  testthat::expect_setequal(res, c("a","b"))
})

testthat::test_that(".safe_fetch_projects reads CSV text into unique ProjectIdentifier vector", {
  csv_text <- "ProjectIdentifier,Other\nP1,1\nP2,2\nP1,3\n"
  mockery::stub(.safe_fetch_projects, ".safe_req_string", function(u) csv_text)
  res <- .safe_fetch_projects("ignored")
  testthat::expect_setequal(res, c("P1","P2"))
})

testthat::test_that("return_tribal_sf filters a provided tribal_list by layer and name", {
  # Build a small fake tribal_list (list of tibbles or data.frames)
  fake_layer <- data.frame(TRIBE_NAME = c("Foo","Bar"), x = 1:2, stringsAsFactors = FALSE)
  fake_tribal_list <- list(layer1 = fake_layer)
  res <- return_tribal_sf("layer1", "Foo", tribal_list = fake_tribal_list)
  testthat::expect_true(is.data.frame(res))
  testthat::expect_equal(nrow(res), 1)
  testthat::expect_equal(res$TRIBE_NAME, "Foo")
})

# testthat::test_that(".safe_fetch_* functions return defaults when offline", {
#   # Force offline so no network calls are attempted
#   withr::with_envvar(c(TADAS_OFFLINE = "true"), {
#     testthat::expect_equal(.safe_fetch_csv_column("https://example.invalid/test.csv", "ID", default = c("a")), c("a"))
#     testthat::expect_equal(.safe_fetch_projects("https://example.invalid/projects.csv"), character())
#     testthat::expect_equal(.safe_fetch_sitetypes("https://example.invalid/sitetype.json"), character())
#     cnty <- .safe_fetch_county("https://example.invalid/national_county.txt")
#     # Should be a data.frame with the expected columns when offline
#     testthat::expect_true(is.data.frame(cnty))
#     # Expect zero rows
#     testthat::expect_equal(nrow(cnty), 0)
#     # columns present and character type
#     testthat::expect_true(all(c("STATE_CD", "STATE_FIPS", "COUNTY_FIPS", "COUNTY_NAME", "COUNTY_FOOBAR") %in% names(cnty)))
#   })
# })

# testthat::test_that(".safe_fetch_sitetypes reads JSON-like input", {
#   # Provide JSON text where jsonlite::fromJSON would parse it to a data.frame/list with codes$value
#   json_text <- jsonlite::toJSON(list(codes = list(value = c("A","B","A"))), auto_unbox = TRUE)
#   # stub jsonlite::fromJSON inside .safe_fetch_sitetypes to return an R list
#   mockery::stub(.safe_fetch_sitetypes, "jsonlite::fromJSON", function(u) list(codes = data.frame(value = c("A","B","A"))))
#   res <- .safe_fetch_sitetypes("ignored")
#   testthat::expect_setequal(res, c("A","B"))
# })
