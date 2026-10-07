test_that("sync_removals matches row count of raw", {
  raw_df <- data.frame(a = 1:5)
  removals_df <- data.frame(flag1 = c(TRUE, FALSE, TRUE))
  
  out <- sync_removals(raw_df, removals_df)
  
  expect_equal(nrow(out), nrow(raw_df))
  expect_equal(names(out), "flag1")
})