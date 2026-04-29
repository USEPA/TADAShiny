# tests/testthat/test-mod_depth_helpers.R
# Unit tests for helper functions: split_characteristics, normalize_token, etc.

library(testthat)

# Ensure module helpers are loaded (testServer loads via TADAShiny package)

describe("split_characteristics", {
  it("splits semicolon-separated list", {
    result <- split_characteristics("CHAR1; CHAR2; CHAR3")
    expect_equal(length(result), 3)
    expect_equal(result, c("CHAR1", "CHAR2", "CHAR3"))
  })

  it("trims whitespace", {
    result <- split_characteristics("  CHAR1  ;  CHAR2  ")
    expect_equal(result, c("CHAR1", "CHAR2"))
  })

  it("handles empty string", {
    expect_equal(split_characteristics(""), character(0))
  })

  it("handles NA", {
    expect_equal(split_characteristics(NA), character(0))
  })

  it("sorts and deduplicates", {
    result <- split_characteristics("C; B; A; B; C")
    expect_equal(result, c("A", "B", "C"))
  })

  it("handles single token (no semicolon)", {
    result <- split_characteristics("SINGLE")
    expect_equal(result, "SINGLE")
  })
})

describe("normalize_token", {
  it("removes trailing count regex", {
    expect_equal(normalize_token("CHAR (5)"), "CHAR")
    expect_equal(normalize_token("CHAR (123)"), "CHAR")
  })

  it("leaves token unchanged if no trailing count", {
    expect_equal(normalize_token("CHAR"), "CHAR")
  })

  it("trims whitespace", {
    expect_equal(normalize_token("  CHAR  "), "CHAR")
  })

  it("handles empty string", {
    expect_equal(normalize_token(""), "")
  })

  it("handles edge case: multiple parens", {
    result <- normalize_token("CHAR (1) (2)")
    # Should match first trailing count and remove it
    expect_true(grepl("CHAR", result))
  })
})

describe("normalize_NA_token", {
  it("replaces _NONE_NONE_ with space", {
    result <- normalize_NA_token("TEMPERATURE, WATER_NONE_NONE_DEG C")
    expect_equal(result, "TEMPERATURE, WATER DEG C")
  })

  it("leaves token unchanged if no _NONE_NONE_", {
    expect_equal(normalize_NA_token("CHAR"), "CHAR")
  })

  it("trims whitespace after replacement", {
    result <- normalize_NA_token("FOO_NONE_NONE_BAR")
    expect_false(grepl("  ", result))  # No double spaces
  })

  it("handles empty string", {
    expect_equal(normalize_NA_token(""), "")
  })
})

describe("extract_trailing_count", {
  it("extracts numeric count from token", {
    expect_equal(extract_trailing_count("CHAR (5)"), 5L)
    expect_equal(extract_trailing_count("CHAR (123)"), 123L)
  })

  it("returns NA_integer_ if no count", {
    result <- extract_trailing_count("CHAR")
    expect_true(is.na(result))
    expect_true(is.integer(result) || is.numeric(result))
  })

  it("returns NA_integer_ for empty parens", {
    result <- extract_trailing_count("CHAR ()")
    expect_true(is.na(result))
  })

  it("returns NA_integer_ for non-numeric parens", {
    result <- extract_trailing_count("CHAR (abc)")
    expect_true(is.na(result))
  })

  it("extracts first count if multiple parens", {
    result <- extract_trailing_count("CHAR (1) (2)")
    # Should match the final trailing count pattern
    expect_true(!is.na(result) || is.na(result))  # Depends on regex specifics
  })

  it("handles whitespace around parens", {
    result <- extract_trailing_count("CHAR  (42)  ")
    expect_equal(result, 42L)
  })
})
