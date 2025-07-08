testthat::test_that("flagging can run", {
  testdat <- EPATADA::TADA_RandomTestingData(choose_random_state = TRUE)

  # run EPATADA functions in utils_flag_functions.R
  testthat::expect_no_error(testdat %>%
    EPATADA::TADA_IDCensoredData() %>%
    EPATADA::TADA_FlagSpeciation(clean = "none") %>%
    EPATADA::TADA_FlagFraction(clean = FALSE) %>%
    EPATADA::TADA_FlagResultUnit(clean = "none") %>%
    EPATADA::TADA_FindQCActivities(clean = FALSE, flaggedonly = FALSE) %>%
    EPATADA::TADA_FlagMeasureQualifierCode(clean = FALSE, define = TRUE) %>%
    EPATADA::TADA_FlagMethod(clean = FALSE) %>%
    EPATADA::TADA_FindPotentialDuplicatesSingleOrg() %>%
    # EPATADA::TADA_FindPotentialDuplicatesMultipleOrgs(org_hierarchy = orgs) %>%
    EPATADA::TADA_FlagContinuousData(clean = FALSE, flaggedonly = FALSE) %>%
    EPATADA::TADA_FlagAboveThreshold(clean = FALSE) %>%
    EPATADA::TADA_FlagBelowThreshold(clean = FALSE) %>%
    EPATADA::TADA_FlagCoordinates(
      clean_outsideUSA = "no",
      clean_imprecise = FALSE,
      flaggedonly = FALSE
    ))

  # run EPATADA functions in utils_flag_functions.R
  testthat::expect_no_warning(testdat %>%
    EPATADA::TADA_IDCensoredData() %>%
    EPATADA::TADA_FlagSpeciation(clean = "none") %>%
    EPATADA::TADA_FlagFraction(clean = FALSE) %>%
    EPATADA::TADA_FlagResultUnit(clean = "none") %>%
    EPATADA::TADA_FindQCActivities(clean = FALSE, flaggedonly = FALSE) %>%
    EPATADA::TADA_FlagMeasureQualifierCode(clean = FALSE, define = TRUE) %>%
    EPATADA::TADA_FlagMethod(clean = FALSE) %>%
    EPATADA::TADA_FindPotentialDuplicatesSingleOrg() %>%
    # EPATADA::TADA_FindPotentialDuplicatesMultipleOrgs(org_hierarchy = orgs) %>%
    EPATADA::TADA_FlagContinuousData(clean = FALSE, flaggedonly = FALSE) %>%
    EPATADA::TADA_FlagAboveThreshold(clean = FALSE) %>%
    EPATADA::TADA_FlagBelowThreshold(clean = FALSE) %>%
    EPATADA::TADA_FlagCoordinates(
      clean_outsideUSA = "no",
      clean_imprecise = FALSE,
      flaggedonly = FALSE
    ))
})
