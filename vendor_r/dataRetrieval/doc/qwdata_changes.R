## ----setup, include=FALSE, message=FALSE------------------
library(knitr)
library(dataRetrieval)
library(dplyr)

options(continue = " ")

knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  fig.height = 7,
  fig.width = 7
)

## ----eval=FALSE-------------------------------------------
#  
#  site_ids <- c("04024430", "04024000")
#  parameterCd <- c("34247", "30234", "32104", "34220")
#  nwisData <- readNWISqw(site_ids, parameterCd)

## ----eval=FALSE-------------------------------------------
#  wqpData <- readWQPqw(paste0("USGS-", site_ids), parameterCd)

## ----echo=FALSE-------------------------------------------
nwisData <- readRDS("nwisData.rds")
wqpData <- readRDS("wqpData.rds")

## ---------------------------------------------------------
nrow(nwisData)

## ---------------------------------------------------------
nrow(wqpData)

## ---------------------------------------------------------
ncol(nwisData)

## ---------------------------------------------------------
ncol(wqpData)

## ---------------------------------------------------------
names(attributes(nwisData))

## ---------------------------------------------------------
names(attributes(wqpData))

## ---------------------------------------------------------
site_NWIS <- attr(nwisData, "siteInfo")
site_WQP <- attr(wqpData, "siteInfo")

param_NWIS <- attr(nwisData, "variableInfo")
param_WQP <- attr(wqpData, "variableInfo")

## ---------------------------------------------------------
library(dplyr)

nwisData_USED <- nwisData %>%
  select(
    site_no, startDateTime, parm_cd,
    hyd_cond_cd, remark_cd, result_va
  ) %>%
  arrange(startDateTime, parm_cd)

knitr::kable(head(nwisData_USED))

## ---------------------------------------------------------
wqpData_USED <- wqpData %>%
  select(
    site_no = MonitoringLocationIdentifier,
    startDateTime = ActivityStartDateTime,
    parm_cd = USGSPCode,
    hyd_cond_cd = HydrologicCondition,
    remark_cd = ResultDetectionConditionText,
    result_va = ResultMeasureValue
  ) %>%
  arrange(startDateTime, parm_cd)
knitr::kable(head(wqpData_USED))

## ---------------------------------------------------------

censored_text <- c(
  "Not Detected",
  "Non-Detect",
  "Non Detect",
  "Detected Not Quantified",
  "Below Quantification Limit"
)

wqpData_USED <- wqpData %>%
  mutate(left_censored = grepl(paste(censored_text, collapse = "|"),
    ResultDetectionConditionText,
    ignore.case = TRUE
  )) %>%
  select(
    site_no = MonitoringLocationIdentifier,
    startDateTime = ActivityStartDateTime,
    parm_cd = USGSPCode,
    left_censored,
    result_va = ResultMeasureValue,
    detection_level = DetectionQuantitationLimitMeasure.MeasureValue,
    dl_units = DetectionQuantitationLimitMeasure.MeasureUnitCode
  ) %>%
  arrange(startDateTime, parm_cd)

knitr::kable(head(wqpData_USED))

## ---------------------------------------------------------
wqpData_USED_codes <- wqpData %>%
  mutate(units = ifelse(is.na(ResultMeasure.MeasureUnitCode),
    DetectionQuantitationLimitMeasure.MeasureUnitCode,
    ResultMeasure.MeasureUnitCode
  )) %>%
  select(
    parm_cd = USGSPCode,
    CharacteristicName, ResultSampleFractionText,
    units
  ) %>%
  distinct()

knitr::kable(wqpData_USED_codes)

## ---------------------------------------------------------
wqpData_USED_codes <- wqpData %>%
  select(
    HydrologicCondition, HydrologicEvent,
    ActivityTypeCode, ActivityMediaName
  ) %>%
  distinct()

knitr::kable(head(wqpData_USED_codes))

## ----codes, echo=FALSE------------------------------------
df <- data.frame(
  NWIS = c(
    "samp_type_cd = 9",
    "hyd_cond_cd = 9",
    "hyd_cond_cd = 5",
    "hyd_cond_cd = 8",
    "medium_cd = 'WS'",
    "hyd_event_cd = 'B'",
    "hyd_event_cd = 'A'",
    "hyd_event_cd = 9"
  ),
  WQP = c(
    "ActivityTypeCode = 'Sample-Routine'",
    "HydrologicCondition = 'Stable, normal stage'",
    "HydrologicCondition = 'Falling stage'",
    "HydrologicCondition = 'Rising stage'",
    "ActivityMediaName = 'Water'",
    "HydrologicEvent = 'Under ice cover'",
    "HydrologicEvent = 'Spring breakup'",
    "HydrologicEvent = 'Routine sample'"
  )
)
knitr::kable(df)

## ----whatdata, eval=FALSE---------------------------------
#  whatNWIS <- whatNWISdata(
#    siteNumber = site_ids,
#    service = "qw"
#  )

## ----whatdatanew, eval=FALSE------------------------------
#  whatWQP <- whatWQPdata(siteNumber = paste0("USGS-", site_ids))

## ----eval=FALSE-------------------------------------------
#  qwData <- readNWISdata(
#    state_cd = "WI",
#    startDate = "2000-01-01",
#    drain_area_va_min = 50, qw_count_nu = 50,
#    qw_attributes = "expanded",
#    qw_sample_wide = "wide",
#    list_of_search_criteria = c(
#      "state_cd",
#      "drain_area_va",
#      "obs_count_nu"
#    ),
#    service = "qw"
#  )

