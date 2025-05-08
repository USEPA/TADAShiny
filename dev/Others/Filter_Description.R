### This script loads the data frame with filter description

library(tidyverse)
library(readxl)

filter_dat <- read_excel("inst/filter_descriptions_draft_20250505.xlsx")
filter_dat <- filter_dat %>%
  dplyr::select(Fields = fields, Description = description)

save.image("inst/extdata/filter_descriptions.RData")