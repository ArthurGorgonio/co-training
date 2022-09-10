# args <- commandArgs(TRUE)
# args <- c("-s", "1", "-l", "100")

source('src/utils.R')

ebal_concordance <- 0.75
selection_rate <- 0.1
for (ratio in c(0.05, 0.2)) {
  source("mainStandards.R")
  source("mainDwsc_select_label.R")
  source("mainDwsc_select.R")
  source('mainDwsc_label.R')
  source("mainEbalV1.R")
  source("mainEbalV2.R")
  source("mainEbalV3.R")
  source("mainEbalV2DwSa_label.R")
  source("mainEbalV2DwSa_select_label.R")
  source("mainEbalV2DwSa_select.R")
}

ebal_concordance <- 0.75
ratio <- 0.1
rm(selection_rate)
for (selection_rate in c(0.05, 0.15, 0.2)) {
  source("mainStandards.R")
  source("mainDwsc_select_label.R")
  source("mainDwsc_select.R")
  source('mainDwsc_label.R')
  source("mainEbalV1.R")
  source("mainEbalV2.R")
  source("mainEbalV3.R")
  source("mainEbalV2DwSa_label.R")
  source("mainEbalV2DwSa_select_label.R")
  source("mainEbalV2DwSa_select.R")
}


selection_rate <- 0.1
ratio <- 0.1
ebal_concordance <- 0.6
source("mainEbalV1.R")
source("mainEbalV2.R")
source("mainEbalV3.R")
source("mainEbalV2DwSa_label.R")
source("mainEbalV2DwSa_select_label.R")
source("mainEbalV2DwSa_select.R")