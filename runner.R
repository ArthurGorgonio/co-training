args <- commandArgs(TRUE)
# args <- c("-s", "1", "-l", "100")

source('src/utils.R')

method <- "Co-Training Dwcs - Labelling"
databases <- c("Abalone.arff", "Arrhythmia.arff", "Car.arff", "Ecoli.arff",
               "Glass.arff", "HillValley.arff", "KrVsKp.arff", "Leukemia.arff",
               "Madelon.arff", "MultipleFeaturesKarhunen.arff", "Seeds.arff",
               "Semeion.arff", "SolarFlare.arff", "SpectfHeart.arff",
               "TicTacToeEndgame.arff", "Twonorm.arff", "Waveform.arff",
               "Wine.arff", "Yeast.arff", "Haberman.arff", "PlanningRelax.arff",
               "Btsc.arff", "MammographicMass.arff", "Pima.arff", "Sonar.arff",
               "SolarFlare1.arff", "Ilpd.arff", "Automobile.arff",
               "GermanCredit.arff", "Flags.arff", "Wilt.arff", "Vehicle.arff",
               "Dermatology.arff", "PhishingWebsite.arff",
               "ImageSegmentation.arff", "Mushroom.arff",
               "OzoneLevelDetection.arff", "Nursery.arff", "Adult.arff",
               "PenDigits.arff", "Musk.arff", "Cnae.arff")
params <- atribArgs(args, databases)

ratios <- params$ratios
selection_rates <- params$selection_rate
ebal_concordances <- params$ebal_concordance

need_run <- TRUE

for (ebal_concordance in ebal_concordances) {
  need_run <- !need_run
  for (selection_rate in selection_rates) {
    for (ratio in ratios) {
      if (need_run) {
        source("mainStandards.R")
        source("mainDwsc_select_label.R")
        source("mainDwsc_select.R")
        source('mainDwsc_label.R')
      }
      source("mainEbalV1.R")
      source("mainEbalV2.R")
      source("mainEbalV3.R")
      source("mainEbalV2DwSa_label.R")
      source("mainEbalV2DwSa_select_label.R")
      source("mainEbalV2DwSa_select.R")
    }
  }
}