#' @description This function check the actual directory has a subdir called src
#'  if exists it's a new working directory
setWorkspace <- function() {
  files <- c("classifiers.R", "crossValidation.R", "database.R", "flexconc.R",
             "functions.R", "statistics.R", "utils.R", "write.R")
  if ("src" %in% list.dirs(full.names = F)) {
    setwd("src")
  } else if (all(files %in% list.files())) {
    print("All files exists!")
  } else {
    stop("The follow file(s) are missing!\n", files[!files %in% list.files()])
  }
}

options(java.parameters = "-Xmx4g")

setWorkspace()
scripts <- list.files(pattern = "*.R")
for (scri in scripts) {
  source(scri)
}

extention <- ".csv"
label <- "class"
form <- as.formula("class ~ .")
databases <- c("Abalone.arff", "Arrhythmia.arff", "Car.arff", "Ecoli.arff",
               "Glass.arff", "HillValley.arff", "KrVsKp.arff",
               "Leukemia.arff", "Madelon.arff", "MultipleFeaturesKarhunen.arff",
               "Secom.arff", "Seeds.arff", "Semeion.arff", "SolarFlare.arff",
               "SpectfHeart.arff", "TicTacToeEndgame.arff", "Twonorm.arff",
               "Waveform.arff", "Wine.arff", "Yeast.arff", "Haberman.arff",
               "PlanningRelax.arff", "Btsc.arff", "MammographicMass.arff",
               "Pima.arff", "Sonar.arff", "SolarFlare1.arff", "Ilpd.arff",
               "Automobile.arff", "GermanCredit.arff", "Flags.arff",
               "Wilt.arff", "Vehicle.arff", "Dermatology.arff", "PhishingWebsite.arff",
               "ImageSegmentation.arff", "Mushroom.arff", "OzoneLevelDetection.arff", "Nursery.arff",
               "Adult.arff", "PenDigits.arff", "Musk.arff", "Cnae.arff")
ratio <- 0.1

## Versions Standard and DWC Standard
bd <- 1

for (dataset in databases) {
  dataName <- strsplit(dataset, ".", T)[[1]][1]
  originalDB <- read.arff(paste("../datasets", dataset, sep = "/"))
  dataL <- holdout(originalDB$class, .9)
  dataTrain <- originalDB[dataL$tr, ]
  dataTest <- originalDB[dataL$ts, ]
  folds <- stratifiedKFold(dataTrain, dataTrain$class)
  
  class_id <- which(colnames(dataTest) == "class")
  
  # SSLRDecisionTree
  method <- "SSLRDecisionTree"
  acc_test <- c()
  fscore_test <- c()
  precision_test <- c()
  recall_test <- c()
  
  ite <- 1
  begin <- Sys.time()
  for (fold in folds) {
    cat("DataSet[", bd, "]:\t", dataName, "\tIt:\t", ite, "\t\tMétodo:", method, "\n")
    
    training_instances <- dataTrain[-fold, ]
    testing_instances <- dataTrain[fold, ]
    
    trainIds <- holdout(training_instances$class, ratio, mode = "random", seed = 19)
    labelIds <- trainIds$tr
    data <- newBase(training_instances, labelIds)
    
    model <- SSLRDecisionTree() %>% SSLR::fit(form, data)
    pred_unlabeled <- predict(model, dataTest, type="raw")
    metrics <- statistics(pred_unlabeled, dataTest$class, "SSLRDecisionTree")
    acc_test <- c(acc_test, metrics$acc)
    fscore_test <- c(fscore_test, metrics$fscore)
    precision_test <- c(precision_test, metrics$precision)
    recall_test <- c(recall_test, metrics$recall)
    
    ite <- ite + 1
  }
  end <- Sys.time()
  
  writeArchive("SSLRDecisionTree.txt", "../", dataName, method, acc_test,
               fscore_test, precision_test, recall_test, begin, end)
  cat("Arquivos do método ", method, " foram salvos.\n\n")
  
  bd <- bd + 1
}

