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
rm(scripts, scri)
extention <- ".csv"
label <- "class"
form <- as.formula("class ~ .")
meansFlexConC1S <- c()
method <- "Co-Training Standard"
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
set.seed(19)
# for (dataset in databases) {
  # dataName <- strsplit(dataset, ".", T)[[1]][1]
  # originalDB <- read.arff(paste("../datasets", dataset, sep = "/"))
  dataName <- "Iris"
  originalDB <- iris
  colnames(originalDB)[5] <- "class"
  originalDB$class <- droplevels(originalDB$class)
  dataL <- holdout(originalDB$class, .9)
  dataTrain <- originalDB[dataL$tr, ]
  dataTest <- originalDB[dataL$ts, ]
  folds <- stratifiedKFold(dataTrain, dataTrain$class)
  
  visao <- criar_visao(dataTrain)
  dat1 <- visao[[1]]
  dat2 <- visao[[2]]
  
  visao_test <- criar_visao(dataTest)
  data_test1 <- visao_test[[1]]
  data_test2 <- visao_test[[2]]
  
  acc_co <- c()
  fscore_co <- c()
  preci_co <- c()
  recall_co <- c()
  
  ite <- 1
  begin <- Sys.time()
  for (fold in folds) {
    
    fold_ids <- match(fold, as.numeric(rownames(dataTrain)))
    
    train1 <- dat1[-fold_ids, ]
    test1 <- dat1[fold_ids, ]
    
    train2 <- dat2[-fold_ids, ]
    test2 <- dat2[fold_ids, ]
    
    trainIds <- holdout(train1$class, ratio, mode = "random")
    labelIds <- trainIds$tr
    data1 <- newBase(train1, labelIds)
    data2 <- newBase(train2, labelIds)
    
    co_training <- coTrainingRandom(data1, data2)
    
    cm1 <- confusionMatrix(co_training[[1]], data_test1)
    cm2 <- confusionMatrix(co_training[[2]], data_test2)
    
    # Acurácia
    acc_model_mean <- mean(getAcc(cm1), getAcc(cm2))
    acc_co <- c(acc_co, acc_model_mean)
    
    # Fscore
    fscore_model_mean <- mean(fmeasure(cm1), fmeasure(cm2))
    fscore_co <- c(fscore_co, fscore_model_mean)
    
    # Precision
    preci_model_mean <- mean(precision(cm1), precision(cm2))
    preci_co <- c(preci_co, preci_model_mean)
    
    # recall
    recall_model_mean <- mean(recall(cm1), recall(cm2))
    recall_co <- c(recall_co, recall_model_mean)
    
    cat("DataSet[", bd, "]:\t", dataName, "\tIt:\t", ite, "\n")
    ite <- ite + 1
    
  }
  end <- Sys.time()
  writeArchive("coTrainingMediaRandom.txt", "../", dataName, method, acc_co,
               fscore_co, preci_co, recall_co, begin, end)
  cat("Arquivos do método ", method, " foram salvos.\n\n")
  bd <- bd + 1
# }

