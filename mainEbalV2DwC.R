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
Naive <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes", c("NaiveBayes", "bayes"))
DT <- make_Weka_classifier("weka/classifiers/rules/DecisionTable", c("DecisionTable", "rules"))
rm(scripts, scri)
baseClassifiers <- learner("J48", list(control = Weka_control(C = 0.05)))
extention <- ".csv"
label <- "class"
form <- as.formula("class ~ .")
funcType <- "probability"
meansFlexConC1S <- c()
method <- "Co-Training EbAL V2 DwC"
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
ratio <- 0.1
myModel <- baseClassifiers
myFuncs <- funcType


## Versions Standard and DWC Standard
bd <- 1
for (dataset in databases) {
  set.seed(19)
  dataName <- strsplit(dataset, ".", T)[[1]][1]
  originalDB <- read.arff(paste("../datasets", dataset, sep = "/"))
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
  acc_co_v1 <- c()
  acc_co_v2 <- c()
  
  fscore_co <- c()
  fscore_co_v1 <- c()
  fscore_co_v2 <- c()
  
  preci_co <- c()
  preci_co_v1 <- c()
  preci_co_v2 <- c()
  
  recall_co <- c()
  recall_co_v1 <- c()
  recall_co_v2 <- c()
  
  ite <- 1
  begin <- Sys.time()
  for (fold in folds) {
    
    train1 <- dat1[-fold, ]
    test1 <- dat1[fold, ]
    
    train2 <- dat2[-fold, ]
    test2 <- dat2[fold, ]
    
    trainIds <- holdout(train1$class, ratio, mode = "random")
    labelIds <- trainIds$tr
    data1 <- newBase(train1, labelIds)
    data2 <- newBase(train2, labelIds)
    classDist <- ddply(train1[, ], ~class, summarise,
                       samplesClass = length(class))
    
    
    co_training <- coTrainingEbalV2Dwc(myModel, myFuncs, data1, data2)
    
    
    cm1 <- confusionMatrix(co_training[[1]], data_test1)
    cm2 <- confusionMatrix(co_training[[2]], data_test2)
    # Acurácia
    acc_model1 <- getAcc(cm1)
    acc_model2 <- getAcc(cm2)
    acc_model_mean <- mean(c(acc_model1, acc_model2))
    acc_co <- c(acc_co, acc_model_mean)
    acc_co_v1 <- c(acc_co_v1, acc_model1)
    acc_co_v2 <- c(acc_co_v2, acc_model2)
    
    # Fscore
    fscore_model1 <- fmeasure(cm1)
    fscore_model2 <- fmeasure(cm2)
    fscore_model_mean <- mean(c(fscore_model1, fscore_model2))
    fscore_co <- c(fscore_co, fscore_model_mean)
    fscore_co_v1 <- c(fscore_co_v1, fscore_model1)
    fscore_co_v2 <- c(fscore_co_v2, fscore_model2)
    
    # Precision
    preci_model1 <- precision(cm1)
    preci_model2 <- precision(cm2)
    preci_model_mean <- mean(c(preci_model1, preci_model2))
    preci_co <- c(preci_co, preci_model_mean)
    preci_co_v1 <- c(preci_co_v1, preci_model1)
    preci_co_v2 <- c(preci_co_v2, preci_model2)
    
    # recall
    recall_model1 <- recall(cm1)
    recall_model2 <- recall(cm2)
    recall_model_mean <- mean(c(recall_model1, recall_model2))
    recall_co <- c(recall_co, recall_model_mean)
    recall_co_v1 <- c(recall_co_v1, recall_model1)
    recall_co_v2 <- c(recall_co_v2, recall_model2)
    
    cat("DataSet[", bd, "]:\t", dataName, "\tIt:\t", ite, "\n")
    ite <- ite + 1
    
  }
  end <- Sys.time()
  writeArchive("coTrainingMediaEbALV2Dwc.txt", "../", dataName, method, acc_co,
               fscore_co, preci_co, recall_co, begin, end)
  writeArchive("coTrainingVisao1EbALV2Dwc.txt", "../", dataName, method,
               acc_co_v1, fscore_co_v1, preci_co_v1, recall_co_v1, begin, end)
  writeArchive("coTrainingVisao2EbALV2Dwc.txt", "../", dataName, method,
               acc_co_v2, fscore_co_v2, preci_co_v2, recall_co_v2, begin, end)
  cat("Arquivos do método ", method, " foram salvos.\n\n")
  bd <- bd + 1
}

