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
method <- "Co-Training EbAL V3"
databases <- c("Haberman.arff")
ratio <- 0.1
myModel <- baseClassifiers
myFuncs <- funcType



## Versions Standard and DWC Standard
bd <- 0
for (dataset in databases) {
  bd <- bd + 1
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
  
  ite <- 0
  begin <- Sys.time()
  for (fold in folds) {
    ite <- ite + 1
    msg <- paste("DataSet[", bd, "]:\t", dataName, "\tIt:\t", ite, "\n", sep="")
    cat(msg)
    
    
    # ?pbPost("note", "Experiment Status - Detailed", msg)
    
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
    
    tryCatch(co_training <- coTrainingEbalV3(myModel, myFuncs, data1, data2),
              error = function(e) {
                typeError <- paste("Error", e, sep = ": ")
                msg <- paste(typeError, "\n\nDataSet[", bd, "]:\t", dataName,
                             "\tIt:\t", ite, "\n", sep="")
                pbPost("note", "Error in the Experiment!", msg)
              }
    )
    
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
  }
  end <- Sys.time()
  
  msg <- paste("DataSet[", bd, "]: ", dataName, "\nAcc: ", round(mean(acc_co)*100, 4),
               sep = "")
  pbPost("note", "Experiment Status - General Result", msg)
  
  writeArchive("coTrainingMediaEbALV3.txt", "../", dataName, method, acc_co,
               fscore_co, preci_co, recall_co, begin, end)
  writeArchive("coTrainingVisao1EbALV3.txt", "../", dataName, method,
               acc_co_v1, fscore_co_v1, preci_co_v1, recall_co_v1, begin, end)
  writeArchive("coTrainingVisao2EbALV3.txt", "../", dataName, method,
               acc_co_v2, fscore_co_v2, preci_co_v2, recall_co_v2, begin, end)
  cat("Arquivos do método ", method, " foram salvos.\n\n")
}

pbPost("file", url = "../coTrainingMediaEbALV3.txt")
