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
databases <- c("Btsc.arff", "PhishingWebsite.arff", "ImageSegmentation.arff",
               "PenDigits.arff", "Musk.arff", "Cnae.arff",
               "Mushroom.arff", "OzoneLevelDetection.arff", "Nursery.arff",
               "Adult.arff"
               )
ratio <- 0.1

## Versions Standard and DWC Standard
bd <- 1
for (dataset in databases) {
  dataName <- strsplit(dataset, ".", T)[[1]][1]
  # originalDB <- read.arff(paste("../all", dataset, sep = "/"))
  originalDB <- read.arff(paste("../datasets", dataset, sep = "/"))
  class_id <- which(colnames(originalDB) == "class")
  if (length(class_id) == 0) {
    class_id <- length(colnames(originalDB))
    colnames(originalDB)[class_id] <- "class"
  }
  dataL <- holdout(originalDB$class, .9, mode = "random")
  dataTrain <- originalDB[dataL$tr, ]
  dataTest <- originalDB[dataL$ts, ]
  folds <- stratifiedKFold(dataTrain, dataTrain$class)
  
  
  
  # GRFClassifierSSLR
  method <- "GRFClassifierSSLR"
  acc_test <- c()
  fscore_test <- c()
  precision_test <- c()
  recall_test <- c()

  ite <- 1

  begin <- Sys.time()
  for (fold in folds) {
    cat("DataSet[", bd, "]:", dataName, "\tIt:", ite, "\t\tMétodo:", method, "\n")

    fold_ids <- match(fold, as.numeric(rownames(dataTrain)))

    training_instances <- dataTrain[-fold_ids, ]
    testing_instances <- dataTrain[fold_ids, ]

    trainIds <- holdout(training_instances$class, ratio, mode = "random", seed = 19)
    labelIds <- trainIds$tr
    data <- newBase(training_instances, labelIds)

    model <- GRFClassifierSSLR(class_mass_normalization = F)
    tryCatch(
      expr = {
        model2 <- SSLR::fit.model_sslr(model, form, data)
        model_trained <- T
      },
      error = function(e) {
        model_trained <- F
      }
    )
    
    if (model_trained) {
      model_trained <- F
      pred_unlabeled <- SSLR::predictions(model2, type = "raw")
      
      data[-labelIds,class_id] <- model2$model$classes[pred_unlabeled]
      
      data <- bind_rows(data, dataTest[,-class_id])
      
      model <- GRFClassifierSSLR(class_mass_normalization = F) %>% SSLR::fit(form, data)
      pred_unlabeled <- SSLR::predictions(model, type = "raw")
      
      metrics <- statistics(pred_unlabeled, dataTest$class, "GRFClassifierSSLR")
      acc_test <- c(acc_test, metrics$acc)
      fscore_test <- c(fscore_test, metrics$fscore)
      precision_test <- c(precision_test, metrics$precision)
      recall_test <- c(recall_test, metrics$recall)
  
      ite <- ite + 1
    }
  }
  end <- Sys.time()
  while (length(acc_test) < 10) {
    acc_test <- c(acc_test, mean(acc_test))
    fscore_test <- c(fscore_test, mean(fscore_test))
    precision_test <- c(precision_test, mean(precision_test))
    recall_test <- c(recall_test, mean(recall_test))
  }
  writeArchive("GRFClassifierSSLR.txt", "../", dataName, method, acc_test,
               fscore_test, precision_test, recall_test, begin, end)
  cat("Arquivos do método ", method, " foram salvos.\n\n")
  bd <- bd + 1
}

