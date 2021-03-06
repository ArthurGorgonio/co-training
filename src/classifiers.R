#' @description Select the best model to compose the ensemble of classifiers.
#'
#' @param ensemble The ensemble.
#' @param newClassifier A new trained classifier to compose the ensemble.
#'
#' @return A new `ensemble` aggregating the best model at ensemble. 
#'
addingEnsemble <- function(ensemble, newClassifier) {
  ensemble[[length(ensemble) + 1]] <- newClassifier
  return(ensemble)
}

#' @description Funtion to set a new k value for each dataset, the value is
#'  the sqrt(number of samples).
#'
#' @param dataset The current databese.
#'
#' @return A new classifier IBk using the right k.
#'
attKValue <- function(dataset) {
  param <- list(control = Weka_control(K = floor(sqrt(nrow(dataset))), X = T))
  return(c(learner("IBk", param)))
}

#' @description Drop all trained models of the ensemble.
#'
#' @param ensemble The ensemble.
#'
#' @return Removing all classifiers of the ensemble.
#'
dropEnsemble <- function(ensemble) {
  return(list())
}

#' @description Predicted values of all instances of the data.
#'
#' @param model The current model.
#' @param data The current dataset.
#' @param funcType The type of the function for each classifier.
#'
#' @return A dataframe with 3 columns the class, confidence value of the class
#'  and the id of the sample in data.
#'
generatePredict <- function(model, data, funcType) {
  pred <- predict(model, data, type = funcType)
  return(create_predict(pred, data))
}

create_predict <- function(prediction, data) {
  if (!is.matrix(prediction)) {
    prediction <- t(as.matrix(prediction))
  }
    col1 <- colnames(prediction)[apply(prediction, 1, which.max)]
    col2 <- apply(prediction, 1, max)
    return(data.frame(cl = col1, pred = col2, id = row.names(data)))
}

#' @description Measure the accuracy per classifier in ensemple on labeled
#'  data.
#'
#' @param ensemble The ensemble of classifiers which be used.
#' @param dataOracle The current labeled batch using oracle classifier.
#'
#' @return A vector with the accuracy per classifier.
#'
measureEnsemble <- function(ensemble, dataOracle) {
  accPerClassifier <- c()
  for (classi in ensemble) {
    cm <- confusionMatrix(classi, dataOracle)
    acc <- getAcc(cm)
    accPerClassifier <- c(accPerClassifier, acc)
  }
  return(accPerClassifier)
}

#' @description Generate the precition on the dataset using a model.
#'
#' @param model A trained classifier will be tested.
#' @param testDB The data set which the classifier will be evaluated.
#'
#' @return A dataset set predicted using the model.
#'
predictClass <- function(model, testDB) {
  colunsNames <- colnames(testDB)
  dbClassOff <- match("class", colunsNames)
  testData <- testDB[, -dbClassOff]
  prediction <- predict(model, testData, "class")
  return(prediction)
}

predictEnsemble <- function(ensemble, oracleDB, all_levels) {
  classPred <- generateMemory(oracleDB, length(all_levels), all_levels)
  for (cl in ensemble) {
    pred <- predictClass(cl, oracleDB)
    pos <- match(pred, colnames(classPred))
    for (sample in 1:length(pos)) {
      classPred[sample, pos[sample]] <- classPred[sample, pos[sample]] + 1
    }
  }
  allClassify <- c()
  for (sample in 1:length(pos)) {
    allClassify <- c(allClassify, which.max(classPred[sample,]))
  }
  ensemblePred <- factor(names(allClassify), levels(oracleDB$class))
  return(ensemblePred)
}

#' @description Select the worst model of the ensemble and remove it.
#'
#' @param ensemble The ensemble.
#' @param dataOracle The current labeled batch of the data stream.
#'
#' @return A new `ensemble` with the worst model removed of the ensemble.
#'
removingEnsemble <- function(ensemble, dataOracle) {
  classifiers <- measureEnsemble(ensemble, dataOracle)
  return(ensemble[-which.min(classifiers)])
}

#' @description Select the worst model of the ensemble and swap with best oracle.
#'
#' @param ensemble The ensemble.
#' @param dataOracle The current labeled batch of the data stream.
#' @param trainedModels A list of trained models that will be used to choose the
#'  best one.
#' @param accuracy A vector with the accuracy per model in `trainedModels`.
#'
#' @return A new `ensemble` with the worst model removed and adding the best
#'  oracle in the ensemble.
#'
swapEnsemble <- function(ensemble, dataOracle, trainedModels, accuracy) {
  ensemble <- removingEnsemble(ensemble, dataOracle)
  ensemble <- addingEnsemble(ensemble, trainedModels, accuracy)
  return(ensemble)
}
