base_ensemble <- function() {
  return(list(c(learner("SMO", list(control = Weka_control(C = 1, K = list("PolyKernel", E = 1)))),
    learner("SMO", list(control = Weka_control(C = .8, K = list("PolyKernel", E = 1)))),
    learner("SMO", list(control = Weka_control(C = 1, K = list("NormalizedPolyKernel", E = 2)))),
    learner("SMO", list(control = Weka_control(C = 1, K = list("RBFKernel")))),
    learner("SMO", list(control = Weka_control(C = 1, K = list("Puk", O = 1, S = 1)))),
    learner("DT", list(control = Weka_control(X = 1, S = list("BestFirst", D = 1, N = 3)))),
    learner("DT", list(control = Weka_control(X = 1, S = list("BestFirst", D = 1, N = 5)))),
    learner("DT", list(control = Weka_control(X = 1, S = list("BestFirst", D = 1, N = 7)))),
    learner("J48", list(control = Weka_control(C = .05, M = 2))),
    learner("J48", list(control = Weka_control(C = .10, M = 2))),
    learner("J48", list(control = Weka_control(C = .20, M = 2))),
    learner("J48", list(control = Weka_control(C = .25, M = 2))),
    learner("IBk", list(control = Weka_control(K = 1, A = list("LinearNNSearch", A = "EuclideanDistance")))),
    learner("IBk", list(control = Weka_control(K = 3, A = list("LinearNNSearch", A = "EuclideanDistance")))),
    learner("IBk", list(control = Weka_control(K = 5, A = list("LinearNNSearch", A = "EuclideanDistance")))),
    learner("IBk", list(control = Weka_control(K = 3, A = list("LinearNNSearch", A = "ManhattanDistance")))),
    learner("IBk", list(control = Weka_control(K = 5, A = list("LinearNNSearch", A = "ManhattanDistance")))),
    learner("Naive", list(control = Weka_control())),
    learner("Naive", list(control = Weka_control(K = TRUE))),
    learner("Naive", list(control = Weka_control(D = TRUE)))
  ), rep("class", 20)))
}

#' @description Calculate the acc value of the training samples.
#'
#' @param c Classifier to be used.
#' @param iniLabDB The data set with samples select before FlexConC call.
#' @param trainSet The data set with the  select in a moment.
#'
#' @return Accuracy of the model trained with a sample set `trainSet`.
#'
calcLocalAcc <- function(c, iniLabDB, trainSet) {
  std <- J48(form, trainSet)
  confMat <- confusionMatrix(std, iniLabDB)
  return(getAcc(confMat))
}

calculate_centroid <- function(labeled) {
  classes <- levels(droplevels(labeled$class))
  features <- ncol(labeled) - 1
  centroid <- matrix(rep(0, features), nrow = length(classes), ncol = features)
  rownames(centroid) <- classes
  for(cl in classes) {
    instances <- which(labeled$class == cl)
    for (feature in 1:features) {
      centroid[cl, feature] <- mean(labeled[instances, feature])
    }
  }
  return (centroid)
}

euclidian_distance <- function(a, b) {
  return (sqrt(sum((a - b)^2)))
}

#' @description Generate the confusion matrix of the model.
#'
#' @param model A trained classifier will be tested.
#' @param testDB The data set which the classifier will be evaluated.
#'
#' @return The confusion matrix.
#'
confusionMatrix <- function(model, testDB) {
  confusion <- table(predictClass(model, testDB), testDB$class)
  return(confusion)
}

#' @description Convert each sample in probPreds in character
#'
#' @param probPreds A data frame which contains class | confidence ratio | id.
#'
#' @return Converted probPreds.
#'
convertProbPreds <- function(probPreds) {
  aux <- sapply(probPreds, is.factor)
  probPreds[aux] <- lapply(probPreds[aux], as.character)
  return(probPreds)
}


# Function co-Training original (w/ fix threshold)
#@param metodo - 1 = co-training original (k=10%)
#                2 = co-training baseado no metodo de Felipe (k=limiar)
#                3 = co-training gradativo (k=limiar que diminui 5% a cada iteracao)
coTrainingOriginal <- function(learner, predFunc, data1, data2, k_fixo = T) {
  maxIts <- 100
  N <- NROW(data1)
  it <- 0
  sup1 <- which(!is.na(data1[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  sup2 <- which(!is.na(data2[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  base_add <- round(nrow(data1[-sup1,]) * 0.1)
  while (((it < maxIts) && ((length(sup1) / N) < 1) && ((length(sup2) / N) < 1))) {
    new_samples1 <- c()
    new_samples2 <- c()
    acertou <- 0
    it <- it + 1
    
    # Create and train the base classifier
    model1 <- generateModel(learner, form, data1[sup1, ])
    model2 <- generateModel(learner, form, data2[sup2, ])
    
    # Classify instances in unlabelled data
    probPreds1 <- generateProbPreds(model1, data1[-sup1,], predFunc)
    probPreds2 <- generateProbPreds(model2, data2[-sup2,], predFunc)
    
    # Sort the instances based on confidence value
    probPreds1_ordenado <- order(probPreds1$p, decreasing = T)
    probPreds2_ordenado <- order(probPreds2$p, decreasing = T)
    
    qtd_add <- min(base_add, length(probPreds1_ordenado))
    
    if (qtd_add > 0) {
      new_samples1 <- probPreds2[probPreds2_ordenado[1:qtd_add], -2]
      new_samples2 <- probPreds1[probPreds1_ordenado[1:qtd_add], -2]
      pos1 <- match(new_samples2$id, rownames(data1))
      pos2 <- match(new_samples1$id, rownames(data2))
      data1[pos1, as.character(form[[2]])] <- new_samples2$cl
      data2[pos2, as.character(form[[2]])] <- new_samples1$cl
      sup1 <- c(sup1, pos1)
      sup2 <- c(sup2, pos2)
    } else {
      new_samples1 <- c()
      new_samples2 <- c()
    }
  }
  model1 <- generateModel(learner, form, data1[sup1, ])
  model2 <- generateModel(learner, form, data2[sup2, ])
  
  model <- list(model1, model2)
  return(model)
}

coTrainingDwc <- function(learner, predFunc, data1, data2, k_fixo = T) {
  maxIts <- 100
  N <- NROW(data1)
  it <- 0
  cls_1 <- match(label, colnames(data1))
  cls_2 <- match(label, colnames(data2))
  sup1 <- which(!is.na(data1[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  sup2 <- which(!is.na(data2[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  base_add <- round(nrow(data1[-sup1,]) * 0.1)
  while (((it < maxIts) && ((length(sup1) / N) < 1) && ((length(sup2) / N) < 1))) {
    new_samples1 <- c()
    new_samples2 <- c()
    acertou <- 0
    it <- it + 1
    # Create and train the base classifier
    model1 <- generateModel(learner, form, data1[sup1, ])
    model2 <- generateModel(learner, form, data2[sup2, ])
    
    # Classify instances in unlabelled data
    probPreds1 <- generateProbPreds(model1, data1[-sup1,], predFunc)
    probPreds2 <- generateProbPreds(model2, data2[-sup2,], predFunc)
    
    centroides_1 <- calculate_centroid(data1[sup1,])
    probPreds_distance_1 <- probPreds1
    centroides_2 <- calculate_centroid(data2[sup2,])
    probPreds_distance_2 <- probPreds2
    for (i in 1:nrow(probPreds_distance_1)) {
      cent_1 <- match(probPreds_distance_1$cl[i], rownames(centroides_1))
      cent_2 <- match(probPreds_distance_2$cl[i], rownames(centroides_2))
      
      dist_inst_1 <- euclidian_distance(data1[probPreds_distance_1$id[i],-cls_1],
                                        centroides_1[cent_1,])
      dist_inst_2 <- euclidian_distance(data2[probPreds_distance_2$id[i],-cls_2],
                                        centroides_2[cent_2,])
      probPreds_distance_1$pred[i] <- probPreds_distance_1$pred[i] / dist_inst_1
      probPreds_distance_2$pred[i] <- probPreds_distance_2$pred[i] / dist_inst_2
    }
    
    # Sort the instances based on confidence value
    probPreds1_ordenado <- order(probPreds_distance_1$pred, decreasing = T)
    probPreds2_ordenado <- order(probPreds_distance_2$pred, decreasing = T)
    
    
    qtd_add <- min(base_add, length(probPreds1_ordenado))
    
    if (qtd_add > 0) {
      new_samples1 <- probPreds_distance_2[probPreds2_ordenado[1:qtd_add], -2]
      new_samples2 <- probPreds_distance_1[probPreds1_ordenado[1:qtd_add], -2]
      pos1 <- match(new_samples2$id, rownames(data1))
      pos2 <- match(new_samples1$id, rownames(data2))
      data1[pos1, as.character(form[[2]])] <- new_samples2$cl
      data2[pos2, as.character(form[[2]])] <- new_samples1$cl
      sup1 <- c(sup1, pos1)
      sup2 <- c(sup2, pos2)
      
    } else {
      new_samples1 <- c()
      new_samples2 <- c()
    }
  }
  model1 <- generateModel(learner, form, data1[sup1, ])
  model2 <- generateModel(learner, form, data2[sup2, ])
  
  model <- list(model1, model2)
  return(model)
}


coTrainingEbalV1 <- function(learner, pred_func, data1, data2) {
  base <- base_ensemble()
  classifiers_ensemble <- base[[1]]
  pred_func_ensemble <- base[[2]]
  maxIts <- 100
  N <- NROW(data1)
  it <- 0
  concordance <- 15
  sup1 <- which(!is.na(data1[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  sup2 <- which(!is.na(data2[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  base_add <- round(nrow(data1[-sup1,]) * 0.1)
  is_sup_empty <- FALSE
  while ((it < maxIts) && ((length(sup1) < N) && (length(sup2) < N)) && !is_sup_empty) {
    new_samples1 <- c()
    new_samples2 <- c()
    acertou <- 0
    it <- it + 1
    # Create and train the base classifier
    model1 <- generateModel(learner, form, data1[sup1, ])
    model2 <- generateModel(learner, form, data2[sup2, ])
    
    # Train base ensemble
    ensemble1 <- train_ensemble(classifiers_ensemble, data1[sup1, ])
    ensemble2 <- train_ensemble(classifiers_ensemble, data2[sup2, ])
    
    ensemblePred1 <- predictEnsemble(ensemble1, data1[-sup1,], data1$class[sup1])
    ensemblePred2 <- predictEnsemble(ensemble2, data2[-sup2,], data2$class[sup2])
    
    # Select the instances to classify
    selected1 <- select_ensemble(ensemblePred1, concordance)
    selected2 <- select_ensemble(ensemblePred2, concordance)
    
    # Converting to id
    sele_id1 <- match(selected1, rownames(data1[-sup1,]))
    sele_id2 <- match(selected2, rownames(data2[-sup2,]))
    
    
    # Classify instances in unlabelled data
    probPreds1 <- generateProbPreds(model1, data1[sele_id1,], pred_func)
    probPreds2 <- generateProbPreds(model2, data2[sele_id2,], pred_func)
    
    # Sort the instances based on confidence value
    probPreds1_ordenado <- order(probPreds1$p, decreasing = T)
    probPreds2_ordenado <- order(probPreds2$p, decreasing = T)
    
    qtd_add <- min(length(probPreds1_ordenado), length(probPreds2_ordenado))
    
    if (qtd_add > 0) {
      new_samples1 <- probPreds2[probPreds2_ordenado[1:qtd_add], -2]
      new_samples2 <- probPreds1[probPreds1_ordenado[1:qtd_add], -2]
      pos1 <- match(new_samples2$id, rownames(data1))
      pos2 <- match(new_samples1$id, rownames(data2))
      data1[pos1, as.character(form[[2]])] <- new_samples2$cl
      data2[pos2, as.character(form[[2]])] <- new_samples1$cl
      sup1 <- c(sup1, pos1)
      sup2 <- c(sup2, pos2)
      
    } else {
      is_sup_empty <- TRUE
    }
  }
  model1 <- generateModel(learner, form, data1[sup1, ])
  model2 <- generateModel(learner, form, data2[sup2, ])
  
  model <- list(model1, model2)
  return(model)
}


## Ebal V1 DWC Version
coTrainingEbalV1Dwc <- function(learner, pred_func, data1, data2) {
  base <- base_ensemble()
  classifiers_ensemble <- base[[1]]
  pred_func_ensemble <- base[[2]]
  maxIts <- 100
  N <- NROW(data1)
  it <- 0
  concordance <- 15
  cls_1 <- match(label, colnames(data1))
  cls_2 <- match(label, colnames(data2))
  sup1 <- which(!is.na(data1[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  sup2 <- which(!is.na(data2[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  base_add <- round(nrow(data1[-sup1,]) * 0.1)
  is_sup_empty <- FALSE
  while ((it < maxIts) && ((length(sup1) < N) && (length(sup2) < N)) && !is_sup_empty) {
    new_samples1 <- c()
    new_samples2 <- c()
    acertou <- 0
    it <- it + 1
    
    # Create and train the base classifier
    model1 <- generateModel(learner, form, data1[sup1, ])
    model2 <- generateModel(learner, form, data2[sup2, ])
    
    # Train base ensemble
    ensemble1 <- train_ensemble(classifiers_ensemble, data1[sup1, ])
    ensemble2 <- train_ensemble(classifiers_ensemble, data2[sup2, ])
    
    ensemblePred1 <- predictEnsemble(ensemble1, data1[-sup1,], data1$class[sup1])
    ensemblePred2 <- predictEnsemble(ensemble2, data2[-sup2,], data2$class[sup2])
    
    # Calculate the Centroids
    centroides_1 <- calculate_centroid(data1[sup1,])
    probPreds_distance_1 <- create_predict(ensemblePred1, data1[-sup1,])
    centroides_2 <- calculate_centroid(data2[sup2,])
    probPreds_distance_2 <- create_predict(ensemblePred2, data2[-sup2,])
    for (i in 1:nrow(probPreds_distance_1)) {
      cent_1 <- match(probPreds_distance_1$cl[i], rownames(centroides_1))
      cent_2 <- match(probPreds_distance_2$cl[i], rownames(centroides_2))
      
      dist_inst_1 <- euclidian_distance(data1[probPreds_distance_1$id[i],-cls_1],
                                        centroides_1[cent_1,])
      dist_inst_2 <- euclidian_distance(data2[probPreds_distance_2$id[i],-cls_2],
                                        centroides_2[cent_2,])
      probPreds_distance_1$pred[i] <- probPreds_distance_1$pred[i] / dist_inst_1
      probPreds_distance_2$pred[i] <- probPreds_distance_2$pred[i] / dist_inst_2
    }
    
    # Sort the instances based on confidence value
    probPreds1_ordenado <- order(probPreds_distance_1$pred, decreasing = T)
    probPreds2_ordenado <- order(probPreds_distance_2$pred, decreasing = T)
    
    qtd_add <- min(base_add, length(probPreds1_ordenado))
    
    if (qtd_add > 0) {
      # Select the instances to classify
      selected1 <- probPreds_distance_1$id[probPreds1_ordenado[1:qtd_add]]
      selected2 <- probPreds_distance_2$id[probPreds2_ordenado[1:qtd_add]]
      
      # Classify instances in unlabelled data
      new_samples1 <- generateProbPreds(model1, data1[selected1,], pred_func)[,-2]
      new_samples2 <- generateProbPreds(model2, data2[selected2,], pred_func)[,-2]
      pos1 <- match(new_samples2$id, rownames(data1))
      pos2 <- match(new_samples1$id, rownames(data2))
      data1[pos1, as.character(form[[2]])] <- new_samples2$cl
      data2[pos2, as.character(form[[2]])] <- new_samples1$cl
      sup1 <- c(sup1, pos1)
      sup2 <- c(sup2, pos2)
      
    } else {
      is_sup_empty <- TRUE
    }
  }
  model1 <- generateModel(learner, form, data1[sup1, ])
  model2 <- generateModel(learner, form, data2[sup2, ])
  
  model <- list(model1, model2)
  return(model)
}


## Ebal V2 Standard

coTrainingEbalV2 <- function(learner, pred_func, data1, data2) {
  base <- base_ensemble()
  classifiers_ensemble <- base[[1]]
  pred_func_ensemble <- base[[2]]
  maxIts <- 100
  N <- NROW(data1)
  it <- 0
  concordance <- 15
  sup1 <- which(!is.na(data1[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  sup2 <- which(!is.na(data2[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  is_sup_empty <- FALSE
  while ((it < maxIts) && ((length(sup1) < N) && (length(sup2) < N)) && !is_sup_empty) {
    new_samples1 <- c()
    new_samples2 <- c()
    acertou <- 0
    it <- it + 1

    # Train base ensemble
    ensemble1 <- train_ensemble(classifiers_ensemble, data1[sup1, ])
    ensemble2 <- train_ensemble(classifiers_ensemble, data2[sup2, ])
    
    ensemblePred1 <- predictEnsemble(ensemble1, data1[-sup1,], data1$class[sup1])
    ensemblePred2 <- predictEnsemble(ensemble2, data2[-sup2,], data2$class[sup2])
    
    # Select the instances to classify
    selected1 <- select_ensemble(ensemblePred1, concordance)
    selected2 <- select_ensemble(ensemblePred2, concordance)
    
    if ((length(selected1) > 0) && (length(selected2) > 0)) {
    
      # Converting to id
      sele_id1 <- match(selected1, rownames(data1[-sup1,]))
      sele_id2 <- match(selected2, rownames(data2[-sup2,]))
     
      # Classify instances in unlabelled data
      probPreds1 <- correct_prediction(ensemblePred1, sele_id1)
      probPreds2 <- correct_prediction(ensemblePred2, sele_id2)
      
      # Sort the instances based on confidence value
      probPreds1_ordenado <- order(probPreds1$pred, decreasing = T)
      probPreds2_ordenado <- order(probPreds2$pred, decreasing = T)
      
      qtd_add <- min(length(probPreds1_ordenado), length(probPreds2_ordenado))
    
      new_samples1 <- probPreds2[probPreds2_ordenado[1:qtd_add], -2]
      new_samples2 <- probPreds1[probPreds1_ordenado[1:qtd_add], -2]
      pos1 <- match(new_samples2$id, rownames(data1))
      pos2 <- match(new_samples1$id, rownames(data2))
      data1[pos1, as.character(form[[2]])] <- new_samples2$cl
      data2[pos2, as.character(form[[2]])] <- new_samples1$cl
      sup1 <- c(sup1, pos1)
      sup2 <- c(sup2, pos2)
      
    } else {
      is_sup_empty <- TRUE
    }
  }
  model1 <- generateModel(learner, form, data1[sup1, ])
  model2 <- generateModel(learner, form, data2[sup2, ])

  model <- list(model1, model2)
  return(model)
}


correct_prediction <- function(ensemblePred, selected) {
  if (length(selected) > 1) {
    return(create_predict(ensemblePred[selected,], ensemblePred[selected,]))
  }
  return(create_predict(ensemblePred[selected,], ensemblePred))
}




## Ebal V2 DWC Version
coTrainingEbalV2Dwc <- function(learner, pred_func, data1, data2) {
  base <- base_ensemble()
  classifiers_ensemble <- base[[1]]
  pred_func_ensemble <- base[[2]]
  maxIts <- 100
  N <- NROW(data1)
  it <- 0
  concordance <- 15
  cls_1 <- match(label, colnames(data1))
  cls_2 <- match(label, colnames(data2))
  sup1 <- which(!is.na(data1[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  sup2 <- which(!is.na(data2[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  base_add <- round(nrow(data1[-sup1,]) * 0.1)
  is_sup_empty <- FALSE
  while ((it < maxIts) && ((length(sup1) < N) && (length(sup2) < N)) && !is_sup_empty) {
    new_samples1 <- c()
    new_samples2 <- c()
    acertou <- 0
    it <- it + 1
    
    # Train base ensemble
    ensemble1 <- train_ensemble(classifiers_ensemble, data1[sup1, ])
    ensemble2 <- train_ensemble(classifiers_ensemble, data2[sup2, ])
    
    ensemblePred1 <- predictEnsemble(ensemble1, data1[-sup1,], data1$class[sup1])
    ensemblePred2 <- predictEnsemble(ensemble2, data2[-sup2,], data2$class[sup2])
    
    # Calculate the Centroids
    centroides_1 <- calculate_centroid(data1[sup1,])
    probPreds_distance_1 <- create_predict(ensemblePred1, data1[-sup1,])
    centroides_2 <- calculate_centroid(data2[sup2,])
    probPreds_distance_2 <- create_predict(ensemblePred2, data2[-sup2,])
    for (i in 1:nrow(probPreds_distance_1)) {
      cent_1 <- match(probPreds_distance_1$cl[i], rownames(centroides_1))
      cent_2 <- match(probPreds_distance_2$cl[i], rownames(centroides_2))
      
      dist_inst_1 <- euclidian_distance(data1[probPreds_distance_1$id[i],-cls_1],
                                        centroides_1[cent_1,])
      dist_inst_2 <- euclidian_distance(data2[probPreds_distance_2$id[i],-cls_2],
                                        centroides_2[cent_2,])
      probPreds_distance_1$pred[i] <- probPreds_distance_1$pred[i] / dist_inst_1
      probPreds_distance_2$pred[i] <- probPreds_distance_2$pred[i] / dist_inst_2
    }
    
    # Sort the instances based on confidence value
    probPreds1_ordenado <- order(probPreds_distance_1$pred, decreasing = T)
    probPreds2_ordenado <- order(probPreds_distance_2$pred, decreasing = T)
    
    qtd_add <- min(base_add, length(probPreds1_ordenado))
    
    if (qtd_add > 0) {
      # Select the instances to classify
      new_samples1 <- probPreds_distance_1[probPreds1_ordenado[1:qtd_add],-2]
      new_samples2 <- probPreds_distance_2[probPreds2_ordenado[1:qtd_add],-2]
      pos1 <- match(new_samples2$id, rownames(data1))
      pos2 <- match(new_samples1$id, rownames(data2))
      data1[pos1, as.character(form[[2]])] <- new_samples2$cl
      data2[pos2, as.character(form[[2]])] <- new_samples1$cl
      sup1 <- c(sup1, pos1)
      sup2 <- c(sup2, pos2)
      
    } else {
      is_sup_empty <- TRUE
    }
  }
  model1 <- generateModel(learner, form, data1[sup1, ])
  model2 <- generateModel(learner, form, data2[sup2, ])
  
  model <- list(model1, model2)
  return(model)
}



train_ensemble <- function(base_classifiers, data_labeled) {
  ensemble <- list()
  for (i in 1:length(base_classifiers)) {
    learner <- base_classifiers[[i]]
    model <- generateModel(learner, form, data_labeled)
    ensemble <- addingEnsemble(ensemble, model)
  }
  return(ensemble)
}


predictEnsemble <- function(ensemble, oracleDB, all_levels) {
  classPred <- generateMemory(oracleDB, length(levels(all_levels)), levels(all_levels))
  for (cl in ensemble) {
    pred <- predictClass(cl, oracleDB)
    pos <- match(pred, colnames(classPred))
    for (sample in 1:length(pos)) {
      classPred[sample, pos[sample]] <- classPred[sample, pos[sample]] + 1
    }
  }
  return(classPred)
}

select_ensemble <- function(classPred, concordance = 15) {
  selected <- c()
  instances <- rownames(classPred)
  for (i in 1:nrow(classPred)) {
    if (classPred[i, which.max(classPred[i,])] >= concordance) {
      selected <- c(selected, instances[i])
    }
  }
  return(selected)
}

classify_ensemble <- function(classPred, all_levels) {
  allClassify <- c()
  for (sample in 1:nrow(classPred)) {
    allClassify <- c(allClassify, which.max(classPred[sample,]))
  }
  ensemblePred <- factor(names(allClassify), levels(all_levels))
  return(ensemblePred)
}

criar_visao <- function(dados) {
  col <- round((ncol(dados) - 1) / 2)
  col1 <- as.integer((ncol(dados) - 1) / 2)
  if ((col + col1) < (ncol(dados) - 1)) {
    col <- col + 1
  }
  xl <- dados[,1:ncol(dados) - 1] #a base dados sem os rotulos
  yl <- dados[-(1:ncol(dados) - 1)] #rotulos da base 
  view <- partition.matrix(xl, sep = length(dados), rowsep = nrow(dados),
                           colsep = c(col,col1))
  data1 <- data.frame(view$`1`$`1`,yl)
  data2 <- data.frame(view$`1`$`2`,yl)
  visoes <- list(data1,data2)
  return(visoes)
}

#' @description Create a classifier from a data set.
#'
#' @param learner A classifier will be trained.
#' @param form The formula of the features and target.
#' @param dataLab Data set which all labeled samples.
#'
#' @return A trained model.
#'
generateModel <- function(learner, form, dataLab) {
  model <- runLearner(learner, form, dataLab)
  return(model)
}

#' @description Generate a matrix with all samples than contains class,
#'  confidence rate and id of each samples in the data.
#'
#' @param model A classifier will be trained.
#' @param dataUnl Data set which all labeled samples.
#' @param predFunc The formula to the classifier use the confidence value.
#'
#' @return Generate a matrix with all samples x class | confidence value | id.
#'
generateProbPreds <- function(model, dataUnl, predFunc) {
  probPreds <- generatePredict(model, dataUnl, predFunc)
  return(probPreds)
}

#' @description Calculate the acc of the main diagonal of the matrix.
#'
#' @param dataset The data set.
#' @param sup The ids of the labeled samples.
#'
#' @return The real ids of the data set.
#'
getRealId <- function(dataset, sup) {
  ids <- as.integer(rownames(dataset))
  return(ids[-sup])
}

getID <- function(base, sup){ #pegr o ID real no data
  base_local <- base
  base_local$id <- seq(1,nrow(base_local))
  base_local <- base_local[-sup,]
  return(base_local$id)
}


#' @description This function set the class atribute to NA without change the
#'  class of selected samples
#'
#' @usage newBase(labeledDB, trainId)
#'
#' @param labeledDB The full dataset without changes
#' @param trainId The vector with the selected samples
#'
#' @return A new dataset with some percents of the samples have the NA in class
#' atribute
#'
newBase <- function(labeledDB, trainId){
  labeledDB[-trainId, label] <- NA
  return(labeledDB)
}

#' @description Change the confidence rate using changeRate param to change the
#'  confidence and flexibilize the algorithm.
#'
#' @param localAcc Accuracy of the model trained with a sample set.
#' @param initialAcc The accuracy of the initial labeled samples.
#' @param confValue The Confidence value of the present iteration.
#' @param changeRate The factor of the change.
#'
#' @return The new confidence value changed by the `changeRate` value.
#'
newConfidence <- function(localAcc, initialAcc, confValue, changeRate) {
  crRatio <- changeRate / 100
  if ((localAcc > (initialAcc + 1)) && ((confValue - crRatio) > 0.0)) {
    confValue <- confValue - crRatio
  } else if ((localAcc < (initialAcc - 1)) && ((confValue + crRatio) <= 1)) {
    confValue <- confValue + crRatio
  }
  return(confValue)
}

#' @description Search in the `memo` matrix, the higger value of the sample
#'  (sum or vote).
#'
#' @param i The index will be searched.
#' @param memo The matrix with the values.
#'
#' @return The label of the index `i`.
#'
searchClass <- function(i, memo) {
  return(colnames(memo)[which.max(memo[match(i, rownames(memo)), ])])
}


#' @description Make a supervised model and get the accuracy of this.
#'
#' @param cl The choosen classifier
#' @param iniLabDB The dataset with the initial samples labeled.
#'
#' @return Return the accuracy of the dataset with the initial samples
#' labeled.
#'
supAcc <- function(cl, iniLabDB) {
  std <- supModel(cl, iniLabDB)
  supConfusionMatrix <- confusionMatrix(std, iniLabDB)
  return(getAcc(supConfusionMatrix))
}

#' @description A supervised model trained with the initial samples.
#'
#' @param cl The classifier to be used.
#' @param iniLabDB The dataset with the initial samples labeled.
#'
#' @return Return a supervised classifier.
#'
supModel <- function(cl, iniLabDB) {
  std <- J48(form, iniLabDB)
  return(std)
}

#' @description Storage the vote of the classifier in each iteration.
#'
#' @param probPreds A data frame with remaining samples.
#' @param memo The history of the choices since the first iteration.
#' @param method The mode to adding the value to `memo` matrix.
#'
#' @return The updated `memo` matrix using fashion.
#'
updateMemory <- function(probPreds, memo, method) {
  distClass <- colnames(memo)
  for (x in 1:nrow(probPreds)) {
    id <- match(probPreds[x, 3], rownames(memo))
    for (y in 1:length(distClass)) {
      if (as.character(probPreds[x, 1]) == as.character(distClass[y])) {
        switch(method,
               "1" = value <- probPreds[x, 2],
               "2" = value <- 1
        )
        memo[id, distClass[y]] <- memo[id, distClass[y]] + value
        break
      }
    }
  }
  return(memo)
}

#' @description Check if the classification is valid. If the train is not valid, 
#'  combine all sets and try to train again.
#'
#' @param data The data set with all samples.
#' @param trainSet A vector with the samples to train.
#' @param oldTrainSet Old vector with the samples to train.
#' @param nClass The total of the classes in the dataset.
#' @param minClass The min samples of each class for training.
#'
#' @return Logical return if the classification is valid.
#'
validClassification <- function(data, trainSet, oldTrainSet, nClass, minClass) {
  if (validTraining(data, trainSet, nClass, minClass)) {
    return(TRUE)
  } else if (!(is.null(length(oldTrainSet)))) {
    trainSet <- c(trainSet, oldTrainSet)
    return(validTraining(data, trainSet, nClass, minClass))
  }
  return(FALSE)
}

#' @description TODO Review: Check if exists a min accetable samples per class.
#' o treino só é válido se todas as classes estiverem representadas no conj.
#' de treimento e se a quantidade de exemplos de cada classe for no mínimo (a
#'  qtdade
#' de exemplos da classe com menor representação no conj. ini. rot.?)
#'
#' @param data The data set with all samples.
#' @param trainIds The real ids of the selected samples to classify in present
#'   iteration.
#' @param nClass The number of the distinct classes in the data set.
#' @param minClass The min samples of each class that training require.
#'
#' @return Logical return training is valid.
#'
validTraining <- function(data, trainIds, nClass, minClass) {
  distClass <- ddply(data[trainIds, ], ~class, summarise, num = length(class))
  if (distClass$num[which.min(distClass$num)] > minClass) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
