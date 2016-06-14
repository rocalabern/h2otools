# roxygen2::roxygenise()

#' @title h2o.model.getPOJO
#' @export
h2o.model.getPOJO <- function(
  localH2O,
  model,
  type="GBM"
) {
  if (type=="GBM") {
    modelPOJO = RCurl::getURLContent(paste0("http://",localH2O@ip,":", localH2O@port, "/2/GBMModelView.java?_modelKey=", model@key))
    invisible(modelPOJO)
  } else if (type=="RF" || type=="DRF") {
    modelPOJO = RCurl::getURLContent(paste0("http://",localH2O@ip,":", localH2O@port, "/2/DRFModelView.java?_modelKey=", model@key))
    invisible(modelPOJO)
  } else {
    message("Not implemented")
    invisible(NULL)
  }
}

#' @title h2o.model.list
#' @export
h2o.model.list <- function(
  folder_models = params$folder_models
) {
  return(list.dirs(folder_models))
}

#' @title h2o.model.import
#' @export
h2o.model.import <- function(
  localH2O,
  model_name,
  folder_models = params$folder_models
) {

  model <- h2o.loadModel(localH2O, paste0(folder_models, model_name))

  tryCatch({
    strVarImpFile = paste0(folder_models,"/",model_name,"/",model_name,"_varimp.csv")
    if (file.exists(strVarImpFile)) {
      message(paste0("[H2O] Loading Var.Imp. ..."))
      dfVarImp = read.table(strVarImpFile)
      model@model$varimp = dfVarImp
      message(paste0("[H2O] Var.Imp. loaded."))
    } else {
      message(paste0("[H2O] Var.Imp. not loaded."))
    }
  }, error = function(e) {
    error_message <- e$message
    warning(paste0("Warn: [H2O] Error loading var.imp. : ", error_message))
  })

  return(model)
}

#' @title h2o.model.export
#' @export
h2o.model.export <- function(
  localH2O,
  model,
  model_name,
  folder_models = params$folder_models,
  force = FALSE,
  type="GBM"
) {

  h2o.saveModel(model, dir=folder_models, name=model_name, force=force)

  tryCatch({
    if (sum(names(model@model)=="varimp")==1) {
      message(paste0("[H2O] Saving Var.Imp. ..."))
      strVarImpFile = paste0(folder_models,"/",model_name,"/",model_name,"_varimp.csv")
      write.table(model@model$varimp, file=strVarImpFile)
      message(paste0("[H2O] Var.Imp. saved."))
    } else {
      warning(paste0("Warn: [H2O] not saving var.imp."))
    }
  }, error = function(e) {
    error_message <- e$message
    warning(paste0("Warn: [H2O] Error saving var.imp. : ", error_message))
  })

  tryCatch({
    message(paste0("[H2O] Saving POJO ..."))
    strJavaFile = paste0(folder_models,"/",model_name,"/",model_name,".java")
    modelPOJO = h2o.model.getPOJO(localH2O, model, type=type)
    cat(modelPOJO, file=strJavaFile)
    message(paste0("[H2O] POJO saved."))
  }, error = function(e) {
    error_message <- e$message
    warning(paste0("Warn: [H2O] Error saving POJO : ", error_message))
  })
}

#' @title h2o.model.info.to.character
#' @export
h2o.model.info.to.character <- function(
  model
) {

  tempFile = tempfile(pattern = "h2o_model_info_to_character_", fileext = ".txt", tmpdir = tempdir())
  sink(tempFile, append=FALSE, type = c("output", "message"), split=FALSE)
  h2o.model.info(model)
  sink()

  strModelInfo = readLines(tempFile, warn=FALSE)
  strModelInfo = paste0(strModelInfo, collapse="\n")
  invisible(strModelInfo)
}

#' @title h2o.model.info
#' @export
h2o.model.info <- function(
  model
) {

  cat(paste0("model@model$params$h2o (server) = ",model@model$params$h2o@ip,":",model@model$params$h2o@port))

  cat(paste0("\nmodel@model$param$description = ", model@model$param$description))
  cat(paste0("\nmodel@model$classification = ", model@model$classification))

  cat(paste0("\nmodel@model$params : "))
  cat(paste0("\n\t length(model@model$params$x) = ", length(model@model$params$x)))
  cat(paste0("\n\t model@model$params$y = ", model@model$params$y))

  cat(paste0("\n\t model@model$params$distribution = ", model@model$params$distribution))
  cat(paste0("\n\t model@model$params$n.trees = ", model@model$params$n.trees))
  cat(paste0("\n\t model@model$params$interaction.depth = ", model@model$params$interaction.depth))
  cat(paste0("\n\t model@model$params$shrinkage = ", model@model$params$shrinkage))
  cat(paste0("\n\t model@model$params$n.minobsinnode = ", model@model$params$n.minobsinnode))
  cat(paste0("\n\t model@model$params$n.bins = ", model@model$params$n.bins))
  cat(paste0("\n\t model@model$params$importance = ", model@model$params$importance))
  cat(paste0("\n\t model@model$params$nfolds = ", model@model$params$nfolds))

  cat(paste0("\n\t model@model$params$group_split = ", model@model$params$group_split))
  cat(paste0("\n\t model@model$params$grid_parallelism = ", model@model$params$grid_parallelism))

  cat(paste0("\n\nmodel@model$priorDistribution = ", paste0(model@model$priorDistribution, collapse=" ")))

  cat("\n\nmodel@model$confusion : \n")
  print(model@model$confusion)

  cat(paste0("\nmodel@model$error = ", model@model$error))

  cat(paste0("\nmodel@model$auc = ", model@model$auc))
  cat(paste0("\nmodel@model$gini = ", model@model$gini))
  cat(paste0("\nmodel@model$F1 = ", model@model$F1))
  cat(paste0("\nmodel@model$F2 = ", model@model$F2))

  cat(paste0("\nmodel@model$accuracy = ", model@model$accuracy))
  cat(paste0("\nmodel@model$precision = ", model@model$precision))
  cat(paste0("\nmodel@model$recall = ", model@model$recall))
  cat(paste0("\nmodel@model$specificity = ", model@model$specificity))
  cat(paste0("\nmodel@model$mcc = ", model@model$mcc))
  cat(paste0("\nmodel@model$max_per_class_err = ", model@model$max_per_class_err))
  cat(paste0("\nmodel@model$best_cutoff = ", model@model$best_cutoff))

  cat(paste0("\n\nmodel@model$varimp : "))
  cat(paste0("\n\t Total vars = ", length(model@model$varimp[,1])))
  cat(paste0("\n\t Total vars used = ", sum(model@model$varimp[,1]>0)))
  cat(paste0("\n\t Total vars not used = ", sum(model@model$varimp[,1]<=0)))
  cat(paste0("\n\t Vars used = ", sum(model@model$varimp[,1]>0)/length(model@model$varimp[,1]),"%"))
}

#' @title h2o.auc
#' @export
h2o.auc <- function (x, y)
{
  id <- order(x)
  return(sum(diff(x[id]) * zoo::rollmean(y[id], 2)))
}

#' @title h2o.distribution
#' @export
h2o.distribution <- function(
  h2o_parsed_data,
  log10scale = 2
) {
#   dfTable = as.data.frame(h2o.table(round(10^log10scale*h2o_parsed_data)))
#   dfTable = dfTable[sort(dfTable$row.names), ]
#   dfTable[,1] = 10^(-log10scale)*dfTable[,1]
  dfTable = as.data.frame(h2o.table(round(h2o_parsed_data, log10scale)))
  dfTable = dfTable[sort(dfTable$row.names), ]
  return(dfTable)
}

#' h2oSplitTrainTest
#' @export
h2oSplitTrainTest <- function(datasetH2O, prob) {
  random <- h2o.runif(datasetH2O)
  sampleTrain <- random<prob
  return (list(
    Train = datasetH2O[sampleTrain, ],
    Test  = datasetH2O[!sampleTrain, ]
    ))
}

#' h2oSplitTrainValidateTest
#' @export
h2oSplitTrainValidateTest <- function(datasetH2O, prob1, prob2) {
  random <- h2o.runif(datasetH2O)
  sampleTrain <- random<prob1
  sampleValidate <- prob1<=random & random<prob2
  sampleTest <- prob2<=random
  return (list(
    Train = datasetH2O[sampleTrain, ],
    Validate = datasetH2O[sampleValidate, ],
    Test = datasetH2O[sampleTest, ]
  ))
}

#' h2o.performance.gaussian
#' @title h2o.performance.gaussian
#' @description
#' Calculates some metrics for regression models (gaussian).
#' @details
#' This method returns some error metrics for regression models. It needs a couple of numeric arrays: the target and the prediction.
#' @export
h2o.performance.gaussian <- function (h2o.target, h2o.pred) {

  MAE = mean(abs(h2o.target[,1][,1]-h2o.pred[,1]))
  MSE = mean((h2o.target[,1]-h2o.pred[,1])^2)
  SMSE = sqrt(mean((h2o.target[,1]-h2o.pred[,1])^2))
  MAPE = mean(abs(h2o.target[,1]-h2o.pred[,1])/abs(h2o.target[,1]))
  SYM.MAPE = mean(abs(h2o.target[,1]-h2o.pred[,1])/(0.5*abs(h2o.pred[,1])+0.5*abs(h2o.target[,1])))
  WMAPE = mean(abs(h2o.target[,1]-h2o.pred[,1]))/mean(abs(h2o.target[,1]))
  BENCHMARK = mean(abs(h2o.target[,1]-mean(h2o.target[,1])))
  PERC.BENCHMARK = mean(abs(h2o.target[,1]-mean(h2o.target[,1]))/abs(h2o.target[,1]))

  message(paste0("MAE = ", formatDec(MAE, round=2)))
  message(paste0("MSE = ", formatDec(MSE, round=2)))
  message(paste0("SMSE = ", formatDec(SMSE, round=2)))
  message(paste0("MAPE = ", formatDec(100*MAPE, round=2)), "%")
  message(paste0("SYM.MAPE = ", formatDec(100*SYM.MAPE, round=2)), "%")
  message(paste0("WMAPE = ", formatDec(100*WMAPE, round=2)), "%")
  message(paste0("BENCHMARK = ", formatDec(BENCHMARK, round=2)))
  message(paste0("PERC.BENCHMARK = ", formatDec(100*PERC.BENCHMARK, round=2)), "%")

  output = list(
    MAE = MAE,
    MSE = MSE,
    SMSE = SMSE,
    MAPE = MAPE,
    SYM.MAPE = SYM.MAPE,
    WMAPE = WMAPE,
    BENCHMARK = BENCHMARK,
    PERC.BENCHMARK = PERC.BENCHMARK
  )
  invisible(output)
}

#' h2oPerformanceAUC
#' @title h2oPerformanceAUC
#' @description
#' AUC model performance evaluation
#' @details
#' This method returns AUC model performance evaluation.
#' @export
h2oPerformanceAUC <- function (h2oServer, dataReal, keyReal, dataModel, keyModel, criterion = "maximum_F1") {
  url <- paste0("http://", h2oServer@ip, ":", h2oServer@port,
                "/2/AUC?actual=",dataReal,"&vactual=",keyReal,
                "&predict=",dataModel,"&vpredict=",keyModel,
                "&thresholds=&threshold_criterion=",criterion)
  print(url)
  response <- getURL(url)
  invisible(fromJSON(response))
}

#' h2oWebAUC
#' @title h2oWebAUC
#' @description
#' Web AUC model evaluation
#' @details
#' Opens H2O url for AUC model evaluation.
#' @export
h2oWebAUC <- function (dataReal, keyReal, dataModel, keyModel, criterion = "maximum_F1", ip=NULL, port=NULL, h2oServer=NULL) {
  if (is.null(h2oServer) && is.null(ip) && is.null(port)) stop("You must give some parameters: h2oServer or ip and port")
  if(!is.null(h2oServer)) {
    ip = h2oServer@ip
    port = h2oServer@port
  }
  url <- paste0("http://", ip, ":", port,
                "/2/AUC.html?actual=",dataReal,"&vactual=",keyReal,
                "&predict=",dataModel,"&vpredict=",keyModel,
                "&thresholds=&threshold_criterion=",criterion)
  print(url)
  browseURL(url)
}
