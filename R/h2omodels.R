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
