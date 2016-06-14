param.dictionary.numeric = character(0)
param.dictionary.categorical = character(0)

h2o.setVar <- function (var, value) {
  strValue = paste(capture.output(dump("value", file="")), collapse = "")
  if (substring(strValue, 1, 9)=="value <- ") {
    strValue = substring(strValue, 10)
  } else if (substring(strValue, 1, 8)=="value<- ") {
    strValue = substring(strValue, 9)
  } else if (substring(strValue, 1, 8)=="value <-") {
    strValue = substring(strValue, 9)
  } else if (substring(strValue, 1, 7)=="value<-") {
    strValue = substring(strValue, 8)
  } else if (substring(strValue, 1, 8)=="value = ") {
    strValue = substring(strValue, 9)
  } else if (substring(strValue, 1, 7)=="value= ") {
    strValue = substring(strValue, 8)
  } else if (substring(strValue, 1, 7)=="value =") {
    strValue = substring(strValue, 8)
  } else if (substring(strValue, 1, 6)=="value=") {
    strValue = substring(strValue, 7)
  }
  unlockBinding(var, env = asNamespace('h2otools'))
  eval(parse(text=paste0(var," <- ",strValue)), envir = asNamespace('h2otools'))
  lockBinding(var, env = asNamespace('h2otools'))
}

#' @title h2o.setDictionary
#' @export
h2o.setDictionary <- function (arrayNumerical, arrayCategoric) {
  h2o.setVar("param.dictionary.numeric", arrayNumerical)
  h2o.setVar("param.dictionary.categorical", arrayCategoric)
}

#' @title h2o.setDictionaryNumeric
#' @export
h2o.setDictionaryNumeric <- function (arrayNumerical) {
  h2o.setVar("param.dictionary.numeric", arrayNumerical)
}

#' @title h2o.setDictionaryCategoric
#' @export
h2o.setDictionaryCategoric <- function (arrayCategoric) {
  h2o.setVar("param.dictionary.categorical", arrayCategoric)
}

#' @title h2o.getDictionary
#' @export
h2o.getDictionary <- function () {
  return(list(param.dictionary.numeric=param.dictionary.numeric, param.dictionary.categorical=param.dictionary.categorical))
}

#' @title h2o.getDictionaryNumeric
#' @export
h2o.getDictionaryNumeric <- function () {
  return(param.dictionary.numeric)
}

#' @title h2o.getDictionaryCategoric
#' @export
h2o.getDictionaryCategoric <- function () {
  return(param.dictionary.categorical)
}