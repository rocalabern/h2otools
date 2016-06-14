# roxygen2::roxygenise()

# param.dictionary.numeric = character(0)
# param.dictionary.categorical = character(0)
# dataset.h2o = dataset

h2o.global.execute <- function(strCode) {
  eval(parse(text=paste0(strCode)), envir=globalenv())
}

#' @title h2o.setNumericColumns
#' @export
h2o.setNumericColumns <- function(
  dataset.h2o,
  listVarsNumeric = param.dictionary.numeric,
  limit.num.messages = 30
) {
  if (!class(dataset.h2o)=="H2OParsedData") stop("Class of dataset is not H2OParsedData.")
  if (nrow(dataset.h2o)<=0) stop("Dataset has no data at all. Minimum 1 row is needed.")

  dataset.h2o.r = as.data.frame(dataset.h2o[1,])

  h2o.global.execute(paste0("capture.output(.h2o <<- h2o::h2o.init(ip = \"",dataset.h2o@h2o@ip,"\", port = ",dataset.h2o@h2o@port,", startH2O = FALSE))"))
  h2o.global.execute(paste0(".frame = h2o.getFrame(.h2o,   \"",dataset.h2o@key,"\")"))

  indNumeric = match(listVarsNumeric, colnames(dataset.h2o.r))[!is.na(match(listVarsNumeric, colnames(dataset.h2o.r)))]
  if (length(indNumeric)>0) {
    message(paste0("[H2OTOOLS] Numeric: ",length(indNumeric)," numeric variables found."))
    for (col in indNumeric) {
      if (length(indNumeric) <= limit.num.messages) message(paste0("\tColumn ",colnames(dataset.h2o.r)[col]," forced to be numeric."))
      h2o.global.execute(paste0(".frame[,",col,"] = as.numeric(as.character(as.data.frame(.frame[,",col,"])[,1]))"))
    }
  } else {
    message(paste0("[H2OTOOLS] Numeric: No numeric variables found."))
  }

  invisible(dataset.h2o)
}

#' @title h2o.setFactorColumns
#' @export
h2o.setFactorColumns <- function(
  dataset.h2o,
  listVarsCategorical = param.dictionary.categorical,
  limit.num.messages = 30
) {
  if (!class(dataset.h2o)=="H2OParsedData") stop("Class of dataset is not H2OParsedData.")
  if (nrow(dataset.h2o)<=0) stop("Dataset has no data at all. Minimum 1 row is needed.")

  dataset.h2o.r = as.data.frame(dataset.h2o[1,])

  h2o.global.execute(paste0("capture.output(.h2o <<- h2o::h2o.init(ip = \"",dataset.h2o@h2o@ip,"\", port = ",dataset.h2o@h2o@port,", startH2O = FALSE))"))
  h2o.global.execute(paste0(".frame = h2o.getFrame(.h2o,   \"",dataset.h2o@key,"\")"))

  indCategorical = match(listVarsCategorical, colnames(dataset.h2o.r))[!is.na(match(listVarsCategorical, colnames(dataset.h2o.r)))]
  if (length(indCategorical)>0) {
    message(paste0("[H2OTOOLS] Categorical: ",length(indCategorical)," categorical variables found."))
    for (col in indCategorical) {
      if (length(indCategorical) <= limit.num.messages) message(paste0("\tColumn ",colnames(dataset.h2o.r)[col]," forced to be categorical."))
      h2o.global.execute(paste0(".frame[,",col,"] = as.factor(.frame[,",col,"])"))
    }
  } else {
    message(paste0("[H2OTOOLS] Categorical: No numeric categorical found."))
  }

  invisible(dataset.h2o)
}

#' @title h2o.setClassColumns
#' @export
h2o.setClassColumns <- function(
  dataset.h2o,
  listVarsNumeric = param.dictionary.numeric,
  listVarsCategorical = param.dictionary.categorical,
  limit.num.messages = 30
) {
  if (!class(dataset.h2o)=="H2OParsedData") stop("Class of dataset is not H2OParsedData.")
  if (nrow(dataset.h2o)<=0) stop("Dataset has no data at all. Minimum 1 row is needed.")

  dataset.h2o.r = as.data.frame(dataset.h2o[1,])

  h2o.global.execute(paste0("capture.output(.h2o <<- h2o::h2o.init(ip = \"",dataset.h2o@h2o@ip,"\", port = ",dataset.h2o@h2o@port,", startH2O = FALSE))"))
  h2o.global.execute(paste0(".frame = h2o.getFrame(.h2o,   \"",dataset.h2o@key,"\")"))

  indNumeric = match(listVarsNumeric, colnames(dataset.h2o.r))[!is.na(match(listVarsNumeric, colnames(dataset.h2o.r)))]
  if (length(indNumeric)>0) {
    message(paste0("[H2OTOOLS] Numeric: ",length(indNumeric)," numeric variables found."))
    for (col in indNumeric) {
      if (length(indNumeric) <= limit.num.messages) message(paste0("\tColumn ",colnames(dataset.h2o.r)[col]," forced to be numeric."))
      h2o.global.execute(paste0(".frame[,",col,"] = as.numeric(as.character(as.data.frame(.frame[,",col,"])[,1]))"))
    }
  } else {
    message(paste0("[H2OTOOLS] Numeric: No numeric variables found."))
  }

  indCategorical = match(listVarsCategorical, colnames(dataset.h2o.r))[!is.na(match(listVarsCategorical, colnames(dataset.h2o.r)))]
  if (length(indCategorical)>0) {
    message(paste0("[H2OTOOLS] Categorical: ",length(indCategorical)," categorical variables found."))
    for (col in indCategorical) {
      if (length(indCategorical) <= limit.num.messages) message(paste0("\tColumn ",colnames(dataset.h2o.r)[col]," forced to be categorical."))
      h2o.global.execute(paste0(".frame[,",col,"] = as.factor(.frame[,",col,"])"))
    }
  } else {
    message(paste0("[H2OTOOLS] Categorical: No numeric categorical found."))
  }

  invisible(dataset.h2o)
}