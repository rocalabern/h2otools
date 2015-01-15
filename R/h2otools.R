#' h2oUnlockKeys
#' @title h2oUnlockKeys
#' @description
#' Unlock all keys
#' @details
#' This method is not safe. It is supposed to unlock all keys. Use at your own risk.
#' @export
h2oUnlockKeys <- function (h2oServer) {
  url <- paste0("http://", h2oServer@ip, ":", h2oServer@port, "/2/UnlockKeys")
  print(url)
  response <- getURL(url)
  print(response)
  invisible(fromJSON(response))
}

#' h2oType
#' @export
h2ormObject <- function (h2oServer, h2oParsedData) {
  if (class(h2oParsedData) != "H2OParsedData") stop("Input is not an H2OParsedData.")
  key <- h2oParsedData$Key
  h2o.rm(h2oServer, keys=key)
  rm(h2oParsedData, envir = sys.frame(1))
  invisible(key)
}

#' h2ormAll
#' @export
h2ormAll <- function(h2oServer) {
  keys <- h2o.ls(h2oServer)$Key
  if (!is.null(keys))
    h2o.rm(h2oServer, keys)
  invisible(keys)
}

#' h2ormLastValues
#' @export
h2ormLastValues <- function(h2oServer, pattern = "Last.value.") {
  if (is.null(pattern) || pattern=="") stop("Pattern is empty.")
  keys <- h2o.ls(h2oServer, pattern = pattern)$Key
  if (!is.null(keys))
    h2o.rm(h2oServer, keys)
  invisible(keys)
}

#' h2ormPattern
#' @export
h2ormPattern <- function(h2oServer, pattern) {
  if (is.null(pattern) || pattern=="") stop("Pattern is empty.")
  keys <- h2o.ls(h2oServer, pattern = pattern)$Key
  if (!is.null(keys))
    h2o.rm(h2oServer, keys)
  invisible(keys)
}

#' h2oKeysPattern
#' @export
h2oKeysPattern <- function(h2oServer, pattern) {
  if (is.null(pattern) || pattern=="") stop("Pattern is empty.")
  return (h2o.ls(h2oServer, pattern = pattern)$Key)
}

#' h2oShowMemory
#' @export
h2oShowMemory <- function(h2oServer) {
  ts <- h2o.ls(h2oServer)
  t <- as.data.frame(cbind(ts$Key, ts$Bytesize / 1024^2))
  colnames(t) <- colnames(ts)
  t
}

#' h2oSetPrefixKey
#' @export
h2oSetPrefixKey <- function (prefix) {
  if (is.null(prefix) || prefix=="") stop("Prefix is empty.")
  if (is.character(prefix)) {
    strPrefix <<- prefix
  } else {
    strPrefix <<- deparse(substitute(prefix))
  }
}

#' h2oKey
#' @export
h2oKey <- function (key,sep="_") {
  if (is.character(key)) {
    paste0(strPrefix,sep,key)
  } else {
    paste0(strPrefix,sep,deparse(substitute(key)))
  }
}

#' h2oKeyPaste
#' @export
h2oKeyPaste <- function (...,sep="_") {
  return (paste0(strPrefix,sep,paste(...,sep=sep,collapse=sep)))
}

#' h2oType
#' @export
h2oType <- function (h2oServer, strKey) {
  if (class(strKey) == "H2OParsedData") strKey = strKey@key
  strURL <- paste0("http://",h2oServer@ip,":",h2oServer@port,"/2/Inspect2?src_key=",strKey)
  message(strURL)
  json_data <- fromJSON(getURL(strURL))
  if (json_data$numCols!=length(json_data$cols)) {
    warning(paste0("H2O columns and parsed columns does not match"))
  }

  output = NULL
  if (length(json_data$cols)>0) {
    for (k in 1:length(json_data$cols)) output = c(output, json_data$cols[[k]]$type)
  }
  return (output)
}

#' h2oToR
#' @export
h2oToR <- function (var, numeric=TRUE) {
  if (class(var) == "H2OParsedData") {
    if (numeric) {
      return (as.numeric(as.data.frame(var)[,1]))
    } else {
      return (as.data.frame(var)[,1])
    }
  } else {
    return (var)
  }
}

#' h2oToRDF
#' @title H2OParsedData object to R data.frame object
#' @description
#' \code{h2oToRDF} returns a R data.frame object.
#' @details
#' Another way to achieve this seems to be: as.numeric(as.vector(as.data.frame(...)))
#' @seealso \code{\link{as.data.frame.H2OParsedData}}
#' @export
h2oToRDF <- function (h2oParsedData, use_hex_string=FALSE, stringsAsFactors=FALSE, blank.lines.skip = FALSE, ...) {
  if (class(h2oParsedData) != "H2OParsedData")
    stop("First argyment myst be must be of class H2OParsedData")
  #   use_hex_string = FALSE
  #   if (as.numeric(R.Version()$major) >= 3) {
  #     if (as.numeric(R.Version()$minor) >= 1) {
  #       use_hex_string = TRUE
  #     }
  #   }
  warning(paste0("Using use_hex_string=",use_hex_string))
  url <- paste("http://", h2oParsedData@h2o@ip, ":", h2oParsedData@h2o@port, "/2/DownloadDataset",
               "?src_key=", URLencode(h2oParsedData@key), "&hex_string=", as.numeric(use_hex_string),
               sep = "")
  ttt <- getURL(url)
  n = nchar(ttt)
  chars_to_trim = 0
  if (n >= 2) {
    c = substr(ttt, n, n)
    if (c == "\n") {
      chars_to_trim = chars_to_trim + 1
    }
    if (chars_to_trim > 0) {
      c = substr(ttt, n - 1, n - 1)
      if (c == "\r") {
        chars_to_trim = chars_to_trim + 1
      }
    }
  }
  if (chars_to_trim > 0) {
    df <- read.csv((tcon <- textConnection(substr(ttt, 1, n - chars_to_trim))), blank.lines.skip = blank.lines.skip,
                   stringsAsFactors=stringsAsFactors,
                   ...)
  } else {
    df <- read.csv((tcon <- textConnection(ttt)), blank.lines.skip = blank.lines.skip,
                   stringsAsFactors=stringsAsFactors,
                   ...)
  }
  close(tcon)
  return(df)
}

#' h2oToRarray
#' @export
h2oToRarray <- function (h2oParsedData, numeric=TRUE, ...) {
  if (class(h2oParsedData) != "H2OParsedData") stop("Input must be a H2OParsedData")
  inputType = h2oType(h2oParsedData@h2o,h2oParsedData)[1]
  if (numeric && inputType != "Real" && inputType != "Int") {
    print(paste0("Input type is: ",inputType))
    stop("Input is not numeric (Real/Int)")
  }
  array = h2oToRDF(h2oParsedData,...)
  if (numeric) array = as.numeric(unlist(array))
  return (array)
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

#' h2oWebStoreView
#' @title h2oWebStoreView
#' @description
#' Web Store View
#' @details
#' Opens H2O url for Store View.
#' @export
h2oWebStoreView <- function (ip=NULL, port=NULL, h2oServer=NULL) {
  if (is.null(h2oServer) && is.null(ip) && is.null(port)) stop("You must give some parameters: h2oServer or ip and port")
  if(!is.null(h2oServer)) {
    ip = h2oServer@ip
    port = h2oServer@port
  }
  url <- paste0("http://", ip, ":", port, "/StoreView.html")
  print(url)
  browseURL(url)
}