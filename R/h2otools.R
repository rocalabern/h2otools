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
  keys <- h2o.ls(h2oServer, pattern = pattern)$Key
  if (!is.null(keys))
    h2o.rm(h2oServer, keys)
  invisible(keys)
}

#' h2oshowMemory
#' @export
h2oshowMemory <- function(h2oServer) {
  ts <- h2o.ls(h2oServer)
  t <- as.data.frame(cbind(ts$Key, ts$Bytesize / 1024^2))
  colnames(t) <- colnames(ts)
  t
}

#' h2oSetPrefixKey
#' @export
h2oSetPrefixKey <- function (prefix) {
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
  if (class(model_scores_real) == "H2OParsedData") {
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
#' @export
h2oToRDF <- function (x, use_hex_string=FALSE, stringsAsFactors=FALSE, blank.lines.skip = FALSE, ...) {
  if (class(x) != "H2OParsedData")
    stop("x must be of class H2OParsedData")
  #   use_hex_string = FALSE
  #   if (as.numeric(R.Version()$major) >= 3) {
  #     if (as.numeric(R.Version()$minor) >= 1) {
  #       use_hex_string = TRUE
  #     }
  #   }
  warning(paste0("Using use_hex_string=",use_hex_string))
  url <- paste("http://", x@h2o@ip, ":", x@h2o@port, "/2/DownloadDataset",
               "?src_key=", URLencode(x@key), "&hex_string=", as.numeric(use_hex_string),
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
h2oToRarray <- function (x, numeric=TRUE, ...) {
  if (class(x) != "H2OParsedData") stop("Input is not an H2OParsedData")
  if (numeric && h2oType(x@h2o,x)[1] != "Real") stop("Input is not Real")
  array = h2oToRDF(x,...)
  if (numeric) array = as.numeric(unlist(array))
  return (array)
}
