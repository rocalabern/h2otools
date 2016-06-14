#' ls.objects
#' @export
ls.objects <- function (
  pattern,
  pos = 1,
  order.by="Size",
  decreasing=TRUE)
{
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  if (length(names)==0) {
    message("Empty workspace")
    invisible(NULL)
  } else {
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {format(utils::object.size(x), units = "auto") })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
      as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!is.null(order.by) && order.by!="Name") out <- out[order(out[[order.by]], decreasing=decreasing), ]
    out
  }
}

#' rs.system
#' @export
rs.system <- function (command) {
  textOutput <- system(command, intern=TRUE)
  cat(paste(textOutput, collapse="\n"))
  cat("\n")
  invisible(textOutput)
}

#' rs.memory
#' @export
rs.memory <- function () {
  rs.system("ps aux | grep 'USER' | grep 'PID' | grep '%CPU' | grep '%MEM' | grep 'COMMAND' | grep -v 'grep'")
  rs.system("ps aux | grep 'h2o.jar' | grep -v 'grep'")
  cat("\n")
  rs.system("ps aux | grep 'USER' | grep 'PID' | grep '%CPU' | grep '%MEM' | grep 'COMMAND' | grep -v 'grep'")
  rs.system("ps aux | grep 'rstudio-server' | grep -v 'grep'")
  cat("\n")
  rs.system("free -m")
  invisible(NULL)
}

#' rs.show
#' @export
rs.show <- function () {
  rs.system("rstudio-server active-sessions")
}

#' rs.suspend
#' @export
rs.suspend <- function (pid) {
  rs.system(paste0("rstudio-server suspend-session ",pid))
}

#' rs.restart
#' @export
rs.restart <- function (pid) {
  rs.system(paste0("rstudio-server force-suspend-session ",pid))
}