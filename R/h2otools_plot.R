#' @title h2o.ggpplot.varimp
#' @export
h2o.ggpplot.varimp <- function(
  model,
  max_vars = 20,
  range_vars = NULL,
  binwidth = 0.05,
  fill = rgb(0.85,0.72,0.2,0.5),
  color = rgb(0, 0, 0, 0.8),
  horizontal = TRUE,
  reverseOrder = FALSE,
  title = "Model",
  ylab = "Percent. Influence",
  xlab = NULL,
  angle = 65,
  hjust = 1,
  log = FALSE
) {
  if (!is.na(match("description", names(model@model$params)))) {
    if (model@model$param$description=="Distributed GBM") {
      metric = "Percent.Influence"
      df = data.frame(x = rownames(model@model$varimp), y = model@model$varimp[,metric])
    } else if (model@model$param$description=="Distributed RF") {
      metric = "Relative importance"
      df = data.frame(x = colnames(model@model$varimp), y = 100*as.numeric(model@model$varimp[metric,])/sum(model@model$varimp[metric,], na.rm = TRUE))
    } else {
      metric = "Percent.Influence"
      df = data.frame(x = rownames(model@model$varimp), y = model@model$varimp[,metric])
    }
  } else {
    metric = "Percent.Influence"
    df = data.frame(x = rownames(model@model$varimp), y = model@model$varimp[,metric])
  }

  df = df[order(df$y, decreasing = TRUE), ]

  df = df[df$y!=0, ]
  df$x <- factor(df$x, levels = df$x, ordered = TRUE)

  if (is.null(range_vars)) {
    df = df[1:min(max_vars,nrow(df)), ]
  } else {
    df = df[range_vars, ]
  }

  if (log) {
    df$y = 1+df$y
  }
  if ( (horizontal && !reverseOrder) || (!horizontal && reverseOrder)) {
    df$x <- factor(df$x, levels = rev(df$x), ordered = TRUE)
  }

  p <- ggplot()
  if (is.null(color)) {
    p <- p + geom_bar(data=df, aes(x=x, y=y), binwidth=binwidth, fill=fill, color=x, stat="identity")
  } else {
    p <- p + geom_bar(data=df, aes(x=x, y=y), binwidth=binwidth, fill=fill, color=color, stat="identity")
  }
  p <- p +
    ggtitle(title) +
    ylab(ylab) +
    xlab(xlab) +
    theme(axis.text.x = element_text(angle = angle, hjust = hjust))
  if (horizontal) p <- p + coord_flip()
  if (log) p <- p + scale_y_log10()
  return(p)
}

#' @title h2o.ggpplot.dfVarImp
#' @export
h2o.read.dfVarImp <- function(
  model_name,
  folder_models = params$folder_models
) {
  strFile = paste0(folder_models,model_name,"_varimp.csv")
  if(file.exists(strFile)) {
    return(read.table(strFile))
  } else {
    message(paste0("File does not exist: ",strFile))
    invisible(NULL)
  }
}

#' @title h2o.ggplot.dfVarImp
#' @export
h2o.ggplot.dfVarImp <- function(
  dfVarImp,
  max_vars = 20,
  range_vars = NULL,
  binwidth = 0.05,
  fill = rgb(0.85,0.72,0.2,0.5),
  color = rgb(0, 0, 0, 0.8),
  horizontal = TRUE,
  reverseOrder = FALSE,
  title = "Model",
  ylab = "Percent. Influence",
  xlab = NULL,
  angle = 65,
  hjust = 1,
  log = FALSE,
  description = "Distributed GBM"
) {
  if (description=="Distributed GBM") {
    metric = "Percent.Influence"
    df = data.frame(x = rownames(dfVarImp), y = dfVarImp[,metric])
  } else if (description=="Distributed RF") {
    metric = "Relative importance"
    df = data.frame(x = colnames(dfVarImp), y = 100*as.numeric(dfVarImp[metric,])/sum(dfVarImp[metric,], na.rm = TRUE))
  } else {
    metric = "Percent.Influence"
    df = data.frame(x = rownames(dfVarImp), y = dfVarImp[,metric])
  }

  df = df[order(df$y, decreasing = TRUE), ]

  df = df[df$y!=0, ]
  df$x <- factor(df$x, levels = df$x, ordered = TRUE)

  if (is.null(range_vars)) {
    df = df[1:min(max_vars,nrow(df)), ]
  } else {
    df = df[range_vars, ]
  }

  if (log) {
    df$y = 1+df$y
  }
  if ( (horizontal && !reverseOrder) || (!horizontal && reverseOrder)) {
    df$x <- factor(df$x, levels = rev(df$x), ordered = TRUE)
  }

  p <- ggplot()
  if (is.null(color)) {
    p <- p + geom_bar(data=df, aes(x=x, y=y), binwidth=binwidth, fill=fill, color=x, stat="identity")
  } else {
    p <- p + geom_bar(data=df, aes(x=x, y=y), binwidth=binwidth, fill=fill, color=color, stat="identity")
  }
  p <- p +
    ggtitle(title) +
    ylab(ylab) +
    xlab(xlab) +
    theme(axis.text.x = element_text(angle = angle, hjust = hjust))
  if (horizontal) p <- p + coord_flip()
  if (log) p <- p + scale_y_log10()
  return(p)
}

#' @title h2o.ggplot.gain
#' @export
h2o.ggplot.gain <- function(
  dfGains,
  title = "Cum. Gain",
  ylab = "% responses",
  xlab = "% customers",
  addAUC = TRUE,
  ndigits = 5,
  color = rgb(0.1,0.1,0.6,0.4),
  color.base = rgb(0,0,0,0.2)
) {

  if (addAUC) {
    auc_gain = h2o.auc(dfGains$Quantile/100, dfGains$Cume.Pct.Total.Lift/100)
    xlab = paste0(xlab, "\n(AUC train = ",round(auc_gain, ndigits),")")
  }

  df = data.frame(x=c(0,dfGains$Quantile,100),y=c(0,dfGains$Cume.Pct.Total.Lift,100))
  p <- ggplot() +
    geom_path(aes(x=c(0,100),y=c(0,100)), color=color.base) +
    geom_path(data=df, aes(x=x, y=y), color=color) +
    geom_point(data=df, aes(x=x, y=y), color=color, size=1) +
    ggtitle(title) +
    ylab(ylab) +
    xlab(xlab)
  return(p)
}

#' @title h2o.ggplot.gains
#' @export
h2o.ggplot.gains <- function(
  dfGains.train,
  dfGains.test,
  title = "Cum. Gain",
  ylab = "% responses",
  xlab = "% customers",
  addAUC = TRUE,
  ndigits = 5,
  color.train = rgb(0.6,0.1,0.1,0.4),
  color.test = rgb(0.1,0.1,0.6,0.4),
  color.point.train = rgb(0.7,0.1,0.1,0.7),
  color.point.test = rgb(0.1,0.1,0.7,0.7),
  color.base = rgb(0,0,0,0.2)
) {

  if (addAUC) {
    auc_gain.train = h2o.auc(dfGains.train$Quantile/100, dfGains.train$Cume.Pct.Total.Lift/100)
    auc_gain.test = h2o.auc(dfGains.test$Quantile/100, dfGains.test$Cume.Pct.Total.Lift/100)
    xlab = paste0(xlab, "\n(AUC train = ",round(auc_gain.train, ndigits)," | ", "AUC test = ",round(auc_gain.test, ndigits),")")
  }

  dfTrain = data.frame(x=c(0,dfGains.train$Quantile,100), y=c(0,dfGains.train$Cume.Pct.Total.Lift,100))
  dfTest = data.frame(x=c(0,dfGains.test$Quantile,100),y=c(0,dfGains.test$Cume.Pct.Total.Lift,100))

  p <- ggplot() +
    geom_path(aes(x=c(0,100),y=c(0,100)), color=color.base) +
    geom_path(data=dfTrain, aes(x=x,y=y, color='train')) +
    geom_point(data=dfTrain, aes(x=x,y=y), color=color.point.train, size=1) +
    geom_path(data=dfTest, aes(x=x,y=y, color='test')) +
    geom_point(data=dfTest, aes(x=x,y=y), color=color.point.test, size=1) +
    ggtitle(title) +
    ylab(ylab) +
    xlab(xlab) +
    scale_colour_manual(name = '', values=c('train'=color.train, 'test'=color.test))

  return(p)
}

#' @title h2o.ggplot.roc
#' @export
h2o.ggplot.roc <- function(
  perf,
  title = "ROC Curve",
  ylab = "sensitivity (TPR)",
  xlab = "1 - specificity (FPR)",
  addAUC = TRUE,
  ndigits = 5,
  color = rgb(0.1,0.1,0.6,0.4),
  color.point = rgb(0.1,0.1,0.7,0.7),
  color.base = rgb(0,0,0,0.2)
) {

  indOpt = which.min(abs(perf@cutoffs-perf@model$best_cutoff))

  if (addAUC) xlab = paste0(xlab, "\n(AUC = ",round(perf@model$auc, ndigits),")")

  df1 = data.frame(x=perf@roc$FPR[-indOpt], y=perf@roc$TPR[-indOpt])
  df2 = data.frame(x=perf@roc$FPR[indOpt], y=perf@roc$TPR[indOpt])
  df3 = data.frame(x=perf@roc$FPR, y=perf@roc$TPR)
  p <- ggplot(environment = environment()) +
    geom_path(aes(x=c(0,1),y=c(0,1)), color=color.base) +
    geom_path(data=df3, aes(x=x, y=y), color=color) +
    geom_point(data=df1, aes(x=x,y=y), color=color, size=1) +
    geom_point(data=df2, aes(x=x,y=y), color=color.point, size=3) +
    ggtitle(title) +
    ylab(ylab) +
    xlab(xlab)
  return(p)
}

#' @title h2o.ggplot.rocs
#' @export
h2o.ggplot.rocs <- function(
  perf.train,
  perf.test,
  title = "ROC Curve",
  ylab = "sensitivity (TPR)",
  xlab = "1 - specificity (FPR)",
  addAUC = TRUE,
  ndigits = 5,
  color.train = rgb(0.6,0.1,0.1,0.4),
  color.test = rgb(0.1,0.1,0.6,0.4),
  color.point.train = rgb(0.7,0.1,0.1,0.7),
  color.point.test = rgb(0.1,0.1,0.7,0.7),
  color.base = rgb(0,0,0,0.2)
) {

  indOpt.train = which.min(abs(perf.train@cutoffs-perf.train@model$best_cutoff))
  indOpt.test = which.min(abs(perf.test@cutoffs-perf.test@model$best_cutoff))

  if (addAUC) {
    xlab = paste0(xlab, "\n(AUC train = ",round(perf.train@model$auc, ndigits)," | ", "AUC test = ",round(perf.test@model$auc, ndigits),")")
  }

  df1.train = data.frame(x=perf.train@roc$FPR[-indOpt.train], y=perf.train@roc$TPR[-indOpt.train])
  df2.train = data.frame(x=perf.train@roc$FPR[indOpt.train], y=perf.train@roc$TPR[indOpt.train])
  df3.train = data.frame(x=perf.train@roc$FPR, y=perf.train@roc$TPR)
  df1.test = data.frame(x=perf.test@roc$FPR[-indOpt.test], y=perf.test@roc$TPR[-indOpt.test])
  df2.test = data.frame(x=perf.test@roc$FPR[indOpt.test], y=perf.test@roc$TPR[indOpt.test])
  df3.test = data.frame(x=perf.test@roc$FPR, y=perf.test@roc$TPR)
  p <- ggplot(environment = environment()) +
    geom_path(aes(x=c(0,1),y=c(0,1)), color=color.base) +
    geom_path(data=df3.train, aes(x=x, y=y, color='train')) +
    geom_point(data=df1.train, aes(x=x,y=y), color=color.point.train, size=1) +
    geom_point(data=df2.train, aes(x=x,y=y, color='train'), size=3) +
    geom_path(data=df3.test, aes(x=x, y=y, color='test')) +
    geom_point(data=df1.test, aes(x=x,y=y), color=color.point.test, size=1) +
    geom_point(data=df2.test, aes(x=x,y=y, color='test'), size=3) +
    ggtitle(title) +
    ylab(ylab) +
    xlab(xlab) +
    scale_colour_manual(name = '', values=c('train'=color.train, 'test'=color.test))
  return(p)
}

#' @title h2o.ggplot.categoric
#' @export
h2o.ggplot.categoric <- function(
  var_cat,
  var_score,
  title = NULL,
  ylab = NULL,
  xlab = NULL,
  outlier.color = NA,
  shape = ".",
  alpha = 0.3,
  angle = 90,
  ylimit = NULL,
  legend.position = "none"
) {

  p <- ggplot(data=data.frame(NULL), environment=environment()) +
    geom_boxplot(aes(x=factor(var_cat), y=var_score, color=factor(var_cat)), outlier.color=outlier.color) +
    geom_jitter(aes(x=factor(var_cat), y=var_score, color=factor(var_cat)), shape=shape, alpha=alpha) +
    ggtitle(title) +
    ylab(ylab) +
    xlab(xlab) +
    theme(legend.position=legend.position, axis.text.x=element_text(angle=angle))
  if (!is.null(ylimit)) {
    p <- p + coord_cartesian(ylim=ylimit)
  }
  return(p)
}

#' @title h2o.ggplot.numeric
#' @export
h2o.ggplot.numeric <- function(
  var_num,
  var_score,
  smooth = 'red',
  colors_grad = c("blue", "red"),
  alpha_smooth = 0.3,
  title = NULL,
  ylab = NULL,
  xlab = NULL,
  shape = '.',
  alpha = 0.1,
  xlimit = NULL,
  ylimit = NULL,
  legend.position = "none"
) {

  p <- ggplot(data=data.frame(NULL), aes(var_num, var_score, color=var_score, alpha=alpha), environment=environment()) +
    geom_point(shape=shape) +
    scale_color_gradientn(colours=colors_grad) +
    ggtitle(title) +
    ylab(ylab) +
    xlab(xlab) +
    theme(legend.position=legend.position)
  if (!is.null(smooth)) {
    p <- p +  geom_smooth(color=smooth, fill=smooth, alpha=alpha_smooth)
  }
  if (!is.null(xlimit) && !is.null(ylimit)) {
    p <- p + coord_cartesian(xlim=xlimit, ylim=ylimit)
  } else if (!is.null(xlimit)) {
    p <- p + coord_cartesian(xlim=xlimit)
  } else if (!is.null(ylimit)) {
    p <- p + coord_cartesian(ylim=ylimit)
  }
  return(p)
}

#' @title h2o.ggplot.density2d
#' @export
h2o.ggplot.density2d <- function(
  var_num1,
  var_num2,
  title = NULL,
  main = ""
) {

  df = data.frame(var_num1=var_num1, var_num2=var_num2)
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}

  p1 <- ggplot(reshape2::melt(df, measure=1:2)) +
    geom_density(aes(x=value,fill=variable,alpha=variable)) +
    scale_alpha_discrete(range = c(0.6, 0.1)) +
    theme(legend.position="none",axis.text.y=element_text(angle=90))

  plegend = g_legend(ggplot(reshape2::melt( df, measure=1:2)) +
                       geom_density(aes(x=value,fill=variable),alpha=0.6))

  p2 <- ggplot(df) +
    geom_bin2d(aes(x=var_num2, y=var_num1), binwidth = c(0.05, 0.05) ) +
    geom_density2d(aes(x=var_num2, y=var_num1), alpha=0.5) +
    geom_abline() +
    theme(legend.position="none")

  p3 <- ggplot(reshape2::melt(df, measure=1:2)) +
    geom_density(aes(x=value,fill=variable,alpha=variable)) +
    scale_alpha_discrete(range = c(0.1, 0.6)) +
    coord_flip() +
    theme(legend.position="none")

  p <- gridExtra::grid.arrange(p1, plegend, p2, p3,
                               nrow=2,
                               main=main,
                               sub=grid::textGrob(title, gp=grid::gpar(font=1,fontsize=10))
  )

  return(p)
}

#' @title h2o.ggplot.distribution
#' @export
h2o.ggplot.distribution <- function(
  h2o_parsed_data,
  log10scale = 2,
  angle = 65,
  order = FALSE,
  orderRev = FALSE,
  color = rgb(0.85,0.72,0.2,0.5),
  stroke = rgb(0, 0, 0, 0.2),
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  stat = "identity"
) {
  library(ggplot2)

  dfTable = as.data.frame(h2o.table(round(10^log10scale*h2o_parsed_data)))
  dfTable = dfTable[order(dfTable$row.names), ]
  dfTable[,1] = 10^(-log10scale)*dfTable[,1]

  x = dfTable[,1]
  y = dfTable[,2]

  if (order) {
    if (orderRev)
      x = factor(x, levels = rev(unique(x)), ordered = TRUE)
    else x = factor(x, levels = unique(x), ordered = TRUE)
  }
  p <- ggplot(data = data.frame(emptydata = logical(0)), environment = environment()) +
    geom_bar(aes(x = x, y = y), fill = color, color = stroke,
             stat = stat) + theme(axis.text.x = element_text(angle = angle,
                                                             hjust = 1)) + labs(title = title) + xlab(xlab) + ylab(ylab)
  return(p)
}

#' @title h2o.report.html
#' @export
h2o.report.html <- function(
  openHTML = interactive(),
  folderHTML = tempdir(),
  tempFileHTML = tempfile(pattern = "h2o_report_html_", fileext = ".html", tmpdir = folderHTML),
  tempFileTXT = tempfile(pattern = "h2o_report_html_", fileext = ".txt", tmpdir = tempdir())
) {

  message(paste0("Temporal txt file: ",tempFileTXT))
  message(paste0("Temporal html file: ",tempFileHTML))

  txt_file_name = strsplit(tempFileTXT, split = "/")
  txt_file_name = unlist(txt_file_name)
  txt_file_name = txt_file_name[length(txt_file_name)]
  message(paste0("Probably a txt file has been created at ",getwd(), "/\nwith the name: ",substr(txt_file_name, 1, nchar(txt_file_name)-4),"-out",".txt"))

  strMarkDown = paste0(
    '---
    title: \"Report\"
    output: html_document
    ---

    ```{r, echo=FALSE}
    h2o.ggplot.roc(perf.train, title=\"ROC Curve (train)\")
    h2o.ggplot.roc(perf.test, title=\"ROC Curve (test)\")

    h2o.ggplot.rocs(perf.train, perf.test)

    h2o.ggplot.gain(dfGains.train, title=\"Cum. Gain (train)\")
    h2o.ggplot.gain(dfGains.test, title=\"Cum. Gain (test)\")

    h2o.ggplot.gains(dfGains.train, dfGains.test)

    h2o.ggpplot.varimp(model, max_vars=30)

    for (vars in split(1:nrow(model@model$varimp), floor((1:nrow(model@model$varimp)-1)/10))) {
    p <- h2o.ggpplot.varimp(model, range_vars=vars)
    plot(p)
    }

    h2o.ggplot.distribution(pred.train[,3], title=\"Score distribution (train)\")
    h2o.ggplot.distribution(pred.test[,3], title=\"Score distribution (test)\")
    ```'
    )

  sink(tempFileTXT, append=FALSE, type = c("output", "message"), split=FALSE)
  cat(strMarkDown)
  sink()
  knitr::opts_knit$set(progress = TRUE, verbose = FALSE, base.dir=folderHTML)
  knitr::knit2html(input=tempFileTXT, output=tempFileHTML, quiet=TRUE)

  #   knitr::opts_knit$set(progress = TRUE, verbose = FALSE, base.dir=folderHTML)
  #   knitr::knit2html(text=strMarkDown, output=tempFileHTML)

  if(openHTML) browseURL(url=tempFileHTML)

  invisible(tempFileHTML)
}