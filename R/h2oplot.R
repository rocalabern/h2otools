#' h2oWebStoreView
#' @export
h2oPlotResults <- function(
  model_score,
  real_target,
  results,
  resultsAUC,
  model,
  strExportFile = NULL,
  width=800,
  height=800) {

  indOpt = which(resultsAUC$aucdata$thresholds==results@model$best_cutoff)

  if (!is.null(strExportFile) && strExportFile!="") png(paste0(strExportFile, '_GAIN.png'),width=width,height=height)
  p <- ggplot() +
    geom_line(aes(x=c(0,1),y=c(0,1)), color=rgb(0,0,0,0.2)) +
    geom_line(aes(x=c(0,results@gains$Quantile,1),y=c(0,results@gains$Cume.Pct.Total.Lift,1)),
              color=rgb(0.1,0.1,0.6,0.4)) +
    geom_point(aes(x=c(0,results@gains$Quantile,1),y=c(0,results@gains$Cume.Pct.Total.Lift,1)),
               size=2, color=rgb(0.1,0.1,0.6,0.6)) +
    ggtitle("Cum. Gain") +
    ylab("% responses") +
    xlab("% customers")
  plot(p)
  if (!is.null(strExportFile) && strExportFile!="") dev.off()

  if (!is.null(strExportFile) && strExportFile!="") png(paste0(strExportFile, '_ROC.png'),width=width,height=height)
  df1 = data.frame(x=1-resultsAUC$aucdata$specificity[-indOpt], y=resultsAUC$aucdata$recall[-indOpt])
  df2 = data.frame(x=1-resultsAUC$aucdata$specificity[indOpt], y=resultsAUC$aucdata$recall[indOpt])
  p <- ggplot() +
    geom_line(aes(x=c(0,1),y=c(0,1)), color=rgb(0,0,0,0.2)) +
    geom_line(aes(x=1-resultsAUC$aucdata$specificity,y=resultsAUC$aucdata$recall),
              color=rgb(0.1,0.1,0.6,0.4)) +
    geom_point(data=df1, aes(x=x,y=y),
               size=2, color=rgb(0.1,0.1,0.6,0.6)) +
    geom_point(data=df2, aes(x=x,y=y),
               size=5, color=rgb(0.1,0.6,0.4,0.6)) +
    ggtitle("ROC curve") +
    ylab("recall") +
    xlab("1 - specificity")
  plot(p)
  if (!is.null(strExportFile) && strExportFile!="") dev.off()

  if (!is.null(strExportFile) && strExportFile!="") png(paste0(strExportFile, '_RelativeImportance.png'),width=width,height=height)
  df = data.frame(x = rownames(model@model$varimp), y = model@model$varimp$"Relative importance")
  df = df[with(df, order(y)), ]
  df = df[df$y!=0, ]
  df$x <- factor(df$x, levels = df$x, ordered = TRUE)
  p <- ggplot() +
    geom_bar(data=df, aes(x=x, y=y), binwidth = 0.05, fill=rgb(0.85,0.72,0.2,0.5), color="black", stat="identity") +
    coord_flip() +
    ggtitle("Model") +
    ylab("Relative importance") +
    xlab("Variables")
  plot(p)
  if (!is.null(strExportFile) && strExportFile!="") dev.off()

  rScore = h2oToRarray(model_score)
  df <- data.frame(x=(rScore-min(rScore))/(max(rScore)-min(rScore)))
  if (!is.null(strExportFile) && strExportFile!="") png(paste0(strExportFile, '_Score.png'),width=width,height=height)
  p <- ggplot(data=df) +
    geom_density(aes(x=x), fill=rgb(0.85,0.72,0.2,0.5),color=rgb(0,0,0,0.4),trim=TRUE) +
    coord_cartesian(xlim = c(0, 1))+
    ggtitle("Distribution") +
    ylab("freq.") +
    xlab("score")
  plot(p)
  if (!is.null(strExportFile) && strExportFile!="") dev.off()

  print(model)
  cat(paste0("best_cutoff: ",results@model$best_cutoff,"\n"))
  print(results@model$confusion)
}