df_importance = h2otools.get_importance_matrix(c_models)
ggplot_importance(df_importance, "scaled_importance")
ggplot_importance(df_importance, "scaled_importance_abs")

h2otools.get_importance_matrix <- function(c_models) {
  if (c_models[[1]]@model$model_summary$family == "binomial") {
    df_importance = cbind(model=1, c_models[[1]]@model$coefficients_table)
    for (i in 2:length(c_models)) {
      df_importance = rbind(df_importance, cbind(model=i, c_models[[i]]@model$coefficients_table))
    }
    names(df_importance)[names(df_importance)=="names"] = "variable"
    df_importance$scaled_importance = df_importance$standardized_coefficients
    df_importance$scaled_importance[df_importance$variable=="Intercept"] = 0
    df_importance$scaled_importance_abs = abs(df_importance$scaled_importance)
    return(df_importance)
  } else {
    invisible(NULL)
  }
}

ggplot_importance <- function (
  df, var_importance, max_rows = 30, angle = 65, order = TRUE, orderRev = FALSE, 
  color = rplot:::param.color.bar, stroke = rgb(0, 0, 0, 0.2), 
  title = "Importance variables", xlab = NULL, ylab = NULL, stat = "identity")
{
  if (!any("model"==colnames(df))) stop("data.frame input should have model as a column")
  if (!any("variable"==colnames(df))) stop("data.frame input should have variable as a column")
  if (!any("model"==colnames(df))) stop("data.frame input should have model as a column")
  if (!any(var_importance==colnames(df))) stop(paste0("data.frame input should have var_importance value as a column name : ",var_importance))
  df_importance = df
  df_importance_reshaped = reshape2::dcast(df_importance[,c("model", "variable", var_importance)], variable ~ model, value.var=var_importance)
  df_importance_reshaped = data.frame(variable=df_importance_reshaped$variable, var_importance=rowMeans(df_importance_reshaped[,-1]))
  df_importance_reshaped[is.na(df_importance_reshaped)] = 0
  df_importance_reshaped = df_importance_reshaped[order(df_importance_reshaped$var_importance, decreasing = TRUE), ]
  n_rows = min(nrow(df_importance_reshaped), max_rows)
  df_importance_reshaped_filt = df_importance_reshaped[order(df_importance_reshaped$var_importance, decreasing = TRUE)[1:n_rows], c("var_importance", "variable")]
  if (order) {
    df_importance_reshaped_filt = as.data.frame(df_importance_reshaped_filt)
    if (orderRev) df_importance_reshaped_filt[["variable"]] = factor(df_importance_reshaped_filt[["variable"]], levels = rev(unique(df_importance_reshaped_filt[["variable"]])), ordered = TRUE)
    else df_importance_reshaped_filt[["variable"]] = factor(df_importance_reshaped_filt[["variable"]], levels = unique(df_importance_reshaped_filt[["variable"]]), ordered = TRUE)
  }
  p <- ggplot2::ggplot(data = df_importance_reshaped_filt, environment = environment()) + 
    ggplot2::geom_bar(ggplot2::aes_string(x = "variable", y = "var_importance"), fill = color, color = stroke, stat = stat) + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = angle, hjust = 1)) + 
    ggplot2::labs(title = title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)
  return(p)
}