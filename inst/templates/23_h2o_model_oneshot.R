if (!exists("df_table")) load("data/df_table.RData")
library(lazyeval)
library(data.table)
library(caret)
library(h2o)
library(rplot)

listVars = c("var1", "var2")
dfTable = df_table

h2oServer = h2o.init(nthreads = -1)

# params ----
close_server = FALSE
# type_model = "glm"
# type_model = "rf"
type_model = "gbm"
nfolds = 5
recalc_s = FALSE
recalc_listVars = FALSE
bTargetAsFactor = TRUE
bFeaturesFactorAsFactor = TRUE
bFeaturesCharacterAsFactor = TRUE
bFeaturesLogicalAsFactor = TRUE

target = "target"
list_key_vars = "ref_id"
if (!exists("listVars") || recalc_listVars) {
  message(paste0("listVars auto-calculated: ",length(listVars)," nvars."))
  listVars = setdiff(colnames(dfTable), c(list_key_vars, target))
} else {
  message(paste0("listVars reused: ",length(listVars)," nvars."))
}

if (type_model == "glm") {
  train_func = h2o.glm
  train_params = lazy(
    list(
      x = listVars,
      y = target,
      training_frame = temp_h2o_table,

      alpha = 0.0,
      lambda = 0.01,

      family = "binomial",
      link = "logit"
    )
  )
} else if (type_model == "rf") {
  train_func = h2o.randomForest
  train_params = lazy(
    list(
      x = listVars,
      y = target,
      training_frame = temp_h2o_table,
      max_depth = 3,
      min_rows = 300,
      ntrees = 200,
      stopping_rounds = 5,
      stopping_metric = "AUC"
    )
  )
} else if (type_model == "gbm") {
  train_func = h2o.gbm
  train_params = lazy(
    list(
      x = listVars,
      y = target,
      training_frame = temp_h2o_table,
      max_depth = 3,
      min_rows = 300,
      ntrees = 200,
      learn_rate = 0.08,
      col_sample_rate = 1,
      stopping_rounds = 5,
      stopping_metric = "logloss"
    )
  )
} else {
  stop(paste0("The type_model configured is not recognized: ",type_model))
}

# Train ----
h2o.dfTable = as.h2o(dfTable)
if (bTargetAsFactor) h2o.dfTable[, target] = h2o::as.factor(h2o.dfTable[, target])
if (bFeaturesFactorAsFactor) for (var in colnames(dfTable)[lapply(dfTable, class) %in% c("factor")]) h2o.dfTable[, var] = h2o::as.factor(h2o.dfTable[, var])
if (bFeaturesCharacterAsFactor) for (var in colnames(dfTable)[lapply(dfTable, class) %in% c("character")]) h2o.dfTable[, var] = h2o::as.factor(h2o.dfTable[, var])
if (bFeaturesLogicalAsFactor) for (var in colnames(dfTable)[lapply(dfTable, class) %in% c("logical")]) h2o.dfTable[, var] = h2o::as.factor(h2o.dfTable[, var])
# summary(h2o.dfTable)

if (!exists("s") || recalc_s) {
  message("s created")
  s = runif(nrow(dfTable))
} else {
  message("s reused")
}
ind_cv = findInterval(s, seq(0,1,1/nfolds))
table(ind_cv)
table(ind_cv, dfTable$target)

cv_models = list()
cv_predict = list()
cv_dfpredict = NULL
for (i in 1:nfolds) {
  temp_h2o_table = h2o.dfTable[which(ind_cv!=i),]
  cv_models[[i]] = do.call(train_func, lazy_eval(train_params))
  h2o.rm(temp_h2o_table)

  cv_predict[[i]] = h2o.predict(cv_models[[i]], h2o.dfTable)

  if (is.null(cv_dfpredict)) {
    cv_dfpredict = as.data.frame(cv_predict[[i]][,ncol(cv_predict[[i]])])
    colnames(cv_dfpredict) = paste0("model_",i)
  } else {
    cv_dfpredict[[paste0("model_",i)]] = as.data.frame(cv_predict[[i]][, ncol(cv_predict[[i]])])[[1]]
  }
}

pred_train = numeric(nrow(h2o.dfTable))
pred_test = numeric(nrow(h2o.dfTable))
for (i_fold in 1:nfolds) {
  ind_cv_i = which(ind_cv==i_fold)
  pred_train[ind_cv_i] = rowMeans(cv_dfpredict[ind_cv_i, -i_fold])
  pred_test[ind_cv_i] = cv_dfpredict[ind_cv_i, i_fold]
}

# Performance Analysis ----
# table(round(pred_train), dfTable[[target]])
# table(round(pred_test), dfTable[[target]])

# Gain curve per bucket ---
for (i_fold in 1:nfolds) {
  r.plot.gains(pred_train[ind_cv==i_fold], dfTable[[target]][ind_cv==i_fold], pred_test[ind_cv==i_fold], dfTable[[target]][ind_cv==i_fold], main=paste0("Gain Curve - bucket ",i_fold))
}

# ROC curve per bucket ---
for (i_fold in 1:nfolds) {
  r.plot.rocs(pred_train[ind_cv==i_fold], dfTable[[target]][ind_cv==i_fold], pred_test[ind_cv==i_fold], dfTable[[target]][ind_cv==i_fold], main=paste0("ROC Curve - bucket ",i_fold))
}

# Lift curve Train and Test ----
r.plot.lift(pred_train, dfTable[[target]], main="Lift Curve (train)")
r.plot.lift(pred_test, dfTable[[target]], main="Lift Curve (test)")

# Gain curve Train and Test ----
r.plot.gains(pred_train, dfTable[[target]], pred_test, dfTable[[target]])

# ROC curve Train and Test ----
r.plot.rocs(pred_train, dfTable[[target]], pred_test, dfTable[[target]])

# Variable Importance ----
if (type_model == "glm") {
  df_importance = cbind(model=1, cv_models[[1]]@model$coefficients_table)
  for (i_fold in 2:nfolds) {
    df_importance = rbind(df_importance, cbind(model=i_fold, cv_models[[1]]@model$coefficients_table))
  }
  names(df_importance)[names(df_importance)=="names"] = "variable"
  df_importance$scaled_importance = df_importance$standardized_coefficients
  df_importance$scaled_importance[df_importance$variable=="Intercept"] = 0
  # df_importance$scaled_importance = abs(df_importance$scaled_importance)
} else if (type_model == "rf" || type_model == "gbm") {
  df_importance = cbind(model=1, cv_models[[1]]@model$variable_importances)
  for (i_fold in 2:nfolds) {
    df_importance = rbind(df_importance, cbind(model=i_fold, cv_models[[i_fold]]@model$variable_importances))
  }
} else {
  stop(paste0("The type_model configured is not recognized: ",type_model))
}
df_importance_reshaped = reshape2::dcast(df_importance[,c("model", "variable", "scaled_importance")], variable ~ model, value.var="scaled_importance")
df_importance_reshaped[is.na(df_importance_reshaped)] = 0
df_importance_reshaped = data.frame(variable=df_importance_reshaped$variable, scaled_importance=rowMeans(df_importance_reshaped[,-1]))
df_importance_reshaped = df_importance_reshaped[order(df_importance_reshaped$scaled_importance, decreasing = TRUE), ]

n_rows = min(nrow(df_importance_reshaped), 30)
df_importance_reshaped_filt <- df_importance_reshaped[order(df_importance_reshaped$scaled_importance, decreasing = TRUE)[1:n_rows], c("scaled_importance", "variable")]
r.ggplot.bar(df_importance_reshaped_filt, x="variable", y="scaled_importance", order=TRUE, title="Importance variables")

# Print binnings score ----
t_binnings = table(findInterval(pred_test, quantile(pred_test, probs=seq(0,1,0.05))), dfTable$target)
t_binnings = t_binnings[nrow(t_binnings):1,]
t_binnings/rowSums(t_binnings)
t_binnings
if (FALSE) {
  strExport = ""
  for (i in 1:nrow(t_binnings)) {
    strExport = paste0(strExport, "\n", t_binnings[i,1], ",", t_binnings[i,2])
  }
  digorig::x.message(strExport)
}

# Close ----
if (close_server) {
  h2o.shutdown(FALSE)
  detach("package:h2o", unload=TRUE)
}
