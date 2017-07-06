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
  df_gridsearch = expand.grid(
    alpha = c(0.0, 0.5, 1.0),
    lambda = c(0.0, 0.0005, 0.001, 0.01, 0.1),
    stringsAsFactors = FALSE
  )
  train_params = lazy(
    list(
      x = listVars,
      y = target,
      training_frame = temp_h2o_table,
      alpha = df_gridsearch$alpha[i_gridsearch],
      lambda = df_gridsearch$lambda[i_gridsearch],
      family = "binomial",
      link = "logit"
    )
  )
} else if (type_model == "rf") {
  train_func = h2o.randomForest
  df_gridsearch = expand.grid(
    max_depth = c(3,5,7),
    min_rows = c(50,80,100,200,300),
    ntrees = 200,
    # stopping_metric = c("AUC", "logloss")
    stopping_metric = "logloss",
    stringsAsFactors = FALSE
  )
  train_params = lazy(
    list(
      x = listVars,
      y = target,
      training_frame = temp_h2o_table,
      max_depth = df_gridsearch$max_depth[i_gridsearch],
      min_rows = df_gridsearch$min_rows[i_gridsearch],
      ntrees = df_gridsearch$ntrees[i_gridsearch],
      stopping_rounds = 5,
      stopping_metric = df_gridsearch$stopping_metric[i_gridsearch]
    )
  )
} else if (type_model == "gbm") {
  train_func = h2o.gbm
  df_gridsearch = expand.grid(
    max_depth = c(3,5,7),
    min_rows = c(50,80,100,200,300),
    ntrees = c(50,80,100,200),
    learn_rate = 0.08,
    col_sample_rate = 1.0,
    # stopping_metric = "AUC"
    stopping_metric = "logloss",
    stringsAsFactors = FALSE
  )
  train_params = lazy(
    list(
      x = listVars,
      y = target,
      training_frame = temp_h2o_table,
      max_depth = df_gridsearch$max_depth[i_gridsearch],
      min_rows = df_gridsearch$min_rows[i_gridsearch],
      ntrees = df_gridsearch$ntrees[i_gridsearch],
      learn_rate = df_gridsearch$learn_rate[i_gridsearch],
      col_sample_rate = df_gridsearch$col_sample_rate[i_gridsearch],
      stopping_rounds = 5,
      stopping_metric = df_gridsearch$stopping_metric[i_gridsearch]
    )
  )
} else {
  warning(paste0("The type_model configured is not recognized: ",type_model))
}
df_gridsearch$row = 1:nrow(df_gridsearch)
df_gridsearch = df_gridsearch[,c("row", setdiff(colnames(df_gridsearch), "row"))]
df_gridsearch_fold = merge(df_gridsearch, data.frame(fold=1:nfolds), by=NULL)
df_gridsearch_fold = df_gridsearch_fold[,c("row", "fold", setdiff(colnames(df_gridsearch_fold), c("row", "fold")))]
df_gridsearch_fold = df_gridsearch_fold[order(df_gridsearch_fold$row, df_gridsearch_fold$fold),]
df_gridsearch$train_auc_gain  = as.numeric(NA)
df_gridsearch$test_auc_gain   = as.numeric(NA)
df_gridsearch$train_auc_roc   = as.numeric(NA)
df_gridsearch$test_auc_roc   = as.numeric(NA)
df_gridsearch_fold$train_auc_gain  = as.numeric(NA)
df_gridsearch_fold$test_auc_gain   = as.numeric(NA)
df_gridsearch_fold$train_auc_roc   = as.numeric(NA)
df_gridsearch_fold$test_auc_roc   = as.numeric(NA)
df_gridsearch_importance = NULL

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

str_timestamp = format(Sys.time(), "%Y%m%d_%H_%M_%S")
timestamped_path = paste0("data/",type_model,"_gridsearch_",str_timestamp)
dir.create(timestamped_path)

save(s, file=paste0(timestamped_path,"/s.RData"))
save(ind_cv, file=paste0(timestamped_path,"/ind_cv.RData"))
write.table(df_gridsearch[,1:(ncol(df_gridsearch)-4)], file=paste0(timestamped_path,"/df_gridsearch_params.csv"), append=FALSE, quote=FALSE, sep=";", row.names=FALSE)

i_gridsearch = 1
# for (i_gridsearch in 1:nrow(df_gridsearch)) {
for (i_gridsearch in 1:3) {

  message(paste0("row ",i_gridsearch," / ",nrow(df_gridsearch)))

  cv_models = list()
  cv_predict = list()
  cv_dfpredict = NULL
  for (i_fold in 1:nfolds) {

    temp_h2o_table = h2o.dfTable[which(ind_cv!=i_fold),]
    cv_models[[i_fold]] = do.call(train_func, c(lazy_eval(train_params), model_id=paste0("MODEL_",type_model,"_numFold_",i_fold,"_gridsearchcombination_",i_gridsearch)))
    h2o.rm(temp_h2o_table)

    cv_predict[[i_fold]] = h2o.predict(cv_models[[i_fold]], h2o.dfTable)

    if (is.null(cv_dfpredict)) {
      cv_dfpredict = as.data.frame(cv_predict[[i_fold]][,ncol(cv_predict[[i_fold]])])
      colnames(cv_dfpredict) = paste0("model_",i_fold)
    } else {
      cv_dfpredict[[paste0("model_",i_fold)]] = as.data.frame(cv_predict[[i_fold]][, ncol(cv_predict[[i_fold]])])[[1]]
    }
  }

  pred_train = numeric(nrow(h2o.dfTable))
  pred_test = numeric(nrow(h2o.dfTable))
  for (i_fold in 1:nfolds) {
    ind_cv_i = which(ind_cv==i_fold)
    pred_train[ind_cv_i] = rowMeans(cv_dfpredict[ind_cv_i, -i_fold])
    pred_test[ind_cv_i] = cv_dfpredict[ind_cv_i, i_fold]
  }

  for (i_fold in 1:nfolds) {
    i_fold_train_pred = cv_dfpredict[[paste0("model_", i_fold)]][ind_cv!=i_fold]
    i_fold_train_target = dfTable[[target]][ind_cv!=i_fold]

    i_fold_test_pred = cv_dfpredict[[paste0("model_", i_fold)]][ind_cv==i_fold]
    i_fold_test_target = dfTable[[target]][ind_cv==i_fold]

    i_gridsearch_fold = which(df_gridsearch_fold$row == i_gridsearch & df_gridsearch_fold$fold == i_fold)

    df_gridsearch_fold$train_auc_gain[i_gridsearch_fold] = rmodel::r.metric.auc.roc(i_fold_train_pred, i_fold_train_target)
    df_gridsearch_fold$test_auc_gain[i_gridsearch_fold] = rmodel::r.metric.auc.roc(i_fold_test_pred, i_fold_test_target)

    df_gridsearch_fold$train_auc_roc[i_gridsearch_fold] = rmodel::r.metric.auc.gain(i_fold_train_pred, i_fold_train_target)
    df_gridsearch_fold$test_auc_roc[i_gridsearch_fold] = rmodel::r.metric.auc.gain(i_fold_test_pred, i_fold_test_target)
  }

  df_gridsearch$train_auc_gain[i_gridsearch] = rmodel::r.metric.auc.gain(pred_train, dfTable[[target]])
  df_gridsearch$test_auc_gain[i_gridsearch] = rmodel::r.metric.auc.gain(pred_test, dfTable[[target]])
  df_gridsearch$train_auc_roc[i_gridsearch] = rmodel::r.metric.auc.roc(pred_train, dfTable[[target]])
  df_gridsearch$test_auc_roc[i_gridsearch] = rmodel::r.metric.auc.roc(pred_test, dfTable[[target]])

  if (type_model == "glm") {
    df_importance = cbind(model=1, cv_models[[1]]@model$coefficients_table)
    for (i_fold in 2:nfolds) {
      df_importance = rbind(df_importance, cbind(model=i_fold, cv_models[[1]]@model$coefficients_table))
    }
    names(df_importance)[names(df_importance)=="names"] = "variable"
    df_importance$scaled_importance = df_importance$standardized_coefficients
    df_importance$scaled_importance[df_importance$variable=="Intercept"] = 0
    df_importance$scaled_importance = abs(df_importance$scaled_importance)
  } else if (type_model == "rf" || type_model == "gbm") {
    df_importance = cbind(model=1, cv_models[[1]]@model$variable_importances)
    for (i_fold in 2:nfolds) {
      df_importance = rbind(df_importance, cbind(model=i_fold, cv_models[[i_fold]]@model$variable_importances))
    }
  } else {
    warning(paste0("The type_model configured is not recognized: ",type_model))
  }
  df_importance_reshaped = reshape2::dcast(df_importance[,c("model", "variable", "scaled_importance")], variable ~ model, value.var="scaled_importance")
  df_importance_reshaped[is.na(df_importance_reshaped)] = 0
  df_importance_reshaped = data.frame(variable=df_importance_reshaped$variable, scaled_importance=rowMeans(df_importance_reshaped[,-1]))
  df_importance_reshaped = df_importance_reshaped[order(df_importance_reshaped$scaled_importance, decreasing = TRUE), ]
  df_importance_reshaped$row = i_gridsearch
  df_importance_reshaped = df_importance_reshaped[,c("row", setdiff(colnames(df_importance_reshaped), c("row")))]

  if (is.null(df_importance)) {
    df_gridsearch_importance = df_importance_reshaped
  } else {
    df_gridsearch_importance = rbind(df_gridsearch_importance, df_importance_reshaped)
  }

  i_gridsearch_fold = which(df_gridsearch_fold$row == i_gridsearch)
  write.table(df_gridsearch_fold[i_gridsearch_fold,], file=paste0(timestamped_path,"/byfold_row_",sprintf("%05d",i_gridsearch),".csv"), append=FALSE, quote=FALSE, sep=";", row.names=FALSE)
  write.table(df_gridsearch[i_gridsearch,], file=paste0(timestamped_path,"/row_",sprintf("%05d",i_gridsearch),".csv"), append=FALSE, quote=FALSE, sep=";", row.names=FALSE)
}

write.table(df_gridsearch_fold, file=paste0(timestamped_path,"/model_",type_model,"_gridsearch_byfold.csv"), append=FALSE, quote=FALSE, sep=";", row.names=FALSE)
write.table(df_gridsearch, file=paste0(timestamped_path,"/model_",type_model,"_gridsearch_summary.csv"), append=FALSE, quote=FALSE, sep=";", row.names=FALSE)

# Close ----
if (close_server) {
  h2o.shutdown(FALSE)
  detach("package:h2o", unload=TRUE)
}