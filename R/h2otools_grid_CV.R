# roxygen2::roxygenise()

#' @title h2o.runif
#' @description It seems h2o.runif from h2o package always return the same vector. This function uses runif from R to gives same performance without that bug.
#' @export
h2o.runif <- function(dataset.h2o, min = 0, max = 1, seed=-1, useFile=TRUE) {
  if (seed>=0) set.seed(seed)
  if (min!=0 || max!=1) {
    s = data.frame(rnd=runif(nrow(dataset.h2o), min = min, max = max))
  } else {
    s = data.frame(rnd=runif(nrow(dataset.h2o)))
  }
  if (useFile) {
    tempFile = tempfile(pattern="h2o_runif_", fileext=".csv", tmpdir=tempdir())
    message(paste0("[H2OTOOLS] runif : ", tempFile))
    write.table(s, file=tempFile, append=FALSE, quote=FALSE, sep=";", row.names=FALSE)
    s = h2o::h2o.uploadFile(dataset.h2o@h2o, tempFile)
  } else {
    s = h2o::as.h2o(dataset.h2o@h2o, s)
  }
  return (s)
}

#' @title h2o.getAUC.ROC
#' @export
h2o.getAUC.ROC <- function(model, dataset.h2o, col.pred=3, col.target=tail(h2o::colnames(dataset.h2o), n=1)) {
  pred = h2o::h2o.predict(model, dataset.h2o)
  perf <- h2o::h2o.performance(pred[,col.pred], dataset.h2o[,col.target])
  return(perf@model$auc)
}

#' @title h2o.getAUC.Gain
#' @export
h2o.getAUC.Gain <- function(model, dataset.h2o, ngroups=100, col.pred=3, col.target=tail(h2o::colnames(dataset.h2o), n=1)) {
  pred = h2o::h2o.predict(model, dataset.h2o)
  dfGains = h2o::h2o.gains(dataset.h2o[,col.target], pred[,col.pred], groups=ngroups, percents = TRUE)
  auc_gains = sum(0.5*dfGains[1:(ngroups-1),4]/100+0.5*dfGains[2:ngroups,4]/100)/ngroups
  return (auc_gains)
}

#' @title h2o.getListModels
#' @description Returns an R list of models from different inputs. Input is normally a base h2o grid search.
#' @export
h2o.getListModels <- function(models.grid) {
  if (class(models.grid)=="H2OGBMGrid") {
    listModels = unlist(models.grid@model)
  } else if (class(models.grid)=="H2OSpeeDRFGrid") {
    listModels = unlist(models.grid@model)
  } else if (class(models.grid)=="H2OGBMModel") {
    listModels = list()
    listModels[[1]] = models.grid
  } else if (class(models.grid)=="H2OSpeeDRFModel") {
    listModels = list()
    listModels[[1]] = models.grid
  } else {
    warning(paste0("Class not recognized : ", class(models.grid)))
    listModels = models.grid
  }
  return(listModels)
}

#' @title h2o.getKeyIndex
#' @export
h2o.getKeyIndex <- function(index, prefix="", num_digits=3) {
  paste0(prefix, sprintf(paste0("%0",num_digits,"d"), index))
}

#' @title h2o.cv.rnd.grid.shrinkage
#' @description It does a grid search for shrinkage.
#' \cr Check \code{\link{h2o.cv.rnd.opt.param}} for a parameter optimization that can work with different algorithms and differents parameters to optimize.
#' @seealso \code{\link{h2o.cv.rnd.grid.shrinkage}} \code{\link{h2o.cv.rnd.opt.param}} \code{\link{h2o.cv.rnd.opt.param.get_best}} \code{\link{h2o.ggplot.dfResultsCV}}
#' @export
h2o.cv.rnd.grid.shrinkage <- function(
  dataset.h2o,
  listVars,
  split.perc = 0.6,
  model_algorithm = h2o.gbm,
  nfolds = 10,
  seq_shrinkage = c(0, seq(from=0.002, to=0.008, by=0.002), seq(from=0.01, to=0.2, by=0.05)),
  model_params =
    list(
      shrinkage=0.1,
      n.trees=30,
      interaction.depth=5,
      n.bins=50,
      n.minobsinnode=1000,
      distribution="bernoulli",
      importance=FALSE,
      balance.classes=TRUE),
  gain.calculate = TRUE,
  gain.ngroups = 100,
  col.pred = 3,
  col.target = tail(h2o::colnames(dataset.h2o), n=1)
) {
  if (class(model_params)!="list") stop(paste0("model_params should be a list : ",class(model_params)))

  if (length(intersect(names(model_params), "data")) > 0) {
    message("The param data will not be used, split is generated inside this function.")
    model_params$data <- NULL
  }

  if (length(intersect(names(model_params), "shrinkage")) > 0) {
    message("The param shrinkage will not be used, seq_shrinkage input will be used instead.")
    model_params$shrinkage <- NULL
  }

  if (length(intersect(names(model_params), "importance")) > 0) {
    message("The param importance will not be used.")
    model_params$importance <- NULL
  }

  model_params$x = listVars
  model_params$y = col.target

  listTrain = list()
  listTest = list()
  for (i_fold in 1:nfolds) {
    s = h2otools::h2o.runif(dataset.h2o)
    listTrain[[i_fold]] = h2o.assign(dataset.h2o[s <= split.perc], h2o.getKeyIndex(i_fold, "train_"))
    listTest[[i_fold]] = h2o.assign(dataset.h2o[s > split.perc], h2o.getKeyIndex(i_fold, "test_"))
  }

  nrows = nfolds*length(seq_shrinkage)
  if (gain.calculate) {
    dfResultsCV =
      data.frame(
        ifold = rep(1:nfolds, length(seq_shrinkage)),
        shrinkage = rep(seq_shrinkage, each=nfolds),
        auc.roc.train = numeric(nrows),
        auc.roc.test = numeric(nrows),
        auc.gain.train = numeric(nrows),
        auc.gain.test = numeric(nrows)
      )
  } else {
    dfResultsCV =
      data.frame(
        ifold = rep(1:nfolds, length(seq_shrinkage)),
        shrinkage = rep(seq_shrinkage, each=nfolds),
        auc.roc.train = numeric(nrows),
        auc.roc.test = numeric(nrows)
      )
  }

  for (i_fold in 1:nfolds) {
    model_params_ifold = model_params

    model_params_ifold$data = listTrain[[i_fold]]
    model_params_ifold$shrinkage = seq_shrinkage

    message(paste0("[H2OTOOLS] FOLD : ", i_fold))

    models = do.call(model_algorithm, model_params_ifold)

    for (i_model in 1:length(seq_shrinkage)) {
      irow = (i_model-1)*nfolds + i_fold
      shrinkage = seq_shrinkage[i_model]
      model = models@model[[i_model]]

      pred.train = h2o::h2o.predict(model, listTrain[[i_fold]])
      perf.train <- h2o::h2o.performance(pred.train[,col.pred], listTrain[[i_fold]][,col.target], gains = gain.calculate, groups=gain.ngroups)
      auc_roc_train = perf.train@model$auc
      if (gain.calculate) {
        auc_gains_train = sum(0.5*perf.train@gains[1:(gain.ngroups-1),4]+0.5*perf.train@gains[2:gain.ngroups,4])/gain.ngroups
      }

      pred.test = h2o::h2o.predict(model, listTest[[i_fold]])
      perf.test <- h2o::h2o.performance(pred.test[,col.pred], listTest[[i_fold]][,col.target], gains = gain.calculate, groups=gain.ngroups)
      auc_roc_test = perf.test@model$auc
      if (gain.calculate) {
        auc_gains_test = sum(0.5*perf.test@gains[1:(gain.ngroups-1),4]+0.5*perf.test@gains[2:gain.ngroups,4])/gain.ngroups
      }

      dfResultsCV$auc.roc.train[irow] = auc_roc_train
      dfResultsCV$auc.roc.test[irow] = auc_roc_test
      if (gain.calculate) {
        dfResultsCV$auc.gain.train[irow] = auc_gains_train
        dfResultsCV$auc.gain.test[irow] = auc_gains_test
      }

      h2o::h2o.rm(pred.train@h2o, pred.train@key)
      h2o::h2o.rm(pred.test@h2o, pred.test@key)
      rm(pred.train)
      rm(pred.test)
    }
  }

  dfResultsCV$f_shrinkage = factor(dfResultsCV$shrinkage, levels=sort(unique(dfResultsCV$shrinkage)), ordered=TRUE)

  return(dfResultsCV)
}

#' @title h2o.cv.rnd.opt.param
#' @description Optimizes 1 parameter. Algorithm tries to avoid over-fitting. So, it does not optimizes test results. Instead, it looks for overlapping of train and test metrics.
#' \cr Check \code{\link{h2o.cv.rnd.opt.param.get_best}} for select best parameter from output from this function.
#' \cr Check \code{\link{h2o.ggplot.dfResultsCV}} for ploting output results from this function.
#' @seealso \code{\link{h2o.cv.rnd.grid.shrinkage}} \code{\link{h2o.cv.rnd.opt.param}} \code{\link{h2o.cv.rnd.opt.param.get_best}} \code{\link{h2o.ggplot.dfResultsCV}}
#' @export
h2o.cv.rnd.opt.param <- function(
  dataset.h2o,
  split.perc = 0.6,
  model_algorithm = h2o.gbm,
  nfolds = 10,
  max_it = 10,
  opt.area = 0.5,
  opt.tol = 0.01,
  opt.var = "shrinkage",
  opt.val = 0.1,
  opt.val.min = 0,
  opt.val.max = 1,
  opt.val.is.integer = FALSE,
  opt.criteria = "overfitting",
  opt.val.rang.perc = 0.9,
  model_params =
    list(
      shrinkage=0.1,
      n.trees=30,
      interaction.depth=5,
      n.bins=50,
      n.minobsinnode=1000,
      distribution="bernoulli",
      importance=FALSE,
      balance.classes=TRUE),
  gain.calculate = TRUE,
  gain.ngroups = 100,
  col.pred = 3,
  col.target = tail(h2o::colnames(dataset.h2o), n=1)
) {
  if (class(model_params)!="list") stop(paste0("model_params should be a list : ",class(model_params)))

  validCriterias = c(
      "overfitting",
      "auc.roc.train.median",
      "auc.roc.test.median",
      "auc.gain.train.median",
      "auc.gain.test.median",
      "auc.roc.train.mean",
      "auc.roc.test.mean",
      "auc.gain.train.mean",
      "auc.gain.test.mean")
  if (is.na(match(opt.criteria, validCriterias))) {
    strValidCrierias = paste0("\t\"",validCriterias,"\"",collapse=", \n")
    stop(paste0("Invalid criteria \"",opt.criteria,"\".\nValid criterias :\n",strValidCrierias))
  }

  if (opt.criteria == "overfitting") {
    message(paste0("[H2OTOOLS] Optimizing to avoid excesive overfitting"))
  } else if (opt.criteria == "auc.roc.train.median") {
    message(paste0("[H2OTOOLS] Optimizing auc.roc.train.median"))
  } else if (opt.criteria == "auc.roc.test.median") {
    message(paste0("[H2OTOOLS] Optimizing auc.roc.test.median"))
  } else if (opt.criteria == "auc.gain.train.median") {
    message(paste0("[H2OTOOLS] Optimizing auc.gain.train.median"))
    gain.calculate = TRUE
  } else if (opt.criteria == "auc.gain.test.median") {
    message(paste0("[H2OTOOLS] Optimizing auc.gain.test.median"))
    gain.calculate = TRUE
  } else if (opt.criteria == "auc.roc.train.mean") {
    message(paste0("[H2OTOOLS] Optimizing auc.roc.train.mean"))
  } else if (opt.criteria == "auc.roc.test.mean") {
    message(paste0("[H2OTOOLS] Optimizing auc.roc.test.mean"))
  } else if (opt.criteria == "auc.gain.train.mean") {
    message(paste0("[H2OTOOLS] Optimizing auc.gain.train.mean"))
    gain.calculate = TRUE
  } else if (opt.criteria == "auc.gain.test.mean") {
    message(paste0("[H2OTOOLS] Optimizing auc.gain.test.mean"))
    gain.calculate = TRUE
  } else {
    strValidCrierias = paste0("\t\"",validCriterias,"\"",collapse=", \n")
    stop(paste0("Might be a criteria not implemented yet : \"",opt.criteria,"\".\nValid criterias :\n",strValidCrierias))
  }

  if (length(intersect(names(model_params), "data")) > 0) {
    message("The param data will not be used, split is generated inside this function.")
    model_params$data <- NULL
  }

  if (length(intersect(names(model_params), opt.var)) > 0) {
    message(paste0("The param ",opt.var," will not be used, this variable will be optimized."))
    model_params[[opt.var]] <- NULL
  }

  if (length(intersect(names(model_params), "importance")) > 0) {
    message("The param importance will not be used.")
    model_params$importance <- NULL
  }

  model_params$x = listVars
  model_params$y = col.target

  listTrain = list()
  listTest = list()
  for (i_fold in 1:nfolds) {
    s = h2otools::h2o.runif(dataset.h2o)
    listTrain[[i_fold]] = h2o.assign(dataset.h2o[s <= split.perc], h2o.getKeyIndex(i_fold, "train_"))
    listTest[[i_fold]] = h2o.assign(dataset.h2o[s > split.perc], h2o.getKeyIndex(i_fold, "test_"))
  }

  if (gain.calculate) {
    dfResultsCV =
      data.frame(
        it = numeric(0),
        ifold = numeric(0),
        opt.var = numeric(0),
        auc.roc.train = numeric(0),
        auc.roc.test = numeric(0),
        auc.gain.train = numeric(0),
        auc.gain.test = numeric(0),

        auc.roc.train.median = numeric(0),
        auc.roc.train.mean = numeric(0),
        auc.roc.test.median = numeric(0),
        auc.roc.test.mean = numeric(0),

        auc.gain.train.median = numeric(0),
        auc.gain.train.mean = numeric(0),
        auc.gain.test.median = numeric(0),
        auc.gain.test.mean = numeric(0)
      )
  } else {
    dfResultsCV =
      data.frame(
        it = numeric(0),
        ifold = numeric(0),
        opt.var = numeric(0),
        auc.roc.train = numeric(0),
        auc.roc.test = numeric(0),

        auc.roc.train.median = numeric(0),
        auc.roc.train.mean = numeric(0),
        auc.roc.test.median = numeric(0),
        auc.roc.test.mean = numeric(0)
      )
  }

  opt.val.next = opt.val
  it = 1
  opt.reached_convergence = FALSE
  repeat{
    for (i_fold in 1:nfolds) {
      model_params_ifold = model_params

      model_params_ifold$data = listTrain[[i_fold]]
      model_params_ifold[[opt.var]] = opt.val.next

      message(paste0("[H2OTOOLS] it = ",it," | FOLD = ", i_fold))
      message(paste0("[H2OTOOLS] ", opt.var, " = ",opt.val.next))

      models = do.call(model_algorithm, model_params_ifold)
      models = h2o.getListModels(models)

      irow = (it-1)*nfolds + i_fold
      dfResultsCV[irow, ] = 0
      dfResultsCV$it[irow] = it
      dfResultsCV$ifold[irow] = i_fold
      dfResultsCV$opt.var[irow] = opt.val.next

      model = models[[1]]

      pred.train = h2o::h2o.predict(model, listTrain[[i_fold]])
      perf.train <- h2o::h2o.performance(pred.train[,col.pred], listTrain[[i_fold]][,col.target], gains = gain.calculate, groups=gain.ngroups)
      auc_roc_train = perf.train@model$auc
      if (gain.calculate) {
        auc_gains_train = sum(0.5*perf.train@gains[1:(gain.ngroups-1),4]+0.5*perf.train@gains[2:gain.ngroups,4])/gain.ngroups
      }

      pred.test = h2o::h2o.predict(model, listTest[[i_fold]])
      perf.test <- h2o::h2o.performance(pred.test[,col.pred], listTest[[i_fold]][,col.target], gains = gain.calculate, groups=gain.ngroups)
      auc_roc_test = perf.test@model$auc
      if (gain.calculate) {
        auc_gains_test = sum(0.5*perf.test@gains[1:(gain.ngroups-1),4]+0.5*perf.test@gains[2:gain.ngroups,4])/gain.ngroups
      }

      dfResultsCV$auc.roc.train[irow] = auc_roc_train
      dfResultsCV$auc.roc.test[irow] = auc_roc_test
      if (gain.calculate) {
        dfResultsCV$auc.gain.train[irow] = auc_gains_train
        dfResultsCV$auc.gain.test[irow] = auc_gains_test
      }

      h2o::h2o.rm(pred.train@h2o, pred.train@key)
      h2o::h2o.rm(pred.test@h2o, pred.test@key)
      rm(pred.train)
      rm(pred.test)
    }

    # Metricas agregadas
    ind = (1+(it-1)*nfolds):((it)*nfolds)
    dfResultsCV$auc.roc.train.median[ind] = quantile(dfResultsCV$auc.roc.train[ind], probs=0.5)
    dfResultsCV$auc.roc.train.mean[ind] = mean(dfResultsCV$auc.roc.train[ind])
    dfResultsCV$auc.roc.test.median[ind] = quantile(dfResultsCV$auc.roc.test[ind], probs=0.5)
    dfResultsCV$auc.roc.test.mean[ind] = mean(dfResultsCV$auc.roc.test[ind])
    if (gain.calculate) {
      dfResultsCV$auc.gain.train.median[ind] = quantile(dfResultsCV$auc.gain.train[ind], probs=0.5)
      dfResultsCV$auc.gain.train.mean[ind] = mean(dfResultsCV$auc.gain.train[ind])
      dfResultsCV$auc.gain.test.median[ind] = quantile(dfResultsCV$auc.gain.test[ind], probs=0.5)
      dfResultsCV$auc.gain.test.mean[ind] = mean(dfResultsCV$auc.gain.test[ind])
    }

    # Criterio de optimizacion
    if (opt.criteria == "overfitting") {
      ind = (1+nrow(dfResultsCV)-nfolds):nrow(dfResultsCV)
      q_train = quantile(dfResultsCV$auc.roc.train[ind], probs=c(0.25,0.75))
      q_test = quantile(dfResultsCV$auc.roc.test[ind], probs=c(0.25,0.75))
      if (q_train[1]>q_test[2]) {
        q_area = 0
      } else if (q_train[2]<q_test[1]) {
        q_area = 1
      } else {
        q_area = abs(min(q_train[2], q_test[2])-max(q_train[1], q_test[1])) / min(abs(q_train[2]-q_train[1]), abs(q_test[2]-q_test[1]))
      }

      if (q_area < opt.area) {
        opt.val.max = opt.val.next
        opt.val.next = 0.5*opt.val.min + 0.5*opt.val.max
      } else {
        opt.val.min = opt.val.next
        opt.val.next = 0.5*opt.val.min + 0.5*opt.val.max
      }

      if (abs(opt.area - q_area) < opt.tol) {
        message(paste0("[H2OTOOLS] Convergence reached : abs(opt.area - q_area) = ", abs(opt.area - q_area), " <= opt.tol = ",opt.tol))
        opt.reached_convergence = TRUE
      }
      message(paste0("[H2OTOOLS] opt.area = ",opt.area))
      message(paste0("[H2OTOOLS] q_area = ",q_area))
    } else {
      if (nrow(dfResultsCV)<3*nfolds) {
        if (it==1) {
          opt.val.next = (opt.val.rang.perc)*opt.val.min + (1-opt.val.rang.perc)*opt.val
          opt.val.min = opt.val.next
        } else {
          opt.val.next = (1-opt.val.rang.perc)*opt.val + (opt.val.rang.perc)*opt.val.max
          opt.val.max = opt.val.next
        }
      } else {
        ind = 1+(1:it-1)*nfolds

        opt.criteria.sorted = sort(dfResultsCV[[opt.criteria]][ind], decreasing = TRUE, index.return = TRUE)

        opt.metric.best1 = opt.criteria.sorted$x[1]
        opt.metric.best2 = opt.criteria.sorted$x[2]
        opt.val.best1 = dfResultsCV[["opt.var"]][ind][opt.criteria.sorted$ix][1]
        opt.val.best2 = dfResultsCV[["opt.var"]][ind][opt.criteria.sorted$ix][2]

        opt.val.max = max(opt.val.best1, opt.val.best2)
        opt.val.min = min(opt.val.best1, opt.val.best2)
        opt.val.next = 0.5*opt.val.min + 0.5*opt.val.max

        if (abs(opt.val.best1 - opt.val.best2) < opt.tol) {
          message(paste0("[H2OTOOLS] Convergence reached : maximized", opt.criteria))
          opt.reached_convergence = TRUE
        }
        message(paste0("[H2OTOOLS] (best2)  \t",opt.var," = ",opt.val.best2," \t",opt.criteria," = ",opt.metric.best2))
        message(paste0("[H2OTOOLS] (best1)  \t",opt.var," = ",opt.val.best1," \t",opt.criteria," = ",opt.metric.best1))
      }
    }

    # Redondeo en caso de parametro entero
    if (opt.val.is.integer) {
      opt.val.next = round(opt.val.next)
    }

    # Criterios de finalizacion
    if (opt.reached_convergence) {
      message(paste0("[H2OTOOLS] Convergence reached."))
      break
    }
    if (it>=max_it) {
      message(paste0("[H2OTOOLS] Max iterations reached : ", it))
      break
    }
    if (!is.na(match(opt.val.next, unique(dfResultsCV[["opt.var"]]))[1])) {
      message(paste0("[H2OTOOLS] No convergence reached but there are no more values to try."))
      break
    }
    it = it + 1
  }

  message(paste0("[H2OTOOLS] Param : ", opt.var))
  message(paste0("[H2OTOOLS] Value ",dfResultsCV$it[1+nfolds*((1:min(nrow(dfResultsCV)/nfolds,max_it)) -1)]," : ", dfResultsCV$opt.var[1+nfolds*((1:min(nrow(dfResultsCV)/nfolds,max_it)) -1)], collapse="\n"))

  return(dfResultsCV)
}

#' @title h2o.cv.rnd.opt.param.get_best
#' @description Selects best optimized parameter. By default selects best median on test set.
#' \cr Input is the output data.frame from \code{\link{h2o.cv.rnd.opt.param}}.
#' @seealso \code{\link{h2o.cv.rnd.grid.shrinkage}} \code{\link{h2o.cv.rnd.opt.param}} \code{\link{h2o.cv.rnd.opt.param.get_best}} \code{\link{h2o.ggplot.dfResultsCV}}
#' @export
h2o.cv.rnd.opt.param.get_best <- function(
  dfResultsCV,
  metric = "auc.roc.test.median"
) {
  var_metrics = dfResultsCV[[metric]]
  print(var_metrics)
  print(max(var_metrics))
  print(which(var_metrics == max(var_metrics)))
  best_opt.var = dfResultsCV$opt.var[which(var_metrics == max(var_metrics))][1]
  return(best_opt.var)
}

#' @title h2o.ggplot.dfResultsCV
#' @description Plots boxplots of some metrics. By default AUC of ROC Curve for train and test.
#' \cr Input is the output data.frame from \code{\link{h2o.cv.rnd.opt.param}}.
#' @seealso \code{\link{h2o.cv.rnd.grid.shrinkage}} \code{\link{h2o.cv.rnd.opt.param}} \code{\link{h2o.cv.rnd.opt.param.get_best}} \code{\link{h2o.ggplot.dfResultsCV}}
#' @export
h2o.ggplot.dfResultsCV <- function(
  dfResultsCV,
  list.vars = c("auc.roc.test", "auc.roc.train"),
  title = NULL,
  ylab = "AUC",
  xlab = NULL,
  guides = FALSE,
  horizontal = FALSE,
  useFactor = TRUE
) {
  df <- dfResultsCV

  if (useFactor) {
    x_var = "f_opt.var"
    df$f_opt.var = factor(df$opt.var, levels=sort(unique(df$opt.var)), ordered=TRUE)
  } else {
    x_var = "opt.var"
  }

  p <- ggplot(df, environment = environment())

  for (var in list.vars) {
    p <- p + geom_boxplot(aes_string(x=x_var, y=var, fill=x_var))
  }

  p <- p +
    ggtitle(title) +
    ylab(ylab) +
    xlab(xlab)

  if (!guides) p <- p + guides(fill=FALSE)

  if (horizontal) p <- p + coord_flip()

  return(p)
}