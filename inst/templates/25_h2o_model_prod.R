if (!exists("df_table")) load("data/df_table.RData")
library(lazyeval)
library(data.table)
library(caret)
library(h2o)
library(rplot)
do.format.AmmountCents <- function (x) {paste0(format(round(x/100, 2), nsmall=2, big.mark=",", decimal.mark=".", scientific=FALSE),"€")}
do.format.Perc <- function (x) {paste0(format(round(100*x, 2), big.mark = ",", decimal.mark = ".", scientific = FALSE),"%")}

listVars = c("var1", "var2")
dfTable = df_table

h2oServer = h2o.init(nthreads = -1)

# params ----
close_server = FALSE
# type_model = "glm"
# type_model = "rf"
type_model = "gbm"
recalc_listVars = FALSE
bTargetAsFactor = TRUE
bFeaturesFactorAsFactor = TRUE
bFeaturesCharacterAsFactor = TRUE
bFeaturesLogicalAsFactor = TRUE

target = "target"
list_key_vars = "ref_id"
if (!exists("listVars") || recalc_listVars) {
  warn(paste0("listVars auto-calculated: ",length(listVars)," nvars."))
  listVars = setdiff(colnames(dfTable), c(list_key_vars, target))
} else {
  message(paste0("listVars reused: ",length(listVars)," nvars."))
}

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


# Train ----
h2o.dfTable = as.h2o(dfTable)
if (bTargetAsFactor) h2o.dfTable[, target] = h2o::as.factor(h2o.dfTable[, target])
if (bFeaturesFactorAsFactor) for (var in colnames(dfTable)[lapply(dfTable, class) %in% c("factor")]) h2o.dfTable[, var] = h2o::as.factor(h2o.dfTable[, var])
if (bFeaturesCharacterAsFactor) for (var in colnames(dfTable)[lapply(dfTable, class) %in% c("character")]) h2o.dfTable[, var] = h2o::as.factor(h2o.dfTable[, var])
if (bFeaturesLogicalAsFactor) for (var in colnames(dfTable)[lapply(dfTable, class) %in% c("logical")]) h2o.dfTable[, var] = h2o::as.factor(h2o.dfTable[, var])
# summary(h2o.dfTable)

temp_h2o_table = h2o.dfTable

model <- do.call(train_func, lazy_eval(train_params))

h2o.pred = h2o.predict(model, h2o.dfTable)

r_pred = as.vector(h2o.pred[,ncol(h2o.pred)])

r.plot.lift(r_pred, dfTable[[target]], main="Lift Curve")
# r.plot.wgain(r_pred, dfTable[[target]], dfTable$amount_cents)
r.plot.gain(r_pred, dfTable[[target]])
r.plot.roc(r_pred, dfTable[[target]], fill=FALSE)

# Create folders ----
dir.create("prod")
dir.create("prod/h2o_model")
dir.create("prod/pojo_model")
dir.create("prod/mojo_model")

# Model ----
sink("prod/log.txt")
model
model@parameters
sink()

# Export model ----
if (type_model == "glm") {
  write.table(
    data.frame(
      variable = names(model@model$coefficients),
      coefficient = model@model$coefficients,
      stringsAsFactors = FALSE
      ),
    "prod/glm_coefficients.csv",
    quote = FALSE, row.names = FALSE, sep=";",
    fileEncoding = "UTF-8")
}
h2o.saveModel(model, "prod/h2o_model/")
h2o.download_pojo(model, "prod/pojo_model")
h2o.download_mojo(model, "prod/mojo_model")

# cutoff & dictionary ----
list_cutoff = seq(0.85,1.0,by=0.01)
n_sim = length(list_cutoff)
dfResults = data.frame(
  it = numeric(n_sim),
  cutoff = numeric(n_sim),
  i = numeric(n_sim),

  pred0_real0 = numeric(n_sim),
  pred0_real1 = numeric(n_sim),
  pred1_real0 = numeric(n_sim),
  pred1_real1 = numeric(n_sim),

  acceptance = numeric(n_sim),
  opportunity = numeric(n_sim),
  DR_acc = numeric(n_sim),
  DR_rej = numeric(n_sim),
  DR_total = numeric(n_sim),

  good_eur_perc = numeric(n_sim),
  bad_eur_perc = numeric(n_sim),
  acceptance_eur_perc = numeric(n_sim),
  opportunity_eur_perc = numeric(n_sim),
  lost_accepted_eur_perc = numeric(n_sim),
  lost_rejected_eur_perc = numeric(n_sim),

  good_eur = numeric(n_sim),
  bad_eur = numeric(n_sim),
  acceptance_eur = numeric(n_sim),
  opportunity_eur = numeric(n_sim),
  total_eur = numeric(n_sim),
  lost_accepted_eur = numeric(n_sim),
  lost_rejected_eur = numeric(n_sim),
  lost_total_eur = numeric(n_sim),

  DR_acc_eur = numeric(n_sim),
  DR_rej_eur = numeric(n_sim),
  DR_total_eur = numeric(n_sim),

  stringsAsFactors = FALSE
  )
row = 1
nquantiles = 100
  varSplits = quantile(r_pred, probs = seq(0, 1, by = 1/nquantiles), na.rm = TRUE)
  if (any(duplicated(varSplits))) warning("r.quantile_normalize : Duplicated splits values using quantiles.")
  varSplits = unique(varSplits)
  nquantiles = length(varSplits) - 1
  dfTemp = dfTable[,c("target"),with=FALSE]
  dfTemp$r_pred_class = findInterval(r_pred, varSplits, rightmost.closed = TRUE, all.inside = FALSE)
  dfDict = dfTemp[, .(DR=mean(target)), by=r_pred_class]
  dfDict = dfDict[order(dfDict$r_pred_class)]
  dfDict$varSplits_inf = varSplits[1:(length(varSplits)-1)]
  dfDict$varSplits_sup = varSplits[2:(length(varSplits))]
for (cutoff in list_cutoff) {
  dfResults$it[row] = row
  dfResults$cutoff[row] = cutoff
  dfResults$i[row] = 1

  r_pred_norm = dfDict$r_pred_class[match(findInterval(r_pred, varSplits, rightmost.closed = TRUE, all.inside = FALSE), dfDict$r_pred_class)]/100
  ind_rejected = which(r_pred_norm > cutoff)
  ind_accepted = which(r_pred_norm <= cutoff)
  ind_rejected_0 = which(r_pred_norm > cutoff & dfTable[[target]] == 0)
  ind_rejected_1 = which(r_pred_norm > cutoff & dfTable[[target]] == 1)
  ind_accepted_0 = which(r_pred_norm <= cutoff & dfTable[[target]] == 0)
  ind_accepted_1 = which(r_pred_norm <= cutoff & dfTable[[target]] == 1)
  ind_0 = which(dfTable[[target]] == 0)
  ind_1 = which(dfTable[[target]] == 1)
  cat(paste0("\nrejected    = ",length(ind_rejected)," / ",nrow(dfTable)))
  print(table(ifelse(r_pred_norm > cutoff,1,0), dfTable[[target]]))
  cat(paste0("\n\tacceptance  = ",do.format.Perc(length(ind_accepted) / nrow(dfTable))))
  cat(paste0("\n\topportunity = ",do.format.Perc(length(ind_rejected_0) / nrow(dfTable))))
  cat(paste0("\n"))
  cat(paste0("\n\tDR (acc) = ",do.format.Perc(mean(dfTable[[target]][ind_accepted]))))
  cat(paste0("\n\tDR (rej) = ",do.format.Perc(ifelse(length(ind_rejected)==0, 0, mean(dfTable[[target]][ind_rejected])))))
  cat(paste0("\n\tDR       = ",do.format.Perc(mean(dfTable[[target]]))))
  cat(paste0("\n"))
  cat(paste0("\n\tgood          = ",do.format.Perc(sum(dfTable$amount_cents[ind_0]) / sum(dfTable$amount_cents)),
             " \t | ", do.format.AmmountCents(sum(dfTable$amount_cents[ind_0]))," / ",do.format.AmmountCents(sum(dfTable$amount_cents))))
  cat(paste0("\n\tbad           = ",do.format.Perc(sum(dfTable$amount_cents[ind_1]) / sum(dfTable$amount_cents)),
             " \t | ", do.format.AmmountCents(sum(dfTable$amount_cents[ind_1]))," / ",do.format.AmmountCents(sum(dfTable$amount_cents))))
  cat(paste0("\n\tacceptance    = ",do.format.Perc(sum(dfTable$amount_cents[ind_accepted]) / sum(dfTable$amount_cents)),
             " \t | ", do.format.AmmountCents(sum(dfTable$amount_cents[ind_accepted]))," / ",do.format.AmmountCents(sum(dfTable$amount_cents))))
  cat(paste0("\n\topportunity   = ",do.format.Perc(sum(dfTable$amount_cents[ind_rejected_0]) / sum(dfTable$amount_cents)),
             " \t | ", do.format.AmmountCents(sum(dfTable$amount_cents[ind_rejected_0]))," / ",do.format.AmmountCents(sum(dfTable$amount_cents))))
  cat(paste0("\n\tlost_accepted = ",do.format.Perc(sum(dfTable$amount_cents[ind_accepted_1]) / sum(dfTable$amount_cents[ind_1])),
             " \t | ", do.format.AmmountCents(sum(dfTable$amount_cents[ind_accepted_1]))," / ",do.format.AmmountCents(sum(dfTable$amount_cents[ind_1]))))
  cat(paste0("\n\tlost_rejected = ",do.format.Perc(sum(dfTable$amount_cents[ind_rejected_1]) / sum(dfTable$amount_cents[ind_1])),
             " \t | ", do.format.AmmountCents(sum(dfTable$amount_cents[ind_rejected_1]))," / ",do.format.AmmountCents(sum(dfTable$amount_cents[ind_1]))))
  cat(paste0("\n"))
  cat(paste0("\n\tDR (acc) = ",do.format.Perc(sum(dfTable$amount_cents[ind_accepted_1]) / sum(dfTable$amount_cents[ind_accepted]))))
  cat(paste0("\n\tDR (rej) = ",do.format.Perc(ifelse(length(ind_rejected)==0, 0, sum(dfTable$amount_cents[ind_rejected_1]) / sum(dfTable$amount_cents[ind_rejected])))))
  cat(paste0("\n\tDR       = ",do.format.Perc(sum(dfTable$amount_cents[ind_1]) / sum(dfTable$amount_cents))))
  cat("\n")

  dfResults$pred0_real0[row] = length(ind_accepted_0)
  dfResults$pred0_real1[row] = length(ind_accepted_1)
  dfResults$pred1_real0[row] = length(ind_rejected_0)
  dfResults$pred1_real1[row] = length(ind_rejected_1)
  dfResults$acceptance[row] = length(ind_accepted) / nrow(dfTable)
  dfResults$opportunity[row] = length(ind_rejected_0) / nrow(dfTable)
  dfResults$DR_acc[row] = mean(dfTable[[target]][ind_accepted])
  dfResults$DR_rej[row] = ifelse(length(ind_rejected)==0, 0, mean(dfTable[[target]][ind_rejected]))
  dfResults$DR_total[row] = mean(dfTable[[target]])
  dfResults$good_eur_perc[row] = sum(dfTable$amount_cents[ind_0]) / sum(dfTable$amount_cents)
  dfResults$bad_eur_perc[row] = sum(dfTable$amount_cents[ind_1]) / sum(dfTable$amount_cents)
  dfResults$acceptance_eur_perc[row] = sum(dfTable$amount_cents[ind_accepted]) / sum(dfTable$amount_cents)
  dfResults$opportunity_eur_perc[row] = sum(dfTable$amount_cents[ind_rejected_0]) / sum(dfTable$amount_cents)
  dfResults$lost_accepted_eur_perc[row] = sum(dfTable$amount_cents[ind_accepted_1]) / sum(dfTable$amount_cents[ind_1])
  dfResults$lost_rejected_eur_perc[row] = sum(dfTable$amount_cents[ind_rejected_1]) / sum(dfTable$amount_cents[ind_1])
  dfResults$good_eur[row] = sum(dfTable$amount_cents[ind_0])
  dfResults$bad_eur[row] = sum(dfTable$amount_cents[ind_1])
  dfResults$acceptance_eur[row] = sum(dfTable$amount_cents[ind_accepted])
  dfResults$opportunity_eur[row] = sum(dfTable$amount_cents[ind_rejected_0])
  dfResults$total_eur[row] = sum(dfTable$amount_cents)
  dfResults$lost_accepted_eur[row] = sum(dfTable$amount_cents[ind_accepted_1])
  dfResults$lost_rejected_eur[row] = sum(dfTable$amount_cents[ind_rejected_1])
  dfResults$lost_total_eur[row] = sum(dfTable$amount_cents[ind_1])
  dfResults$DR_acc_eur[row] = sum(dfTable$amount_cents[ind_accepted_1]) / sum(dfTable$amount_cents[ind_accepted])
  dfResults$DR_rej_eur[row] = ifelse(length(ind_rejected)==0, 0, sum(dfTable$amount_cents[ind_rejected_1]) / sum(dfTable$amount_cents[ind_rejected]))
  dfResults$DR_total_eur[row] = sum(dfTable$amount_cents[ind_1]) / sum(dfTable$amount_cents)
  row = row + 1
}
write.table(
  dfDict,
  "prod/cutoff_dictionary.csv",
  quote = FALSE, row.names = FALSE, sep=";",
  fileEncoding = "UTF-8")

# # Export Input/Output ETL ----
write.table(
  dfTable[,list_key_vars,with=FALSE],
  "prod/validate_etl_input.csv",
  quote = FALSE, row.names = FALSE, sep=";",
  fileEncoding = "UTF-8")

write.table(
  dfTable[,listVars,with=FALSE],
  "prod/validate_etl_output.csv",
  quote = FALSE, row.names = FALSE, sep=";",
  fileEncoding = "UTF-8")

# Export Input/Output Scoring ----
dfScoring = data.table::copy(dfTable[,list_key_vars,with=FALSE])
dfScoring$scoring = r_pred
dfScoring$scoring_norm = r_pred_norm
dfScoring$target = dfTable[[target]]
write.table(
  dfScoring,
  "prod/validate_scoring.csv",
  quote = FALSE, row.names = FALSE, sep=";",
  fileEncoding = "UTF-8")

# Export Unit Test ----
dfUnitTest = data.table::copy(dfTable[,c(list_key_vars, listVars),with=FALSE])
dfUnitTest$scoring = r_pred
dfUnitTest$scoring_norm = r_pred_norm
dfUnitTest$target = dfTable[[target]]
write.table(
  dfUnitTest,
  "prod/unit_test.csv",
  quote = FALSE, row.names = FALSE, sep=";",
  fileEncoding = "UTF-8")

# Analysis results ----
ggplot(dfResults) +
  geom_line(aes(x=cutoff, y=0.25*acceptance_eur_perc), col=rgb(0,0,1,0.5)) +
  labs(title = "Acceptance") + xlab("cutoff") + ylab("acceptance")

ggplot(dfResults) +
  geom_line(aes(x=cutoff, y=opportunity_eur_perc), col=rgb(0,0,1,0.5)) +
  labs(title = "Opportunity rate left") + xlab("cutoff") + ylab("opportunity")

ggplot(dfResults) +
  geom_hline(data=data.frame(pos=0.14), aes(yintercept=pos), col=rgb(1,0,0,0.5), linetype=2) +
  geom_line(aes(x=cutoff, y=(0.125/0.09018706)*DR_total_eur), col=rgb(0,0,0,0.5)) +
  geom_line(aes(x=cutoff, y=(0.125/0.09018706)*DR_acc_eur), col=rgb(0,0,1,0.5)) +
  labs(title = "DR in EUR (adjusted)") + xlab("cutoff") + ylab("dr_eur")

ggplot(dfResults) +
  geom_line(aes(x=cutoff, y=DR_total_eur), col=rgb(0,0,0,0.5)) +
  geom_line(aes(x=cutoff, y=DR_acc_eur), col=rgb(0,0,1,0.5)) +
  labs(title = "DR in EUR") + xlab("cutoff") + ylab("dr_eur")

ggplot(dfResults) +
  geom_line(aes(x=cutoff, y=DR_total), col=rgb(0,0,0,0.5)) +
  geom_line(aes(x=cutoff, y=DR_acc), col=rgb(0,0,1,0.5)) +
  labs(title = "DR in #") + xlab("cutoff") + ylab("dr")

# Close ----
if (close_server) {
  h2o.shutdown(FALSE)
  detach("package:h2o", unload=TRUE)
}
