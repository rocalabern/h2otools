library(data.table)
load("data/df_all_features.RData")

df_table = df_all_features

save(df_table, file = "data/df_table.RData")
