library(Boruta)
library(readxl)
library(tidyverse) 

gse202203 <- read.csv("GSE202203_withOS.csv")
new_genes <- read.csv("p(nooverlap).csv", sep=";", header=TRUE)

new_genes_list <- na.omit(c(t(new_genes)))
new_genes_clean <- unique(new_genes_list)
new_genes_clean <- c(new_genes_clean, 'OS')

genes_not_in_dataset <- setdiff(new_genes_clean, colnames(gse202203))
new_genes_clean <- setdiff(new_genes_clean, genes_not_in_dataset)

gse202203_new <- gse202203[, new_genes_clean]

write.csv(gse202203_new, "GSE202203_New_df.csv", row.names=TRUE)

X <- gse202203_new[, !(names(gse202203_new) %in% c("OS"))]
y <- gse202203_new$OS

y <- as.factor(y) # Ensure the target variable is a factor

set.seed(500)

boruta_output <- Boruta(X, y, doTrace=2, maxRuns=15)

importance_score <- attStats(boruta_output)
write.csv(importance_score, "importance_score_gse202203.csv", row.names=TRUE)

confirmed <- getSelectedAttributes(boruta_output, withTentative = FALSE)
tentative <- getSelectedAttributes(boruta_output, withTentative = TRUE)
