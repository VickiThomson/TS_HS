install.packages("devtools")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Rsamtools", version = "3.9")
library(Rsamtools)

install.packages("geomorph")
library(geomorph)
install.packages("devtools")
devtools::install_github("cardiomoon/ggiraphExtra")

