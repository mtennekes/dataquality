library(Hmisc)
library(VIM)
library(tabplotGTK)
library(editrules)
library(igraph)
library(ggplot2)
library(gridBase)
library(data.table)

sapply(list.files("./pkg/R", full.names=TRUE), source)
load("./pkg/data/sam.rda")
load("./pkg/data/reg_t1.rda")
load("./pkg/data/reg_t2.rda")



