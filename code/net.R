## forward regression
library(Matrix)
library(parallel)
library(glmnet)


source("code/zmvy.R")

X <- readRDS("data/x.rds")
rownames(X) <- NULL
X <- lapply(colnames(X),function(j)[

F <- X
F@x <- F@x/M[F@i+1]

FMV <- cBind(F,M,V)
fit <- cv.glmnet(FMV,lY,foldid=foldid)

saveRDS(fit,"results/net.rds",compress=FALSE)


