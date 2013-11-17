## parallel random forest prediction

library(parallel) 
library(randomForest)
library(Matrix)

system.time(source("code/zmvy.R"))
Z <- Z[,yvar]
colnames(Z) <- paste("z",colnames(Z),sep=".")
YX <- as.data.frame(as.matrix(cBind(lY,Z,M,V)))

source("code/drawoos.R")

cat("\nstart cluster\n")
cl <- makeCluster(nc <- 16)
print(cl)

fitrf <- function(yx){
    require(randomForest)
    rfi <- randomForest(x=as.matrix(yx[,-1]),y=yx[,1],
      ntree=64, mtry=40, proximity=FALSE, nodesize=10)
    rfi$y <- rfi$predicted <- NULL
    return(rfi)
}

cat("\nsplit:\n")
print(system.time(
  yxs <- split(YX,rep(1:nc,ceiling(nobs/nc))[1:nobs])))
cat("fit:\n")
print(system.time(
  rf <- parLapply(cl, yxs, fitrf)))
cat("combine:\n")
print(system.time(
  rf <- do.call(combine,rf)))
cat("save:\n")
print(system.time(
  saveRDS(rf,"results/rFfit.rds",compress=FALSE)))

oos <- matrix(nrow=nfold, ncol=3, 
  dimnames=list(1:nfold,c("mse","var","r2")))
for(k in 1:nfold){
  ## build Z
  Z <- buildz(k)
  Z <- Z[,yvar]
  colnames(Z) <- paste("z",colnames(Z),sep=".")
  YX <- as.data.frame(as.matrix(cBind(lY,Z,M,V)))

  lo <- rando[(chunks[k]+1):chunks[k+1]]
  yxs <- split(YX[-lo,],
    rep(1:nc,ceiling(nobs/nc))[1:nobs])

  rf <- do.call(combine,
      parLapply(cl, yxs, fitrf))
  yhat <- predict(rf, YX[lo,-1])
  oos[k,'mse'] <- mean( (YX[lo,1]-yhat)^2 )
  oos[k,'var'] <- mean( (YX[lo,1]-mean(YX[lo,1]))^2 )
  oos[k,'r2'] <- 1 - oos[k,'mse']/oos[k,'var']
  cat(sprintf("r2[%d] = %g\n",k,oos[k,'r2']))
  print(oos[k,])
}
cat("done; write to table.\n")
write.table(oos,file="results/rFoos.txt",sep="|",row.names=FALSE)

stopCluster(cl)

