## forward regression
library(Matrix)
library(parallel)
library(gamlr)

system.time(source("code/zmvy.R"))
Z <- Z[,yvar]
colnames(Z) <- paste("z",colnames(Z),sep=".")
X <- cBind(Z,M,V)

source("code/drawoos.R")

## fit it
system.time(
	lif <- gamlr(X,lY, doxx=TRUE, 
			lambda.min.ratio=1e-4,gamma=1,verb=TRUE))
liB <- coef(lif)

save(liB,lif,
	file="results/fwdfit.rda",compress=FALSE)

loopit <- function(k){
  Z <- buildz(k)
  Z <- Z[,yvar]
  colnames(Z) <- paste("z",colnames(Z),sep=".")
  X <- cBind(Z,M,V)

  lo <- rando[(chunks[k]+1):chunks[k+1]]
  oos <- list()
	oos$var <- mean( (lY[lo]-mean(lY[lo]))^2 )	
	lif <- gamlr(X[-lo,],lY[-lo], doxx=TRUE, 
			lambda.min.ratio=1e-4,gamma=1)
	yhat <- predict(lif,X[lo,])
  oos$mse <- mean( (lY[lo]-yhat)^2 )
  oos$r2 <- 1 - oos$mse/oos$var

  cat(sprintf("r2[%d] = %g\n",k,oos$r2))
  unlist(oos)
}

cat("start OOS loop\n")
oos <- mclapply(1:nfold, loopit, mc.cores=16)
oos <- do.call(rbind,oos)
cat("done; write to table.\n")
write.table(oos, file="results/fwdoos.txt",
 	sep="|",row.names=FALSE)


