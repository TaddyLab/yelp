## multinomial inverse regression for review text
## written to be run via slurm and parallel (see cv.sbatch)

source("code/setup.R")
source("code/drawoos.R")

cat("looping over folds: \n")
for(i in 1:nfold){
	lo <- rando[(chunks[i]+1):chunks[i+1]]
	fit <- dmr(counts=x[-lo,], 
			covars=V[-lo,], 
			mu=mu[-lo], 
			cl=cl, 
			nlambda=20,
			gamma=1,
			verb=TRUE,
			standardize=FALSE,
			family="gaussian",
			zeta=0.05)
	beta <- coef(fit)
	z <- tcrossprod(x,beta[ZKEEP,])
	z@x <- z@x/(exp(mu[z@i+1])-1)
	saveRDS(z, 
		file=sprintf("data/cvout/fold%03dz%03d.rds",i,k), 
		compress=FALSE)
	cat(sprintf(" %d \n",i))
}

cat("done\n")
stopCluster(cl) 
cat("cluster stopped\n\n")

cat("warnings:\n")
warnings()

cat("done\n")



