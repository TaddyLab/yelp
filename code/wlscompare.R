## multinomial inverse regression for review text
## written to be run via slurm and parallel (see irfit.sbatch)

source("code/setup.R")

B <- read.table("results/B.txt", 
  sep="|", header=FALSE,
  quote="", comment="",
  col.names=c("i","j","x"))
B$i <- factor(B$i,levels=c("intercept",colnames(V)))
B <- as.matrix(sparseMatrix(i=as.numeric(B$i),
        j=as.numeric(B$j), x=B$x,
        dimnames=list(levels(B$i),levels(B$j))))

for(ebuff in 0:-7){
	system.time({
		fit <- dmr(cl=cl, covars=V, counts=x,  mu=mu,  
			nlambda=20,
			gamma=1,
			verb=2,
			#maxit=500,
			lambda.min.ratio=1e-3,
			family="gaussian",
			buff=exp(ebuff),
			standardize=FALSE)})

	beta <- coef(fit)
	r <- cor(c(B[,colnames(beta)]),c(as.matrix(beta)))
	sto <- sprintf("%d|%g|%g\n",k,ebuff,r)
	cat(sto,
		file="results/wlscompare.txt",append=TRUE)
	cat(sto)
}

stopCluster(cl) 



