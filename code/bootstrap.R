## forward regression
args <- commandArgs(TRUE)
k <- as.integer(args[1])
N <- Sys.getenv("SLURM_JOB_NAME")
J <- Sys.getenv("SLURM_JOBID")
cat(sprintf("%s-%s part%03d bootstrap\n",N,J,k))

source("code/bootset.R")

who <- Sys.info()['nodename']
what <- detectCores()
cat(sprintf("on %s with %d cores\n", who, what))
where <- sprintf("/project/taddy/fresh/yelp/logs/%s-%s",N,J)
cl <- makeCluster(what, type="PSOCK",
	outfile=sprintf("%s/snow%03d.log",where,k))

X <- readRDS(sprintf("data/x/part%03d.rds",k))
rownames(V) <- NULL

for(b in 1:B){
 	ind <- sample.int(n,n,replace=TRUE)
	cat(sprintf("%d: %d,%d,%d first 3 @ %s\n",
		b,ind[1],ind[2],ind[3],date()))

	m <- M[ind]
	x <- X[ind,]
	v <- V[ind,]
	fix <- mu[ind]

	fit <- dmr(cl=cl, counts=x, covars=v, mu=fix,  
		nlambda=20, maxit=400, gamma=1,
		standardize=FALSE)

	phi <- coef(fit)
	z <- tcrossprod(x,phi[zget,])
	z@x <- z@x/m[z@i+1]
	colnames(z) <- paste("z",colnames(z),sep=".")
	saveRDS(z, 
		file=sprintf("data/snowboot/z%03d-%03d.rds",b,k), 
		compress=FALSE)
}

cat("done\n")
stopCluster(cl) 
cat("cluster stopped\n\n")

cat("warnings:\n")
warnings()

cat("done\n")

