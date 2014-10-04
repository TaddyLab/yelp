
#### code to fit MN regression.  

library(methods)
library(lattice)
library(parallel)
library(Matrix)
library(gamlr)
library(distrom)

args <- commandArgs(TRUE)
part <- as.integer(args[1])
penweight <- as.numeric(args[2])

J <- Sys.getenv("SLURM_JOB_NAME")

cat(sprintf("%s chunk %03d\n",J,part))

who <- Sys.info()['nodename']
what <- detectCores()
cat(sprintf("on %s with %d cores\n", who, what))

## hard path for logging from the snow cluster
where <- sprintf("/project/taddy/yelp/results/%s",J)
cl <- makeCluster(what,type="FORK",outfile=sprintf("%s/log/snow%03d.log",where,part))
print(cl)

x <- readRDS(sprintf("data/x/part%03d.rds",part))[,1:16]
cat(sprintf("x from `%s' to `%s'\n",colnames(x)[1],tail(colnames(x),1)))

load("data/meta.rda")
v <- cBind(REV,CAT,GEO,BIZ)
n <- nrow(v)
d <- ncol(v)
drev <- ncol(REV)
cat(sprintf("v from `%s' to `%s'\n",colnames(v)[1],tail(colnames(v),1)))

K <- 5
foldid <- rep(1:K,each=ceiling(n/K))[1:n]
cat(sprintf("running %d fold CV\n",K))

for(thisfold in 1:K){
	train <- which(foldid != thisfold)

	fit <- dmr(cl=cl, covars=v[train,], counts=x[train,],  mu=log(m[train]), 
			varweight=c(rep(1,drev),rep(1/penweight,d-drev)), 
			gamma=1,standardize=FALSE)

	z <- tcrossprod(x,coef(fit)[c("funny","useful","cool","stars"),])
	saveRDS(z, file=sprintf("%s/data/z%d-%03d.rds",where,thisfold,part), compress=FALSE)
	print(thisfold)
}

stopCluster(cl) 

cat("warnings:\n")
warnings()

cat("done\n")





