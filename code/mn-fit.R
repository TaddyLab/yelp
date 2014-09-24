
#### code to fit MN regression.  

library(methods)
library(lattice)
library(parallel)
library(Matrix)
library(gamlr)
library(distrom)

args <- commandArgs(TRUE)
part <- as.integer(args[1])
I <- Sys.getenv("SLURM_JOBID")
J <- Sys.getenv("SLURM_JOB_NAME")

cat(sprintf("%s-%s chunk %03d\n",J,I,part))

who <- Sys.info()['nodename']
what <- detectCores()
cat(sprintf("on %s with %d cores\n", who, what))

## hard path for logging from the snow cluster
where <- sprintf("/project/taddy/yelp/results/%s/%s",J,I)
cl <- makeCluster(what,type="FORK",outfile=sprintf("%s-log/snow%03d.log",where,part))
print(cl)

x <- readRDS(sprintf("data/x/part%03d.rds",part))
cat(sprintf("x from `%s' to `%s'\n",colnames(x)[1],tail(colnames(x),1)))

load("data/meta.rda")
v <- cBind(REV,GEO,CAT,BIZ)
cat(sprintf("v from `%s' to `%s'\n",colnames(v)[1],tail(colnames(v),1)))

nobs <- nrow(v)
nvar <- ncol(v)
nwrd <- ncol(x)

cat("\n starting fit\n")
print(system.time({
	fit <- dmr(cl=cl, 
			covars=v, counts=x,  mu=log(m),  
			gamma=10,verb=2,
			lambda.min.ratio=1e-3,
			standardize=FALSE)}))

cat("done with fit\n")
stopCluster(cl) 

saveRDS(fit, file=sprintf("%s-data/fit%03d.rds",where,part), compress=FALSE)

cat("\nextracting coef as table\n")
system.time({
	beta <- coef(fit)
	b <- summary(beta)
	b$j <- colnames(beta)[b$j]
	b$i <- rownames(beta)[b$i]
	write.table(b,
        file=sprintf("%s-data/b%03d.txt",where,part),
        row.names=FALSE,col.names=FALSE,sep="|",quote=FALSE)
})
cat(sprintf("%.1f%% nonzero\n", mean(beta[-1,]!=0)*100 ))

cat("\nprojecting z\n")
system.time({ 
	z <- tcrossprod(x,beta[colnames(REV),])
	print(colnames(z))
	z@x <- z@x/(m[z@i+1]-1)
	saveRDS(z, file=sprintf("%s-data/z%03d.txt",where,part), compress=FALSE)
})

cat("warnings:\n")
warnings()

cat("done\n")





