library(methods)
library(lattice)
library(parallel)
library(Matrix)
library(gamlr)
library(dmr)

args <- commandArgs(TRUE)
k <- as.integer(args[1])
J <- Sys.getenv("SLURM_JOBID")
N <- Sys.getenv("SLURM_JOB_NAME")

cat(sprintf("%s-%s part%03d IR fit\n",N,J,k))

who <- Sys.info()['nodename']
what <- detectCores()
cat(sprintf("on %s with %d cores\n", who, what))

cat("\nread covars:\n")
system.time(load("data/covars.rda"))
cat("V is ")
print(object.size(V),u="Mb")
print(dim(V))
nobs <- nrow(V)

ZKEEP <- c("funny","useful","cool",       
 	"funny:days","funny:days2","useful:days",
 	"useful:days2","cool:days","cool:days2",
 	"usr.stars","usr.count","usr.funny","usr.useful","usr.cool")

cat("\nread x part:\n")
system.time(x <- readRDS(sprintf("data/x/part%03d.rds",k)))
cat("x is ")
print(object.size(x),u="Mb")
print(dim(x))

## hard path for logging from the snow cluster
cat("\nstart cluster\n")
where <- sprintf("/project/taddy/fresh/yelp/logs/%s-%s", N,J)
cl <- makeCluster(what,type="FORK",
		outfile=sprintf("%s/snow%03d.log",where,k))
print(cl)

cat("done.\n")

