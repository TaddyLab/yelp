## read the worker ouput and sum
library(methods)
library(lattice)
library(Matrix)
library(gamlr)

cat(sprintf("combining @ %s\n",date()))

J <- Sys.getenv("SLURM_JOB_NAME")
K <- 5

for(k in 1:K){
	Z <- 0
	for(z in Sys.glob(sprintf("results/%s/data/z%d-*.rds",J,k))){
		Z <- Z + readRDS(z) }
	Z <- as.matrix(Z)
	write.table(Z,row.names=FALSE, file=sprintf("results/%s/z%d.txt",J,k), sep="|",quote=FALSE)
	rm(Z)
}

cat(sprintf("done @ %s\n",date()))


