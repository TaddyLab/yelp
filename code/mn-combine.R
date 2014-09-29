## read the worker ouput and sum
library(methods)
library(lattice)
library(Matrix)
library(gamlr)

cat(sprintf("combining @ %s\n",date()))

I <- Sys.getenv("SLURM_JOBID")
J <- Sys.getenv("SLURM_JOB_NAME")

cat("cat B\n")
system(sprintf("cat results/%s/%s-data/b*.txt > results/%s/beta.txt",J,I,J))

cat("sum Z\n")
Z <- 0
for(z in Sys.glob(sprintf("results/%s/%s-data/z*.rds",J,I))){
	Z <- Z + readRDS(z) }
Z <- as.matrix(Z)
write.table(Z,row.names=FALSE, file=sprintf("results/%s/z.txt",J), sep="|",quote=FALSE)
rm(Z)

cat("combine fits\n")
fits <- NULL
for(f in Sys.glob(sprintf("results/%s/%s-data/fit*.rds",J,I))){ 
	fits <- c(fits,readRDS(f)) }
saveRDS(fits, file=sprintf("results/%s/fits.rds",J), compress=FALSE)

cat(sprintf("done @ %s\n",date()))


