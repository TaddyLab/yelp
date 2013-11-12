## read the worker ouput and sum
library(methods)
library(lattice)
library(Matrix)

cat("cat\n")
system("cat data/irout/b*.txt > results/B.txt")

load("data/covars.rda")
Z <- 0
cat("sum Z\n")
for(z in Sys.glob("data/irout/z*.rds")){
	print(z)
	print(system.time(Z <- Z + readRDS(z)))
}
cat("add m to Z\n")
system.time(Z <- cBind(Z,m=exp(mu)-1))
cat("save Z\n")
system.time(
	saveRDS(Z, file="results/Z.rds",compress=FALSE))

cat("bind fit\n")
fit <- list()
for(f in Sys.glob("data/irout/fit*.rds")){
	print(f)
	fit <- c(fit, readRDS(f))
}
cat("save fit\n")
saveRDS(fit,"results/dmr.rds",compress=FALSE)

cat("done\n")

