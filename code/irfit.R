## multinomial inverse regression for review text
## written to be run via slurm and parallel (see irfit.sbatch)

source("code/setup.R")

cat("starting fit\n")
system.time({
	fit <- dmr(cl=cl, covars=V, counts=x,  mu=mu,  
			nlambda=20,
			gamma=1,
			verb=2,
			maxit=500,
			#lambda.min.ratio=1e-3,
			#family="gaussian",
			standardize=FALSE)})

cat("done with fit\n")
stopCluster(cl) 
cat("cluster stopped\n\n")

cat("save full fit\n")
system.time({
	saveRDS(fit, file=sprintf("data/irout/fit%03d.rds",k), compress=FALSE)})

cat("\nextracting coef as table\n")
system.time({
	beta <- coef(fit)
	b <- summary(beta)
	b$j <- colnames(beta)[b$j]
	b$i <- rownames(beta)[b$i]
	write.table(b,
        file=sprintf("data/irout/b%03d.txt",k),
        row.names=FALSE,col.names=FALSE,sep="|",quote=FALSE)
})
cat(sprintf("%.1f%% nonzero\n", mean(beta[-1,]!=0)*100 ))

cat("\nprojecting z and saving\n")
zget <- c(
		"usr.stars","usr.count","usr.funny","usr.useful","usr.cool",
		"funny", "useful", "cool",
      	"funny:days", "funny:days2", "useful:days",
      	"useful:days2", "cool:days", "cool:days2")
system.time({ 
	z <- tcrossprod(x,beta[zget,])
	print(colnames(z))
	z@x <- z@x/(exp(mu[z@i+1])-1)
	saveRDS(z, file=sprintf("data/irout/z%03d.rds",k), compress=FALSE)
})

cat("warnings:\n")
warnings()

cat("done\n")
