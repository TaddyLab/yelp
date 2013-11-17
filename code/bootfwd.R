## read the worker ouput and sum
source("code/bootset.R")
V <- V[,-which(colnames(V)%in%yvar)]
 
BI <- vector(length=B,mode="list")
for(b in 1:B){
	BI[[b]] <- sample.int(n,n,replace=TRUE)
	cat(sprintf("%d: ", b))
	print(BI[[b]][1:3])
}

geteffects <- function(b){
	require(Matrix)
	require(gamlr)

	Z <- 0
	for(z in Sys.glob(sprintf("data/snowboot/z%03d-*.rds",b))){
		print(z)
		Z <- Z + readRDS(z)
	}
	VMZ <- as.data.frame(as.matrix(cBind(V[BI[[b]],],M[BI[[b]]],Z)))
	YI <- Y[BI[[b]]]
	system.time(fwd <- glm(YI ~ ., data=VMZ, family="poisson"))
	beta <- coef(fwd)
	return(beta[dvar])
}

cl <- makePSOCKcluster(16, outfile="data/snowboot/fwd.log")
clusterExport(cl,c("V","M","Y","dvar","BI"))

alpha <- parLapply(cl,1:B,geteffects)
alpha <- do.call(rbind, alpha)

write.table(alpha, "results/treatments.txt",
	sep="|", row.names=FALSE,col.names=dvar,quote=FALSE)

alpha <- read.table("results/treatments.txt", sep="|", header=TRUE)

pdf("graphs/boots.pdf",width=8,height=2)
par(mfrow=c(1,5), mai=c(0.4,.4,0.2,0.2),omi=c(.2,.2,.1,0))
for(d in colnames(alpha)){
	hist(alpha[,d],col="grey60",border="grey90",breaks=4,
		bty="n",ylab="",xlab="", main=d,freq=FALSE)
}
mtext("coefficient",side=1,font=3,outer=TRUE)
mtext("density",side=2,font=3,outer=TRUE)
dev.off()























