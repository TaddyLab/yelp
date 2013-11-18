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
	print(b)
	return(beta[dvar])
}

cl <- makePSOCKcluster(16, outfile="data/snowboot/fwd.log")
clusterExport(cl,c("V","M","Y","dvar","BI"))

alpha <- parLapply(cl,1:B,geteffects)
alpha <- do.call(rbind, alpha)

write.table(alpha, "results/treatments.txt",
	sep="|", row.names=FALSE,col.names=dvar,quote=FALSE)

alpha <- read.table("results/treatments.txt", sep="|", header=TRUE)

glmm <- c(0.0581505, 0.1448512, 0.1050877, 0.3363976,-0.1648610)
glms <- c(0.0017694398,0.0008040118,0.0004546722,0.0022478543,0.0020281476)
names(glmm) <- names(glms) <- 
	c("usr.stars","usr.count","usr.funny","usr.useful","usr.cool")

pdf("graphs/boots.pdf",width=8,height=2)
par(mfrow=c(1,5), mai=c(0.4,.4,0.2,0.2),omi=c(.2,.2,.1,0))
for(d in colnames(alpha)){
	hist(alpha[,d],col="grey60",border="grey90",breaks=4,
		bty="n",ylab="",xlab="", main=d,freq=FALSE)
	polygon(x = glmm[d] + c(-2,-2,2,2)*glms[d], y=c(0,1e9,1e9,0), 
		border=FALSE, col=rgb(255,165,0,150,max=255))
}
mtext("coefficient",side=1,font=3,outer=TRUE)
mtext("density",side=2,font=3,outer=TRUE)
dev.off()























