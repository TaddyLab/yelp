## read the worker ouput and sum
source("code/bootset.R")
vdrop <- c(yvar,"stars.overusr","stars.overbiz")
V <- V[,-which(colnames(V)%in%vdrop)]
 
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
	VMZ <- cBind(V,M,Z)

	system.time(fwd <- gamlr(VMZ,Y,family="poisson",
			lambda.min.ratio=1e-4,gamma=5,verb=TRUE))
	beta <- coef(fwd,s=100)
	return(beta[dvar,])
}

cl <- makePSOCKcluster(16, outfile="data/snowboot/fwd.log")
clusterExport(cl,c("V","M","Y","dvar"))

alpha <- parLapply(cl,1:B,geteffects)
alpha <- do.call(rbind, alpha)

write.table(alpha, "results/treatments.txt",
	sep="|", row.names=FALSE,col.names=dvar,quote=FALSE)

# glm comparitor from inference.R
#               Estimate   Std. Error   z value      Pr(>|z|)
# usr.stars   0.06188816 0.0017804125  34.76057 9.594343e-265
# usr.count   0.14122226 0.0008198472 172.25436  0.000000e+00
# usr.funny   0.10260340 0.0004705413 218.05398  0.000000e+00
# usr.useful  0.32281658 0.0022746863 141.91696  0.000000e+00
# usr.cool   -0.14840443 0.0020573980 -72.13209  0.000000e+00

glmm <- c( 0.06188816, 0.14122226, 0.10260340, 0.32281658,-0.14840443)
glms <- c(0.0017804125,0.0008198472,0.0004705413,0.0022746863,0.0020573980)
names(glmm) <- names(glms) <- c("usr.stars","usr.count",
								"usr.funny","usr.useful","usr.cool")
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























