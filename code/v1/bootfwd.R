## read the worker ouput and sum
source("code/bootset.R")
V <- V[,-which(colnames(V)%in%yvar)]

BI <- vector(length=B,mode="list")
for(b in 1:B){
	BI[[b]] <- sample.int(n,n,replace=TRUE)
	#cat(sprintf("%d: ", b))
	#print(BI[[b]][1:3])
}

# geteffects <- function(b){
# 	require(Matrix)
# 	require(gamlr)

# 	Z <- 0
# 	for(z in Sys.glob(sprintf("data/snowboot/z%03d-*.rds",b)))
# 		Z <- Z + readRDS(z)
# 	VMZ <- as.data.frame(as.matrix(cBind(V[BI[[b]],],M[BI[[b]]],Z)))
# 	YI <- Y[BI[[b]]]
# 	print(system.time(fwd <- glm(YI ~ ., data=VMZ, family="poisson")))
# 	beta <- coef(fwd)
# 	print(b)
# 	return(beta[dvar])
# }

#cl <- makeForkCluster(16, outfile="logs/bootfwd.log")
#clusterExport(cl,c("V","M","Y","dvar","BI"))
bi <- 1:B
# alpha <- parLapply(cl,bi,geteffects)
# stopCluster(cl)
# alpha <- do.call(rbind,alpha)

# alpha <- c()
# for(b in 1:B){
#  	alpha <- rbind(alpha,geteffects(b))
# }

# write.table(alpha, "results/treatments.txt",
# 	sep="|", row.names=FALSE,col.names=dvar,quote=FALSE)


# uncnd <- glm(Y ~ ., data=as.data.frame(as.matrix(V)), family="poisson")
# glmm <- coef(uncnd)[dvar]
# glms <- coef(summary(uncnd))[dvar,2]
# names(glmm) <- names(glms) <- dvar

alpha <- read.table("results/treatments.txt", sep="|", header=TRUE)
library(gamlr)

guntau <- function(b){
	require(Matrix)
	require(gamlr)
	VI <- V[BI[[b]],]
	YI <- Y[BI[[b]]]
	print(system.time(
		fwd <- gamlr(VI, YI, family="poisson", 
			gamma=5, lambda.min.ratio=1e-4, maxit=1e5)
		))
	print(b)
	beta <- coef(fwd,s=100)["usr.count",]
	print(beta)
	return(beta)
} 

tau <- c()
for(b in 1:B){
	tau <- c(tau, guntau(b))
}

taum <- matrix(tau)
# write.table(taum, "results/guntau.txt",
#  	sep="|", row.names=FALSE,col.names=FALSE,quote=FALSE)
# stopCluster(cl)
# alpha <- do.call(rbind,alpha)

# pdf("graphs/boots.pdf",width=8,height=2)
# par(mfrow=c(1,5), mai=c(0.4,.4,0.2,0.2),omi=c(.2,.2,.1,0))
# for(d in colnames(alpha)){
# 	hist(alpha[,d],col="grey60",border="grey90",breaks=5,
# 		bty="n",ylab="",xlab="", main=d,freq=FALSE,xlim=range(c(alpha[,d],glmm[d]+2*glms[d])))
# 	polygon(x = glmm[d] + c(-2,-2,2,2)*glms[d], y=c(0,1e9,1e9,0), 
# 		border=FALSE, col=rgb(255,165,0,150,max=255))
# }
# mtext("coefficient",side=1,font=3,outer=TRUE)
# mtext("density",side=2,font=3,outer=TRUE)
# dev.off()



alpha <- read.table("results/treatments.txt", sep="|", header=TRUE)[,'usr.count']
tau <- read.table("results/tau.txt",header=FALSE)[,1]

nb <- 5
pe <- hist(tau,breaks=nb,plot=FALSE)
pez <- hist(alpha,breaks=nb,plot=FALSE)
de <- density(tau,adjust=1.5)
dez <- density(alpha,adjust=1.5)

yl <- c(0,max(pez$density,pe$density,de$y,dez$y))
xl <- range(c(pez$breaks,pe$breaks,dez$x,dez$x))

pdf("graphs/counteffect.pdf", width=6, height=3)
par(mfrow=c(1,2), mai=c(0.5,.5,0.1,0.1),omi=c(.2,.2,.1,0))
plot( pe, col="grey50",border="grey20",freq=FALSE, 
	bty="n",ylab="",xlab="", main="", xlim=xl,ylim=yl)  
plot( pez, col=rgb(255,165,0,150,max=255), border="grey20",freq=FALSE, add=T) 

plot(de, col="grey10",lwd=2, type="l", 
	bty="n",ylab="",xlab="", main="", xlim=xl,ylim=yl)
lines( dez, col="darkorange", lwd=2) 

mtext("1SD usr.count effect", side=1,font=3,outer=TRUE)
mtext("bootstrap density", side=2,font=3,outer=TRUE)
par(xpd=NA)
legend(x=0.06,y=65, h=TRUE, bty="n", legend=c("with z", "without text"), 
	lwd=4, col=c("darkorange","grey30"))
dev.off()





















