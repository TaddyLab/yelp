## inference for the effect of review attributes.
library(dmr)
system.time(source("code/zmvy.R"))
colnames(Z) <- paste("z",colnames(Z),sep=".")
X <- as.data.frame(as.matrix(cBind(Z[,paste("z",yvar,sep=".")],M,V)))
xsd <- apply(X,2,sd)
## get full Z
zX <- as.data.frame(as.matrix(cBind(Z,M,V)))
zxsd <- apply(zX,2,sd)

## fit it
flm <- glm(Y~., data=X, family="poisson")
print(summary(flm)$coef[dvar,])
zflm <- glm(Y~., data=zX, family="poisson")
print(summary(zflm)$coef[dvar,])

fB <- coef(summary(flm))
zfB <- coef(summary(zflm))

dvar <-c("usr.stars",
	"usr.count","usr.funny",
	"usr.useful","usr.cool")



save(fB,zfB,
	file="results/wlsinference.rda",compress=FALSE)


## fit it
# X <- cBind(Z[,paste("z",yvar,sep=".")],M,V)
# xsd <- apply(X,2,sd)
# system.time(
# 	pof <- gamlr(,Y,family="poisson",
# 		lambda.min.ratio=1e-4,gamma=5,verb=TRUE,tol=1e-8))
# ### plot paths
# fwd <- pof
# fwd$beta <- fwd$beta*xsd
# bigs <- B[order(-abs(B))[1:12]]
# col <- rep("grey70",length(B))
# names(col) <- names(B)
# col[names(bigs)] <- rainbow(length(bigs))
# bigs['usr.count'] <- bigs['usr.count']-.01
# bigs['usr.cool'] <- bigs['usr.cool']+.01
# bigs['cool:days'] <- bigs['cool:days']-.04
# bigs['M'] <- bigs['M']+.005
# bigs['usr.funny'] <- bigs['usr.funny']-.02
# pdf(file="fwd_paths.pdf",width=7,height=3.75)
# par(omi=c(0,1,0,0),mai=c(1,0,0.1,1),xpd=NA)
# plot(fwd,col=col,df=FALSE,select=FALSE,yaxt="n",ylab="", font.main=1)
# axis(side=4)
# text(x=rep(-8,length(bigs)),y=bigs,labels=names(bigs), 
# 	font=3, col=rainbow(length(bigs)), adj=1,cex=.8)
# mtext(side=4,"Beta*sd(x)",line=3)
# dev.off()


