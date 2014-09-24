## inference for the effect of review attributes.
library(dmr)
system.time(source("code/zmvy.R"))
colnames(Z) <- paste("z",colnames(Z),sep=".")
X <- as.data.frame(as.matrix(cBind(Z[,paste("z",yvar,sep=".")],M,V)))
xsd <- apply(X,2,sd)
## get full Z
zX <- as.data.frame(as.matrix(cBind(Z,M,V)))
zxsd <- apply(zX,2,sd)

dvar <-c("usr.stars",
	"usr.count","usr.funny",
	"usr.useful","usr.cool")

## fit it
flm <- glm(Y~., data=X, family="poisson")
print(summary(flm)$coef[dvar,])
zflm <- glm(Y~., data=zX, family="poisson")
print(summary(zflm)$coef[dvar,])

fB <- coef(summary(flm))
zfB <- coef(summary(zflm))



save(fB,zfB,
	file="results/inference.rda",compress=FALSE)


## fit it
Xs <- cBind(Z[,paste("z",yvar,sep=".")],M,V)
system.time(
	pof <- gamlr(Xs,Y,family="poisson",
		lambda.min.ratio=1e-8,gamma=10,verb=TRUE,tol=1e-8))

### plot paths
fwd <- pof
fwd$beta <- fwd$beta*xsd
fwd$lambda <- fwd$lambda[1:45]
fwd$beta <- fwd$beta[,1:45]
fwd$alpha <- fwd$alpha[1:45]
B <- coef(fwd,s=100)[-1,]
bigs <- B[order(-abs(B))[1:12]]
col <- rep("grey70",length(B))
names(col) <- names(B)
col[names(bigs)] <- rainbow(length(bigs))
col['usr.useful'] <- 'gold'
bigs['M'] <- bigs['M']+.02
bigs['z.funny'] <- bigs['z.funny']+.01
bigs['z.funny:days'] <- bigs['z.funny:days']+.02
bigs['z.cool:days'] <- bigs['z.cool:days']-.015
bigs['z.useful'] <- bigs['z.useful']-.025
bigs['usr.count'] <- bigs['usr.count']-.045
bigs['days2'] <- bigs['days2']-.01

pdf(file="fwd_paths.pdf",width=7,height=3.75)
par(omi=c(0,1,0,0),mai=c(.9,0,0.1,1),xpd=NA)
plot(fwd,col=col,df=FALSE,select=FALSE,yaxt="n",ylab="", font.main=1)
axis(side=4,las=1)
text(x=rep(-7,length(bigs)),y=bigs,labels=names(bigs), 
	font=3, col=col[names(bigs)], adj=1,cex=.8)
mtext(side=4,"Beta*sd(x)",line=3)
dev.off()


