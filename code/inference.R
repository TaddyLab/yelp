## inference for the effect of review attributes.
library(dmr)
system.time(source("code/zmvy.R"))
X <- cBind(Z,M,V)
xsd <- apply(X,2,sd)
## get full Z
Z <- readRDS("results/Z.rds")
colnames(Z)[1:5] <- paste("z",colnames(Z)[1:5],sep=".")
zX <- cBind(Z[,1:5],X)
zX <- as.matrix(zX)
zxsd <- apply(zX,2,sd)

## fit it
system.time(
	pof <- gamlr(X,Y,family="poisson",
		lambda.min.ratio=1e-4,gamma=5,verb=TRUE,tol=1e-8))

system.time(
	lif <- gamlr(X,lY, doxx=TRUE, 
			lambda.min.ratio=1e-4,gamma=1,verb=TRUE,tol=1e-8))

zf <- gamlr(zX,Y,family="poisson",
		lambda.min.ratio=1e-4,gamma=5,verb=TRUE)

DX <- as.data.frame(as.matrix(zX))[,-(17:18)]
zlm <- glm(Y~., data=DX, family="poisson")
dvar <-c("usr.stars",
	"usr.count","usr.funny",
	"usr.useful","usr.cool")
print(summary(zlm)$coef[dvar,])

save(zf,pof,lif,zxsd,xsd,
	file="results/fwdfit.rda",compress=FALSE)


load("results/fwdfit.rda")

B <- coef(pof,s=100)[-1,]*xsd
zB <- coef(zf,s=100)[-1,]*zxsd

round(zB[zdrp],3)
round(B[zdrp],3)

# > round(zB[zdrp],3)
#  usr.stars  usr.count  usr.funny usr.useful   usr.cool 
#      0.061      0.141      0.103      0.319     -0.145 
# > round(B[zdrp],3)
#  usr.stars  usr.count  usr.funny usr.useful   usr.cool 
#      0.061      0.148      0.104      0.328     -0.152 

# usr.count                           0.1412222559 8.198472e-04 172.254360899
# usr.funny                           0.1026034035 4.705413e-04 218.053981090
# usr.useful                          0.3228165764 2.274686e-03 141.916962625
# usr.cool                           -0.1484044257 2.057398e-03 -72.132094035

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


