## plots and summary
library(dmr)
wlsc <- read.table("results/wlscompare.txt",sep="|")
r <- tapply(wlsc[,3],wlsc[,2],mean)

load("data/covars.rda")
X <- readRDS("data/x.rds")

B <- read.table("results/poB.txt", 
  sep="|", header=FALSE,
  quote="", comment="",
  col.names=c("i","j","x"))
B$i <- factor(B$i,levels=c("intercept",colnames(V)))
B$j <- factor(B$j,levels=colnames(X))
poB <- as.matrix(sparseMatrix(i=as.numeric(B$i),
        j=as.numeric(B$j), x=B$x,
        dimnames=list(levels(B$i),levels(B$j))))

B <- read.table("results/B.txt", 
  sep="|", header=FALSE,
  quote="", comment="",
  col.names=c("i","j","x"))
B$i <- factor(B$i,levels=c("intercept",colnames(V)))
B$j <- factor(B$j,levels=colnames(X))
wlsB <- as.matrix(sparseMatrix(i=as.numeric(B$i),
        j=as.numeric(B$j), x=B$x,
        dimnames=list(levels(B$i),levels(B$j))))

poZ <- as.matrix(readRDS("results/poZ.rds"))
wlsZ <- as.matrix(readRDS("results/Z.rds"))
sapply(colnames(poZ),function(j) cor(poZ[,j],wlsZ[, j]))
n <- nrow(poZ)

png(file="wlscompare.png",width=9,height=2.8,units='in',res=300)
par(mfrow=c(1,3),mai=c(.6,.7,.5,.4))
plot(as.numeric(names(r)), r, type="l", lwd=2, cex.lab=1.4,
    xlab="log zeta", ylab="correlation",
    bty="n", main="WLS vs Poisson beta")
abline(v=-3,lty=2)
par(mai=c(.6,.7,.2,.4))
plot(c(poB),c(wlsB), cex.lab=1.4,
  pch=16,cex=.25, bty="n",
  xlab="Poisson beta", ylab="WLS beta")
plot(c(poZ[,7:8]),c(wlsZ[,7:8]),cex.lab=1.4,
  col=rep(c("turquoise","forestgreen"),each=n), 
  pch=16,cex=.25, bty="n", 
  xlim=c(-0.2,0.2),
  xlab="Poisson Z", ylab="WLS Z")
legend("bottomright",fill=c("turquoise","forestgreen"),
  border=FALSE,bty="n",legend=c("funny","useful"),cex=1.4)
dev.off()

library(dmr)
library(glmnet)
net <- readRDS("results/net.rds")
rfo <- read.table("results/PorFoos.txt",
        sep="|",header=TRUE)
lio <- read.table("results/Pofwdoos.txt",
        sep="|",header=TRUE)
print(sd(rfo$mse)) ## tiny

pdf(file="cv.pdf",width=7,height=4)
par(mai=c(1,.8,.3,.2))
plot(log(net$lambda), net$cvm, col=2, pch=16,
  xlab="log lambda", ylab="mean squared error", 
  ylim=range(c(net$cvm,rfo$mse,lio$mse)), bty="n")
abline(h=mean(lio$mse),col="navy", lwd=2)
abline(h=mean(rfo$mse),col="gold", lwd=2)
legend("topleft", cex=1.1,
  fill=c("red","navy","gold"), 
  border="grey90",bty="n",
  legend=c(sprintf("Lasso (min mean R2=%.3f)",1-min(net$cvm)/net$cvm[1]),
      sprintf("IR-linear (mean R2=%.3f)",mean(lio$r2)),
      sprintf("IR-randomForest (mean R2=%.3f)",mean(rfo$r2))))
dev.off()


B <- read.table("results/B.txt", 
  sep="|", header=FALSE,
  quote="", comment="",
  col.names=c("i","j","x"))
B$i <- factor(B$i)#,levels=rownames(wlsB))
B[order(-abs(B$x))[1:20],]
B <- sparseMatrix(i=as.numeric(B$i),
        j=as.numeric(B$j), x=B$x,
        dimnames=list(levels(B$i),levels(B$j)))


bigstar <- B["stars",order(-abs(B["stars",]))[1:40]]
bigfun <- B["funny",order(-abs(B["funny",]))[1:40]]

library(wordcloud)
## we'll size the word proportional to its in-topic probability
## and only show those with > 0.004 omega
par(mfrow=c(1,2),omi=c(0,0,.8,0))
wordcloud(names(bigstar), freq=abs(bigstar), ordered.colors=TRUE, col=1+(bigstar<0))
mtext("stars",font=2,col=8, line=5, cex=3)
wordcloud(names(bigfun), freq=abs(bigfun), ordered.colors=TRUE, col=1+(bigfun<0))
mtext("funny",font=2,col=8, line=5, cex=3)

x <- readRDS("data/x.rds")
d <- colSums(x)
names(d) <- colnames(x)
bigs<-c('awesome','excellent','fantastic',
  'favorite','bland','worst',
  'love',
  'yummy','awful','alway','overpric','hipst',
  'groupon','jetta','!!!','atmosphere',
  'shit','reasonab','fuck','god',
  'support','variety','decent')
#names(c(bigstar[1:20],bigfun[1:20]))
col <- c("grey20","blue")[1+(B["cool",bigs]>0)]
par(xpd=NA,mai=c(.8,.8,.1,.1))
bigmat <- exp(as.matrix(t(B[c("stars","funny"), bigs])))
plot(bigmat,bty="n",type="n",xlim=c(.55,1.8))
text(bigmat,labels=bigs,cex=log(d[bigs])/10, col=col)

# > cor(Z[,'usr.count'],wlsZ[,'usr.count'])
# [1] 0.8954603
# > var <- "funny"
# > cor(Z[,var],wlsZ[,var])
# [1] 0.8706752
# > var <- "cool"
# > cor(Z[,var],wlsZ[,var])
# [1] 0.6769865
# > var <- "useful"
# > cor(Z[,var],wlsZ[,var])
# [1] 0.7898124
# > cor(c(as.matrix(B)),c(as.matrix(wlsB)))
# [1] 0.790685

# $`scaled:scale`
#        stars        funny       useful         cool         days        days2 
# 1.216157e+00 1.874861e+00 2.238150e+00 1.949718e+00 5.233413e+02 1.036633e+06 
#   funny:days  funny:days2  useful:days useful:days2    cool:days   cool:days2 
# 2.446246e+03 3.822579e+06 2.685168e+03 4.117123e+06 2.391289e+03 3.661669e+06 

B[c('funny','funny:days','funny:days2'),] <- 
  B[c('funny','funny:days','funny:days2'),]/c(1.874861,2.446246e+03,3.822579e+06)

B[c('useful','useful:days','useful:days2'),] <- 
  B[c('useful','useful:days','useful:days2'),]/c(2.238150,2.685168e+03,4.117123e+06)

B[c('cool','cool:days','cool:days2'),] <- 
  B[c('cool','cool:days','cool:days2'),]/c(1.949718e+00,2.391289e+03,3.661669e+06)

B['stars',] <- B['stars',]/1.216157


vc <- 'stars'
round(exp(B[vc,][order(-B[vc,])[1:6]]),2)
round(exp(B[vc,][order(-B[vc,])[7:12]]),2)

 #   dimsum     misir      rito      fart 
 #     5.35      2.74      2.27      2.23
 #  prankst  porkwich  governor      bong
 #     2.12      2.11      2.10      2.05
 # detector    mormon    reggae    viciou 
 #     2.04      2.03      2.02      2.02 
 #    thong      plot    safari metropoli 
 #     2.00      2.00      1.98      1.95 

B[c('funny','funny:days','funny:days2'),'dimsum']
#      Chinese Asian Fusion cityChandler         cool   Vietnamese  
#     9.448375    -4.657093     2.517415    -2.058031    -2.054590 
#     cityMesa       Donuts        funny   funny:days  cityPhoenix 
#     1.944427    -1.772237     1.677801    -1.537780     1.510298 

B[order(-abs(B[,'dimsum']))[1:10],'dimsum']

tkn='dimsum'; B[order(-abs(B[,tkn]))[1:10],tkn]

tkn=':-)'; B[order(-abs(B[,tkn]))[1:10],tkn]
#    Buffets     cool:days    cool:days2     Fast Food         stars 
#  0.9289012     0.6645471    -0.5049206     0.4798990     0.3345952 
#     useful         Pizza   cityGilbert   funny:days2 Beauty & Spas 
# -0.2636958    -0.2271394     0.1544538    -0.1343172     0.1060747 

tkn=':-('; B[order(-abs(B[,tkn]))[1:10],tkn]
#    Buffets       stars        cool  usr.useful   usr.count       days2 
#  1.61242313 -0.41590720 -0.37527923  0.27722608  0.23156245 -0.15956262 
# cityGilbert Restaurants cityPhoenix  cool:days2 
#  0.12903084  0.09769956 -0.07893840 -0.06788629 

fit <- readRDS("results/dmr.rds")

plotpath <- function(tkn, vr,vc){
  cols <- rep(8,nrow(fit[[1]]$b))
  names(cols) <- rownames(fit[[1]]$b)
  cols[vr] <- vc
  plot(fit[[tkn]],col=cols,select=FALSE)
  lam <- fit[[tkn]]$lambda[which.min(AIC(fit[[tkn]]))]
  abline(v=log(lam),lty=3)
  mtext(tkn,font=2,line=2)
  legend("topright", bty="n", cex=.85,
    legend=vr,fill=vc,border="grey80")
}

pdf(file="ir_paths.pdf",width=7.25,height=3)
par(mfrow=c(1,3),xpd=FALSE)
plotpath(':-)',
  c('funny','useful','cool','stars','cityGilbert','Buffets'),
  c("cyan",3,4,2,"darkorange","purple"))
plotpath(':-(',
  c('funny','useful','cool','stars','cityGilbert','Buffets'),
  c("cyan",3,4,2,"darkorange","purple"))
plotpath('sex',
  c('funny','useful',"Lingerie", "Local Flavor", "Adult", "Bars"),
  c("cyan",3,4,2,"darkorange","purple"))
dev.off()



library(textir)

## drops for pred
yvar <- c("funny","useful","cool",       
  "funny:days","funny:days2","useful:days",
  "useful:days2","cool:days","cool:days2")
dvar <-c("usr.stars",
  "usr.count","usr.funny",
  "usr.useful","usr.cool")

load("data/covars.rda")
V <- V[,-which(colnames(V)%in%yvar)]
V <- t(t(V)/sdev(V))

ratings <- readRDS("data/ratings.rds")
Y <- rowSums(ratings[,-1])
lY <- log(Y+1)
print(nobs <- length(Y))

X <- cBind(M,V)


fwd <- gamlr(X, Y, family="poisson", verb=TRUE, gamma=10, lambda.min.ratio=1e-4, scale=FALSE)
B <- coef(fwd,s=100)[-1,]

pdf(width=7,height=3,file="yelp_fwdgray.pdf")
par(omi=c(0,.7,0,0),mai=c(1,0,0.1,1),xpd=NA)
plot(fwd, col="grey50", select=FALSE,df=FALSE,yaxt="n",ylab="")
axis(side=4)
mtext(side=4,"Beta*sd(x)",line=3)
dev.off()


dvar <-c("usr.stars",
  "usr.count","usr.funny",
  "usr.useful","usr.cool")
cols <- rep("grey50",ncol(X))
names(cols) <- colnames(X)
cols[dvar] <- c("gold","hotpink","green","red","blue")

pdf(width=7,height=3,file="yelp_fwd.pdf")
par(omi=c(0,.7,0,0),mai=c(1,0,0.1,1),xpd=NA)
plot(fwd, col=cols, select=FALSE,df=FALSE,yaxt="n",ylab="")
axis(side=4)
mtext(side=4,"Beta*sd(x)",line=3)
text(x=rep(-8,5),y=B[dvar],labels=dvar, 
    font=3, col=cols[dvar], adj=1)
dev.off()

## fit it
# X <- cBind(Z[,paste("z",yvar,sep=".")],M,V)
# xsd <- apply(X,2,sd)
# system.time(
#   pof <- gamlr(,Y,family="poisson",
#     lambda.min.ratio=1e-4,gamma=5,verb=TRUE,tol=1e-8))
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
#   font=3, col=rainbow(length(bigs)), adj=1,cex=.8)
# mtext(side=4,"Beta*sd(x)",line=3)
# dev.off()



