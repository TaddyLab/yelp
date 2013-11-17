## plots and summary
library(dmr)
library(glmnet)
net <- readRDS("results/net.rds")
rfo <- read.table("results/rFoos.txt",
        sep="|",header=TRUE)
lio <- read.table("results/fwdoos.txt",
        sep="|",header=TRUE)
print(sd(rfo$mse)) ## tiny

pdf(file="cv.pdf",width=7,height=4)
par(mai=c(1,.8,.3,.2))
plot(log(net$lambda), net$cvm, col=2, pch=16,
  xlab="log lambda", ylab="mean squared error", 
  ylim=range(c(net$cvm,rfo$mse,lio$mse)), bty="n")
abline(h=mean(lio$mse),col="navy", lwd=2)
abline(h=mean(rfo$mse),col="gold", lwd=2)
legend("topleft", cex=1.2,
  fill=c("red","navy","gold"), 
  border="grey90",bty="n",
  legend=c("Lasso (min mean R2=.461)",
      "IR-linear (mean R2=.455)",
      "IR-randomForest (mean R2=.521)"))
dev.off()

1-min(net$cvm)/net$cvm[1]
# [1] 0.4607141
mean(lio$r2)
# [1] 0.4552066
mean(rfo$r2)
# [1] 0.5205378

load("data/covars.rda")

B <- read.table("results/B.txt", 
  sep="|", header=FALSE,
  quote="", comment="",
  col.names=c("i","j","x"))
B$i <- factor(B$i,levels=rownames(wlsB))
B[order(-abs(B$x))[1:20],]
B <- sparseMatrix(i=as.numeric(B$i),
        j=as.numeric(B$j), x=B$x,
        dimnames=list(levels(B$i),levels(B$j)))

> cor(Z[,'usr.count'],wlsZ[,'usr.count'])
[1] 0.8954603
> var <- "funny"
> cor(Z[,var],wlsZ[,var])
[1] 0.8706752
> var <- "cool"
> cor(Z[,var],wlsZ[,var])
[1] 0.6769865
> var <- "useful"
> cor(Z[,var],wlsZ[,var])
[1] 0.7898124
> cor(c(as.matrix(B)),c(as.matrix(wlsB)))
[1] 0.790685

round(exp(B['funny',][order(-B['funny',])[1:16]]),2)
 #   dimsum     misir      rito      fart 
 #     5.35      2.74      2.27      2.23
 #  prankst  porkwich  governor      bong
 #     2.12      2.11      2.10      2.05
 # detector    mormon    reggae    viciou 
 #     2.04      2.03      2.02      2.02 
 #    thong      plot    safari metropoli 
 #     2.00      2.00      1.98      1.95 

B[order(-abs(B[,'dimsum']))[1:10],'dimsum']
#      Chinese Asian Fusion cityChandler         cool   Vietnamese  
#     9.448375    -4.657093     2.517415    -2.058031    -2.054590 
#     cityMesa       Donuts        funny   funny:days  cityPhoenix 
#     1.944427    -1.772237     1.677801    -1.537780     1.510298 

B[order(-abs(B[,'misir']))[1:10],'misir']

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
