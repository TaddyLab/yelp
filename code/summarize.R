## plots and summary
library(distrom)
load("data/meta.rda")
V <- cBind(REV,CAT,GEO,BIZ)
sdv <- sqrt(colMeans(V^2))
var <- c("funny","useful","cool","stars")

#######  IR paths

fit <- readRDS("results/mn2/fits.rds")

vcol <- c("#66c2a5","#fc8d62","dodgerblue","gold")
names(vcol) <- var
plotpath <- function(tkn){
  f <- fit[[tkn]]
  f$beta <- f$beta*sdv
  plot(f,col="grey80")
  mtext(tkn,font=2,line=2)
  for(v in var){
  	lines(log(fit[[tkn]]$lambda),fit[[tkn]]$beta[v,],col=vcol[v],lwd=2)}
}

pdf(file="ir_paths.pdf",width=7.25,height=4)
par(mfrow=c(1,3),xpd=FALSE)
plotpath(':-)')
plotpath(':-/')
legend("bottomright", bty="n", cex=1.5,
  legend=var,fill=vcol,border="grey80")
plotpath(':-(')
dev.off()

######### top words

X <- readRDS("data/text.rds")
sdx <- sqrt(colMeans(X^2))
mrg <- crossprod(X, V[,var])/sdx

getB <- function(pw){
  read.table(sprintf("results/mn%d/beta.txt",pw),
  	sep="|", header=FALSE,
  	quote="", comment="", as.is=TRUE,
  	col.names=c("i","j","x")) }

B2 <- getB(2)
B20 <- getB(20)
B200 <- getB(200)

varp <- c("stars","stars","funny","useful","cool")
tops <- function(v,pw=2,sgn=1,n=10){
	if(pw==0){
		return(rownames(mrg)[order(-sgn*mrg[,v])[1:n]])
	}
	B <- get(sprintf("B%d",pw))
	b <- B[B$i==v,-1]
	bt <- b[order(-sgn*b$x)[1:n],1]
	bt[is.na(bt)] <- ""
	bt
}

W <-matrix("",ncol=4,nrow=20)
pw <- c(0,2,20,200)

for(j in 1:4){
	for(i in 1:5){
		if(i==2) sgn <- -1
		else sgn <- 1
		W[(i-1)*4 + j,2] <- pw[j]
		if(j>1) W[(i-1)*4 + j,3] <- sum(get(sprintf("B%d",pw[j]))$i==varp[i]) 
		W[(i-1)*4 + j,4] <- 
			paste(c("\\footnotesize ",tops(varp[i],pw[j],sgn)),collapse=" ")
	}
}
W[c(2,6,10,14,18),1] <- c("+stars","-stars","funny","useful","cool")
W <- sub("_","-",W)
W <- sub("é","e",W)

for(i in 1:nrow(W)){
 	cat(paste(W[i,],collapse=" & "), "\\\\\n")
 	if(i %in% c(4,8,12,16,20)) cat("\\hline\n")
}

###### projections
pw <- 20

z <- read.table(sprintf("results/mn%d/z.txt",pw),sep="|",header=TRUE)
# z <- z*m
# z[z==Inf] <- 0
# z[z==-Inf] <- 0
cc <- cbind( cor(REV[,var]), cor(z[,var]) )
fancy <- function(c){
	if(abs(c)<0.05) return("")
	if(c==1) return("1")
	return(sprintf("%.01f",c))
}
for(i in 1:4){
	cat(var[i])
	for(j in 1:4)
	  cat(" & ", fancy(cc[i,j]))
	cat(" & \\hskip 2cm", var[i])
	for(j in 5:8)
	  cat(" & ", fancy(cc[i,j]))
	 cat("\\\\\n")
}

lm <- m<50
zm <- z[lm,]
um <- uid[lm]
names(um)[order(-zm$funny)[1:3]]
names(um)[order(-zm$useful)[1:3]]
names(um)[order(-zm$cool)[1:3]]


########  treatment estimation
y <- REV[,"stars"]*attributes(REV)$`scaled:scale`["stars"]+
		attributes(REV)$`scaled:center`["stars"]
d <- as.matrix(data.frame(d=log(round(
	REV[,"usr.count"]*attributes(REV)$`scaled:scale`["usr.count"]+
		attributes(REV)$`scaled:center`["usr.count"])+1)))
mrg <- gamlr(d,y)
coef(mrg)["d",]
# [1] 0.003216478

r <- REV[,!colnames(REV)%in%c("stars","usr.count")]/2
x <- cBind(r,CAT,GEO,BIZ)

notext <- gamlr(cBind(d,x),y,free=1+1:ncol(x),gamma=10)
coef(notext)["d",]
# [1] 0.01210891

xz <- cBind(x,as.matrix(z[,c("stars","usr.count")]),
	cBind(r,CAT)*z$stars,cBind(r,CAT)*z$usr.count)
# > ncol(xz)
# [1] 12624
# vs 11938 + 13938*(1+342) = 4792672 (approx 5 million).

withtext <- gamlr(cBind(d,xz),y,free=1+1:ncol(xz),gamma=10)
coef(withtext)["d",]
# [1] 0.02032455










































	