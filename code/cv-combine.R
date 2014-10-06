## read the worker ouput and sum
J <- Sys.getenv("SLURM_JOB_NAME")

library(methods)
library(lattice)
library(Matrix)
library(gamlr)

cat(sprintf("combining @ %s\n",date()))

foldid <- readRDS(sprintf("results/%s/data/foldid.rds",J))
K <- max(foldid)

for(k in 1:K){
	Z <- 0
	for(z in Sys.glob(sprintf("results/%s/data/z%d-*.rds",J,k))){
		Z <- Z + readRDS(z) }
	Z <- as.matrix(Z)
	write.table(Z,row.names=FALSE, file=sprintf("results/%s/z%d.txt",J,k), sep="|",quote=FALSE)
	rm(Z)
}

cat(sprintf("predicting @ %s\n",date()))

library(distrom)
load("data/meta.rda")
votes <- c("funny","useful","cool")
X <- cBind(REV[,!colnames(REV)%in%votes],CAT,GEO,BIZ,m)
dtm <- readRDS("data/text.rds")
Xdtm <- cBind(X,dtm)

r2 <- as.data.frame(matrix(nrow=K,ncol=6,
	dimnames=list(1:K,
		paste(rep(votes,each=2),c("dtm","z"),sep="."))))
for(k in 1:K){
	train <- which(foldid!=k)
	Z <- as.matrix(read.table(
		sprintf("results/%s/z%d.txt",J,k),
		header=TRUE,sep="|")[,votes])
	r2k <- c()
	for(v in votes){
		y <- REV[,v]
		vary <- var(y[-train])
		z <- Z[,v]
		Xz <- cBind(X,z)

		fitdtm <- gamlr(x=Xdtm[train,],y=y[train],lambda.min.ratio=1e-3)		
		fitz <- gamlr(x=Xz[train,],y=y[train],lambda.min.ratio=1e-3)

		edtm <- predict(fitdtm, Xdtm[-train,])[,1]-y[-train]
		ez <- predict(fitz, Xz[-train,])[,1]-y[-train]

		r2k <- c(r2k, 
			1 - c(var(edtm),var(ez))/vary)
		cat(k,v,"\n")
	}
	r2[k,] <- r2k
}
write.table(r2, file=sprintf("results/%s/r2.txt",J),
			row.name=FALSE, sep="|")
print(round(colMeans(r2),3))

pdf(sprintf("results/%s/egfwdfits.pdf",J),width=8,height=5)
par(mfrow=c(1,2))
plot(fitdtm, main="DTM")
plot(fitz, main="Z")
dev.off()

cat(sprintf("done @ %s\n",date()))


