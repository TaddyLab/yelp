## read the worker ouput and sum
library(methods)
library(lattice)
library(Matrix)
library(gamlr)

cat(sprintf("combining @ %s\n",date()))

J <- Sys.getenv("SLURM_JOB_NAME")
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
y <- log(rowSums(REV[,votes])*attributes(REV)$`scaled:scale`[votes]+
		attributes(REV)$`scaled:center`[votes]+1)

mse <- c()
for(k in 1:K){
	train <- which(foldid==k)
	z <- rowSums(read.table(
		sprintf("results/%s/z%d.txt",J,k),
		header=TRUE,sep="|")[,votes])

	linraw <- gamlr(x=cBind(X,dtm)[train,],y=y[train])
	linz <- gamlr(x=cBind(X,z)[train,],y=y[train])
	linz2 <- gamlr(x=cBind(X,z,X*z)[train,],y=y[train])

	yhatlraw <- predict(linraw, cBind(X,dtm)[-train,])[,1]
	yhatlz <- predict(linz, cBind(X,z)[-train,])[,1]
	yhatlz2 <- predict(linz2, cBind(X,z,X*z)[-train,])[,1]

	mse <- rbind(mse,
		matrix(c(var(yhatlraw-y[-train]),
				var(yhatlz-y[-train]),
				var(yhatlz2-y[-train])), 
			nrow=1))
	print(k)
}
colnames(mse) <- c("full text","z","z2")
write.table(mse, file=sprintf("results/%s/mse.txt",J),
		row.name=FALSE, sep="|")

cat(sprintf("done @ %s\n",date()))


