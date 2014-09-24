## yelp data proc and output

## read source data from tokenize.py
rev <- read.table("data/yelp_rev_attr.txt", sep="\t",
	colClasses=c("integer",rep("character",4),rep("integer",4)), row.names=2,
	col.names=c("rkey","review","user","business","date","stars","funny","useful","cool"))
usr <- read.table("data/yelp_usr.txt", sep="\t", row.names=1,
	colClasses=c("character","numeric",rep("integer",4)),
	col.names=c("user","usr.stars","usr.count","usr.funny","usr.useful","usr.cool"))
biz <- read.table("data/yelp_biz_attr.txt", sep="\t", row.names=2,
	colClasses=c("integer","character",rep("factor",2),rep("numeric",2)),
	col.names=c("bkey","business","city","state","biz.stars","biz.count"))
biz$city <- relevel(biz$city,"Phoenix")

txt <- read.table("data/yelp_rev_text.txt", comment.char="", quote="",
	colClasses=c("integer","factor","integer"), sep="\t",
	col.names=c("rkey","term","count"))
cat <- read.table("data/yelp_biz_cats.txt", comment.char="", quote="",
	colClasses=c("integer","factor"), sep="\t",
	col.names=c("bkey","category"))

## turn review text into sparse matrix
library(Matrix)
X <- sparseMatrix(i=as.numeric(txt$rkey),j=as.numeric(txt$term),x=txt$count,
		dimnames=list(rownames(rev),levels(txt$term)))

## limit to only reviews with known users
ru <- rev$user%in%rownames(usr)
rev <- rev[ru,] 
X <- X[ru,]
X <- X[,colSums(X>0)>20]
m <- rowSums(X)
X <- X[m>0,]
rev <- rev[m>0,]
m <- m[m>0]

## Construct Covariates
age <- as.numeric(as.Date("2013-01-19")) - as.numeric(as.Date(rev$date))
uid <- rev$user
bid <- rev$business
n <- length(uid)

BIZ <- sparseMatrix(i=1:n, j=as.numeric(biz[bid,"bkey"]),x=rep(1,n),
		dimnames=list(NULL, bkey=paste("biz",1:max(biz$bkey),sep="")))
BIZ <- BIZ[,colSums(BIZ)!=0]

CAT <- sparseMatrix(i=as.numeric(cat$bkey),
		j=as.numeric(cat$category),
		x=rep(1,nrow(cat)),
		dimnames=list(rownames(biz),levels(cat$category)))
CAT <- CAT[rev$business,colSums(CAT)>5] 
rownames(CAT) <- NULL

GEO <- sparseMatrix(i=1:n, j=as.numeric(biz[bid,"city"]),x=rep(1,n),
		dimnames=list(NULL, city=levels(biz$city)))

REV <- scale(data.frame( 
		funny = rev$funny/sqrt(age),
 		useful = rev$useful/sqrt(age),
 		cool = rev$cool/sqrt(age),
 		stars  = rev$stars-biz[bid,"biz.stars"],
 		usr.funny = usr[uid,"usr.funny"]/usr[uid,"usr.count"],
 		usr.useful = usr[uid,"usr.useful"]/usr[uid,"usr.count"],
 		usr.cool = usr[uid,"usr.cool"]/usr[uid,"usr.count"],
 		usr.stars = usr[uid,"usr.stars"]-3.75,
 		usr.count = usr[uid,"usr.count"]-ave(age, uid, FUN=rank),
 		biz.stars = biz[bid,"biz.stars"]-3.75,
 		biz.count = biz[bid,"biz.count"]))
names(uid) <- rownames(rev)

## store the objects
save(BIZ,CAT,GEO,REV,uid,m,n,file="data/meta.rda", compress=FALSE)
saveRDS(X, file="data/text.rds", compress=FALSE)

## convert X to list and store in chunks
N <- 128
system("rm -rf data/x/")
system("mkdir data/x")
p <- ncol(X)
rownames(X) <- NULL
chunks <- round(seq.int(0,p,length=N+1))
print(chunks)
for(i in 1:N){
	x <- X[,(chunks[i]+1):chunks[i+1]]
	attr(x, 'part') <- i
	saveRDS(x, file=sprintf("data/x/part%03d.rds",i), compress=FALSE)
	cat(i," ")
}






