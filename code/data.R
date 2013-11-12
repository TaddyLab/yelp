## yelp data proc and output

## read and build source data
rev <- read.table("data/yelp_rev_attr.txt", sep="\t",
	colClasses=c("integer",rep("character",4),rep("integer",4)), row.names=2,
	col.names=c("rkey","review","user","business","date","stars","funny","useful","cool"))
usr <- read.table("data/yelp_usr.txt", sep="\t", row.names=1,
	colClasses=c("character","numeric",rep("integer",4)),
	col.names=c("user","usr.stars","usr.count","usr.funny","usr.useful","usr.cool"))
biz <- read.table("data/yelp_biz_attr.txt", sep="\t", row.names=2,
	colClasses=c("integer",rep("character",3),rep("numeric",2)),
	col.names=c("bkey","business","city","state","biz.stars","biz.count"))

txt <- read.table("data/yelp_rev_text.txt", comment.char="", quote="",
	colClasses=c("integer","factor","integer"), sep="\t",
	col.names=c("rkey","term","count"))
cat <- read.table("data/yelp_biz_cats.txt", comment.char="", quote="",
	colClasses=c("integer","factor"), sep="\t",
	col.names=c("bkey","category"))

library(Matrix)
X <- sparseMatrix(i=as.numeric(txt$rkey),j=as.numeric(txt$term),x=txt$count,
		dimnames=list(rownames(rev),levels(txt$term)))
C <- sparseMatrix(i=as.numeric(cat$bkey),j=as.numeric(cat$category),
		dimnames=list(rownames(biz),levels(cat$category)))

## grab only reviews with known users
ru <- rev$user%in%rownames(usr)
rev <- rev[ru,] 
X <- X[ru,]
## drop rare terms and categories
X <- X[,colSums(X>0)>20]
C <- C[,colSums(C)>5] 

## grab votes
ratings <- rev[,c('stars','funny','useful','cool')]
saveRDS(ratings, file="data/ratings.rds",compress=FALSE)

## processing covariates

rev$days <- as.numeric(as.Date("2013-01-19")) - as.numeric(as.Date(rev$date))
rev$days2 <- rev$days^2
rev$stars.overusr <- rev$stars-usr[rev$user,'usr.stars']
rev$stars.overbiz <- rev$stars-biz[rev$business,'biz.stars']

rr <- scale(model.matrix( ~ stars + stars.overusr + stars.overbiz +
			(funny+useful+cool)*(days + days2), data=rev)[,-1])

uu <- usr[rev$user,]
uu[,3:5] <- uu[,3:5]-rev[,c('funny','useful','cool')]
uu[,3:5] <- uu[,3:5]/sapply(uu[,2],function(u) max(1,u))
uu <- scale(uu)

biz$city <- factor(biz$city,levels=c("!",unique(biz$city)))
biz$state <- factor(biz$state,levels=c("!",unique(biz$state)))
biz <- biz[rev$business,]
biz[,5] <- biz[,5]-1
biz[,4:5] <- scale(biz[,4:5])
bb <- model.matrix(~ biz.stars + biz.count + city + state, data=biz)[,-1]

R <- cbind(rr,uu,bb)
V <- cBind(R,as(C[rev$business,],"dgCMatrix"))
any(rownames(V)!=rownames(X))

## store V and mu
mu <- log(rowSums(X)+1)
save(V, mu, file="data/covars.rda", compress=FALSE)
saveRDS(X, file="data/x.rds", compress=FALSE)

## convert X to list and store
N <- 64
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
}

