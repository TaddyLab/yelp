## read for pred
cat("\nread data...\n")

Z <- readRDS("results/Z.rds")

## drops for pred
yvar <- c("funny","useful","cool",       
 	"funny:days","funny:days2","useful:days",
 	"useful:days2","cool:days","cool:days2")
dvar <-c("usr.stars",
	"usr.count","usr.funny",
	"usr.useful","usr.cool")
zget <- c(yvar,dvar)

load("data/covars.rda")
V <- V[,-which(colnames(V)%in%yvar)]

ratings <- readRDS("data/ratings.rds")
Y <- rowSums(ratings[,-1])
lY <- log(Y+1)
print(nobs <- length(Y))

## define utility for cv
buildz <- function(k){
  Z <- 0
  for(z in Sys.glob(sprintf(
         "data/cvout/fold%03dz*.rds",k)))
	Z <- Z + readRDS(z)
  print(colnames(Z))
  Z
}

cat("done.\n")

