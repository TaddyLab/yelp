## read for pred
cat("\nread data...\n")

Z <- readRDS("results/Z.rds")
M <- Z[,'m'] 
Z <- Z[,1:(ncol(Z)-1)]

## drops for pred
zdrp <- c("usr.stars","usr.count","usr.funny","usr.useful","usr.cool")

Z <- Z[,-which(colnames(Z)%in%zdrp)]
print(colnames(Z))

load("data/covars.rda")
V <- V[,-which(colnames(V)%in%colnames(Z))]

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
  Z <- Z[,-which(colnames(Z)%in%zdrp)]
  print(colnames(Z))
  Z
}

cat("done.\n")

