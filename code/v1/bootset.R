library(lattice)
library(Matrix)
library(gamlr)
library(parallel)
library(dmr)

B <- 100

load("data/covars.rda")
ratings <- readRDS("data/ratings.rds")
M <- exp(mu)-1
Y <- rowSums(ratings[,-1])

yvar <- c("funny","useful","cool",       
 	"funny:days","funny:days2","useful:days",
 	"useful:days2","cool:days","cool:days2")
dvar <-c("usr.stars",
	"usr.count","usr.funny",
	"usr.useful","usr.cool")
zget <- c(yvar,dvar)

set.seed(5807)
n <- nrow(V)
