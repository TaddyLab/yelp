
cat("\nset foldid\n")
set.seed(5807) ## should be safe...
nfold <- 20
rando <- sample.int(nobs)
print(rando[10^{0:5}])
chunks <- round(seq.int(0,nobs,length.out=nfold+1))
foldid <- rep.int(1:nfold,times=diff(chunks))[rando]

