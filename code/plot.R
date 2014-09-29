## plots and summary
library(distrom)

pw <- 200
plotfits <- function(pw, )

fit <- readRDS("results/mn20/9876649-data/fit120.rds")
par(mfrow=c(2,2))
plot(fit[[1]],main=names(fit)[1])
plot(fit[[12]],main=names(fit)[12])
plot(fit[[50]],main=names(fit)[50])
plot(fit[[93]],main=names(fit)[93])
dev.off()
