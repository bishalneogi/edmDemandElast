#Load required packages
library(boot)
library(micEconAids)
#Load data, be sure to adjust your working directory
# aidsSystemData3 <- read.csv("aidsSystemData3.csv")
# aidsSystemData3LR <- read.csv("aidsSystemData3LR.csv")
#Use reduced data frame for estimation
load(dat.Rda)
load(datLR.Rda)
#Create vectors of price and share names for AIDS estimation
priceNames <- c("pCorn", "pBarley", "pSoy", "pPeanut", "pCottonSeed",
                "pRice", "pWheat")
shareNames <- c("wCorn", "wBarley", "wSoy", "wPeanuts", "wCottonSeed",
                "wRice", "wWheat")
# Short Run Elasticities ---------------------------------------------------
#Estimate short run elasticities
bestA0 <- aidsBestA0(priceNames, shareNames, "eTotal", data=dat)
estResultSR <- aidsEst(priceNames, shareNames, "eTotal", data = dat, 
                     priceIndex = "S", method = "LA", alpha0 = bestA0$alpha0, restrict.regMat = TRUE)#restrict.regMat = TRUE forces consistency with theory.  Probably best to set to FALSE and check that.
wMeans <- colMeans(dat[,shareNames])
SR <- aidsElas(estResult$coef, shares = wMeans, priceIndex = "S", method = "Ch")
#Check SR results for consistency with demand theory
checkConsist(estResultSR)
# bootstrapping with 1000 replications 
#Create a function for the boot package to use
elast <- function(data = dat, indices ){
    d <- data[indices,]
    return(aidsElas(aidsEst(priceNames, shareNames, "eTotal", d , priceIndex = "S", method = "LA", alpha0 = aidsBestA0(priceNames, shareNames, "eTotal", priceNames ,shareNames , colMeans(d[,shareNames]) , d)$alpha0, restrict.regMat = TRUE)$coef, shares = colMeans(d[,shareNames]), priceIndex = "S", method = "Ch")$marshall)
}
# Use the boot package to calculate std. error.
resultsSR <- boot(data=dat, statistic=elast, 
                R=1000)
resultsSR
plot(results, index=1) # intercept 
plot(results, index=2) # wt 
plot(results, index=3) # disp 

## Long Run Elasticities-----------------------
#Long-Run Elasticity Estimation
#price lagged 10 years
bestA0LR <- aidsBestA0(priceNames, shareNames, "eTotal", data=datLR)
estResultLR <- aidsEst(priceNames, shareNames, "eTotal", data = datLR, priceIndex = "S", method = "LA", alpha0 = bestA0$alpha0LR, restrict.regMat = TRUE)
wMeans <- colMeans(datLR[,shareNames])
#LR
LR <- aidsElas(estResultLR$coef, shares = wMeans, method = "Ch", priceIndex = "S")
checkConsist(estResultLR)
# bootstrapping with 1000 replications 
#Create a function for the boot package to use
elastLR <- function(data = datLR, indices ){
    d <- data[indices,]
    return(aidsElas(aidsEst(priceNames, shareNames, "eTotal", d , priceIndex = "S", method = "LA", alpha0 = aidsBestA0(priceNames, shareNames, "eTotal", priceNames ,shareNames , colMeans(d[,shareNames]) , d)$alpha0, restrict.regMat = TRUE)$coef, shares = colMeans(d[,shareNames]), priceIndex = "S", method = "Ch")$marshall)
}
# Use the boot package to calculate std. error.
resultsLR <- boot(data=datLR, statistic=elastLR, 
                  R=1000)
resultsLR
plot(results, index=1) # intercept 
plot(results, index=2) # wt 
plot(results, index=3) # disp 
