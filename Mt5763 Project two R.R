#Generating some random data to work with.
set.seed(1)
n <- 100
x <- runif(n)
y <- x + 1 + rexp(n)


#Data for use with vectorisation needs to be ordered: Intercept, covariates, dependent variable.
#Also must be in a datamatrix rather than a dataframe.
data.m <- cbind("(Intercept)" = 1, x, y)
data.f <- data.frame(x,y)


#~~~~~~~~~~Vectorisation~~~~~~~~~~~~~~~~~~~~~~~~~

boot.lm.vector <- function(index, inputData) {
  
  #Random sampling from input data with replacement.
  d <- inputData[sample.int(nrow(inputData), replace = T),]
  
  #Define matrix with covariates and intercept.
  a <- ncol(inputData)-1
  X <- d[, 1:a]
  
  #Vector with dependent variable.
  y <- d[, a+1]
  
  #Solve for coefficients. Solve requires square matrix, hence use of crossprod
  solve(crossprod(X), crossprod(X,y))
  
}

system.time(r1 <- t(replicate(10000, boot.lm.vector(1, data.m))[,1,]))


a <- c(mean(r1[,1]), mean(r1[,2]))
a
#~~~~~~~~~~~~~~~~Parallelising~~~~~~~~~~~~~~~~~~~~~

library(parallel)
library(doParallel)
nCores <- detectCores()
myClust <- makeCluster(nCores-1, type = "PSOCK")


system.time(rtest <- parLapply(myClust, 1:10000, fun = boot.lm.vector, inputData = data.m)) 


#Note that this method for obtaining the results works for one covariate. 
#Will need to adapt to retrieve the values for multiple covariates.

rtestdf <- plyr::ldply(rtest)
mylist <- rtestdf[,1]
resultsHolder <- numeric(ncol(data.m)-1)
for(i in 1:ncol(data.m)-1){
  resultsHolder[i] <- mean(mylist[seq(i,nrow(rtestdf),ncol(data.m)-1)])
}
resultsHolder





#~~~~~~~~~~~~~Carl's function~~~~~~~~~~~~~~~~~~~~~~

lmBoot <- function(inputData, nBoot){
  
  for(i in 1:nBoot){
    
    # resample our data with replacement
    bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
    
    # fit the model under this alternative reality
    bootLM <- lm(y ~ x, data = bootData)
    
    # store the coefs
    if(i == 1){
      
      bootResults <- matrix(coef(bootLM), ncol = 2)
      
    } else {
      
      bootResults<- rbind(bootResults, matrix(coef(bootLM), ncol = 2))
      
    }
    
    
  } # end of i loop
  
  bootResults
  
}

system.time(r2 <- lmBoot(data.f, 10000))




#~~~~~~~~~Microbenchmarking vs boot package~~~~~~~~~~~~~~~~~~~~~

library(microbenchmark)
library(boot)

bootpkg.lm <- function(formula, data, indices){
  d <- data[indices, ]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

#system.time(results <- boot(data = data.f, statistic = bootpkg.lm, R=10000, formula = y ~ x))
#results

#Now performing the microbenchmarking. Only using 1000 bootstraps and 10 repititions due to how long
#the boot() expression takes (I did wait it out once, results were similar for 100 repitions).
microbenchmark(parLapply(myClust, 1:1000, fun = boot.lm.vector, inputData = data.m), times = 10)
microbenchmark(boot(data = data.f, statistic = bootpkg.lm, R=1000, formula = y ~.), times = 10)
