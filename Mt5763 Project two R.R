#Generating some random data to work with.
set.seed(1)
n <- 100
x <- runif(n)
y <- x + 1 + rexp(n)


#Data for use with vector needs to be ordered: Intercept, covariates, dependent variable.
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
registerDoParallel(myClust)
nCores <- detectCores()
myClust <- makeCluster(nCores-1, type = "PSOCK")

system.time(rtest <- parLapply(myClust, 1:10000, fun = boot.lm.vector, inputData = data.m)) 


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


#Need to check results are the same!

#~~~~~~~~~Microbenchmarking vs boot package~~~~~~~~~~~~~~~~~~~~~

library(microbenchmark)
library(boot)

bootpkg.lm <- function(formula, data, indices){
  d <- data[indices, ]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

system.time(results <- boot(data = data.f, statistic = bootpkg.lm, R=10000, formula = y ~ x))


results

