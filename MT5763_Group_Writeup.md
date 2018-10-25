The new function uses a vectorised form of linear regression to produce
the coefficients. The input data must have it's columns ordered
"Intercept", "Covariates", "Dependent Variable", ie we have

<table>
<thead>
<tr class="header">
<th>Intercept</th>
<th>x1</th>
<th>x2</th>
<th>...</th>
<th>xn</th>
<th>y</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>1</td>
<td>x11</td>
<td>x12</td>
<td>...</td>
<td>x1n</td>
<td>y1</td>
</tr>
</tbody>
</table>

and so on. From this we construct the matrix X which includes the
intercept and covariates:

$$
\\textbf{X} = \\begin{pmatrix} 1 & x\_{11} & x\_{12}  & \\cdots & x\_{1n} \\\\
1 & x\_{21} & x\_{22}  & \\cdots & x\_{2n} \\\\
\\vdots & \\vdots & \\vdots & \\ddots & \\vdots & \\\\
1 & x\_{n1} & x\_{n2} & \\cdots & x\_{nn}
\\end{pmatrix}
$$
 and the vector y that includes the dependent variable:

$$
y = \\begin{pmatrix} y\_{1} \\\\ y \_{2} \\\\ \\vdots \\\\ y\_{n} \\end{pmatrix}
$$
 We wish to solve the matrix equation
**X***z* = *y*
 Where *z* is a vector containing our regression coefficients, ie if we
compute the first line of this calculation we find:

*z*<sub>1</sub> + *z*<sub>2</sub>*x*<sub>11</sub> + *z*<sub>3</sub>*x*<sub>12</sub> + ⋯ + *z*<sub>*n*</sub>*x*<sub>1*n*</sub> = *y*<sub>1</sub>
 Solving over the entire dataset gives us a good idea of what the values
of *z*<sub>*i*</sub> are. R has the command solve() built in for exactly
this type of problem, however solve() only works if **X** is a square
matrix. To remedy this, we multiply both sides of the equation by the
transpose of **X**. This is what is being done using crossprod(). This
obviously has no effect on the solutions *z*<sub>*i*</sub>, it merely
frames the problem in such a way that solve() is satisfied.

Having covered the mathematical framework, we now look at the code that
actually does this.

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

    #Non parallel method of repeating bootstraps, still produces very respectable times.

    system.time(r1 <- t(replicate(10000, boot.lm.vector(1, data.m))[,1,]))

    ##    user  system elapsed 
    ##   0.547   0.034   0.590

    #a <- c(mean(r1[,1]), mean(r1[,2]))
    #a

    #~~~~~~~~~~~~~~~~Parallelising~~~~~~~~~~~~~~~~~~~~~

    library(parallel)
    library(doParallel)

    ## Loading required package: foreach

    ## Loading required package: iterators

    nCores <- detectCores()
    myClust <- makeCluster(nCores-1, type = "PSOCK")


    system.time(rtest <- parLapply(myClust, 1:10000, fun = boot.lm.vector, inputData = data.m)) 

    ##    user  system elapsed 
    ##   0.027   0.002   0.385
