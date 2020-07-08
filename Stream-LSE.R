# Xinxin Yang
# Student id: 25715
# Email: moshuixinxin@gmail.com
#
#-----------------------------------
# Algorithm 1:Stream-LSE #
#-----------------------------------
# X are input variables
# y is the output variable
# m are blocks of whole data
# constant is the learning rate. 
## It has two options: constant or n/N (n is the sample size of a block, N is the whole sample size of observation). 
#--------------------------------------

streamMulti <- function(X,y,m,constant){
  
  # get the column names 
  Colnames <- colnames(X)

  # n is the sample size of each block (or how many rows a block contains)
  n = nrow(X)/m
  
  # initialize beta
  init = 0
  beta <- matrix(init,nrow = ncol(X),ncol = 1) 
  betaIter <- matrix(NA, nrow = ncol(X), ncol = m)
  
  # initialize MSE
  mseIter <- matrix(NA, nrow = m, ncol = 1)
  # initialize R squared 
  rsqIter  <- matrix(NA, nrow = m, ncol = 1)
  # initialize matrix
  yhat <- matrix(init, nrow = 1, ncol = 1)
  zhat <- matrix(init, nrow = ncol(X), ncol = 1)
  z <- matrix(init, nrow = ncol(X), ncol = ncol(X))
  
  # split the whole data into m blocks
  for (i in 1:m) {
    xsamp <-X[((i-1)*n+1): (n*i),]
    ysamp = y[((i-1)*n+1): (n*i),]
    
    message('Coming the block:',i)
  
    # determine the learning rate q 
    if ( is.na(constant) ){
      #print("initial q is not a constant")
      N <- nrow(xsamp)*i
      q <- n/N
    } else {
      #print("constant q")
      q <- constant
    }
    
    message('q value: ', q)
    
    # compute the z, zhat and yhat

    z <- 1/n * q* (t(xsamp) %*% xsamp) + (1-q) * z
    
    zhat <- 1/n * q* (t(xsamp) %*% ysamp) + (1-q) * zhat
    
    yhat <- 1/n * q* (t(ysamp) %*% ysamp) + (1-q) *yhat
    
    # update beta 
    betanew  =  solve(z) %*% (zhat) 

    beta <- betanew
    message('beta is:',beta)
    
    # update the minimization of mse
    
    mse <- yhat - 2*( t(beta) %*% zhat) + (t(beta) %*% z %*% beta)
    
    message('mse is:', mse)
    
    # residual sum of squares
    rss <- nrow(X)*mse
    
    # total sum of squares
    tss <- sum((y- mean(y))^2)
    
    # R-Squared 
    rsq <- 1- (rss/tss)
  
    mseIter[i,] <- mse
    betaIter[,i] <- beta
    rsqIter[i,] <- rsq

  }
  
  # change the rownames of the cofficients
  rownames(betaIter) <- c("(Intercept)", Colnames[2:ncol(X)])
  colnames(betaIter) <- paste0("Block", 1:ncol(betaIter))

  
  # create a data table for storing the MSE and R-Squared
  dt <-setNames(data.table(matrix(nrow = m, ncol = 3)), c("Block", "MSE", "R.Squared"))
  dt$"Block" <- 1:m
  dt$"MSE" <- mseIter
  dt$"R.Squared" <-rsqIter
  
  # stores the mse and the parameters
  result <- list('Coefficients'= betaIter,'summary'= dt)
  #print(xtable(t(result$Coefficients), "Stream-LSE Model"))
  #print(result$summary)
  return(result)
}
