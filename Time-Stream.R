# Xinxin Yang
# Student id: 25715
# Email: moshuixinxin@gmail.com
#--------------------------------------------
# Algorithm 2: Time-Stream #
#--------------------------------------------
# DS is the input data set
# m are blocks of whole data set
# Md is a regression model for Dataset DS
#-------------------------------------------

timeStream<- function (DS, m, Md)
{
  #print(paste0("Total data blocks: ", m))
  
  # getting the column names of data set
  colNames = names(DS)
  
  # getting the number of columns in the data set
  
  numCol = ncol(DS)
  
  #print(paste0("Total data rows: ", nrow(DS)))
  
  # n is the sample size of each block (or how many rows a block contains)
  n = nrow(DS)/m
  
  # first block 
  model = lm (Md, data = DS[1:n,])
  
  # beta coefficient of first block
  beta= model$coefficients

  # initialize a empty beta matrix
  coefIter <- matrix(NA, nrow = ncol(DS), ncol = m)
  coefIter[,1]= beta

  # initialize a empty X matrix
  X_new = data.frame(matrix(NA, ncol = ncol(DS)-1, nrow = n))

  for (i in 2:m) {
    
    DT <-DS[((i-1)*n+1): (n*i),] # all cols
    
    y_new = DT[,1]

    # updating the new X
    for (j in 1:n)
    {
      x_new = j*DT[j,2:numCol]
      X_new[j,] = x_new
    }
    
    # combine the y and X variables
    newDS = cbind(y_new,X_new)
    
    # changing the column names of new data set
    colnames(newDS) = colNames
    
    # setting a new model
    fit.model = lm(Md, data = newDS)

    summary = summary(fit.model)
    
    #getting the coef.
    coef = fit.model$coefficients
    coefIter[,i] = coef
    rownames(coefIter) <- c("(Intercept)", colnames(DS)[2:ncol(DS)])
    
    #mse
    mse =  mean(fit.model$residuals^2)
    
    # stores the summary of the model and the coefficients
    result <- list("summary"=summary, "coef" = coefIter )
  }
  return (result)
}
