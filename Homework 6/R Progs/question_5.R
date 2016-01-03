source("rrline.r")

cval.prog <- function (data,n.folds)
{
  n <- length(data$x)
  floorlen <- floor(n/n.folds)
  folds <- rep(1:n.folds,floorlen)
  k <- n - length(folds)
  
  if(k>0){
    #Create 1:k till k times
    folds = c(folds,1:k)
  }
  
  foldindicies = sample(folds,n,replace=FALSE)
  fit = run.rrline(data$x, data$y)
  theta = c(fit$a, fit$b)
  stat = numeric(n.folds)
  cverr = numeric(n.folds)
  cv.mat = matrix(0, n.folds, 2)
  for (i in 1:n.folds)
  {
    b = foldindicies == i
    cv = run.rrline(data[b,]$x, data[b,]$y)
    cv.mat[i,1] = cv$a
    cv.mat[i,2] = cv$b
    oobdata = data[!b,]
    pred = oobdata$x*cv$b + cv$a
    cverr[i] = sum((oobdata$y - pred)^2)/nrow(data[!b,])
  }
  mean.theta = apply(cv.mat, 2, mean)
  bias = theta - mean.theta
  var = diag(cov(cv.mat))
  se = sqrt(var)
  avgcverr = mean(cverr)
  return(out = list(boot.a = mean.theta[[1]], boot.b = mean.theta[[2]], bias = bias, var = var, se=se, avgcverr=avgcverr))
}

attach(faithful)
data.input = data.frame(waiting,eruptions)
names(data.input) = c("x", "y")
cval.res = cval.prog(data.input, 3)
print(cval.res)

#It can be note that the program runs RR run program runs three times, which is as expected.
