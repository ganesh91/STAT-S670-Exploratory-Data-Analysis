source("rrline.r")

boot <- function(data, R)
{
  n <- length(data$x)
  index <- 1:n
  #Create a matrix with R rows
  theta <- matrix(0,R,2)
  oob.err <- numeric(R)
  boot.in <- numeric(R)
  fit <- run.rrline(data$x,data$y)
  theta.hat <- c(fit$a,fit$b) 
  for(i in 1:R)
  {
    sample.index = sample(index, n, replace = TRUE)
    inbag <- data[sample.index,]
    oob.ind <- setdiff(index, unique(sample.index))
    oob.data <- data[oob.ind,]
    bootstrapped.y <- run.rrline(inbag$x, inbag$y)
    theta[i,1] <- bootstrapped.y$a
    theta[i,2] <- bootstrapped.y$b
    print(theta)
    print(oob.data$y)
    bootstrap.out <- oob.data$x*bootstrapped.y$b + bootstrapped.y$a
    print(bootstrap.out)
    print(oob.data$y)
    oob.err[i] <- sum((oob.data$y - bootstrap.out)^2)/length(oob.data)
  }
  mean.theta = apply(theta,2,mean)
  bias = theta.hat - mean.theta
  var = diag(cov(theta))
  se = sqrt(var)
  print(oob.err)
  avgooberr = mean(oob.err)
  return(out = list(boot.a = mean.theta[[1]], boot.b = mean.theta[[2]], bias = bias, var = var, se=se, avgooberr=avgooberr))
}
attach(faithful)
data <- data.frame(waiting,eruptions)
names(data) = c("x", "y")
boot.est = boot(data, 10)
print(boot.est)
