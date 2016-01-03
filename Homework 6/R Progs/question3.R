x <- c(576,635,558,578,666,580,555,661,651,605,653,575,545,572,594)
y <- c(339,330,281,303,344,307,300,343,336,313,312,274,276,288,296)
data.df <- data.frame(x, y)

#Find Pearson correlation
corr.df <- cor(x, y, method = c("pearson"))
#------- 3 (a)
#Estimate theta user fischer's formula
theta <- 0.5*((1+corr.df)/(1-corr.df))
var.df<-1/(length(x)-3)
ci.l<-corr.df-1.96*var.df
ci.u<-corr.df+1.96*var.df

#-- Part B
jackknife.mean<-function(dat){
  n=nrow(dat)
  corr.df <- cor(dat$x, dat$y, method = c("pearson"))
  theta<-0.5*log((1+corr.df)/(1-corr.df))
  p.values <- numeric(n)
  est.theta <- numeric(n)
  for (i in 1:n){
    new.data<-dat[-i,]
    new.corr<-cor(new.data$x,new.data$y,method = c("pearson"))
    est.theta[i] <- 0.5*log((1+new.corr)/(1-new.corr))
    p.values[i] <- n*theta - (n-1)*est.theta[i]
  }
  bias = theta - mean(p.values)
  variance = var(p.values)
  se = sd(p.values)/sqrt(n)
  ci.lower = mean(p.values) - qt(0.975, df = (n-1))*se
  ci.upper = mean(p.values) + qt(0.975, df = (n-1))*se
  return(list(bias=bias, var = variance, ps = p.values,ci=c(ci.lower,ci.upper)))
}

#---Part(c)
jackknife.theta <- jackknife.mean(data.df)
print(jackknife.theta)

stem(jackknife.theta$ps)
plot(jackknife.theta$ps)
jack.df2 <- data.df[-1,]
jack.mean2 <- jackknife.mean(jack.df2)
plot(jack.mean2$ps)

#----part(d)
bootstrap.est <- function (dat,n.sam)
{
  n <- length(dat$x)
  index <- 1:n
  rho <- cor(dat$x, dat$y, method = c("pearson"))
  theta <- 0.5*log((1 + rho)/(1 - rho))
  stat <- numeric(n.sam)
  oob.err <- numeric(n.sam)
  for (i in 1:n.sam)
  {
    sampleindex<- sample(index,n,replace<-TRUE)
    stat[i] <- cor(dat$x[sampleindex], dat$y[sampleindex],method=c("pearson"))
    oob.i <- setdiff(index,unique(sampleindex))
    oob.dat <- dat[oob.i,]
    oob.err[i] <- sum((oob.dat-stat[i])^2)/length(oob.i)
  }
  bias <- theta - mean(stat)
  variance <- var(stat)
  se <- sqrt(variance)
  avg.oob.err <- mean(oob.err)
  output <- list(bias=bias,var=variance,se=se,avg.oob.err=avg.oob.err)
}
boot.est<-bootstrap.est(data.df,10)
print(boot.est)