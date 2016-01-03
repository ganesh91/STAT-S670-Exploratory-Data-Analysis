g.dist.estimates<-function(sample.pop){
  source("lvalprogs.r")
  source("rrline.r")
  ll<-lval(sample.pop)
  pp1 <- 1/2^(1:nrow(ll)-1)
  gau1 <- abs(qnorm(pp1))
  pp2 <- abs((pp1-1/3)/(nrow(ll)-1 + 1/3))
  gau2 <- abs(qnorm(abs(pp2)))
  est2.g <- log((ll[,3] - ll[1,2])/(ll[1,2]-ll[,2]))/gau2
  
  # Estimation of g
  est.g <- median(est2.g[-1]) 
  p <- c(0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975, 0.99, 0.995)
  zp <- qnorm(p)
  est.Y <- (exp(est.g*zp)-1)/est.g
  rr <- run.rrline(est.Y,quantile(sample.pop,p))
  #Run Resistant Regression for A and B Estimates
  return (b=list(g=est.g,A=rr$a,B=rr$b))
}


bootstrap.g<-function(pops,sims){
  g.est <- c()
  A.est <- c()
  B.est <- c()
  for (i in 1:sims){
    boot.sample<-sample(pops,length(pops),replace = TRUE)
    r.val <- g.dist.estimates(boot.sample)
    g.est[i]<-r.val$g
    A.est[i]<-r.val$A
    B.est[i]<-r.val$B
  }
  best.g <- mean(g.est)
  g.lower <- best.g - qt(0.975,df=length(g.est)-1)*sd(g.est)
  g.upper <- best.g + qt(0.975,df=length(g.est)-1)*sd(g.est)
  best.A <- mean(A.est)
  A.lower <- best.A - qt(0.975,df=length(A.est)-1)*sd(A.est)
  A.upper <- best.A + qt(0.975,df=length(A.est)-1)*sd(A.est)
  best.B <- mean(B.est)
  B.lower <- best.B - qt(0.975,df=length(B.est)-1)*sd(B.est)
  B.upper <- best.B + qt(0.975,df=length(B.est)-1)*sd(B.est)
  
  return(b=list(g=best.g,a=best.A,b=best.B,g.lower,g.upper,A.lower,A.upper,B.lower,B.upper))
}



a<-c(1092,1137,1197,1237,1301,1523,1577,1619,1626,1644,1672,1748,1768,1780,1796,1816,1843,1844,1902,1919,1983,1993,2025,2028,2032,2036,2072,2078,2090,2137,2162,2163,2180,2185,2194,2225,2230,2233,2234,2235,2265,2270,2274,2281,2289,2319,2322,2357,2381,2398,2421,2421,2443,2522,2549,2552,2581,2618,2618,2620,2624,2642,2647,2666,2705,2721,2740,2804,2819,2823,2860,2873,2906,2913,2926,2929,2931,2931,2934,2939,2961,3020,3023,3044,3047,3048,3096,3174,3190,3199,3204,3222,3225,3278,3287,3292,3300,3339,3361,3412,3462,3503,3530,3589,3672,3734,3749,3783,3854,3901,3932,3995,4001,4006,4118,4134,4320,4346,4385,4401,4522,4565,4581,4593,4629,4855,4868,4878,4885,4907,4962,4975,5021,5127,5155,5160,5183,5229,5242,5379,5383,5513,5555,5619,5755,5774,5890,5899,5988,6161,6185,6818,7406,7419,8175,8220,8282,8827,9027,9042,9805)
bs.val <- bootstrap.g(a,1000)
print(paste("The g Estimate is ",bs.val[1]," and Confidence interval is between",bs.val[4]," and ",bs.val[5]))
print(paste("The A Estimate is ",bs.val[2]," and Confidence interval is between",bs.val[6]," and ",bs.val[7]))
print(paste("The B Estimate is ",bs.val[3]," and Confidence interval is between",bs.val[8]," and ",bs.val[9]))

HDistBackXform=function(h,A,B,data){
  n=length(data)
  output=numeric(n)
  g=function(z){z*exp(h*z^2)-((x-A)/B)}
  # Begin loop on i where data[i] is the ith data value
  for(i in 1:n){
    x=data[i]
    obj=uniroot(g,interval=c(-6,6))
    output[i]=obj$root
  }
  return(output)
}

