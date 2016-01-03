barlettline<-function(x,y){
  n3 <- floor((length(x)+1.99)/3)
  x.order <- order(x)
  meanxL <- mean(x[x.order][1:n3])
  meanxR <- mean(rev(x[x.order])[1:n3])
  meanyL <- mean(y[x.order][1:n3])
  meanyR <- mean(rev(y[x.order])[1:n3])
  slope <- (meanyR - meanyL)/(meanxR - meanxL)
  xmean=mean(x)
  ymean=mean(y)
  intercept=ymean-slope*xmean
  ret_val <- c(intercept,slope)
}