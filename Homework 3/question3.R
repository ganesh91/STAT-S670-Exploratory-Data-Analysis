#Transformation plots
xlf<-function(xl,xu,median){
  return (((xl+xu)/2)-median)
}
ylf<-function(xl,xu,median){
  return (((xu-median)^2+(median-xl)^2)/(4*median))
}

median<-c(179140.5)
letters<-c("F","E","D","C","B","A")
xl<-c(45578.0,20007.0,13994.0,6463.0,2207.5,1175.0)
xu<-c(685306.0,1418788.0,2112425.5,3052772.5,6456959.0,9818605.0)
xlab <- as.numeric()
ylab <- as.numeric()
p <- as.numeric()
for (i in seq(1:length(letters))){
  ytemp<-ylf(xl[i],xu[i],median)
  xtemp<-xlf(xl[i],xu[i],median)
  ylab <- c(ylab,ytemp)
  xlab <- c(xlab,xtemp)
  p<-c(p,(1-(ytemp/xtemp)))
}
estimator<-cbind(letters,xl,xu,xlab,ylab,p)
print(estimator)
