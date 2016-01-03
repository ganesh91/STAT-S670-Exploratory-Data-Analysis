plotTransformBatch<-function(inlist){
  #inlist<-(CA$population)
  source("lvalprogs.r")
  tab <- as.data.frame(lval(inlist))
  M <- tab$Lower[1];
  spreads <- tab[tab$Spread>0,1:5]
  xaxis <- as.numeric()
  yaxis <- as.numeric()
  pestimate <- as.numeric()
  for (i in seq(1:length(spreads$Mid))){
    xaxis<-c(xaxis,((spreads$Upper[i]-M)^2+(M-spreads$Lower[i])^2)/(4*M))
    yaxis<-c(yaxis,(((spreads$Upper[i]+spreads$Lower[i])/2)-M))
    pestimate <- c(pestimate,1-(yaxis[i]/xaxis[i]))
  }
  spreads["xaxis"]<-xaxis
  spreads["yaxis"]<-yaxis
  spreads["pestimate"]<-pestimate
  spreads<-spreads[complete.cases(spreads),]
  print(spreads)
  plot(spreads$xaxis,spreads$yaxis,main="Spread vs Level Plot",xlab="x estimates",ylab="Y estimates")
  y<-lm(spreads$yaxis~spreads$xaxis)
  abline(y,lty=2,col="blue")
  print(paste("The power is ",1-coefficients(y)[2]))
  print(paste("The slope is ",coefficients(y)[2]))
  #return (spreads)
}