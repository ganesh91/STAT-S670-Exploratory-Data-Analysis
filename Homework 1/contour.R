#Program 1
#Author: Ganesh Nagarajan
#Plots the image plot of function defined in bivariate.
library(fields)
biVariate <- function(x,y) {cos(x)*sin(y)}
plot3d <- function(xVector,yVector){
  x <- xVector
  y <- yVector
  if (length(x) == length(y)){
    plotMatrix <- outer(x,y,biVariate)
    #Replace all NaS in Plot Martix with Zeroes
    plotMatrix[is.na(plotMatrix)]<-0
    #Draw the Image Plot
    par(mfrow=c(1,2))
    image.plot(x,y,plotMatrix,main="Image plot")
    contour(x,y,plotMatrix,main="Contour plot")
  }
  else{
    print("The size of the vectors don't match each other")
  }
}
#Plot the image map and plot the derivatives
plot3dDerivative<- function(xVector,yVector){
  plotMatrix <- outer(xVector,yVector,biVariate)
  image.plot(xVector,yVector,plotMatrix,main="Image plot")
  derivBiVariate <- deriv(~sin(x)*cos(y),  c("x","y"),function(x,y){})
  gradient<-as.data.frame(attributes(
    derivBiVariate(xVector,yVector))$gradient)
  arrows(xVector,yVector,xVector+gradient$x,yVector+
           gradient$y,length = 0.25,col = "red")
}