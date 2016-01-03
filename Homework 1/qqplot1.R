#Program 3
#Author: Ganesh Nagarajan
#Generate two Datasets using rGamma
set.seed(100)
dataSet1 <- rgamma(1000,shape = 1)
dataSet2 <- rgamma(1000,shape = 50)
#Draw the exact density function using curve.
hist(dataSet1,freq=F,ylim =  0+c(0,1),
     main="Exact Density of DS1",xlab = "DS1")
curve(dgamma(x,1),
      0,4,add=T,col="blue")
hist(dataSet2,freq=F,ylim =  0+c(0,0.06),
     main="Exact Density of DS2",xlab = "DS2")
curve(dgamma(x,50),
      0,100,add=T,col="blue")
#Configure the base plotting system for 2x2 plot
par(mfrow = c(2,2))
#Plot the Hitograms followed by the QQ plot
hist(dataSet1,main = "Histogram of DataSet1",xlab = "Data Set 1")
hist(dataSet2,main = "Histogram of DataSet2",xlab = "Data Set 2")
qqnorm(dataSet1,main = "QQ Plot for Data Set 1")
qqnorm(dataSet2,main = "QQ Plot for Data Set 2")
#Sumamry of the DataSet to determine the  mean-median relationship
summary(dataSet1)
summary(dataSet2)
par(mfcol = c(1,2))
#Plot the Histograms followed by the Kernel Distribution Plots for Data Set 1
hist(dataSet1,main = "Kernel Distribution Functions",
     xlab = "Data Set 1",prob = 1)
lines(density(dataSet1,kernel = "gaussian"),col = 2,cex = 0.5)
lines(density(dataSet1,kernel = "rectangular"),col = 3,cex = 0.5)
lines(density(dataSet1,kernel = "triangular"),col = 4,cex = 0.5)
curve(dgamma(x,1),0,4,add=T,col=6)
legend(
  "topright",legend = c("Gaussian","Rectangular","Triangular","Exact Density"),
  col = c(2,3,4,6),lty = 1
)
#Plot the Histogram
hist(dataSet2,main = "Kernel Distribution Functions",
     xlab = "Data Set 2",prob = 1)
lines(density(dataSet2,kernel = "gaussian"),col = 2)
lines(density(dataSet2,kernel = "rectangular"),col = 3)
lines(density(dataSet2,kernel = "triangular"),col = 4)
curve(dgamma(x,50),0,100,add=T,col=6)
legend(
  "topright",legend = c("Gaussian","Rectangular","Triangular","Exact Density"),
  col = c(2,3,4,6),lty = 1,cex = 0.5)
