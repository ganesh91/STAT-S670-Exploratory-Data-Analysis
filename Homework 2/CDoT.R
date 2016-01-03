library(aplpack)
library(outliers)
A<-c(19.50,16.72,20.92,16.42,21.22,15.40,20.68,14.55,20.23,
     15.11,20.95,16.68,14.67,16.50,22.15,20.14,18.33,14.20,
     11.61,22.24,18.75,14.22,15.03,22.07,13.34,12.73,19.23,
     19.74,19.74,20.60,19.29,18.22,23.65,17.44,13.07,19.00,
     18.44,17.25,19.19,12.77,14.10,16.69,16.92,21.92,20.84,
     18.43,19.54,23.61,21.40,28.34,20.43,20.43,15.58,16.58,
     22.44,14.59,18.70,16.79,14.12,13.67,15.94,24.04,15.42,
     16.26,17.74,12.37,16.87,16.28,17.97,19.56,13.56,16.13,
     18.20,17.29,19.38,20.47,16.75,16.69,15.93,14.73,17.83,
     19.78,15.78,16.17,17.18,13.90,15.33,16.10,12.03,17.92,
     23.56,11.35,19.10,12.91,18.32,19.24,11.57,14.33,13.60,
     13.12,11.19,14.33,16.91,13.03,17.32,10.70,12.56,16.04)
B<- ts(A,frequency = 12,start=c(1997,1))
plot(B,main="Monthly Injury Rates",xlab="Years",ylab="Injury Rate")
tsDecompose <- decompose(B)
plot(tsDecompose)
stem.leaf(B,rule.line = "Dixon")
hist(B,main = "Histogram of Injury Rates",xlab="Injury Rates",freq = F)
lines(density(B,kernel = "epanechnikov"),col=2,lty=2)
legend("topright",legend = c("Distribution Density"),
  col = c(2),lty = 2,cex = 0.5)
qqnorm(B,main="QQ Plot for DoT Dataset")
qqline(B,lty=2,col=2)
outlier(B)