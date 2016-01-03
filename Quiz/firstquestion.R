#author: Ganesh Nagarajan
#Question 1: The employee productivity Dataset
install.packages("fields")
library(fields)
employee<-1:15
test1<-c(50,35,15,64,53,18,40,24,16,67,46,64,32,71,16)
label1<-rep("Test1",length(test1))
test2<-c(58,46,40,76,62,39,57,41,31,75,62,64,54,65,51)
label2<-rep("Test2",length(test2))
test<-c(test1,test2)
label<-c(label1,label2)
experiment<-as.data.frame(cbind(test,label))
experiment$test<-as.numeric(as.character(experiment$test))
boxplot(experiment$test~experiment$label,notch=TRUE,col=c("green","red"),main="Box Plot for Tests")
#since the notch is above the fourths, the medians are statistically different
# Saved as 1aBoxPlot
source("lvalprogs.r")
lval(test1)
# From the lval program 
#Depth Lower Upper   Mid Spread pseudo-s
#M   8.0  40.0  40.0 40.00    0.0   0.0000
#F   4.5  21.0  58.5 39.75   37.5  27.7988
#E   2.5  16.0  65.5 40.75   49.5  21.5152
#D   1.5  15.5  69.0 42.25   53.5  17.4367
#C   1.0  15.0  71.0 43.00   56.0  15.0317
#Hence COnsidering only the fourths, the psuefo sigma is 27.7988 and median is 40.0

lval(test2)
#Depth Lower Upper   Mid Spread pseudo-s
#M   8.0  57.0  57.0 57.00    0.0   0.0000
#F   4.5  43.5  63.0 53.25   19.5  14.4554
#E   2.5  39.5  70.0 54.75   30.5  13.2568
#D   1.5  35.0  75.5 55.25   40.5  13.1997
#C   1.0  31.0  76.0 53.50   45.0  12.0790
#Henc considering only the fourths, the psuedo sigma is 14.4554 and median is 57.0

#Generate normal distributions
norm1<-rnorm(n = 500,mean = 40,sd=27.7988)
norm2<-rnorm(n=500,mean=57.0,sd=14.4554)
label1<-rep("Test1",length(norm1))
label2<-rep("Test2",length(norm2))
norm<-c(norm1,norm2)
label<-c(label1,label2)
experiment<-cbind(norm,label)
experiment<-as.data.frame(experiment)
experiment$norm<-as.numeric(as.character(experiment$norm))
#saved as 1bboxplotrnorm.r
lval(norm1)
#Depth    Lower    Upper     Mid   Spread pseudo-s
#M 50.5  39.7381  39.7381 39.7381   0.0000   0.0000
#F 125.5  19.6935  58.5935 39.1435  38.9001  28.8367
#E  63.0   7.2071  70.1659 38.6865  62.9588  27.3651
#D  32.0  -5.8083  82.5962 38.3939  88.4045  28.8128
#C  16.5 -12.0893  94.7725 41.3416 106.8617  28.6841
#B   8.5 -19.0019  99.3672 40.1827 118.3691  27.4782
#A   4.5 -24.1353 104.4407 40.1527 128.5760  26.5921
#Z   2.5 -42.2115 114.9871 36.3878 157.1985  29.5478
#Y   1.5 -46.8638 120.9625 37.0494 167.8263  29.0796
#X   1.0 -49.1577 125.1430 37.9926 174.3007  28.1378
lval(norm2)
#Depth   Lower    Upper     Mid  Spread pseudo-s
#M 250.5 57.0504  57.0504 57.0504  0.0000   0.0000
#F 125.5 47.2452  66.9209 57.0831 19.6757  14.5856
#E  63.0 40.1962  72.4154 56.3058 32.2192  14.0041
#D  32.0 35.1657  79.0169 57.0913 43.8511  14.2919
#C  16.5 30.9208  84.5694 57.7451 53.6485  14.4005
#B   8.5 27.6973  91.3087 59.5030 63.6115  14.7668
#A   4.5 26.2662  93.8051 60.0356 67.5389  13.9684
#Z   2.5 24.7697  95.5741 60.1719 70.8044  13.3088
#Y   1.5 23.9078  98.5111 61.2095 74.6032  12.9267
#X   1.0 23.7685 101.3797 62.5741 77.6113  12.5290

# The mid sumamries are increasing in second test normal distriution.
# This illustrates the distribution is skewed right
# However the midsumamries are incresing initially then decreasing for norm1.

