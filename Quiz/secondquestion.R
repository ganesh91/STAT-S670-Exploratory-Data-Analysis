data("pressure")
attach(pressure)
dataset<-pressure
y<-dataset$pressure*0.1333
x<-dataset$temperature+273.15
source("plotTransformBatch.R")
plot(x,y,main = "Temperature vs Pressure plot",xlab="Temperature",ylab="pressure")
#We can use square x plot
plotTransformBatch(y)
#Depth  Lower    Upper     Mid   Spread      xaxis    yaxis pestimate
#F   5.5 0.0240  16.8625  8.4432  16.8385   52.74523  7.27025 0.8621629
#E   3.0 0.0008  50.1208 25.0608  50.1200  510.92523 23.88780 0.9532460
#D   2.0 0.0002  74.3814 37.1908  74.3812 1142.55015 36.01780 0.9684760
#C   1.5 0.0001  90.9106 45.4553  90.9105 1716.58409 44.28235 0.9742032
#B   1.0 0.0000 107.4398 53.7199 107.4398 2407.07773 52.54690 0.9781698
#[1] "The power is  0.981490497681884"
#[1] "The slope is  0.0185095023181165"
median1<-median(x)
median2<-median(y)
fit<-lm(y~x)
ycord<-y-median2-fit$coefficients[2]*(x-median1)#Lecture notes page 28 transformation
c<-fit$coefficients[2]
xcord<-c^2*(x-median1)^2
plot(xcord,ycord,main="Transform for straightness")
b<-lm(ycord~xcord)
b$coefficients[2]
1-0.0384317
#Roundinf it, to net 0.5 slope is 0.5, ideally square root transform should be used
plot(x^0.5,y^0.5,main="Transform for straightness")
fit=lm(x^0.5,y^0.5)
abline(fit)
plot(log10(x),log10(y),main="Transform for straightness")
fit=lm(log10(y)~log10(x))
abline(fit)
#slope=-46.95
#intercept=17.61
plot(1/x,log10(y),main="Transform for straightness")
fit=lm(log10(y)~I(1/x))
abline(fit)
#slope=-3173.230
#intercept=7.061
