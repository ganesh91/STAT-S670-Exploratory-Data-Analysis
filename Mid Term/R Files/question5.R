#--Input Values
x<-c(0.450,0.45,0.450,1.300,1.300,1.300,2.400,2.400,2.400,4.000,4.00,4.000,6.100,6.100,6.100,8.05,8.050,8.050,11.150,11.150,11.150,13.150,13.150,13.150,15.000,15.00,15.000)
y<-c(0.342,0.00,0.825,1.780,0.954,0.641,1.751,1.275,1.173,3.123,2.61,2.574,3.179,3.008,2.671,3.06,3.943,3.437,4.807,3.356,2.783,5.138,4.703,4.257,3.604,4.15,3.425)
#- 5 a RR on plot a
rrline.x<-rrline1(x,y)
plot(x,y,main="Scatter plot of time and cal",xlab = "time",ylab = "cal")
abline(rrline.x$a,rrline.x$b)
rrline.x$a
rrline.x$b
#Slope=0.26,intercept=0.88
#----------

#--- 5b Pending
#-----------


#-- 5c --- Square Root Transforamtion---
sqrtx<-sqrt(x)
plot(sqrtx,y,main="Transformed plot",xlab="log(x)",ylab="y")
---------------
  
#----5d --- RR line on Transformed data
rrline.sqrtx<-rrline1(sqrtx,y)
abline(rrline.sqrtx$a,rrline.sqrtx$b)
#------------------

#-- 5 e------- COmment on Residual plots
rrline.x$sumres
rrline.sqrtx$sumres
#direct:16.38401
#transformed:13.13
#Transformed plot looks more transformed than the other one
#---5 f------ Residual plot d--------
plot(rrline.sqrtx$fit,rrline.sqrtx$res,main="Residual Plot for Transformed X",xlab = "Fitted Values",ylab="Residuals")
abline(0,0,lty=2)
rr.iter1<-run.rrline(rrline.sqrtx$fit,rrline.sqrtx$res,iter = 1)
abline(rr.iter1$a,rr.iter1$b,col="blue")
rr.iter1$a
rr.iter1$b
rr.iter3<-run.rrline(rrline.sqrtx$fit,rrline.sqrtx$res,iter = 5)
abline(rr.iter3$a,rr.iter3$b,col="red")
rr.iter3$a
rr.iter3$b
# The above ones are actual values
# Hypothetically,
source("myplotfit.r")
source("rrline.r")
x<-c(0.450,0.45,0.450,1.300,1.300,1.300,2.400,2.400,2.400,4.000,4.00,
     4.000,6.100,6.100,6.100,8.05,8.050,8.050,11.150,11.150,11.150,
     13.150,13.150,13.150,15.000,15.00,15.000)
y<-c(0.342,0.00,0.825,1.780,0.954,0.641,1.751,1.275,1.173,3.123,2.61,
     2.574,3.179,3.008,2.671,3.06,3.943,3.437,4.807,3.356,2.783,5.138,
     4.703,4.257,3.604,4.15,3.425)

intercept <- 0.256
slope <- -0.124
residuals <- y-slope*x-intercept
#Iteration 1
rr1<-rrline1(x,residuals)
paste("Intercept : ",rr1$a," Slope : ",rr1$b)
#Iteration 2
residuals <- y - rr1$b - rr1$b
rr1<-rrline1(x,residuals)
paste("Intercept : ",rr1$a," Slope : ",rr1$b)