#--Question 1.a
library(aplpack)
wt2<-c(143,-184,182,-110,1017,986,1010,1001,-111,-60,-151,-111,1024,1031,1028)
summary(wt2)
#--Question 1.b
range.wt2<-(range(wt2)[2]-range(wt2)[1])/length(wt2)
#Round should be the nearest power of 10 which is 100 in our case
stem.leaf(wt2,100)
stem(wt2,2)
boxplot(wt2,horizontal = TRUE,main14="wt2 dataset box plot")
#No There are No outliers.
#--Question 2
noOutliers<-function(x){0.4+(0.007*x)}
noOutliers(5000)
#35.4
set.seed(3)
boxplot(rnorm(5000,mean=5,sd=0.5),horizontal = TRUE,main="Simulation of Random 5000 samples")
#Around 30 points




#####Question 6
setwd("C:\\Users\\Ganesh\\Google Drive\\Courses\\STAT S 670\\Mid Term\\R Files")
source("myplotfit.r")
source("rrline.r")
red<-c(5,6,3,11,10)
white<-c(14,10,6,12,21)
pink<-c(16,24,15,26,32)
mat<-rbind(red,white,pink)
colnames(mat)<-c("A1","A2","A3","A4","A5")
res<-medpolish(mat)
print(res)
# Pink has the maximum row effect, and Red has the minimum row effect
# Area A5 has the highest visitors and A3 has the lowest visitors
Analog_R_Square<- 1-((sum(abs(res$residuals))) /(sum(abs(mat-res$overall))))
print(Analog_R_Square)
diag.MP(res)
symbolPlot(mat)
forgetitplot(res)

