library(ggplot2)
setwd("C:/Users/Ganesh/Google Drive/Courses/STAT S 670/Homework 3")
source("plotTransformBatch.R")
CEOCompensation<-read.table("ceo.txt",header = TRUE)
summary(CEOCompensation)
nrow(CEOCompensation)
max(CEOCompensation)
plot(CEOCompensation$TotalCompensation,main="CEO Compensation - Scatter Plot",ylab = "Salary")
plotC<-cbind(sort(CEOCompensation$TotalCompensation),1:nrow(CEOCompensation))
colnames(plotC)<-c("salary","index")
plotC<-as.data.frame(plotC)
qplot(index,salary,data=plotC,geom="line",main = "Sorted Salaries vs Index")
hist(CEOCompensation$TotalCompensation,breaks = 100,freq = F,main = "Distribution of Salaries",xlab = "Compensation")
lines(density(CEOCompensation$TotalCompensation,kernel = "epanechnikov"),lty=1,col="red",lwd=2)
qqnorm(CEOCompensation$TotalCompensation,main="QQ Plot for Salary Distribution")
qqline(CEOCompensation$TotalCompensation)
plotTransformBatch((CEOCompensation$TotalCompensation))
#Box plot without any transfo
boxplot((CEOCompensation$TotalCompensation),main="Without Transformation")

#One Transformation 4th root that equals out
plotTransformBatch((CEOCompensation$TotalCompensation)^0.25)
boxplot((CEOCompensation$TotalCompensation)^0.25,main="Fourth root transormation")
#One Transformation log that equals out
plotTransformBatch((CEOCompensation$TotalCompensation)^0.5)
boxplot((CEOCompensation$TotalCompensation)^0.5,main="Square root transform Transformation")

FL<-c(2142,1788,1517,1248,963.5,727.5,579,345,114)
M<-c(3678,4115.5,4400.5,4799,4978.75,5241,5394.5,5510.25,5494)
FU<-c(4944,6643,7284,8350,8994,9754.5,10210,10675.5,1087)
Letters<-c("F","E","D","C","B","A","Z","Y","")
z<-cbind(FL,M,FU)
z<-(690*(z)^(1/3))-7000
z<-cbind(Letters,z)