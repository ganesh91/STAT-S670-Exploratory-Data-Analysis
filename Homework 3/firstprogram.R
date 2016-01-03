library(noncensus)
data(counties)
findSpread <- function(nList){
  sortedInput<-sort(nList)
  medianFlg<-(1+length(sortedInput))/2
  median <- ifelse(medianFlg==floor(medianFlg),sortedInput[medianFlg],(sortedInput[medianFlg-0.5]+sortedInput[medianFlg+0.5])/2)
  #print(median)
  flFlg<-(1+floor(medianFlg))/2
  fl<-ifelse(flFlg==floor(flFlg),sortedInput[flFlg],(sortedInput[flFlg-0.5]+sortedInput[flFlg+0.5]/2))
  #print(fl)
  fuFlg<-length(sortedInput)-flFlg+1
  fu<-ifelse(fuFlg==floor(fuFlg),sortedInput[fuFlg],(sortedInput[fuFlg-0.5]+sortedInput[fuFlg+0.5]/2))
  #print(fu)
  return(fu-fl)
}
states <- levels(counties$state)
medianDS <- as.numeric()
spreadDS <- as.numeric()

for (i in states){
  inState <- subset(counties,state==i)
  inPop <- sort(inState$population[!is.na(inState$population)])
  median <- median(inState$population)
  medianDS <- c(medianDS,median)
  spread <- findSpread(inPop)
  spreadDS <- c(spreadDS,spread)
}

statesSpread <- cbind.data.frame(states,medianDS,spreadDS)
statesSpread <- statesSpread[complete.cases(statesSpread),]
statesSpread <- subset(statesSpread,medianDS>0 & spreadDS >0)
statesSpread[statesSpread$states %!in% c("PR","GU","VI"),]

plot(statesSpread$medianDS,statesSpread$spreadDS,main="Spread vs Level Plot without log",xlab = "Median",ylab="Spread")
linear<-lm(statesSpread$spreadDS~statesSpread$medianDS)
abline(linear,lty=2,col="blue")
1-linear$coefficients[2]

plot(log10(statesSpread$medianDS),log10(statesSpread$spreadDS),main="Spread vs Level Plot with log",xlab = "Median",ylab="Spread")
linear<-lm(log10(statesSpread$spreadDS)~log10(statesSpread$medianDS))
abline(linear,lty=2,col="blue")
linear$coefficients[2]
1-linear$coefficients[2]

CA <- subset(counties,state=="CA")
source("lvalprogs.r")
lval(CA$population)
floors<-c(45578.0,20007.0,13994.0,6463.0,2207.5,1175.0)
ceils<-c(685306.0,1418788.0,2112425.5,3052772.5,6456959.0,9818605.0)

y<-ggplot(counties,aes(state,population,color=state))+geom_boxplot()+theme_bw()+guides(color=FALSE)
print(y)

z<-ggplot(counties,aes(state,log(population),color=state,guides=FALSE))+geom_boxplot()+theme_bw()+guides(color=FALSE)
print(z)

y<-ggplot(CA,aes(state,-(population)^5,guides=FALSE))+geom_boxplot()+theme_bw()+guides(color=FALSE)
print(y)
