library(cluster)
library(reshape2)
library(ggplot2)
plotAggregate <- function(statesList,df1,xlabc,ylabc,mainc,colc){
  dataFrame1<-df1[which(df1$StateName %in% statesList),2:3]
  dataFrame1<-aggregate(dataFrame1,by=list("yearTrend"=dataFrame1$variable),mean)
  plot(dataFrame1$yearTrend,dataFrame1$value,type="b",xlab=xlabc,ylab = ylabc,main=mainc,col=colc)
  abline(h=50,lty=2)
}
plotqq <- function(statesList,df1,xlabc,ylabc,mainc,colc){
  dataFrame1<-df1[which(df1$StateName %in% statesList),2:3]
  #dataFrame1<-aggregate(dataFrame1,by=list("yearTrend"=dataFrame1$variable),mean)
  #qqnorm(dataFrame1$value,xlab=xlabc,ylab = ylabc,main=mainc,col=colc)
  #qqline(dataFrame1$value)
  hist(dataFrame1$value,xlab=xlabc,ylab = ylabc,main=mainc,col=colc)
}
data("votes.repub")
df<-as.data.frame(votes.repub[,26:30])
colnames(df)<-c(1956,1960,1964,1968,1972)
df["StateName"]<-rownames(df)
df1<-melt(df,id.vars = "StateName")
df1$StateName<-as.factor(df1$StateName)
df1$variable<-as.integer(as.character(df1$variable))
df1$value[is.na(df1$value)]<-0
NorthEast=c("Connecticut","Delaware","Maine","Massachusetts","New Hampshire","New Jersey","New York","Pennsylvania","Rhode Island","Vermont")
MaEc<-c("Kentucky","Maryland","North Carolina","South Carolina","Tennessee","Virginia","West Virginia")
South<-c("Alabama","Arkansas","Florida","Georgia","Louisiana","Mississippi","Oklahoma","Texas")
midwest<-c("Illinois","Indiana","Iowa","Kansas","Michigan","Minnesota","Missouri","Nebraska","Ohio","Wisconsin")
rockies<-c("Colorado","Idaho","Montana","North Dakota","South Dakota","Utah","Wyoming")
west<-c("Alaska","Arizona","California","Hawaii","Nevada","New Mexico","Oregon","Washington")
par(mfrow=c(2,3))
plotAggregate(NorthEast,df1,"Years","Votes Percentage","North East Region",2)
plotAggregate(MaEc,df1,"Years","Votes Percentage","Mid Atlantic/East Central Region",3)
plotAggregate(South,df1,"Years","Votes Percentage","Southern Region",4)
plotAggregate(midwest,df1,"Years","Votes Percentage","Mid West Region",2)
plotAggregate(rockies,df1,"Years","Votes Percentage","Rockies Region",3)
plotAggregate(west,df1,"Years","Votes Percentage","West Region",4)
par(mfrow=c(2,3))
plotqq(NorthEast,df1,"Years","Votes Percentage","North East Region",2)
plotqq(MaEc,df1,"Years","Votes Percentage","Mid Atlantic/East Central Region",3)
plotqq(South,df1,"Years","Votes Percentage","Southern Region",4)
plotqq(midwest,df1,"Years","Votes Percentage","Mid West Region",2)
plotqq(rockies,df1,"Years","Votes Percentage","Rockies Region",3)
plotqq(west,df1,"Years","Votes Percentage","West Region",4)
qqnorm(df1$value,main="QQ plot for the entire dataset")
qqline(df1$value)
qplot(variable,value,data=df1,facets = Region~.,color=StateName,geom="line")+theme(legend.position="bottom")+guides(colour=guide_legend(nrow=5))+ggtitle("Votes by Country by Region")
