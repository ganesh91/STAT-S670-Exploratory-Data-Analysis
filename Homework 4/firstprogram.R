library(ggplot2)
source("barlettline.R")
source("rrline.r")
#Resistant Regression Trials
age <- c(109, 113, 115, 116, 119, 120, 121, 124, 126, 129, 130, 133, 134, 135, 137, 139, 141, 142)
height <- c(13.76, 147.8, 136.8, 140.7, 132.7, 145.4, 135.0, 133.0, 148.5, 148.3, 147.5, 148.8, 133.2, 148.7, 152.0, 150.6, 165.3, 149.9)
df<-as.data.frame(cbind(age,height))
lmfit<-lm(height~age)
barlett_var<-barlettline(age,height)
rr_var<-run.rrline(age,height)
ggplot()+geom_point(data=df,aes(x=age,y=height))+geom_abline(intercept=lmfit$coefficients[1],slope=lmfit$coefficients[2],color="blue")+geom_text(x=130,y=180,aes(label=paste("1","2",lmfit$coefficients[1])),parse = FALSE)+geom_abline(intercept=barlett_var[1],slope=barlett_var[2],color="red",style="2")+geom_abline(intercept=rr_var$a,slope=rr_var$b,color="orange")+labs(title="Resistant Regression Lines",x="Age",y="Height",color="Legend")+geom_text(x=130,y=130,label=c("test"))
lmeq=paste("Least Squares y =",lmfit$coefficients[2]," x",lmfit$coefficients[1])
rrfit=paste("Resistant Regression y =",rr_var[2]," x+",rr_var[1])
barlett=paste("Barlett Line y =",barlett_var[2]," x",barlett_var[1])
rrfit
lmeq
barlett