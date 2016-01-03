library(cluster)
library(reshape2)
data("votes.repub")
df<-as.data.frame(votes.repub[,26:30])
colnames(df)<-c(1956,1960,1964,1968,1972)
df["StateName"]<-rownames(df)
df1<-melt(df,id.vars = "StateName")
df1$StateName<-as.factor(df1$StateName)
df1$variable<-as.integer(as.character(df1$variable))
df1["Region"]<-as.character("NA")
NorthEast=c("Connecticut","Delaware","Maine","Massachusetts","New Hampshire","New Jersey","New York","Pennsylvania","Rhode Island","Vermont")
MaEc<-c("Kentucky","Maryland","North Carolina","South Carolina","Tennessee","Virginia","West Virginia")
South<-c("Alabama","Arkansas","Florida","Georgia","Louisiana","Mississippi","Oklahoma","Texas")
midwest<-c("Illinois","Indiana","Iowa","Kansas","Michigan","Minnesota","Missouri","Nebraska","Ohio","Wisconsin")
rockies<-c("Colorado","Idaho","Montana","North Dakota","South Dakota","Utah","Wyoming")
west<-c("Alaska","Arizona","California","Hawaii","Nevada","New Mexico","Oregon","Washington")
df1$Region[which(df1$StateName %in% NorthEast)]<-"North East"
df1$Region[which(df1$StateName %in% South)]<-"South"
df1$Region[which(df1$StateName %in% west)]<-"West"
df1$Region[which(df1$StateName %in% rockies)]<-"Rockies"
df1$Region[which(df1$StateName %in% midwest)]<-"Mid West"
df1$Region[which(df1$StateName %in% MaEc)]<-"Mid Atlantic / East Central"
boxplot(df1$value,main="Box plot of the Republican votes from 1956 to 1972")
boxplot(df1$value~df1$Region, main="Box plot of Republican votes by Region")
qplot(variable,value,data=df1[which(df1$Region == "North East"),],color=StateName,geom="line")

a<-ggplot(subset(df1,Region == "North East"),aes(x=variable,y=value,color=StateName))+geom_line()+ggtitle("North East")
b<-ggplot(subset(df1,Region == "South"),aes(x=variable,y=value,color=StateName))+geom_line()+ggtitle("South")
c<-ggplot(subset(df1,Region == "Rockies"),aes(x=variable,y=value,color=StateName))+geom_line()+ggtitle("Rockies")
d<-ggplot(subset(df1,Region == "West"),aes(x=variable,y=value,color=StateName))+geom_line()+ggtitle("West")
e<-ggplot(subset(df1,Region == "Mid West"),aes(x=variable,y=value,color=StateName))+geom_line()+ggtitle("Mid West")
f<-ggplot(subset(df1,Region == "Mid Atlantic / East Central"),aes(x=variable,y=value,color=StateName))+geom_line()+ggtitle("Mid Atlantic / East Central")
multiplot(a,b,c,d,e,f, cols=2)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

