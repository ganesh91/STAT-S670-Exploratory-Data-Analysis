setwd("C:/Users/Ganesh/Google Drive/Courses/STAT S 670/Mid Term")
mu<-read.csv("smoothing.csv",header = FALSE)
x<-mu$V1
y<-mu$V2
mu = function(t){t + 0.5 *exp(-50*(t-0.5)^2)}
curve(mu(x),0,1,col="red")
points(x,y)
t=ksmooth(x,y,kernel="normal",bandwidth=0.5)
lines(t$x,t$y,col="violet")
text(t$x[1],t$y[1],"Bandwidth = 0.5",col="violet")
t=ksmooth(x,y,kernel="normal",bandwidth=0.3)
lines(t$x,t$y,col="green")
text(t$x[20],t$y[20],"Bandwidth = 0.3",col="green")
t=ksmooth(x,y,kernel="normal",bandwidth=0.25)
lines(t$x,t$y,col="blue")
text(t$x[30],t$y[30],"Bandwidth = 0.25",col="blue")
t=ksmooth(x,y,kernel="normal",bandwidth=0.15)
lines(t$x,t$y)
text(t$x[40],t$y[40],"Bandwidth = 0.15")