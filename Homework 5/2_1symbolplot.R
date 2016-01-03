C1<-c(22.2,44.5,59.6,73.2,86.8)
C2<-c(10.5,15.5,29.0,36.5,46.2)
C3<-c(3.53,5.76,9.71,14.0,21.1)
C4<-c(1.04,1.98,2.45,3.40,5.40)
C5<-c(.641,.974,1.80,2.60,3.64)
mat<-cbind(C1,C2,C3,C4,C4)
result<-medpolish(mat)
result
symbolPlot(mat)
plot(result)
diag.MP(result)
mat<-log(mat)
symbolPlot(mat)
result<-medpolish(mat)
result
diag.MP(result)

