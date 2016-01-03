c1<-c(25.3,32.1,38.8,25.4)
c2<-c(25.3,29.1,31.0,21.1)
c3<-c(18.2,18.8,19.3,20.3)
c4<-c(18.3,24.3,15.7,24.0)
c5<-c(16.3,19.0,16.8,17.5)
mat<-cbind(c1,c2,c3,c4,c5)

twoway.median2 <- function(mat){ # first column then row
  meff.MP <- median(mat)
  beff.MP <- apply(mat,2,median,na.rm=T)  # column medians
  mat.res <- mat - matrix(rep(beff.MP,each=nrow(mat)),byrow=F,nrow=nrow(mat));
  aeff.MP <- apply(mat.res,1,median,na.rm=T) # row effect
  beff.MP <- beff.MP - median(beff.MP)  # column effect
  res.MP <- mat.res - matrix(rep(aeff.MP,each=ncol(mat)),byrow=T,ncol=ncol(mat))
  list(overall=meff.MP, row=aeff.MP, col=beff.MP, res=res.MP)
}

lv1<-twoway.median2(mat)
lv1
lv2<-twoway.median2(lv1$res)
lv2
stem(c(lv2$res),2)