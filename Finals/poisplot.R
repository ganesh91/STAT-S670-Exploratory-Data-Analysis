source("rrline.R")
poisplot <- function(k,nk,which) {
  lenk <- length(k)
  if(missing(which)) which <- (1:lenk)
  k0 <- k[which]
  nk0 <- nk[which]
  k1 <- k0[nk[which] > 0]
  nk1 <- nk0[nk0 > 0]
  N <- sum(nk1)
  # Modification to n_k, used in CI
  nk2 <- nk1
  nk2[nk1==1] <- exp(-1)
  nk2[nk1 > 1] <- (nk1[nk1 > 1])*(1 - 0.8/N) - 0.67
  phik <- log((gamma(k1 + 1))*nk2/N)
  rr <- run.rrline(k1,phik)
  pkhat <- nk1/N
  cilim <- 1.96*sqrt((1-pkhat)/(nk1-(.47+.25*pkhat)*sqrt(nk1)))
  rng <- range(c(phik-cilim,phik+cilim))
  par(mfrow=c(1,2))
  # Poissonness plot with confidence intervals
  plot(k1,phik,ylim=rng,xlim=range(k0)+c(-0.5,0.5),xlab="k",
       ylab="phik",type="n", main="Poisson plot", sub=
         paste( paste("Intercept=",format(round(rr$coef[6,1],3))),
                paste(", Slope=", format(round(rr$coef[6,2],3)))))
  text(k1,phik,format(nk1))
  segments(k1,phik-cilim,k1,phik+cilim,lty=2)
  abline(rr$coef[6,1],rr$coef[6,2],col=2)
  lamhat <- exp(rr$coef[6,2])
  tmp <- ifelse(nk0 > 0, sqrt(2+4*nk0), 1)
  exptd <- N*exp(-1*lamhat)*(lamhat^k0)/gamma(k0+1)
  dk <- tmp - sqrt(4*exptd + 1)
  # Residual plot
  plot(k0,dk,xlab="k",ylab="FT residual")
  abline(h=c(-2,0,2),lty=c(2,1,2),col=c(2,1,2))
  list(k=k1,nk=nk1,nkstar=nk2,phik=phik,cilim=cilim,
       int=rr$coef[6,1],slope=rr$coef[6,2],res=dk,expected=exptd)
}
  