trial1 <- rnorm(120)
trial2a <- trial1[1:60]
trial2b <- trial1[61:120]
trial3a <- trial1[1:40]
trial3b <- trial1[41:70]
trial3c <- trial1[71:90]
trial3d <- trial1[91:100]
trial3e <- trial1[101:105]
trial3f <- trial1[106:110]
trial3g <- trial1[111:115]
trial3h <- trial1[116:120]
par(mfrow=c(1,3))
boxplot(trial1,main="Trial 1")
boxplot(trial2a,trial2b,main="Trial 2")
boxplot(trial3a,trial3b,trial3c,trial3d,trial3e,trial3f,trial3g,trial3h,main="Trial 3")