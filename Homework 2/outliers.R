calculateOutliers<-function (sampleSize){
  i = length(sampleSize);
  outlier=0;
  for ( i in seq(1:i)){
    outlier=outlier+0.4+(0.00698*sampleSize[i])
  }
  print(outlier)
}
sampleSize<-c(100)
sampleSize<-c(60,60)
sampleSize<-c(40,30,20,10,5,5,5)