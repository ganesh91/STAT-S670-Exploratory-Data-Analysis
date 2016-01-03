#Program 2
#Author: Ganesh Nagarajan
#Plots the stem and leaf plot for the given function.
stemleafplot<- function(listArray,scale){
  sortedArray <- sort(listArray)
  minArray <- min(listArray)
  maxArray <- max(listArray)
  count <- nrow(length)
  stem(listArray,scale)
}

#input vector from probem 2(b)
dataSet <- c(0.12,0.15,0.15,0.10,0.13,0.15,0.14,
             0.08,0.11,0.09,0.14,0.09,0.13,0.14,
             0.12,0.16,0.15,0.13,0.12,0.12,0.09)
#call the function with scale 0.25
stemleafplot(dataSet,0.25)
#call the function with scale 0.5
stemleafplot(dataSet,0.5)
#call the function with scale 1
stemleafplot(dataSet,1)