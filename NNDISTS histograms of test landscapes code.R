
#nearest neighbours function comes from spatstat package which is unavailable for the R studio installed on the server
library("spatstat")
library("moments")

directory="C:/Users/abuga/Desktop/Final Metapop R Package/changing to mean min/50 patches"
landscapes.data<-read.csv("50_p_10landscapes_clevel500.csv")

#SUBSETTING DATA BY LANDSCAPE TYPE
##############################################################################################################
clustered.data<-subset(landscapes.data, landscape.type=="clustered")
random.data<-subset(landscapes.data, landscape.type=="random")
regular.data<-subset(landscapes.data, landscape.type=="regular")
##############################################################################################################

plot.nndist<-function(data, title){
  avg.data<-data.frame(seq(1:10), rep(NA, 10))
  colnames(avg.data)<-c("landscape", "avg.nndist")
  min.data<-data.frame(seq(1:10), rep(NA, 10))
  colnames(min.data)<-c("landscape", "min.nndist")
  for (i in 1:nrow(avg.data)){
    avg.data[i,2]<-mean(nndist(data[(50*i-49):(50*i),6:55]))
    min.data[i,2]<-min(nndist(data[(50*i-49):(50*i),6:55]))
    
  }
  hist(avg.data$avg.nndist, xlab="Nearest Neighbour Interpatch Distances", ylab="Avg. Frequency within Landscapes", 
       main = title, xlim=range(1:10))
  #hist(min.data$min.nndist, xlab="Nearest Neighbour Interpatch Distances", ylab="Avg. Frequency within Landscapes", main = title, xlim=range(1:10))
  avg.min.nndist<-mean(min.data$min.nndist)
  skewness<-skewness(avg.data$avg.nndist)
  kurtosis<-kurtosis(avg.data$avg.nndist)
  return(c(skewness, kurtosis, avg.min.nndist))
}
plot.nndist(regular.data, "More Uniform Landscapes")
plot.nndist(random.data, "Random Landscapes")
plot.nndist(clustered.data, "More Clustered Landscapes")
?hist()
