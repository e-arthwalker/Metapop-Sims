#CHOOSING ALPHA VALUES 
#################################################################################################################
#choose alpha values for simulation based on each landscape type's average minimum interpatch distance
#thus, levels of alpha represent diff species but of equivalent dispersal ability within the given landscape
#controls for the fact that clustering will have smaller nearest neighbour distances by way of the clustering algorithm, while regular will have larger nndists
#isolates for the effect of regularity in patch spacing

#nndists.distribution = data.frame of summary stats of nndists for all three landscape types 
#(aka output from NNDISTS Distribution Summary function)
#landscape.limit = size of landscape (max x and y coordinate that was possible in landscape generation)

#############################################################################################################
choose.alphas<-function(nndists.distribution, landscape.limit){
  a.1<-nndists.distribution[3]/8 #average dispersal distance 1/8 mean(min) nndist
  a.2<-nndists.distribution[3]/4 #average dispersal distance 1/4 mean(min) nndist
  a.3<-nndists.distribution[3]/2 #average dispersal distance 1/2 mean(min) nndist
  a.4<-nndists.distribution[3] #average dispersal distance mean(min) nndist
  a.5<-nndists.distribution[3]*2 #average dispersal distance 2x mean(min) nndist
  a.6<-nndists.distribution[3]*4 #average dispersal distance 4x mean(min) nndist
  a.7<-nndists.distribution[3]*8 #average dispersal distance 8x mean(min) nndist
  a.8<-100 #average dispersal distance landscape limit
  alphas<-data.frame(a.1, a.2, a.3, a.4, a.5, a.6, a.7, a.8)
  alphas<-1/alphas
  return(alphas)
}
###########################################################################################################
#CHECK
#alphas<-choose.alphas(nndists.distribution=clustered.nndists.dist, landscape.limit=100)
#alphas
