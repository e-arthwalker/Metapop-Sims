#running replicates of the degradation and destruction scenarios of landscapes for a given set of parameters

#n.patches = number of patches desired in landscapes
#landscape limit = size of landscapes (max x and y coord of patches)
#landscape.type = specifiy whether landscapes should be "clustered", "random" or "regular" in patch distribution
#no.runs = level of clustering (number of times the clustering algorithm is iterated for)
#a = 1/average dispersal distance of the species
#n.landscapes = number of landscapes put through the degradation and destruction scenarios
#scaled.lm = value you would like to scale lambda.M to initially
############################################################################################
gen.destroy.and.degrade.landscapes<-function(n.patches, landscape.limit, landscape.type, no.runs, a, n.landscapes, scaled.lm){
  for (i in 1:n.landscapes){ #for each landscape
    landscape<-create.landscape(n.patches=n.patches, landscape.limit=landscape.limit, landscape.type=landscape.type, no.runs=no.runs) 
    #create that landscape
    #scaling delta starting with initial lambda.M of 20
    delta<-(lambda.M=lambda.M.function(landscape=landscape, a=a, delta=1))/scaled.lm #scale delta to achieve the desired initial 
    #persistence capacity of the pristine landscape (this is done to make the impacts of degradation and destruction comparable 
    #across landscapes
    output<-destroy.vs.degrade(landscape=landscape, a=a, delta=delta) #sequentially destroy patches and degrade copies of the landscape
    #until no habitat remains, while calculating metrics measuring their impact until no habitat remains
    output<-data.frame(landscape.type, output) #include the respective landscape type with the output
    #shift % loss column, (fix this in the destroy vs degrade function then remove this here)
    output["percent.habitatloss"]<-c(0, head(output["percent.habitatloss"], dim(output)[1]-1)[[1]]) #Adjust the percent habitat loss
    #column so that it starts from zero
    output$landscape.type<-landscape.type #the output column landscape.type should contain the landscape type
    output$alpha<-a #the output column alpha should contain alpha's value
    output$rep.no<-i #provide a rep no to identify to which landscape the data belongs
    #combine the data
    if(i==1){data<-output}
    else{data<-rbind(data, output)}
  }
  return(data)
}
############################################################################################
