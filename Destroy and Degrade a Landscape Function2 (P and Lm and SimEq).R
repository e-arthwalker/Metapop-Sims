#LOOPING BOTH DEGRADATION VS. DESTRUCTION FOR A GIVEN LANDSCAPE 
#takes about 10 min (for landscapes with 50 patches)
#################################################################################################################
#takes a given landscape and seperately performs two scenarios in which all patches are
#either destroyed or degraded until no habitat remains, outputing the metapopulation persistence capacity, 
#average probability of patch occupancy at equilibrium across all patches, average number of patches occupied for last 50 of 1000
#timesteps of the metapopulation sim, and timestep at which the metapopulation went extinct

#landscape = a dataframe created by the create landscape function which specifies patch locations, areas, 
#and and an interpatch distance matrix
#a = 1/(average dispersal distance) for a species
#delta = the ratio of the extinction to colonization rate of a species

############################################################################################
destroy.vs.degrade<-function(landscape, a, delta){
  
  a=a
  delta=delta
  col.rate<-1
  ext.rate<-delta
  initial.landscape<-landscape #save the initial landscape
  r.landscape<-landscape #input the initial landscape for the destruction process
  e.landscape<-landscape #input the initial landscape for the erosion process
  n.patches<-length(landscape$patch.ID)  
  
  total.area<-sum(initial.landscape$A) 
  #the total initial area is the sum of the areas in the initial landscape
  prev.loss<-0 #initially the no destruction has occured ****
  
  lambda.M.r<-rep(NA, n.patches)
  eq.size.r<-rep(NA, n.patches)
  time.to.eq.r<-rep(NA, n.patches)
  sim.eq.size.r<-rep(NA, n.patches)
  lambda.M.e<-rep(NA, n.patches)
  eq.size.e<-rep(NA, n.patches)
  sim.eq.size.e<-rep(NA, n.patches)
  time.to.eq.e<-rep(NA, n.patches)
  percent.habitatloss<-rep(NA, n.patches)
  eq.p.r<-rep(NA, n.patches)
  eq.p.e<-rep(NA, n.patches)
  avg.p.r<-rep(NA, n.patches)
  avg.p.e<-rep(NA, n.patches)
  order.lost<-rep(NA, n.patches)
  for(j in 1:(n.patches-1)) { #until all but one patch has been removed
    w<-subset(r.landscape, rowSums(is.na(r.landscape)) != (3+n.patches))
    q<-c(colSums(is.na(w)) != (n.patches-j+1))
    w<-subset(w, select=q)
    #w provides the landscape data for only the remaining patches in
    #the landscape undergoing destruction for each iteration
    #CALCULATE lambda.M
    lambda.M.r[j]<-lambda.M.function(landscape=w,
                                     a=a, delta=delta) #for removal landscape
    lambda.M.e[j]<-lambda.M.function(landscape=e.landscape, 
                                     a=a, delta=delta) #for eroding landscape
    #CALCULATE P.STAR.r
    p.star.r<-pstar.function(landscape=w, a=a, delta=delta, iterations=1000)
    eq.size.r[j]<-sum(p.star.r*w$A)
    eq.p.r[j]<-sum(p.star.r)
    avg.p.r[j]<-sum(p.star.r)/(n.patches-j+1)
    #SIMULATE SRLM AND PROVIDE TO GET TIME EXTINCT AND SIZE AT EQUILIBRIUM 
    SRLM.output.r<-SRLM.sim(landscape=w, a=a, delta=delta, timesteps=1000, p.initial=avg.p.r[1], p.star=p.star.r)#set the initial P* for the simulation to be the P* of the previous amount of habitat
    time.to.eq.r[j]<-SRLM.output.r$time.to.eq
    sim.eq.size.r[j]<-SRLM.output.r$eq.size/(n.patches-j+1)
    #cALCULATE P.star.e
    p.star.e<-pstar.function(landscape = e.landscape, a=a, delta=delta, iterations=1000)
    eq.size.e[j]<-sum(p.star.e*e.landscape$A)
    eq.p.e[j]<-sum(p.star.e)
    avg.p.e[j]<-sum(p.star.e)/n.patches
    #sim of eroding landscape
    SRLM.output.e<-SRLM.sim(landscape=e.landscape, a=a, delta=delta, timesteps=1000, p.initial=avg.p.r[1], p.star=p.star.e) #set the initial P* for the simulation to be the P* of the previous amount of habitat
    time.to.eq.e[j]<-SRLM.output.e$time.to.eq
    sim.eq.size.e[j]<-SRLM.output.e$eq.size/n.patches
    #REMOVE A RANDOM PATCH FROM THE LANDSCAPE UNDERGOING DESTRUCTION
    x<-w$patch.ID
    r<-sample(x, 1, replace=T) 
    #pick a random number between 1 and n.patches to determin the patch to remove
    #RECORD AMOUNT OF HABITAT LOST WITH PATCH REMOVAL
    #need to update this to account for unequal areas. 
    #get percent habitat loss from previous iteration and - the area of the patch that was lost on this iteration
    percent.habitatloss[j]<-prev.loss+r.landscape$A[r]/total.area
    prev.loss<-percent.habitatloss[j]
    r.landscape$A[r]<-NA
    r.landscape$x.coord[r]<-NA
    r.landscape$y.coord[r]<-NA
    d.r<-as.matrix(r.landscape[,5:(dim(r.landscape)+4)])
    d.r[r,]<-NA
    d.r[,r]<-NA
    r.landscape<-data.frame(patch.ID=r.landscape$patch.ID,
                            A=r.landscape$A,
                            x.coord=r.landscape$x.coord,
                            y.coord=r.landscape$y.coord,
                            d=d.r) #update the landscape data with patch r removed
    #RECORD WHICH PATCH WAS LOST WHEN
    order.lost[r]<-j 
    #percent.habitatloss[j]<-(1-((length(r.landscape$patch.ID)-j)/length(r.landscape$patch.ID)))
    #ERODE PATCH AREAS FROM THE LANDSCAPE UNDERGOING DEGRADATION
    e.landscape$A<-initial.landscape$A*(1-percent.habitatloss[j]) 
    #erodes patches by an equivalent % to patch loss
  }
  output<-data.frame(initial.landscape$patch.ID, 
                     initial.landscape$A,
                     initial.landscape$x.coord,
                     initial.landscape$y.coord,
                     order.lost,
                     lambda.M.r,
                     eq.size.r,
                     eq.p.r,
                     avg.p.r,
                     time.to.eq.r,
                     sim.eq.size.r,
                     percent.habitatloss,
                     lambda.M.e,
                     eq.size.e,
                     eq.p.e,
                     avg.p.e,
                     time.to.eq.e, 
                     sim.eq.size.e,
                     delta
  )
  return(output)}
###########################################################################################################
