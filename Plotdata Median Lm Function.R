#Calculating data for plots (binning destruction and calculating median lambda.M for percent loss
#, using median because it reflects that the data distribution is heavily skewed,
#, confidence intervals claculated using bootstrapping)
##############################################################################################################
plotdata.median.Lm<-function(input.data, scaled.lm){
  input.data<-input.data
  input.data$binned.percent.loss <- cut(input.data$percent.habitatloss, c(-Inf,seq(0, 1, 0.05)), labels=seq(0,1,0.05)) 
  #binning data at intervals of 5% habitat loss
  percent.loss<-levels(droplevels(input.data$binned.percent.loss)) #drop levels (don't want this as a factor)
  bins<-percent.loss #create bins for each 5% increment of habitat loss
  #set up data to be filled
  destruction.median<-rep(NA, length(bins))
  destruction.upper.CI<-rep(NA, length(bins))
  destruction.lower.CI<-rep(NA, length(bins))
  degradation.lambda.M<-rep(NA, length(bins))
  for (i in 1:length(bins)) { #for each bin of 5% habitat loss
    x<-subset(input.data, input.data$binned.percent.loss==bins[i]) #subset the data for that bin
    destruction.median[i] <- median(x$lambda.M.r)/scaled.lm #calculate the median persistence capacity within that bin
    bootobject <- boot(x$lambda.M.r, function(u,j) median(u[j]), R=1000) #bootstrap the median persistence capacity
    CI<-boot.ci(bootobject, conf=0.95, type="basic") #get bootstrapped 95% Confidence Intervals
    if(is.null(CI)==TRUE){destruction.upper.CI[i]<-destruction.median[i]
    destruction.lower.CI[i]<-destruction.median[i]} #if no CI was provided because all the data 
                          #is the same just set the CI's ast the median
    else{ #otherwise...
      destruction.upper.CI[i]<-CI$basic[5]/scaled.lm
      destruction.lower.CI[i]<-CI$basic[4]/scaled.lm} #divide the 95% CI's by the pristine persistence capacity
    degradation.lambda.M[i]<-((1-((i-1)*0.05))^2) #degradation's data just follows the curve we predicted so just 
                       #plot this clean line (otherwise this line becomes jerky because of the data being binned)
    #bind the 95% CI's to not exceed 1 or fall beneath 0 (we know these are impossible values)
    if(destruction.upper.CI[i]>1){destruction.upper.CI[i]<-1} 
    if(destruction.lower.CI[i]>1){destruction.lower.CI[i]<-1}
    if(destruction.upper.CI[i]<0){destruction.upper.CI[i]<-0}
    if(destruction.lower.CI[i]<0){destruction.lower.CI[i]<-0}
  }
  plot.data<-data.frame(percent.loss=seq(0,1,0.05), destruction.median, destruction.upper.CI, destruction.lower.CI, 
                        degradation.lambda.M) #put the necessary data for the plot together as output
  return(plot.data)
}
##############################################################################################################a=c(0.067, 0.105, 0.429, 0.571, 0.96, 100)
#CHECK
