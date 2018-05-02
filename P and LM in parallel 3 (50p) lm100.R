rm(list=ls()) #clear workspace

#LOAD REQUIRED PACKAGES
############################################################################################################
setwd("C:/Users/Administrator/Desktop/Final Metapop R Package/Functions") #access working directory that all 
#my metapop functions are stored in
source("Create Landscape Function.r") #load create.landscape.function
source("Lambda M Function.r") #load the calculate lamda.M.function
source("Pstar Function.r") #load the persistence.function
source("time to eq.r") #load the SRLM.sim function
source("Destroy and Degrade a Landscape Function2 (P and LM).r") #destroy.vs.degrade.function only calculating 
#Pstar and Lambda.M 
source("Degrade and Destroy Multiple Landscapes Function.r") #replicates landscape creation and destroy vs. 
#degrade specified metapop parameters a specified number of times
source("Vary Alpha Destroy and Degrade Function for parallel.r") #provides replicates for a range of alphas 
#across a landscape type
#############################################################################################################

#SET WHERE YOU WANT THE OUTPUT TO GO ***modify this appropiately***
#############################################################################################################
#############################################################################################################
setwd("C:/Users/Administrator/Desktop/Final Metapop R Package/changing to mean min/50 patches")
#access working directory that all my metapop functions are stored in
#############################################################################################################
#############################################################################################################

#SET UP TO PROCESS IN PARALLEL (The simulations take a long time otherwise)
#############################################################################################################
library("doParallel") 
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
#############################################################################################################

#specify the number of simulation iterations wanted ***modify this appropiately***
##############################################################################################################
##############################################################################################################
its.of.5=100 #enter how many iterations total you want divided by 5
##############################################################################################################
##############################################################################################################

#PROCESS TO BE RUN ON EACH CORE: NOTE! for each alpha value 5 sepperate cores run a specified number of iterations to give 5*that number 
#of iterations for each alpha value for each alpha value 5 sepperate cores run 100 iterations to give 500 
#iterations for each alpha value
############################################################################################################
parallel.alphas<-function(j, n.reps, k){
  alpha.data<-read.csv("C:/Users/Administrator/Desktop/Final Metapop R Package/changing to mean min/50 patches/alphas.csv")
  #alpha.data<-alpha.data[,-1]
  alpha.data<-alpha.data[,-10]
  regular.data<-vary.alpha.in.parallel(alphas=alpha.data[1,j+1], landscape.type="regular", n.landscapes=n.reps,
                                       k=k, n.patches=50, landscape.limit=100, no.runs=500, scaled.lm=100)
  random.data<-vary.alpha.in.parallel(alphas=alpha.data[2,j+1], landscape.type="random", n.landscapes=n.reps,
                                      k=k, n.patches=50, landscape.limit=100, no.runs=500, scaled.lm=100)
  clustered.data<-vary.alpha.in.parallel(alphas=alpha.data[3,j+1], landscape.type="clustered", n.landscapes=n.reps,
                                         k=k, n.patches=50, landscape.limit=100, no.runs=500, scaled.lm=100)
  rbind(regular.data, random.data, clustered.data)
}
##############################################################################################################

#GET THE DATA
##############################################################################################################
output<- foreach (k=1:5, .combine=rbind)  %:%
 foreach(i=((k*8)-7):(k*8), .combine=rbind) %dopar% parallel.alphas(j=i-((k-1)*8), n.reps=its.of.5, k=k)
write.csv(output, "500reps_PnLM_3_50p_lm100.csv")
#############################################################################################################

# turn parallel processing off and run sequentially again:
#############################################################################################################
registerDoSEQ()
stopCluster(cl)
stopImplicitCluster(cl)
#############################################################################################################






