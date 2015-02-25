###############################
# Footy Tipping Simulation
# Ensembling of Tipsters
#
# Parallel Version
#
# Phil Brierley
# Oct 2013
#
###############################

#clean all objects from memory
rm(list=ls())

#set memory
memsize <- 3200
if (memory.limit() < memsize)  memory.limit(size=memsize)


#----------------------
#parameters to set
#-----------------------
Number_of_Tipsters <- 12
Number_of_Games <- 20000
Number_ofSeasons <- 100
Tipster_Strength <- 0.6
threads <- 2 #depends on how many processors you have


#----------------------------------------------
# main function to simulate a season
#----------------------------------------------
simulateSeason <- function(f){
        
        #gernerate random tips
        Results = matrix(rbinom(Number_of_Games*Number_of_Tipsters,1,Tipster_Strength),Number_of_Games,Number_of_Tipsters)
        
        #my tip is a majority vote - hence median score
        Results[,Number_of_Tipsters] <- apply(Results[,1:(Number_of_Tipsters-1)],1,median)
        
        #find the mean score per tipster over the season
        seasonSummary <- apply(Results,2,mean)
        
}
#end of function
#---------------------------------------------





#--------------------------------
# the parallel stuff
#--------------------------------

#load library
library(snowfall)
?snowfall
#initiate clusters
sfStop()
sfInit(parallel = TRUE, cpus = threads, type = "SOCK")
sfExport(list = c("Number_of_Games","Number_of_Tipsters","Tipster_Strength"))

#start the clock
timeStart <- Sys.time() 

#do the calculation in parallel
seasonSummary <- sfClusterApplyLB(1:Number_ofSeasons, simulateSeason)

#stack results together into a data frame
runningSummary <- do.call(rbind.data.frame, seasonSummary)
colnames(runningSummary) <- paste('punter',1:ncol(runningSummary))
colnames(runningSummary)[ncol(runningSummary)] <- paste('ensemble')

#record the time it took
totTime <-  as.numeric(Sys.time() - timeStart, units = "secs")
myText <- paste('Avg calculation time per season = ', formatC(totTime/ Number_ofSeasons,digits=2,format='f') ,'seconds')

#stop clusters
sfStop()




#--------------------------------------
#plot the results
#--------------------------------------
bestPunter <-  apply(runningSummary[,1:(Number_of_Tipsters-1)],1,max)
plot(runningSummary[,Number_of_Tipsters]
     ,type='l'
     ,col='red'
     ,ylim=c(Tipster_Strength - 0.1,1)
     ,xlab='Season'
     ,ylab='% of Games Correct'
     ,main=myText)

lines(bestPunter,col='blue')
abline(h=Tipster_Strength,col='green')
bp <- mean(bestPunter)
abline(h=bp,col='blue')
mv <- mean(runningSummary[,Number_of_Tipsters])
abline(h=mv,col='red')

legend("topright"
       ,inset=.05
       ,c(paste("Majority Vote (avg=",mv,")")
          ,paste("Best Tipster (avg=",bp,")")
          ,paste('expected (',Tipster_Strength,')'))
       ,fill=c('red','blue','green')
       ,horiz=FALSE)

