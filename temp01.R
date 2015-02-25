#--------------------------------------
# Footy Tipping Simulation
# Ensembling of Tipsters
#
# Phil Brierley
# Oct 2013
#---------------------------------------


#clean all objects from memory
rm(list=ls())


#------------------------------
# adjustable parameters to set
#------------------------------
Number_of_Tipsters <- 12
Number_of_Games <- 2000
Number_ofSeasons <- 100
Tipster_Strength <- 0.6
#------------------------------


#simulation each season
for (Season in 1:Number_ofSeasons){
        
#gernerate random tips
Results = matrix(rbinom(Number_of_Games*Number_of_Tipsters,1,Tipster_Strength),
                 Number_of_Games,Number_of_Tipsters)
        
#majority vote = median score
Results[,Number_of_Tipsters] <- apply(Results[,1:(Number_of_Tipsters-1)],1,median)
        
#find the mean score per tipster over the season
seasonSummary <- apply(Results,2,mean)
        
#stack the seasons together
if (Season == 1) {
        runningSummary <- seasonSummary
        } else {
        runningSummary <- rbind(runningSummary,seasonSummary)
        }
        
} #Number_ofSeasons


#give the columns sensible names
colnames(Results)[1:Number_of_Tipsters] <- paste('Tipster',1:Number_of_Tipsters)
colnames(Results)[Number_of_Tipsters] <- paste('Majority Vote')
colnames(runningSummary) <- colnames(Results)


#plot the results
bestPunter <-  apply(runningSummary[,1:(Number_of_Tipsters-1)],1,max)

plot(runningSummary[,Number_of_Tipsters]
     ,type='l'
     ,col='red'
     ,ylim=c(Tipster_Strength - 0.1,1)
     ,xlab='Season'
     ,ylab='% of Games Correct')

lines(bestPunter,col='blue')
abline(h=Tipster_Strength,col='green')
bp <- mean(bestPunter)
abline(h=bp,col='blue')
mv <- mean(runningSummary[,Number_of_Tipsters])
abline(h=mv,col='red')

legend("topright"
       , inset=.05
       , c(paste("Majority Vote (avg=",mv,")"),paste("Best Tipster (avg=",bp,")")
           ,paste('expected (',Tipster_Strength,')'))
       ,fill=c('red','blue','green')
       ,horiz=FALSE)

runningSummary
head(Results)
