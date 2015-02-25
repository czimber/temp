#####################################################
#
# A generic method to calculate the importance
# of variables in any model
#
# Phil Brierley
# Oct 2013
#
#####################################################

#clean all objects from memory
rm(list=ls())


#set memory
memsize <- 3200
if (memory.limit() < memsize)  memory.limit(size=memsize)


#libraries
library(nnet)
library(randomForest)
library(gbm)

#----------------------
#parameters to set
#-----------------------

#what model are we building
modTypes <- vector()
modTypes[1] = 'linear_regression'
modTypes[2] = 'neural_net'
modTypes[3] = 'gbm'
modTypes[4] = 'random_forest'

#a number >-=0 and < 1
deletion_threshold <- 0.05 

#for data set generation
Number_of_Useful_Variables <- 10
Number_of_Junk_Variables <- 10
Number_of_Records <- 1000
Number_of_Removed_Useful_Variables <- 0
Include_Junk_Variables <- TRUE

#importance testing loops
numloopsImportance <- 100  

#train test split
Train_Percent <- 0.5

#multithreading
threads <- 2



#-----------------------------------------
# error function
#-----------------------------------------
calc_error <- function(act,pred){
        
        aact <- as.matrix(act)
        ppred <- as.matrix(pred)
        
        return (sqrt(colSums(((ppred) - (aact)) ^ 2) / nrow(aact)))
        
}



#------------------------
#generate a data set
#------------------------

#set seed if you want to regenerate the same data set
set.seed(42)

useful <- matrix(runif(Number_of_Records*Number_of_Useful_Variables,0,1),Number_of_Records,Number_of_Useful_Variables)
junk <- matrix(runif(Number_of_Records*Number_of_Junk_Variables,0,1),Number_of_Records,Number_of_Junk_Variables)

colnames(useful) <- paste('useful',1:ncol(useful),sep="_")
colnames(junk) <- paste('junk',1:ncol(junk),sep="_")

#create the target
useful_weightings <- sort(runif(Number_of_Useful_Variables,0,1),decreasing=TRUE)
target <- useful %*% useful_weightings

#remove some useful variables 
useful <- useful[,1:(Number_of_Useful_Variables-Number_of_Removed_Useful_Variables)]

#create a data set
if (Include_Junk_Variables){
        myData <- data.frame(cbind(useful,junk,target))
} else {
        myData <- data.frame(cbind(useful,target))
}

#target - what we are predicting
theTarget <- 'target'

targindex <- ncol(myData)
colnames(myData)[targindex] <- theTarget 





#----------------------------------------------------
# divide data set into train and test
#----------------------------------------------------
trainrows <- runif(nrow(myData)) < Train_Percent
if(length(which(trainrows)) < 2) stop('not enough training cases')
testrows <- !trainrows




#-------------------------------------------------
# function for calculating variable importance
#--------------------------------------------------

varImporatnce <- function(variable){
        
        #initialse the errors
        errorTrain <- 0
        errorTest <- 0
        
        #copy this variable data
        temp <- myData[,variable]
        
        for(i in 1:numloopsImportance){  
                
                #scramble the values of this variable
                myData[,variable] <- temp[order(runif(length(temp)))]
                
                #calculate the predictions
                if (modType == 'neural_net'){
                        predictions <- predict(model,newdata=myData[,-targindex],type='raw')
                }
                
                if (modType == 'linear_regression'){
                        predictions <- predict(model, myData)
                } 
                
                if (modType == 'random_forest'){
                        predictions <- predict(model, myData,type="response")
                } 
                
                if (modType == 'gbm'){
                        predictions <- predict.gbm(model, myData[,-targindex],type="response",n.trees = model$n.trees)
                } 
                
                #calculate the error
                errorTest <- errorTest + calc_error(myData[testrows,theTarget],predictions[testrows])
                errorTrain <- errorTrain + calc_error(myData[trainrows,theTarget],predictions[trainrows])
        }
        
        #return average train and test error
        c(errorTrain/numloopsImportance,errorTest/numloopsImportance)
        
        
}



#----------------------------------------
# set up multithreading
#---------------------------------------
library(snowfall) #for parallel processing
library(rlecuyer)
sfInit(parallel = TRUE, cpus = threads, type = "SOCK")
sfClusterSetupRNG()



sfExport(list = c('myData','trainrows','testrows','numloopsImportance','calc_error','theTarget','targindex'))




####################################
# LOOP THROUGH ALL MODEL TYPES
####################################

variables <- setdiff(colnames(myData),theTarget)
candidates_for_deletion <- NULL

for (modType in modTypes){
        
        #-----------------------------
        #build a model
        #----------------------------
        
        if (modType == 'linear_regression'){
                model <- lm(as.formula(paste(theTarget, " ~ . ")) 
                            , data=myData[trainrows,])
                basePredictions <-  predict(model, myData)
        }
        
        if (modType == 'neural_net'){
                model <- nnet(x=myData[trainrows,-targindex]
                              ,y=myData[trainrows,targindex]
                              ,size=5
                              ,linout=TRUE)
                basePredictions <- predict(model,newdata=myData[,-targindex],type='raw')
                
        }
        
        if (modType == 'random_forest'){
                model <- randomForest(x= myData[trainrows,-targindex]
                                      ,y=myData[trainrows,targindex]
                                      ,ntree=1000)
                basePredictions <-  predict(model,myData,type="response")
        }
        
        if (modType == 'gbm'){
                model <- gbm(as.formula(paste(theTarget, " ~ . ")),         # formula
                             data=myData[trainrows,],                   # dataset
                             distribution="gaussian",     # see the help for other choices
                             n.trees=1000,                # number of trees
                             shrinkage=0.05,              # shrinkage or learning rate,
                             keep.data=FALSE,              # keep a copy of the dataset with the object
                             verbose=FALSE,               # don't print out progress
                             n.cores=1)                   # use only a single core (detecting #cores is # error-prone, so avoided here)
                
                basePredictions <- predict.gbm(object=model, newdata=myData[,-targindex],type="response",n.trees = model$n.trees)
                
        }
        
        
        #calculate the error
        full_Train_Error <- calc_error(myData[trainrows,theTarget],basePredictions[trainrows])
        full_Test_Error <- calc_error(myData[testrows,theTarget],basePredictions[testrows])
        
        
        #Export model to threads
        sfExport(list = c('modType','model'))
        if (modType == 'neural_net') sfLibrary(nnet)
        if (modType == 'random_forest') sfLibrary(randomForest)         
        if (modType == 'gbm') sfLibrary(gbm)    
        
        
        #-------------------------------------
        # calculate variable importance
        #-------------------------------------  
        s <- sfClusterApplyLB(variables,varImporatnce)
        s <- do.call(rbind.data.frame,s)
        colnames(s) <- c('Train','Test')
        row.names(s) <- variables
        
        #get the full model error
        s$Train <- s$Train / full_Train_Error
        s$Test <- s$Test / full_Test_Error
        
        #scale to 0-1
        myRows <- which(s$Train > 1)
        s[myRows,c('Train')] <- s[myRows,c('Train')] / max(s$Train)
        s[-myRows,c('Train')] <-0
        
        myRows <- which(s$Test > 1)
        s[myRows,c('Test')] <- s[myRows,c('Test')] / max(s$Test)
        s[-myRows,c('Test')] <-0
        
        #pick candidates for deletion based on a threshold
        my_candidates_for_deletion <- rownames(s[which(s$Test < deletion_threshold),])  
        candidates_for_deletion <- c(my_candidates_for_deletion,candidates_for_deletion)
        
        #get the ranking of each variable
        s1 <- s
        s1 <- s1[order(s1$Test,decreasing = TRUE),]
        s1$Rank <- 1:nrow(s1)
        colnames(s1)[ncol(s1)] <- paste('Rank',modType,sep="_")
        s1 <-s1[order(row.names(s1)),]
        
        #combine the rankings  
        if (modType == modTypes[1]){
                rankings <- s1[ncol(s1)]
        } else {
                rankings <- cbind(rankings,s1[ncol(s1)])
        }  

        #---------------------------------------
        # plot the chart
        #---------------------------------------
        s <- s[order(s$Test),]
        x <- barplot(as.matrix(t(s))
                     ,horiz=TRUE
                     ,beside=TRUE
                     ,main = paste(modType,'variable importance\nTrain RMSE =', formatC(full_Train_Error,digits = 5,format='f'),'\nTest RMSE = ',formatC(full_Test_Error,digits = 5,format='f'))
                     ,col=c("aliceblue","forestgreen")
                     ,xlim=c(-0.2,1)
                     ,axes = FALSE
                     ,axisnames = FALSE)
        text(-0.1,colSums(x)/2,row.names(s),col='blue')
        legend('bottomright',inset=0.05,c('Train','Test'),fill=c("aliceblue","forestgreen"))
        abline(v=0)
        
        
} #end looping through model types