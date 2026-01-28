#------------------------------------------------------------------------------#
#model.R
#
#Here we define the paramters to be used. 
#Also we run the main model, and several dignostic of this
#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#
#remove everything from memory
#------------------------------------------------------------------------------#
rm(list=ls())




#------------------------------------------------------------------------------#
#load TAF
#------------------------------------------------------------------------------#
library(TAF)
taf.library('stockassessment')



#------------------------------------------------------------------------------#
#Make the directory tree
#------------------------------------------------------------------------------#
mkdir("model")



#------------------------------------------------------------------------------#
#Prepare the object
#------------------------------------------------------------------------------#
load('input/dat.Rda')
conf<-defcon(dat)
conf$keyLogFpar[2,]<-c(-1,0,1,2,3,4,5,6,7,8,8)
conf$keyLogFpar[3,]<-c(9,10,-1,-1,-1,-1,-1,-1,-1,-1,-1)
conf$keyLogFpar[4,]<-c(11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1)
conf$keyLogFpar[5,]<-c(-1,12,13,14,15,16,17,17,17,17,17)
conf$keyLogFpar[6,]<-c(-1,18,19,20,21,22,23,24,25,26,26)
conf$keyLogFpar[7,-1]<-27
conf$stockRecruitmentModelCode<-3
conf$fbarRange <- c(5,12)
conf$keyVarObs[1,] <- c(0,1,2,3,3,3,3,3,3,3,3)
conf$keyVarObs[2,-1] <- c(3,4,4,4,4,4,4,4,4,4)+1
conf$keyVarObs[3,1:2] <- c(6)
conf$keyVarObs[4,1] <- 7
conf$keyVarObs[5,-1] <- c(8,9,10,10,10,10,10,10,10,10)
conf$keyVarObs[6,-1] <- c(10,11,12,12,12,12,12,12,12,12)+1
conf$keyVarObs[7,-1] <- c(15,15,15,15,15,15,15,16,16,17)-1
conf$obsCorStruct[5]<-'AR'
conf$keyCorObs[5,-1]<-0
par<-defpar(dat,conf)
fit <- sam.fit(dat,conf,par)
save(fit,file='model/fit.Rda')


#------------------------------------------------------------------------------#
#Diagnostic
#------------------------------------------------------------------------------#
fit_pro <- procres(fit)
res_fit <- residuals(fit)
sim_fit <- simstudy(fit,nsim = 50)
jit_fit <- jit(fit)
LO_fit <- leaveout(fit)
YY <- max(fit$data$years)
R<-cbind((YY-1)-0:4, YY-0:4, (YY-1)-0:4, YY-0:4, YY-0:4, YY-0:4, (YY-1)-0:4)
ret <- retro(fit, year=R)
R2<-cbind((YY-1)-0:4, YY-0:4, (YY-1)-0:4, YY-0:4, YY-0:4, YY-0:4, (YY-2)-0:4)
ret2 <- retro(fit, year=R2)
save(fit_pro,res_fit,sim_fit,LO_fit,ret,ret2,file='model/diag.Rda')



