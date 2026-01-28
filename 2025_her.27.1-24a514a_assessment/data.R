#------------------------------------------------------------------------------#
#data.R
#In this script, we prepare read and prepare the data for the SAM assessment. 
#
#The dataflow follows the structure of a standard TAF project
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
#remove everything from memory
#------------------------------------------------------------------------------#
rm(list=ls())



#------------------------------------------------------------------------------#
#load packages
#------------------------------------------------------------------------------#
library(TAF)
taf.library('stockassessment')



#------------------------------------------------------------------------------#
#load user-defined functions
#------------------------------------------------------------------------------#
source(taf.boot.path('rscript/utils.r'))



#------------------------------------------------------------------------------#
#make the data directory to store the data products
#------------------------------------------------------------------------------#
mkdir("data")  #we use this category to store human-readable files
mkdir("input") #we use this category to store the data products as .rds




#------------------------------------------------------------------------------#
#read data related to the catch
#------------------------------------------------------------------------------#
#Catch data
cn <- read.ices(taf.data.path('caa.txt')) 
cn_cv <- read.ices(taf.data.path('caa_cv.txt'))
cw <- read.ices(taf.data.path('cw.txt'))

#------------------------------------------------------------------------------#
#Add fraction data
#------------------------------------------------------------------------------#
lf<-read.ices(taf.data.path('lf.txt'))
pf<-read.ices(taf.data.path('pf.txt'))
pm<-read.ices(taf.data.path('pm.txt'))

#------------------------------------------------------------------------------#
#Add dummy data for allowing SAM to run 
#------------------------------------------------------------------------------#
dw<-read.ices(taf.data.path('dw.txt'))
lw<-read.ices(taf.data.path('lw.txt'))


#------------------------------------------------------------------------------#
#Add biological data
#------------------------------------------------------------------------------#
#Natural mortality
nm<-read.ices(taf.data.path('nm.txt'))

#Maturity
#Also the file includes data from 1950, but only data from 1988 is used
mo <- read.ices(taf.data.path('matprop.txt'))
mo <- mo[as.numeric(rownames(mo))>=1988,]
mo <- mo[,as.numeric(colnames(mo))%in%c(2:12)]

#Stock weight by age
sw<-read.ices(taf.data.path('west.txt'))
sw <- sw[as.numeric(rownames(sw))>=1988,]
sw<-sw[,as.numeric(colnames(sw))>=2]
sw[,as.numeric(colnames(sw))==12]<-rowMeans(sw[,as.numeric(colnames(sw))>=12])
sw <- sw[,as.numeric(colnames(sw))<=12]

#------------------------------------------------------------------------------#
#Observation data
#------------------------------------------------------------------------------#
surveys<-read.ices(taf.data.path('survey_T2025-02-05.txt'))
surveys.cv<-read.ices(taf.data.path('surveycv_T2025-02-05.txt'))

#This is a place holder in case we cannot use the recalculated BESS indeks
#In case this is initialised, the weighting of the time series sould be switched
#off
surveys$recBar[,2]<-c(36.495,16.167,5.535,2.595,1.626,0.433,0.315,1.504,1.078,
                      5.029,1.822,9.023,1.573,2.138,6.035,0.209,0.231,0.12,0.882,
                      32.92,43.995)
surveys$recBar[,3]<-c(0.901,6.973,1.62,6.378,3.987,1.807,0.234,0.006,1.285,0.092,
                      6.825,3.214,3.089,3.465,1.299,6,1.816,0.36,NA,4.443,22.109)

#Remove years from surveys
surveys$recBar[rownames(surveys$recBar)%in% c(2016,2018,2022),]<-NA
surveys.cv$recBar[rownames(surveys.cv$recBar)%in% c(2016,2018,2022),]<-NA


#Remove ages from the BESS survey
att_ <- attributes(surveys$recBar)
surveys$recBar<-surveys$recBar[,colnames(surveys$recBar)%in%c(2,3)]
surveys.cv$recBar<-surveys.cv$recBar[,colnames(surveys.cv$recBar)%in%c(2,3)]
attributes(surveys$recBar)$time <- att_$time
attributes(surveys$recBar)$twofirst <- att_$twofirst




#------------------------------------------------------------------------------#
#RFID data
#------------------------------------------------------------------------------#
#Use the rfid data as index. 
index_tag1 <- read.ices(taf.data.path('rfid_index_BioSampleID.txt'))$RFID
index_tag1.cv <- read.ices(taf.data.path('rfid_cv_BioSampleID.txt'))$RFID





#------------------------------------------------------------------------------#
#Split the spawning survey into two different surveys
#------------------------------------------------------------------------------#
surveys$NASF_old <- surveys$NASF[as.numeric(rownames(surveys$NASF))<=2008,]
surveys$NASF_new <- surveys$NASF[as.numeric(rownames(surveys$NASF))>=2015,]
surveys.cv$NASF_old <- surveys.cv$NASF[as.numeric(rownames(surveys.cv$NASF))<=2008,]
surveys.cv$NASF_new <- surveys.cv$NASF[as.numeric(rownames(surveys.cv$NASF))>=2015,]
attributes(surveys$NASF_old)$time <- c(0.16,0.16)
attributes(surveys$NASF_new)$time <- c(0.16,0.16)
surveys$NASF<-NULL



#------------------------------------------------------------------------------#
#Add the RFID data
#------------------------------------------------------------------------------#
surveys$RFID <- index_tag1[,-1]
attributes(surveys$RFID)$time <- c(0.9,0.9)




#------------------------------------------------------------------------------#
#Add wieghts to observation data
#------------------------------------------------------------------------------#
ttt<- computeExternalWeight(mu_data = cn,cv_data = cn_cv)
attributes(cn)$weight = ttt$weight
write.csv(round(1/sqrt(ttt$weight),digits = 3),file = 'data/tab.4.4.8.1.txt')

ttt<- computeExternalWeight(mu_data = surveys$NASF_old,cv_data = surveys.cv$NASF_old)
attributes(surveys$NASF_old)$weight = ttt$weight
write.csv(round(1/sqrt(ttt$weight),digits = 3),file = 'data/tab.4.4.8.2a.txt')

ttt<- computeExternalWeight(mu_data = surveys$NASF_new,cv_data = surveys.cv$NASF_new)
attributes(surveys$NASF_new)$weight = ttt$weight
write.csv(round(1/sqrt(ttt$weight),digits = 3),file = 'data/tab.4.4.8.2b.txt')

ttt<- computeExternalWeight(mu_data = surveys$IESNS,cv_data = surveys.cv$IESNS)
attributes(surveys$IESNS)$weight = ttt$weight
write.csv(round(1/sqrt(ttt$weight),digits = 3),file = 'data/tab.4.4.8.4.txt')

ttt<- computeExternalWeight(mu_data = surveys$Bar,cv_data = surveys.cv$Bar)
attributes(surveys$Bar)$weight = ttt$weight
write.csv(round(1/sqrt(ttt$weight),digits = 3),file = 'data/tab.4.4.8.3.txt')

# ttt<- computeExternalWeight(mu_data = surveys$recBar,cv_data = surveys.cv$recBar)
# attributes(surveys$recBar)$weight = ttt$weight
ttt<- computeExternalWeight(mu_data = surveys$RFID,cv_data = index_tag1.cv[,-1])
attributes(surveys$RFID)$weight = ttt$weight
write.csv(round(1/sqrt(ttt$weight),digits = 3),file = 'data/tab.4.4.8.5.txt')


#------------------------------------------------------------------------------#
#Load data into sam data object
#------------------------------------------------------------------------------#
dat<-setup.sam.data(surveys=surveys,
                    residual.fleet = cn,
                    prop.mature = mo,
                    stock.mean.weight = sw,
                    catch.mean.weight = cw,
                    dis.mean.weight = dw,
                    land.mean.weight = lw,
                    prop.f=pf,
                    prop.m = pm,
                    natural.mortality = nm,
                    land.frac = lf)
save(dat,file='input/dat.Rda')

