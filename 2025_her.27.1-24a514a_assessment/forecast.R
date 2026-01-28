#------------------------------------------------------------------------------#
#forecast.R
#
#Here we run the short term forecast for the NSS herring. 
#------------------------------------------------------------------------------#






#------------------------------------------------------------------------------#
# Remove everything from memory to avoid conflicts
#------------------------------------------------------------------------------#
rm(list=ls())





#------------------------------------------------------------------------------#
#Load library, and source files
#------------------------------------------------------------------------------#
library(icesTAF)
taf.library(ggplot2)
taf.library('stockassessment')
taf.library(quarto)
taf.library(ggplotify)   
taf.library(patchwork)  
taf.library(ggrepel)
taf.library(bbmle) #Package used for DD effect on maturity


source(taf.boot.path('rscript/forecast_utils.R'))
source(taf.boot.path('rscript/utils.r'))
source(taf.boot.path('rscript/SAM_forecast_from_GIT.R'))
source(taf.boot.path('rscript/DD_model.r'))




#------------------------------------------------------------------------------#
#Load the fit object, and make a replica to avoid messing up with the official
#------------------------------------------------------------------------------#
load('boot/rscript/fit_official_benchmark.Rda')
fit_old <- fit
load('model/fit.Rda')



#------------------------------------------------------------------------------#
#load last runned forecast
#------------------------------------------------------------------------------#
load('boot/rscript/forecast_2024.Rda')
forecast_last<-tt_deterministic



#------------------------------------------------------------------------------#
#Make the folder structure
#This is redundant if the report.R has been runned
#------------------------------------------------------------------------------#
icesTAF::mkdir('report')
icesTAF::mkdir('report/tables')
icesTAF::mkdir('report/figures')




#------------------------------------------------------------------------------#
#User input data
#------------------------------------------------------------------------------#
TAC <- 401794           #Last year advice, this is in unit tonnes
nation_quota <- 435.010 #Declared quota intermediate year, note that it must be in units ktonnes




#------------------------------------------------------------------------------#
#Reference values
#------------------------------------------------------------------------------#
blim <- 2286
flim =0.291
fmsy <- 0.21
msy_btrigger <- 3177
bpa <- msy_btrigger#3184



#------------------------------------------------------------------------------#
#Management plan
#------------------------------------------------------------------------------#
fmp <- 0.14
TAC_cap <- c(0.20,0.25)

scenario <- function(blim,bpa,ftarget){
  MP<- c()
  MP$ssb<-c(seq(0,blim),seq(blim,bpa),seq(bpa,12000))
  MP$F <-c(rep(0.05,length(seq(0,blim))),
           (seq(blim,bpa)-blim)*(ftarget-0.05)/(bpa-blim)+0.05,
           rep(ftarget,length(seq(bpa,12000))))
  MP <- data.frame(MP)
  return(MP)
}
MP<-scenario(blim,bpa,ftarget = fmp)

#------------------------------------------------------------------------------#
#Test to see the difference in MP (new and old values) and FMSY
#------------------------------------------------------------------------------#
MP2<-scenario(2500,3184,ftarget = fmp)
MP$FMSY <- fmsy
MP$FMSY_old <- 0.157
MP[MP$ssb<msy_btrigger,]$FMSY<-MP[MP$ssb<msy_btrigger,]$FMSY*MP[MP$ssb<msy_btrigger,]$ssb/msy_btrigger
MP[MP$ssb<msy_btrigger,]$FMSY_old<-MP[MP$ssb<msy_btrigger,]$FMSY_old*MP[MP$ssb<msy_btrigger,]$ssb/3184

ggplot(MP,aes(x=ssb,y=F))+
  geom_line(aes(colour='MP'))+
  xlim(0,5000)+
  geom_line(aes(y=FMSY,colour='FMSY'))+
  geom_line(aes(y=FMSY_old,colour='FMSY (old)'))+
  geom_line(data=MP2,aes(colour='MP (old)'))




#------------------------------------------------------------------------------#
#some general settings that may be redundant
#------------------------------------------------------------------------------#
rec.years = fit$data$years    #Specify the range to pull recruitment from, this is must go in the forecast but it is not used since we draw from the parameter
rec_from_par = T              #Compute recruitment from parameter 
switch_year_range = T         #Flag to initialize a small hack in the forecast function, so it fits the structure of the data
ave.years <- tail(fit$data$years,3) # which years to compute the means
sim_years=3





#------------------------------------------------------------------------------#
#Set seed: 
#------------------------------------------------------------------------------#
seed_value <- 1234







#------------------------------------------------------------------------------#
#Predict the maturity at age
#------------------------------------------------------------------------------#
tmp <- get_mopred(fit)
mo_pred <- tmp$mo_pred
forecast_data <- tmp$forecast_data




#------------------------------------------------------------------------------#
#plot the maturity at age
#------------------------------------------------------------------------------#
p<-ggplot(forecast_data[forecast_data$age%in%c(3:8),],aes(y=propMat,x=log(Estimate)))+geom_point()+facet_wrap(~age)+
  geom_line(aes(y=mo_pred,x=log(Estimate)),colour='blue')+
  geom_point(data=forecast_data[forecast_data$year>=2026 & forecast_data$age%in%c(3:8),],
             aes(x=log(Estimate),y=round(mo_pred,digits = 1),colour=as.factor(year)))+
  theme_bw()+
  labs(colour = "Year")
ggsave("report/figures/fig_predicted_maturity.png", p, width = 8, height = 6, dpi = 600)






#------------------------------------------------------------------------------#
#Add predicted maturity into the future
#------------------------------------------------------------------------------#
fit2 <- fit
fit2$data$propMat<-rbind(fit2$data$propMat,mo_pred[rownames(mo_pred)>=(max(fit$data$years)+1),])



#------------------------------------------------------------------------------#
#A hack as there is something strange with the dimension in the fit object
#------------------------------------------------------------------------------#
if(length(dim(fit2$data$catchMeanWeight))==3){fit2$data$catchMeanWeight<-fit2$data$catchMeanWeight[,,]}
fit2$data$landFrac<-fit2$data$landFrac[,,]
fit2$data$landMeanWeight<-fit2$data$landMeanWeight[,,]
fit2$data$propF<-fit2$data$propF[,,]





#------------------------------------------------------------------------------#
#Add predicted maturity and do forecast
#------------------------------------------------------------------------------#
print('Run 1/8 MP')



#------------------------------------------------------------------------------#
#Compute the TAC for next year. 
#The structure is written to mimic the XSAM structure of forecast
#------------------------------------------------------------------------------#
fBarW =  c(NA,rep(fmp,sim_years))
catchval.exact = c(nation_quota,rep(NA,sim_years))
set.seed(seed_value)
tt<-forecast_XSAM(fit2,fBarW =  fBarW,
                  catchval.exact = catchval.exact,
                  ave.years = ave.years,
                  rec.years = rec.years,
                  deterministic = F,
                  bias_correct = F,
                  rec_from_par = rec_from_par,
                  switch_year_range = switch_year_range)


#Apply management plan, and update the fbarW
for(i in 1:sim_years){
  print(i)
  
  #update the projection, using the MP if below Btrigger
  fBarW[i+1]<-MP$F[MP$ssb==round(median(tt[[i+1]]$ssb))]
  
  #for future reference in the table
  if(i==1)new_F<-fBarW[i+1]
  
  #The cap rule 
  if(round(median(tt[i+1][[1]]$ssb))>=bpa){
    #If larger then cap
    if(((median(tt[[i+1]]$catch)-median(tt[[i]]$catch))/median(tt[[i]]$catch))>TAC_cap[2]){
      fBarW[i+1] =  NA
      catchval.exact[i+1] = median(tt[[i]]$catch)*(1+TAC_cap[2])
    }
    
    if(((median(tt[[i+1]]$catch)-median(tt[[i]]$catch))/median(tt[[i]]$catch))<(-TAC_cap[1])){
      fBarW[i+1] =  NA
      catchval.exact[i+1] = median(tt[[i]]$catch)*(1-TAC_cap[1])
    }
  }
  
  #Refresh
  set.seed(seed_value)
  tt<-forecast_XSAM(fit2,fBarW =  fBarW,
                    catchval.exact = catchval.exact,
                    ave.years = ave.years,
                    rec.years = rec.years,
                    deterministic = F,
                    bias_correct = F,
                    rec_from_par = rec_from_par,
                    switch_year_range = switch_year_range)
  fBarW[i+1] =  NA
  catchval.exact[i+1] = median(tt[[i+1]]$catch)
  print(tt)
}



#Do the stochastic forecast on the management plan
set.seed(seed_value)
tt_stochastic<-forecast_XSAM(fit2,
                             fBarW =  fBarW,
                             catchval.exact = catchval.exact,
                             ave.years = ave.years,
                             rec.years = rec.years,
                             deterministic = F,
                             rec_from_par = rec_from_par,
                             bias_correct = F,
                             switch_year_range = switch_year_range,addTSB = T)
save(tt_stochastic,file='model/forecast.Rda')




#------------------------------------------------------------------------------#
#MSY approach
#------------------------------------------------------------------------------#
print('Run 2/8 FMsy')
fbarW <- c(NA,fmsy,fmsy,fmsy)
catchval.exact = c(nation_quota,NA,NA,NA)
for(i in 2:4){
  print(i)
  set.seed(seed_value)
  tt<-forecast_XSAM(fit2,fBarW =  fbarW,
                    catchval.exact = catchval.exact,
                    ave.years = ave.years,
                    rec.years = rec.years,
                    deterministic = F,
                    bias_correct = F,
                    rec_from_par = rec_from_par,
                    switch_year_range = switch_year_range)
  
  #reduce F if SSB is below btrigger
  fbarW[i]<-fbarW[i]*median(tt[[i]]$ssb)/msy_btrigger

  #if so, run the new F
  if(fbarW[i]<fmsy){
    tt<-forecast_XSAM(fit2,fBarW =  fbarW,
                      catchval.exact = catchval.exact,
                      ave.years = ave.years,
                      rec.years = rec.years,
                      deterministic = F,
                      bias_correct = F,
                      rec_from_par = rec_from_par,
                      switch_year_range = switch_year_range)
  }
  
  #store the new F
  if(i==2)new_Fmsy<-round(fbarW[2],digits = 3)
  
  fbarW[i]<-NA
  catchval.exact[i]<- median(tt[[i]]$catch)
}

set.seed(seed_value)
tt_stochastic<-forecast_XSAM(fit2,fBarW =  c(NA,NA,NA,NA),
                             catchval.exact  =  c(median(tt[[1]]$catch),
                                                  median(tt[[2]]$catch),
                                                  median(tt[[3]]$catch),
                                                  median(tt[[4]]$catch)),
                             ave.years = ave.years,
                             deterministic = F,
                             rec.years = rec.years,
                             rec_from_par = rec_from_par,
                             switch_year_range = switch_year_range,
                             bias_correct = F)

save(tt_stochastic,file='model/forecast_MSY.Rda')





#------------------------------------------------------------------------------#
#0 catch
#------------------------------------------------------------------------------#
print('Run 3/8 0-catch')
set.seed(seed_value)
tt_stochastic<-forecast_XSAM(fit2,
                             catchval.exact = c(nation_quota,0,0,0),
                             ave.years = ave.years,
                             rec.years = rec.years,
                             deterministic = F,
                             bias_correct = F,
                             rec_from_par = rec_from_par,
                             switch_year_range = switch_year_range)
save(tt_stochastic,file='model/forecast_0catch.Rda')




#------------------------------------------------------------------------------#
#Fpa
#------------------------------------------------------------------------------#
print('Run 4/8 Fpa')
set.seed(seed_value)
tt<-forecast_XSAM(fit2,fBarW =  c(NA,fmsy,fmsy,fmsy),
                  catchval.exact = c(nation_quota,NA,NA,NA),
                  ave.years = ave.years,
                  rec.years = rec.years,
                  deterministic = F,
                  bias_correct = F,
                  rec_from_par = rec_from_par,
                  switch_year_range = switch_year_range)

set.seed(seed_value)
tt_stochastic<-forecast_XSAM(fit2,fBarW =  c(NA,NA,NA,NA),
                             catchval.exact  =  c(median(tt[[1]]$catch),
                                                  median(tt[[2]]$catch),
                                                  median(tt[[3]]$catch),
                                                  median(tt[[4]]$catch)),
                             ave.years = ave.years,
                             deterministic = F,
                             rec.years = rec.years,
                             rec_from_par = rec_from_par,
                             switch_year_range = switch_year_range,
                             bias_correct = F)
save(tt_stochastic,file='model/forecast_Fpa.Rda')



#------------------------------------------------------------------------------#
#Flim
#------------------------------------------------------------------------------#
print('Run 5/8 Flim')
set.seed(seed_value)
tt<-forecast_XSAM(fit2,fBarW =  c(NA,flim,flim,flim),
                  catchval.exact = c(nation_quota,NA,NA,NA),ave.years = ave.years,
                  rec.years = rec.years,deterministic = F,bias_correct = F,
                  rec_from_par = rec_from_par,switch_year_range = switch_year_range,)

set.seed(seed_value)
tt_stochastic<-forecast_XSAM(fit2,fBarW =  c(NA,NA,NA,NA),
                             catchval.exact  =  c(median(tt[[1]]$catch),
                                                  median(tt[[2]]$catch),
                                                  median(tt[[3]]$catch),
                                                  median(tt[[4]]$catch)),
                             ave.years = ave.years,
                             deterministic = F,
                             rec.years = rec.years,
                             rec_from_par = rec_from_par,
                             switch_year_range = switch_year_range,
                             bias_correct = F)
save(tt_stochastic,file='model/forecast_Flim.Rda')




#------------------------------------------------------------------------------#
#SSB = blim
#------------------------------------------------------------------------------#
print('Run 6/8 Blim')
set.seed(seed_value)


tt_stochastic<-forecast_XSAM(fit2,
                             fBarW =  c(NA,NA,NA,NA),
                             catchval.exact  =  c(nation_quota,NA,NA,NA),
                             nextssb = c(NA,blim,blim,blim),
                             ave.years = ave.years,
                             deterministic = F,
                             rec.years = rec.years,
                             rec_from_par = rec_from_par,
                             switch_year_range = switch_year_range,
                             bias_correct = F)

save(tt_stochastic,file='model/forecast_Blim.Rda')




#------------------------------------------------------------------------------#
#SSB = Bpa
#------------------------------------------------------------------------------#
print('Run 7/8 Bpa')
set.seed(seed_value)
tt_stochastic<-forecast_XSAM(fit2,fBarW =  c(NA,NA,NA,NA),
                             catchval.exact  =  c(nation_quota,NA,NA,NA),
                             nextssb = c(NA,msy_btrigger,msy_btrigger,msy_btrigger),
                             ave.years = ave.years,
                             deterministic = F,
                             rec.years = rec.years,
                             rec_from_par = rec_from_par,
                             switch_year_range = switch_year_range,
                             bias_correct = F)
save(tt_stochastic,file='model/forecast_Bpa.Rda')



#------------------------------------------------------------------------------#
#Fsq
#------------------------------------------------------------------------------#
load('model/forecast.Rda')
print('Run 8/8 SQ')
f_sq <- fbartable(tt_stochastic)[1,1]
tt<-forecast_XSAM(fit2,fBarW =  c(NA,f_sq,f_sq,f_sq),
                  catchval.exact = c(nation_quota,NA,NA,NA),
                  ave.years = ave.years,
                  rec.years = rec.years,
                  deterministic = F,
                  bias_correct = F,
                  rec_from_par = rec_from_par,
                  switch_year_range = switch_year_range)
tt_stochastic<-forecast_XSAM(fit2,fBarW =  c(NA,NA,NA,NA),
                             catchval.exact  =  c(median(tt[[1]]$catch),
                                                  median(tt[[2]]$catch),
                                                  median(tt[[3]]$catch),
                                                  median(tt[[4]]$catch)),
                             ave.years = ave.years,
                             deterministic = F,
                             rec.years = rec.years,
                             rec_from_par = rec_from_par,
                             switch_year_range = switch_year_range,
                             bias_correct = F)
save(tt_stochastic,file='model/forecast_SQ.Rda')







#------------------------------------------------------------------------------#
#Basis table
#------------------------------------------------------------------------------#
load('model/forecast.Rda')
basis <- data.frame(
  'SSB_intermediat' = round(median(tt_stochastic[[1]]$ssb)),
  catch_intermediat = round(median(tt_stochastic[[1]]$catch)*1000),
  'SSB_forecast'=median(tt_stochastic[[2]]$ssb),
  'FbarW_intermediat'=round(median(tt_stochastic[[1]]$fbar),digits = 3),
  'Recruitment'=paste(round(c(median(tt_stochastic[[1]]$rec),
                              median(tt_stochastic[[2]]$rec),
                              median(tt_stochastic[[3]]$rec))/1000,digits = 3),
                      sep = ',')
)






#------------------------------------------------------------------------------#
#Output table
#------------------------------------------------------------------------------#
out_data <- c()
out_table <- c()





#------------------------------------------------------------------------------#
#Management plan
#------------------------------------------------------------------------------#
load('model/forecast.Rda')
sam_forecast <- SAMforecast_output(tt_stochastic)
sam_forecast<-merge(sam_forecast,
                    data.frame('year'=tt_stochastic[[1]]$year:tt_stochastic[[4]]$year,'CatchSAM'=
                                 c(median(tt_stochastic[[1]]$catch),median(tt_stochastic[[2]]$catch),
                                   median(tt_stochastic[[3]]$catch),median(tt_stochastic[[4]]$catch))))
sam_forecast$rationale <- 'MP'
sam_forecast$assessment_year <- max(fit2$data$years)
out_data<-rbind(out_data,sam_forecast)


Catches = median(tt_stochastic[[2]]$catch)*1000
fbarw <- round(median(tt_stochastic[[2]]$fbar),digits = 3)
fbarw_q <- as.numeric(round(quantile(tt_stochastic[[2]]$fbar, probs = c(0.05, 0.95)),digits=3))
ssb <- round(median(tt_stochastic[[3]]$ssb),digits = 3)
ssb_q <- as.numeric(round(quantile(tt_stochastic[[3]]$ssb, probs = c(0.05, 0.95)),digits=3))
risc = mean(tt_stochastic[[3]]$ssb<blim)

ssbchange = (ssb-basis[1,]$SSB_forecast)/basis[1,]$SSB_forecast*100
TACchange = round((Catches-TAC)/TAC*100,digits = 1)
Catchchange = round((Catches/1000-nation_quota)/nation_quota*100,digits = 1)

out_table <- rbind(out_table,
                   data.frame(Rationale = 'management strategy',
                              Catches = Catches,
                              Basis = 'F=0.14',
                              Finn = round(new_F,3),
                              FW  = paste0(fbarw,'(',fbarw_q[1],',',fbarw_q[2],')'),
                              ssb  = paste0(ssb,'(',ssb_q[1],',',ssb_q[2],')'),
                              ssb_pred = ssb,
                              risc = risc,
                              ssbchange = ssbchange,
                              TACchange = TACchange,
                              Catchchange = Catchchange
                   )
                   )





#------------------------------------------------------------------------------#
#FMSY approach
#------------------------------------------------------------------------------#
load('model/forecast_MSY.Rda')

sam_forecast <- SAMforecast_output(tt_stochastic)
sam_forecast<-merge(sam_forecast,
                    data.frame('year'=tt_stochastic[[1]]$year:tt_stochastic[[4]]$year,'CatchSAM'=
                                 c(median(tt_stochastic[[1]]$catch),median(tt_stochastic[[2]]$catch),median(tt_stochastic[[3]]$catch),median(tt_stochastic[[4]]$catch))))
sam_forecast$rationale <- 'F=0.157'
sam_forecast$assessment_year <- max(fit2$data$years)




Catches = median(tt_stochastic[[2]]$catch)*1000
fbarw <- round(median(tt_stochastic[[2]]$fbar),digits = 3)
fbarw_q <- as.numeric(round(quantile(tt_stochastic[[2]]$fbar, probs = c(0.05, 0.95)),digits=3))
ssb <- round(median(tt_stochastic[[3]]$ssb),digits = 3)
ssb_q <- as.numeric(round(quantile(tt_stochastic[[3]]$ssb, probs = c(0.05, 0.95)),digits=3))
risc = mean(tt_stochastic[[3]]$ssb<blim)


ssbchange = (ssb-basis[1,]$SSB_forecast)/basis[1,]$SSB_forecast*100
TACchange = round((Catches-TAC)/TAC*100,digits = 1)
Catchchange = round((Catches/1000-nation_quota)/nation_quota*100,digits = 1)


out_table <- rbind(out_table,
                   data.frame(Rationale = 'MSY approach',
                              Basis = 'F=0.21',
                              Catches = Catches,
                              Finn = new_Fmsy,
                              FW  = paste0(fbarw,'(',fbarw_q[1],',',fbarw_q[2],')'),
                              ssb  = paste0(ssb,'(',ssb_q[1],',',ssb_q[2],')'),
                              ssb_pred = ssb,
                              risc = risc,
                              ssbchange = ssbchange,
                              TACchange = TACchange,
                              Catchchange = Catchchange
                   ))



#------------------------------------------------------------------------------#
#Zero catch
#------------------------------------------------------------------------------#
load('model/forecast_0catch.Rda')
sam_forecast <- SAMforecast_output(tt)
sam_forecast<-merge(sam_forecast,
                    data.frame('year'=tt[[1]]$year:tt[[2]]$year,'CatchSAM'=
                                 c(median(tt[[1]]$catch),median(tt[[2]]$catch),median(tt[[3]]$catch),median(tt[[4]]$catch))))
sam_forecast$rationale <- 'F=0'
sam_forecast$assessment_year <- max(fit2$data$years)



Catches = median(tt_stochastic[[2]]$catch)*1000
fbarw <- round(median(tt_stochastic[[2]]$fbar),digits = 3)
fbarw_q <- as.numeric(round(quantile(tt_stochastic[[2]]$fbar, probs = c(0.05, 0.95)),digits=3))
ssb <- round(median(tt_stochastic[[3]]$ssb),digits = 3)
ssb_q <- as.numeric(round(quantile(tt_stochastic[[3]]$ssb, probs = c(0.05, 0.95)),digits=3))
risc = mean(tt_stochastic[[3]]$ssb<blim)


ssbchange = (ssb-basis[1,]$SSB_forecast)/basis[1,]$SSB_forecast*100
TACchange = round((Catches-TAC)/TAC*100,digits = 1)
Catchchange = round((Catches/1000-nation_quota)/nation_quota*100,digits = 1)

out_table <- rbind(out_table,
                   data.frame(Rationale = 'Zero Catch',
                              Basis = 'F=0',
                              Catches = Catches,
                              Finn = 0,
                              FW  = paste0(fbarw,'(',fbarw_q[1],',',fbarw_q[2],')'),
                              ssb  = paste0(ssb,'(',ssb_q[1],',',ssb_q[2],')'),
                              ssb_pred = ssb,
                              risc = risc,
                              ssbchange = ssbchange,
                              TACchange = TACchange,
                              Catchchange = Catchchange
                   ))





#------------------------------------------------------------------------------#
# Fpa
#------------------------------------------------------------------------------#
load('model/forecast_Fpa.Rda')
sam_forecast <- SAMforecast_output(tt_stochastic)
sam_forecast<-merge(sam_forecast,
                    data.frame('year'=tt_stochastic[[1]]$year:tt_stochastic[[4]]$year,'CatchSAM'=
                                 c(median(tt_stochastic[[1]]$catch),median(tt_stochastic[[2]]$catch),
                                   median(tt_stochastic[[3]]$catch),median(tt_stochastic[[4]]$catch))))
sam_forecast$rationale <- 'F=Fpa'
sam_forecast$assessment_year <- max(fit2$data$years)



Catches = median(tt_stochastic[[2]]$catch)*1000
fbarw <- round(median(tt_stochastic[[2]]$fbar),digits = 3)
fbarw_q <- as.numeric(round(quantile(tt_stochastic[[2]]$fbar, probs = c(0.05, 0.95)),digits=3))
ssb <- round(median(tt_stochastic[[3]]$ssb),digits = 3)
ssb_q <- as.numeric(round(quantile(tt_stochastic[[3]]$ssb, probs = c(0.05, 0.95)),digits=3))
risc = mean(tt_stochastic[[3]]$ssb<blim)


ssbchange = (ssb-basis[1,]$SSB_forecast)/basis[1,]$SSB_forecast*100
TACchange = round((Catches-TAC)/TAC*100,digits = 1)
Catchchange = round((Catches/1000-nation_quota)/nation_quota*100,digits = 1)


out_table <- rbind(out_table,
                   data.frame(Rationale = 'Fpa',
                              Basis = 'F=Fpa',
                              Catches = Catches,
                              Finn = fmsy,
                              FW  = paste0(fbarw,'(',fbarw_q[1],',',fbarw_q[2],')'),
                              ssb  = paste0(ssb,'(',ssb_q[1],',',ssb_q[2],')'),
                              ssb_pred = ssb,
                              risc = risc,
                              ssbchange = ssbchange,
                              TACchange = TACchange,
                              Catchchange = Catchchange
                   ))





# 
# #------------------------------------------------------------------------------#
# #Flim
# #------------------------------------------------------------------------------#
# load('model/forecast_Flim.Rda')
# 
# 
# sam_forecast <- SAMforecast_output(tt_stochastic)
# sam_forecast<-merge(sam_forecast,
#                     data.frame('year'=tt_stochastic[[1]]$year:tt_stochastic[[4]]$year,'CatchSAM'=
#                                  c(median(tt_stochastic[[1]]$catch),median(tt_stochastic[[2]]$catch),
#                                    median(tt_stochastic[[3]]$catch),median(tt_stochastic[[4]]$catch))))
# sam_forecast$rationale <- 'F=Flim'
# sam_forecast$assessment_year <- max(fit2$data$years)
# 
# 
# 
# 
# 
# Catches = median(tt_stochastic[[2]]$catch)*1000
# fbarw <- round(median(tt_stochastic[[2]]$fbar),digits = 3)
# fbarw_q <- as.numeric(round(quantile(tt_stochastic[[2]]$fbar, probs = c(0.05, 0.95)),digits=3))
# ssb <- round(median(tt_stochastic[[3]]$ssb),digits = 3)
# ssb_q <- as.numeric(round(quantile(tt_stochastic[[3]]$ssb, probs = c(0.05, 0.95)),digits=3))
# risc = mean(tt_stochastic[[3]]$ssb<blim)
# ssbchange = round((round(median(tt_stochastic[[3]]$ssb),digits = 3)-
#                      round(basis[1,]$SSB_forecast,digits = 3))/
#                     round(basis[1,]$SSB_forecast,digits = 3)*100,digits = 3)
# TACchange = round((Catches-TAC)/TAC*100,digits = 1)
# Catchchange = round((median(tt_stochastic[[2]]$catch)-nation_quota)/nation_quota*100,digits = 1)
# 
# 
# out_table <- rbind(out_table,
#                    data.frame(Rationale = 'Flim',
#                               Basis = 'F=0.291',
#                               Catches = Catches,
#                               Finn = 0.291,
#                               FW  = paste0(fbarw,'(',fbarw_q[1],',',fbarw_q[2],')'),
#                               ssb  = paste0(ssb,'(',ssb_q[1],',',ssb_q[2],')'),
#                               risc = risc,
#                               ssbchange = ssbchange,
#                               TACchange = TACchange,
#                               Catchchange = Catchchange
#                    ))
# 
# 



#------------------------------------------------------------------------------#
#SSB = BLIM
#------------------------------------------------------------------------------#
load('model/forecast_Blim.Rda')
sam_forecast <- SAMforecast_output(tt_stochastic)
sam_forecast<-merge(sam_forecast,
                    data.frame('year'=tt_stochastic[[1]]$year:tt_stochastic[[4]]$year,'CatchSAM'=
                                 c(median(tt_stochastic[[1]]$catch),
                                   median(tt_stochastic[[2]]$catch),
                                   median(tt_stochastic[[3]]$catch),
                                   median(tt_stochastic[[4]]$catch))))
sam_forecast$rationale <- 'SSB=Blim'
sam_forecast$assessment_year <- max(fit2$data$years)



Catches = median(tt_stochastic[[2]]$catch)*1000
fbarw <- round(median(tt_stochastic[[2]]$fbar),digits = 3)
fbarw_q <- as.numeric(round(quantile(tt_stochastic[[2]]$fbar, probs = c(0.05, 0.95)),digits=3))
ssb <- blim#round(median(tt_stochastic[[3]]$ssb),digits = 3)
ssb_q <- as.numeric(round(quantile(tt_stochastic[[3]]$ssb, probs = c(0.05, 0.95)),digits=3))
#Small hack as SAM forecast struggles to hit the blim
risc = mean((tt_stochastic[[3]]$ssb-(median(tt_stochastic[[3]]$ssb)-blim)<blim))#mean(tt_stochastic[[3]]$ssb<blim)


ssbchange = (ssb-basis[1,]$SSB_forecast)/basis[1,]$SSB_forecast*100
TACchange = round((Catches-TAC)/TAC*100,digits = 1)
Catchchange = round((Catches/1000-nation_quota)/nation_quota*100,digits = 1)


out_table <- rbind(out_table,
                   data.frame(Rationale = 'SSB=Blim',
                              Basis = paste0('F=',fbarw),
                              Catches = Catches,
                              Finn = fbarw,
                              FW  = paste0(fbarw,'(',fbarw_q[1],',',fbarw_q[2],')'),
                              ssb  = paste0(ssb,'(',ssb_q[1],',',ssb_q[2],')'),
                              ssb_pred = ssb,
                              risc = risc,
                              ssbchange = ssbchange,
                              TACchange = TACchange,
                              Catchchange = Catchchange
                   ))


#------------------------------------------------------------------------------#
#SSB = Bpa
#------------------------------------------------------------------------------#

load('model/forecast_Bpa.Rda')
sam_forecast <- SAMforecast_output(tt_stochastic)
sam_forecast<-merge(sam_forecast,
                    data.frame('year'=tt_stochastic[[1]]$year:tt_stochastic[[4]]$year,'CatchSAM'=
                                 c(median(tt_stochastic[[1]]$catch),
                                   median(tt_stochastic[[2]]$catch),
                                   median(tt_stochastic[[3]]$catch),
                                   median(tt_stochastic[[4]]$catch))))
sam_forecast$rationale <- 'SSB=Bpa'
sam_forecast$assessment_year <- max(fit2$data$years)


Catches = median(tt_stochastic[[2]]$catch)*1000
fbarw <- round(median(tt_stochastic[[2]]$fbar),digits = 3)
fbarw_q <- as.numeric(round(quantile(tt_stochastic[[2]]$fbar, probs = c(0.05, 0.95)),digits=3))
ssb <- msy_btrigger#round(median(tt_stochastic[[3]]$ssb),digits = 3)
ssb_q <- as.numeric(round(quantile(tt_stochastic[[3]]$ssb, probs = c(0.05, 0.95)),digits=3))
#Smal hack as SAM struggles to hit bpa
risc = mean((tt_stochastic[[3]]$ssb-(median(tt_stochastic[[3]]$ssb)-msy_btrigger)<blim))#mean(tt_stochastic[[3]]$ssb<blim)



ssbchange = (ssb-basis[1,]$SSB_forecast)/basis[1,]$SSB_forecast*100
TACchange = round((Catches-TAC)/TAC*100,digits = 1)
Catchchange = round((Catches/1000-nation_quota)/nation_quota*100,digits = 1)


out_table <- rbind(out_table,
                   data.frame(Rationale = 'SSB=MSY Btrigger',
                              Basis = paste0('F=',fbarw),
                              Catches = Catches,
                              Finn = fbarw,
                              FW  = paste0(fbarw,'(',fbarw_q[1],',',fbarw_q[2],')'),
                              ssb  = paste0(ssb,'(',ssb_q[1],',',ssb_q[2],')'),
                              ssb_pred = ssb,
                              risc = risc,
                              ssbchange = ssbchange,
                              TACchange = TACchange,
                              Catchchange = Catchchange
                   ))





#------------------------------------------------------------------------------#
#FbarW = Status quo
#------------------------------------------------------------------------------#
load('model/forecast_SQ.Rda')
sam_forecast <- SAMforecast_output(tt_stochastic)
sam_forecast<-merge(sam_forecast,
                    data.frame('year'=tt_stochastic[[1]]$year:tt_stochastic[[4]]$year,'CatchSAM'=
                                 c(median(tt_stochastic[[1]]$catch),
                                   median(tt_stochastic[[2]]$catch),
                                   median(tt_stochastic[[3]]$catch),median(tt_stochastic[[4]]$catch))))
sam_forecast$rationale <- 'SQ'
sam_forecast$assessment_year <- max(fit2$data$years)



Catches = median(tt_stochastic[[2]]$catch)*1000
fbarw <- round(median(tt_stochastic[[2]]$fbar),digits = 3)
fbarw_q <- as.numeric(round(quantile(tt_stochastic[[2]]$fbar, probs = c(0.05, 0.95)),digits=3))
ssb <- round(median(tt_stochastic[[3]]$ssb),digits = 3)
ssb_q <- as.numeric(round(quantile(tt_stochastic[[3]]$ssb, probs = c(0.05, 0.95)),digits=3))
risc = mean(tt_stochastic[[3]]$ssb<blim)


ssbchange = (ssb-basis[1,]$SSB_forecast)/basis[1,]$SSB_forecast*100
TACchange = round((Catches-TAC)/TAC*100,digits = 1)
Catchchange = round((Catches/1000-nation_quota)/nation_quota*100,digits = 1)


out_table <- rbind(out_table,
                   data.frame(Rationale = 'SQ',
                              Basis = paste0('F=',f_sq),
                              Catches = Catches,
                              Finn = f_sq,
                              FW  = paste0(fbarw,'(',fbarw_q[1],',',fbarw_q[2],')'),
                              ssb  = paste0(ssb,'(',ssb_q[1],',',ssb_q[2],')'),
                              ssb_pred = ssb,
                              risc = risc,
                              ssbchange = ssbchange,
                              TACchange = TACchange,
                              Catchchange = Catchchange
                   ))






#------------------------------------------------------------------------------#
#Write input table
#------------------------------------------------------------------------------#
load('model/forecast.Rda')
tmp <- data.frame(
  age=min(fit2$data$minAgePerFleet):max(fit2$data$maxAgePerFleet),
  stockno = as.numeric(tail(ntable(fit2),1)),
  natmort=as.numeric(tail(fit2$data$natMor,1)),
  maturity=as.numeric(tail(fit$data$propMat,1)),
  PropM=as.numeric(tail(fit2$data$propM,1)),
  PropF=as.numeric(tail(fit2$data$propF,1)),
  SWeight=as.numeric(round(tail(fit2$data$stockMeanWeight,1),digits = 3)),
  Explot = as.numeric(round(tail(faytable(fit),1),3)),
  CWeight=round(colMeans(tail(fit2$data$catchMeanWeight,3)),digits = 3)
)
write.csv2(tmp,
           file='report/tables/4.8.1.1a.txt'
)



#------------------------------------------------------------------------------#
#Write second input table
#Need to check the rule
#------------------------------------------------------------------------------#
tmp$stockno[-1]<-NA
tmp$maturity <- as.numeric(head(tail(fit2$data$propMat,n=3),n=1))
tmp$maturity2 <- as.numeric(tail(head(tail(fit2$data$propMat,n=3),n=2),n=1))
SWeight=round(colMeans(tail(fit2$data$stockMeanWeight,3)),digits = 3)
write.csv2(
  tmp,
  file='report/tables/4.8.1.1b.txt'
)





#------------------------------------------------------------------------------#
#Figure 4.8.1.1
#------------------------------------------------------------------------------#
load('model/forecast.Rda')
p<-forecast_exploitation_plot(tt=tt_stochastic,fit=fit2)
p
ggsave(filename = 'report/figures/Fig.4.8.1.1.png', 
       plot = p,
       height=8,
       width=15, 
       units="cm", 
       dpi = 300,  
       bg = "transparent")



# 
# 
# #------------------------------------------------------------------------------#
# #Figure TSB
# #------------------------------------------------------------------------------#
# png('report/figures/TSB.png')
# stockassessment::tsbplot(tt_stochastic)
# dev.off()




#------------------------------------------------------------------------------#
#Handle table 4.8.2.1
#------------------------------------------------------------------------------#
load('model/forecast.Rda')
# write.csv2(
#   data.frame(
#     'SSB_intermediat' = round(median(tt_stochastic[[1]]$ssb)),
#     catch_intermediat = round(median(tt_stochastic[[1]]$catch)*1000),
#     'SSB_forecast'=round(median(tt_stochastic[[2]]$ssb)),
#     'FbarW_intermediat'=round(median(tt_stochastic[[1]]$fbar),digits = 3),
#     'Recruitment'=paste(round(c(median(tt_stochastic[[1]]$rec),
#                                 median(tt_stochastic[[2]]$rec),
#                                 median(tt_stochastic[[3]]$rec))/1000,digits = 3),
#                         sep = ',')
#   ),
#   file = 'report/tables/tab4.8.2.1a.txt')
# 


oout <- data.frame(
  'SSB_intermediat' = paste(round(tail(ssbtable(fit),n=1)[1]/1000,digits=3)),
  'catch_intermediat' = round(median(tt_stochastic[[1]]$catch)*1000),
  'SSB_next' = paste(round(median(tt_stochastic[[2]]$ssb)/1000,digits=3)),
  'FbarW_intermediat'=paste(round(median(tt_stochastic[[1]]$fbar),digits = 3)),
  'rec_intermediat' = round(tail(rectable(fit),n=1)[1]),
  'rec_next' = round(median(tt_stochastic[[2]]$rec)),
  'rec_next2' = round(median(tt_stochastic[[3]]$rec))
)



write.csv2(t(oout),file = 'report/tables/tab4.8.2.1a.txt')

#------------------------------------------------------------------------------#
#Plot 3 year projection
#------------------------------------------------------------------------------#
load('model/forecast.Rda')
sam_forecast <- SAMforecast_output(tt_stochastic)
sam_forecast<-merge(sam_forecast,
                    data.frame('year'=tt_stochastic[[1]]$year:tt_stochastic[[4]]$year,'
                               CatchSAM'=
                                 c(mean(tt_stochastic[[1]]$catch),
                                   mean(tt_stochastic[[2]]$catch),
                                   mean(tt_stochastic[[3]]$catch),
                                   mean(tt_stochastic[[4]]$catch))))
sam_forecast$assessment_year <- max(fit$data$years)


#Merge with assessment run 
ssb <- as.data.frame(stockassessment::ssbtable(fit2))
ssb$year <- as.numeric(rownames(ssb))
names(ssb)<-c('ssb','ssblow','ssbhigh','year')
dd<-sam_forecast[,c('year','ssb','ssblow','ssbhigh')]


ssbmgt<-rbind(ssb,dd[dd$year>max(ssb$year),])
ssbmgt$facet <- 'mgt'


fbar <- compute_FbarW(fit2)
names(fbar)<-c('fbar','fbarlow','fbarhigh','year')
fbar<-rbind(fbar,sam_forecast[,c('year','fbar','fbarlow','fbarhigh')])


load('model/forecast_MSY.Rda')
sam_forecast <- SAMforecast_output(tt_stochastic)
sam_forecast<-merge(sam_forecast,
                    data.frame('year'=tt_stochastic[[1]]$year:tt_stochastic[[4]]$year,'
                               CatchSAM'=
                                 c(mean(tt_stochastic[[1]]$catch),
                                   mean(tt_stochastic[[2]]$catch),
                                   mean(tt_stochastic[[3]]$catch),
                                   mean(tt_stochastic[[4]]$catch))))
sam_forecast$assessment_year <- max(fit$data$years)


fbar_msy <- compute_FbarW(fit2)
names(fbar_msy)<-c('fbar','fbarlow','fbarhigh','year')
fbar_msy<-rbind(fbar_msy,sam_forecast[,c('year','fbar','fbarlow','fbarhigh')])


#Merge with assessment run 
ssb <- as.data.frame(stockassessment::ssbtable(fit2))
ssb$year <- as.numeric(rownames(ssb))
names(ssb)<-c('ssb','ssblow','ssbhigh','year')
dd<-sam_forecast[,c('year','ssb','ssblow','ssbhigh')]


ssbmsy<-rbind(ssb,dd[dd$year>max(ssb$year),])
ssbmsy$facet <- 'msy'


ssb<-rbind(ssbmgt,ssbmsy)

plot_ssb <- ggplot(data=ssb[ssb$facet=='mgt',], aes(x=year, y=as.numeric(ssb), 
                                 ymin=ssblow, ymax=ssbhigh)) +
  annotate("rect",
           xmin = -Inf, xmax = Inf,  # hele x-aksen
           ymin = -Inf, ymax = msy_btrigger,  # fra bunnen opp til bpa
           fill = "red", alpha = 0.1) +
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = msy_btrigger, ymax = Inf,
           fill = "green", alpha = 0.1) +
  geom_ribbon(alpha=0.4) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = msy_btrigger) +
  geom_vline(xintercept = max(fit$data$years)) +
  ggtitle('SSB from 3 year forecast') +
  ylab('SSB') +
  theme_bw()
plot_ssb_MSY <- ggplot(data=ssb[ssb$facet=='msy',], aes(x=year, y=as.numeric(ssb),
                                                    ymin=ssblow, ymax=ssbhigh)) +
  annotate("rect",
           xmin = -Inf, xmax = Inf,  # hele x-aksen
           ymin = -Inf, ymax = msy_btrigger,  # fra bunnen opp til bpa
           fill = "red", alpha = 0.1) +
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = msy_btrigger, ymax = Inf,
           fill = "green", alpha = 0.1) +
  geom_ribbon(alpha=0.4) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = msy_btrigger) +
  geom_vline(xintercept = max(fit$data$years)) +
  ggtitle('SSB from 3 year forecast (MSY)') +
  ylab('SSB') +
  theme_bw()

plot_fbar <- ggplot(data=fbar,aes(x=year,y=fbar,ymin=fbarlow,ymax=fbarhigh))+
  annotate("rect",
           xmin = -Inf, xmax = Inf,  
           ymin = -Inf, ymax = fmp,  
           fill = "green", alpha = 0.1) +
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = fmp, ymax = Inf,
           fill = "red", alpha = 0.1) +
  geom_line()+geom_point()+
  geom_ribbon(alpha=0.4)+
  ggtitle('fbar from 3 year forecast')+
  geom_hline(yintercept = fmp)+
  geom_vline(xintercept = max(fit$data$years))+ylab('FbarW')+theme_bw()


plot_fbar_MSY <- ggplot(data=fbar_msy,aes(x=year,y=fbar,ymin=fbarlow,ymax=fbarhigh))+
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = -Inf, ymax = fmsy,
           fill = "green", alpha = 0.1) +
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = fmsy, ymax = Inf,
           fill = "red", alpha = 0.1) +
  geom_line()+geom_point()+
  geom_ribbon(alpha=0.4)+
  ggtitle('fbar from 3 year forecast (MSY)')+
  geom_hline(yintercept = fmsy)+
  geom_vline(xintercept = max(fit$data$years))+ylab('FbarW')+theme_bw()

png('output/figures/Fig.4.8.1.2.png')
gridExtra::grid.arrange(plot_ssb,plot_fbar,plot_ssb_MSY,plot_fbar_MSY,ncol=2)
dev.off()





#------------------------------------------------------------------------------#
#Predicted age distribution in catch, management plan
#------------------------------------------------------------------------------#
load('model/forecast.Rda')
caa <- stockassessment::read.ices(taf.data.path('caa.txt'))
caa <- tail(caa)
for(i in 1:nrow(caa)){
  caa[i,]<-caa[i,]/max(caa[i,])
}
caa <- reshape2::melt(caa)
names(caa)<- c('year','age','caa')
dd <- c()
for(i in 1:length(tt_stochastic)){
  
  tmp<- data.frame(age=2:12,
                   caa = rowSums(tt_stochastic[[i]]$catchatage),
                   year = tt_stochastic[[i]]$year)
  tmp$caa<-tmp$caa/max(tmp$caa)
  dd <-rbind(dd,tmp)
  
}


dd<-rbind(dd,caa)

data_matrix <- reshape2::dcast(dd, age ~ year, value.var = "caa")
rownames(data_matrix) <- data_matrix$age
data_matrix$age <- NULL
data_matrix <- as.matrix(data_matrix)

one_step <- reshape2::melt(t(head(tail(t(data_matrix),n=3),n=1)))
names(one_step)<-c('age','year','prop')
one_step$prop<-one_step$prop/sum(one_step$prop)*100


png("report/figures/Fig.4.8.1.3.png", width = 2000, height = 1200, res = 150)

p1 <- as.ggplot(function() {
  corrplot::corrplot(data_matrix, method = "color", 
                     cl.lim = c(0.7, 1), is.corr = FALSE)
})

p2 <- ggplot(one_step, aes(x = age, y = prop)) +
  geom_line() +
  ylab("Proportion in catch (%)") +
  xlab("Age") +
  theme_bw() +
  facet_wrap(~year) +
  scale_x_continuous(breaks = seq(min(one_step$age), max(one_step$age), by = 1))

(p1 | p2)
dev.off()






#------------------------------------------------------------------------------#
#Diagnostic plots
#------------------------------------------------------------------------------#
load('model/forecast.Rda')


#Function to get N
getN <- function(x){
  idx <- fit$conf$keyLogFsta[1,]+1
  nsize <- length(idx)
  ret <- exp(x[,1:nsize])
  ret
}



tmp <- data.frame(
  age = 2:12,
  value = as.numeric(head(tail(ntable(fit2),2),1)),
  case = paste0(max(fit2$data$years),' assessment'),
  year = max(fit2$data$years)-1
)

tmp <- rbind(tmp,data.frame(
  age = 2:12,
  value = as.numeric(apply(getN(forecast_last[[1]]$sim),2,median)),
  case =  paste0(forecast_last[[1]]$year,' forecast'),
  year = forecast_last[[1]]$year
))

tmp <- rbind(tmp,data.frame(
  age = 2:12,
  value = as.numeric(apply(getN(forecast_last[[2]]$sim), 2, median)),
  case =  paste0(forecast_last[[1]]$year,' forecast'),
  year = forecast_last[[2]]$year
))
tmp <- rbind(tmp,data.frame(
  age = 2:12,
  value = as.numeric(tail(ntable(fit2),1)),
  # value = as.numeric(apply(getN(tt_stochastic[[1]]$sim), 2, median)),
  case = paste0(tt_stochastic[[1]]$year,' assessment'),
  year = tt_stochastic[[1]]$year
))
# tmp <- rbind(tmp,data.frame(
#   age = 2:12,
#   value = as.numeric(apply(getN(tt_stochastic[[2]]$sim), 2, median)),
#   case = paste0(tt_stochastic[[1]]$year,' forecast'),
#   year = tt_stochastic[[2]]$year
# ))




ggplot(tmp, aes(x = age, y = value, colour = case)) +
  geom_line() +
  ylab('Stock number') +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  facet_wrap(~year) +
  ggtitle('Stock numbers') +
  scale_x_continuous(breaks = seq(3, max(tmp$age), by = 2))



ggsave(filename = 'report/figures/Fig.4.8.3.1.png', height=10,width=20, units="cm",
       dpi = 300,  bg = "transparent")





tmp<-data.frame(
  age=2:12,
  value = as.numeric(head(tail(faytable(fit2),2),1)),
  case = paste('Forecaset',max(fit2$data$years),sep=' ')  
)
tmp <- rbind(tmp,data.frame(
  age=2:12,
  value = as.numeric(tail(faytable(fit_old),1)),
  case = paste('Forecaset',max(fit_old$data$years),sep=' ')  
)
)


ggplot(tmp,aes(x=age,y=value,colour=case))+geom_line()+  # Specify the size range of the points
  theme(
    panel.background = element_rect(fill = "white"),  # Set background to white
    panel.grid.major = element_blank(),  # Remove grid
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    text = element_text(size=6)
  )



#------------------------------------------------------------------------------#
#Differences in predicted and observed catch at age
#------------------------------------------------------------------------------#
tmp<- data.frame(age=2:12,
                 caa_predicted = as.numeric(rowSums(forecast_last[[1]]$catchatage))/1000,
                 caa_observed = as.numeric(tail(read.ices(taf.data.path('caa.txt')),1)),
                 year = forecast_last[[1]]$year,
                 case='Predicted')


ggplot(tmp,aes(x=age,y=caa_predicted))+geom_line(aes(colour='Predicted'))+
  geom_line(aes(y=caa_observed,colour='Observed'))+ylab('Catch at age')+theme_bw()+
  annotate("text", x = 3, y = 700, label = paste0('Expected: ', 
                                                  round(unique(forecast_last[[1]]$catch)), ' kton'), 
           color = "black")+
  annotate("text", x = 3, y = 650, label = paste0('Observed: ', 
                                                  round(sum(tail(read.ices(taf.data.path('caa.txt'))*read.ices(taf.data.path('cw.txt')),1))), ' kton'), 
           color = "black")+
  ggtitle('Predicted and observed catch at age')


ggsave(filename = 'report/figures/Fig.4.8.3.2.png', height=15,width=20, units="cm",
       dpi = 300,  bg = "transparent")




#------------------------------------------------------------------------------#
#
#------------------------------------------------------------------------------#
sw <- reshape2::melt(colMeans(tail(fit_old$data$stockMeanWeight,3)))
names(sw)<-'sw_old'
sw$age <- rownames(sw)
sw$sw_updated <- as.numeric(tail(fit$data$stockMeanWeight,1))
sw$cw_old <- as.numeric(colMeans(tail(fit_old$data$catchMeanWeight[,,1],3)))
sw$cw_updated <- as.numeric(tail(fit$data$catchMeanWeight[,,1],1))

pp<-gridExtra::grid.arrange(
ggplot(sw,aes(x=as.numeric(age),y=sw_old))+
  geom_line(aes(colour='Forecast'))+
  geom_line(aes(y=sw_updated,colour='Updated'))+
  theme_bw()+xlab('age')+ylab('stock mean weight')+
  scale_x_continuous(breaks = seq(2, max(tmp$age), by = 2))
,

ggplot(sw,aes(x=as.numeric(age),y=cw_old))+
  geom_line(aes(colour='Forecast'))+
  geom_line(aes(y=cw_updated,colour='Updated'))+
  theme_bw()+xlab('age')+ylab('catch mean weight')+
  scale_x_continuous(breaks = seq(2, max(tmp$age), by = 2))
)



ggsave(filename = 'report/figures/Fig.4.8.3.3.png',plot=pp, height=20,width=15, units="cm",
       dpi = 300,  bg = "transparent")




print(out_table)


#------------------------------------------------------------------------------#
#ICES rounding rules
#------------------------------------------------------------------------------#
new_output <- data.frame(
  Rationale = out_table$Rationale,
  catches = round(out_table$Catches),
  F = out_table$Finn,
  SSB = round(out_table$ssb_pred*1000),
  SSB_change = out_table$ssbchange,
  catch_change = out_table$Catchchange,
  advice_change = out_table$TACchange,
  probability = out_table$risc*100
)

new_output$F<-icesAdvice::icesRound(new_output$F)
new_output$SSB_change<-icesAdvice::icesRound(new_output$SSB_change)
new_output$catch_change[new_output$catch_change>=100]<-round(new_output$catch_change[new_output$catch_change>=100])
new_output$catch_change[new_output$catch_change<100]<-
  icesAdvice::icesRound(new_output$catch_change[new_output$catch_change<100])


new_output$advice_change[new_output$advice_change>=100]<-round(new_output$advice_change[new_output$advice_change>=100])
new_output$advice_change[new_output$advice_change<100]<-
  icesAdvice::icesRound(new_output$advice_change[new_output$advice_change<100])


new_output$probability<-icesAdvice::icesRound(new_output$probability)



names(new_output)<-c('Rationale',
                     paste0('Total Catch (',max(fit$data$years)+1,')'),
                     paste0('F(',max(fit$data$years)+1,')'),
                     paste0('SSB(',max(fit$data$years)+2,')'),
                     '% SSB change',
                     '% catch change',
                     '% advice change',
                     paste('% probability',max(fit$data$years)+2)
)
write.csv2(new_output,file='report/tables/tab4.8.2.1b.txt')




write.csv2(out_table,file='report/tables/tab4.8.2.1b_alternative.txt')
save(out_table,new_output,file='model/forecast_options.Rda')













#placeholder in case for installment failure
# tinytex::install_tinytex()
# tinytex::tlmgr_install(c("multirow", "booktabs", "longtable", "array"))






quarto_render(
  input       = "forecast_presentation.qmd",
  output_file = "forecast_presentation.pdf"
)

file.rename("forecast_presentation.pdf", "output/presentations/forecast_presentation.pdf")











