
#------------------------------------------------------------------------------#
#A hack as there is something strange with the dimension in the fit object
#------------------------------------------------------------------------------#
if(length(dim(fit2$data$catchMeanWeight))==3){fit2$data$catchMeanWeight<-fit2$data$catchMeanWeight[,,]}
fit2$data$landFrac<-fit2$data$landFrac[,,]
fit2$data$landMeanWeight<-fit2$data$landMeanWeight[,,]
fit2$data$propF<-fit2$data$propF[,,]


#FbarW and catch

fBarW =  c(NA,rep(fmp,sim_years))
catchval.exact = c(nation_quota,rep(NA,sim_years))

#------------------------------------------------------------------------------#
#Compute the TAC for next year. 
#The structure is written to mimic the XSAM structure of forecast
#------------------------------------------------------------------------------#
set.seed(seed_value)
tt<-forecast_XSAM(fit2,fBarW =  fBarW,
                  catchval.exact = catchval.exact,
                  ave.years = ave.years,
                  rec.years = rec.years,
                  deterministic = F,
                  bias_correct = F,
                  rec_from_par = rec_from_par,
                  switch_year_range = switch_year_range)


for(i in 1:sim_years){
  print(i)
  
  #update the projection, using the MP if below Btrigger
  fBarW[i+1]<-MP$F[MP$ssb==round(median(tt[i+1][[1]]$ssb))]

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



#------------------------------------------------------------------------------#
#Do the stochastic forecast
#------------------------------------------------------------------------------#
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

dd <- as.data.frame(print(tt_stochastic))
names(dd)<-c('fbar','fbarlow','fbarhigh','recruitment','recruitmentlow','recruitmenthigh',
             'ssb','ssblow','ssbhigh','catch','catchlow','catchhigh')
dd$year <- as.numeric(rownames(dd))
sam_forecast <- dd#SAMforecast_output(tt_stochastic)
# 
# sam_forecast<-merge(sam_forecast,
#                     data.frame('year'=tail(sam_forecast$year,n=4),'CatchSAM'=
#                                  c(median(tt_stochastic[[1]]$catch),
#                                    median(tt_stochastic[[2]]$catch),
#                                    median(tt_stochastic[[3]]$catch),
#                                    median(tt_stochastic[[4]]$catch)))
#                     )
sam_forecast$rationale <- 'MP'
sam_forecast$assessment_year <- max(fit2$data$years)



#Forecast option
#FMSY

#F=0

#FPa

#Flim

#SSB_next = Blim

#SSB_nest = Bpa

#F = F_thisyear
