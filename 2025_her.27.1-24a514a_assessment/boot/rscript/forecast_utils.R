#-------------------------------------------------------------------------------#
#some functions
#-------------------------------------------------------------------------------#
compute_FbarW <- function(fit){
  fit$conf$fbarRange
  
  fromto <- fit$conf$fbarRange-(fit$conf$minAge-1) 
  
  F_a<-faytable(fit)
  N_a<-ntable(fit)
  F_bar <- rowSums(F_a[,min(fromto):max(fromto)]*N_a[,min(fromto):max(fromto)])/rowSums(N_a[,min(fromto):max(fromto)])
  
  F_a<-exp(t(fit$pl$logF-fit$plsd$logF)[,fit$conf$keyLogFsta[1,]+1])
  F_bar_min <- rowSums(F_a[,min(fromto):max(fromto)]*N_a[,min(fromto):max(fromto)])/rowSums(N_a[,min(fromto):max(fromto)])
  
  F_a<-exp(t(fit$pl$logF+fit$plsd$logF)[,fit$conf$keyLogFsta[1,]+1])
  F_bar_max <- rowSums(F_a[,min(fromto):max(fromto)]*N_a[,min(fromto):max(fromto)])/rowSums(N_a[,min(fromto):max(fromto)])
  
  dd<-data.frame(Estimate=F_bar,
                 Low=F_bar_min,
                 High=F_bar_max,
                 year=as.numeric(names(F_bar)))
  dd<-head(dd,-1)
  return(dd)
}

SAMforecast_output <- function(simlist){
  fit_ <- attributes(simlist)$fit
  
  collect <- function(x){
    quan <- quantile(x, c(.50,.025,.975))
    c(median=quan[1], low=quan[2], high=quan[3])
  }
  
  
  #Add last year of F
  fbarW<-compute_FbarW(fit_)[,1:3]
  last <- collect(simlist[[1]]$fbar)
  names(last)<-names(fbarW)
  fbarW<-rbind(fbarW,last)
    
  out <- data.frame(
    'ssb'=exp(fit_$sdrep$value[names(fit_$sdrep$value)=='logssb']),
    ssblow = exp(fit_$sdrep$value[names(fit_$sdrep$value)=='logssb'])-2*(fit_$sdrep$sd[names(fit_$sdrep$value)=='logssb'])*exp(fit_$sdrep$value[names(fit_$sdrep$value)=='logssb']),
    ssbhigh = exp(fit_$sdrep$value[names(fit_$sdrep$value)=='logssb'])+2*(fit_$sdrep$sd[names(fit_$sdrep$value)=='logssb'])*exp(fit_$sdrep$value[names(fit_$sdrep$value)=='logssb']),
    'fbar'= fbarW$Estimate,  
    fbarlow = fbarW$Low,
    fbarhigh = fbarW$High,
    'recruitment'=exp(fit_$sdrep$value[names(fit_$sdrep$value)=='logR']),
    recruitmentlow = exp(fit_$sdrep$value[names(fit_$sdrep$value)=='logR'])-2*(fit_$sdrep$sd[names(fit_$sdrep$value)=='logR'])*exp(fit_$sdrep$value[names(fit_$sdrep$value)=='logR']),
    recruitmenthigh = exp(fit_$sdrep$value[names(fit_$sdrep$value)=='logR'])+2*(fit_$sdrep$sd[names(fit_$sdrep$value)=='logR'])*exp(fit_$sdrep$value[names(fit_$sdrep$value)=='logR']),
    'year' = fit_$data$years
  )
  
  
  
  out<-out[!out$year == max(out$year),]
  for(i in 1:length(simlist)){
    ssb <- collect(simlist[[i]]$ssb)
    fbar <- collect(simlist[[i]]$fbar)
    rec <- collect(simlist[[i]]$rec)
    if(i == 1){
      out_tmp <- data.frame(
        year = max(out$year)+(i),
        # ssb = ssb[1],
        ssb = mean(simlist[[i]]$ssb),
        ssblow = ssb[2],
        ssbhigh = ssb[3],
        fbar = fbar[1],
        fbarlow = fbar[2],
        fbarhigh = fbar[3],
        recruitment = rec[1],
        recruitmentlow = rec[2],
        recruitmenthigh = rec[3]
      )
      
    }else{
      out_tmp <- data.frame(
        year = max(out$year)+1,
        # ssb = ssb[1],
        ssb = mean(simlist[[i]]$ssb),
        ssblow = ssb[2],
        ssbhigh = ssb[3],
        fbar = fbar[1],
        fbarlow = fbar[2],
        fbarhigh = fbar[3],
        recruitment = rec[1],
        recruitmentlow = rec[2],
        recruitmenthigh = rec[3]
      )
    }
    out <- rbind(out,out_tmp)
  }
  return(out)
}

forecast_exploitation_plot <- function(tt,fit){
  
  FF <- faytable(fit)
  FF<-tail(FF,7)
  for(i in 1:nrow(FF)){
    FF[i,]<-FF[i,]/max(FF[i,])
  }
  FF<-reshape2::melt(FF)
  
  
  ff_tmp <- data.frame(Var1 = max(FF$Var1)+1,
                       Var2 = 2:12,
                       value= rowMeans(tt[[2]]$fatage)
  )
  ff_tmp$value = ff_tmp$value/max(ff_tmp$value)
  
  #add predicted F
  FF <- rbind(FF,ff_tmp)
  p<-ggplot(FF,aes(x=Var2,y=value,colour=as.factor(Var1)))+geom_line()+  # Specify the size range of the points
    theme(
      panel.background = element_rect(fill = "white"),  # Set background to white
      panel.grid.major = element_blank(),  # Remove grid
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      text = element_text(size=8)
    )+
    guides(colour = guide_legend(title = 'Year '))+
    xlab('year')+ylab('Selection')
  return(p)
}


#Function for finding similar sized cohorts. 
#This is used for drawing from the historical 
find_similar_cohorts <- function(target_R, data, tol = 0.2) {
  log_target <- log(target_R)
  unique_cohorts <- unique(data[c("yc", "R")])
  diff_logR <- abs(log(unique_cohorts$R) - log_target)
  similar_cohorts <- unique_cohorts$yc[diff_logR <= tol]
  return(similar_cohorts)
}

#Function to grab all relevant data from the fit object
read_all_data_from_fit <- function(fit){
  sw <- reshape2::melt(fit$data$stockMeanWeight)
  names(sw)<-c('year','age','sw')
  cw <- reshape2::melt(fit$data$catchMeanWeight[,,1])
  names(cw)<-c('year','age','cw')
  propMat<-reshape2::melt(fit$data$propMat)
  names(propMat)<-c('year','age','propMat')
  R <- as.data.frame(rectable(fit))
  R$year <- as.numeric(rownames(R))
  rownames(R)<-NULL
  R$age<-2
  R$yc <- R$year-R$age
  R$age <- NULL
  R$year <- NULL
  weight_table <- plyr::join(plyr::join(sw,cw),propMat)
  weight_table$yc <- weight_table$year-weight_table$age
  weight_table <- plyr::join(weight_table,R)
  weight_table <- weight_table[!is.na(weight_table$Estimate),]
  weight_table$logN <- log(weight_table$Estimate)
  return(weight_table)
}



get_mopred <-function(fit){
  #------------------------------------------------------------------------------#
  #Grab all needed data from the fit object
  #------------------------------------------------------------------------------#
  weight_data<-read_all_data_from_fit(fit)
  
  
  #------------------------------------------------------------------------------#
  #Start on the density dependent maturity model
  #------------------------------------------------------------------------------#
  weight_data$propMat[weight_data$propMat==0]<-0.0001
  weight_data$propMat[weight_data$propMat==1]<-0.9999
  weight_data$Low<-NULL
  weight_data$High<-NULL
  
  forecast_mo <- expand.grid(year = ( max(fit$data$years)+1):(max(fit$data$years)+sim_years),
                             age = min(fit$data$minAgePerFleet):max(fit$data$maxAgePerFleet))
  forecast_mo$yc <- forecast_mo$year - forecast_mo$age
  
  forecast_mo<-plyr::join(forecast_mo,unique(weight_data[weight_data$yc%in%forecast_mo$yc,c('yc','Estimate','logN')]))
  
  recpars <- partable(fit)[row.names(partable(fit)) %in% c('rec_pars_0','logSdLogN_0'),1]
  forecast_mo[is.na(forecast_mo$logN),]$logN <- as.numeric(recpars[2])
  forecast_mo[is.na(forecast_mo$Estimate),]$Estimate <- exp(as.numeric(recpars[2]))
  forecast_mo$sw <- NA
  forecast_mo$cw <- NA
  forecast_mo$propMat <- NA
  
  # forecast_data<-weight_data
  forecast_data <- rbind(weight_data,forecast_mo)
  forecast_data[forecast_data$year%in%tail(unique(weight_data$year),5),]$propMat = NA
  
  
  # Fit model to your data
  fit_maturity <- fit_density_dependent_model(forecast_data[!is.na(forecast_data$propMat),], 
                                              age_col = "age", density_col = "Estimate", response_col = "propMat")
  
  # fit_maturity <- fit_density_dependent_model(weight_data, age_col = "age", density_col = "Estimate", response_col = "propMat")
  # Extract parameters
  params <- coef(fit_maturity)
  # Predict maturity on the same data
  forecast_data$mo_pred <- predict_maturity(forecast_data, params, age_col = "age", density_col = "Estimate")
  
  
  mo_pred<-make_predicted_maturity_table(data=forecast_data)
  
  return(list(mo_pred=mo_pred,forecast_data = forecast_data))
}

