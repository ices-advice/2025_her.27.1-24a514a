
#------------------------------------------------------------------------------#
# For the current assessment of NSSH we use the external CV information to compute
# the weight of the different data sources. 
# Estimates observation-level weights by modeling the mean-variance relationship 
#via Taylor's law, applying a log-space transformation to stabilize variance, 
#and computing inverse-variance weights for downstream analysis.
#------------------------------------------------------------------------------#
computeExternalWeight <- function(mu_data,cv_data,k=1){
  
  #The taylor function
  taylorvar<-function(alfa,beta,n,k,mu){
    k^(2-beta)*(alfa/n)*(mu)^beta
  }
  
  #Get the statistic assuming linear relationship between variance and mu on log scale
  f_stat <- lm(log(as.vector(mu_data*cv_data)**2)~log(as.vector(mu_data)))
  
  #Metric for smoothed variance
  v_smoothed<-taylorvar(alfa=exp(f_stat$coef[1]),beta=f_stat$coef[2],n=1,k=1,mu=mu_data)
  
  #Computation of the smoothed weight
  logv<-log(v_smoothed/mu_data^2+1)
  #quantile representing variablity in
  sd<-sqrt(logv)
  sd[is.na(sd)]<-1
  weight <- 1/(sd**2)
  
  #Computation of the data as raw data
  logv<-log((mu_data*cv_data)**2/mu_data^2+1)
  sd<-sqrt(logv)
  sd[is.na(sd)]<-1
  weight_raw <- 1/(sd**2)
  
  #return the object
  return(list(mu = mu_data, 
              v_smoothed=v_smoothed,
              variance = (mu_data*cv_data)**2,
              weight = weight,weight_raw=weight_raw))
}






#------------------------------------------------------------------------------#
#plot the parameter correlation. but there is some odd stuff in this version and 
#should not be used untill fixed
#------------------------------------------------------------------------------#
plot.parameter.cor <- function(fit){
  #get correlation matrix of fixed parameters
  ttt<- cov2cor(fit$sdrep$cov.fixed)
  #Get duplicated parameter names
  unique_names <-unique(rownames(ttt))[table(rownames(ttt))>1]
  #add index on parameter name to separate these
  counter <- rep(0, length(unique_names))
  for(i in 1:length(rownames(ttt))) {
    index <- which(unique_names == rownames(ttt)[i])
    counter[index] <- counter[index] + 1
    if(rownames(ttt)[i]%in%unique_names){
      rownames(ttt)[i] <- paste0(rownames(ttt)[i], "_", counter[index]-1)}
  }
  colnames(ttt)<-rownames(ttt)
  
  # Create ggplot with color based on positive and negative values
  p<-ggplot(reshape2::melt(ttt), aes(x = Var1, y = Var2, size = abs(value), color = factor(sign(value)))) +
    geom_point() +
    scale_color_manual(values = c("blue", "red"), guide = FALSE) +
    labs(
      x = "",
      y = ""
    )+
    guides(size = FALSE) +  # Remove size legend
    scale_size(range = c(0.00, 3)) +  # Specify the size range of the points
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotate x-axis ticks
      panel.background = element_rect(fill = "white"),  # Set background to white
      panel.grid.major = element_blank(),  # Remove grid
      panel.grid.minor = element_blank(),
      panel.border = element_blank()
    )
  return(p)
}


plot.data.weight<-
  function(log_obs){
    p<-ggplot(log_obs,aes(x=as.numeric(year),
                          y=as.numeric(age),
                          size=sqrt(weight),colour=sqrt(weight)))+
      geom_point()+ 
      scale_colour_gradient(low = "black", high = "red",guide='none')+
      theme(axis.text.x = element_text(angle = 90))+
      ylab('Age')+xlab('Year')+
      scale_alpha(guide = 'none')+
      scale_radius(limits=c(0,sqrt(1000)),name='relative weight',guide='none',range = c(1,5))+
      scale_y_continuous(breaks=seq(2,12,1))+
      scale_x_continuous(breaks=seq(1990,2050,5))+
      theme(text = element_text(size=6))+
      theme(
        panel.background = element_rect(fill = "white"),  # Set background to white
        # panel.grid.major = element_blank(),  # Remove grid
        # panel.grid.minor = element_blank(),
        panel.border = element_blank()
      )
    return(p)
  }


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# plot.survey.on.fit <- function(fit){
#   
#   tmp <- as.data.frame(fit$data$aux)
#   tmp$logobs <- fit$data$logobs
#   mo<- reshape2::melt(fit$data$propMat)
#   names(mo)<-c('year','age','mo')
#   tmp<-plyr::join(tmp,mo)
#   mo<- reshape2::melt(fit$data$stockMeanWeight)
#   names(mo)<-c('year','age','sw')
#   tmp<-plyr::join(tmp,mo)
#   tt<-fit$conf$keyLogFpar
#   colnames(tt)<-2:12
#   tt<-reshape2::melt(tt)
#   names(tt)<-c('fleet','age','logq_key')
#   tmp<-plyr::join(tmp,tt)
#   tmp<-tmp[tmp$logq_key>-1,]
#   tmp$logq<-fit$pl$logFpar[tmp$logq_key+1]
#   tmp$logq_sd<-fit$plsd$logFpar[tmp$logq_key+1]
#   
#   tmp$N <- exp(tmp$logobs)/exp(tmp$logq)
#   tmp$N_min <- exp(tmp$logobs)/exp(tmp$logq-tmp$logq_sd)
#   tmp$N_max <- exp(tmp$logobs)/exp(tmp$logq+tmp$logq_sd)
#   tmp$b <- tmp$N*tmp$mo*tmp$sw
#   tmp$b_min <- tmp$N_min*tmp$mo*tmp$sw
#   tmp$b_max <- tmp$N_max*tmp$mo*tmp$sw
#   
#   tmp<-tmp[tmp$fleet!=3,]
#   # tmp[(tmp$fleet==2 ),]
#   
#   
#   sum_b_by_year_fleet <- as.data.frame(tmp %>%
#                                          dplyr::group_by(year, fleet) %>%
#                                          dplyr::summarize(sum_b = sum(b, na.rm = TRUE),
#                                                           sum_b_min = sum(b_min, na.rm = TRUE),
#                                                           sum_b_max = sum(b_max, na.rm = TRUE)
#                                          ))
#   
#   sum_b_by_year_fleet[(sum_b_by_year_fleet$fleet==2 & sum_b_by_year_fleet$sum_b_max==0),]$sum_b<-NA
#   sum_b_by_year_fleet[(sum_b_by_year_fleet$fleet==2 & sum_b_by_year_fleet$sum_b_max==0),]$sum_b_min<-NA
#   sum_b_by_year_fleet[(sum_b_by_year_fleet$fleet==2 & sum_b_by_year_fleet$sum_b_max==0),]$sum_b_max<-NA
#   
#   
#   ssb_fit <- as.data.frame(ssbtable(fit))
#   ssb_fit$year <- rownames(ssb_fit)
#   # sum_b_by_year_fleet[sum_b_by_year_fleet$sum_b==0,]$sum_b <- NA
#   # sum_b_by_year_fleet[sum_b_by_year_fleet$sum_b_min==0,]$sum_b_min <- NA
#   # sum_b_by_year_fleet[sum_b_by_year_fleet$sum_b_max==0,]$sum_b_max <- NA
#   
#   p<-ggplot(ssb_fit,aes(x=as.numeric(year),y=Estimate))+geom_line()+
#     geom_ribbon(aes(x=as.numeric(year),ymin=Low,ymax=High),alpha=0.8,fill='darkgrey')+
#     geom_ribbon(data=sum_b_by_year_fleet,
#                 aes(x=as.numeric(year),y=sum_b,ymin=sum_b_min,ymax=sum_b_max,
#                     fill=as.factor(fleet),group=fleet),alpha=0.8)+
#     guides(size = FALSE,fill=FALSE,colour=FALSE)+
#     geom_line(data=sum_b_by_year_fleet,aes(x=year,y=sum_b,colour=as.factor(fleet),group=fleet))+
#     ylab('SSB')+xlab('year') +  # Specify the size range of the points
#     theme(
#       panel.background = element_rect(fill = "white"),  # Set background to white
#       panel.grid.major = element_blank(),  # Remove grid
#       panel.grid.minor = element_blank(),
#       panel.border = element_blank()
#     )
#   p
#   return(p)
#   
# }
# 
# plot.data.weight<-function(log_obs){
#   p<-ggplot(log_obs,aes(x=as.numeric(year),
#                         y=as.numeric(age),
#                         size=sqrt(weight),colour=sqrt(weight)))+
#     geom_point()+ 
#     scale_colour_gradient(low = "black", high = "red",guide='none')+
#     theme(axis.text.x = element_text(angle = 90))+
#     ylab('Age')+xlab('Year')+
#     scale_alpha(guide = 'none')+
#     scale_radius(limits=c(0,sqrt(1000)),name='relative weight',guide='none',range = c(1,10))+
#     scale_y_continuous(breaks=seq(2,12,1))+
#     scale_x_continuous(breaks=seq(1990,2050,5))+
#     theme(text = element_text(size=8))+
#     theme(
#       panel.background = element_rect(fill = "white"),  # Set background to white
#       # panel.grid.major = element_blank(),  # Remove grid
#       # panel.grid.minor = element_blank(),
#       panel.border = element_blank()
#     )
#   return(p)
# }


#------------------------------------------------------------------------------#
#Function to grab the Leave out data as a table
#------------------------------------------------------------------------------#
LO_table <- function(LO){
  base <- attributes(LO)$fit
  
  ssb <- as.data.frame(ssbtable(base))
  ssb$year <- rownames(ssb)
  ssb$case <- 'base'
  ssb$which <- 'SSB'
  
  fbar <- compute_FbarW(base)
  fbar$case <- 'base'
  fbar$which <- 'FbarW'
  
  rec <- as.data.frame(rectable(base))
  rec$year <- rownames(rec)
  rec$case <- 'base'
  rec$which <- 'Rec'
  
  out <- rbind(ssb,fbar,rec)
  for(lo in names(LO)){
    tmp <- LO[names(LO) ==  lo][[1]]
    
    ssb <- as.data.frame(ssbtable(tmp))
    ssb$year <- rownames(ssb)
    ssb$case <- lo
    ssb$which <- 'SSB'
    
    fbar <- compute_FbarW(tmp)
    fbar$case <- lo
    fbar$which <- 'FbarW'
    
    rec <- as.data.frame(rectable(tmp))
    rec$year <- rownames(rec)
    rec$case <- lo
    rec$which <- 'Rec'
    
    out <- rbind(out,ssb,fbar,rec)
  }
  out$year <- as.numeric(out$year)
  return(out)
}



#------------------------------------------------------------------------------#
#Function to compute the Fbar weighted with abundance
#------------------------------------------------------------------------------#
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



#------------------------------------------------------------------------------#
#Plot leave out that also includes FbarW
#------------------------------------------------------------------------------#
plot.LO <- function(rett,what = 'SSB',yrange=NULL,main=''){
  lo <- names(rett)
  data_out<-c()
  for(i in 1:length(rett)){
    tt<-as.data.frame(ssbtable(rett[[i]]))
    tt$year <- as.numeric(rownames(tt))
    tt$case <- lo[i]
    tt$type <- 'SSB'
    data_out <- rbind(data_out,tt)
    tt<-as.data.frame(rectable(rett[[i]]))
    tt$year <- as.numeric(rownames(tt))
    tt$case <- lo[i]
    tt$type <- 'R'
    data_out <- rbind(data_out,tt)
    tt<-compute_FbarW(rett[[i]])
    tt$case <- lo[i]
    tt$type <- 'FbarW'
    data_out <- rbind(data_out,tt)
  }
  
  
  data_out<-data_out[data_out$type %in% what,]
  
  p<-ggplot()+
    geom_ribbon(data=data_out[data_out$case=='base',],aes(x=year,ymin=Low,ymax=High),alpha=0.3)+
    geom_line(data=data_out[data_out$case=='base',],aes(x=year,y=Estimate),colour='black')+
    geom_line(data=data_out[data_out$case!='base',],aes(x=year,y=Estimate,
                                                        colour=as.factor(case),group=case))+
    theme(strip.background = element_blank(),
          strip.placement = "outside")+
    theme(panel.grid = element_blank(),
          legend.position = "bottom",
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white")
    )+
    scale_colour_manual(
      values = c("red", "blue", "green", "orange", "purple",'brown')  # replace or add colors as needed
    )+
    guides(fill = guide_legend(title = 'case: '),
           colour = guide_legend(title = 'case: '))
  
  if(!is.null(yrange)){p <- p+ylim(yrange)}
  p <- p+ggtitle(main)
  
  return(p)  
}




#------------------------------------------------------------------------------#
#Function to plot the retro, this also includes the mohns rho in the figure
#------------------------------------------------------------------------------#
plot.retro <- function(rett,what = 'SSB'){
  tt<-attributes(rett)$fit
  my<-max(tt$data$years)
  data_out<-as.data.frame(ssbtable(attributes(rett)$fit))
  data_out$year<-as.numeric(rownames(data_out))
  data_out$peel <- my
  data_out$type <- 'SSB'
  tt<- as.data.frame(rectable(attributes(rett)$fit))
  tt$year <- as.numeric(rownames(tt))
  tt$peel <- my
  tt$type <- 'R'
  data_out <- rbind(data_out,tt)
  tt<- compute_FbarW(attributes(rett)$fit)
  tt$peel <- my
  tt$type <- 'FbarW'
  data_out <- rbind(data_out,tt)
  for(i in 1:length(rett)){
    tt<-as.data.frame(ssbtable(rett[[i]]))
    tt$year <- as.numeric(rownames(tt))
    tt$peel <- my-i
    tt$type <- 'SSB'
    data_out <- rbind(data_out,tt)
    tt<-as.data.frame(rectable(rett[[i]]))
    tt$year <- as.numeric(rownames(tt))
    tt$peel <- my-i
    tt$type <- 'R'
    data_out <- rbind(data_out,tt)
    tt<-compute_FbarW(rett[[i]])
    tt$peel <- my-i
    tt$type <- 'FbarW'
    data_out <- rbind(data_out,tt)
  }
  data_out$mohn<-NA
  
  mmm<- mohn(rett)
  data_out$mohn[data_out$type=='R']<-paste("R: Mohn's rho: ",round(mmm[1],digits = 3))
  data_out$mohn[data_out$type=='SSB']<-paste("SSB: Mohn's rho: ",round(mmm[2],digits = 3))
  data_out$mohn[data_out$type=='FbarW']<-paste("FbarW: Mohn's rho: ",round(mmm[3],digits = 3))
  unique(data_out$mohn)
  data_out<-data_out[data_out$type %in% what,]
  
  p<-ggplot()+
    geom_ribbon(data=data_out[data_out$peel==my,],aes(x=year,ymin=Low,ymax=High),alpha=0.3)+
    geom_line(data=data_out[data_out$peel==my,],aes(x=year,y=Estimate),colour='black')+
    geom_line(data=data_out[data_out$peel<my,],aes(x=year,y=Estimate,colour=as.factor(peel),group=peel))+
    facet_wrap(~mohn,scales = 'free_y')+#, strip.position = "left",labeller = as_labeller(c(FbarW = '',
    # R = 'Recruitment in millions',
    # SSB = "SSB in thousands tonnes")))+
    
    scale_colour_manual(
      values = c("red", "blue", "green", "orange", "purple",'brown')  # replace or add colors as needed
    )+
    theme(strip.background = element_blank(),
          strip.placement = "outside")+
    theme(panel.grid = element_blank(),
          legend.position = "bottom",
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white")
    )+
    guides(fill = guide_legend(title = 'Peel: '),
           colour = guide_legend(title = 'Peel: '))
  
  p  
  return(p)
}


#------------------------------------------------------------------------------#
#A comparison between the parameters
#TODO: this is rigged for the XSAM model, so need to be changed for the new 
#assessment
#------------------------------------------------------------------------------#
parameter_table <- function(fit,fit_old){
  
  print('Warning: this is rigged for the old assessment and need to be updated')
  year_new<-max(fit$data$years)
  year_old<-max(fit_old$data$years)
  
  
  
  #Grab parameters
  sampar<-summary(fit$sdrep)
  sampar_old<-summary(fit_old$sdrep)
  
  
  #Initial parameters
  SAM <- c()
  t<-fit$pl$logN
  tt<-fit$plsd$logN
  sam <- c()
  sam$Parameter <- c('initlogN_age02','initlogN_age03','initlogN_age04','initlogN_age05',
                     'initlogN_age06','initlogN_age07','initlogN_age08','initlogN_age09','initlogN_age10','initlogN_age11')
  sam$Value<-as.numeric(t[2:11,1])
  sam$Std<-as.numeric(tt[2:11,1])
  sam$grp <- 'log(N_1988)'
  sam$year <- year_new
  SAM <- rbind(SAM,as.data.frame(sam))
  
  t<-fit_old$pl$logN
  tt<-fit_old$plsd$logN
  sam <- c()
  sam$Parameter <- c('initlogN_age02','initlogN_age03','initlogN_age04','initlogN_age05','initlogN_age06','initlogN_age07','initlogN_age08','initlogN_age09','initlogN_age10','initlogN_age11')
  sam$Value<-as.numeric(t[2:11,1])
  sam$Std<-as.numeric(tt[2:11,1])
  sam$grp <- 'log(N_1988)'
  sam$year <- year_old
  SAM <- rbind(SAM,as.data.frame(sam))
  
  
  #____________________________________________________________________________________#
  t<-sampar[1:16,]
  sam <- c()
  sam$Parameter <- c('logqpar_Fleet1_03','logqpar_Fleet1_04',
                     'logqpar_Fleet1_05','logqpar_Fleet1_06','logqpar_Fleet1_07','logqpar_Fleet1_08-12','logqpar_Fleet4_02','logqpar_Fleet5_03','logqpar_Fleet5_04','logqpar_Fleet5_05','logqpar_Fleet5_06','logqpar_Fleet5_07','logqpar_Fleet5_08','logqpar_Fleet5_09','logqpar_Fleet5_10','logqpar_Fleet5_11-12')
  sam$Value<-as.numeric(t[,1])
  sam$Std<-as.numeric(t[,2])
  sam$grp <- 'log(q) (catchability)'
  sam$year <- year_new
  SAM <- rbind(SAM,as.data.frame(sam))
  
  
  t<-sampar_old[1:16,]
  sam <- c()
  sam$Parameter <- c('logqpar_Fleet1_03','logqpar_Fleet1_04',
                     'logqpar_Fleet1_05','logqpar_Fleet1_06','logqpar_Fleet1_07','logqpar_Fleet1_08-12','logqpar_Fleet4_02','logqpar_Fleet5_03','logqpar_Fleet5_04','logqpar_Fleet5_05','logqpar_Fleet5_06','logqpar_Fleet5_07','logqpar_Fleet5_08','logqpar_Fleet5_09','logqpar_Fleet5_10','logqpar_Fleet5_11-12')
  sam$Value<-as.numeric(t[,1])
  sam$Std<-as.numeric(t[,2])
  sam$grp <- 'log(q) (catchability)'
  sam$year <- year_old
  SAM <- rbind(SAM,as.data.frame(sam))
  
  
  #____________________________________________________________________________________#
  sam <- c()
  sam$Parameter <- 'logs2_1'
  sam$Value<-fit$pl$logSdLogN[2]
  sam$Std<-fit$plsd$logSdLogN[2]
  sam$grp <- 'log(sigma^2) in F-process and recruitment'
  sam$year <- year_new
  SAM <- rbind(SAM,as.data.frame(sam))
  
  sam <- c()
  sam$Parameter <- 'logs2_1'
  sam$Value<-fit_old$pl$logSdLogN[2]
  sam$Std<-fit_old$plsd$logSdLogN[2]
  sam$grp <- 'log(sigma^2) in F-process and recruitment'
  sam$year <- year_old
  SAM <- rbind(SAM,as.data.frame(sam))
  
  
  
  #____________________________________________________________________________________#
  t <- log(exp(fit$pl$sepFlogSd)**2)
  tt <- log(exp(fit$plsd$sepFlogSd)**2)
  sam <- c()
  sam$Parameter <- 'logs2_2'
  sam$Value<-t[1]
  sam$Std<-tt[1]
  sam$grp <- 'log(sigma^2) in F-process and recruitment'
  sam$year <- year_new
  SAM <- rbind(SAM,as.data.frame(sam))
  
  sam <- c()
  sam$Parameter <- 'logs2_4'
  sam$Value<-t[2]
  sam$Std<-tt[2]
  sam$grp <- 'log(sigma^2) in F-process and recruitment'
  sam$year <- year_new
  SAM <- rbind(SAM,as.data.frame(sam))
  
  t <- log(exp(fit_old$pl$sepFlogSd)**2)
  tt <- log(exp(fit_old$plsd$sepFlogSd)**2)
  sam <- c()
  sam$Parameter <- 'logs2_2'
  sam$Value<-t[1]
  sam$Std<-tt[1]
  sam$grp <- 'log(sigma^2) in F-process and recruitment'
  sam$year <- year_old
  SAM <- rbind(SAM,as.data.frame(sam))
  
  sam <- c()
  sam$Parameter <- 'logs2_4'
  sam$Value<-t[2]
  sam$Std<-tt[2]
  sam$grp <- 'log(sigma^2) in F-process and recruitment'
  sam$year <- year_old
  SAM <- rbind(SAM,as.data.frame(sam))
  
  
  
  #____________________________________________________________________________________#
  sam <- c()
  sam$Parameter <- 'logsR2'
  sam$Value<-log(exp(fit$pl$logSdLogN[1])**2)
  sam$Std<-log(exp(fit$plsd$logSdLogN[1])**2)
  sam$grp <- 'log(sigma^2) in F-process and recruitment'
  sam$year <- year_new
  SAM <- rbind(SAM,as.data.frame(sam))
  
  sam <- c()
  sam$Parameter <- 'logsR2'
  sam$Value<-log(exp(fit_old$pl$logSdLogN[1])**2)
  sam$Std<-log(exp(fit_old$plsd$logSdLogN[1])**2)
  sam$grp <- 'log(sigma^2) in F-process and recruitment'
  sam$year <- year_old
  SAM <- rbind(SAM,as.data.frame(sam))
  
  
  #____________________________________________________________________________________#
  t<-sampar[18,]
  sam <- c()
  sam$Parameter <- 'loghvec'
  sam$Value<-log(exp(as.numeric(t[1]))**2)
  sam$Std<-log(exp(as.numeric(t[2]))**2)
  sam$grp <- 'Remainding parameters'
  sam$year <- year_new
  SAM <- rbind(SAM,as.data.frame(sam))
  
  t<-sampar_old[18,]
  sam <- c()
  sam$Parameter <- 'loghvec'
  sam$Value<-log(exp(as.numeric(t[1]))**2)
  sam$Std<-log(exp(as.numeric(t[2]))**2)
  sam$grp <- 'Remainding parameters'
  sam$year <- year_old
  SAM <- rbind(SAM,as.data.frame(sam))
  
  
  
  #____________________________________________________________________________________#
  t<-sampar[19,]
  sam <- c()
  sam$Parameter <- 'rec_loga'
  sam$Value<-as.numeric(t[1])
  sam$Std<-as.numeric(t[2])
  sam$grp <- 'Remainding parameters'
  sam$year <- year_new
  SAM <- rbind(SAM,as.data.frame(sam))
  
  t<-sampar_old[19,]
  sam <- c()
  sam$Parameter <- 'rec_loga'
  sam$Value<-as.numeric(t[1])
  sam$Std<-as.numeric(t[2])
  sam$grp <- 'Remainding parameters'
  sam$year <- year_old
  SAM <- rbind(SAM,as.data.frame(sam))
  
  
  #____________________________________________________________________________________#
  sam <- c()
  sam$Parameter <- 'aYpar'
  sam$Value<-as.numeric(-fit$pl$sepFalpha[10])
  sam$Std<-as.numeric(fit$plsd$sepFalpha[10])
  sam$grp <- 'Seperable F parameter'
  sam$year <- year_new
  SAM <- rbind(SAM,as.data.frame(sam))
  
  
  sam <- c()
  sam$Parameter <- 'aYpar'
  sam$Value<-as.numeric(-fit_old$pl$sepFalpha[10])
  sam$Std<-as.numeric(fit_old$plsd$sepFalpha[10])
  sam$grp <- 'Seperable F parameter'
  sam$year <- year_old
  SAM <- rbind(SAM,as.data.frame(sam))
  
  
  #____________________________________________________________________________________#
  
  sam <- c()
  sam$Parameter <- 'bY'
  sam$Value<-as.numeric(2/(1+exp(-2*fit$pl$sepFlogitRho[2]))-1)
  sam$Std<-as.numeric(2/(1+exp(-2*exp(fit$pl$sepFlogSd[2])**2))-1)
  sam$grp <- 'Seperable F parameter'
  sam$year <- year_new
  SAM <- rbind(SAM,as.data.frame(sam))
  
  sam <- c()
  sam$Parameter <- 'bY'
  sam$Value<-as.numeric(2/(1+exp(-2*fit_old$pl$sepFlogitRho[2]))-1)
  sam$Std<-as.numeric(2/(1+exp(-2*exp(fit_old$pl$sepFlogSd[2])**2))-1)
  sam$grp <- 'Seperable F parameter'
  sam$year <- year_old
  SAM <- rbind(SAM,as.data.frame(sam))
  
  
  #____________________________________________________________________________________#
  t<-sampar[20:28,]
  sam <- c()
  sam$Parameter <- c('aUpar_02','aUpar_03','aUpar_04','aUpar_05','aUpar_06','aUpar_07',
                     'aUpar_08','aUpar_09','aUpar_10')
  sam$Value<-as.numeric(t[,1])
  sam$Std<-as.numeric(t[,2])
  sam$grp <- 'Seperable F parameter'
  sam$year <- year_new
  SAM <- rbind(SAM,as.data.frame(sam))
  
  t<-sampar_old[20:28,]
  sam <- c()
  sam$Parameter <- c('aUpar_02','aUpar_03','aUpar_04','aUpar_05','aUpar_06','aUpar_07',
                     'aUpar_08','aUpar_09','aUpar_10')
  sam$Value<-as.numeric(t[,1])
  sam$Std<-as.numeric(t[,2])
  sam$grp <- 'Seperable F parameter'
  sam$year <- year_old
  SAM <- rbind(SAM,as.data.frame(sam))
  
  
  #____________________________________________________________________________________#
  
  sam <- c()
  sam$Parameter <- 'bU'
  sam$Value<-as.numeric(2/(1+exp(-2*fit$pl$sepFlogitRho[1]))-1)
  sam$Std<-as.numeric(2/(1+exp(-2*exp(fit$pl$sepFlogSd[1])**2))-1)
  sam$grp <- 'Seperable F parameter'
  sam$year <- year_new
  SAM <- rbind(SAM,as.data.frame(sam))
  
  
  sam <- c()
  sam$Parameter <- 'bU'
  sam$Value<-as.numeric(2/(1+exp(-2*fit_old$pl$sepFlogitRho[1]))-1)
  sam$Std<-as.numeric(2/(1+exp(-2*exp(fit_old$pl$sepFlogSd[1])**2))-1)
  sam$grp <- 'Seperable F parameter'
  sam$year <- year_old
  SAM <- rbind(SAM,as.data.frame(sam))
  
  return(SAM)
}





# --------------------------------------------------------------#
#Sam implementation to get the process error
#------------------------------------------------------------------------------#
get.procres.sdrep <- function(fit,...){
  pp <- fit$pl
  attr(pp,"what") <- NULL
  pp$missing <- NULL
  #fit.co <- stockassessment:::refit(fit,startingValues=pp, run=FALSE)
  fit.co <- stockassessment:::sam.fit(fit$data,fit$conf,pp, run=FALSE, map=fit$obj$env$map)
  fit.co$obj$env$data$resFlag <- 1
  fit.co$obj$retape()
  fit$sdrep <- TMB::sdreport(fit.co$obj,fit$opt$par,...)
  
  #Grab the residual and store it into a matrix
  ret <- fit$sdrep$value[names(fit$sdrep$value)%in%'resN']
  mat <- t(matrix(ret, nrow = (fit$conf$maxAge - fit$conf$minAge+1), byrow = F))
  colnames(mat) <- fit$conf$minAge:fit$conf$maxAge
  rownames(mat) <- fit$data$years[-1]
  
  #Scale residual with logSdLogN
  mat <- mat*
    do.call(rbind, replicate(nrow(mat), exp(fit$pl$logSdLogN[fit$conf$keyVarLogN+1]), simplify = FALSE))
  
  return(mat)
}





