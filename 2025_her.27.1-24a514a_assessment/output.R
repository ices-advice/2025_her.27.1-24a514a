#------------------------------------------------------------------------------#
#output.R
#
#Description: 
#Here we produce all the figures, tables, and presentation that is not used in 
#the WGWIDE report
#------------------------------------------------------------------------------#




#------------------------------------------------------------------------------#
#remove everything from memory
#------------------------------------------------------------------------------#
rm(list=ls())




#------------------------------------------------------------------------------#
#load TAF
#------------------------------------------------------------------------------#
library(TAF)
taf.library(ggplot2)
taf.library(stockassessment)
taf.library(reshape2)
taf.library(quarto)
source(taf.boot.path('rscript/utils.r'))




#------------------------------------------------------------------------------#
#Make the directory tree
#------------------------------------------------------------------------------#
mkdir("output")
mkdir("output/tables")
mkdir("output/figures")
mkdir("output/presentations")




#------------------------------------------------------------------------------#
#load data
#------------------------------------------------------------------------------#
#Previous assessment
#TODO: the previous runned assessment may be stored in the boot/data folder
load('boot/rscript/fit_official_benchmark.Rda')
fit_old <- fit          #store the previously made assessment

load('model/diag.Rda')  #read object with diagnostic
load('model/fit.Rda')




#------------------------------------------------------------------------------#
#Process the signal from the data
#First we grab the signal as from the fit object
#------------------------------------------------------------------------------#
out <-as.data.frame(fit$data$aux)
out$fleetName<-attr(fit$data,'fleetNames')[out$fleet]
out$logobs <- fit$data$logobs
out$weight <- fit$data$weight
out$yc <- out$year-out$age





#------------------------------------------------------------------------------#
#grab the input data for the residual catch, and plot it
#------------------------------------------------------------------------------#
cn <- reshape2::melt(read.ices(taf.data.path('caa.txt')) )
names(cn)<-c('year','age','cn')
cn_cv <- reshape2::melt(read.ices(taf.data.path('caa_cv.txt')))
names(cn_cv)<-c('year','age','cv')
cn <- plyr::join(cn,cn_cv)
cn$var <- (cn$cn*cn$cv)**2

out$fleetName[out$fleetName == 'Residual catch']<-'Catch'
p_catch<-gridExtra::grid.arrange(
  ggplot(out[out$fleet==1 & out$age<12,],aes(x=year,y=logobs,group=yc,colour=as.factor(yc)))+
    geom_line()+theme_bw()+
    ggtitle(paste(unique(out[out$fleet==1,]$fleetName),'signals'))+
    theme(legend.position = "none")+ylab('log catch')
  ,
  ggplot(cn,aes(x=log(cn),y=log(var)))+geom_point()+
    geom_point(data=cn[cn$year==(max(cn$year)),],colour='red')+theme_bw()+
    ggtitle(paste(unique(out[out$fleet==1,]$fleetName),'variance'))+
    ylab('log variance')+xlab('log catch')
  # ,
  # ggplot(out[out$fleet==1 & !is.na(out$logobs),],aes(x=year,y=age,size=weight,colour=weight))+
  #   geom_point()+
  #   theme_bw()+
  #   ggtitle(paste(unique(out[out$fleet==1,]$fleetName),'data weight'))+
  #   theme(legend.position = "none")+
  #   scale_colour_gradient(low = "black", high = "red")
  ,
  ncol=2
)
ggsave("output/figures/fig_catchsignal.png", p_catch, width = 12, height = 4, dpi = 300)




#------------------------------------------------------------------------------#
#Now process the IESNS survey, and plot it
#------------------------------------------------------------------------------#
cn <- reshape2::melt(read.ices(taf.data.path('survey_T2025-02-05.txt'))$IESNS )
names(cn)<-c('year','age','cn')
cn_cv <- reshape2::melt(read.ices(taf.data.path('surveycv_T2025-02-05.txt'))$IESNS)
names(cn_cv)<-c('year','age','cv')
cn <- plyr::join(cn,cn_cv)
cn$var <- (cn$cn*cn$cv)**2



p_IESNS<-gridExtra::grid.arrange(
  ggplot(out[out$fleet==2 & out$age<12,],aes(x=year,y=logobs,group=yc,colour=as.factor(yc)))+
    geom_line()+theme_bw()+ggtitle(paste(unique(out[out$fleet==2,]$fleetName),'signals'))+
    theme(legend.position = "none")+ylab('log index')
  ,
  ggplot(cn,aes(x=log(cn),y=log(var)))+geom_point()+
    geom_point(data=cn[cn$year==(max(cn$year)),],colour='red')+theme_bw()+
    ggtitle(paste(unique(out[out$fleet==2,]$fleetName),'variance'))+
    ylab('log variance')+xlab('log index')
  ,
  # ggplot(out[out$fleet==2 & !is.na(out$logobs),],aes(x=year,y=age,size=weight,colour=weight))+geom_point()+
  #   theme_bw()+
  #   ggtitle(paste(unique(out[out$fleet==2,]$fleetName),'data weight'))+
  #   theme(legend.position = "none")+
  #   scale_colour_gradient(low = "black", high = "red")
  # ,
  ncol=2
)

ggsave("output/figures/fig_IESNSsignal.png", p_IESNS, width = 12, height = 4, dpi = 300)




#------------------------------------------------------------------------------#
#prepare the figure for BESS survey
#------------------------------------------------------------------------------#

cn <- reshape2::melt(read.ices(taf.data.path('survey_T2025-02-05.txt'))$recBar )
names(cn)<-c('year','age','cn')
cn_cv <- reshape2::melt(read.ices(taf.data.path('surveycv_T2025-02-05.txt'))$recBar)
names(cn_cv)<-c('year','age','cv')
cn <- plyr::join(cn,cn_cv)
cn$var <- (cn$cn*cn$cv)**2
cn <- cn[cn$age%in%c(1,2),]


out$fleetName[out$fleetName == 'recBar']<-'BESS'
p_BESS<-gridExtra::grid.arrange(
  ggplot(out[out$fleet==3 & out$age<12,],aes(x=year,y=logobs,group=age,colour=as.factor(age)))+
    geom_line()+theme_bw()+ggtitle('BESS signals')+
    theme(legend.position = "none")+ylab('log index')+geom_point()
  ,
  # ggplot(cn[cn$age%in%c(2,3),],aes(x=log(cn),y=log(var)))+geom_point()+
  #   geom_point(data=cn[cn$year==(max(cn$year)),],colour='red')+
  #   ggtitle('BESS variance')+ylab('log variance')+xlab('log index')
  # ,
  # ggplot(out[out$fleet==3 & !is.na(out$logobs),],aes(x=year,y=age,size=weight,colour=weight))+geom_point()+
  #   theme_bw()+ggtitle('BESS data weight')+
  #   theme(legend.position = "none")+
  #   scale_colour_gradient(low = "black", high = "red")
  # ,
  ncol=1
)

ggsave("output/figures/fig_BESSsignal.png", p_BESS, width = 12, height = 4, dpi = 300)



#------------------------------------------------------------------------------#
#prepare the plot for the IESNS recruitment
#------------------------------------------------------------------------------#

cn <- reshape2::melt(read.ices(taf.data.path('survey_T2025-02-05.txt'))$Bar )
names(cn)<-c('year','age','cn')
cn_cv <- reshape2::melt(read.ices(taf.data.path('surveycv_T2025-02-05.txt'))$Bar)
names(cn_cv)<-c('year','age','cv')
cn <- plyr::join(cn,cn_cv)
cn$var <- (cn$cn*cn$cv)**2



p_IESNSrec<-gridExtra::grid.arrange(
  ggplot(out[out$fleet==4 & out$age<12,],aes(x=year,y=logobs,group=age),colour='black')+
    geom_line()+theme_bw()+ggtitle('IESNS,bar signal')+
    theme(legend.position = "none")+ylab('log index')+geom_point()
  ,
  ggplot(cn,aes(x=log(cn),y=log(var)))+geom_point()+
    ggtitle('IESNS,bar variance')+ylab('log variance')+xlab('log index')
  # ,
  # ggplot(out[out$fleet==4 & !is.na(out$logobs),],
  #        aes(x=year,y=age,size=weight,colour=weight))+geom_point()+
  #   theme_bw()+ggtitle('IESNS,bar data weight')+
  #   theme(legend.position = "none")+
  #   scale_colour_gradient(low = "black", high = "red")
  ,
  ncol=2
)

ggsave("output/figures/fig_IESNSrecsignal.png", p_IESNSrec, width = 12, height = 4, dpi = 300)






#------------------------------------------------------------------------------#
#prepare the figure for the NASF old
#------------------------------------------------------------------------------#
cn <- reshape2::melt(read.ices(taf.data.path('survey_T2025-02-05.txt'))$NASF )
names(cn)<-c('year','age','cn')
cn_cv <- reshape2::melt(read.ices(taf.data.path('surveycv_T2025-02-05.txt'))$NASF)
names(cn_cv)<-c('year','age','cv')
cn <- plyr::join(cn,cn_cv)
cn$var <- (cn$cn*cn$cv)**2



p_NASFold <- gridExtra::grid.arrange(
  ggplot(out[out$fleet==5 & out$age<12,],aes(x=year,y=logobs,group=yc,colour=as.factor(yc)))+
    geom_line()+theme_bw()+ggtitle(paste(unique(out[out$fleet==5,]$fleetName),'signals'))+
    theme(legend.position = "none")+ylab('log index')+geom_point()
  ,
  ggplot(cn[cn$year<2015,],aes(x=log(cn),y=log(var)))+geom_point()+
    theme_bw()+
    ggtitle(paste(unique(out[out$fleet==5,]$fleetName),'variance'))+ylab('log variance')+xlab('log index')
  ,
  # ggplot(out[out$fleet==5 & !is.na(out$logobs),],aes(x=year,y=age,size=weight,colour=weight))+geom_point()+
  #   theme_bw()+ggtitle(paste(unique(out[out$fleet==5,]$fleetName),'data weight'))+
  #   theme(legend.position = "none")+
  #   scale_colour_gradient(low = "black", high = "red")
  # ,
  ncol=2
)


ggsave("output/figures/fig_NASFoldsignal.png", p_NASFold, width = 12, height = 4, dpi = 300)



#------------------------------------------------------------------------------#
#Prepare the plot for the NASF new
#------------------------------------------------------------------------------#

p_NASFnew <- gridExtra::grid.arrange(
  ggplot(out[out$fleet==6 & out$age<12,],aes(x=year,y=logobs,group=yc,colour=as.factor(yc)))+
    geom_line()+theme_bw()+ggtitle(paste(unique(out[out$fleet==6,]$fleetName),'signals'))+
    theme(legend.position = "none")+ylab('log index')
  ,
  ggplot(cn[cn$year>=2015,],aes(x=log(cn),y=log(var)))+geom_point()+
    geom_point(data=cn[cn$year>=max(cn$year),],aes(x=log(cn),y=log(var)),colour='red')+
    theme_bw()+
    ggtitle(paste(unique(out[out$fleet==6,]$fleetName),'variance'))+ylab('log variance')+xlab('log index')
  ,
  # ggplot(out[out$fleet==6 & !is.na(out$logobs),],aes(x=year,y=age,size=weight,colour=weight))+geom_point()+
  #   theme_bw()+ggtitle(paste(unique(out[out$fleet==6,]$fleetName),'data weight'))+
  #   theme(legend.position = "none")+
  #   scale_colour_gradient(low = "black", high = "red")
  # ,
  ncol=2
)


ggsave("output/figures/fig_NASFnewsignal.png", p_NASFnew, width = 12, height = 4, dpi = 300)



#------------------------------------------------------------------------------#
#prepare the figure for the RFID
#------------------------------------------------------------------------------#

cn <- reshape2::melt(read.ices(taf.data.path('rfid_index_BioSampleID.txt'))$RFID )
names(cn)<-c('year','age','cn')
cn_cv <- reshape2::melt(read.ices(taf.data.path('rfid_cv_BioSampleID.txt'))$RFID)
names(cn_cv)<-c('year','age','cv')
cn <- plyr::join(cn,cn_cv)
cn$var <- (cn$cn*cn$cv)**2



p_RFID <- gridExtra::grid.arrange(
  ggplot(out[out$fleet==7 & out$age<12,],aes(x=year,y=logobs,group=yc,colour=as.factor(yc)))+
    geom_line()+theme_bw()+ggtitle(paste(unique(out[out$fleet==7,]$fleetName),'signal'))+
    theme(legend.position = "none")+ylab('log index')
  ,
  ggplot(cn,aes(x=log(cn),y=log(var)))+geom_point()+
    geom_point(data=cn[cn$year==(max(cn$year)),],colour='red')+
    theme_bw()+
    ggtitle(paste(unique(out[out$fleet==7,]$fleetName),'variance'))+ylab('log variance')+xlab('log index')
  ,
  # ggplot(out[out$fleet==7 & !is.na(out$logobs),],aes(x=year,y=age,size=weight,colour=weight))+geom_point()+
  #   theme_bw()+ggtitle(paste(unique(out[out$fleet==7,]$fleetName),'data weight'))+
  #   theme(legend.position = "none")+
  #   scale_colour_gradient(low = "black", high = "red")
  # ,
  ncol=2
)


ggsave("output/figures/fig_RFIDsignal.png", p_RFID, width = 12, height = 4, dpi = 300)




#------------------------------------------------------------------------------#
#prepare the figure for the cathability parameters
#------------------------------------------------------------------------------#


dd <- as.data.frame(partable(fit))
dd$parname <- rownames(dd)
dd$case <- 'updated'
dd_out <- dd

dd <- as.data.frame(partable(fit_old))
dd$parname <- rownames(dd)
dd$case <- 'previous'
dd_out <- rbind(dd_out,dd)



dd_out$facet <- 'rest'
dd_out$facet[grepl('logFpar',dd_out$parname)]<-'Catchabilty'


dd_out$parname[dd_out$parname=='logFpar_0']<-'IESNS age03'
dd_out$parname[dd_out$parname=='logFpar_1']<-'IESNS age04'
dd_out$parname[dd_out$parname=='logFpar_2']<-'IESNS age05'
dd_out$parname[dd_out$parname=='logFpar_3']<-'IESNS age06'
dd_out$parname[dd_out$parname=='logFpar_4']<-'IESNS age07'
dd_out$parname[dd_out$parname=='logFpar_5']<-'IESNS age08'
dd_out$parname[dd_out$parname=='logFpar_6']<-'IESNS age09'
dd_out$parname[dd_out$parname=='logFpar_7']<-'IESNS age10'
dd_out$parname[dd_out$parname=='logFpar_8']<-'IESNS age11-12'
dd_out$parname[dd_out$parname=='logFpar_9']<-'BESS age02'
dd_out$parname[dd_out$parname=='logFpar_10']<-'BESS age03'
dd_out$parname[dd_out$parname=='logFpar_11']<-'IESNS_rec age02'
dd_out$parname[dd_out$parname=='logFpar_12']<-'NASF_old age03'
dd_out$parname[dd_out$parname=='logFpar_13']<-'NASF_old age04'
dd_out$parname[dd_out$parname=='logFpar_14']<-'NASF_old age05'
dd_out$parname[dd_out$parname=='logFpar_15']<-'NASF_old age06'
dd_out$parname[dd_out$parname=='logFpar_16']<-'NASF_old age07'
dd_out$parname[dd_out$parname=='logFpar_17']<-'NASF_old age08-12'
dd_out$parname[dd_out$parname=='logFpar_18']<-'NASF_new age03'
dd_out$parname[dd_out$parname=='logFpar_19']<-'NASF_new age04'
dd_out$parname[dd_out$parname=='logFpar_20']<-'NASF_new age05'
dd_out$parname[dd_out$parname=='logFpar_21']<-'NASF_new age06'
dd_out$parname[dd_out$parname=='logFpar_22']<-'NASF_new age07'
dd_out$parname[dd_out$parname=='logFpar_23']<-'NASF_new age08'
dd_out$parname[dd_out$parname=='logFpar_24']<-'NASF_new age09'
dd_out$parname[dd_out$parname=='logFpar_25']<-'NASF_new age10'
dd_out$parname[dd_out$parname=='logFpar_26']<-'NASF_new age11-12'
dd_out$parname[dd_out$parname=='logFpar_27']<-'RFID age3-12'
dd_out$fleet <- sub(" .*", "", dd_out$parname)

plot_catchability <- ggplot(dd_out[dd_out$facet == 'Catchabilty', ], 
                            aes(x = parname, y = par, fill = case)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = par - `sd(par)`, ymax = par + `sd(par)`),
                width = 0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Parameter", y = "Estimate", fill = "Case") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

plot_catchability

ggsave("output/figures/fig_catchability.png", plot_catchability, width = 12, height = 8, dpi = 600)




#------------------------------------------------------------------------------#
#observation variance
#------------------------------------------------------------------------------#
dd_out$facet[grepl('logSdLogObs',dd_out$parname)]<-'ObservationVariance'


dd_out$parname[dd_out$parname=='logSdLogObs_0']<-'Catch age02'
dd_out$parname[dd_out$parname=='logSdLogObs_1']<-'Catch age03'
dd_out$parname[dd_out$parname=='logSdLogObs_2']<-'Catch age04'
dd_out$parname[dd_out$parname=='logSdLogObs_3']<-'Catch age05-12'

dd_out$parname[dd_out$parname=='logSdLogObs_4']<-'IESNS age03'
dd_out$parname[dd_out$parname=='logSdLogObs_5']<-'IESNS age04-12'

dd_out$parname[dd_out$parname=='logSdLogObs_6']<-'BESS age02-3'
dd_out$parname[dd_out$parname=='logSdLogObs_7']<-'IESNS_rec age02'

dd_out$parname[dd_out$parname=='logSdLogObs_8']<-'NASF_old age03'
dd_out$parname[dd_out$parname=='logSdLogObs_9']<-'NASF_old age04'
dd_out$parname[dd_out$parname=='logSdLogObs_10']<-'NASF_old age05-12'

dd_out$parname[dd_out$parname=='logSdLogObs_11']<-'NASF_new age03'
dd_out$parname[dd_out$parname=='logSdLogObs_12']<-'NASF_new age04'
dd_out$parname[dd_out$parname=='logSdLogObs_13']<-'NASF_new age05-12'
dd_out$parname[dd_out$parname=='logSdLogObs_14']<-'RFID age03-9'
dd_out$parname[dd_out$parname=='logSdLogObs_15']<-'RFID age10-11'
dd_out$parname[dd_out$parname=='logSdLogObs_16']<-'RFID age12'
dd_out$fleet <- sub(" .*", "", dd_out$parname)

plot_ObservationVariance <- ggplot(dd_out[dd_out$facet == 'ObservationVariance', ], 
                                   aes(x = parname, y = par, fill = case)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = par - `sd(par)`, ymax = par + `sd(par)`),
                width = 0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Parameter", y = "Estimate", fill = "Case") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


ggsave("output/figures/fig_ObservationVariance.png", plot_ObservationVariance, 
       width = 12, height = 10, dpi = 600)



#------------------------------------------------------------------------------#
#and then the rest of the parameters
#------------------------------------------------------------------------------#
plot_rest <- ggplot(dd_out[dd_out$facet == 'rest', ], 
                    aes(x = parname, y = par, fill = case)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = par - `sd(par)`, ymax = par + `sd(par)`),
                width = 0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Parameter", y = "Estimate", fill = "Case") +
  theme_bw() 


ggsave("output/figures/fig_rest_config.png", plot_rest, width = 8, height = 6, dpi = 600)





#------------------------------------------------------------------------------#
##RESIDUAL
#------------------------------------------------------------------------------#

res <- res_fit
class(res)<-'data.frame'
res<-res[!is.na(res$observation),]
res<-res[res$year!=1988,]
res$fleetNames <- attr(fit$data,'fleetNames')[res$fleet]

res$fleetNames[res$fleetNames=='Bar']<-'IESNS bar'
res$fleetNames[res$fleetNames=='recBar']<-'BESS'
res$fleetNames[res$fleetNames=='Residual catch']<-'Catch'
plot_residual <- ggplot(res, aes(x = year, y = age)) +
  geom_point(aes(size = abs(residual), color = residual > 0), alpha = 0.5) +
  scale_color_manual(
    values = c("TRUE" = "blue", "FALSE" = "red"),
    labels = c("FALSE" = "Negative", "TRUE" = "Positive"),
    name = "Residual sign"
  ) +
  scale_size_continuous(
    range = c(1, 6),   # adjust size range if you want
    breaks = c(0, 4, 6),
    name = "Residual magnitude"
  ) +
  facet_wrap(~fleetNames) +
  theme_bw() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
plot_residual
ggsave("output/figures/fig_residual.png", plot_residual, width = 12, 
       height = 8, dpi = 600)


#------------------------------------------------------------------------------#
##RESIDUAL QQplot
#------------------------------------------------------------------------------#

p<-ggplot(res,aes(sample=residual))+
  geom_qq(distribution = stats::qnorm)+
  geom_qq_line()+
  ggtitle('QQplot by fleet')+ylim(c(-5,5))+ylab('')+xlab('')+facet_wrap(~fleetNames)+
  theme_bw() 
p1<-ggplot(res,aes(sample=residual))+
  geom_qq(distribution = stats::qnorm)+
  geom_qq_line()+
  ggtitle('QQplot all fleet')+ylim(c(-5,5))+ylab('')+xlab('')+
  theme_bw() 

plot_qqres <-gridExtra::grid.arrange(p1,p,ncol=2)

ggsave("output/figures/fig_residualQQ.png", plot_qqres, width = 12, height = 8, dpi = 600)





#------------------------------------------------------------------------------#
#Process residuals
#------------------------------------------------------------------------------#
res <-fit_pro
class(res)<-'data.frame'
res$fleetName <- 'log(N)'
res[res$fleet==1,]$fleetName<-'ResidualCatch'

plot_processresiduals <- ggplot(res, aes(x = year, y = age)) +
  geom_point(aes(size = abs(residual), color = residual > 0), alpha = 0.5) +
  scale_color_manual(
    values = c("TRUE" = "blue", "FALSE" = "red"),
    labels = c("FALSE" = "Negative", "TRUE" = "Positive"),
    name = "Residual sign"
  ) +
  scale_size_continuous(
    range = c(1, 6),   # adjust size range if you want
    breaks = c(0, 4, 6),
    name = "Residual magnitude"
  ) +
  facet_wrap(~fleetName) +
  theme_bw() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("output/figures/fig_processresidual.png", plot_processresiduals, width = 12, height = 8, dpi = 600)


#------------------------------------------------------------------------------#
#Process residuals
#------------------------------------------------------------------------------#
plot_residualqq<-ggplot(res,aes(sample=residual))+
  geom_qq(distribution = stats::qnorm)+
  geom_qq_line()+
  ggtitle('QQplot of process residual')+ylim(c(-5,5))+ylab('')+xlab('')+facet_wrap(~fleetName)+
  theme_bw() 


ggsave("output/figures/fig_processresidualQQ.png", plot_residualqq, width = 12, height = 8, dpi = 600)






#------------------------------------------------------------------------------#
#Compute the process error
#------------------------------------------------------------------------------#
pe <- melt(get.procres.sdrep(fit))
colnames(pe)<-c('year','age','pe')
pe <- plyr::join(pe , data.frame(age = fit$conf$minAge:fit$conf$maxAge,
                                 par = exp(fit$pl$logSdLogN[fit$conf$keyVarLogN+1]))
)
pe$case <- 'updated'


#hack due to update in SAM. i.e the preivouse assessment needs to be refitted
fit_test <- sam.fit(fit_old$dat,fit$conf,par=defpar(fit_old$dat,fit$conf))
pe_old <- melt(get.procres.sdrep(fit_test))
colnames(pe_old)<-c('year','age','pe')
pe_old$case <- 'previous'
pe_old <- plyr::join(pe_old , data.frame(age = fit_test$conf$minAge:fit_test$conf$maxAge,
                                         par = exp(fit_test$pl$logSdLogN[fit_test$conf$keyVarLogN+1]))
)


N_tab <- melt(ntable(fit))
colnames(N_tab)<-c('year','age','N')
pe_N <- plyr::join(pe,N_tab)

N_tab_old <- melt(ntable(fit_test))
colnames(N_tab_old)<-c('year','age','N')
pe_N_old <- plyr::join(pe_old,N_tab_old)

pe_N <- rbind(pe_N,pe_N_old)

pe_N$YC <- pe_N$year-pe_N$age

dd<-pe_N[c('year','age','YC','N')]
dd<-unique(dd[dd$age==2,])
names(dd)[4]<-'R'
dd$age<-NULL
dd$year <- NULL
pe_N<-plyr::join(pe_N,dd)

plot_processerror <- ggplot(pe_N[pe_N$age !=2, ], aes(x = year,y=pe,colour=case,fill=case)) +
  # geom_col(aes(x=year-0.2,y = pe), 
  #          position = "dodge", width = 0.3) +
  geom_col(position = position_dodge(width = 0.8), width = 0.4, alpha = 0.4) +
  facet_wrap(~age) +
  xlab('year') +
  ylab('Process Error') +
  scale_fill_manual(values = c("black", "red")) +
  scale_color_manual(values = c("black", "red")) +
  theme_bw() 



ggsave("output/figures/fig_processError.png", plot_processerror, width = 12, height = 8, dpi = 600)











#------------------------------------------------------------------------------#
#make the leave out plot
#------------------------------------------------------------------------------#

LO_fit$base<-attr(LO_fit,'fit')
names(LO_fit)[names(LO_fit)=='w.o. recBar']<-'BESS'
names(LO_fit)[names(LO_fit)=='w.o. Bar']<-'IESNS, bar'
plot_SSB_LR<-gridExtra::grid.arrange(plot.LO(LO_fit,what = 'SSB')+ggtitle('SSB'),
                                     plot.retro(ret,what = 'SSB'),ncol=2)
ggsave("output/figures/fig_SSB_LR.png", plot_SSB_LR, width = 12, height = 8, dpi = 600)


plot_SSB_LR<-gridExtra::grid.arrange(plot.LO(LO_fit,what = 'FbarW')+ggtitle('FbarW'),
                                     plot.retro(ret,what = 'FbarW'),ncol=2)
ggsave("output/figures/fig_Fbar_LR.png", plot_SSB_LR, width = 12, height = 8, dpi = 600)


plot_SSB_LR<-gridExtra::grid.arrange(plot.LO(LO_fit,what = 'R')+ggtitle('Recruitment'),
                                     plot.retro(ret,what = 'R'),ncol=2)
ggsave("output/figures/fig_R_LR.png", plot_SSB_LR, width = 12, height = 8, dpi = 600)



plot_LO<-gridExtra::grid.arrange(plot.LO(LO_fit,what = 'SSB')+theme(legend.position = "none"),
                                 plot.LO(LO_fit,what = 'FbarW')+theme(legend.position = "none"),
                                 plot.LO(LO_fit,what = 'R'),ncol=1)


ggsave("output/figures/fig_LO.png", plot_LO, width = 12, height = 8, dpi = 600)





#------------------------------------------------------------------------------#
#Make the retro plot
#------------------------------------------------------------------------------#
plot_retro <- gridExtra::grid.arrange(plot.retro(ret,what = 'SSB')+theme(legend.position = "none"),
                                      plot.retro(ret,what = 'FbarW')+theme(legend.position = "none"),
                                      plot.retro(ret,what = 'R'),ncol=1)

ggsave("output/figures/fig_retro.png", plot_retro, width = 12, height = 8, dpi = 600)




#------------------------------------------------------------------------------#
#Make the stock summary plot
#------------------------------------------------------------------------------#
fbar <- compute_FbarW(fit)
ssb <- as.data.frame(ssbtable(fit))
ssb$year <- as.numeric(rownames(ssb))

rec <- as.data.frame(rectable(fit))
rec$year <- as.numeric(rownames(rec))

plot_status <- gridExtra::grid.arrange(
  ggplot(ssb,aes(x=year,y=Estimate,ymin=Low,ymax=High))+geom_line()+geom_ribbon(alpha=0.4)+
    theme_bw() +ggtitle('SSB') ,
  
  ggplot(fbar,aes(x=year,y=Estimate,ymin=Low,ymax=High))+geom_line()+geom_ribbon(alpha=0.4)+
    theme_bw() +ggtitle('FbarW'),
  
  ggplot(rec,aes(x=year,y=Estimate,ymin=Low,ymax=High))+geom_line()+geom_ribbon(alpha=0.4)+
    theme_bw() +ggtitle('R'),
  
  ncol=3
)

ggsave("output/figures/fig_summary.png", plot_status, width = 16, height = 6, dpi = 600)





#------------------------------------------------------------------------------#
#Make the presentation
#------------------------------------------------------------------------------#
quarto_render(
  input       = "assessment_presentation.qmd",
  output_file = "assessment_presentation.pdf"
)
file.rename("assessment_presentation.pdf", "output/presentations/assessment_presentation.pdf")


