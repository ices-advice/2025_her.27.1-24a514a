#------------------------------------------------------------------------------#
#report.R
#
#Description: 
#Here we produce all the figures and tables that is used for the report section
#in WGWIDE
#------------------------------------------------------------------------------#
#TODO: work is still needed to produce all tables and figures

#------------------------------------------------------------------------------#
#remove everything from memory
#------------------------------------------------------------------------------#
rm(list=ls())



#------------------------------------------------------------------------------#
#Load utils
#------------------------------------------------------------------------------#
source('boot/rscript/utils.r')
library(TAF)
taf.library('stockassessment')
taf.library('ggplot2')
library(gridExtra)
taf.library(reshape2)



#------------------------------------------------------------------------------#
#load fit objects
#------------------------------------------------------------------------------#
load('boot/rscript/fit_official_benchmark.Rda')
fit_old <- fit
load('model/fit.Rda')
load('model/diag.Rda')



#------------------------------------------------------------------------------#
#prepare folder structure
#------------------------------------------------------------------------------#
mkdir("report")
mkdir("report/tables")
mkdir("report/figures")



#------------------------------------------------------------------------------#
#List of tables to produce
#Move tables to correct folder
#------------------------------------------------------------------------------#
# Table 4.4.8.1. Norwegian spring-spawning herring. Coefficient of variance of catch-at-age used by SAM.
file.rename("data/tab.4.4.8.1.txt", "report/tables/tab.4.4.8.1.txt")
#Table 4.4.8.2. Norwegian spring-spawning herring. Coefficient of variance of NASF used by SAM.
file.rename("data/tab.4.4.8.2a.txt", "report/tables/tab.4.4.8.2a.txt")
file.rename("data/tab.4.4.8.2b.txt", "report/tab.4.4.8.2b.txt")
#Table 4.4.8.3. Norwegian spring-spawning herring. Coefficient of variance  of IESNS used by SAM.
file.rename("data/tab.4.4.8.3.txt", "report/tables/tab.4.4.8.3.txt")
#Table 4.4.8.4. Norwegian spring-spawning herring. Coefficient of variance  of IESNS recruitment index used by SAM.
file.rename("data/tab.4.4.8.4.txt", "report/tables/tab.4.4.8.4.txt")
#Table 4.4.8.5. Norwegian spring-spawning herring. Coefficient of variance  of IESNS recruitment index used by SAM.
file.rename("data/tab.4.4.8.5.txt", "report/tables/tab.4.4.8.5.txt")




#------------------------------------------------------------------------------#
#Table 4.5.1.1. Norwegian spring-spawning herring. Parameter estimates of the 
#final SAM model fit. The estimates from the benchmark 2025 is shown.
#------------------------------------------------------------------------------#
new <- as.data.frame(partable(fit)[,c(1,2)])
names(new)<-paste(c('Estimate','sd'),max(fit$data$years))
old <- as.data.frame(partable(fit_old)[,c(1,2)])
names(old)<-paste(c('Estimate','sd'),max(fit_old$data$years))
new$Parameter <- rownames(new)
rownames(new)<-NULL
old$Parameter <- rownames(old)
rownames(old)<-NULL
tab<-plyr::join(new,old)
tab[,c(1,2,4,5)]<-round(tab[,c(1,2,4,5)],digits = 3)
write.csv(tab[,c(3,1,2,4,5)],file = 'report/tables/4.5.1.1.csv')



#------------------------------------------------------------------------------#
#Table 4.5.1.2. Norwegian spring-spawning herring. Point estimates of Stock in 
#numbers (millions).
#------------------------------------------------------------------------------#
write.csv(round(ntable(fit)),file = 'report/tables/4.5.1.2.csv')



#------------------------------------------------------------------------------#
# Table 4.5.1.3. Norwegian spring-spawning herring. Point estimates of
# fishing mortality.
#------------------------------------------------------------------------------#
write.csv(round(faytable(fit),digits=3),file = 'report/tables/4.5.1.3.csv')



#------------------------------------------------------------------------------#
# Table 4.5.1.4. Norwegian spring-spawning herring. Final stock summary table. 
#High and low represent approximate 95% confidence limits.
#------------------------------------------------------------------------------#
rectab <- as.data.frame(rectable(fit))
rectab<-round(rectab[c('Estimate','High','Low')])

ssbtab <- as.data.frame(ssbtable(fit))
ssbtab<-round(ssbtab[c('Estimate','High','Low')])

fbartab <- compute_FbarW(fit)
fbartab<-fbartab[c('Estimate','High','Low')]

caa <- stockassessment::read.ices(taf.data.path('caa.txt'))
caa[caa<0]<-NA


catch<-data.frame(catch=round(rowSums(fit$data$catchMeanWeight[,,1]*caa,na.rm = T),3))

tmp <- c(
  round(mean(rectab$Estimate)),round(mean(rectab$High)),round(mean(rectab$Low)),
  round(mean(ssbtab$Estimate)),round(mean(ssbtab$High)),round(mean(ssbtab$Low)),
  round(mean(catch$catch)),
  round(mean(fbartab$Estimate),3),round(mean(fbartab$High),3),round(mean(fbartab$Low),3)
  
)

fbartab<-rbind(round(fbartab,3),c('','',''))
catch <- rbind(catch,'')
tab<-cbind(rectab,ssbtab,catch,fbartab)
out <- rbind(tab,tmp)
rownames(out)[length(rownames(out))] = 'Average'
colnames(out)<-c('Recruitment','High','Low','SSB','High','Low','Catch','Fbar','High','Low')
write.table(out,file="report/tables/tab4.5.1.4.txt",sep=";",quote=F)



# Table 4.8.1.1. Norwegian spring-spawning herring. Input to short-term prediction. Stock size in millions and weight in kg.

# Table 4.8.2.1. Norwegian spring-spawning herring. Short-term prediction.
#+The catch options:



#List of figures





#------------------------------------------------------------------------------#
# Figure 4.5.1.1
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


dd_out$parname[dd_out$parname=='logFpar_0']<-'IESNS age 03'
dd_out$parname[dd_out$parname=='logFpar_1']<-'IESNS age 04'
dd_out$parname[dd_out$parname=='logFpar_2']<-'IESNS age 05'
dd_out$parname[dd_out$parname=='logFpar_3']<-'IESNS age 06'
dd_out$parname[dd_out$parname=='logFpar_4']<-'IESNS age 07'
dd_out$parname[dd_out$parname=='logFpar_5']<-'IESNS age 08'
dd_out$parname[dd_out$parname=='logFpar_6']<-'IESNS age 09'
dd_out$parname[dd_out$parname=='logFpar_7']<-'IESNS age 10'
dd_out$parname[dd_out$parname=='logFpar_8']<-'IESNS age 11-12'
dd_out$parname[dd_out$parname=='logFpar_9']<-'BESS age 02'
dd_out$parname[dd_out$parname=='logFpar_10']<-'BESS age 03'
dd_out$parname[dd_out$parname=='logFpar_11']<-'IESNS_rec age 02'
dd_out$parname[dd_out$parname=='logFpar_12']<-'NASF_old age 03'
dd_out$parname[dd_out$parname=='logFpar_13']<-'NASF_old age 04'
dd_out$parname[dd_out$parname=='logFpar_14']<-'NASF_old age 05'
dd_out$parname[dd_out$parname=='logFpar_15']<-'NASF_old age 06'
dd_out$parname[dd_out$parname=='logFpar_16']<-'NASF_old age 07'
dd_out$parname[dd_out$parname=='logFpar_17']<-'NASF_old age 08-12'
dd_out$parname[dd_out$parname=='logFpar_18']<-'NASF_new age 03'
dd_out$parname[dd_out$parname=='logFpar_19']<-'NASF_new age 04'
dd_out$parname[dd_out$parname=='logFpar_20']<-'NASF_new age 05'
dd_out$parname[dd_out$parname=='logFpar_21']<-'NASF_new age 06'
dd_out$parname[dd_out$parname=='logFpar_22']<-'NASF_new age 07'
dd_out$parname[dd_out$parname=='logFpar_23']<-'NASF_new age 08'
dd_out$parname[dd_out$parname=='logFpar_24']<-'NASF_new age 09'
dd_out$parname[dd_out$parname=='logFpar_25']<-'NASF_new age 10'
dd_out$parname[dd_out$parname=='logFpar_26']<-'NASF_new age 11-12'
dd_out$parname[dd_out$parname=='logFpar_27']<-'RFID age 3-12'

dd_out$fleet <- sub(" .*", "", dd_out$parname)
dd_out[dd_out$facet=='Catchability']$parname <- sub(".*?(\\d{2}(?:-\\d{2})?)$", "\\1", dd_out$parname)


plot_catchability <- ggplot(dd_out[dd_out$facet == 'Catchabilty', ], 
                            aes(x = parname, y = par, fill = case)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = par - `sd(par)`, ymax = par + `sd(par)`),
                width = 0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Parameter", y = "Estimate", fill = "Case") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~fleet, scales = "free_x")+ggtitle('Catchability parameter')+xlab('ages')





dd_out$facet[grepl('logSdLogObs',dd_out$parname)]<-'ObservationVariance'


dd_out$parname[dd_out$parname=='logSdLogObs_0']<-'Catch age 02'
dd_out$parname[dd_out$parname=='logSdLogObs_1']<-'Catch age 03'
dd_out$parname[dd_out$parname=='logSdLogObs_2']<-'Catch age 04'
dd_out$parname[dd_out$parname=='logSdLogObs_3']<-'Catch age 05-12'

dd_out$parname[dd_out$parname=='logSdLogObs_4']<-'IESNS age 03'
dd_out$parname[dd_out$parname=='logSdLogObs_5']<-'IESNS age 04-12'

dd_out$parname[dd_out$parname=='logSdLogObs_6']<-'BESS age 02-03'
dd_out$parname[dd_out$parname=='logSdLogObs_7']<-'IESNS_rec age 02'

dd_out$parname[dd_out$parname=='logSdLogObs_8']<-'NASF_old age 03'
dd_out$parname[dd_out$parname=='logSdLogObs_9']<-'NASF_old age 04'
dd_out$parname[dd_out$parname=='logSdLogObs_10']<-'NASF_old age 05-12'

dd_out$parname[dd_out$parname=='logSdLogObs_11']<-'NASF_new age 03'
dd_out$parname[dd_out$parname=='logSdLogObs_12']<-'NASF_new age 04'
dd_out$parname[dd_out$parname=='logSdLogObs_13']<-'NASF_new age 05-12'
dd_out$parname[dd_out$parname=='logSdLogObs_14']<-'RFID age 03-09'
dd_out$parname[dd_out$parname=='logSdLogObs_15']<-'RFID age 10-11'
dd_out$parname[dd_out$parname=='logSdLogObs_16']<-'RFID age 12'
dd_out$fleet <- sub(" .*", "", dd_out$parname)
# dd_out$parname <- sub(".*?(\\d{2}(?:-\\d{2})?)$", "\\1", dd_out$parname)

plot_ObservationVariance <- ggplot(dd_out[dd_out$facet == 'ObservationVariance', ], 
                                   aes(x = parname, y = par, fill = case)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = par - `sd(par)`, ymax = par + `sd(par)`),
                width = 0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Parameter", y = "Estimate", fill = "Case") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('observation variance')+xlab('')


plot_rest <- ggplot(dd_out[dd_out$facet == 'rest', ], 
                    aes(x = parname, y = par, fill = case)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = par - `sd(par)`, ymax = par + `sd(par)`),
                width = 0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Parameter", y = "Estimate", fill = "Case") +
  theme_bw() +xlab('')+ggtitle('Rest')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


p <- gridExtra::grid.arrange(
  plot_catchability,
  plot_ObservationVariance,
  plot_rest,
  ncol = 1,
  heights = c(1.5, 1, 1)  # relative row heights
)

ggsave(filename = 'report/figures/Fig.4.5.1.1.png',p, height=30, width=20,units="cm", dpi = 600,  bg = "transparent")




#------------------------------------------------------------------------------#
# Figure 4.5.1.2.
#------------------------------------------------------------------------------#
FAY <- faytable(fit)
Fpattern<- t(faytable(fit))
for(i in 1:ncol(Fpattern))Fpattern[,i]<-Fpattern[,i]/max(Fpattern[,i])
Fpattern<- t(Fpattern)
png(file="report/figures/Fig.4.5.1.2.png")
par(mfrow=c(2,2),mar=c(2,2,1,1))
persp(y=2:12,x=as.numeric(rownames(Fpattern)),z=(Fpattern),shade=0.5,theta=-50,phi=20,
      ticktype="detailed",xlab="Year",ylab="Age",zlab="F-pattern",col="lightblue",zlim=c(0,1.2))
persp(y=2:12,x=as.numeric(rownames(Fpattern)),z=(Fpattern),shade=0.5,theta=-20,phi=20,
      ticktype="detailed",xlab="Year",ylab="Age",zlab="F-pattern",col="lightblue",zlim=c(0,1.2))
persp(y=2:12,x=as.numeric(rownames(Fpattern)),z=(Fpattern),shade=0.5,theta=20,phi=20,
      ticktype="detailed",xlab="Year",ylab="Age",zlab="F-pattern",col="lightblue",zlim=c(0,1.2))
persp(y=2:12,x=as.numeric(rownames(Fpattern)),z=(Fpattern),shade=0.5,theta=50,phi=20,
      ticktype="detailed",xlab="Year",ylab="Age",zlab="F-pattern",col="lightblue",zlim=c(0,1.2))
dev.off()






#------------------------------------------------------------------------------#
# Figure 4.5.1.3. 
#------------------------------------------------------------------------------#
dat <- fit$data
tmp <- as.data.frame(dat$aux)
tmp$weight <- dat$weight
tmp$log_obs <- dat$logobs


#prepare each panel
p<-plot.data.weight(tmp[tmp$fleet==1 & !is.na(tmp$log_obs),])
plot1<-p+ggtitle('Residual catch weight')+xlim(range(tmp$year))+ylim(range(tmp$age))

p<-plot.data.weight(tmp[tmp$fleet==2& !is.na(tmp$log_obs),])
plot2<-p+ggtitle('IESNS weight')+xlim(range(tmp$year))+ylim(range(tmp$age))
p<-plot.data.weight(tmp[tmp$fleet==3& !is.na(tmp$log_obs),])
plot3<-p+ggtitle('BESS weight')+xlim(range(tmp$year))+ylim(range(tmp$age))+
  scale_x_continuous(breaks=seq(1990,2050,2))
p<-plot.data.weight(tmp[tmp$fleet==4& !is.na(tmp$log_obs),])
plot4<-p+ggtitle('IESNS rec weight')+xlim(range(tmp$year))+ylim(range(tmp$age))+
  scale_x_continuous(breaks=seq(1990,2050,2))
p<-plot.data.weight(tmp[tmp$fleet==5& !is.na(tmp$log_obs),])
plot5<-p+ggtitle('NASF old weight')+
  scale_x_continuous(breaks=seq(1990,2050,2))
p<-plot.data.weight(tmp[tmp$fleet==6& !is.na(tmp$log_obs),])
plot6<-p+ggtitle('NASF new weight')+
  scale_x_continuous(breaks=seq(1990,2050,2))
p<-plot.data.weight(tmp[tmp$fleet==7& !is.na(tmp$log_obs),])
plot7<-p+ggtitle('RFID weight')+
  scale_x_continuous(breaks=seq(1990,2050,1))
#merge all panels and plot
p<-gridExtra::grid.arrange(plot1, plot2,plot5,plot6,plot7, ncol = 2)
ggsave('report/figures/Fig.4.5.1.3.png',p,width=15, height=15, units="cm", 
       device = "tiff", dpi = 300,  bg = "transparent")



#------------------------------------------------------------------------------#
# Figure 4.5.1.4. 
#------------------------------------------------------------------------------#
attributes(res_fit)$fleetNames[attributes(res_fit)$fleetNames=='recBar']<-'BESS'
attributes(res_fit)$fleetNames[attributes(res_fit)$fleetNames=='Bar']<-'IESNS, recruitment'
png(file="report/figures/Fig.4.5.1.4.png",
    width = 1200,   # width in pixels
    height = 1200,  # height in pixels
    res = 150)
plot(res_fit)
dev.off()






#-------------------------------------------------------------------------------
# Figure 4.5.1.5.
#-------------------------------------------------------------------------------
class(res_fit)<-'data.frame'
res_fit<-res_fit[!is.na(res_fit$observation),]
res_fit$fleet<-attributes(res_fit)$fleetNames[res_fit$fleet]

pp1<-ggplot(res_fit,aes(x=observation,y=observation+residual))+geom_point()+
  facet_wrap(~fleet,ncol=1,scales = 'free')+
  ylab('Predicted')+xlab('Observed')+
  theme(
    panel.background = element_rect(fill = "white"),  # Set background to white
    panel.grid.major = element_blank(),  # Remove grid
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )+geom_smooth(method='lm',colour='green',se = F,size=0.5)+
  ggtitle('Observed vs Predicted')

pp2<-ggplot(res_fit,aes(sample=residual))+geom_qq(distribution = stats::qnorm)+geom_qq_line()+facet_wrap(~fleet,ncol = 1,scales = 'free_y')+
  ylab('Sample Quantiles')+xlab('Theoretical Quantiles')+
  theme(
    panel.background = element_rect(fill = "white"),  # Set background to white
    panel.grid.major = element_blank(),  # Remove grid
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )+ggtitle('Norm Q-Q Plot')

p<-grid.arrange(pp1, pp2,ncol = 2)

ggsave('report/figures/Fig.4.5.1.5.png',p,width=25, height=30, units="cm", 
       device = "tiff", dpi = 300,  bg = "transparent")






#-------------------------------------------------------------------------------
#Figure 4.5.1.6, Process residuals
#-------------------------------------------------------------------------------
png(file="report/figures/Fig.4.5.1.6.png",
    width = 1200,   # width in pixels
    height = 1000,  # height in pixels
    res = 150)
plot(fit_pro)
dev.off()




#-------------------------------------------------------------------------------
#Figure 4.5.1.7, QQplot of process residuals
#-------------------------------------------------------------------------------
class(fit_pro)<-'data.frame'
fit_pro$fleet[fit_pro$fleet==1]<-'joint sample residual log(N)'
fit_pro$fleet[fit_pro$fleet==2]<-'Residual catch'
plot_residualqq<-ggplot(fit_pro,aes(sample=residual))+
  geom_qq(distribution = stats::qnorm)+
  geom_qq_line()+
  ggtitle('QQplot of process residual')+ylim(c(-5,5))+ylab('')+xlab('')+facet_wrap(~fleet)+
  theme_bw() 
ggsave("report/figures/Fig.4.5.1.7.png", plot_residualqq, width = 12, height = 8, dpi = 600)





#------------------------------------------------------------------------------#
#Fig.4.5.1.8: process error
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
  geom_col(position = position_dodge(width = 0.8), width = 0.4, alpha = 0.4) +
  facet_wrap(~age) +
  xlab('year') +
  ylab('Process Error') +
  scale_fill_manual(values = c("black", "red")) +
  scale_color_manual(values = c("black", "red")) +
  theme_bw() 


ggsave("report/figures/Fig.4.5.1.8.png", plot_processerror, width = 12, height = 8, dpi = 600)




#------------------------------------------------------------------------------#
#Fig.4.5.1.9.
#------------------------------------------------------------------------------#
plot_retro <- gridExtra::grid.arrange(plot.retro(ret,what = 'SSB')+theme(legend.position = "none"),
                                      plot.retro(ret,what = 'FbarW')+theme(legend.position = "none"),
                                      plot.retro(ret,what = 'R'),ncol=1)

ggsave("report/figures/Fig.4.5.1.9.png", plot_retro, width = 8, height = 8, dpi = 600)



#------------------------------------------------------------------------------#
# Figure 4.5.1.10. 
#------------------------------------------------------------------------------#
LO_fit$base<-attr(LO_fit,'fit')
names(LO_fit)[2]<-'w.o. BESS'
names(LO_fit)[3] <- 'w.o. IESNS bar'
plot_LO<-gridExtra::grid.arrange(plot.LO(LO_fit,what = 'SSB')+theme(legend.position = "none"),
                                 plot.LO(LO_fit,what = 'FbarW')+theme(legend.position = "none"),
                                 plot.LO(LO_fit,what = 'R'),ncol=1)
ggsave("report/figures/Fig.4.5.1.10.png", plot_LO, width = 8, height = 8, dpi = 600)



#------------------------------------------------------------------------------#
# Figure 4.5.1.11.
#------------------------------------------------------------------------------#
ssbtab <- as.data.frame(stockassessment::ssbtable(fit))
ssbtab$year<-as.numeric(rownames(ssbtab))
fbarw<-compute_FbarW(fit)
rectab <- as.data.frame(stockassessment::rectable(fit))
rectab$year <- as.numeric(rownames(rectab))
caa <- stockassessment::read.ices('boot/data/caa.txt')
caa[caa<0]<-NA
catch<-data.frame(catch=rowSums(fit$data$catchMeanWeight[,,1]*caa,na.rm = T),
                  year = rownames(fit$data$catchMeanWeight))

p0<-ggplot(catch, aes(x=as.numeric(year), y=catch)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge())+ggtitle('Landings')+ylab('Thousands tonnes')+theme_bw()
p1<-ggplot(ssbtab,aes(x=year,y=Estimate,ymin=Low,ymax=High))+geom_ribbon(colour='grey',fill='grey')+geom_line()+
  ggtitle('SSB')+ylab('Thousands tonnes')+theme_bw()
p2<-ggplot(fbarw,aes(x=year,y=Estimate,ymin=Low,ymax=High))+geom_ribbon(colour='grey',fill='grey')+geom_line()+
  ggtitle('Fishing mortality')+ylab(expression(bar(F)[w5-12]))+theme_bw()
p3<-ggplot(rectab,aes(x=year,y=Estimate,ymin=Low,ymax=High))+geom_ribbon(colour='grey',fill='grey')+geom_line()+
  ggtitle('Recruitment (age2)')+ylab('Millions')+theme_bw()


p<-grid.arrange(p0,p3,p2,p1,ncol = 2,nrow=2)
ggsave(filename = 'report/figures/Fig.4.5.1.11.png',p, height=20, width=20,units="cm", dpi = 600,  bg = "transparent")




