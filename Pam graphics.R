
library(ggplot2)
library(plyr)
library(tidyr)
library(dplyr)
library(reshape2)
library(data.table)
library(ez)
library(car)
library(lmPerm)
library(lmerTest)

##bringing 'all pam' as csv. from Pam data file

d <- all_pam
names(d)[8]<-paste("effective")
d$treatment[grepl('ambient', d$treatment)] <- 'Ambient'
View(d)

p_rETR2 <-  ggplot(d, aes(y=rETR, x=PAR, colour=treatment,group=treatment)) + 
  #geom_point(aes(colour= treatment),size=2,alpha=0.5)+
  geom_smooth(size=0.8,alpha=0.5,na.rm = FALSE,level=0.95)+
  scale_color_manual(values=c("black","Blue","red")) +
  theme_bw() %+replace% 
  theme(legend.position="none",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor.y=element_blank(), 
        panel.background = element_blank(),
        axis.title =element_text(size=9),
        axis.text=element_text(size=9),
        axis.text.x = element_text(size=9))+
  #scale_x_continuous("PAR", labels = as.numeric(PAR), breaks = PAR)+
  xlab(bquote('µmol photons '~ m^-2~s^-1*''))+ylab('Relative ETR')
# labs(x=parse(text='Âµmol photons, m^-2s^-1'),y=parse(text='rETR'))+
#guides(fill=guide_legend(title="Treatments"))
p_rETR2

p_effective <-  ggplot(d, aes(y=effective, x=PAR, colour=treatment,group=treatment)) + 
  #  geom_line(size=0.5)+#geom_point(aes(shape=treatment, color=treatment))+
  # geom_errorbar(aes(ymin = mean_eff-se, ymax= mean_eff+se),  
  #              width=.5, position=position_dodge(0.05))+
  #geom_point(aes(colour= treatment),size=1,alpha=0.5)+
  geom_smooth(size=0.8, alpha=0.5,na.rm = FALSE,level=0.95)+
  scale_color_manual(values=c("black","blue","red")) +
  theme_bw() %+replace% 
  theme(legend.position="none",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor.y=element_blank(), 
        panel.background = element_blank(),
        axis.title =element_text(size=9),
        axis.text=element_text(size=9),
        axis.text.x = element_text(size=9))+
  #xlab(bquote('Âµmol photons' ('m^-2 s^-1')))
  xlab(bquote('µmol photons '~ m^-2~s^-1*''))+ylab('Effective quantum yield')
#labs(x=expression(paste("Âµmol photons ", m^{-2})),y='Effective quantum yield')
p_effective

p_NPQ2 <-  ggplot(d, aes(y=NPQ, x=PAR, colour=treatment,group=treatment)) + 
  #  geom_line(size=0.5)+#geom_point(aes(shape=treatment, color=treatment))+
  # geom_errorbar(aes(ymin = mean_eff-se, ymax= mean_eff+se),  
  #              width=.5, position=position_dodge(0.05))+
  #geom_point(aes(colour= treatment),size=2,alpha=0.5)+
  geom_smooth(size=0.8,alpha=0.5,na.rm = FALSE,level=0.95)+
  scale_color_manual(values=c("black","Blue","red")) +
  theme_bw() %+replace% 
  theme(legend.position="right",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor.y=element_blank(), 
        panel.background = element_blank(),
        axis.title =element_text(size=9),
        axis.text=element_text(size=9),
        axis.text.x = element_text(size=9))+
  xlab(bquote('µmol photons '~ m^-2~s^-1*''))+ylab('Non-photochemical quenching')+
# scale_x_continuous(limits  = c(21, 32), breaks = seq(21, 32, by = 1)) 
#labs(x=expression(paste("Âµmol photons ", m^{-2})),
#      y='Non-photochemical quenching')+
# labs(x=parse(text='Âµmol photons, m^-2s^-1'),y=parse(text='rETR'))+
guides(color=guide_legend(override.aes=list(fill=NA)))
p_NPQ2

ggsave("NPQ.jpeg", plot = p_NPQ, width = 9 , height = 8.5 ,dpi=300, 
       units = "cm")
ggsave("Effective_quantum_yield.jpeg", plot = p_effective, width = 9 , height = 8.5 ,dpi=300, 
       units = "cm")
ggsave("rETR.jpeg", plot = p_rETR2, width = 9 , height = 8.5 ,dpi=300, 
       units = "cm")
ggsave("for_legend_only.jpeg", plot = p_NPQ2, width = 9 , height = 8.5 ,dpi=300, 
       units = "cm")
#########not using this
merged <- merge(x=rETR_mean, y= effective_mean,by = c("treatment","PAR"))
View(merged)

View(effective_mean)
View(rETR_mean)

MyMerge <- function(x, y){
  df <- merge(x, y, by = c("treatment","PAR"), all.x= TRUE, all.y= TRUE)
  return(df)
}
PAM_merged <- Reduce(MyMerge, list(rETR_mean, effective_mean, NPQ_mean))
####################################