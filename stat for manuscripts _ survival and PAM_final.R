library(ggplot2)
library(plyr)
library(tidyr)
library(dplyr)
library(reshape2)
library(data.table)
library(ez)
library(car)
library(lmPerm)
library(nlme)
library(lmerTest)
library(MASS)
library(emmeans)

d_diff <- survival_diff

rm(d)
str(d)
View(d_all)
View(d_diff)


#####survival plot 
#is there a different in survival depandeant on source with interacion of treatment, sea vs RSS?
#simple one way Anova
###not using this one
##this is just the planulae from the system (RSS)

d <- survival_for_r

day7<-subset(d, d$day=='%day_7')

simple_anova = aov(data=day7, survival ~ source*treatment)
anova(simple_anova)

TUKEY1 <- TukeyHSD(aov(simple_anova),"source", ordered = TRUE)
TUKEY1

shapiro.test(simple_anova$residuals) #>>>>normal


###However, the simpe Anova is not the right model, i need to make the aquaria the random effect, so I use Lmer. 
#is there a differce in the survival between the day 7 survival of RSS seawater
#conditions. based on the survival of day 7, with aquaria as a random effect.

qqp(d_diff$difference, "norm") #>>>> normal distirbution
View(d)

fixed_diff_mod <-  lmer(data=d_diff, difference ~ treatment+(1|aquarium),REML=FALSE)
summary(fixed_diff_mod)
anova(fixed_diff_mod)


## There is siginigficat decrease in survival in RCP 4.5 and 8.5 

####graphics##########

s <- ggplot(d, aes(x = treatment, y = survival, fill = day))+
  geom_boxplot()+
  theme_bw() %+replace% 
  theme(legend.position="none", axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor.y=element_blank(),
        panel.background = element_blank(),
        axis.title =element_text(size=10),
        axis.text=element_text(size=10),
        axis.text.x = element_text(size=10))+
  scale_y_continuous(labels = scales::percent)+
  labs(x="",y="Survival (%)")+
  scale_fill_manual(values = c("gray75", "white"), 
                    labels = c("%day_3" = "Day 3" , "%day_7" = "Day 7"))+
  scale_x_discrete(limits=c("Ambient","RCP 4.5","RCP 8.2"))+
  #                  labels = c("amb" = "Ambient\n SW" , "medium" = "SW+2°C \n pH ~ 7.8", "high"="SW+4°C\n pH ~ 7.6"))+
  annotate("text", x=1, y=0.01, label= "365", size= 3)+
  annotate("text", x=2, y=0.01, label= "286", size= 3)+
  annotate("text", x=3, y=0.01, label= "50", size= 3)+
  annotate("text", x=0.55, y=1, label= "(A)", size= 4)

s <- s+  theme(legend.title=element_blank())

s

ggsave("survival_A.jpeg", plot = s, width = 8.5 , height = 8.5 ,dpi=300, 
       units = "cm")

####Figure B is similar code, using survival data of embryos from the Sea


####PAM data, the data file has 2 sheets, each of them is used as a seperate csv. here

d_all <- all_pam
rm(all_pam)
d_eq <- Pam_equations
d_all$Aquaria <- as.factor(d_all$Aquaria)
d_all$PAR <- as.factor(d_all$PAR)


names(d_all)[8]<-paste("Effective")

#checking each parameter seperately
#the NPQ  ## the PAM data
ggplot(d_continuous, aes(x = NPQ)) +
  geom_density(alpha = .2, fill = "#FF6666")

d_NPQ <- na.omit(d_all)
View(d_NPQ)
qqnorm(d_NPQ$NPQ, col ='blue')
qqline(d_NPQ$NPQ, col ='red')

logNPQ <- log10(d_NPQ$NPQ) 
qqnorm(logNPQ, col ='blue')
qqline(d_NPQ$NPQ, col ='red')

###SUMMARY PER TREATMENT
NPQ_sum <- ddply(d_all, .(treatment, PAR), summarise, 
                        mean= mean(NPQ,na.rm=TRUE),
                 sd= sd(NPQ,na.rm=TRUE))

View(NPQ_sum)

View(d_all)

qqnorm(mo_NPQ$resid, col ='blue')
qqline(mo_NPQ$resid, col ='red')
skewness(mo_NPQ$resid)

###with aquaria as a nested random effect
ddd$sqrt_NPQ <- sqrt(ddd$NPQ)

fit_NPQ2<- lmer(sqrt_NPQ~treatment:PAR+(1|treatment/Aquaria),REML = FALSE, data=ddd)
anova(fit_NPQ2)
summary(fit_NPQ2)


###check pam equations difference
rm(Pam_equations)
dd <- Pam_equations
head(dd)


lmm_rETR_MAX<-lmer(rETRmax~treatment+ (1|Aquaria),REML = FALSE, data=dd)
anova(lmm_rETR_MAX)
plot(lmm_rETR_MAX)

qqnorm(resid(lmm_rETR_MAX))
qqline(resid(lmm_rETR_MAX))

shapiro.test(resid(lmm_rETR_MAX))
leveneTest(rETRmax~treatment,d=dd)


##checking for differnces in the slope=alpha

##Mixed effect model
d_eq$rETRmax <- as.integer(d_eq$rETRmax)
str(d_eq)

lmm_slope<-lmer(Slope~treatment+(1|Aquaria),REML = FALSE, data=dd) 
anova(lmm_slope)
summary(lmm_slope)

leveneTest(Slope~treatment,d=dd)
shapiro.test(resid(lmm_slope))

####differences in eK
lmm_Ek<-lmer(Ek~treatment+(1|Aquaria),REML = FALSE, data=dd) 
anova(lmm_Ek)

leveneTest(Ek~treatment,d=d_eq)

###check differences in fv/fm - for that i need all the effective in PAR=0

d_PAM <- All_pam
View(d_PAM)
FVFM <- subset(d_PAM, d_PAM$PAR=='1')
View(FVFM)
names(FVFM)[8]<-paste("FVFM")

FVFM_sum <- ddply(FVFM, .(treatment), summarise, 
                 mean= mean(FVFM,na.rm=TRUE),
                 sd= sd(FVFM,na.rm=TRUE))


####differences in FVFM
names(FVFM)[8]<-paste("FVFM")

lmm_FVFM<-lmer(sqrt(FVFM)~treatment+(1|Aquaria),REML = FALSE, data=FVFM) 
anova(lmm_FVFM)

hist(residuals(lmm_FVFM))
qqnorm(resid(lmm_FVFM))
qqline(resid(lmm_FVFM))

require(multcomp)
summary(glht(fit,  linfct=mcp(treatment = "Tukey")))

TukeyHSD(aov(fit), "treatment" )

summary(fit)

shapiro.test(fit$residuals)
leveneTest(rETRmax~treatment,d=dd)

####NPQmax --- not using this one

d_PAM <- All_pam
View(d_PAM)
npq_max <- subset(d_PAM, d_PAM$PAR=='701')
View(npq_max)
lmm_NPQ_MAX<-lmer(NPQ~treatment+(1|Aquaria),REML = FALSE, data=npq_max) 
anova(lmm_NPQ_MAX)

View(lmm_NPQ_MAX)

