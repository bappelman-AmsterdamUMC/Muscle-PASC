library(readxl)
library(tableone)
library(car)
library(nortest)
library(MASS)
library(nlme)
library(circlize)
library(rstatix)
library(emmeans)
library(ComplexHeatmap)
library(cocor)
library(ggplot2)
library(ggsignif)
library(ggpubr)

#boxcox function
boxcoxT <- function(MedX){
  
  MedLev <- MedX + 0.001
  metabs <- data.frame(MedLev)
  
  fit <- lm(MedLev ~1, data=metabs)
  z <- boxcox(fit, lambda = seq(-5,5, by=0.1), plotit=FALSE, data = metabs)
  
  lamda <- z$x[which.max(z$y)]
  #print(paste("lambda:",lamda))
  
  if(lamda!=0){ metabs$MedLevT <- ((metabs$MedLev^lamda) -1 )/lamda }
  if(lamda==0){ metabs$MedLevT <- log(metabs$MedLev) }
  #hist(metabs$MedLevT)
  
  return(scale(metabs$MedLevT))
}

# Extended Table 1 --------------------------------------------------------
PASC <- read_excel("Source data.xlsx", sheet = "Extended data table 1")
factorVars <- c("sex", "pre_cov_vacc_1")
vars <- c( "age","sex",  "bmi", "CCI", "Hospitalization", "pre_cov_vacc_1", "pre_cov_werk_uur_1", "post_cov_werk_uur_1", "days_since_infection", "days_since_infection_latest")
nonnorm <- c("CCI", "days_since_infection", "days_since_infection_latest", "pre_cov_werk_uur_1", "post_cov_werk_uur_1")

tableOne <- CreateTableOne(vars = vars, data = PASC, strata = "Group",  factorVars = factorVars)
tab2mat <- print(tableOne, showAllLevels = FALSE, nonnormal = nonnorm, minMax = FALSE, varLabels = TRUE, explain=T, noSpaces = TRUE, missing = TRUE)


# Extended Table 2 --------------------------------------------------------
PASC <- read_excel("Source data.xlsx", sheet = "Extended data table 2")
PASC$Time <- factor(PASC$Time, levels =c("Baseline", "1-day after PEM", "1-week after PEM"))

factorVars <- c("Fatique", "Concentration", "Mucle_joint_pain","Chest_pain", "Dyspnea", "Anosmia", "Sore_troat", "Cold", "Cough")
vars <- c("Fatique", "Fatigue severity", "Concentration", "Concentration severity", "Mucle_joint_pain", "Chest_pain", "Dyspnea", "Anosmia", "Sore_troat", "Cold", "Cough")
nonnorm <- c("Fatigue severity", "Concentration severity")

tableTwo <- CreateTableOne(vars = vars, data = PASC, strata = c("Time"),  factorVars = factorVars)
tab3mat <- print(tableTwo, showAllLevels = FALSE, nonnormal = nonnorm, minMax = FALSE, varLabels = TRUE, explain=T, noSpaces = TRUE, missing = TRUE)


# Figure 1. ---------------------------------------------------------------
PASC <- read_excel("Source data.xlsx", sheet = "Figure1")

#Set variables
PASC$sex <- as.factor(PASC$sex)
PASC$group <- as.factor(PASC$group)

#Check normality
p_norm <- sapply(PASC[,c(6:8)], shapiro.test)
p_norm
hist(PASC$VO2max) #check for all variables

#panel A VO2max
stat_vo2 <- t.test(VO2max ~ group, data = PASC)
stat_vo2 # p<0.0001 ***
stat_vo2 <- wilcox.test(VO2max ~ group, data = PASC)
stat_vo2 # p<0.0001 ***

fig_vo2max<- ggplot(data  = PASC, aes(x = group, y = VO2max)) +
  geom_boxplot(aes(fill = group), width = 0.6, position = position_dodge(width = 0.6), outlier.shape = NA) +
  geom_point(aes(fill = group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.6)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("VO2max")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  geom_signif(annotation = formatC("**", digits = 1), y_position = 55, xmin = 1, xmax = 2, tip_length = c(0.01, 0.01))+ 
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60))+
  ylab("VO2 (ml.min-1.kg-1")
fig_vo2max



#Panel B - Peak power output
stat_vo2 <- t.test(peakpoweroutput ~ group, data = PASC)
stat_vo2 # p=0.001342*
#sensitivity
stat_vo2 <- wilcox.test(peakpoweroutput ~ group, data = PASC)
stat_vo2 # p=0.001491*

fig_peakpower<- ggplot(data  = PASC, aes(x = group, y = peakpoweroutput)) +
  geom_boxplot(aes(fill = group), width = 0.6, position = position_dodge(width = 1), outlier.shape = NA) +
  geom_point(aes(fill = group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.6)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("VO2max")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  geom_signif(annotation = formatC("*", digits = 1), y_position = 380, xmin = 1, xmax = 2, tip_length = c(0.01, 0.01))+ 
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 420))+
  ylab("Peak power output")
fig_peakpower

#Panel C - Gas exchange treshold (GET)
stat_vo2 <- t.test(GET ~ group, data = PASC)
stat_vo2 # p=0.0139 *
#sensitivity
stat_vo2 <- wilcox.test(GET ~ group, data = PASC)
stat_vo2 # p=0.01499 *


get <- ggplot(data  = PASC, aes(x = group, y = GET)) +
  geom_boxplot(aes(fill = group), width = 0.6, position = position_dodge(width = 0.6), outlier.shape = NA) +
  geom_point(aes(fill = group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.6)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("Gas exchange treshold")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  geom_signif(annotation = formatC("*", digits = 1), y_position = 2.9, xmin = 1, xmax = 2, tip_length = c(0.01, 0.01))+ 
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3.2))+
  ylab("GET (ml.min-1.kg-1")
get


#Panel D - Muscle deoxygenation
PASC_1D <- read_excel("Source data.xlsx", sheet = "Figure1_panel_D")
PASC_1D$Group <- as.factor(PASC_1D$Group)
PASC_1D$Time <- as.factor(PASC_1D$variable) 

PASC_1D_fig <-PASC_1D
PASC_1D <- filter(PASC_1D, PASC_1D$Time != "HHB 0%W")
PASC_1D <- filter(PASC_1D, PASC_1D$Time != "HHB 20%W")
PASC_1D$Time <- factor(PASC_1D$Time, levels =c("HHB 40%W", "HHB 60%W", "HHB 80%W", "HHB 100%W"))


model <-lme(value ~ Time*Group, data=PASC_1D, random = ~ Time|ID, na.action = na.omit, control="optim")
Anova(model, type="III")
#no interaction, use type II
Anova(model, type="II")


#figure
PASC_1D_fig$Time <- factor(PASC_1D_fig$Time, levels =c("HHB 0%W", "HHB 20%W", "HHB 40%W", "HHB 60%W", "HHB 80%W", "HHB 100%W"))

down_sd <- function(x) {
  data.frame(ymin = mean(x,na.rm=TRUE)-sd(x,na.rm=TRUE), # minimum
             ymax= mean(x,na.rm=TRUE))
}
up_sd <- function(x) {
  data.frame(ymin = mean(x, na.rm=TRUE), # minimum
             ymax= mean(x,na.rm=TRUE)+sd(x,na.rm = TRUE))}

PASC_1D_fig$variable <- as.factor(PASC_1D_fig$variable)

rel_power_graph<-ggplot(PASC_1D_fig, aes(x=Time, y=value, width=0.12, group=Group))+
  geom_vline(xintercept="HHB 20%W", linetype="dashed", size=1.5)+
  stat_summary(fun.y='mean',geom="line")+
  stat_summary(data=PASC_1D_fig[PASC_1D_fig$Group=="Long COVID",],fun.data = down_sd, geom="errorbar")+
  stat_summary(data=PASC_1D_fig[PASC_1D_fig$Group=="Healthy",],fun.data=up_sd,geom="errorbar")+
  stat_summary(fun.y='mean',geom="point", col='black',aes(fill=Group), shape=21,size=8)+
  scale_y_continuous(limits=c(0,101),breaks=c(0,25,50,75,100), expand=c(0,0))+
  scale_fill_manual(values=c("Healthy"="white", "Long COVID"="red"))+
  theme(legend.position = "none",
        #axis.line = element_line(colour="black", size = line_size),
        #axis.line=element_blank(),
        axis.line=element_line(colour="black"),
        panel.background = element_rect(fill="white", colour = "white"),
        panel.grid.major = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(angle = 90, vjust = 1),
        axis.title.x = element_text(angle = 0, vjust = -0.2))+
  scale_x_discrete(labels=c("0","20","40","60","80","100"))+
  xlab("Relative Work Rate (%)")+
  ylab("\u0394 Deoxy[heme] (%max)")
rel_power_graph
  



#panel E
  PASC_1E <- read_excel("Source data.xlsx", sheet = "Figure1_panel_E") 
  PASC_1E$Group <- as.factor(PASC_1E$Group)
  
  model2 <-lme(HHb ~ Power*Group, data=PASC_1E, random = ~ 1|ID, na.action = na.omit, control="optim")
  Anova(model2, type="III") #0.058 interaction term

  
  PASC_1E_fig<-  ggplot(data=  PASC_1E, aes(x=Power, y=HHb))+
    stat_summary(data=PASC_1E[PASC_1E$Group=="Healthy",],fun.y='mean',geom="line")+
    stat_summary(data=PASC_1E[PASC_1E$Group=="Long COVID",],fun.y='mean',geom="line")+
    stat_summary(data=PASC_1E[PASC_1E$Group=="Healthy",],fun.data=up_sd,geom="errorbar",width=4)+
    stat_summary(data=PASC_1E[PASC_1E$Group=="Long COVID",],fun.data = down_sd, geom="errorbar", width=4)+
    stat_summary(data=PASC_1E[PASC_1E$Group=="Healthy",] ,fun.y='mean',geom="point", col='black',fill="white",aes(fill="Healthy"), shape=21,size=8)+
    stat_summary(data=PASC_1E[PASC_1E$Group=="Long COVID",] ,fun.y='mean',geom="point", col='black', fill="red",aes(fill="Long COVID"), shape=21,size=8)+
    scale_colour_manual(labels=c("Long COVID"="Long COVID","Healthy"="Healthy"),values = c("Long COVID"="red","Healthy"="black"))+
    theme(legend.position = c(0.3,0.80),
          legend.direction = "horizontal",
          axis.line=element_line(colour="black"),
          panel.background = element_rect(fill="white", colour = "white"),
          panel.grid.major = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.title.y = element_text(angle = 90, vjust = 1),
          axis.title.x = element_text(angle = 0, vjust = -0.2))+
    scale_y_continuous(limits=c(0,101), breaks=c(0,25,50,75,100), expand=c(0,0))+
    scale_x_continuous(limits=c(40,200), breaks=c(0,50, 75,100, 125, 150, 175), expand=c(0,0))+
    xlab("Work Rate (W)")+
    ylab("\u0394 Deoxy[heme] (%max)")
  PASC_1E_fig
  
  
  
  
  
# Figure 2 ----------------------------------------------------------------
  #panel B
  capdensity <- read_excel("Source data.xlsx", sheet = "Figure2")
  
  p_norm <- sapply(capdensity[,c(3:4, 9)], shapiro.test)
  p_norm #cap-fiber and vo2 normal, Cap/mm2 not normal distributed. 
  
  #Statistical test
  wilcox.test(`Cap/mm2` ~ Group, data = capdensity) #0.11
  
  cap1<- ggplot(data  = capdensity, aes(x = Group, y = `Cap/mm2`)) +
    geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
    geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
    theme_bw() + 
    theme(aspect.ratio = 2/1) + 
    scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
    ggtitle("Capillary density")+
    geom_signif(annotation = formatC("ns", digits = 1), y_position = 600, xmin = 1, xmax = 2, tip_length = c(0.01, 0.01))+
    theme(plot.title = element_text(hjust = 0.5, size = 12))+
    theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
    theme(axis.line = element_line(color = 'black'))+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 700))+
    ylab("Number of capillaries/mm2")
  cap1
  
  
  
  #normal
  t.test(cap_fiber_ratio ~ Group, data = capdensity) #0.066
  
  #sensitivity
  wilcox.test(cap_fiber_ratio ~ Group, data = capdensity) #0.026
  
  
  cap2<- ggplot(data  = capdensity, aes(x = Group, y = cap_fiber_ratio)) +
    geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
    geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
    theme_bw() + 
    theme(aspect.ratio = 2/1) + 
    scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
    ggtitle("Capillary-fiber ratio")+
    geom_signif(annotation = formatC("ns", digits = 1), y_position = 3.5, xmin = 1, xmax = 2, tip_length = c(0.01, 0.01))+
    theme(plot.title = element_text(hjust = 0.5, size = 12))+
    theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
    theme(axis.line = element_line(color = 'black'))+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 4))+
    ylab("Capillary/fiber ratio")
  cap2
  

  #Figure C
  Fig_cap_vo2 <- ggscatter(capdensity, x = "VO2max", y = "cap_fiber_ratio",
                           color = "Group", palette = c("#00004d", "#FF0000"),
                           add = "reg.line", conf.int = F) + 
    stat_cor(aes(color = Group))
  Fig_cap_vo2
  
  
#Panel D -fiber type distribution by percentage of number of fibers
fiber <- read_excel("Source data.xlsx", sheet = "Figure2")

p_norm <- sapply(fiber[,c(9:12)], shapiro.test)
p_norm
#Type_IIx_and hybrids and total_hybrids not normally distributed
hist(fiber$perc_number_fibers_total_hybrids)
hist(fiber$`perc_number_fibers_Type_IIx_and_hybrids`)

fiberbox <- fiber
fiberbox[,9:12] <- apply(fiberbox[,9:12], 2, function(x) boxcoxT(x))
p_norm <- sapply(fiberbox[,c(9:12)], shapiro.test)
p_norm
#Type_IIx_and hybrids not normally distributed, total_hybrid are

t.test(fiber$perc_number_fibers_Type_I_and_hybrids ~ fiber$Group, paired = F, alternative = "two.sided") #p = 0.16
t.test(fiber$perc_number_fibers_Type_IIa    ~ fiber$Group, paired = F, alternative = "two.sided") #p = 0.25
wilcox.test(fiber$`perc_number_fibers_Type_IIx_and_hybrids` ~ fiber$Group, paired = F, alternative = "two.sided") #p = 0.03
t.test(fiberbox$perc_number_fibers_total_hybrids ~ fiberbox$Group, paired = F, alternative = "two.sided") #p = 0.003

#sensitivity
wilcox.test(fiber$perc_number_fibers_Type_I_and_hybrids ~ fiber$Group, paired = F, alternative = "two.sided") #p = 0.25
wilcox.test(fiber$perc_number_fibers_Type_IIa    ~ fiber$Group, paired = F, alternative = "two.sided") #p = 0.25
wilcox.test(fiber$`perc_number_fibers_Type_IIx_and_hybrids` ~ fiber$Group, paired = F, alternative = "two.sided") #p = 0.04
wilcox.test(fiberbox$perc_number_fibers_total_hybrids ~ fiberbox$Group, paired = F, alternative = "two.sided") #p = 0.005


fiber2 <- subset(fiber, select =c(ID, Group, perc_number_fibers_Type_I_and_hybrids,perc_number_fibers_Type_IIa, perc_number_fibers_Type_IIx_and_hybrids, perc_number_fibers_total_hybrids))
fiber3 <- reshape2::melt(fiber2, id.vars=c("ID", "Group"))
fiber3$Group <- as.factor(fiber3$Group)

fibperc_number<- ggplot(data  = fiber3, aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 4,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
  theme_bw() + 
  theme(aspect.ratio = 1/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  geom_signif(annotation = formatC("ns", digits = 1), y_position = 0.96, xmin = 0.82, xmax = 1.15, tip_length = c(0.01, 0.01))+
  geom_signif(annotation = formatC("ns", digits = 1), y_position = 0.96, xmin =1.82, xmax = 2.15, tip_length = c(0.01, 0.01))+
  geom_signif(annotation = formatC("*", digits = 1), y_position = 0.96, xmin = 2.82, xmax = 3.15, tip_length = c(0.01, 0.01))+
  geom_signif(annotation = formatC("*", digits = 1), y_position = 0.96, xmin = 3.82, xmax = 4.15, tip_length = c(0.01, 0.01))+
  ggtitle("Fiber type distribution")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1))+
  ylab("Percentage (%)")
fibperc_number



#Panel E - peak power output / weighted FCSA
PASC <- read_excel("Source data.xlsx", sheet = "Figure2")
Peakpower_FCSA <- ggscatter(PASC, x = "Weighted FCSA", y = "peakpoweroutput",
                     color = "Group", palette = c("#00004d", "#FF0000"),
                     add = "reg.line", conf.int = F) + 
  stat_cor(aes(color = Group))
Peakpower_FCSA


x <- PASC$`Weighted FCSA`
y <- PASC$peakpoweroutput
group <- PASC$Group

# Compute the correlations within each group
cor_within_group <- cor.test(x[group == "Healthy"], y[group == "Healthy"])
cor_between_group <- cor.test(x[group == "Long COVID"], y[group == "Long COVID"])

# Compare the correlations using cocor
comparison_result <- cocor.indep.groups(
  cor_within_group$estimate, 
  cor_between_group$estimate, 
  length(x[group == "Healthy"]), 
  length(x[group == "Long COVID"])
)

# Print the comparison result
print(comparison_result)


#calculate difference in intercept. 
model_group1 <- lm(y[group == "Healthy"] ~ x[group == "Healthy"])
model_group2 <- lm(y[group == "Long COVID"] ~ x[group == "Long COVID"])

# Perform ANCOVA to test for intercept difference
anova_result <- anova(model_group1, model_group2)

# Extract p-value from the ANCOVA result
anova_result$"Pr(>F)"[1]



#Panel F - Vo2max / SDH
PASC_VO2SDH <-  filter(PASC, !is.na(PASC$SDH))
PASC_VO2SDH <-  filter(PASC_VO2SDH, !is.na(PASC_VO2SDH$VO2max))
VO2_SDH <- ggscatter(PASC, x = "SDH", y = "VO2max",
                    color = "Group", palette = c("#00004d", "#FF0000"),
                    add = "reg.line", conf.int = F) + 
  stat_cor(aes(color = Group))
VO2_SDH

x <- PASC_VO2SDH$SDH
y <- PASC_VO2SDH$VO2max
group <- PASC_VO2SDH$Group

# Compute the correlations within each group
cor_within_group <- cor.test(x[group == "Healthy"], y[group == "Healthy"])
cor_between_group <- cor.test(x[group == "Long COVID"], y[group == "Long COVID"])

# Compare the correlations using cocor
comparison_result <- cocor.indep.groups(
  cor_within_group$estimate, 
  cor_between_group$estimate, 
  length(x[group == "Healthy"]), 
  length(x[group == "Long COVID"])
)

# Print the comparison result
print(comparison_result)


# Figure 3 ----------------------------------------------------------------
PASC <- read_excel("Source data.xlsx", sheet = "Figure3A+B")

#Set variables
PASC$Group <- as.factor(PASC$Group)
PASC$Time <- as.factor(PASC$Time)

#Check normality
p_norm <- sapply(PASC[,c(4:5)], shapiro.test)
p_norm
hist(PASC$OXPHOS) #OXPHOS just normally distributed
PASC$Time <- factor(PASC$Time, levels =c("Baseline", "1-day after PEM"))

#PANEL A
model <-lme(OXPHOS ~ Group*Time, data=PASC, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(model, type="III")
#no interaction
Anova(model, type="II")

oxphos<- ggplot(data  = PASC, aes(x = Time, y = OXPHOS)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  geom_signif(annotation = formatC("*", digits = 1), y_position = 145, xmin = 0.85, xmax = 1.15, tip_length = c(0.01, 0.01))+
  geom_signif(annotation = formatC("*", digits = 1), y_position = 153, xmin = 0.85, xmax = 1.85, tip_length = c(0.01, 0.01))+
  geom_signif(annotation = formatC("*", digits = 1), y_position = 163, xmin = 1.15, xmax = 2.15, tip_length = c(0.01, 0.01))+
  geom_signif(annotation = formatC("*", digits = 1), y_position = 145, xmin = 1.85, xmax = 2.15, tip_length = c(0.01, 0.01))+
  ggtitle("OXPHOS capacity")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 170))+
  ylab("Respiration \n pmol*s-1*mg-1")
oxphos


#PANEL B
model <-lme(SDH ~ Group*Time, data=PASC, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(model, type="III")
#interaction effect - no type II test
emmeans(model, list(pairwise ~ Group*Time), adjust = "BH")

SDH2<- ggplot(data  = PASC, aes(x = Time, y = SDH)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_discrete(name = "", labels = c("Healthy controls", "Long COVID"))+
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("Succinate dehydrogenase activity")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  geom_signif(annotation = formatC("*", digits = 1), y_position = 0.0000175, xmin = 1.15, xmax = 2.15, tip_length = c(0.01, 0.01))+ 
  geom_signif(annotation = formatC("*", digits = 1), y_position = 0.0000167, xmin = 1.85, xmax = 2.15, tip_length = c(0.01, 0.01))+ 
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.000020))+
  ylab("SDH activity \n (A660*um-1*s-1")
SDH2


#Panel D+E - 
#See extended figure 6 for all measurements. 

# Figure 4 --------------------------------------------------------
amyloid <- read_excel("Source data.xlsx", sheet = "Figure4")

#panel B
amyloid$Time <- factor(amyloid$Time, levels =c("Baseline", "1-day after PEM"))


#Check normality - not normally distributed
p_norm <- sapply(amyloid[,c(2)], shapiro.test)
p_norm


amyloidbox <- amyloid
amyloidbox[,2] <- apply(amyloidbox[,2], 2, function(x) boxcoxT(x))
p_norm <- sapply(amyloidbox[,c(2)], shapiro.test)
p_norm
hist(amyloidbox$Amyloid_deposit)
#normally distributed after boxcox transformation

model <-lme(Amyloid_deposit ~ Group*Time, data=amyloidbox, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(model, type="III")
#no interaction, use type II
Anova(model, type="II")

Fig_amyloid<- ggplot(data  = amyloid, aes(x = Time, y = Amyloid_deposit)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.6), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.6)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("Amyloid containing deposits")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  theme(legend.position = "none", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 140))+
  ylab("Number of amyloid deposits/mm2")
Fig_amyloid




# Figure 5 --------------------------------------------------------
#panel A 
pathology <- read_excel("Source data.xlsx", sheet = "Figure5")
pathology$Group <- as.factor(pathology$Group)
pathology$Time <- as.factor(pathology$Time)
pathology$Time <- factor(pathology$Time, levels =c("Baseline", "1-day after PEM"))
pathology$fibreatrophy <- as.factor(pathology$fibreatrophy)
pathology$necrosis <- as.factor(pathology$necrosis)
pathology$internalnuclei <- as.factor(pathology$internalnuclei)
pathology$regeneration <- as.factor(pathology$regeneration)

net = table(pathology$fibreatrophy, pathology$Group, pathology$Time) 
net 

prophitleft = prop.table(net[,,1],2); prophitleft # Col % on first sub-table
prophitright = prop.table(net[,,2],2); prophitright # Col % on second sub-table

handval = as.character(sort(unique(pathology$Time))); handval
spotval = as.character(sort(unique(pathology$Group))); spotval

PercentHits = rbind(prophitleft[2,],prophitright[2,]) * 100
PercentHits = round(PercentHits,2)
rownames(PercentHits) = handval; colnames(PercentHits) = spotval
PercentHits
contrasts(pathology$Time) = contr.sum(2); contrasts(pathology$Group) = contr.sum(2)

pathology$fibreatrophy <- as.factor(pathology$fibreatrophy)

naiveglm = glm(fibreatrophy ~ Group*Time, family=binomial, data = pathology)
Anova(naiveglm, type="III")
#no interaction, use type II
Anova(naiveglm, type="II")


#panel B
net = table(pathology$necrosis, pathology$Group, pathology$Time) 
net 

prophitleft = prop.table(net[,,1],2); prophitleft # Col % on first sub-table
prophitright = prop.table(net[,,2],2); prophitright # Col % on second sub-table

handval = as.character(sort(unique(pathology$Time))); handval
spotval = as.character(sort(unique(pathology$Group))); spotval

PercentHits = rbind(prophitleft[2,],prophitright[2,]) * 100
PercentHits = round(PercentHits,2)
rownames(PercentHits) = handval; colnames(PercentHits) = spotval
PercentHits
#contrasts(test2$Time) = contr.sum(2); contrasts(test2$group) = contr.sum(2)

naiveglm = glm(necrosis ~ Group*Time, family=binomial, data = pathology)
Anova(naiveglm, type="III")

#no interaction, use type II
Anova(naiveglm, type="II")


#panel C
net = table(pathology$internalnuclei, pathology$Group, pathology$Time) 
net 
prophitleft = prop.table(net[,,1],2); prophitleft # Col % on first sub-table
prophitright = prop.table(net[,,2],2); prophitright # Col % on second sub-table

handval = as.character(sort(unique(pathology$Time))); handval
spotval = as.character(sort(unique(pathology$Group))); spotval

PercentHits = rbind(prophitleft[2,],prophitright[2,]) * 100
PercentHits = round(PercentHits,2)
rownames(PercentHits) = handval; colnames(PercentHits) = spotval
PercentHits
contrasts(pathology$Time) = contr.sum(2); contrasts(pathology$Group) = contr.sum(2)

naiveglm = glm(internalnuclei ~ Group*Time, family=binomial, data = pathology)
Anova(naiveglm, type="III")
#no interaction, use type II
Anova(naiveglm, type="II")


#panel D
net = table(pathology$regeneration, pathology$Group, pathology$Time) 
net 

prophitleft = prop.table(net[,,1],2); prophitleft # Col % on first sub-table
prophitright = prop.table(net[,,2],2); prophitright # Col % on second sub-table

handval = as.character(sort(unique(pathology$Time))); handval
spotval = as.character(sort(unique(pathology$Group))); spotval

PercentHits = rbind(prophitleft[2,],prophitright[2,]) * 100
PercentHits = round(PercentHits,2)
rownames(PercentHits) = handval; colnames(PercentHits) = spotval
PercentHits
contrasts(pathology$Time) = contr.sum(2); contrasts(pathology$Group) = contr.sum(2)


naiveglm = glm(regeneration ~ Group*Time, family=binomial, data = pathology)
Anova(naiveglm, type="III")
#no interaction: 
Anova(naiveglm, type="II")

#panel E-G
immune <- read_excel("Source data.xlsx", sheet = "Figure5")
immune$Group <- as.factor(immune$Group)
immune$Time <- as.factor(immune$Time)
immune$CD3 <- as.factor(immune$CD3)
immune$CD68 <- as.factor(immune$CD68)
immune$CD20 <- as.factor(immune$CD20)
immune$Time <- factor(immune$Time, levels =c("Baseline", "1-day after PEM"))

#panel E
net = table(immune$CD3, immune$Group, immune$Time) 
net 

prophitleft = prop.table(net[,,1],2); prophitleft # Col % on first sub-table
prophitright = prop.table(net[,,2],2); prophitright # Col % on second sub-table

handval = as.character(sort(unique(immune$Time))); handval
spotval = as.character(sort(unique(immune$Group))); spotval

PercentHits = rbind(prophitleft[2,],prophitright[2,]) * 100
PercentHits = round(PercentHits,2)
rownames(PercentHits) = handval; colnames(PercentHits) = spotval
PercentHits #hier percentages
contrasts(immune$Time) = contr.sum(2); contrasts(immune$Group) = contr.sum(2) 

naiveglm = glm(CD3 ~ Group*Time, family=binomial, data = immune)
Anova(naiveglm, type="III") 

n <- emmeans(naiveglm, list(pairwise ~ Group*Time), adjust = "BH")
pairs(n, simple = "each")


# CD68 = panel F 
net = table(immune$CD68, immune$Group, immune$Time) 
net 

prophitleft = prop.table(net[,,1],2); prophitleft # Col % on first sub-table
prophitright = prop.table(net[,,2],2); prophitright # Col % on second sub-table

handval = as.character(sort(unique(immune$Time))); handval
spotval = as.character(sort(unique(immune$Group))); spotval

PercentHits = rbind(prophitleft[2,],prophitright[2,]) * 100
PercentHits = round(PercentHits,2)
rownames(PercentHits) = handval; colnames(PercentHits) = spotval
PercentHits #hier percentages
contrasts(immune$Time) = contr.sum(2); contrasts(immune$Group) = contr.sum(2) 

naiveglm = glm(CD68 ~ Group*Time, family=binomial, data = immune)
Anova(naiveglm, type="III")

#No interaction: 
Anova(naiveglm, type="II")


# CD20- panel G
net = table(immune$CD20, immune$Group, immune$Time) 
net 

prophitleft = prop.table(net[,,1],2); prophitleft # Col % on first sub-table
prophitright = prop.table(net[,,2],2); prophitright # Col % on second sub-table

handval = as.character(sort(unique(immune$Time))); handval
spotval = as.character(sort(unique(immune$Group))); spotval

PercentHits = rbind(prophitleft[2,],prophitright[2,]) * 100
PercentHits = round(PercentHits,2)
rownames(PercentHits) = handval; colnames(PercentHits) = spotval
PercentHits #hier percentages
contrasts(immune$Time) = contr.sum(2); contrasts(immune$Group) = contr.sum(2) 

naiveglm = glm(CD20 ~ Group*Time, family=binomial, data = immune)
Anova(naiveglm, type="III")

#no interaction
Anova(naiveglm, type="II")





# Figure 6 ----------------------------------------------------------------
viral <- read_excel("Source data.xlsx", sheet = "Figure 6")
p_norm <- sapply(viral[, (4) ], shapiro.test)
p_norm #not normally distributed

viralbox <- viral
viralbox[,4] <- apply(viralbox[,4], 2, function(x) boxcoxT(x))
p_norm <- sapply(viralbox[,c(4)], shapiro.test)
p_norm #normally distributed
hist(viralbox$`Nucleocapsid_protein`)



viral2 <-lme(Nucleocapsid_protein ~ Group*Time, data=viralbox, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(viral2, type="III")
#no interaction: 
Anova(viral2, type="II")

#corrected for time since infection 
viral3 <-lme(Nucleocapsid_protein ~ Group*Time+Days_since_latest_infection, data=viralbox, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(viral3, type="III")
#no interaction: 
Anova(viral3, type="II")

viral$Time <- factor(viral$Time, levels =c("Baseline", "1-day after PEM"))
cov<- ggplot(data  = viral, aes(x = Time, y = Nucleocapsid_protein)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+  
  scale_y_continuous(expand = c(0, 0), limits = c(0, 260))+
  ylab("SARS-CoV-2 nucleocapsid protein \n abundance/mm2")
cov


# Extended figure 2 --------------------------------------------------------
#Panel A
severity <- read_excel("Source data.xlsx", sheet = "Extended Data Figure 2AB")

p_norm <- sapply(severity[,c(4)], shapiro.test)
p_norm #no normal distribution

severitybox <- severity
severitybox[,4] <- apply(severitybox[,4], 2, function(x) boxcoxT(x))
p_norm <- sapply(severitybox[,c(4)], shapiro.test)
p_norm #Transformation does not make it normal -> use delta. 

severity$FSS <- NULL
reshape2::dcast(data = severity,formula = ID+Group~Time, value.var = "MFI") -> MFI_wide

MFI_wide$delta_base_week <- MFI_wide$`1-week after PEM` - MFI_wide$Baseline
MFI_wide$delta_base_day <- MFI_wide$`1-day after PEM` - MFI_wide$Baseline

shapiro.test(MFI_wide$delta_base_week) #0.003 #not normal, use wilcox
wilcox.test(MFI_wide$delta_base_week ~ MFI_wide$Group, paired = F, alternative = "two.sided") #p<0.0001 ** interaction is present

shapiro.test(MFI_wide$delta_base_day) #0.64 #normal, use T-test
t.test(MFI_wide$delta_base_day ~ MFI_wide$Group, paired = F, alternative = "two.sided") #p<0.002 * interaction is present

severity$Time <- factor(severity$Time, levels =c("Baseline", "1-day after PEM", "1-week after PEM"))
Fig_MFI<- ggplot(data  = severity, aes(x = Time, y = MFI)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  geom_signif(annotation = formatC("**", digits = 1), y_position = 110, xmin = 0.85, xmax = 1.15, tip_length = c(0.01, 0.01))+
  geom_signif(annotation = formatC("**", digits = 1), y_position = 110, xmin = 1.85, xmax = 2.15, tip_length = c(0.01, 0.01))+
  geom_signif(annotation = formatC("**", digits = 1), y_position = 110, xmin = 2.85, xmax = 3.15, tip_length = c(0.01, 0.01))+ 
  ggtitle("Multidimensional Fatigue Inventory")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 135))+
  ylab("MFI fatigue score")
Fig_MFI

severity$uni <- paste(severity$Group, severity$Time)

kruskal.test(MFI ~ uni, data = severity)
pairwise.wilcox.test(severity$MFI, severity$uni, p.adjust.method = "BH", paired = F)




#Panel B 
severity <- read_excel("Source data.xlsx", sheet = "Extended Data Figure 2AB")

p_norm <- sapply(severity[,c(5)], shapiro.test)
p_norm #no normal distribution

severitybox <- severity
severitybox[,5] <- apply(severitybox[,5], 2, function(x) boxcoxT(x))
p_norm <- sapply(severitybox[,c(5)], shapiro.test)
p_norm #Transformation does not make it normal -> use delta. 

severity$MFI <- NULL
severity <- filter(severity, severity$Time != "1-day after PEM")

reshape2::dcast(data = severity,formula = ID+Group~Time, value.var = "FSS") -> FSS_wide

FSS_wide$delta <- FSS_wide$`1-week after PEM` - FSS_wide$Baseline
shapiro.test(FSS_wide$delta)
#not normal, use wilcox
wilcox.test(FSS_wide$delta ~ FSS_wide$Group, paired = F, alternative = "two.sided") #p = 0.08 = no interaction

fss<- ggplot(data  = severity, aes(x = Time, y = FSS)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  geom_signif(annotation = formatC("**", digits = 1), y_position = 8, xmin = 0.85, xmax = 1.15, tip_length = c(0.01, 0.01))+
  geom_signif(annotation = formatC("**", digits = 1), y_position = 8, xmin = 1.85, xmax = 2.15, tip_length = c(0.01, 0.01))+
  ggtitle("Fatigue Severity Scale")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10))+
  ylab("FSS fatigue score")
fss

severity$uni <- paste(severity$Group, severity$Time)

kruskal.test(FSS ~ uni, data = severity)
pairwise.wilcox.test(severity$FSS, severity$uni, p.adjust.method = "BH", paired = F)


#Panel C
steps <- read_excel("Source data.xlsx", sheet = "Extended Data Figure2C")

p_norm <- sapply(steps[,c(3)], shapiro.test)
p_norm #normal distribution

stat_steps <-lme(Steps ~ Group*Time, data=steps, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(stat_steps, type="III")

#no interaction, use type II
Anova(stat_steps, type="II")
emmeans(model, list(pairwise ~ Group*Time), adjust = "BH")


step2<- ggplot(data  = steps, aes(x = Time, y = Steps)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.6), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.6)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("Average steps per day")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 16000))+
  ylab("Number of steps")
step2





# Extended figure 3 --------------------------------------------------------
extfig3 <- read_excel("Source data.xlsx", sheet = "Extended Data Figure 3")

#Panel A -maximal ventilation
p_norm <- sapply(extfig3[,c(3)], shapiro.test)
p_norm #non normal distribution
wilcox.test(extfig3$Maxventilation ~ extfig3$Group, paired = F, alternative = "two.sided") #p = 0.03 * 

maxventilation<- ggplot(data  = extfig3, aes(x = Group, y = Maxventilation)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.6), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.6)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_discrete(name = "", labels = c("Healthy controls", "Long COVID"))+
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("Maximal ventilation")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  geom_signif(annotation = formatC("*", digits = 1), y_position = 230, xmin = 1, xmax = 2, tip_length = c(0.01, 0.01))+ 
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 250))+
  ylab("VE (L.min-1")
maxventilation


#panel B
p_norm <- sapply(extfig3[,c(4)], shapiro.test)
p_norm #non normal distribution
t.test(extfig3$petco2 ~ extfig3$Group, paired = F, alternative = "two.sided") #p = 0.005 * 

#sensitivity
wilcox.test(extfig3$petco2 ~ extfig3$Group, paired = F, alternative = "two.sided") #p = 0.008 * 

#PETCO2
PETCO2 <- ggplot(data  = extfig3, aes(x = Group, y = petco2)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.6), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.6)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_discrete(name = "", labels = c("Healthy controls", "Long COVID"))+
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("PETCO2")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  geom_signif(annotation = formatC("*", digits = 1), y_position = 52, xmin = 1, xmax = 2, tip_length = c(0.01, 0.01))+ 
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(30, 55))+
  ylab("PETCO2 (mmHg)")
PETCO2

#panel C
p_norm <- sapply(extfig3[,c(5)], shapiro.test)
p_norm #normal distribution
t.test(extfig3$vevco2slope ~ extfig3$Group, paired = F, alternative = "two.sided") #p = 0.004 * 

#sensitivity
wilcox.test(extfig3$vevco2slope ~ extfig3$Group, paired = F, alternative = "two.sided") #p = 0.01193

vevco2slope<- ggplot(data  = extfig3, aes(x = Group, y = vevco2slope)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.6), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.6)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("Ve/VCO2 slope")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  geom_signif(annotation = formatC("*", digits = 1), y_position = 45, xmin = 1, xmax = 2, tip_length = c(0.01, 0.01))+ 
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  ylim(10,50)+
  ylab("VE.VCO2^-1 slope")
vevco2slope

#panel D 
p_norm <- sapply(extfig3[,c(6)], shapiro.test)
p_norm #normal distribution
t.test(extfig3$vo2hrslope ~ extfig3$Group, paired = F, alternative = "two.sided") #p = 0.06 

#sensitivity
wilcox.test(extfig3$vo2hrslope ~ extfig3$Group, paired = F, alternative = "two.sided") #p = 0.089 


VO2HR <- ggplot(data  = extfig3, aes(x = Group, y = vo2hrslope)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.6), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.6)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("VO2/heart rate slope")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  geom_signif(annotation = formatC("ns", digits = 1), y_position = 95, xmin = 1, xmax = 2, tip_length = c(0.01, 0.01))+ 
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,110))+
ylab("VO2.HR-1 slope")
VO2HR


#panel E
p_norm <- sapply(extfig3[,c(7)], shapiro.test)
p_norm #normal distribution
t.test(extfig3$HRpeak ~ extfig3$Group, paired = F, alternative = "two.sided") #p = 0.47 

#sensitivity
wilcox.test(extfig3$HRpeak ~ extfig3$Group, paired = F, alternative = "two.sided") #p = 0.61


maxhr <- ggplot(data  = extfig3, aes(x = Group, y = HRpeak)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.6), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.6)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("Maximal heart rate")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  geom_signif(annotation = formatC("ns", digits = 1), y_position = 210, xmin = 1, xmax = 2, tip_length = c(0.01, 0.01))+ 
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(120,220))+
  ylab("Heart rate (beats.min-1)")
maxhr


#panel F
p_norm <- sapply(extfig3[,c(8)], shapiro.test)
p_norm #normal distribution
t.test(extfig3$O2pulseMax ~ extfig3$Group, paired = F, alternative = "two.sided") #p = 0.0028 *

#sensitivity
wilcox.test(extfig3$O2pulseMax ~ extfig3$Group, paired = F, alternative = "two.sided") #p = 0.0047*

maxo2pulse <- ggplot(data  = extfig3, aes(x = Group, y = O2pulseMax)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.6), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.6)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("PEAK O2 pulse")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  geom_signif(annotation = formatC("*", digits = 1), y_position = 23, xmin = 1, xmax = 2, tip_length = c(0.01, 0.01))+ 
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,25))+
  ylab("02 pulse (ml.beat-1)")
maxo2pulse


#panel G 
p_norm <- sapply(extfig3[,c(9)], shapiro.test)
p_norm #normal distribution
t.test(extfig3$RER ~ extfig3$Group, paired = F, alternative = "two.sided") #p = 0.047

#sensitivity
wilcox.test(extfig3$RER ~ extfig3$Group, paired = F, alternative = "two.sided") #p = 0.09

RER_fig <- ggplot(data  = extfig3, aes(x = Group, y = RER)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.6), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 4,  alpha = 0.7, shape = 21, position = position_dodge(width=0.6)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("Respiratory Exchange Ratio")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  geom_signif(annotation = formatC("*", digits = 1), y_position = 1.45, xmin = 1, xmax = 2, tip_length = c(0.01, 0.01))+ 
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0.75,1.5))+
  ylab("Respiratory Exchange Ratio")
RER_fig


#panel H
extfig3_lactate <- subset(extfig3, select =c(ID, Group, lactate_pre, lactate_post))

extfig3_lactate_long <- reshape2::melt(extfig3_lactate, id.vars=c("ID", "Group"))
model <-lme(value ~ Group*variable, data=extfig3_lactate_long, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(model, type="III")
#no interaction, use type II
Anova(model, type="II")


lactate<- ggplot(data  = extfig3_lactate_long, aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_discrete(name = "", labels = c("Healthy controls", "Long COVID"))+
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  geom_signif(annotation = formatC("**", digits = 1), y_position = 23, xmin =0.82, xmax = 1.85, tip_length = c(0.01, 0.01))+
  geom_signif(annotation = formatC("**", digits = 1), y_position = 25, xmin = 1.15, xmax = 2.15, tip_length = c(0.01, 0.01))+
  ggtitle("Capillary lactate")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 30))+
  ylab("Lactate concentration (mmol/L)")
lactate


# Extended figure 4 --------------------------------------------------------
extfig4 <- read_excel("Source data.xlsx", sheet = "Extended Data Figure 4")

#FCSA

extfig4 <- read_excel("Source data.xlsx", sheet = "Extended Data Figure 4")
p_norm <- sapply(extfig4[,c(4:6)], shapiro.test)
p_norm #normal distribution

extfig4.2 <- reshape2::melt(extfig4, id.vars=c("ID", "Group", "sex"))
fibmale<- ggplot(data  = extfig4.2, aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  geom_signif(annotation = formatC("*", digits = 1), y_position = 9000, xmin =0.82, xmax = 1.15, tip_length = c(0.01, 0.01))+
  geom_signif(annotation = formatC("ns", digits = 1), y_position = 9000, xmin = 1.82, xmax = 2.15, tip_length = c(0.01, 0.01))+
  geom_signif(annotation = formatC("ns", digits = 1), y_position = 9000, xmin = 2.82, xmax = 3.15, tip_length = c(0.01, 0.01))+
  ggtitle("Fiber cross sectional data - AREA um2")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10000))+
  ylab("Area (um2")
fibmale


#normal 
t.test(extfig4$CSA1 ~ extfig4$Group, paired = F, alternative = "two.sided") #p = 0.09 *
t.test(extfig4$CSA2a ~ extfig4$Group, paired = F, alternative = "two.sided") #p = 0.69
t.test(extfig4$CSA2x ~ extfig4$Group, paired = F, alternative = "two.sided") #p = 0.72



# Males FCSA 

extfig4 <- read_excel("Source data.xlsx", sheet = "Extended Data Figure 4")
extfig4 <- filter(extfig4, extfig4$sex == "Male")
p_norm <- sapply(extfig4[,c(4:6)], shapiro.test)
p_norm #normal distribution

extfig4_male <- reshape2::melt(extfig4, id.vars=c("ID", "Group", "sex"))
fibmale<- ggplot(data  = extfig4_male, aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  geom_signif(annotation = formatC("ns", digits = 1), y_position = 9000, xmin =0.82, xmax = 1.15, tip_length = c(0.01, 0.01))+
  geom_signif(annotation = formatC("ns", digits = 1), y_position = 9000, xmin = 1.82, xmax = 2.15, tip_length = c(0.01, 0.01))+
  geom_signif(annotation = formatC("ns", digits = 1), y_position = 9000, xmin = 2.82, xmax = 3.15, tip_length = c(0.01, 0.01))+
  ggtitle("Fiber cross sectional data - male - AREA um2")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10000))+
    ylab("Area (um2")
fibmale

t.test(extfig4$CSA1 ~ extfig4$Group, paired = F, alternative = "two.sided") #p = 0.72
t.test(extfig4$CSA2a ~ extfig4$Group, paired = F, alternative = "two.sided") #p = 0.12
t.test(extfig4$CSA2x ~ extfig4$Group, paired = F, alternative = "two.sided") #p = 0.33

# Females FCSA 

extfig4 <- read_excel("Source data.xlsx", sheet = "Extended Data Figure 4")

extfig4 <- filter(extfig4, extfig4$sex == "Female")
p_norm <- sapply(extfig4[,c(4:6)], shapiro.test)
p_norm #normal distribution except CSA2x

extfig4_female <- reshape2::melt(extfig4, id.vars=c("ID", "Group", "sex"))
fibmale<- ggplot(data  = extfig4_female, aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  geom_signif(annotation = formatC("*", digits = 1), y_position = 9000, xmin =0.82, xmax = 1.15, tip_length = c(0.01, 0.01))+
  geom_signif(annotation = formatC("ns", digits = 1), y_position = 9000, xmin = 1.82, xmax = 2.15, tip_length = c(0.01, 0.01))+
  geom_signif(annotation = formatC("ns", digits = 1), y_position = 9000, xmin = 2.82, xmax = 3.15, tip_length = c(0.01, 0.01))+
  ggtitle("Fiber cross sectional data - female - AREA um2")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10000))+
  ylab("Area (um2")
fibmale


#normal 
t.test(extfig4$CSA1 ~ extfig4$Group, paired = F, alternative = "two.sided") #p = 0.02 *
t.test(extfig4$CSA2a ~ extfig4$Group, paired = F, alternative = "two.sided") #p = 0.43
wilcox.test(extfig4$CSA2x ~ extfig4$Group, paired = F, alternative = "two.sided") #p = 0.44


#Panel G -fiber type distribution by percentage area occupied
fiber <- read_excel("Source data.xlsx", sheet = "Figure2")
p_norm <- sapply(fiber[,c(13:15)], shapiro.test)
p_norm
#perc_area_Type_IIx  not normally distributed
hist(fiber$perc_area_Type_IIx_typeIIa_IIx)


fiberbox <- fiber
fiberbox[,13:15] <- apply(fiberbox[,13:15], 2, function(x) boxcoxT(x))
p_norm <- sapply(fiberbox[,c(13:15)], shapiro.test)
p_norm
#perc_area_Type_IIx_typeIIa_IIx normal



t.test(fiber$perc_area_Type_I_type_I_IIa_hybrids  ~ fiber$Group, paired = F, alternative = "two.sided") #p = 0.05
t.test(fiber$perc_area_Type_IIa_type_I_IIa_IIa_IIx     ~ fiber$Group, paired = F, alternative = "two.sided") #p = 0.95
t.test(fiberbox$perc_area_Type_IIx_typeIIa_IIx ~ fiberbox$Group, paired = F, alternative = "two.sided") #p = 0.013

#sensitivity
wilcox.test(fiber$perc_area_Type_I_type_I_IIa_hybrids  ~ fiber$Group, paired = F, alternative = "two.sided") #p = 0.03
wilcox.test(fiber$perc_area_Type_IIa_type_I_IIa_IIa_IIx     ~ fiber$Group, paired = F, alternative = "two.sided") #p = 0.88
wilcox.test(fiberbox$perc_area_Type_IIx_typeIIa_IIx ~ fiberbox$Group, paired = F, alternative = "two.sided") #p = 0.02

fiber2 <- subset(fiber, select =c(ID, Group, perc_area_Type_I_type_I_IIa_hybrids,perc_area_Type_IIa_type_I_IIa_IIa_IIx, perc_area_Type_IIx_typeIIa_IIx))
fiber3 <- reshape2::melt(fiber2, id.vars=c("ID", "Group"))
fiber3$Group <- as.factor(fiber3$Group)

fibperc_area<- ggplot(data  = fiber3, aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 4,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
  theme_bw() + 
  theme(aspect.ratio = 1/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  geom_signif(annotation = formatC("p=0.05", digits = 1), y_position = 0.9, xmin = 0.82, xmax = 1.16, tip_length = c(0.01, 0.01))+
  geom_signif(annotation = formatC("ns", digits = 1), y_position = 0.96, xmin =1.82, xmax = 2.15, tip_length = c(0.01, 0.01))+
  geom_signif(annotation = formatC("*", digits = 1), y_position = 0.96, xmin = 2.82, xmax = 3.15, tip_length = c(0.01, 0.01))+
  ggtitle("Fiber type distribution")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1))+
  ylab("Percentage (%)")
fibperc_area





# Extended figure 5 --------------------------------------------------------
extfig5 <- read_excel("Source data.xlsx", sheet = "Extended Data Figure 5")

#Set variables
extfig5$Group <- as.factor(extfig5$Group)
extfig5$Time <- as.factor(extfig5$Time)
extfig5$Time <- factor(extfig5$Time, levels =c("Baseline", "1-day after PEM"))
extfig5$OXPHOS_ETS_ratio <- extfig5$OXPHOS/extfig5$ETS

#Check normality
p_norm <- sapply(extfig5[,c(4:13)], shapiro.test)
p_norm
#normally distributed: ETS, S-linked_complex-2, OXPHOS.

extfig5_box <- extfig5
extfig5_box[,4:13] <- apply(extfig5_box[,4:13], 2, function(x) boxcoxT(x))
p_norm <- sapply(extfig5_box[,c(4:13)], shapiro.test)
p_norm #all normal. Biochemical coupling effiency is borderline 0.049
hist(extfig5_box$Coupling_efficiency) #look OK, use boxcox transformation.


#PANEL A
model <-lme(LEAK ~ Group*Time, data=extfig5_box, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(model, type="III")
#no interaction, use Type II
Anova(model, type="II")


#LEAK
fig_leak <- ggplot(data  = extfig5, aes(x = Time, y = LEAK)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.6), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.6)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("LEAK")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40))+
  ylab("Respiration \n pmol*s-1*mg-1")
fig_leak


#panel B
model <-lme(NAD_linked_complex_1 ~ Group*Time, data=extfig5_box, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(model, type="III")
#no interaction, use Type II
Anova(model, type="II")


#
fig_NAD_linked_complex_1<- ggplot(data  = extfig5, aes(x = Time, y = NAD_linked_complex_1)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.6), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.6)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("NADH-linked complex I")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 110))+
  ylab("Respiration \n pmol*s-1*mg-1")
fig_NAD_linked_complex_1


#panel C
model <-lme(ETS ~ Group*Time, data=extfig5, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(model, type="III")
#no interaction, use Type II
Anova(model, type="II")


#
fig_ETS <- ggplot(data  = extfig5, aes(x = Time, y = ETS)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.6), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.6)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("ETS capacity")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 210))+
  ylab("Respiration \n pmol*s-1*mg-1")
fig_ETS


#panel D
model <-lme(s_linked_complex_2 ~ Group*Time, data=extfig5, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(model, type="III")
#no interaction, use Type II
Anova(model, type="II")


#
fig_s_linked_complex_2 <- ggplot(data  = extfig5, aes(x = Time, y = s_linked_complex_2)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.6), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.6)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("Succinate complex II")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 210))+
  ylab("Respiration \n pmol*s-1*mg-1")
fig_s_linked_complex_2


#panel E
model <-lme(OXPHOS_ETS_ratio ~ Group*Time, data=extfig5_box, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(model, type="III")
#no interaction, use Type II
Anova(model, type="II")


#
fig_OXPHOS_ETS_ratio <- ggplot(data  = extfig5, aes(x = Time, y = OXPHOS_ETS_ratio)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.6), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.6)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("OXPHOS/ETS_ratio")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5))+
  ylab("Arbitrary unit")
fig_OXPHOS_ETS_ratio


#panel F
model <-lme(OXPHOS_ETS_control_ratio ~ Group*Time, data=extfig5_box, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(model, type="III")
#no interaction, use Type II
Anova(model, type="II")


#
fig_OXPHOS_ETS_control_ratio <- ggplot(data  = extfig5, aes(x = Time, y = OXPHOS_ETS_control_ratio)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.6), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.6)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("OXPHOS:ETS control ratio")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.25))+
  ylab("Arbitrary unit")
fig_OXPHOS_ETS_control_ratio


#panel G
model <-lme(ETS_OXPHOS_control_efficiency ~ Group*Time, data=extfig5_box, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(model, type="III")
#no interaction, use Type II
Anova(model, type="II")


#
fig_ETS_OXPHOS_control_efficiency <- ggplot(data  = extfig5, aes(x = Time, y = ETS_OXPHOS_control_efficiency)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.6), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.6)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("ETS-OXPHOS control efficiency")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1))+
  ylab("Arbitrary unit")
fig_ETS_OXPHOS_control_efficiency

#panel H
model <-lme(Coupling_efficiency ~ Group*Time, data=extfig5_box, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(model, type="III")
#no interaction, use Type II
Anova(model, type="II")


#
fig_Coupling_efficiency <- ggplot(data  = extfig5, aes(x = Time, y = Coupling_efficiency)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.6), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.6)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("Biochemical coupling efficiency")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0.7, 1))+
  ylab("Arbitrary unit")
fig_Coupling_efficiency

#panel I
model <-lme(intsdh ~ Group*Time, data=extfig5_box, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(model, type="III")


n <- emmeans(model, list(pairwise ~ Group*Time), adjust = "BH")
pairs(n, simple = "each")

#
fig_intsdh <- ggplot(data  = extfig5, aes(x = Time, y = intsdh)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.6), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.6)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("Integrated SDH")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.16))+
  ylab("A660*um^1*s^-1")
fig_intsdh



# Extended figure 6 --------------------------------------------------------
#Panel A
#4 control participants were excluded in quality control
metabolomics <- read_excel("Source data.xlsx", sheet = "Extended Data Figure 6A")

metabolomics$Group <- as.factor(metabolomics$Group)
metabolomics$Time <- as.factor(metabolomics$Time)

#create dataframe for panel C
metabolomics_panelC <- metabolomics

p_norm <- sapply(metabolomics[,c(4:119)], shapiro.test)
p_norm

metabolomics_box <- metabolomics
metabolomics_box[,4:ncol(metabolomics_box)] <- apply(metabolomics_box[,4:ncol(metabolomics_box)], 2, function(x) boxcoxT(x))
metabolomics_box <- as.data.frame(metabolomics_box)
p_norm <- sapply(metabolomics_box[,c(4:119)], shapiro.test)

#create dataframe for heatmap later
metabolomics_box_heatmap <- metabolomics_box
#make names to be used in function
colnames(metabolomics_box) <- make.names(colnames(metabolomics_box))


mixed_function <- function(x){
  ctrl <- lmeControl(opt='optim')
  dat <- metabolomics_box
  print("pass")
  
  group <- anova(lme(as.formula( paste(x, "~ Time*Group")), data=dat, random = ~ 1|ID, control=ctrl, na.action = na.omit))[3,4]
  print("pass2")
  
  Time <- anova(lme(as.formula( paste(x, "~ Time*Group")), data=dat, random = ~ 1|ID, na.action = na.omit, control=ctrl))[2,4]
  print("pass3")
  
  int <- anova(lme(as.formula( paste(x, "~ Time*Group")), data=dat, random = ~ 1|ID, na.action = na.omit, control=ctrl))[4,4]
  print("pass4")
  
  vec <- c(group,Time,int)
  names(vec) <- c("Group","Time", "Interaction")
  return(vec )
}


#all
results_metabolomics <- sapply(names(metabolomics_box[ ,c(4:119)]), mixed_function)


#transpose
results_metabolomics <- as.data.frame(t(results_metabolomics))

#extra col name
results_metabolomics$Metabolite <- row.names(results_metabolomics)


#attach domain for multiple testing adjustment
domains <- read_excel("Source data.xlsx", sheet = "Ext Data Fig 6A - Domain Muscle")
domains$Metabolite <- make.names(domains$Metabolite)

results_metabolomics <- merge(results_metabolomics, domains, by = "Metabolite")

#convert to original names
domains2 <- read_excel("Source data.xlsx", sheet = "Ext Data Fig 6A - Domain Muscle")
domains2$Metabolite2 <- make.names(domains$Metabolite)
results_metabolomics <- merge(results_metabolomics, domains2, by.x = "Metabolite", by.y = "Metabolite2")

results_metabolomics$Domain.x <- NULL
results_metabolomics$Metabolite <- NULL
results_metabolomics <- results_metabolomics %>% dplyr::rename(Metabolite = Metabolite.y)
results_metabolomics <- results_metabolomics %>% dplyr::rename(Domain = Domain.y)



#BH adjustment within pathway
allvars <- c("Proline","Asparagine","Threonine", "Carnosine", "Methionine", "Histidine","Isoleucine","Betaine","Phenylalanine","Serine","Phosphoserine", "Leucine","Valine", "Alanine",
             "Tyrosine","Hippuric acid", "Taurine", "Glycine", "Hydroxyphenyllactic acid",
             "Lysine","Carnitine", "Carnitine (C10:0)", "Carnitine (C8:0)", "Carnitine (C6:0)", "Glutarylcarnitine", "Carnitine (C4:0)", "Carnitine (C3:0)",
             "Carnitine (C10:1)","Aminoadipic acid","Carnitine (C2:0)", "Methylmalonylcarnitine",
             "Glutathione", "Homocystine", "Ophthalmic acid", "2-Aminobutyric acid", "Homoglutathione","Pyroglutamic acid",  "Oxiglutathione","CoA-Glutathione",
             "Fructose-1,6-diphosphate","Dihydroxyacetone-P","Glucose","2-phosphoglyceric acid", "Lactate", "Phosphoenolpyruvate",  "Hexose-P", "1,3-Diphosphoglyceric acid","UDP-hexose","Pyruvate",
             "Glycerol-2P", "Glycerophosphocholine","Glycerol-3P", "CDP-choline", "CDP-ethanolamine", "Phosphorylethanolamine", "Glycerophosphoinositol","Choline",  
             "ADP-Ribose", "Nicotinamide", "Tryptophan","NAD+", "NADH", "NADPH",  "NADP+",    
             "Creatine-P", "Creatine","Creatinine", "2-Hydroxybutyric acid","2-Hydroxyglutarate", 
             "Sedoheptulose-7P",  "Glycerate","Ribose-5P",
             "ADP", "GDP", "Adenylthiomethylpentose","Adenine","GMP","AMP", "IMP","GTP","FAICAR", "ATP", "Adenosine","Uric acid","AICAR", "Xanthine","Inosine", "Hypoxanthine","Guanosine",
             "CDP", "UDP-HexNac","CTP", "UDP", "Uridine", "UTP", "beta-Alanine", "Uracil",
             "S-adenosyl homocysteine", "S-adenosyl methionine",
             "Coenzyme_A", "Succinate", "Malate", "Pantothenic acid", "Acetyl-CoA", "Homocitrate", "Glutamine", "Glutamate",  "FAD","Alpha-Ketoglutarate", "Citric acid",
             "Argininosuccinate", "Citrulline","Arginine",  "Ornithine", "Acetylglutamic acid")

## arrange according to format
bh <- results_metabolomics %>%slice(match(allvars, Metabolite))

## split in domains
amin <- bh[1:20, ]
carn <- bh[21:32, ]
glut <- bh[33:40, ]
glyc <- bh[41:50, ]
lip <- bh[51:58, ]
nad <- bh[59:65, ]
creat <- bh[66:68, ]
ket <- bh[69:70, ]
pent <- bh[71:73, ]
pur <- bh[74:90, ]
pyr <- bh[91:98, ]
oth<- bh[99:100, ]
tca<- bh[101:111, ]
urea<- bh[112:116, ]

amin$adj.p <- p.adjust(amin$Group, method = "BH")
carn$adj.p <- p.adjust(carn$Group, method = "BH")
glut$adj.p <- p.adjust(glut$Group, method = "BH")
glyc$adj.p <- p.adjust(glyc$Group, method = "BH")
lip$adj.p <- p.adjust(lip$Group, method = "BH")
nad$adj.p <- p.adjust(nad$Group, method = "BH")
creat$adj.p <- p.adjust(creat$Group, method = "BH")
ket$adj.p <- p.adjust(ket$Group, method = "BH")
pent$adj.p <- p.adjust(pent$Group, method = "BH")
pur$adj.p <- p.adjust(pur$Group, method = "BH")
pyr$adj.p <- p.adjust(pyr$Group, method = "BH")
oth$adj.p <- p.adjust(oth$Group, method = "BH")
tca$adj.p <- p.adjust(tca$Group, method = "BH")
urea$adj.p <- p.adjust(urea$Group, method = "BH")

results_metabolomics <- rbind(amin, carn, glut, glyc, lip, nad, creat, ket, pent, pur,pyr, oth, tca,urea)
results_metabolomics <- results_metabolomics %>% dplyr::rename(Adjusted_group_p_value = adj.p)


#HEDGES G - PANEL A
metabolomics_box_long <- reshape2::melt(metabolomics_box_heatmap, id.vars=c("ID", "Group", "Time"))
#attach domain
domains <- read_excel("Source data.xlsx", sheet = "Ext Data Fig 6A - Domain Muscle")
metabolomics_box_long <- merge(metabolomics_box_long, domains, by.x = "variable", by.y = "Metabolite")
metabolomics_box_long$Domain <- as.factor(metabolomics_box_long$Domain)



Baseline <- filter(metabolomics_box_long,  metabolomics_box_long$Time == "Baseline")
one_day_PEM <- filter(metabolomics_box_long,  metabolomics_box_long$Time == "1-day after PEM")


Baseline$Group <- factor(Baseline$Group, ordered = F, 
                    levels = c("Long COVID", "Healthy"))
one_day_PEM$Group <- factor(one_day_PEM$Group, ordered = F, 
                            levels = c("Long COVID", "Healthy"))


effect_size_baseline <- Baseline  %>%
  group_by(Domain, variable) %>%
  cohens_d(value ~ Group, hedges.correction = T, var.equal = F) %>% as.data.frame() %>%
  mutate(effsize_2 = ifelse(effsize > -0.2 & effsize < 0.2, 0, effsize),
         effsize_cat = cut(effsize,
                           breaks = c(-Inf, -0.8, -0.5, -0.2, 0.2, 0.5, 0.8, Inf),
                           labels = c("Large decrease (<-0.8)", "Moderate decrease (<-0.5)", "Small decrease (<-0.2)", "Negligible difference", 
                                      "Small increase (>0.2)", "Moderate increase (>0.5)", "Large increase (>0.8)")) %>% as.factor())

effect_size_baseline$comparison <- "Baseline"


effect_size_one_day_PEM <- one_day_PEM  %>%
  group_by(Domain, variable) %>%
  cohens_d(value ~ Group, hedges.correction = T, var.equal = F) %>% as.data.frame() %>%
  mutate(effsize_2 = ifelse(effsize > -0.2 & effsize < 0.2, 0, effsize),
         effsize_cat = cut(effsize,
                           breaks = c(-Inf, -0.8, -0.5, -0.2, 0.2, 0.5, 0.8, Inf),
                           labels = c("Large decrease (<-0.8)", "Moderate decrease (<-0.5)", "Small decrease (<-0.2)", "Negligible difference", 
                                      "Small increase (>0.2)", "Moderate increase (>0.5)", "Large increase (>0.8)")) %>% as.factor())

effect_size_one_day_PEM$comparison <- "one_day_PEM"

effect_size <- rbind(effect_size_baseline, effect_size_one_day_PEM)


#make figure
figure <- effect_size %>% select(Domain, variable, effsize, comparison) %>%
  pivot_wider(names_from = "comparison", values_from = effsize)

## change colors based on results
input <- as.matrix(figure[ , c(-1, -2)])
input_save <- input
rownames(input_save) <- figure$variable
input[input >-0.2 & input <0.2] <- 0
rownames(input) <- figure$variable
col_fun = colorRamp2(breaks=c(-0.8, 0,0.8), colors= c("#2B4C9A", "white","#DE1D18" ))


## use mean value before setting 0.2 to 0
domains <- domains[!duplicated(domains$Metabolite), ]
input_save <- input_save %>% as.data.frame()
input_save$mean_hedge <- rowMeans(input_save)
input_save$variable <- row.names(input_save)
input_save<- merge(input_save, domains, by.x = "variable", by.y = "Metabolite")

input_save$domain_order <-  ifelse(input_save$Domain == "Aminoacid", 1, 
                                   ifelse(input_save$Domain == "Carnitine", 2,
                                          ifelse(input_save$Domain == "Creatine", 3,
                                                 ifelse(input_save$Domain == "Glycolysis", 4,
                                                        ifelse(input_save$Domain == "Glutathione", 5,
                                                               ifelse(input_save$Domain == "Ketone metabolism", 6,
                                                                      ifelse(input_save$Domain == "Lipid biosynthesis", 7,
                                                                             ifelse(input_save$Domain == "Pentose Phosphate", 8, 
                                                                                    ifelse(input_save$Domain ==  "Purines", 9, 
                                                                                           ifelse(input_save$Domain == "Pyrimidine", 10, 
                                                                                                  ifelse(input_save$Domain == "NAD", 11, 
                                                                                                         ifelse(input_save$Domain == "TCA", 12, 
                                                                                                                ifelse(input_save$Domain == "Tryptophan", 13,
                                                                                                                       ifelse(input_save$Domain == "Urea cycle", 14, 
                                                                                                                              ifelse(input_save$Domain == "Other", 15, 16)))))))))))))))

input_save <- input_save[order(input_save$domain_order, -abs(input_save$mean_hedge)), ]

order <- c("Proline","Asparagine","Threonine", "Carnosine", "Methionine", "Histidine","Isoleucine","Betaine","Phenylalanine","Serine","Phosphoserine", "Leucine","Valine", "Alanine",
           "Tyrosine","Aspartate","Hippuric acid", "Taurine", "Glycine", "Hydroxyphenyllactic acid",
           "Lysine","Carnitine", "Carnitine (C10:0)", "Carnitine (C8:0)", "Carnitine (C6:0)", "Glutarylcarnitine", "Carnitine (C4:0)", "Carnitine (C3:0)",
           "Carnitine (C10:1)","Aminoadipic acid","Carnitine (C2:0)", "Methylmalonylcarnitine",
           "Glutathione", "Homocystine", "Ophthalmic acid", "2-Aminobutyric acid", "Homoglutathione","Pyroglutamic acid",  "Oxiglutathione","CoA-Glutathione",
           "Fructose-1,6-diphosphate","Dihydroxyacetone-P","Glucose","2-phosphoglyceric acid", "Lactate", "Phosphoenolpyruvate",  "Hexose-P", "1,3-Diphosphoglyceric acid","UDP-hexose","Pyruvate",
           "Glycerol-2P", "Glycerophosphocholine","Glycerol-3P", "CDP-choline", "CDP-ethanolamine", "Phosphorylethanolamine", "Glycerophosphoinositol","Choline",  
           "ADP-Ribose", "Nicotinamide", "Tryptophan","NAD+", "NADH", "NADPH",  "NADP+",    
           "2-Hydroxybutyric acid", "Creatine-P", "Creatine","Creatinine", "2-Hydroxyglutarate", 
           "Sedoheptulose-7P",  "Glycerate","Ribose-5P",
           "ADP", "GDP", "Adenylthiomethylpentose","Adenine","GMP","AMP", "IMP","GTP","FAICAR", "ATP", "Adenosine","Uric acid","AICAR", "Xanthine","Inosine", "Hypoxanthine","Guanosine",
           "CDP", "UDP-HexNac","CTP", "UDP", "Uridine", "UTP", "beta-Alanine", "Uracil",
           "S-adenosyl homocysteine", "S-adenosyl methionine",
           "Coenzyme_A", "Succinate", "Malate", "Pantothenic acid", "Homocitrate", "Glutamine", "Acetyl-CoA", "Glutamate",  "FAD","Alpha-Ketoglutarate", "Citric acid",
           "Argininosuccinate", "Citrulline","Arginine",  "Ornithine", "Acetylglutamic acid")


Heatmap_metabolomics_muscle <- Heatmap(input, column_title = "Metabolomics_muscle", col = col_fun,
                   column_title_gp = gpar(size=12, fontface="bold"), show_column_names = T, column_names_rot = 0,
                   show_row_names = T, show_row_dend = F, show_column_dend = F, row_names_side = "right",
                   row_names_gp = gpar(fontsize = 6), cluster_rows = F, cluster_columns = F,
                   border=T,
                   row_order = order,
                   row_split = factor(figure$Domain, levels = c("Aminoacid", "Carnitine", "Creatine", "Glycolysis",  "Glutathione", "Ketone metabolism", "Lipid biosynthesis", "Pentose Phosphate", "Purines", "Pyrimidine", "NAD", "TCA", "Tryptophan",  "Urea cycle","Other")),
                   cluster_row_slices = F,
                   heatmap_legend_param = list(at = c(-0.8, -0.5, -0.2, 0, 0.2, 0.5, 0.8),
                                               labels = c("<-0.8", "<-0.5", "<-0.2", ">-0.2 & <0.2", ">0.2", ">0.5", ">0.8"),
                                               title = "Hedges' g", color_bar = "discrete", fontsize = 8),
                   row_title_rot = 0, row_title_gp = gpar(fontsize = 30, fontface="bold"),
                   column_names_gp = gpar(fontsize = 15, fontface="bold", hjust = 0.8),
                   rect_gp = gpar(col = "black", lwd = 0.5),
                   border_gp = gpar(col = "black", lwd = 1))
Heatmap_metabolomics_muscle




#Panel B

metabolomics <- read_excel("Source data.xlsx", sheet = "Extended Data Figure 6B")

metabolomics$Group <- as.factor(metabolomics$Group)
metabolomics$Time <- as.factor(metabolomics$Time)

#create dataframe for panel D
metabolomics_panelD <- metabolomics


p_norm <- sapply(metabolomics[,c(4:86)], shapiro.test)
p_norm

metabolomics_box <- metabolomics
metabolomics_box[,4:ncol(metabolomics_box)] <- apply(metabolomics_box[,4:ncol(metabolomics_box)], 2, function(x) boxcoxT(x))
metabolomics_box <- as.data.frame(metabolomics_box)
p_norm <- sapply(metabolomics_box[,c(4:86)], shapiro.test)
#create dataframe for heatmap later
metabolomics_box_heatmap <- metabolomics_box

#make names to be used in function
colnames(metabolomics_box) <- make.names(colnames(metabolomics_box))


mixed_function <- function(x){
  ctrl <- lmeControl(opt='optim')
  dat <- metabolomics_box
  print("pass")
  
  group <- anova(lme(as.formula( paste(x, "~ Time*Group")), data=dat, random = ~ 1|ID, control=ctrl, na.action = na.omit))[3,4]
  print("pass2")
  
  Time <- anova(lme(as.formula( paste(x, "~ Time*Group")), data=dat, random = ~ 1|ID, na.action = na.omit, control=ctrl))[2,4]
  print("pass3")
  
  int <- anova(lme(as.formula( paste(x, "~ Time*Group")), data=dat, random = ~ 1|ID, na.action = na.omit, control=ctrl))[4,4]
  print("pass4")
  
  vec <- c(group,Time,int)
  names(vec) <- c("Group","Time", "Interaction")
  return(vec )
}


#all
results_metabolomics <- sapply(names(metabolomics_box[ ,c(4:86)]), mixed_function)


#transpose
results_metabolomics <- as.data.frame(t(results_metabolomics))

#extra col name
results_metabolomics$Metabolite <- row.names(results_metabolomics)


#attach domain for multiple testing adjustment
domains <- read_excel("Source data.xlsx", sheet = "Ext Data Fig 6B - Domain blood")
domains$Metabolite <- make.names(domains$Metabolite)

results_metabolomics <- merge(results_metabolomics, domains, by = "Metabolite")

#convert to original names
domains2 <- read_excel("Source data.xlsx", sheet = "Ext Data Fig 6B - Domain blood")
domains2$Metabolite2 <- make.names(domains$Metabolite)
results_metabolomics <- merge(results_metabolomics, domains2, by.x = "Metabolite", by.y = "Metabolite2")

results_metabolomics$Domain.x <- NULL
results_metabolomics$Metabolite <- NULL
results_metabolomics <- results_metabolomics %>% dplyr::rename(Metabolite = Metabolite.y)
results_metabolomics <- results_metabolomics %>% dplyr::rename(Domain = Domain.y)





#BH adjustment within pathway
allvars <- c("Threonine", "Asparagine", "Proline", "Alanine", "Methionine", "Phenylalanine", "Isoleucine","Taurine",  "Aspartate",  "Valine", "Betaine", "Cystine", "Leucine", 
             "Tyrosine", "Hydroxyphenyllactic acid",  "Cysteine", "Histidine","Glycine", "Hippuric acid", 
             "Carnitine", "Lysine",  "Carnitine (C2:0)", "Carnitine (C4:0)", "Glutarylcarnitine","Carnitine (C5:0)","Carnitine (C10:0)",  "Carnitine (C8:0)",   "Carnitine (C3:0)",  "Carnitine (C10:1)", 
             "Creatinine", "Creatine",
             "Glutathione", "2-Aminobutyric acid", "Pyroglutamic acid", "Oxiglutathione", 
             "Phosphoenolpyruvate", "Dihydroxyacetone-P", "Glucose-6P", "Hexose-P", "Lactate", "Glucose", "2-phosphoglyceric acid", "2-Dehydrogluconate", "Gluconate", "Pyruvate", 
             "Phosphorylethanolamine", "Glycerophosphocholine", "2-PY", "ADP-Ribose",  
             "2-Hydroxybutyric acid", "2-Hydroxyglutarate", 
             "Gluconate-6P", "Glycerate", 
             "GDP","ADP", "AMP", "GMP", "ATP", "Xanthine", "IMP","Hypoxanthine", "Allantoin",  "Uric acid", "FAICAR", 
             "CMP", "UMP", "Uracil", "Uridine", 
             "TMP", "Citric acid", "Glutamine", "Alpha-Ketoglutarate", "Glutamate", "Malate", "Succinate", 
             "Kynurenine", "Tryptophan", "Aminoadipic acid", "Kynurenic acid", "Arginine", "Ornithine", "Acetylglutamic acid", "Citrulline")


bh <- results_metabolomics %>%slice(match(allvars, Metabolite))

## split in domains
amin <- bh[1:19, ]
carn <- bh[20:29, ]
creat <- bh[30:31, ]
glut <- bh[32:35, ]
glyc <- bh[36:45, ]
lip <- bh[46:47, ]
nad <- bh[48:49, ]
ket <- bh[50:51, ]
pent <- bh[52:53, ]
pur <- bh[54:64, ]
pyr <- bh[65:68, ]
tca<- bh[69:75, ]
tryp<- bh[76:79, ]
urea<- bh[80:83, ]


amin$adj.p <- p.adjust(amin$Group, method = "BH")
carn$adj.p <- p.adjust(carn$Group, method = "BH")
glut$adj.p <- p.adjust(glut$Group, method = "BH")
glyc$adj.p <- p.adjust(glyc$Group, method = "BH")
lip$adj.p <- p.adjust(lip$Group, method = "BH")
nad$adj.p <- p.adjust(nad$Group, method = "BH")
creat$adj.p <- p.adjust(creat$Group, method = "BH")
ket$adj.p <- p.adjust(ket$Group, method = "BH")
pent$adj.p <- p.adjust(pent$Group, method = "BH")
pur$adj.p <- p.adjust(pur$Group, method = "BH")
pyr$adj.p <- p.adjust(pyr$Group, method = "BH")
tryp$adj.p <- p.adjust(tryp$Group, method = "BH")
tca$adj.p <- p.adjust(tca$Group, method = "BH")
urea$adj.p <- p.adjust(urea$Group, method = "BH")

results_metabolomics <- rbind(amin, carn, glut, glyc, lip, nad, creat, ket, pent, pur,pyr, tryp, tca,urea)
results_metabolomics <- results_metabolomics %>% dplyr::rename(Adjusted_group_p_value = adj.p)




#HEDGES G - PANEL B
metabolomics_box_long <- reshape2::melt(metabolomics_box_heatmap, id.vars=c("ID", "Group", "Time"))
#attach domain
domains <- read_excel("Source data.xlsx", sheet = "Ext Data Fig 6B - Domain blood")
metabolomics_box_long <- merge(metabolomics_box_long, domains, by.x = "variable", by.y = "Metabolite")
metabolomics_box_long$Domain <- as.factor(metabolomics_box_long$Domain)



Baseline <- filter(metabolomics_box_long,  metabolomics_box_long$Time == "Baseline")
one_day_PEM <- filter(metabolomics_box_long,  metabolomics_box_long$Time == "1-day after PEM")
one_week_PEM <- filter(metabolomics_box_long,  metabolomics_box_long$Time == "1-week after PEM")

Baseline$Group <- factor(Baseline$Group, ordered = F, 
                         levels = c("Long COVID", "Healthy"))
one_day_PEM$Group <- factor(one_day_PEM$Group, ordered = F, 
                            levels = c("Long COVID", "Healthy"))
one_week_PEM$Group <- factor(one_week_PEM$Group, ordered = F, 
                            levels = c("Long COVID", "Healthy"))




effect_size_baseline <- Baseline  %>%
  group_by(Domain, variable) %>%
  cohens_d(value ~ Group, hedges.correction = T, var.equal = F) %>% as.data.frame() %>%
  mutate(effsize_2 = ifelse(effsize > -0.2 & effsize < 0.2, 0, effsize),
         effsize_cat = cut(effsize,
                           breaks = c(-Inf, -0.8, -0.5, -0.2, 0.2, 0.5, 0.8, Inf),
                           labels = c("Large decrease (<-0.8)", "Moderate decrease (<-0.5)", "Small decrease (<-0.2)", "Negligible difference", 
                                      "Small increase (>0.2)", "Moderate increase (>0.5)", "Large increase (>0.8)")) %>% as.factor())

effect_size_baseline$comparison <- "Baseline"


effect_size_one_day_PEM <- one_day_PEM  %>%
  group_by(Domain, variable) %>%
  cohens_d(value ~ Group, hedges.correction = T, var.equal = F) %>% as.data.frame() %>%
  mutate(effsize_2 = ifelse(effsize > -0.2 & effsize < 0.2, 0, effsize),
         effsize_cat = cut(effsize,
                           breaks = c(-Inf, -0.8, -0.5, -0.2, 0.2, 0.5, 0.8, Inf),
                           labels = c("Large decrease (<-0.8)", "Moderate decrease (<-0.5)", "Small decrease (<-0.2)", "Negligible difference", 
                                      "Small increase (>0.2)", "Moderate increase (>0.5)", "Large increase (>0.8)")) %>% as.factor())

effect_size_one_day_PEM$comparison <- "one_day_PEM"


effect_size_one_week_PEM <- one_week_PEM  %>%
  group_by(Domain, variable) %>%
  cohens_d(value ~ Group, hedges.correction = T, var.equal = F) %>% as.data.frame() %>%
  mutate(effsize_2 = ifelse(effsize > -0.2 & effsize < 0.2, 0, effsize),
         effsize_cat = cut(effsize,
                           breaks = c(-Inf, -0.8, -0.5, -0.2, 0.2, 0.5, 0.8, Inf),
                           labels = c("Large decrease (<-0.8)", "Moderate decrease (<-0.5)", "Small decrease (<-0.2)", "Negligible difference", 
                                      "Small increase (>0.2)", "Moderate increase (>0.5)", "Large increase (>0.8)")) %>% as.factor())

effect_size_one_week_PEM$comparison <- "one_week_PEM"
effect_size <- rbind(effect_size_baseline, effect_size_one_day_PEM, effect_size_one_week_PEM)


#make figure
figure <- effect_size %>% select(Domain, variable, effsize, comparison) %>%
  pivot_wider(names_from = "comparison", values_from = effsize)

## change colors based on results
input <- as.matrix(figure[ , c(-1, -2)])
input_save <- input
rownames(input_save) <- figure$variable
input[input >-0.2 & input <0.2] <- 0
rownames(input) <- figure$variable
col_fun = colorRamp2(breaks=c(-0.8, 0,0.8), colors= c("#2B4C9A", "white","#DE1D18" ))


## use mean value before setting 0.2 to 0
domains <- domains[!duplicated(domains$Metabolite), ]
input_save <- input_save %>% as.data.frame()
input_save$mean_hedge <- rowMeans(input_save)
input_save$variable <- row.names(input_save)
input_save<- merge(input_save, domains, by.x = "variable", by.y = "Metabolite")



input_save$domain_order <-  ifelse(input_save$Domain == "Aminoacid", 1, 
                                   ifelse(input_save$Domain == "Carnitine", 2,
                                          ifelse(input_save$Domain == "Creatine", 3,
                                                 ifelse(input_save$Domain == "Glycolysis", 4,
                                                        ifelse(input_save$Domain == "Glutathione", 5,
                                                               ifelse(input_save$Domain == "Ketone metabolism", 6,
                                                                      ifelse(input_save$Domain == "Lipid biosynthesis", 7,
                                                                             ifelse(input_save$Domain == "Pentose Phosphate", 8, 
                                                                                    ifelse(input_save$Domain ==  "Purines", 9, 
                                                                                           ifelse(input_save$Domain == "Pyrimidine", 10, 
                                                                                                  ifelse(input_save$Domain == "NAD", 11, 
                                                                                                         ifelse(input_save$Domain == "TCA", 12, 
                                                                                                                ifelse(input_save$Domain == "Tryptophan", 13,
                                                                                                                       ifelse(input_save$Domain == "Urea cycle", 14, 
                                                                                                                              ifelse(input_save$Domain == "Other", 15, 16)))))))))))))))
table(input_save$Domain)


table(input_save$domain_order)
input_save <- input_save[order(input_save$domain_order, -abs(input_save$mean_hedge)), ]

order <- c("Threonine", "Asparagine", "Proline", "Alanine", "Methionine", "Phenylalanine", "Isoleucine","Taurine",  "Aspartate",  "Valine", "Betaine", "Cystine", "Leucine", 
           "Tyrosine", "Hydroxyphenyllactic acid",  "Cysteine", "Histidine","Glycine", "Hippuric acid", 
           "Carnitine", "Lysine",  "Carnitine (C2:0)", "Carnitine (C4:0)", "Glutarylcarnitine","Carnitine (C5:0)","Carnitine (C10:0)",  "Carnitine (C8:0)",   "Carnitine (C3:0)",  "Carnitine (C10:1)", 
           "Creatinine", "Creatine",
           "Glutathione", "2-Aminobutyric acid", "Pyroglutamic acid", "Oxiglutathione", 
           "Phosphoenolpyruvate", "Dihydroxyacetone-P", "Glucose-6P", "Hexose-P", "Lactate", "Glucose", "2-phosphoglyceric acid", "2-Dehydrogluconate", "Gluconate", "Pyruvate", 
           "Phosphorylethanolamine", "Glycerophosphocholine", "2-PY", "ADP-Ribose",  
           "2-Hydroxybutyric acid", "2-Hydroxyglutarate", 
           "Gluconate-6P", "Glycerate", 
           "GDP","ADP", "AMP", "GMP", "ATP", "Xanthine", "IMP","Hypoxanthine", "Uric acid", "Allantoin",   "FAICAR", 
           "CMP", "UMP", "Uracil", "Uridine", 
           "TMP", "Citric acid", "Glutamine", "Alpha-Ketoglutarate", "Glutamate", "Malate", "Succinate", 
           "Kynurenine", "Tryptophan", "Aminoadipic acid", "Kynurenic acid", "Arginine", "Ornithine", "Acetylglutamic acid", "Citrulline" )



Heatmap_metabolomics_blood <- Heatmap(input, column_title = "Metabolomics_blood", col = col_fun,
                   column_title_gp = gpar(size=12, fontface="bold"), show_column_names = T, column_names_rot = 0,
                   show_row_names = T, show_row_dend = F, show_column_dend = F, row_names_side = "right",
                   row_names_gp = gpar(fontsize = 6), cluster_rows = F, cluster_columns = F,
                   border=T,
                   row_order = order,
                   row_split = factor(figure$Domain, levels = c("Aminoacid", "Carnitine", "Creatine", "Glycolysis",  "Glutathione", "Ketone metabolism", "Lipid biosynthesis", "Pentose Phosphate", "Purines", "Pyrimidine", "NAD", "TCA", "Tryptophan",  "Urea cycle","Other")),
                   cluster_row_slices = F,
                   heatmap_legend_param = list(at = c(-0.8, -0.5, -0.2, 0, 0.2, 0.5, 0.8),
                                               labels = c("<-0.8", "<-0.5", "<-0.2", ">-0.2 & <0.2", ">0.2", ">0.5", ">0.8"),
                                               title = "Hedges' g", color_bar = "discrete", fontsize = 8),
                   row_title_rot = 0, row_title_gp = gpar(fontsize = 30, fontface="bold"),
                   column_names_gp = gpar(fontsize = 15, fontface="bold", hjust = 0.8),
                   rect_gp = gpar(col = "black", lwd = 0.5),
                   border_gp = gpar(col = "black", lwd = 1))
Heatmap_metabolomics_blood

#panel C 
metabolomics_panelC$ratio <- metabolomics_panelC$`Citric acid`/metabolomics_panelC$Lactate
metabolomics_panelC$ratio <- as.numeric(metabolomics_panelC$ratio)
metabolomics_panelC$Time <- factor(metabolomics_panelC$Time, levels =c("Baseline", "1-day after PEM"))

p_norm <- sapply(metabolomics_panelC["ratio"], shapiro.test)
p_norm #not normal - boxcox

metabolomics_panelC_box <- metabolomics_panelC
metabolomics_panelC_box["ratio"] <- apply(metabolomics_panelC_box["ratio"], 2, function(x) boxcoxT(x))
p_norm <- sapply(metabolomics_panelC_box["ratio"], shapiro.test)
p_norm #normal distribution



#calculate with ratios 2 
model <-lme(ratio ~ Group*Time, data=metabolomics_panelC_box, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(model, type="III")
#no interaction, use Type II
Anova(model, type="II")

Muscle_citric_lactate<- ggplot(data  = metabolomics_panelC, aes(x = Time, y = ratio)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("Skeletal muscle Citric acid/lactate ratio")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1))
Muscle_citric_lactate


#panel D 
metabolomics_panelD$ratio <- metabolomics_panelD$`Citric acid`/metabolomics_panelD$Lactate
metabolomics_panelD$ratio <- as.numeric(metabolomics_panelD$ratio)
metabolomics_panelD$Time <- factor(metabolomics_panelD$Time, levels =c("Baseline", "1-day after PEM", "1-week after PEM"))

p_norm <- sapply(metabolomics_panelD["ratio"], shapiro.test)
p_norm #not normal - boxcox

metabolomics_panelD_box <- metabolomics_panelD
metabolomics_panelD_box["ratio"] <- apply(metabolomics_panelD_box["ratio"], 2, function(x) boxcoxT(x))
p_norm <- sapply(metabolomics_panelD_box["ratio"], shapiro.test)
p_norm #normal distribution



#calculate with ratios 
model <-lme(ratio ~ Group*Time, data=metabolomics_panelD_box, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(model, type="III")
#no interaction, use Type II
Anova(model, type="II")

Blood_citric_lactate<- ggplot(data  = metabolomics_panelD, aes(x = Time, y = ratio)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("Blood muscle Citric acid/lactate ratio")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1))
Blood_citric_lactate
























# Extended figure 7 --------------------------------------------------------
extfig7 <- read_excel("Source data.xlsx", sheet = "Extended Data Figure 7")
extfig7$Group <- as.factor(extfig7$Group)
extfig7$Time <- as.factor(extfig7$Time)
extfig7$Time <- factor(extfig7$Time, levels =c("Baseline", "1-day after PEM", "1-week after PEM"))

p_norm <- sapply(extfig7[,c(4:9)], shapiro.test)
p_norm
#all not normally distributed 

extfig7_box <- extfig7
extfig7_box[,4:9] <- apply(extfig7_box[,4:9], 2, function(x) boxcoxT(x))
p_norm <- sapply(extfig7_box[,c(4:9)], shapiro.test)
p_norm
#The absolute values are normally distributed after boxcox transformation

#panel A
model <-lme(ABS_ND1 ~ Group*Time, data=extfig7_box, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(model, type="III")
#no interaction
Anova(model, type="II")

mt_ND1 <- ggplot(data  = extfig7, aes(x = Time, y = ABS_ND1)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("mt-ND1")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 11000))+
  ylab("Gene expression (AU)")
mt_ND1

#panel B
model <-lme(ABS_ND6 ~ Group*Time, data=extfig7_box, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(model, type="III")
#no interaction
Anova(model, type="II")

mt_ND6 <- ggplot(data  = extfig7, aes(x = Time, y = ABS_ND6)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("mt-ND6")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6000))+
  ylab("Gene expression (AU)")
mt_ND6

#panel C
model <-lme(ABS_ND1_ND6 ~ Group*Time, data=extfig7_box, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(model, type="III")
#no interaction
Anova(model, type="II")

mt_ND1_ND6 <- ggplot(data  = extfig7, aes(x = Time, y = ABS_ND1_ND6)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("mt-ND1/ND6")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8000))+
  ylab("Gene expression (AU)")
mt_ND1_ND6




#panel D
extfig7_D <- subset(extfig7, select =c(ID, Group, Time, REL_ND1))
reshape2::dcast(data = extfig7_D ,formula = ID+Group~Time, value.var = "REL_ND1") -> extfig7_D_2
extfig7_D_2$deltat1t4 <- extfig7_D_2$`1-day after PEM` - extfig7_D_2$Baseline
extfig7_D_2$deltat1t5 <- extfig7_D_2$`1-week after PEM` - extfig7_D_2$Baseline

p_norm <- sapply(extfig7_D_2[,c(6:7)], shapiro.test)
p_norm

kruskal.test(deltat1t4 ~ Group, data = extfig7_D_2)
kruskal.test(deltat1t5 ~ Group, data = extfig7_D_2)

#no differences between groups on timepoints
extfig7_D$unique <- paste(extfig7_D$Time, extfig7_D$Group)
extfig7_D$unique<- as.factor(extfig7_D$unique)
summary(aov(REL_ND1 ~ unique, data = extfig7_D))
TukeyHSD(aov(REL_ND1 ~ unique, data = extfig7_D))

rel_mt_ND1 <- ggplot(data  = extfig7, aes(x = Time, y = REL_ND1)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("Relative mt-ND1")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15))+
  ylab("Gene expression (AU)")
rel_mt_ND1


#panel E
extfig7_E <- subset(extfig7, select =c(ID, Group, Time, REL_ND6))
reshape2::dcast(data = extfig7_E ,formula = ID+Group~Time, value.var = "REL_ND6") -> extfig7_E_2
extfig7_E_2$deltat1t4 <- extfig7_E_2$`1-day after PEM` - extfig7_E_2$Baseline
extfig7_E_2$deltat1t5 <- extfig7_E_2$`1-week after PEM` - extfig7_E_2$Baseline

p_norm <- sapply(extfig7_E_2[,c(6:7)], shapiro.test)
p_norm

kruskal.test(deltat1t4 ~ Group, data = extfig7_E_2)
kruskal.test(deltat1t5 ~ Group, data = extfig7_E_2)

#no differences between groups on timepoints
extfig7_E$unique <- paste(extfig7_E$Time, extfig7_E$Group)
extfig7_E$unique<- as.factor(extfig7_E$unique)
summary(aov(REL_ND6 ~ unique, data = extfig7_E))
TukeyHSD(aov(REL_ND6 ~ unique, data = extfig7_E))


rel_mt_ND6 <- ggplot(data  = extfig7, aes(x = Time, y = REL_ND6)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("Relative mt-ND6")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20))+
  ylab("Gene expression (AU)")
rel_mt_ND6

#panel F
extfig7_F <- subset(extfig7, select =c(ID, Group, Time, Relative_ND1_ND6))
reshape2::dcast(data = extfig7_F ,formula = ID+Group~Time, value.var = "Relative_ND1_ND6") -> extfig7_F_2
extfig7_F_2$deltat1t4 <- extfig7_F_2$`1-day after PEM` - extfig7_F_2$Baseline
extfig7_F_2$deltat1t5 <- extfig7_F_2$`1-week after PEM` - extfig7_F_2$Baseline

p_norm <- sapply(extfig7_F_2[,c(6:7)], shapiro.test)
p_norm

kruskal.test(deltat1t4 ~ Group, data = extfig7_F_2)
kruskal.test(deltat1t5 ~ Group, data = extfig7_F_2)

#no differences between groups on timepoints
extfig7_F$unique <- paste(extfig7_F$Time, extfig7_F$Group)
extfig7_F$unique<- as.factor(extfig7_F$unique)
summary(aov(Relative_ND1_ND6 ~ unique, data = extfig7_F))
TukeyHSD(aov(Relative_ND1_ND6 ~ unique, data = extfig7_F))


REL_mt_ND1_ND6 <- ggplot(data  = extfig7, aes(x = Time, y = Relative_ND1_ND6)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("Relative mt-ND1/ND6")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15))+
  ylab("Gene expression (AU)")
REL_mt_ND1_ND6


#Panel G-I

extfig7G_I <- read_excel("Source data.xlsx", sheet = "Extended Data Figure 7G-I")
extfig7G_I$Group <- as.factor(extfig7G_I$Group)
extfig7G_I$Time <- as.factor(extfig7G_I$Time)
extfig7G_I$Time <- factor(extfig7G_I$Time, levels =c("Baseline", "1-day after PEM", "1-week after PEM"))

p_norm <- sapply(extfig7G_I[,c(4:7)], shapiro.test)
p_norm
#all not normally distributed 

extfig7G_I_box <- extfig7G_I
extfig7G_I_box[,4:7] <- apply(extfig7G_I_box[,4:7], 2, function(x) boxcoxT(x))
p_norm <- sapply(extfig7G_I_box[,c(4:7)], shapiro.test)
p_norm #all are normally distributed

#Panel G
model <-lme(CK ~ Group*Time, data=extfig7G_I_box, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(model, type="III")
#no interaction
Anova(model, type="II")

CK<- ggplot(data  = extfig7G_I, aes(x = Time, y = CK)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("Creatine kinase")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 500))+
  ylab("U/L")
CK

#Panel H 
model <-lme(`CK_MB` ~ Group*Time, data=extfig7G_I_box, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(model, type="III")
#no interaction
Anova(model, type="II")

CK_MB<- ggplot(data  = extfig7G_I, aes(x = Time, y = CK_MB)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("CK-MB")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5))+
  ylab("ug/L")
CK_MB

#Panel I 
model <-lme(Cortisol ~ Group*Time, data=extfig7G_I_box, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(model, type="III")
#no interaction
Anova(model, type="II")

#correction for time since waking up

model <-lme(Cortisol ~ Group*Time+Minutes_since_waking, data=extfig7G_I_box, random = ~ 1|ID, na.action = na.omit, control="optim")
Anova(model, type="III")
#Still not signficantly different

Cortisol<- ggplot(data  = extfig7G_I, aes(x = Time, y = Cortisol)) +
  geom_boxplot(aes(fill = Group), width = 0.6, position = position_dodge(width = 0.7), outlier.shape = NA) +
  geom_point(aes(fill = Group), size = 2,  alpha = 0.7, shape = 21, position = position_dodge(width=0.7)) +
  theme_bw() + 
  theme(aspect.ratio = 2/1) + 
  scale_fill_manual(values=c("#FFFFFF", "#FF0000"))+
  ggtitle("CK-MB")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1500))+
  ylab("nmol/L")
Cortisol
