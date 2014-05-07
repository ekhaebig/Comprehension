setwd("F:/Comprehension")
source("LookingWhileListeningBeta.r")
#options(stringsAsFactors = FALSE)
library(ggplot2)
library(dplyr)
library(lattice)
library(lmSupport)

# Load the data

variablesloaded <- load(file="./matched_level1.Rdata")  # This has subject, Group, 
# Proportion of looking between 200 & 


load(file="./looking_data.Rdata")
load(file='looking_data_with_MatchedPinfo_clean.Rdata')


m_proportion_over_trials <- d_matched %.% 
  group_by(Subject,ASD,Condition,Time) %.% 
  summarize(Proportion=mean(GazeByImageAOI,na.rm=TRUE))

m_proportion_over_trials <- merge(m_proportion_over_trials,unique(d_matched[,c("Subject","CDIWG_WU")]),by="Subject")
head(m_proportion_over_trials)

save(m_proportion_over_trials,file="./matched_level1.Rdata")


testWindow <- m_proportion_over_trials %.% filter(Time>=200 & Time<=1800)
dim(m_proportion_over_trials) - dim(testWindow)
#testWindowSummary collapses across time-bins to create a proportion of 
# time looking by participant x condition (3 rows per participant )
testWindowSummary <- testWindow %.% group_by(Subject,ASD,Condition) %.% 
  summarise(ProportionTestWindow=mean(Proportion,na.rm=TRUE))%.% ungroup()
# testWindowSummaryWide creates a wide dataset that has only one row per 
#   participant, and a column for each condition  
testWindowSummaryWide <- reshape(testWindowSummary, idvar="Subject", 
                                 direction="wide", v.names="ProportionTestWindow", timevar="Condition") 


baselineWindow <- m_proportion_over_trials %.% filter(Time>=-1400 & Time<200)
dim(m_proportion_over_trials) - dim(baselineWindow)
#same as lines above, but for baseline 
baselineWindowSummary <- baselineWindow %.% group_by(Subject,ASD,Condition) %.% summarise(ProportionBaseline=mean(Proportion,na.rm=TRUE))

baselineWindowSummaryWide <- reshape(baselineWindowSummary, idvar="Subject", direction="wide", v.names="ProportionBaseline", timevar="Condition") 

########## MERGE CALCULATED VARIABLES ABOVE INTO IN PARTICPANT INFO 

# merge the participant data into the main dataset 
d_matched <- merge(d_matched, testWindowSummaryWide,by="Subject")
d_matched <- merge(d_matched, baselineWindowSummaryWide,by="Subject")

#the dataset that you can use to run basic analyses is "participantinfo".  You can use this within R or export to Excel, SPSS, etc. 
save(d_matched,file='looking_data_with_MatchedPinfo_clean.Rdata')
#write.csv(particpantinfo, "participantInfoWithMeanProportion.csv", row.names = FALSE)
#str(particpantinfo)


DiffScore <- testWindowSummary$ProportionTestWindow - baselineWindowSummary$ProportionBaseline

dm.ancova <- testWindowSummary[,c("Subject","ASD","Condition")]
dm.ancova <- merge(dm.ancova,unique(d[,c("Subject","CDIWG_WU")]),by="Subject")
dm.ancova$C_CDIWG_WU <- dm.ancova$CDIWG_WU - mean(dm.ancova$CDIWG_WU)
dm.ancova$DiffScore <- DiffScore

library(lmSupport)
contrasts(dm.ancova$ASD) <- varContrasts(dm.ancova$ASD,Type="DUMMY",RefLevel=2)
# 1 = ASD as reference group, 2 = TD as reference group (RefLevel)
# Orthogonal contrast for group
#contrasts(dm.ancova$ASD) <- varContrasts(dm.ancova$ASD,Type="POC",POCList=list(c(1,-1)))
contrasts(dm.ancova$Condition) <- varContrasts(dm.ancova$Condition,Type="POC",
                                               POCList=list(c(1,-1,0),c(-1,-1,2)))

matched.ancova<-lm(DiffScore~ASD*Condition,data=dm.ancova)
summary(matched.ancova)
Anova(matched.ancova,type=3)
levels(dm.ancova$Condition)
library(nlme)
library(lme4)
# Create a centered CDI variable
m_proportion_over_trials$C_CDIWG_WU <- m_proportion_over_trials$CDIWG_WU - mean(m_proportion_over_trials$CDIWG_WU)

# RIMs
empty <- lmer(Proportion ~ 1 + (1 | Subject), data = m_proportion_over_trials)

RIM <- lmer(Proportion~Time+ASD*Condition+C_CDIWG_WU+(1|Subject),data=m_proportion_over_trials)
summary(RIM)

sq.RIM <- lmer(Proportion~Time+I(Time^2)+ASD*Condition+C_CDIWG_WU+(1|Subject),data=m_proportion_over_trials)
summary(sq.RIM)

unstruct <- gls(Proportion  ~ Time, data = m_proportion_over_trials, 
                correlation = corSymm(form = ~ 1 | Subject),  
                weights = varIdent(form = ~ 1|Subject), method="ML")
# Errors

RSM <- lmer(Proportion~Time+ASD*Condition+C_CDIWG_WU+(Time|Subject),data=m_proportion_over_trials)
summary(RSM)

sq.RSM <- lmer(Proportion~Time+I(Time^2)+ASD*Condition+C_CDIWG_WU+(Time|Subject),data=m_proportion_over_trials)
summary(sq.RSM)

anova(RIM, sq.RIM, RSM, sq.RSM)
anova(RSM, sq.RSM)

RIM <- lme(Proportion ~ Time+ASD+Condition, random = ~ 1 | Subject, m_proportion_over_trials, method = 'ML')
