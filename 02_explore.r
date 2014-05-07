setwd("F:/Comprehension")
setwd("G:/Comprehension")
source("LookingWhileListeningBeta.r")
#options(stringsAsFactors = FALSE)
library(ggplot2)
library(dplyr)
library(lattice)

# Load the data

load(file="./looking_data.Rdata")
d <- longTable
rm(longTable)
str(d)

d <- d[,1:27]  # takes out the variables after the 27th variable (Vineland, ADOS, ADI-R)
str(d)

#######################################################
#########              RECODING & CLEANING 
#######################################################

# Code participant
d$ASD <- as.factor(ifelse(as.numeric(d$Subject) >= 500, "ASD", "TD"))

# Label coded looks with stimuli names.
badLookCodes <- c("-", ".", "0.5")
badLooks <- d$Look %in% badLookCodes
d$GazeByImageAOI <- as.character(d$Look)
d$GazeByImageAOI[badLooks] <- NA
d$GazeByImageAOI <- as.numeric(d$GazeByImageAOI)

# Rename Condition Levels
levels(d$Condition) <- c("BothDiff","PerceptualSim","SemanticSim")

# Before dropping any trials based on percentage missing data, make a copy of the dataset for the baseline window
d_baseline <- d %.% filter(-2000 <= Time, Time <= 0)

#Filter data to contain only measurements in the [-2000,3000] time window
d <- d %.% filter(Time>=-2000 & Time <=3000)

# Compute the proportion of missing data by subject x trial number during the
# test window
proportion_na <- function(xs) sum(is.na(xs)) / length(xs)
prop_na <- d %.% filter(200 <= Time, Time <= 1800) %.% 
  group_by(Subject, TrialNo) %.% 
  summarise(PropNA = proportion_na(GazeByImageAOI))

# Exclude trials with more than 50% missing data
to_exclude <- prop_na %.% filter(PropNA > .5)

# Remove these trials from the whole dataset
d <- anti_join(d, to_exclude)

# Count trials 
d %.% group_by(Subject, Order) %.% 
  summarise(NTrials = n_distinct(TrialNo)) %.% 
  summarise(TrialsBySubject = sum(NTrials)) %.% 
  summarize(Total = sum(TrialsBySubject))

#Exclude conditions within a subject with fewer than 4 trials per block 
trial_count_by_condition <- d %.% 
  group_by(Subject, Condition, Order) %.% 
  summarise(TrialCount = n_distinct(TrialNo)) %.% 
  arrange(TrialCount) %.% filter(TrialCount < 4) #this line eliminates block-conditions that have strictly less than 4 trials
d <- anti_join(d, trial_count_by_condition)

# Print the number of trials by subject in each condition, and create histograms of number of trials by condition  

count_trials <- d %.% group_by(Subject, Condition) %.% summarise(NumTrials=n_distinct(TrialNo))
count_trials 
with(count_trials,hist(NumTrials[Condition=="BothDiff"]))
with(count_trials,hist(NumTrials[Condition=="PerceptualSim"]))
with(count_trials,hist(NumTrials[Condition=="SemanticSim"]))


# AggregateLooks makes a column called "Target", so create a column called
# "Target2" to avoid interfering with the inner workings of AggregateLooks
d$Target2 <- d$Target


#######################################################
########### PLOTS 
#######################################################

############## CREATE SEPARATE DATSETS FOR (1) Matched Participants and (2) Participants with CDI greater than threshold


##### Matched subject dataset 
#list of participant IDs to remove 
participants <- c(301, 413, 417, 418, 422, 501, 502, 504, 506, 507, 510, 511, 513, 517, 518, 521, 523, 524) 
d_matched <- d %.% mutate(match=match(Subject, participants, nomatch=NA)) %.% filter(is.na(match)) 
save(d_matched,file="./looking_data_with_MatchedPinfo_clean.Rdata")

m_count_trials <- d_matched %.% group_by(Subject, Condition) %.% summarise(NumTrials=n_distinct(TrialNo))
m_count_trials 
with(m_count_trials,hist(NumTrials[Condition=="BothDiff"]))
with(m_count_trials,hist(NumTrials[Condition=="PerceptualSim"]))
with(m_count_trials,hist(NumTrials[Condition=="SemanticSim"]))

# Descriptive Count of participants and # of trials in the aggregated plot above
d_matched %.% group_by(ASD, Subject) %.% summarise(Count = n_distinct(TrialNo))
#count of trials per condition for each subject
d_matched %.% group_by(ASD, Subject, Condition) %.% summarise(Count = n_distinct(TrialNo))
# number of participants included by group and condition 
d_matched %.% group_by(ASD, Condition) %.% summarise(Count = n_distinct(Subject))

names(d_matched)
summary(d_matched)


##################################################################
##### Proportion of Looking Plots, using matched participants  ###
##################################################################
# Aggregated Plots
# aggregated = each child contributes one average proportion of looks across trials
#   in each condition to the average of Group proportion of looking in the plots
# by_subject <- AggregateLooks(d_matched, ASD + Subject + Time + Condition ~ GazeByImageAOI)
by_subject <- d_matched %.%
  group_by(ASD,Condition,Time) %.%
  summarize(Proportion=mean(GazeByImageAOI,na.rm=TRUE))

m_aggregated_proportion <- by_subject %.% group_by(ASD, Condition, Time) %.% summarise(Proportion = mean(Proportion))
qplot(data = m_aggregated_proportion, x = Time, y = Proportion, color = Condition) + 
  facet_grid(ASD ~ .) + geom_line()+ geom_line(y=.5, colour="gray48") + labs(title = "Matched Groups")

# Get proportion of looking across trials within a condition for each time bin
m_proportion_over_trials <- d_matched %.% 
  group_by(Subject,ASD,Condition,Time) %.% 
  summarize(Proportion=mean(GazeByImageAOI,na.rm=TRUE))

m_proportion_over_trials <- merge(m_proportion_over_trials,unique(d_matched[,c("Subject","CDIWG_WU")]),by="Subject")
head(m_proportion_over_trials)

save(m_proportion_over_trials,file="./matched_level1.Rdata")


# Mean proportion of looking over test window and baseline window
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
head(d_matched)

#the dataset that you can use to run basic analyses is "participantinfo".  You can use this within R or export to Excel, SPSS, etc. 
save(d_matched,file='looking_data_with_MatchedPinfo_clean.Rdata')
#write.csv(particpantinfo, "participantInfoWithMeanProportion.csv", row.names = FALSE)
#str(particpantinfo)


#### Analyses for mated groups  ##################
#ANCOVA
# Compute difference score Testwindow - Baseline
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
#contrasts(dm.ancova$Condition) <- varContrasts(dm.ancova$Condition,Type="POC",
#                                           POCList=list(c(1,-1,0),c(-1,-1,2)))
contrasts(dm.ancova$Condition) <- varContrasts(dm.ancova$Condition,Type="DUMMY",RefLevel=1)

matched.ancova<-lm(DiffScore~ASD*Condition+C_CDIWG_WU,data=dm.ancova)
summary(matched.ancova)
matched.anova<-lm(DiffScore~ASD*Condition,data=dm.ancova)
summary(matched.anova)
Anova(matched.ancova,type=3)  # This tests the aggregated effect of condition, not the contrasts
levels(dm.ancova$Condition)

################### HLM   ##########################
library(nlme)
library(lme4)
library(lmSupport)

# Create a centered CDI variable
testWindow$C_CDIWG_WU <- testWindow$CDIWG_WU - mean(testWindow$CDIWG_WU)

# Visualize the distribution of Proportion of looks to targets in the test window
hist(testWindow$Proportion)

# Look at contrasts and reset them if wanted
contrasts(testWindow$ASD)
levels(testWindow$Condition)
contrasts(testWindow$ASD) <- varContrasts(testWindow$ASD,Type="DUMMY",RefLevel=2)
# 1 = ASD as reference group, 2 = TD as reference group (RefLevel)
# Orthogonal contrast for group
#contrasts(testWindow$ASD) <- varContrasts(testWindow$ASD,Type="POC",POCList=list(c(1,-1)))
#contrasts(testWindow$Condition) <- varContrasts(testWindow$Condition,Type="POC",
#                                          POCList=list(c(1,-1,0),c(-1,-1,2)))
contrasts(testWindow$Condition) <- varContrasts(testWindow$Condition,Type="DUMMY",RefLevel=1)

# Empty model
empty <- lme(Proportion ~ 1, random = ~ 1 | Subject, data = testWindow, method='ML')
VarCorr(empty)

# ERROR: unable to find an inherited method for function "VarCorr", for signature "lme"
# detach("package:lme4")  

(VC.empty <- as.numeric(VarCorr(empty)))
(ICC <- VC.empty[1]/(VC.empty[1]+VC.empty[2]))
# ICC isn't very high (02041)

# RIMs
RIM <- lme(Proportion ~ Time + ASD*Condition + C_CDIWG_WU, random = ~ 1|Subject, testWindow, method='ML')
#RIM <- lmer(Proportion~Time+ASD*Condition+C_CDIWG_WU+(1|Subject),data=testWindow)
summary(RIM)

sq.RIM <- lme(Proportion ~ Time + I(Time^2) + ASD*Condition + C_CDIWG_WU,
              random = ~ 1|Subject, testWindow, method='ML')
#sq.RIM <- lmer(Proportion~Time+I(Time^2)+ASD*Condition+C_CDIWG_WU+(1|Subject),data=testWindow)
summary(sq.RIM)

unstruct <- gls(Proportion  ~ Time + ASD*Condition + C_CDIWG_WU, data = testWindow, 
                correlation = corSymm(form = ~ 1 | Subject),  
                weights = varIdent(form = ~ 1|Subject), method="ML")
# I bet I don't get convergence because the time window????

# RSM
#RSM <- lmer(Proportion~Time+ASD*Condition+C_CDIWG_WU+(Time|Subject),data=m_proportion_over_trials)
RSM <- lme(Proportion ~ Time + ASD*Condition + C_CDIWG_WU, random = ~ Time|Subject, testWindow, method='ML')
summary(RSM)


sq.RSM <- lme(Proportion ~ Time + I(Time^2) + ASD*Condition + C_CDIWG_WU, random = ~ Time|Subject, testWindow, method='ML')
summary(sq.RSM)

anova(RIM, sq.RIM, RSM, sq.RSM)
anova(RSM, sq.RSM)

sq.unstruct <- update(unstruct, Proportion ~ Time + I(Time^2) + ASD*Condition + C_CDIWG_WU)
summary(sq.unstruct)


RSM.AR1 <- update(RSM, correlation=corAR1(, form = ~ 1|Subject))  # convergence error

#(RIM.AR1 <- lme(Proportion ~ Time + ASD*Condition + C_CDIWG_WU, random = ~ 1 | Subject,
#               testWindow, method = 'ML', correlation=corAR1(, form = ~ 1|Subject)))
RIM.AR1 <- update(RIM, correlation=corAR1(, form = ~ 1|Subject))

sq.RIM.AR1 <- update(sq.RIM, correlation=corAR1(, form = ~ 1|Subject))
summary(sq.RIM.AR1)

RSM.hetero <- update(RSM, weights=varIdent(form = ~ 1|Subject))  # convergence error
sq.RSM.hetero <- update(sq.RSM, weights=varIdent(form = ~ 1|Subject))  # convergence error


anova(RIM, sq.RIM, RIM.AR1, sq.RIM.AR1, RSM, sq.RSM)
anova(RIM.AR1, sq.RIM.AR1)

logLik(RIM.AR1)*-2
logLik(sq.RIM.AR1)*-2   ##### This gives you your deviance score!!!!!!!
summary(sq.RIM.AR1)

#> anova(RIM, sq.RIM, RIM.AR1, sq.RIM.AR1, RSM, sq.RSM)
#Model df        AIC        BIC   logLik   Test   L.Ratio p-value
#RIM            1 10  -4358.294  -4296.201 2189.147                         
#sq.RIM         2 11  -4482.863  -4414.560 2252.431 1 vs 2 126.56906  <.0001
#RIM.AR1        3 11 -13623.141 -13554.838 6822.570                         
#sq.RIM.AR1     4 12 -13641.803 -13567.292 6832.902 3 vs 4  20.66276  <.0001
#RSM            5 12  -4871.952  -4797.440 2447.976                         
#sq.RSM         6 13  -5019.244  -4938.523 2522.622 5 vs 6 149.29250  <.0001

#> summary(sq.RIM.AR1)
#Linear mixed-effects model fit by maximum likelihood
#Data: testWindow 
#AIC       BIC   logLik
#-13641.8 -13567.29 6832.902

#Random effects:
#  Formula: ~1 | Subject
#(Intercept)  Residual
#StdDev: 2.74888e-05 0.1483597

#Correlation Structure: AR(1)
#Formula: ~1 | Subject 
#Parameter estimate(s):
#  Phi 
#0.9678042 
#Fixed effects: Proportion ~ Time + I(Time^2) + ASD * Condition + C_CDIWG_WU 
#                                                 Value    Std.Error   DF   t-value p-value
#(Intercept)                                    0.5053493 0.03481305 3644 14.516090  0.0000
#Time                                           0.0002514 0.00004009 3644  6.270960  0.0000
#I(Time^2)                                     -0.0000001 0.00000002 3644 -4.553994  0.0000
#ASDASD_v_TD                                   -0.1752398 0.03424227   22 -5.117646  0.0000
#ConditionPerceptualSim_v_BothDiff             -0.0858869 0.01640706 3644 -5.234752  0.0000
#ConditionSemanticSim_v_BothDiff               -0.0998861 0.02899559 3644 -3.444872  0.0006
#C_CDIWG_WU                                     0.0003266 0.00022281   22  1.465966  0.1568
#ASDASD_v_TD:ConditionPerceptualSim_v_BothDiff  0.1981812 0.01483805 3644 13.356288  0.0000
#ASDASD_v_TD:ConditionSemanticSim_v_BothDiff    0.1815953 0.02065940 3644  8.789963  0.0000
#Correlation: 
#  (Intr) Time   I(T^2) ASDASD_v_TD CPS__B CSS__B C_CDIW
#Time                                          -0.489                                               
#I(Time^2)                                      0.369 -0.977                                        
#ASDASD_v_TD                                   -0.511  0.000  0.000                                 
#ConditionPerceptualSim_v_BothDiff             -0.578  0.158  0.004  0.124                          
#ConditionSemanticSim_v_BothDiff               -0.628  0.183  0.000  0.112       0.884              
#C_CDIWG_WU                                    -0.060  0.000  0.000  0.117       0.000  0.000       
#ASDASD_v_TD:ConditionPerceptualSim_v_BothDiff  0.134  0.000  0.000 -0.263      -0.470 -0.258  0.000
#ASDASD_v_TD:ConditionSemanticSim_v_BothDiff    0.154  0.000  0.000 -0.302      -0.327 -0.371  0.000
#ASDASD__TD:CP
#Time                                                       
#I(Time^2)                                                  
#ASDASD_v_TD                                                
#ConditionPerceptualSim_v_BothDiff                          
#ConditionSemanticSim_v_BothDiff                            
#C_CDIWG_WU                                                 
#ASDASD_v_TD:ConditionPerceptualSim_v_BothDiff              
#ASDASD_v_TD:ConditionSemanticSim_v_BothDiff    0.696       

#Standardized Within-Group Residuals:
#  Min          Q1         Med          Q3         Max 
#-3.10023474 -0.66045952 -0.02447747  0.66881273  3.46339138 

#Number of Observations: 3675
#Number of Groups: 25 


##### Participants with minimum CDI subset 
## set thresholds for minimum CDI scores for inclusion in the subset 
TD_CDI_min <- 75
ASD_CDI_min <- 75 

d_CDI <- d %.% filter((CDIWG_WU>=TD_CDI_min & ASD=="TD") | (CDIWG_WU>=ASD_CDI_min & ASD=="ASD" ))



############## PLOTs OF PROPORTION LOOKING THROUGHOUT FULL TRIAL, BY CONDITION

# Aggregated Plots
# aggregated = each child contributes one average proportion of looks across trials
#   in each condition to the average of Group proportion of looking in the plots
by_subject <- d %.%
  group_by(ASD,Condition,Time) %.%
  summarize(Proportion=mean(GazeByImageAOI,na.rm=TRUE))

#by_subject <- AggregateLooks(d, ASD + Subject + Time + Condition ~ GazeByImageAOI)
aggregated_proportion <- by_subject %.% group_by(ASD, Condition, Time) %.% summarise(Proportion = mean(Proportion))
qplot(data = aggregated_proportion, x = Time, y = Proportion, color = Condition) + 
  facet_grid(ASD ~ .) + geom_line()+ geom_line(y=.5, colour="gray48") + 
  labs(title = "Full Dataset - Looking Behavior")


full_proportion_over_trials <- d %.% 
  group_by(Subject,ASD,Condition,Time) %.% 
  summarize(Proportion=mean(GazeByImageAOI,na.rm=TRUE))

full_proportion_over_trials <- merge(full_proportion_over_trials,unique(d[,c("Subject","CDIWG_WU","Bayley_composite","Gender","Maternal_ed")]),by="Subject")
head(full_proportion_over_trials)


save(full_proportion_over_trials,file="./full_level1.Rdata")


testWindow <- by_subject %.% filter(Time>=200 & Time<=1800)
dim(by_subject) - dim(testWindow)
#testWindowSummary collapses across time-bins to create a proportion of 
# time looking by participant x condition (3 rows per participant )
testWindowSummary <- testWindow %.% group_by(Subject,ASD,Condition) %.% 
  summarise(ProportionTestWindow=mean(Proportion,na.rm=TRUE)) %.% ungroup()
# testWindowSummaryWide creates a wide dataset that has only one row per 
#   participant, and a column for each condition  
testWindowSummaryWide <- reshape(testWindowSummary, idvar="Subject", 
    direction="wide", v.names="ProportionTestWindow", timevar="Condition") 


baselineWindow <- by_subject %.% filter(Time>=-1400 & Time<200)
dim(by_subject) - dim(baselineWindow)
#same as lines above, but for baseline 
baselineWindowSummary <- baselineWindow %.% group_by(Subject,ASD,Condition) %.% summarise(ProportionBaseline=mean(Proportion,na.rm=TRUE))
baselineWindowSummaryWide <- reshape(baselineWindowSummary, idvar="Subject", direction="wide", v.names="ProportionBaseline", timevar="Condition") 

########## MERGE CALCULATED VARIABLES ABOVE INTO IN PARTICPANT INFO 

# merge the participant data into the main dataset 
#d <- merge(d, testWindowSummaryWide,by="Subject")
#d <- merge(d, baselineWindowSummaryWide,by="Subject")

#the dataset that you can use to run basic analyses is "participantinfo".  You can use this within R or export to Excel, SPSS, etc. 
save(d,file='looking_data_with_pinfo_clean.Rdata')
#write.csv(particpantinfo, "participantInfoWithMeanProportion.csv", row.names = FALSE)
#str(particpantinfo)

# Descriptive Count of participants and # of trials in the aggregated plot above
d %.% group_by(ASD, Subject) %.% summarise(Count = n_distinct(TrialNo))
#count of trials per condition for each subject
d %.% group_by(ASD, Subject, Condition) %.% summarise(Count = n_distinct(TrialNo))
# number of participants included 
d %.% group_by(ASD, Condition) %.% summarise(Count = n_distinct(Subject))

# Analyses
#ANCOVA
# Compute difference score Testwindow - Baseline
DiffScore <- testWindowSummary$ProportionTestWindow - baselineWindowSummary$ProportionBaseline

d.ancova <- testWindowSummary[,c("Subject","ASD","Condition")]
d.ancova <- merge(d.ancova,unique(d[,c("Subject","CDIWG_WU")]),by="Subject")
d.ancova$C_CDIWG_WU <- d.ancova$CDIWG_WU - mean(d.ancova$CDIWG_WU)
d.ancova$DiffScore <- DiffScore



library(lmSupport)
contrasts(d.ancova$ASD) <- varContrasts(d.ancova$ASD,Type="DUMMY",RefLevel=2)
# 1 = ASD as reference group, 2 = TD as reference group (RefLevel)
# Orthogonal contrast for group
#contrasts(d.ancova$ASD) <- varContrasts(d.ancova$ASD,Type="POC",POCList=list(c(1,-1)))
contrasts(d.ancova$Condition) <- varContrasts(d.ancova$Condition,Type="POC",POCList=list(c(1,-1,0),c(-1,-1,2)))

ancova <-lm(DiffScore~ASD*Condition,data=d.ancova)
ancova_cdi <-lm(DiffScore~ASD*Condition+C_CDIWG_WU,data=d.ancova)
summary(ancova)
summary(ancova_cdi)

levels(d.ancova$Condition)


# +--------------------------------+
# |  Exploratory Data Analysis     | ####
# +--------------------------------+
# :::::::::::::  TABLES :::::::::::::::
# The data is pretty balanced
table(d_matched$time)
with(d_matched, tapply(m_aggregated_proportion, list(time), mean))  
with(NYS, tapply(attit, list(age), sd))  # look for pattern
# mean change - is this linear?
# Standard deviation = variance increased
# look at this stuff in project ( does your outcome look close to normal distribution)
#     and your ICC should not be 0 (that would mean that clustering doesn't matter
#     and that you shouldn't be using multilevel analyses.) justify that you should use HLM.

with(NYS, tapply(expo, list(age), mean))  # should exposure be used as a predictor (chicken and egg problem)
with(NYS, tapply(expo, list(age), sd))  

table(NYS$female)   # level-1 observations (attitude ratings collected from males & females
with(NYS, tapply(attit, list(female,age), mean))  # females increased in variability over time (more than males)
with(NYS, tapply(attit, list(female,age), sd)) 


table(NYS$minority)   # level-1 observations
with(NYS, tapply(attit, list(minority,age), mean))
with(NYS, tapply(attit, list(minority,age), sd))

table(NYS$income)
with(NYS, tapply(attit, list(income), mean))  
with(NYS, tapply(attit, list(income), sd))
with(NYS, tapply(attit, list(income), sd))/sqrt(table(NYS$income))
# based on each attitude mean, based on income, the data is really only monotonic
# And it's not even continuous
# Income probably aren't big deals here.

# with(NYS, tapply(attit, list(minority,female), mean))

(att.gender <- with(NYS, tapply(attit, list(female,age), mean)))  
(att.minority <- with(NYS, tapply(attit, list(minority,age), mean)))  



# :::::::::::::  FIGURES :::::::::::::::

range(NYS$attit)
xval <- c(11:15)

par(mfrow= c(1,2))
# Variance increase part (more than linear)
# And there are gender differences (and it's relatively parallel, but not linear)
plot(xval, xval, type = 'n', xlab = '', ylab = '', xaxt = 'n', ylim = c(0, 1.25))
# axis(1, at = 0:4, labels = 11:15)  if age11 is used instead of age
axis(1, at = xval)  
for (i in unique(NYS$id)){
  with(NYS[NYS$id == i, ], 
       lines(age, attit, col = c('aquamarine', 'gold')[female + 1]))
  # points for single observations
  with(NYS[NYS$id == i, ], if(length(id) == 1) 
    points(age, attit, col = c('aquamarine', 'gold')[female + 1], pch = 4))
}
lines(xval, att.gender[1, ], type = 'o', col = 'darkblue', lwd = 2, pch = 16)
lines(xval, att.gender[2, ], type = 'o', col = 'darkred', lwd = 2, pch = 16)
legend('topleft', c('male','female','male.mean','female.mean'), bty = 'n', col=c('aquamarine','gold','darkblue','darkred'), pch=16)
title("Attitudes toward Deviance by Gender")


# Plot of attitudes towards deviance by minority (ethnicity)
# gender matters more than minority status
# you would probably want random slope here
# variability has gotten even greater over time
plot(xval, xval, type = 'n', xlab = '', ylab = '', xaxt = 'n', ylim = c(0, 1.25))
# axis(1, at = 0:4, labels = 11:15)  if age11 is used instead of age
axis(1, at = xval)  
for (i in unique(NYS$id)){
  with(NYS[NYS$id == i, ], 
       lines(age, attit, col = c('aquamarine', 'gold')[minority + 1]))
  # points for single observations
  with(NYS[NYS$id == i, ], if(length(id) == 1) 
    points(age, attit, col = c('aquamarine', 'gold')[minority + 1], pch = 4))
}
lines(xval, att.minority[1, ], type = 'o', col = 'darkblue', lwd = 2, pch = 16)
lines(xval, att.minority[2, ], type = 'o', col = 'darkred', lwd = 2, pch = 16)
legend('topleft', c('non-minority','minority','non-minority.mean','minority.mean'), bty = 'n', col=c('aquamarine','gold','darkblue','darkred'), pch=16)
title("Attitudes toward Deviance by Ethnicity")




# ::::::::::::::::: MODLES ::::::::::::::::::::::::

# Fit multilevel models for longitudinal data 
# "latent growth curve models" with linear and quadratic slopes


empty <- lme(attit ~ 1, random =  ~ 1 | id, data = NYS, method="ML")
VarCorr(empty)

# ERROR: unable to find an inherited method for function "VarCorr", for signature "lme"
# detach("package:lme4")  

(VC.empty <- as.numeric(VarCorr(empty)))
(ICC <- VC.empty[1]/(VC.empty[1]+VC.empty[2]))
# ICC isn't very high (0.432), but the kids are relatively young and this is
# only over 5 years of time.



# Q1: linear random slope  

unstruct <- gls(attit  ~ age11, data = NYS, correlation = corSymm(form = ~ 1 | id),  weights = varIdent(form = ~ 1|age11), method="ML")

RIM <- lme(attit ~ age11, random = ~ 1 | id, NYS, method = 'ML')

RSM <- lme(attit ~ age11, random = ~ age11 | id, NYS, method = 'ML')
# the slopes are not parallel, that's why the RSM is better than the RIM.

indep <- gls(attit  ~ age11, data = NYS, method="ML")   # not multilevel - this model isn't even close!


anova(indep, RIM, RSM, unstruct)



############## WORD-LEVEL PLOTS 

# Keep only DD trials & [0,1800] time window for the word-level plots 
just_dd <- d %.% filter(Condition == "BothDiff") %.% filter(Time>=-2000 & Time <=3000)

# Word level proportion of looking in target window by group
# note that these plots do not aggregate by subject first, because there are few trials per subject within a single word 

dd_target_by_group <- AggregateLooks(just_dd, ASD + Time + Condition + Target2 ~ GazeByImageAOI)
qplot(data = dd_target_by_group, x = Time, y = Proportion, color = ASD) + facet_wrap("Target2")


############## PLOTs OF FULL TRIAL BY CONDITION, WITH PROBLEM WORDS EXCLUDED

### exclude words 
words_to_exclude <- c("bear", "brush", "clock", "doll", "egg", 
                      "hat", "mouth", "orange")
d_excludewords <- d %.% filter(!is.element(Target2, words_to_exclude))
d_matched_excludewords <- d_matched %.% filter(!is.element(Target2, words_to_exclude))


###### run the same plot as in the previous section
by_subject <- AggregateLooks(d_excludewords, ASD + Subject + Time + Condition ~ GazeByImageAOI)
aggregated_proportion <- by_subject %.% group_by(ASD, Condition, Time) %.% summarise(Proportion = mean(Proportion))
qplot(data = aggregated_proportion, x = Time, y = Proportion, color = Condition) + 
  facet_grid(ASD ~ .) + geom_line() + labs(title = "aggregated looks")

# Descriptive Count of participants and # of trials in the aggregated plot above
d_excludewords %.% group_by(ASD, Subject) %.% summarise(Count = n_distinct(TrialNo))


##### same plot as above, using only matched participants 

by_subject <- AggregateLooks(d_matched_excludewords, ASD + Subject + Time + Condition ~ GazeByImageAOI)
aggregated_proportion <- by_subject %.% group_by(ASD, Condition, Time) %.% summarise(Proportion = mean(Proportion))
qplot(data = aggregated_proportion, x = Time, y = Proportion, color = Condition) + 
  facet_grid(ASD ~ .) + geom_line() + labs(title = "aggregated looks")

# Descriptive Count of participants and # of trials in the aggregated plot above
d_matched_excludewords %.% group_by(ASD, Subject) %.% summarise(Count = n_distinct(TrialNo))

############## SIDE-BIAS PLOTS 
# plot difference in performance (across conditions) when target image is on left side / right side 
by_subject <- AggregateLooks(d, ASD + Subject + Time  + TargetImage ~ GazeByImageAOI)
aggregated_proportion <- by_subject %.% group_by(ASD, Time, TargetImage) %.% summarise(Proportion = mean(Proportion))
qplot(data = aggregated_proportion, x = Time, y = Proportion, color = TargetImage) + 
  facet_grid(ASD ~ .) + geom_line(y=.5) + labs(title = "aggregated looks")

# same plot as above, matched particiants only 
by_subject <- AggregateLooks(d_matched, ASD + Subject + Time  + TargetImage ~ GazeByImageAOI)
aggregated_proportion <- by_subject %.% group_by(ASD, Time, TargetImage) %.% summarise(Proportion = mean(Proportion))
qplot(data = aggregated_proportion, x = Time, y = Proportion, color = TargetImage) + 
  facet_grid(ASD ~ .) + geom_line(y=.5) + labs(title = "aggregated looks")


 

#######################################################
#########              BASELINE ANALYSIS      ? Saliency Effects ???
#######################################################

# Compute the proportion of missing data by subject x trial number during the entire baseline window
prop_na_baseline <- d_baseline %.% 
  group_by(Subject, TrialNo) %.% 
  summarise(PropNA = proportion_na(GazeByImageAOI))

# Exclude trials with more than 50% missing data
to_exclude_baseline <- prop_na_baseline %.% filter(PropNA > .5)

# Remove these trials from the whole dataset
d_baseline <- anti_join(d_baseline, to_exclude_baseline)

# Count trials 
d_baseline %.% group_by(Subject, Order) %.% 
  summarise(NTrials = n_distinct(TrialNo)) %.% 
  summarise(TrialsBySubject = sum(NTrials)) %.% 
  summarize(Total = sum(TrialsBySubject))

#rename the variable 'TargetImage' to 'TargetLocation' for compatibility with previously written syntax
d_baseline <- rename(d_baseline, replace=c('TargetImage'='TargetLocation'))

#clean label for target image "dog"  (in some cases, there are spaces after the word)
d_baseline$ImageL[d_baseline$ImageL=='dog '] <- 'dog'   

#Create a variable that defines the target & distractor images by name 
dL <- filter(d_baseline, TargetLocation=='l') %.% mutate(TargetImage=ImageL, DistractorImage=ImageR)
dR <- filter(d_baseline, TargetLocation=='r') %.% mutate(TargetImage=ImageR, DistractorImage=ImageL)
d_baseline <- rbind(dL, dR)
d_baseline <- mutate(d_baseline, ImagePair=paste(TargetImage, DistractorImage, sep='_'))

#Create a variable ImagePair that contains the two image name in alphabetical order
d_baseline$FirstImage <- apply(d_baseline[, c("TargetImage", "DistractorImage") ], 1, min)
d_baseline$SecondImage <- apply(d_baseline[, c("TargetImage", "DistractorImage") ], 1, max)
d_baseline <- mutate(d_baseline, ImagePair=paste(FirstImage, SecondImage, sep='_')) 

#Create a variable that indicates whether the subject is looking toward the first image or not 
#for compatibility with aggregate looks function, the variable is coded "Target" if looking at the first image in pair; "distractor" if looking at second
d_baseline$GazeByImageAOIOrder[d_baseline$GazeByImageAOI == 'Target' & d_baseline$TargetImage==d_baseline$FirstImage] <- 'Target'
d_baseline$GazeByImageAOIOrder[d_baseline$GazeByImageAOI == 'Distractor' & d_baseline$TargetImage==d_baseline$SecondImage] <- 'Target'
d_baseline$GazeByImageAOIOrder[d_baseline$GazeByImageAOI == 'Distractor' & d_baseline$TargetImage==d_baseline$FirstImage] <- 'Distractor'
d_baseline$GazeByImageAOIOrder[d_baseline$GazeByImageAOI == 'Target' & d_baseline$TargetImage==d_baseline$SecondImage] <- 'Distractor'

# calculate percent looking to target in the [-2000,0] time interval
# sort by bias 
imgbias <- AggregateLooks(d_baseline, ImagePair ~ GazeByImageAOIOrder)
imgbias <- arrange(imgbias, Proportion) %.% select(ImagePair, Proportion)

#calculate percent look to target in baseline by group 
imgbiasbygp <- AggregateLooks(d_baseline, ImagePair + ASD ~ GazeByImageAOIOrder)
imgbiasasd <- filter(imgbiasbygp, ASD=='ASD') %.% select(ImagePair, Proportion_ASD=Proportion)
imgbiastd <- filter(imgbiasbygp, ASD=='TD') %.% select(ImagePair, Proportion_TD=Proportion)

#merge results to one table 
imgbias <- merge(imgbias, imgbiasasd, by='ImagePair' )
imgbias <- merge(imgbias, imgbiastd, by='ImagePair' )


#export results
write.csv(imgbias, "image_bias.csv", row.names = FALSE)


###################################
# next step analyses 

# outcome variables: 
# proportion of looking to target in post versus in baseline ( [200,1800]
# average should occur first by trial over time, and then by condition x kid 

# reaction time 

# ANOVA - pre versus post onse

###################################

#longest fixation to target 

#restrict time range to 200-2000

# this needs to be done without NAs
#needs to be edited to make sure grouping works properly

FixationLengthTarget <- function(d, GazeByImageAOI) {
  
  #transform the GazeByImageAOI variable into a 0/1 variable 
  nonzero_values <- c("Target" , "tracked" , NA)
  d <- d %.% mutate(look = (ifelse(GazeByImageAOI=="Target", 1, (ifelse(match(GazeByImageAOI, nonzero_values, nomatch=0), NA, 0 )))))
  
  #calculate run lengths 
  rl <- rle(d$look)
  len <- rl$lengths
  v <- rl$values 
  
  #calculate cumulative lengths in order to use the value for indexing 
  cumlen <- cumsum(len)
  z <- d$look 
  
  # replace the 0 at the end of each zero-block in z by the 
  # negative of the length of the preceding 1-block
  
  drops <- c(0, diff(v)) <0 
  z[ cumlen[ drops ] ] <- -len[ c(drops[-1],FALSE) ]
  d %.% mutate(FixationLength=look*cumsum(z))
}

d2 <- FixationLengthTarget(d, GazeByImageAOI)


###################################
