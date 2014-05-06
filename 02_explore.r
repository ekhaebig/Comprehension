source("//cifs/l2t/scripts/LookingWhileListeningBeta.r", chdir = TRUE)
options(stringsAsFactors = FALSE)
library(ggplot2)
library(dplyr)
library(lattice)

# Load the data
setwd("//cifs/COMP/ExperimentalTasks/SemanticTask/DataAnalysis/TristanDataAnalysis/")
d <- read.csv("looking_data.csv")
str(d)

#######################################################
#########              RECODING & CLEANING 
#######################################################

# Code participant
d$ASD <- ifelse(d$Subject >= 500, "ASD", "TD")

# Label coded looks with stimuli names.
d$GazeByImageAOI <- d$Look
d$GazeByImageAOI[d$GazeByImageAOI == 1] <- "Target"
d$GazeByImageAOI[d$GazeByImageAOI == 0] <- "Distractor"

d$Condition[d$Condition == "DD"] <- "BothDiff"
d$Condition[d$Condition == "SD"] <- "SemanticSim"
d$Condition[d$Condition == "DS"] <- "PerceptualSim"

d$GazeByImageAOI <- ifelse(is.element(d$GazeByImageAOI, c("-", ".", "0.5")), 
                           NA, d$GazeByImageAOI)

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
attach(count_trials)
hist(NumTrials[Condition=="BothDiff"])
hist(NumTrials[Condition=="PerceptualSim"])
hist(NumTrials[Condition=="SemanticSim"])


# AggregateLooks makes a column called "Target", so create a column called
# "Target2" to avoid interfering with the inner workings of AggregateLooks
d$Target2 <- d$Target


#######################################################
########### PLOTS 
#######################################################

############## CREATE SEPARATE DATSETS FOR (1) Matched Participants and (2) Participants with CDI greater than threshold


##### Matched subject dataset 
#list of participant IDs to remove 
participants <- c(301, 413, 416, 507, 506, 504, 501, 524, 511, 517, 521, 523) 
d_matched <- d %.% mutate(match=match(Subject, participants, nomatch=NA)) %.% filter(is.na(match)) 


##### Participants with minimum CDI subset 
## set thresholds for minimum CDI scores for inclusion in the subset 
TD_CDI_min <- 75
ASD_CDI_min <- 75 

d_CDI <- d %.% filter((CDIWG_WU>=TD_CDI_min & ASD=="TD") | (CDIWG_WU>=ASD_CDI_min & ASD=="ASD" ))



############## PLOTs OF PROPORTION LOOKING THROUGHOUT FULL TRIAL, BY CONDITION

# Aggregated Plots
# aggregated = each child contributes one average proportion of looks across trials
#   in each condition to the average of Group proportion of looking in the plots
by_subject <- AggregateLooks(d, ASD + Subject + Time + Condition ~ GazeByImageAOI)
aggregated_proportion <- by_subject %.% group_by(ASD, Condition, Time) %.% summarise(Proportion = mean(Proportion))
qplot(data = aggregated_proportion, x = Time, y = Proportion, color = Condition) + 
  facet_grid(ASD ~ .) + geom_line(y=.5, colour="gray48") + labs(title = "aggregated looks")


# with confidence interval 
# note from NA: we are still unsure whether this is the correct method for calculating confidence intervals 
 
by_subject <- by_subject %.%  mutate(Variance=Proportion*(1-Proportion)/(Target+Others))
aggregated_proportion <- by_subject %.% group_by(ASD, Condition, Time) %.% 
  summarise(Proportion = mean(Proportion), 
            PooledVariance=sum(Variance*(Target+Others-1))/sum(Target+Others-1),
            LB=Proportion-1.96*sqrt(PooledVariance),
            UB=Proportion+1.96*sqrt(PooledVariance)) 

qplot(data = aggregated_proportion, x = Time, y = Proportion, color = Condition) + 
  facet_grid(ASD ~ .) + geom_line(y=.5, colour="gray48") + labs(title = "aggregated looks") +
  geom_ribbon(data=aggregated_proportion, aes(ymin=LB, ymax=UB), alpha=.3) 

# Descriptive Count of participants and # of trials in the aggregated plot above
d %.% group_by(ASD, Subject) %.% summarise(Count = n_distinct(TrialNo))
#count of trials per condition for each subject
d %.% group_by(ASD, Subject, Condition) %.% summarise(Count = n_distinct(TrialNo))
# number of participants included 
d %.% group_by(ASD, Condition) %.% summarise(Count = n_distinct(Subject))


######same plot as above, using participants with more than 75 words in the ASD group 

d_CDI <- d_CDI %.% filter(ASD=="ASD")
by_subject <- AggregateLooks(d_CDI, Subject + Time + Condition ~ GazeByImageAOI)
aggregated_proportion <- by_subject %.% group_by(Condition, Time) %.% summarise(Proportion = mean(Proportion))
qplot(data = aggregated_proportion, x = Time, y = Proportion, color = Condition) + 
   geom_line()+ geom_line(y=.5, colour="gray48") + labs(title = "Children with ASD with words>=75")

# Descriptive Count of participants and # of trials in the aggregated plot above
d_CDI %.% group_by(ASD, Subject) %.% summarise(Count = n_distinct(TrialNo))
#count of trials per condition for each subject
d_CDI %.% group_by(ASD, Subject, Condition) %.% summarise(Count = n_distinct(TrialNo))
# number of participants included by group and condition 
d_CDI %.% group_by(ASD, Condition) %.% summarise(Count = n_distinct(Subject))


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
