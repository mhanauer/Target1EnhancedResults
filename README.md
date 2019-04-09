---
title: "Enhanced Results"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, include=FALSE}
setwd("P:/Evaluation/TN Lives Count_Writing/4_Target1_EnhancedCrisisFollow-up/3_Data & Data Analyses")
datPreAdult = read.csv("Target1EnhancedBaseAdult.csv", header = TRUE)
datPostAdult = read.csv("Target1EnhancedPostAdult.csv", header = TRUE)
datPreYouth = read.csv("Target1EnhancedBaseYouth.csv", header= FALSE, row.names = NULL)
datPostYouth = read.csv("Target1EnhancedPostYouth.csv", header = FALSE, row.names = NULL)
datAdultTreat = read.csv("AdultTreatments.csv", header = TRUE)
library(foreign)
library(nnet)
library(ggplot2)
library(prettyR)
library(nlme)
library(prettyR)
library(descr)
library(Amelia)
library(mitools)
library(BaylorEdPsych)
library(openxlsx)
library(lavaan)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(prettyR)
library(semTools)
library(GPArotation)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(lordif)
library(Amelia)
library(plyr)
library(DescTools)
library(MissMech)
library(jtools)
library(paran)
library(effsize)
library(multcomp)
library(MuMIn)
library(installr)
library(konfound)
library(multcomp)





head(datPreAdult)
# subset the variables that you want
datPreAdult =datPreAdult[c(1, 3:4, 6,8,10, 12:13, 15:34, 36:45, 47:51, 53:59)]
dim(datPreAdult)
head(datPostAdult)
datPostAdult = datPostAdult[c(1, 3:22,24:33, 35:39, 41:47)]
head(datPostAdult)
dim(datPostAdult)
# Rename added variables otherwise everything else should be the same
colnames(datPreAdult)[colnames(datPreAdult) == "Added.V2..Thinking.of.Ways.to.Kill.Self"] = "Added"   

## Now merge everything
datAdult = merge(datPreAdult, datPostAdult, by = "Adult.ID", sort = TRUE)
head(datAdult)
dim(datAdult)

# Need to er
#datAdultTreat = read.csv("AdultTreatment.csv", header = TRUE)
#Now we need to merge the treatment variable before we transform into long format to avoid duplication.

head(datAdultTreat)

## If you don't have a treatment you cannot be included
datAdult = merge(datAdult, datAdultTreat, by = "Adult.ID", sort = TRUE)
head(datAdult)
dim(datAdult)

### This is the actual sample size, because you cannot be included in the study if you do not have a treatment
dim(datAdult)


datAdult = reshape(datAdult, varying = list(c("Desire.to.succeed.x", "Desire.to.succeed.y"), c("My.own.plan.to.stay.well.x", "My.own.plan.to.stay.well.y"), c("Goals.in.life.x", "Goals.in.life.y"), c("Believe.I.can.meet.personal.goals.x", "Believe.I.can.meet.personal.goals.y"), c("Purpose.in.life.x", "Purpose.in.life.y"), c("Fear.doesn.t.stop.me......x", "Fear.doesn.t.stop.me......y"), c("I.can.handle.my.life.x", "I.can.handle.my.life.y"), c("I.like.myself.x", "I.like.myself.y"), c("If.people.really.knew.me.......x", "If.people.really.knew.me.......y"), c("Who.I.want.to.become.x", "Who.I.want.to.become.y"), c("Something.good.will.happen.x", "Something.good.will.happen.y"), c("I.m.hopeful.about.future.x", "I.m.hopeful.about.future.y"), c("Continue.to.have.new.interests.x", "Continue.to.have.new.interests.y"), c("Coping.with.mental.illness.not.focus.of.life.x", "Coping.with.mental.illness.not.focus.of.life.y"), c("Symptoms.interfere.less.and.less.x", "Symptoms.interfere.less.and.less.y"), c("Symptoms.problem.for.shorter.periods.x", "Symptoms.problem.for.shorter.periods.y"), c("Know.when.to.ask.for.help.x", "Know.when.to.ask.for.help.y"), c("Willing.to.ask.for.help.x", "Willing.to.ask.for.help.y"), c("I.ask.for.help.when.I.need.it..x", "I.ask.for.help.when.I.need.it..y"), c("I.can.handle.stress..x", "I.can.handle.stress..y"), c("Better.off.if.I.were.gone.x", "Better.off.if.I.were.gone.y"), c("Happier.without.me.x", "Happier.without.me.y"), c("Death.would.be.a.relief.x", "Death.would.be.a.relief.y"), c("Wish.they.could.be.rid.of.me.x", "Wish.they.could.be.rid.of.me.y"), c("Make.things.worse.x", "Make.things.worse.y"), c("Feel.like.I.belong.x", "Feel.like.I.belong.y"), c("Have.many.caring.and.supportive.friends.x", "Have.many.caring.and.supportive.friends.y"), c("Feel.disconnected.x", "Feel.disconnected.y"), c("Feel.like.an.outsider.x", "Feel.like.an.outsider.y"), c("Close.to.other.people.x", "Close.to.other.people.y"), c("Unable.to.take.care.of.self.x", "Unable.to.take.care.of.self.y"), c("Not.recover.or.get.better.x", "Not.recover.or.get.better.y"), c("I.am.to.blame.x", "I.am.to.blame.y"), c("Unpredictable.x", "Unpredictable.y"), c("Dangerous.x", "Dangerous.y"), c("Wish.life.would.end..x", "Wish.life.would.end..y"), c("Life.not.worth.living.x", "Life.not.worth.living.y"), c("Life.so.bad..feel.like.giving.up..x", "Life.so.bad..feel.like.giving.up..y"), c("Better.for.everyone.if.I.were.to.die..x", "Better.for.everyone.if.I.were.to.die..y"), c("Added.x", "Added.y"), c("No.solution.to.my.problems.x", "No.solution.to.my.problems.y"), c("Believe.my.life.will.end.in.suicide..x", "Believe.my.life.will.end.in.suicide..y")), direction = "long", times = c(0,1))



colnames(datAdult) = c("ID", "Age", "Gender", "Race", "SexualOrientation", "RelationshipStatus", "Edu", "Employment", "Treatment", "Time", "RAS1", "RAS2", "RAS3", "RAS4", "RAS5", "RAS6", "RAS7", "RAS8", "RAS9", "RAS10", "RAS11", "RAS12", "RAS13", "RAS14", "RAS15", "RAS16", "RAS17", "RAS18", "RAS19", "RAS20", "INQ1", "INQ2", "INQ3", "INQ4", "INQ5", "INQ6", "INQ7", "INQ8", "INQ9", "INQ10", "SSMI1", "SSMI2", "SSMI3", "SSMI4", "SSMI5", "SIS1", "SIS2", "SIS3", "SIS4", "SIS5", "SIS6", "SIS7")

# Drop last column id 
datAdult = data.frame(datAdult)
head(datAdult)
datAdult$NA. = NULL
dim(datAdult)

datAdult = datAdult[order(datAdult$ID),]


describe.factor(datAdult$Treatment)

# Two treatments have B with space first so try and recode those as just B's
datAdult$Treatment = ifelse(datAdult$Treatment == "A", 1, ifelse(datAdult$Treatment =="B", 2, ifelse(datAdult$Treatment == " B", 2, ifelse(datAdult$Treatment == "C", 3, datAdult$Treatment)))) 
describe.factor(datAdult$Treatment)

dim(subset(datAdult, Time  == 1))
dim(subset(datAdult, Time  == 0))


# Three items are reversed scored: f = 6, g = 7, j = 10
datAdult$INQ6 = ifelse(datAdult$INQ6 == 1, 5, ifelse(datAdult$INQ6 == 2,4, ifelse(datAdult$INQ6  == 3,3, ifelse(datAdult$INQ6  == 4,2, ifelse(datAdult$INQ6  == 5,1,datAdult$INQ6)))))

datAdult$INQ7= ifelse(datAdult$INQ7== 1, 5, ifelse(datAdult$INQ7== 2,4, ifelse(datAdult$INQ7 == 3,3, ifelse(datAdult$INQ7 == 4,2, ifelse(datAdult$INQ7 == 5,1,datAdult$INQ7)))))

datAdult$INQ10= ifelse(datAdult$INQ10== 1, 5, ifelse(datAdult$INQ10== 2,4, ifelse(datAdult$INQ10 == 3,3, ifelse(datAdult$INQ10 == 4,2, ifelse(datAdult$INQ10 == 5,1,datAdult$INQ10)))))


head(datAdult)

#Checking for issues with adult data set
## In the paper start with 115, because we have three less people later
## 763.0 likely double entry 1131.0, 1272
datAdult[c(187:189, 227:229), c(1:5)]

datAdult = datAdult[-c(187,189, 227,229),]

## Two people dropped, because of two dups
dim(datAdult)
describe.factor(datAdult$Time)
### SO 116 is the analytical starting place#######

datDemo = datAdult[,c(1:10)]


# Create pre data sets for psychometrics
head(datAdult)
RAS = datAdult[c(11:30)]
head(RAS)

head(datAdult)
INQ = datAdult[c(31:40)]
head(INQ)

SSMI = datAdult[c(41:45)]
head(SSMI)

SIS = datAdult[c(46:52)]
SIS

# Subscale one: f =6, g = 7, h = 8, I = 9,  j = 10, k = 11, l = 12, m = 13, t = 20
RASSub1 = RAS[c(6:13, 20)]


# Subscale two q = 17, r= 18, s= 19
head(RAS)
RASSub2 = RAS[c(17:19)]
# Subscale three: a = 1, b = 2, c = 3, d= 4, e = 5
head(RAS)
RASSub3 = RAS[c(1:5)]

# Subscale five: n = 14, o = 15, p = 16
head(RAS)
RASSub5 = RAS[c(14:16)]


#Subscale 1 for INQ: a = 1, b = 2, c = 3, d = 4, e = 5
INQSub1 = INQ[c(1:5)]

#Subscale 2 for INQ: f-j: 6-10
INQSub2 = INQ[c(6:10)]

#Subscale 1 for SIS: a-d: 1:4
SISSub1 = SIS[c(1:4)]

#Subscale 2 for SIS: e-g: 5:7
SISSub2 = SIS[c(5:7)]

SSMIPrePost = datAdult[c(41:45)]
# Creating sum scores for the data analysis that contains all data not just pre data
datAdultDemos = datAdult[c(1:10)]


RASTotalScoreF1 = rowSums(RASSub1)
RASTotalScoreF2 = rowSums(RASSub2)
RASTotalScoreF3 = rowSums(RASSub3)
RASTotalScoreF5 = rowSums(RASSub5)
INQTotalScoreF1 = rowSums(INQSub1)
INQTotalScoreF2 = rowSums(INQSub2)
SISTotalScoreF1 = rowSums(SISSub1)
SISTotalScoreF2 = rowSums(SISSub2)
SSMITotalScore = rowSums(SSMIPrePost)



datAdultAnalysis = data.frame(datDemo, RASTotalScoreF1, RASTotalScoreF2, RASTotalScoreF3, RASTotalScoreF5, INQTotalScoreF1, INQTotalScoreF2, SISTotalScoreF1, SISTotalScoreF2, SSMITotalScore)
dim(datAdultAnalysis)

#Need code gender, race, sexual orientation, edu, employment, RelationshipStatus as binary
#Gender: 2 = 1, 1 = 0 female
#Race: 7 = 0, all else 1 non-white
#Sex Orien: 3 = 0, all else 1 sexual minrotiry
#Edu: 2 = 1, all else = 0; high school over lower for one
#Employment 1 = 1 else = 0; unemployed versus everyone else
#Relationship Status: 1,2,3,4 = 1 else = 0 single


datAdultAnalysis$Gender = ifelse(datAdultAnalysis$Gender == 2,1, 0)
datAdultAnalysis$Race = ifelse(datAdultAnalysis$Race == 7,0, 1)
datAdultAnalysis$SexualOrientation = ifelse(datAdultAnalysis$SexualOrientation == 3,0, 1)
datAdultAnalysis$Edu = ifelse(datAdultAnalysis$Edu <= 2,1, 0)
datAdultAnalysis$Employment = ifelse(datAdultAnalysis$Employment == 1,1, 0)

datAdultAnalysis$RelationshipStatus = ifelse(datAdultAnalysis$RelationshipStatus <= 4, 1, 0)
describe.factor(datAdultAnalysis$RelationshipStatus)
# For the complete data set I need to drop SIS, because there is a ton of missing data
dim(datAdultAnalysis)
describe.factor(datAdultAnalysis$SISTotalScoreF2)
datAdultAnalysis$SISTotalScoreF2 = NULL
 
```
Missing data
```{r}

datAdultAnalysisComplete = na.omit(datAdultAnalysis)
dim(datAdultAnalysisComplete)[1]

## Getting the percentage of data missing for each variable across
dim(datAdultAnalysis)[1]
## Calculating how much data is missing
1-(dim(datAdultAnalysisComplete)[1]/dim(datAdultAnalysis)[1])

### Get number of people at each time point
datAdultAnalysisCompletePre = subset(datAdultAnalysisComplete, Time == 0) 

dim(datAdultAnalysisCompletePre)

datAdultAnalysisCompletePost = subset(datAdultAnalysisComplete, Time == 1) 
dim(datAdultAnalysisCompletePost)

### With missing
describe.factor(datAdultAnalysis$Time)


describe.factor(datAdultAnalysis$ID)

datAdultAnalysis = datAdultAnalysis[order(datAdultAnalysis$ID),]


library(MissMech)

head(datAdultAnalysis)
dim(datAdultAnalysis)
TestMCARNormality(datAdultAnalysis)
```


#####################
Checking descriptives at each time point
#####################
```{r}
datAdultAnalysisBase = subset(datAdultAnalysis, Time == 0)
dim(datAdultAnalysisBase)
describe(datAdultAnalysisBase)
describe.factor(datAdultAnalysisBase$Gender)
describe.factor(datAdultAnalysisBase$Race)
describe.factor(datAdultAnalysisBase$RelationshipStatus)
describe.factor(datAdultAnalysisBase$Edu)
describe.factor(datAdultAnalysisBase$Employment)
describe.factor(datAdultAnalysisBase$Treatment)

round(apply(datAdultAnalysisBase, 2, sd, na.rm = TRUE),2)


# Post
datAdultAnalysisPost = subset(datAdultAnalysis, Time == 1)
dim(datAdultAnalysisPost)
describe(datAdultAnalysisPost)
describe.factor(datAdultAnalysisPost$Gender)
describe.factor(datAdultAnalysisPost$Race)
describe.factor(datAdultAnalysisPost$RelationshipStatus)
describe.factor(datAdultAnalysisPost$Edu)
describe.factor(datAdultAnalysisPost$Employment)

round(apply(datAdultAnalysisPost, 2, sd, na.rm = TRUE),2)


# Get percentage of missing values
library(ForImp)
missingness(datAdultAnalysis)
```
Just see if you can check out the mean differences over time for each treatment
```{r}
compmeans(datAdultAnalysis$RASTotalScoreF1, datAdultAnalysis$Time)
```


Here I am checking the randomization. Using three logisitic regression comparing T1 versus T2 across covariates at baseline, then T1 versus T3 and finally T2 versus T3.

There is significant in relationship status between treatment two and three for relationship status.  Single people are more likely to be in treatment two relative to treatement three 
```{r}
datAdultRandomT12 = subset(datAdultAnalysisComplete, Time == 0 & Treatment == 1  | Treatment == 2)
datAdultRandomT12$Treatment = factor(datAdultRandomT12$Treatment)
datAdultRandomT12$Treatment == ifelse(datAdultRandomT12$Treatment == 1, 1, 0)

modelT12 = glm(Treatment ~  Age + Gender + Race + SexualOrientation + RelationshipStatus + Edu + Employment + RASTotalScoreF1+ RASTotalScoreF2+ RASTotalScoreF3+ RASTotalScoreF5+ INQTotalScoreF1+ INQTotalScoreF2+ SSMITotalScore+ SISTotalScoreF1, family = "binomial", data = datAdultRandomT12)

summary(modelT12)

datAdultRandomT13 = subset(datAdultAnalysisComplete, Time == 0 & Treatment == 1  | Treatment == 3)
datAdultRandomT13$Treatment = factor(datAdultRandomT13$Treatment)
datAdultRandomT13$Treatment == ifelse(datAdultRandomT13$Treatment == 1, 1, 0)


modelT13 = glm(Treatment ~  Age + Gender + Race + SexualOrientation + RelationshipStatus + Edu + Employment + RASTotalScoreF1+ RASTotalScoreF2+ RASTotalScoreF3+ RASTotalScoreF5+ INQTotalScoreF1+ INQTotalScoreF2+ SSMITotalScore+ SISTotalScoreF1, data = datAdultRandomT13, family = "binomial")

summary(modelT13)


datAdultRandomT23 = subset(datAdultAnalysisComplete, Time == 0 & Treatment == 2  | Treatment == 3)
datAdultRandomT23$Treatment = factor(datAdultRandomT23$Treatment)
datAdultRandomT23$Treatment == ifelse(datAdultRandomT23$Treatment == 2, 1, 0)


modelT23 = glm(Treatment ~  Age + Gender + Race + SexualOrientation + RelationshipStatus + Edu + Employment + RASTotalScoreF1+ RASTotalScoreF2+ RASTotalScoreF3+ RASTotalScoreF5+ INQTotalScoreF1+ INQTotalScoreF2+ SSMITotalScore+ SISTotalScoreF1, data = datAdultRandomT23, family = "binomial")

summary(modelT23)
```
################################################
Multilevel with treatment only with imputed data
################################################
Regular, standardized, percentage change and sens
```{r, include=FALSE}
dim(datAdultAnalysisComplete)

### Scale the outcomes
head(datAdultAnalysisComplete[,11:18])
scale_outcomes = scale(datAdultAnalysisComplete[,11:18])
colMeans(scale_outcomes)
apply(scale_outcomes, 2, sd)

### Get the log of the outcomes
head(datAdultAnalysisComplete[,11:18])

log_outcomes= log(datAdultAnalysisComplete[,11:18])

datAdultAnalysisComplete = data.frame(datAdultAnalysisComplete, scale_outcomes, log_outcomes)

datAnalysisT1 = subset(datAdultAnalysisComplete, Treatment == 1)
datAnalysisT2 = subset(datAdultAnalysisComplete, Treatment == 2)
datAnalysisT3 = subset(datAdultAnalysisComplete, Treatment == 3)

head(datAdultAnalysisComplete$SSMITotalScore.2)

```

###############
RASF1 Time and T1
###############
```{r, echo=FALSE}
output_reg = lmer(RASTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_reg)
confint(output_reg)
output_stand = lmer(RASTotalScoreF1.1 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_stand)
output_log = lmer(RASTotalScoreF1.2 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(RASTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT1)
konfound(output_reg, Time)
```

###############
RASF1 Time and T2
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(RASTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_reg)
confint(output_reg)
output_stand = lmer(RASTotalScoreF1.1 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_stand)
output_log = lmer(RASTotalScoreF1.2 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(RASTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT2)
konfound(output_reg, Time)

```
###############
RASF1 Time and T3
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(RASTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_reg)
confint(output_reg)
output_stand = lmer(RASTotalScoreF1.1 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_stand)
output_log = lmer(RASTotalScoreF1.2 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(RASTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT3)
konfound(output_reg, Time)
```
###############
RASF2 Time and T1
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(RASTotalScoreF2 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_reg)
confint(output_reg)
output_stand = lmer(RASTotalScoreF2.1 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_stand)
output_log = lmer(RASTotalScoreF2.2 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(RASTotalScoreF2 ~ Time + (1 | ID), data  = datAnalysisT1)
konfound(output_reg, Time)
```
###############
RASF2 Time and T2
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(RASTotalScoreF2 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_reg)
confint(output_reg)
output_stand = lmer(RASTotalScoreF2.1 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_stand)
output_log = lmer(RASTotalScoreF2.2 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(RASTotalScoreF2 ~ Time + (1 | ID), data  = datAnalysisT2)
konfound(output_reg, Time)
```

###############
RASF2 Time and T3
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(RASTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_reg)
confint(output_reg)
output_stand = lmer(RASTotalScoreF1.1 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_stand)
output_log = lmer(RASTotalScoreF1.2 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(RASTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT3)
konfound(output_reg, Time)
```
###############
RASF3 Time and T1
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(RASTotalScoreF3 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_reg)
confint(output_reg)
output_stand = lmer(RASTotalScoreF3.1 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_stand)
output_log = lmer(RASTotalScoreF3.2 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(RASTotalScoreF3 ~ Time + (1 | ID), data  = datAnalysisT1)
konfound(output_reg, Time)
```
###############
RASF3 Time and T2
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(RASTotalScoreF3 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_reg)
confint(output_reg)
output_stand = lmer(RASTotalScoreF3.1 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_stand)
output_log = lmer(RASTotalScoreF3.2 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(RASTotalScoreF3 ~ Time + (1 | ID), data  = datAnalysisT2)
konfound(output_reg, Time)
```
###############
RASF3 Time and T3
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(RASTotalScoreF3 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_reg)
confint(output_reg)
output_stand = lmer(RASTotalScoreF3.1 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_stand)
output_log = lmer(RASTotalScoreF3.2 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(RASTotalScoreF3 ~ Time + (1 | ID), data  = datAnalysisT3)
konfound(output_reg, Time)
```

###############
RASF5 Time and T1
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(RASTotalScoreF5 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_reg)
confint(output_reg)
output_stand = lmer(RASTotalScoreF5.1 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_stand)
output_log = lmer(RASTotalScoreF5.2 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(RASTotalScoreF5 ~ Time + (1 | ID), data  = datAnalysisT1)
konfound(output_reg, Time)
```
###############
RASF5 Time and T2
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(RASTotalScoreF5 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_reg)
confint(output_reg)
output_stand = lmer(RASTotalScoreF5.1 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_stand)
output_log = lmer(RASTotalScoreF5.2 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(RASTotalScoreF5 ~ Time + (1 | ID), data  = datAnalysisT2)
konfound(output_reg, Time)
```
###############
RASF5 Time and T3
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(RASTotalScoreF5 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_reg)
confint(output_reg)
output_stand = lmer(RASTotalScoreF5.1 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_stand)
output_log = lmer(RASTotalScoreF5.2 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(RASTotalScoreF5 ~ Time + (1 | ID), data  = datAnalysisT3)
konfound(output_reg, Time)

```
###############
INQF1 Time and T1
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(INQTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_reg)
confint(output_reg)
output_stand = lmer(INQTotalScoreF1.1 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_stand)
output_log = lmer(INQTotalScoreF1.2 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(INQTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT1)
konfound(output_reg, Time)
```

###############
INQF1 Time and T2
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(INQTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_reg)
confint(output_reg)
output_stand = lmer(INQTotalScoreF1.1 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_stand)
output_log = lmer(INQTotalScoreF1.2 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(INQTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT2)
konfound(output_reg, Time)
```
###############
INQF1 Time and T3
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(INQTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_reg)
confint(output_reg)
output_stand = lmer(INQTotalScoreF1.1 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_stand)
output_log = lmer(INQTotalScoreF1.2 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(INQTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT3)
konfound(output_reg, Time)
```
###############
INQF2 Time and T1
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(INQTotalScoreF2 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_reg)
confint(output_reg)
output_stand = lmer(INQTotalScoreF2.1 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_stand)
output_log = lmer(INQTotalScoreF2.2 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(INQTotalScoreF2 ~ Time + (1 | ID), data  = datAnalysisT1)
konfound(output_reg, Time)
```

###############
INQF2 Time and T2
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(INQTotalScoreF2 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_reg)
confint(output_reg)
output_stand = lmer(INQTotalScoreF2.1 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_stand)
output_log = lmer(INQTotalScoreF2.2 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(INQTotalScoreF2 ~ Time + (1 | ID), data  = datAnalysisT2)
konfound(output_reg, Time)
```
###############
INQF2 Time and T3
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(INQTotalScoreF2 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_reg)
confint(output_reg)
output_stand = lmer(INQTotalScoreF2.1 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_stand)
output_log = lmer(INQTotalScoreF2.2 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(INQTotalScoreF2 ~ Time + (1 | ID), data  = datAnalysisT3)
konfound(output_reg, Time)
```
###############
SSMI Time and T1
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(SSMITotalScore ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_reg)
confint(output_reg)
output_stand = lmer(SSMITotalScore.1 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_stand)
output_log = lmer(SSMITotalScore.2 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(SSMITotalScore ~ Time + (1 | ID), data  = datAnalysisT1)
konfound(output_reg, Time)
```
###############
SSMI Time and T2
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(SSMITotalScore ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_reg)
confint(output_reg)
output_stand = lmer(SSMITotalScore.1 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_stand)
output_log = lmer(SSMITotalScore.2 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(SSMITotalScore ~ Time + (1 | ID), data  = datAnalysisT2)
konfound(output_reg, Time)
```
###############
SSMI Time and T3
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(SSMITotalScore ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_reg)
confint(output_reg)
output_stand = lmer(SSMITotalScore.1 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_stand)
output_log = lmer(SSMITotalScore.2 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(SSMITotalScore ~ Time + (1 | ID), data  = datAnalysisT3)
konfound(output_reg, Time)
```
###############
SISF1 Time and T1
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(SISTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_reg)
confint(output_reg)
output_stand = lmer(SISTotalScoreF1.1 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_stand)
output_log = lmer(SISTotalScoreF1.2 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(SISTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT1)
konfound(output_reg, Time)
```
###############
SISF1 Time and T2
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(SISTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_reg)
confint(output_reg)
output_stand = lmer(SISTotalScoreF1.1 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_stand)
output_log = lmer(SISTotalScoreF1.2 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(SISTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT2)
konfound(output_reg, Time)
```

###############
SISF1 Time and T3
###############
```{r, echo=FALSE}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(SISTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_reg)
confint(output_reg)
output_stand = lmer(SISTotalScoreF1.1 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_stand)
output_log = lmer(SISTotalScoreF1.2 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_log)



uninstall.packages("lmerTest")
output_reg = lmer(SISTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT3)
konfound(output_reg, Time)
```
##################################
All treatmentments diff Score RASF1
##################################
```{r}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(RASTotalScoreF1 ~ SexualOrientation + Time*factor(Treatment) +  + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg)
confint(output_reg)

output_reg_stand = lmer(RASTotalScoreF1.1 ~ SexualOrientation + Time*factor(Treatment) +  + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg_stand)

output_reg_log = lmer(RASTotalScoreF1.2 ~ SexualOrientation + Time*factor(Treatment) +  + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg_log)

```
RASF1 Contrasts 
```{r}
K = matrix(c(0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 1, -1), ncol = 7, nrow = 2, byrow = TRUE)
t = glht(output_reg, linfct = K)
t_sum = summary(t)
t_sum
confint(t)

t_stand = glht(output_reg_stand, linfct = K)
t_stand_sum = summary(t_stand)
t_stand_sum

t_log = glht(output_reg_log, linfct = K)
t_log_sum = summary(t_log)
t_log_sum

```

##################################
All treatmentments diff Score RASF2
##################################
```{r}
output_reg = lmer(RASTotalScoreF2 ~ SexualOrientation + Time*factor(Treatment) +  + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg)
confint(output_reg)

output_reg_stand = lmer(RASTotalScoreF2.1 ~ SexualOrientation + Time*factor(Treatment) +  + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg_stand)

output_reg_log = lmer(RASTotalScoreF2.2 ~ SexualOrientation + Time*factor(Treatment) +  + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg_log)

```
RASF2 Contrasts 
```{r}
K = matrix(c(0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 1, -1), ncol = 7, nrow = 2, byrow = TRUE)
t = glht(output_reg, linfct = K)
t_sum = summary(t)
t_sum
confint(t)

t_stand = glht(output_reg_stand, linfct = K)
t_stand_sum = summary(t_stand)
t_stand_sum

t_log = glht(output_reg_log, linfct = K)
t_log_sum = summary(t_log)
t_log_sum

```

##################################
All treatmentments diff Score RASF3
##################################
```{r}
output_reg = lmer(RASTotalScoreF3 ~ SexualOrientation + Time*factor(Treatment) +  + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg)
confint(output_reg)

output_reg_stand = lmer(RASTotalScoreF3.1 ~ SexualOrientation + Time*factor(Treatment) +  + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg_stand)

output_reg_log = lmer(RASTotalScoreF3.2 ~ SexualOrientation + Time*factor(Treatment) +  + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg_log)

```
RASF2 Contrasts 
```{r}
K = matrix(c(0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 1, -1), ncol = 7, nrow = 2, byrow = TRUE)
t = glht(output_reg, linfct = K)
t_sum = summary(t)
t_sum
confint(t)

t_stand = glht(output_reg_stand, linfct = K)
t_stand_sum = summary(t_stand)
t_stand_sum

t_log = glht(output_reg_log, linfct = K)
t_log_sum = summary(t_log)
t_log_sum

```
##################################
All treatmentments diff Score RASF5
##################################
```{r}
output_reg = lmer(RASTotalScoreF5 ~ SexualOrientation + Time*factor(Treatment) +  + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg)
confint(output_reg)

output_reg_stand = lmer(RASTotalScoreF5.1 ~ SexualOrientation + Time*factor(Treatment)   + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg_stand)

output_reg_log = lmer(RASTotalScoreF5.2 ~ SexualOrientation + Time*factor(Treatment)  + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg_log)

uninstall.packages("lmerTest")
output_reg = lmer(RASTotalScoreF5 ~ SexualOrientation + Time*factor(Treatment) +  + (1 | ID), data  = datAdultAnalysisComplete)

konfound(output_reg, "Time:factor(Treatment)3")

```
RASF5 Contrasts 
```{r}
K = matrix(c(0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 1, -1), ncol = 7, nrow = 2, byrow = TRUE)
t = glht(output_reg, linfct = K)
t_sum = summary(t)
t_sum
confint(t)

t_stand = glht(output_reg_stand, linfct = K)
t_stand_sum = summary(t_stand)
t_stand_sum

t_log = glht(output_reg_log, linfct = K)
t_log_sum = summary(t_log)
t_log_sum
```
##################################
All treatmentments diff Score INQTotalScoreF1
##################################
```{r}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(INQTotalScoreF1 ~ SexualOrientation + Time*factor(Treatment) +  + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg)
confint(output_reg)

output_reg_stand = lmer(INQTotalScoreF1.1 ~ SexualOrientation + Time*factor(Treatment)   + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg_stand)

output_reg_log = lmer(INQTotalScoreF1.2 ~ SexualOrientation + Time*factor(Treatment)  + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg_log)

```
RASF5 Contrasts 
```{r}
K = matrix(c(0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 1, -1), ncol = 7, nrow = 2, byrow = TRUE)
t = glht(output_reg, linfct = K)
t_sum = summary(t)
t_sum
confint(t)

t_stand = glht(output_reg_stand, linfct = K)
t_stand_sum = summary(t_stand)
t_stand_sum

t_log = glht(output_reg_log, linfct = K)
t_log_sum = summary(t_log)
t_log_sum
```


##################################
All treatmentments diff Score INQTotalScoreF2
##################################
```{r}
install.packages("lmerTest")
library(lmerTest)
output_reg = lmer(INQTotalScoreF2 ~ SexualOrientation + Time*factor(Treatment) +  + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg)
confint(output_reg)

output_reg_stand = lmer(INQTotalScoreF2.1 ~ SexualOrientation + Time*factor(Treatment)   + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg_stand)

output_reg_log = lmer(INQTotalScoreF2.2 ~ SexualOrientation + Time*factor(Treatment)  + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg_log)

```
INQTotalScoreF2 Contrasts 
```{r}
K = matrix(c(0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 1, -1), ncol = 7, nrow = 2, byrow = TRUE)
t = glht(output_reg, linfct = K)
t_sum = summary(t)
t_sum
confint(t)

t_stand = glht(output_reg_stand, linfct = K)
t_stand_sum = summary(t_stand)
t_stand_sum

t_log = glht(output_reg_log, linfct = K)
t_log_sum = summary(t_log)
t_log_sum
```
##################################
All treatmentments diff Score SSMITotalScore
##################################
```{r}
output_reg = lmer(SSMITotalScore ~ SexualOrientation + Time*factor(Treatment) +  + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg)
confint(output_reg)

output_reg_stand = lmer(SSMITotalScore.1 ~ SexualOrientation + Time*factor(Treatment)   + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg_stand)

output_reg_log = lmer(SSMITotalScore.2 ~ SexualOrientation + Time*factor(Treatment)  + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg_log)

```
SSMITotalScore Contrasts 
```{r}
K = matrix(c(0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 1, -1), ncol = 7, nrow = 2, byrow = TRUE)
t = glht(output_reg, linfct = K)
t_sum = summary(t)
t_sum
confint(t)

t_stand = glht(output_reg_stand, linfct = K)
t_stand_sum = summary(t_stand)
t_stand_sum

t_log = glht(output_reg_log, linfct = K)
t_log_sum = summary(t_log)
t_log_sum
```
##################################
All treatmentments diff Score SISTotalScoreF1
##################################
```{r}
output_reg = lmer(SISTotalScoreF1 ~ SexualOrientation + Time*factor(Treatment) +  + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg)
confint(output_reg)

output_reg_stand = lmer(SISTotalScoreF1.1 ~ SexualOrientation + Time*factor(Treatment)   + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg_stand)

output_reg_log = lmer(SISTotalScoreF1.2 ~ SexualOrientation + Time*factor(Treatment)  + (1 | ID), data  = datAdultAnalysisComplete)
summary(output_reg_log)

```
SISTotalScoreF1 Contrasts 
```{r}
K = matrix(c(0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 1, -1), ncol = 7, nrow = 2, byrow = TRUE)
t = glht(output_reg, linfct = K)
t_sum = summary(t)
t_sum
confint(t)

t_stand = glht(output_reg_stand, linfct = K)
t_stand_sum = summary(t_stand)
t_stand_sum

t_log = glht(output_reg_log, linfct = K)
t_log_sum = summary(t_log)
t_log_sum
```


