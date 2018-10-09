---
title: "Psychometrics Gatekeeper Prelim Results"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
################
Data cleaning
################
```{r}
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
library(robustlmm)
library(jtools)
library(lmtest)
library(lmerTest)
library(MuMIn)
library(HLMdiag)
library(Hmisc)
library(stargazer)

setwd("P:/Evaluation/TN Lives Count_Writing/3_Target1_SUICClinicalTrainingComparison/3_Data & Analyses")
datPre = read.csv("Pre.csv", header = FALSE, row.names = NULL)

datPre = t(datPre)
write.csv(datPre, "datPre.csv", row.names = FALSE)
datPre = read.csv("datPre.csv", header = TRUE)
head(datPre)
dim(datPre)

datPre = datPre[,c(1, 3, 7:18, 21:35, 38:45, 49:72, 78:80, 83, 85, 94)]
datPre = data.frame(datPre)
head(datPre)

colnames(datPre) = c("ID", "Treatment", "Sec1Qa", "Sec1Qb", "Sec1Qc", "Sec1Qd", "Sec1Qe", "Sec1Qf", "Sec1Qg", "Sec1Qh", "Sec1Qi", "Sec1Qj", "Sec1Qk", "Sec1Ql", "Sec2Qa", "Sec2Qb", "Sec2Qc", "Sec2Qd", "Sec2Qe", "Sec2Qf", "Sec2Qg",  "Sec2Qh", "Sec2Qi", "Sec2Qj", "Sec2Qk", "Sec2Ql", "Sec2Qm", "Sec2Qn", "Sec2Qo", "Sec3Qa", "Sec3Qb", "Sec3Qc", "Sec3Qd", "Sec3Qe", "Sec3Qf", "Sec3Qg", "Sec3Qh", "Sec4QaA", "Sec4QaB", "Sec4QbA", "Sec4QbB", "Sec4QcA", "Sec4QcB", "Sec4QdA", "Sec4QdB", "Sec4QeA", "Sec4QeB", "Sec4QfA","Sec4QfB", "Sec4QgA", "Sec4QgB", "Sec4QhA", "Sec4QhB", "Sec4QiA", "Sec4QiB", "Sec4QjA", "Sec4QjB", "Sec4QkA", "Sec4QkB", "Sec4QlA", "Sec4QlB","Age", "Gender", "Eth", "Race", "Edu", "Clinical_Staff")
head(datPre)
### Get rif of first row once you figure out which variables to keep
datPre = datPre[-1,]
datPre = data.frame(datPre)
head(datPre)

#Only retain clincial staff 1
datPre = subset(datPre, Clinical_Staff == 1)
head(datPre)
dim(datPre)

setwd("P:/Evaluation/TN Lives Count_Writing/3_Target1_SUICClinicalTrainingComparison/3_Data & Analyses")
datPost = read.csv("Post.csv", header = FALSE, row.names = NULL)

datPost = t(datPost)
write.csv(datPost, "datPost.csv", row.names = FALSE)
datPost = read.csv("datPost.csv", header = TRUE)
head(datPost)

datPost = datPost[,c(3, 5, 15:26, 29:43, 46:53, 57:80)]

datPost = data.frame(datPost)

colnames(datPost) = c("ID", "Treatment", "Sec1Qa", "Sec1Qb", "Sec1Qc", "Sec1Qd", "Sec1Qe", "Sec1Qf", "Sec1Qg", "Sec1Qh", "Sec1Qi", "Sec1Qj", "Sec1Qk", "Sec1Ql", "Sec2Qa", "Sec2Qb", "Sec2Qc", "Sec2Qd", "Sec2Qe", "Sec2Qf", "Sec2Qg",  "Sec2Qh", "Sec2Qi", "Sec2Qj", "Sec2Qk", "Sec2Ql", "Sec2Qm", "Sec2Qn", "Sec2Qo", "Sec3Qa", "Sec3Qb", "Sec3Qc", "Sec3Qd", "Sec3Qe", "Sec3Qf", "Sec3Qg", "Sec3Qh", "Sec4QaA", "Sec4QaB", "Sec4QbA", "Sec4QbB", "Sec4QcA", "Sec4QcB", "Sec4QdA", "Sec4QdB", "Sec4QeA", "Sec4QeB", "Sec4QfA","Sec4QfB", "Sec4QgA", "Sec4QgB", "Sec4QhA", "Sec4QhB", "Sec4QiA", "Sec4QiB", "Sec4QjA", "Sec4QjB", "Sec4QkA", "Sec4QkB", "Sec4QlA", "Sec4QlB")
head(datPost)

# making treatment null, because it does not change and it will make so it doesn't repeat later 
datPost$ID = as.factor(datPost$ID)
datPost = datPost[-1,]
datPost$Treatment = NULL
datPost = data.frame(datPost)

dim(datPre)
dim(datPost)


write.csv(datPre, "datPre.csv", row.names = FALSE)
datPre = read.csv("datPre.csv", header = TRUE)

write.csv(datPost, "datPost.csv", row.names = FALSE)
datPost = read.csv("datPost.csv", header = TRUE)

#Should not have ID one, because they are the wrong code and not in datPre
datPrePost = merge(datPre, datPost, by = "ID",  all.x = TRUE, sort = TRUE)



dat3month = read.csv("3month.csv", header  = TRUE)
head(dat3month)
dat3month = dat3month[c(7, 11:22, 23:69)]
dim(datPost)
head(datPost)

# Now rename everything 
colnames(dat3month) = c("ID", "Sec1Qa", "Sec1Qb", "Sec1Qc", "Sec1Qd", "Sec1Qe", "Sec1Qf", "Sec1Qg", "Sec1Qh", "Sec1Qi", "Sec1Qj", "Sec1Qk", "Sec1Ql", "Sec2Qa", "Sec2Qb", "Sec2Qc", "Sec2Qd", "Sec2Qe", "Sec2Qf", "Sec2Qg",  "Sec2Qh", "Sec2Qi", "Sec2Qj", "Sec2Qk", "Sec2Ql", "Sec2Qm", "Sec2Qn", "Sec2Qo", "Sec3Qa", "Sec3Qb", "Sec3Qc", "Sec3Qd", "Sec3Qe", "Sec3Qf", "Sec3Qg", "Sec3Qh", "Sec4QaA", "Sec4QaB", "Sec4QbA", "Sec4QbB", "Sec4QcA", "Sec4QcB", "Sec4QdA", "Sec4QdB", "Sec4QeA", "Sec4QeB", "Sec4QfA","Sec4QfB", "Sec4QgA", "Sec4QgB", "Sec4QhA", "Sec4QhB", "Sec4QiA", "Sec4QiB", "Sec4QjA", "Sec4QjB", "Sec4QkA", "Sec4QkB", "Sec4QlA", "Sec4QlB")


datPrePost3month = merge(datPrePost, dat3month, by = "ID", all.x = TRUE, sort = TRUE)

head(datPrePost3month)


### Now make long format
### These variables are not included: 							

datPrePost3month = reshape(datPrePost3month, varying  = list(c("Sec1Qa.x", "Sec1Qa.y", "Sec1Qa"), c("Sec1Qb.x", "Sec1Qb.y", "Sec1Qb"), c("Sec1Qc.x", "Sec1Qc.y", "Sec1Qc"), c("Sec1Qd.x", "Sec1Qd.y", "Sec1Qd"), c("Sec1Qe.x", "Sec1Qe.y", "Sec1Qe"), c("Sec1Qf.x", "Sec1Qf.y", "Sec1Qf"), c("Sec1Qg.x", "Sec1Qg.y", "Sec1Qg"), c("Sec1Qh.x", "Sec1Qh.y", "Sec1Qh"), c("Sec1Qi.x", "Sec1Qi.y", "Sec1Qi"), c("Sec1Qj.x", "Sec1Qj.y", "Sec1Qj"), c("Sec1Qk.x", "Sec1Qk.y", "Sec1Qk"), c("Sec1Ql.x", "Sec1Ql.y", "Sec1Ql"), c("Sec2Qa.x", "Sec2Qa.y", "Sec2Qa"), c("Sec2Qb.x", "Sec2Qb.y", "Sec2Qb"), c("Sec2Qc.x", "Sec2Qc.y", "Sec2Qc"), c("Sec2Qd.x", "Sec2Qd.y", "Sec2Qd"), c("Sec2Qe.x", "Sec2Qe.y", "Sec2Qe"), c("Sec2Qf.x", "Sec2Qf.y", "Sec2Qf"), c("Sec2Qg.x", "Sec2Qg.y", "Sec2Qg"), c("Sec2Qh.x", "Sec2Qh.y", "Sec2Qh"), c("Sec2Qi.x", "Sec2Qi.y", "Sec2Qi"), c("Sec2Qj.x", "Sec2Qj.y", "Sec2Qj"), c("Sec2Qk.x", "Sec2Qk.y", "Sec2Qk"), c("Sec2Ql.x", "Sec2Ql.y", "Sec2Ql"), c("Sec2Qm.x", "Sec2Qm.y", "Sec2Qm"), c("Sec2Qn.x", "Sec2Qn.y", "Sec2Qn"), c("Sec2Qo.x", "Sec2Qo.y", "Sec2Qo"), c("Sec3Qa.x", "Sec3Qa.y","Sec3Qa"), c("Sec3Qb.x", "Sec3Qb.y", "Sec3Qb"), c("Sec3Qc.x", "Sec3Qc.y", "Sec3Qc"), c("Sec3Qd.x", "Sec3Qd.y", "Sec3Qd"), c("Sec3Qe.x", "Sec3Qe.y", "Sec3Qe"), c("Sec3Qf.x", "Sec3Qf.y", "Sec3Qf"), c("Sec3Qg.x", "Sec3Qg.y", "Sec3Qg"), c("Sec3Qh.x", "Sec3Qh.y", "Sec3Qh"), c("Sec4QaA.x", "Sec4QaA.y", "Sec4QaA"), c("Sec4QaB.x", "Sec4QaB.y", "Sec4QaB"), c("Sec4QbA.x", "Sec4QbA.y", "Sec4QbA"), c("Sec4QbB.x", "Sec4QbB.y", "Sec4QbB"), c("Sec4QcA.x", "Sec4QcA.y", "Sec4QcA"), c("Sec4QcB.x", "Sec4QcB.y", "Sec4QcB"), c("Sec4QdA.x", "Sec4QdA.y", "Sec4QdA"), c("Sec4QdB.x", "Sec4QdB.y", "Sec4QdB"), c("Sec4QeA.x", "Sec4QeA.y", "Sec4QeA"), c("Sec4QeB.x", "Sec4QeB.y", "Sec4QeB"), c("Sec4QfA.x", "Sec4QfA.y", "Sec4QfA"), c("Sec4QfB.x", "Sec4QfB.y", "Sec4QfB"), c("Sec4QgA.x", "Sec4QgA.y", "Sec4QgA"), c("Sec4QgB.x", "Sec4QgB.y", "Sec4QgB"), c("Sec4QhA.x", "Sec4QhA.y", "Sec4QhA"), c("Sec4QhB.x", "Sec4QhB.y", "Sec4QhB"), c("Sec4QiA.x", "Sec4QiA.y", "Sec4QiA"), c("Sec4QiB.x", "Sec4QiB.y", "Sec4QiB"), c("Sec4QjA.x", "Sec4QjA.y", "Sec4QjA"), c("Sec4QjB.x", "Sec4QjB.y", "Sec4QjB"), c("Sec4QkA.x", "Sec4QkA.y", "Sec4QkA"), c("Sec4QkB.x", "Sec4QkB.y", "Sec4QkB"), c("Sec4QlA.x", "Sec4QlA.y", "Sec4QlA"), c("Sec4QlB.x", "Sec4QlB.y", "Sec4QlB")), direction = "long", times =c(0,1,2))

head(datPrePost3month)


write.csv(datPrePost3month, "datPrePost3month.csv", row.names = FALSE)
datPrePost3month = read.csv("datPrePost3month.csv", header = TRUE)




describe.factor(datPrePost3month$Sec1Qf.x)
datPrePost3month$Sec1Qf.x = as.factor(datPrePost3month$Sec1Qf.x)
describe.factor(datPrePost3month$Sec1Qf.x)
datPrePost3month$Sec1Qf.x = ifelse(datPrePost3month$Sec1Qf.x== 5, NA, datPrePost3month$Sec1Qf.x)
describe.factor(datPrePost3month$Sec1Qf.x)
datPrePost3month$Sec1Qf.x = ifelse(datPrePost3month$Sec1Qf.x == 2,1,0)
describe.factor(datPrePost3month$Sec1Qf.x)

describe.factor(datPrePost3month$Sec1Qi.x)
datPrePost3month$Sec1Qi.x = ifelse(datPrePost3month$Sec1Qi.x == 5, NA, datPrePost3month$Sec1Qi.x)
datPrePost3month$Sec1Qi.x = ifelse(datPrePost3month$Sec1Qi.x == 9, NA, datPrePost3month$Sec1Qi.x)
describe.factor(datPrePost3month$Sec1Qi.x)



describe.factor(datPrePost3month$Sec1Qg.x)
datPrePost3month$Sec1Qg.x = ifelse(datPrePost3month$Sec1Qg.x == 3, NA, datPrePost3month$Sec1Qg.x)
describe.factor(datPrePost3month$Sec1Qg.x)



describe.factor(datPrePost3month$Sec1Qh.x)
datPrePost3month$Sec1Qh.x = ifelse(datPrePost3month$Sec1Qh.x == 4, NA, datPrePost3month$Sec1Qh.x)
describe.factor(datPrePost3month$Sec1Qh.x)

describe.factor(datPrePost3month$Sec1Qk.x) 
datPrePost3month$Sec1Qk.x =ifelse(datPrePost3month$Sec1Qk.x == 5, NA, datPrePost3month$Sec1Qk.x)
describe.factor(datPrePost3month$Sec1Qk.x)


describe.factor(datPrePost3month$Sec2Qf.x)
datPrePost3month$Sec2Qf.x = ifelse(datPrePost3month$Sec2Qf.x == 0, NA, datPrePost3month$Sec2Qf.x)
describe.factor(datPrePost3month$Sec2Qf.x)



describe.factor(datPrePost3month$Sec1Ql.x)
datPrePost3month$Sec1Ql.x = ifelse(datPrePost3month$Sec1Ql.x > 1, NA, datPrePost3month$Sec1Ql.x)
describe.factor(datPrePost3month$Sec1Ql.x)


describe.factor(datPrePost3month$Sec2Qo.x)
datPrePost3month$Sec2Qo.x = ifelse(datPrePost3month$Sec2Qo.x == 0, NA, datPrePost3month$Sec2Qo.x)
describe.factor(datPrePost3month$Sec2Qo.x)


# 5 = -3; 4 = -2, 3 = -1, 6=0, 7=1,  8 = 2, 1 = NA, NA = NA, 9 = 3
describe.factor(datPrePost3month$Sec4QfA.x)
datPrePost3month$Sec4QfA.x = ifelse(datPrePost3month$Sec4QfA.x == " ", NA, ifelse(datPrePost3month$Sec4QfA.x == "-", NA, datPrePost3month$Sec4QfA.x))
describe.factor(datPrePost3month$Sec4QfA.x)

datPrePost3month$Sec4QfA.x = ifelse(datPrePost3month$Sec4QfA.x == 5, -3, ifelse(datPrePost3month$Sec4QfA.x  == 4, -2, ifelse(datPrePost3month$Sec4QfA.x == 3, -1, ifelse(datPrePost3month$Sec4QfA.x == 6, 0, ifelse(datPrePost3month$Sec4QfA.x == 7, 1, ifelse(datPrePost3month$Sec4QfA.x == 8, 2, ifelse(datPrePost3month$Sec4QfA.x == 1, NA, ifelse(datPrePost3month$Sec4QfA.x == 9, 3, datPrePost3month$Sec4QfA.x ))))))))
describe.factor(datPrePost3month$Sec4QfA.x)

describe.factor(datPrePost3month$Sec4QfB.x)
datPrePost3month$Sec4QfB.x = ifelse(datPrePost3month$Sec4QfB.x == -23, NA, datPrePost3month$Sec4QfB.x)
describe.factor(datPrePost3month$Sec4QfB.x)

describe.factor(datPrePost3month$Sec4QgB.x)
datPrePost3month$Sec4QgB.x = ifelse(datPrePost3month$Sec4QgB.x == -11, NA, datPrePost3month$Sec4QgB.x)
describe.factor(datPrePost3month$Sec4QgB.x)


describe.factor(datPrePost3month$Sec4QhA.x)
datPrePost3month$Sec4QhA.x = ifelse(datPrePost3month$Sec4QhA.x == -4, NA, datPrePost3month$Sec4QhA.x)
describe.factor(datPrePost3month$Sec4QhA.x)

describe.factor(datPrePost3month$Sec4QeB.x)
datPrePost3month$Sec4QeB.x = ifelse(datPrePost3month$Sec4QeB.x == -32, NA, ifelse(datPrePost3month$Sec4QeB.x == -4, NA, datPrePost3month$Sec4QeB.x))
describe.factor(datPrePost3month$Sec4QeB.x)

describe.factor(datPrePost3month$Sec2Qh.x)
datPrePost3month$Sec2Qh.x = ifelse(datPrePost3month$Sec2Qh.x == 56, NA, datPrePost3month$Sec2Qh.x)
describe.factor(datPrePost3month$Sec2Qh.x)


datPrePost3month$Sec4QaA.x =  datPrePost3month$Sec4QaA.x--2.71
datPrePost3month$Sec4QaB.x =  datPrePost3month$Sec4QaB.x- 1.86 

datPrePost3month$Sec4QbA.x =  datPrePost3month$Sec4QbA.x--2.71
datPrePost3month$Sec4QbB.x =  datPrePost3month$Sec4QbB.x- 1.86 

datPrePost3month$Sec4QcA.x =  datPrePost3month$Sec4QcA.x--2.14
datPrePost3month$Sec4QcB.x =  datPrePost3month$Sec4QcB.x-2.14 

datPrePost3month$Sec4QdA.x =  datPrePost3month$Sec4QdA.x-1.29 
datPrePost3month$Sec4QdB.x =  datPrePost3month$Sec4QdB.x--2.71

datPrePost3month$Sec4QeA.x =  datPrePost3month$Sec4QeA.x-2.43 
datPrePost3month$Sec4QeB.x =  datPrePost3month$Sec4QeB.x--2.71 

datPrePost3month$Sec4QfA.x =  datPrePost3month$Sec4QfA.x--2 
datPrePost3month$Sec4QfB.x =  datPrePost3month$Sec4QfB.x-2.57


datPrePost3month$Sec4QgA.x =  datPrePost3month$Sec4QgA.x-2  
datPrePost3month$Sec4QgB.x =  datPrePost3month$Sec4QgB.x--1.29 

datPrePost3month$Sec4QhA.x =  datPrePost3month$Sec4QhA.x--2.29 
datPrePost3month$Sec4QhB =   datPrePost3month$Sec4QhB.x-2.14

datPrePost3month$Sec4QiA.x =  datPrePost3month$Sec4QiA.x--1.29 
datPrePost3month$Sec4QiB.x =  datPrePost3month$Sec4QiB.x-1.29  

datPrePost3month$Sec4QjA.x =  datPrePost3month$Sec4QjA.x-2.29 
datPrePost3month$Sec4QjB.x =  datPrePost3month$Sec4QjB.x--2.43  

datPrePost3month$Sec4QkA.x =  datPrePost3month$Sec4QkA.x--2.42  
datPrePost3month$Sec4QkB.x =  datPrePost3month$Sec4QkB.x-2.43 

datPrePost3month$Sec4QlA.x =  datPrePost3month$Sec4QlA.x-2.00 
datPrePost3month$Sec4QlB.x =  datPrePost3month$Sec4QlB.x-3.00 


#Now we are getting total scores for the three measures use apply and create a smaller data set.

#Then put that data set back together call it analysis at the end.


#Now we saying that if there is a missing value for any of the resposnes that the total score will be NA.  This is probably better than skipping missing values, because it could be the case that if you have only one response to one item, then that would be the total score, which would not be accurate.  

#Need to get rid of time, before the sum of the variables to be summed, because that will mess up the math and we needed time from the earlier analysis for the CFA to get only the baseline data.

#Then created dicotmoized variables
#Gender: Males = 1, Female = 0 no
##Race: White =1, other racial identity
#Edu: Bachelors or lower = 1, higher than Bachelors = 0

head(datPrePost3month)

datPrePost3monthSec1 = datPrePost3month[,c(9,10:21)]
head(datPrePost3monthSec1)
datPrePost3monthSec1Base = subset(datPrePost3monthSec1, time == 0)
describe.factor(datPrePost3monthSec1Base$time)
datPrePost3monthSec1Base$time = NULL

datPrePost3monthSec1Base = data.frame(datPrePost3monthSec1Base)
write.csv(datPrePost3monthSec1Base, "datPrePost3monthSec1Base.csv", row.names = FALSE)
datPrePost3monthSec1Base = read.csv("datPrePost3monthSec1Base.csv", header = TRUE)

head(datPrePost3month)
datPrePost3monthSec2 = datPrePost3month[,c(9, 22:36)]
head(datPrePost3monthSec2)


datPrePost3monthSec2Base = subset(datPrePost3monthSec2, time == 0)
describe.factor(datPrePost3monthSec2Base$time)
datPrePost3monthSec2Base$time = NULL

head(datPrePost3monthSec2Base)

datPrePost3monthSec2Base = data.frame(datPrePost3monthSec2Base)
write.csv(datPrePost3monthSec2Base, "datPrePost3monthSec2Base.csv", row.names = FALSE)
datPrePost3monthSec2Base = read.csv("datPrePost3monthSec2Base.csv", header = TRUE)


head(datPrePost3month)
datPrePost3monthSec3 = datPrePost3month[,c(9, 37:44)]
### Need to get reverse scoring for A,C,E,G
head(datPrePost3monthSec3)
summary(datPrePost3monthSec3)

datPrePost3monthSec3$Sec3Qa.x = ifelse(datPrePost3monthSec3$Sec3Qa.x == 1, 5, ifelse(datPrePost3monthSec3$Sec3Qa.x == 2,4, ifelse(datPrePost3monthSec3$Sec3Qa.x == 3,3, ifelse(datPrePost3monthSec3$Sec3Qa.x == 4,2, ifelse(datPrePost3monthSec3$Sec3Qa.x == 5,1,datPrePost3monthSec3$Sec3Qa.x)))))
describe.factor(datPrePost3monthSec3$Sec3Qa.x)


datPrePost3monthSec3$Sec3Qc.x = ifelse(datPrePost3monthSec3$Sec3Qc.x == 1, 5, ifelse(datPrePost3monthSec3$Sec3Qc.x == 2,4, ifelse(datPrePost3monthSec3$Sec3Qc.x == 3,3, ifelse(datPrePost3monthSec3$Sec3Qc.x == 4,2, ifelse(datPrePost3monthSec3$Sec3Qc.x == 5,1,datPrePost3monthSec3$Sec3Qc.x)))))
describe.factor(datPrePost3monthSec3$Sec3Qc.x)

datPrePost3monthSec3$Sec3Qd.x = ifelse(datPrePost3monthSec3$Sec3Qd.x == 1, 5, ifelse(datPrePost3monthSec3$Sec3Qd.x == 2,4, ifelse(datPrePost3monthSec3$Sec3Qd.x == 3,3, ifelse(datPrePost3monthSec3$Sec3Qd.x == 4,2, ifelse(datPrePost3monthSec3$Sec3Qd.x == 5,1,datPrePost3monthSec3$Sec3Qd.x)))))
describe.factor(datPrePost3monthSec3$Sec3Qd.x)


datPrePost3monthSec3$Sec3Qg.x = ifelse(datPrePost3monthSec3$Sec3Qg.x == 1, 5, ifelse(datPrePost3monthSec3$Sec3Qg.x == 2,4, ifelse(datPrePost3monthSec3$Sec3Qg.x == 3,3, ifelse(datPrePost3monthSec3$Sec3Qg.x == 4,2, ifelse(datPrePost3monthSec3$Sec3Qg.x == 5,1,datPrePost3monthSec3$Sec3Qg.x)))))
describe.factor(datPrePost3monthSec3$Sec3Qg.x)


datPrePost3monthSec3Base = subset(datPrePost3monthSec3, time == 0)
describe.factor(datPrePost3monthSec3Base$time)
datPrePost3monthSec3Base$time = NULL

datPrePost3monthSec3Base = data.frame(datPrePost3monthSec3Base)
write.csv(datPrePost3monthSec3Base, "datPrePost3monthSec3Base.csv", row.names = FALSE)
datPrePost3monthSec3Base = read.csv("datPrePost3monthSec3Base.csv", header = TRUE)
head(datPrePost3monthSec3Base)

head(datPrePost3month)
datPrePost3monthSec4 = datPrePost3month[,c(9, 45:68)]
head(datPrePost3monthSec4)

datPrePost3monthSec4Base = subset(datPrePost3monthSec4, time == 0)
describe.factor(datPrePost3monthSec4Base$time)
datPrePost3monthSec4Base$time = NULL




datPrePost3monthSec1$time = NULL
datPrePost3monthSec2$time = NULL
datPrePost3monthSec3$time = NULL
datPrePost3monthSec4$time = NULL

write.csv(datPrePost3monthSec1, "datPrePost3monthSec1.csv", row.names = FALSE)
datPrePost3monthSec1 = read.csv("datPrePost3monthSec1.csv", header = TRUE)

write.csv(datPrePost3monthSec2, "datPrePost3monthSec2.csv", row.names = FALSE)
datPrePost3monthSec2 = read.csv("datPrePost3monthSec2.csv", header = TRUE)

write.csv(datPrePost3monthSec3, "datPrePost3monthSec3.csv", row.names = FALSE)
datPrePost3monthSec3 = read.csv("datPrePost3monthSec3.csv", header = TRUE)

write.csv(datPrePost3monthSec4, "datPrePost3monthSec4.csv", row.names = FALSE)
datPrePost3monthSec4 = read.csv("datPrePost3monthSec4.csv", header = TRUE)

write.csv(datPrePost3monthSec1, "datPrePost3monthSec1.csv", row.names = FALSE)


sum(is.na(datPrePost3monthSec1))
Sec1Total = rowSums(datPrePost3monthSec1)
Sec2Total = rowSums(datPrePost3monthSec2)
Sec3Total = rowSums(datPrePost3monthSec3)
Sec4Total = rowSums(datPrePost3monthSec4)

datPrePost3monthAnalysisJen = data.frame(ID = datPrePost3month$ID, Treatment = datPrePost3month$Treatment, Age =  datPrePost3month$Age, Gender = datPrePost3month$Gender, Race = datPrePost3month$Race, Edu = datPrePost3month$Edu, Time = datPrePost3month$time,Sec1Total =  Sec1Total, Sec2Total = Sec2Total, Sec3Total = Sec3Total, Sec4Total = Sec4Total, datPrePost3monthSec1, datPrePost3monthSec2, datPrePost3monthSec3, datPrePost3monthSec4)

write.csv(datPrePost3monthAnalysisJen, "GatekeeperData.csv", row.names = FALSE)


datPrePost3monthAnalysis = data.frame(ID = datPrePost3month$ID, Treatment = datPrePost3month$Treatment, Age =  datPrePost3month$Age, Gender = datPrePost3month$Gender, Race = datPrePost3month$Race, Edu = datPrePost3month$Edu, Time = datPrePost3month$time,Sec1Total =  Sec1Total, Sec2Total = Sec2Total, Sec3Total = Sec3Total, Sec4Total = Sec4Total)

# No casese non female or male gender
describe.factor(datPrePost3monthAnalysis$Gender)
datPrePost3monthAnalysis$Gender = ifelse(datPrePost3monthAnalysis$Gender == 1,1,0)
datPrePost3monthAnalysis$Race = ifelse(datPrePost3monthAnalysis$Race == 5, 0, 1)

describe.factor(datPrePost3monthAnalysis$Edu)


datPrePost3monthAnalysis$Edu = ifelse(datPrePost3monthAnalysis$Edu < 6, 1, 0)

#datPrePost3monthAnalysisComplete = subset(datPrePost3monthAnalysis, Treatment == 1 | Treatment == 2)

# Getting the data ready
write.csv(datPrePost3monthAnalysis, "datPrePost3monthAnalysis.csv", row.names = FALSE)
datPrePost3monthAnalysis = read.csv("datPrePost3monthAnalysis.csv", header = TRUE)

# Maybe make sure time and treatment are treated as factors
datPrePost3monthAnalysis$Treatment = as.factor(datPrePost3monthAnalysis$Treatment)
datPrePost3monthAnalysis$Gender = as.factor(datPrePost3monthAnalysis$Gender)
datPrePost3monthAnalysis$Race = as.factor(datPrePost3monthAnalysis$Race)
datPrePost3monthAnalysis$Edu = as.factor(datPrePost3monthAnalysis$Edu)

head(datPrePost3monthAnalysis)

```
Assess missing values for prePost3month and prePost
```{r}
sum(is.na(datPrePost3monthAnalysis))
datPrePost3monthAnalysisComplete = na.omit(datPrePost3monthAnalysis)
dim(datPrePost3monthAnalysis)[1]
dim(datPrePost3monthAnalysisComplete)[1]

1-(dim(datPrePost3monthAnalysisComplete)[1] / dim(datPrePost3monthAnalysis)[1])

# Missing values for prePost
write.csv(datPrePost3monthAnalysis, "datPrePost3monthAnalysis.csv", row.names = FALSE)
datPrePost3monthAnalysis = read.csv("datPrePost3monthAnalysis.csv", header = TRUE)
datPrePostAnalysis = subset(datPrePost3monthAnalysis, Time  == 0 | Time == 1)
write.csv(datPrePostAnalysis, "datPrePostAnalysis.csv", row.names = FALSE)
datPrePostAnalysis = read.csv("datPrePostAnalysis.csv", header = TRUE)
describe.factor(datPrePostAnalysis$Time)

## Ok no need to impute for pre and post
datPrePostAnalysisComplete = na.omit(datPrePostAnalysis)
1-(dim(datPrePostAnalysisComplete)[1]/dim(datPrePostAnalysis)[1])
TestMCARNormality(datPrePostAnalysis)

## Missing values for pre, post, and 3month
datPrePost3monthAnalysisComplete = na.omit(datPrePost3monthAnalysis)
1-(dim(datPrePost3monthAnalysisComplete)[1]/dim(datPrePost3monthAnalysis)[1])
write.csv(datPrePost3monthAnalysis, "datPrePost3monthAnalysis.csv", row.names = FALSE)
datPrePost3monthAnalysis = read.csv("datPrePost3monthAnalysis.csv", header = TRUE)

TestMCARNormality(datPrePost3monthAnalysis)
```
Get descriptives for each time point
```{r}
datPrePost3monthAnalysisBase = subset(datPrePost3monthAnalysis, Time == 0)
describe(datPrePost3monthAnalysis)


datPrePost3monthAnalysisPost = subset(datPrePost3monthAnalysis, Time == 1)
describe(datPrePost3monthAnalysisPost)


datPrePost3monthAnalysis3month = subset(datPrePost3monthAnalysis, Time == 2)
describe(datPrePost3monthAnalysis3month)
```


Now generate missing data for varibles
There are 18 data points so six people with treatment for three month follow-up but no treatment ID for pre or post. 
Deleting them for now, but will check in later on this

If data is still missing that means that there is zero data for the response

Getting rid of missing values after imputation, because if there are still missing values that means that the entire data set is empty
```{r}
head(datPrePost3monthAnalysis)
summary(datPrePost3monthAnalysis$ID)

# Getting rid of NA's for treatment
datPrePost3monthAnalysis = subset(datPrePost3monthAnalysis, Treatment == 1 | Treatment == 2 | Treatment == 3)
describe.factor(datPrePost3monthAnalysis$Treatment)
m = 10
head(datPrePost3monthAnalysis)

datPrePost3monthAnalysisImpute = amelia(m = m, datPrePost3monthAnalysis, noms = c("Gender", "Race", "Edu"), idvars = c("ID", "Treatment"), ts = "Time")

compare.density(datPrePost3monthAnalysisImpute, var = "Sec1Total")
compare.density(datPrePost3monthAnalysisImpute, var = "Sec2Total")
compare.density(datPrePost3monthAnalysisImpute, var = "Sec3Total")
compare.density(datPrePost3monthAnalysisImpute, var = "Sec4Total")
summary(datPrePost3monthAnalysisImpute)
datAnalysisAll = lapply(1:m, function(x){datPrePost3monthAnalysisImpute$imputations[[x]]})

datAnalysisAllComplete = NULL
for(i in 1:m){
 datAnalysisAllComplete[[i]] = na.omit(datAnalysisAll[[i]])
}


```
Now get desciptives for base
```{r}
datAnalysisAllDes = lapply(1:m, function(x){subset(datAnalysisAll[[x]], Time == 0)})

mean.out = NULL
for(i in 1:m) {
  mean.out[[i]] = apply(datAnalysisAllDes[[i]], 2, mean)
  mean.out = data.frame(mean.out)
}
mean.out

descFun = function(x){
  x = data.frame(t(x))
}
mean.out = descFun(mean.out)
mean.out

# now get sds
sd.out = NULL
for(i in 1:m) {
  sd.out[[i]] = apply(datAnalysisAllDes[[i]], 2, sd)
  sd.out = data.frame(sd.out)
}
sd.out = descFun(sd.out)
sd.out
mean.sd.out= mi.meld(mean.out, sd.out)
mean.sd.out

```
Now get descriptives for post
```{r}
datAnalysisAllDes = lapply(1:m, function(x){subset(datAnalysisAll[[x]], Time == 1)})

mean.out = NULL
for(i in 1:m) {
  mean.out[[i]] = apply(datAnalysisAllDes[[i]], 2, mean)
  mean.out = data.frame(mean.out)
}
mean.out

descFun = function(x){
  x = data.frame(t(x))
}
mean.out = descFun(mean.out)
mean.out

# now get sds
sd.out = NULL
for(i in 1:m) {
  sd.out[[i]] = apply(datAnalysisAllDes[[i]], 2, sd)
  sd.out = data.frame(sd.out)
}
sd.out = descFun(sd.out)
sd.out
mean.sd.out= mi.meld(mean.out, sd.out)
mean.sd.out

```
Now get descriptives for 3month
```{r}
datAnalysisAllDes = lapply(1:m, function(x){subset(datAnalysisAll[[x]], Time == 2)})

mean.out = NULL
for(i in 1:m) {
  mean.out[[i]] = apply(datAnalysisAllDes[[i]], 2, mean)
  mean.out = data.frame(mean.out)
}
mean.out

descFun = function(x){
  x = data.frame(t(x))
}
mean.out = descFun(mean.out)
mean.out

# now get sds
sd.out = NULL
for(i in 1:m) {
  sd.out[[i]] = apply(datAnalysisAllDes[[i]], 2, sd)
  sd.out = data.frame(sd.out)
}
sd.out = descFun(sd.out)
sd.out
mean.sd.out= mi.meld(mean.out, sd.out)
mean.sd.out

```
#########################
Creating wide format data
#########################
```{r}
for(i in 1:m){
datAnalysisAll[[i]]$Time = as.factor(datAnalysisAll[[i]]$Time)
}


datAnalysisWide = NULL

for(i in 1:m){
datAnalysisWide[[i]] = reshape(data = datAnalysisAll[[i]], v.names = c("Sec1Total", "Sec2Total", "Sec3Total", "Sec4Total"), timevar = "Time", direction = "wide", idvar = "ID")
}



```



Need to create a difference score


########################################### 
Outcome 1 for pre and post 
For treatment 1
############################################ 

```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 0 | Time == 1)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 1)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time  + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec1Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec1Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
#outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
########################################### 
Outcome 1 for pre and post 
For treatment 2
############################################ 
SignificantOutcome
```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 0 | Time == 1)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 2)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time  + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec1Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}


outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNull[[i]])
}
outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
########################################### 
Outcome 1 for pre and post 
For treatment 3
############################################ 
Dropped 

```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 0 | Time == 1)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 3)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec1Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec1Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
#outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```

########################################### 
Outcome 2 for pre and post 
For treatment 1
############################################ 
SignificantOutcome
```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 0 | Time == 1)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 1)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec2Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}


outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNull[[i]])
}
outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
########################################### 
Outcome 2 for pre and post 
For treatment 2
############################################ 
SignificantOutcome

```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 0 | Time == 1)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 2)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec2Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}



outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNull[[i]])
}

outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

#SignificantOutcome
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
SignificantOutcomes
Moderation graph for Section two pre post for treatment two
```{r}

datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 0 | Time == 1)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 2)
}

#SignificantOutcome
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}

modGraph = NULL
for(i in 1:m){
  modGraph[[i]] = interact_plot(model = outputReg[[i]], pred = "Time", modx = "Age", cluster = "ID", data = datAnalysisAllComplete[[i]])
}
modGraph



```




########################################### 
Outcome 2 for pre and post 
For treatment 3
############################################ 
Significant Outcome
```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 0 | Time == 1)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 3)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec2Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}


outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNull[[i]])
}
outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```

########################################### 
Outcome 3 for pre and post 
For treatment 1
############################################ 
SignificantOutcome
```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 0 | Time == 1)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 1)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec3Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec3Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
#outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
########################################### 
Outcome 3 for pre and post 
For treatment 2
############################################ 

```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 0 | Time == 1)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 2)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec3Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec3Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
#outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
########################################### 
Outcome 3 for pre and post 
For treatment 3
############################################ 

```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 0 | Time == 1)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 3)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec3Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec3Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
#outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
---
title: "Untitled"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

########################################### 
Outcome 4 for pre and post 
For treatment 1
############################################ 

```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 0 | Time == 1)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 1)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec4Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec4Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
#outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
########################################### 
Outcome 4 for pre and post 
For treatment 2
############################################ 

```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 0 | Time == 1)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 2)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec4Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec4Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
#outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
########################################### 
Outcome 4 for pre and post 
For treatment 3
############################################ 

```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 0 | Time == 1)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 3)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec4Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec4Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
#outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
---
title: "Untitled"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
########################################### 
Outcome 1 for post and 3month 
For treatment 1
############################################ 

```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 1 | Time == 2)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 1)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec1Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec1Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
#outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
########################################### 
Outcome 1 for post and 3month 
For treatment 2
############################################ 

```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 1 | Time == 2)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 2)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec1Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec1Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
#outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
########################################### 
Outcome 1 for post and 3month 
For treatment 3
############################################ 

```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 1 | Time == 2)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 3)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec1Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec1Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
#outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```

########################################### 
Outcome 2 for post and 3month 
For treatment 1
############################################ 

```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 1 | Time == 2)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 1)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec2Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec2Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
#outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
########################################### 
Outcome 2 for post and 3month 
For treatment 2
############################################ 

```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 1 | Time == 2)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 2)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec2Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec2Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
#outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
########################################### 
Outcome 2 for post and 3month 
For treatment 3
############################################ 

```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 1 | Time == 2)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 3)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec2Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec2Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
#outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
---
title: "Untitled"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

########################################### 
Outcome 3 for post and 3month 
For treatment 1
############################################ 

```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 1 | Time == 2)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 1)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec3Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec3Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
#outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
########################################### 
Outcome 3 for post and 3month 
For treatment 2
############################################ 

```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 1 | Time == 2)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 2)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec3Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec3Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
#outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
########################################### 
Outcome 3 for post and 3month 
For treatment 3
############################################ 

```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 1 | Time == 2)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 3)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec3Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec3Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
#outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
---
title: "Untitled"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

########################################### 
Outcome 4 for post and 3month 
For treatment 1
############################################ 

```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 1 | Time == 2)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 1)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec4Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec4Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
#outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
########################################### 
Outcome 4 for post and 3month 
For treatment 2
############################################ 

```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 1 | Time == 2)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 2)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec4Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec4Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
#outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
########################################### 
Outcome 4 for post and 3month 
For treatment 3
############################################ 

```{r}
datAnalysisAllPrePost = NULL
for(i in 1:m){
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAll[[i]], Time == 1 | Time == 2)
  datAnalysisAllPrePost[[i]] = subset(datAnalysisAllPrePost[[i]], Treatment == 3)
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec4Total ~  + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec4Total ~ Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
}

outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
#outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```

Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAllPrePost[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```

#####################
Model1 pre, post, 3month
#####################
Now run model 1

```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ factor(Treatment)*poly(Time, 2) +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 


```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec1Total ~  + (1 | ID), data  = datAnalysisAll[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec1Total ~ factor(Treatment)*Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
}

outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
#outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModel1Level1 = NULL
for(i in 1:m){
  residModel1Level1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModel1Level1[[i]])
}

```
Now moderator model
```{r}
modGraph = NULL

for(i in 1:m){
  modGraph[[i]] = cat_plot(model = outputRegNoPoly[[i]], pred = "Time", modx = "Treatment", cluster = "ID", data = datAnalysisAllComplete[[i]])
}
modGraph

outputRegNoPoly1 = outputRegNoPoly[[1]]
datAnalysisAll1 = datAnalysisAll[[1]]

cat_plot(model  = outputRegNoPoly1, pred = "Time", modx = "Treatment", cluster = "ID", data = datAnalysisAll1)

dim(datAnalysisAll1)
datAnalysisAll1Complete = na.omit(datAnalysisAll)

dim(datAnalysisAll1)
datAnalysisAll1Complete = na.omit(datAnalysisAll1)
dim(datAnalysisAll1Complete)
outputRegNoPoly1
```
Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ factor(Treatment)*Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ factor(Treatment)*Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ factor(Treatment)*Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ factor(Treatment)*Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
Now get contrats
```{r}
K1 = matrix(c(0,1,-1,0,0,0,0,0,0,0),1)
K2 = matrix(c(0,1,-1,0,0,0,0,0,1,-1),1)
K = K1-K2; K

t = NULL
for(i in 1:m){
  t[[i]] = glht(outputRegNoPoly[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t
# No way to systematically grab the standard errors so just ballpark.  If significant maybe do something else.

```
######################
Model 2
######################
Now run model 2

Range for outcome variable is 15 to 96

```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ factor(Treatment)*poly(Time, 2, raw = TRUE) +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
Model 2 contrast effects (do this later)

Model 2 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec2Total ~  + (1 | ID), data  = datAnalysisAll[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec2Total ~ factor(Treatment)*Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
}


outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
outputAnova

```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModelLevel1 = NULL
for(i in 1:m){
  residModelLevel1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModelLevel1[[i]])
}


residModelLevel2 = NULL
for(i in 1:m){
residModelLevel2[[i]] = HLMresid(outputReg[[i]], level = "ID", standardize = TRUE)
}

for(i in 1:m){
hist(residModelLevel2[[i]])
}
```
Now moderator model
```{r}
modGraph = NULL
for(i in 1:m){
  modGraph[[i]] = cat_plot(model = outputReg[[i]], pred = "Time", modx = "Treatment", cluster = "ID", y.label = "Section 2 Total Score", geom = "line", interval  = FALSE , data = datAnalysisAllComplete[[i]])
}
modGraph

```
Model one with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ factor(Treatment)*Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ factor(Treatment)*Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ factor(Treatment)*Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ factor(Treatment)*Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Now get contrats
```{r}
K = matrix(c(rep(0,11), -1,1),1)
summary(outputReg[[1]])

t = NULL
for(i in 1:m){
  t[[i]] = glht(outputReg[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t


# No way to systematically grab coefs or se's must manually do it

```
######################
Model 3
######################
Now run model 3

```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ factor(Treatment)*poly(Time, 2, raw = TRUE) +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}

quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output

se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec2Total ~  + (1 | ID), data  = datAnalysisAll[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec2Total ~ factor(Treatment)*Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
}

outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
outputAnova
```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModelLevel1 = NULL
for(i in 1:m){
  residModelLevel1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModelLevel1[[i]])
}

rangeResid = NULL
for(i in 1:m){
  rangeResid[[i]] = range(residModelLevel1[[i]])
}

rangeResid

residModelLevel2 = NULL
for(i in 1:m){
residModelLevel2[[i]] = HLMresid(outputReg[[i]], level = "ID", standardize = TRUE)
}

for(i in 1:m){
hist(residModelLevel2[[i]])
}
```
Now moderator model
```{r}
modGraph = NULL
for(i in 1:m){
  modGraph[[i]] = cat_plot(model = outputReg[[i]], pred = "Time", modx = "Treatment", cluster = "ID", data = datAnalysisAllComplete[[i]])
}
modGraph

```
Model three with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ factor(Treatment)*Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ factor(Treatment)*Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ factor(Treatment)*Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3Total ~ factor(Treatment)*Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Now get contrats
```{r}
K = matrix(c(rep(0,11), 1, -1), 1)

t = NULL
for(i in 1:m){
  t[[i]] = glht(outputReg[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t
# No way to systematically grab the standard errors so just ballpark.  If significant maybe do something else.

```
######################
Model 4
######################
Now run model 4

```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ factor(Treatment)*poly(Time, 2, raw = TRUE) +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

```
Model 1 contrast effects (do this later)

Model 1 model comparision
First develop null models
Then compare the no poly term with the poly term

Need output regular (Reg) for model comparision cannot compare a summary of a model to another summary of a model
```{r}
outputRegNull = list()

for(i in 1:m){
  outputRegNull[[i]] = lmer(Sec4Total ~  + (1 | ID), data  = datAnalysisAll[[i]])
}

outputRegNoPoly = list()
for(i in 1:m){
outputRegNoPoly[[i]] = lmer(Sec4Total ~ factor(Treatment)*Time +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
}


outputAnova = NULL
for(i in 1:m){
  outputAnova[[i]] = anova(outputReg[[i]], outputRegNoPoly[[i]], outputRegNull[[i]])
}
outputAnova
```
Now model one diagnoistics 
In the paper if you need to cite these statistics just give a range
```{r}
rSquare = NULL
for(i in 1:m){
  rSquare[[i]]= r.squaredLR(outputReg[[i]])
}
rSquare

residModelLevel1 = NULL
for(i in 1:m){
  residModelLevel1[[i]] = HLMresid(outputReg[[i]], level = 1, standardize = TRUE)
}
for(i in 1:m){
hist(residModelLevel1[[i]])
}

residModelLevel2 = NULL
for(i in 1:m){
residModelLevel2[[i]] = HLMresid(outputReg[[i]], level = "ID", standardize = TRUE)
}

for(i in 1:m){
hist(residModelLevel2[[i]])
}
```
Now moderator model
```{r}
modGraph = NULL
for(i in 1:m){
  modGraph[[i]] = cat_plot(model = outputReg[[i]], pred = "Time", modx = "Treatment", cluster = "ID", data = datAnalysisAllComplete[[i]])
}
modGraph

```
Model three with moderators 
, Gender, Age, and Race
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ factor(Treatment)*Time* +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ factor(Treatment)*Time*Gender +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ factor(Treatment)*Time*Age +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)


output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec4Total ~ factor(Treatment)*Time*Race +   + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
Now get contrats
```{r}
K = matrix(c(rep(0,11), 1, -1), 1)

t = NULL
for(i in 1:m){
  t[[i]] = glht(outputReg[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t
# No way to systematically grab the standard errors so just ballpark.  If significant maybe do something else.

```

