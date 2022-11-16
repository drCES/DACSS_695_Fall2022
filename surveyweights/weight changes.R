
###Task: Reweight Sample Data to Make Walker Stapleton & Jared Polis "tied" in the election estimates 


library(haven)
library(tidyverse)
library(srvyr)
library(survey)
library(poliscidata)
library(anesrake)

sample <- read_dta("surveyweights/sample.dta")
sample<-as.data.frame(sample)

#Currently, there are 5 variables included in this weighting scheme. There are other demographic possibilities

names(sample) #this gives you a list of all variables in the sample data - look for additional demos if you want 

#If you want to calculate MOE, use this function 
moe_fun <- function(p, n, cv) {
  step1<-p*(1-p)
  step2<-step1/n
  se<-(step2)^.5
  moe<-cv*se
  print(moe)
}

moe<-moe_fun(.5, 800, 1.96) #Plug in appropriate values for MOE

###

#Save new vectors with target population values for weights 
sex <- c(.525, .475)  ##Target values for females and males; label order (female, male)
age_group  <- c(.132, .183, .15, .248, .287)   #Target values for 5 age groups 
race_4 <-c(.7143, .0501, .1768, .0588) #Target values race/ethnic identities - white, black, Hispanic, all others
educ <-c(.2875, .2545, .082, .252, .124) #Target values education - HS or less, Some college, AA, BA, Graduate degree
pid_4 <-c(.3675, .2838, .305, .0437) #Target values Party Registration - (Democrats, Independents, Republicans, All 3rd Parties)  

#Combine the demographic vectors into a list
targets <- list(sex, age_group, race_4, educ, pid_4)
# remember, these names will have to match the column names & order in the sample data 
names(targets) <- c("sex", "age_group", "race_4", "educ", "pid_4")

myweights <- anesrake(targets, sample, 
                      caseid = sample$caseid, cap = 8, type = "pctlim", pctlim=.05)    

sample$full_weight  <- unlist(myweights[1])

summary(sample$full_weight)

#Gubernatorial Vote Choice - Weighted & Unweighted 
gov_w<-sample %>%
  as_survey(weights = c(full_weight)) %>%
  filter(!is.na(gov_choice)) %>% 
  group_by(gov_choice) %>%
  summarise(n = survey_total()) %>% 
  mutate(weight_support = n /sum(n)) 

gov_uw<-sample %>%
  group_by(gov_choice) %>%
  filter(!is.na(gov_choice)) %>% 
  summarise(n = n()) %>% 
  mutate(unweight_support = n /sum(n))

gov_combo<-cbind(gov_uw, gov_w) 


gov_combo$diff <- gov_combo$weight_support - gov_combo$unweight_support
gov_outcome<-cbind(gov_combo$gov_choice,  gov_combo$unweight_support, gov_combo$weight_support, gov_combo$diff) 
colnames(gov_outcome) <- c("candidate", "unewighted support", "weighted support",  "diff") 
gov_outcome



xtp(sample, gov_choice, age_group) #Crosstab between regular numbers correct and PowerBall correct
xtp(sample, gov_choice, pid_4) #Crosstab between regular numbers correct and PowerBall correct
xtp(sample, gov_choice, race_4) #Crosstab between regular numbers correct and PowerBall correct
xtp(sample, gov_choice, educ, w=full_weight) #Crosstab between regular numbers correct and PowerBall correct
xtp(sample, gov_choice, educ) #Crosstab between regular numbers correct and PowerBall correct






