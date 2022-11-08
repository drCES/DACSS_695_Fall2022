
################################################################################
# DACSS 695: Survey Research Methods                                           #
# Carey E. Stapleton                                                           #
# University of Massachusetts Amherst                                          #
# cstapleton@umass.edu                                                         #
# Sampling                                                                     #
# Last update: 11/6/22                                                         #
# Thanks to Jeff Harden at University of Notre Dame for code                   #
################################################################################

library(tidyverse)
library(poliscidata)
library(ggplot2)
library(plotly)

### Fair Coin ###

###Flipping a Coin 10 times 
#Question: If I flip a coin 10 times and it is heads 9 times, is it a fair coin?
size<-1
flips<-10
p<-.5

coin.flip <- t(rbinom(flips, size, p))
freq(coin.flip)

head(coin.flip)


###Coin Flip Example using Simulation - Flipping a Coin 10 times 1,000 times 

flips<-10
size<-1
times<-1000
p<-.5
coin.flips <- (replicate(times, rbinom(flips, size, p)))
freq(coin.flips)

coin.dist<-matrix(data=NA, nrow=1, ncol=times)

for (i in 1:times){
  coin.dist[,i] <- sum(coin.flips[,i])
}

mean(coin.dist[1,])
freq(coin.dist[1,])
sd(coin.dist[1,])



#######################Margin of Error Example 

####Hand Calculating Margin of Error 
p<-.5   #If you do not know the proportion value, always default to .5 because that leads to the largest estimated MOE
n<-550 #This is simply the response count in your sample
cv<-1.96 #Critical value relates to how confident you want to be in your results. Use 1.96 for 95% confidence interval; 1.65 for 90% confidence interval; 2.575 for 99% confidence interval

step1<-p*(1-p)
step2<-step1/n
se<-(step2)^.5
moe<-cv*se
print(moe)


###Creating a Margin of Error function for proportions &  large populations 

moe_fun <- function(p, n, cv) {
  step1<-p*(1-p)
  step2<-step1/n
  se<-(step2)^.5
  moe<-cv*se
  print(moe)
}

moe_fun(p, n, cv) #Plug in appropriate values for MOE
moe_fun(.5, 10000, 1.96) #Plug in appropriate values for MOE


###Looking at Margin of Error at Different Sample Sizes

p<-.5    
n<-1000    
cv<-1.96  
nn <- seq(100, 2000, by = 50)  #This creates a vector that iterates by 50 starting from 100 and ending at 2,000
moe_sample<-as.data.frame(moe_fun(p, nn, cv)) #This calculates the margin of error at each sample size & saves the results

my_data <- data.frame(moe_sample, nn)  #This combines the data frame with vector of sample sizes for graphing purposes 

head(my_data) # This shows us the column names and first few values of data. Always look at your data
my_data<-rename(my_data, moe = moe_fun.p..nn..cv.)

a<-ggplot(data=my_data, aes(x=nn, y=moe)) +
  geom_line()+ ylim(0, .1)+
  geom_point() +   theme_bw() + theme_minimal() +
  labs(title="Margin of Error by Different Sample Sizes",x="Sample Size", y = "Margin of Error")

ggplotly(a)
a


###Graphing MOE by Sample Size and Critical Value 
p<-.5      #If you do not know the proportion value, always default to .5 because that leads to the largest estimated MOE
n<-1000    #This is simply the response count in your sample
nn <- seq(100, 2000, by = 50)  #This creates a vector that iterates by 50 starting from 100 and ending at 2,000
cv1<-1.65
cv2<-1.96
cv3<-2.575

moe_sample1<-as.data.frame(moe_fun(p, nn, cv1)) #Calculates MOE at CV of 1.65
moe_sample2<-as.data.frame(moe_fun(p, nn, cv2)) #Calculates MOE at CV of 1.96
moe_sample3<-as.data.frame(moe_fun(p, nn, cv3)) #Calculates MOE at CV of 2.57

my_data <- data.frame(moe_sample1, moe_sample2, moe_sample3, nn)  #This combines the data frame with vector of sample sizes for graphing purposes 

head(my_data)

my_data<-rename(my_data, cv_165 = moe_fun.p..nn..cv1.)
my_data<-rename(my_data, cv_196 = moe_fun.p..nn..cv2.)
my_data<-rename(my_data, cv_257 = moe_fun.p..nn..cv3.)

a<-ggplot(data=my_data, aes(x=nn, y=cv_165)) +
  geom_line(linetype=2, size=1.25)+ ylim(0, .16)+ geom_line(aes(y=cv_196), color="gray", size=1.25) + geom_line(aes(y=cv_257), color="steelblue", linetype=3 , size=1.5) + theme_bw() + theme_minimal() +
  labs(title="Margin of Error by Different Sample Sizes",x="Sample Size", y = "Margin of Error") 

ggplotly(a)


#####Random Sampling Illustration
##### Let's Sample the 2020 Presidential Election Voter Population 

###Draw 1 sample at a time and see 
jb_vote<-.513 #Biden's presidential vote share in 2020 nationally
dt_vote<-.468 #Trump's presidential vote share in 2020 nationally
else_vote<-(1-(jb_vote+dt_vote)) #All other candidates' vote share in 2020 nationally
times<-1000
n<-500

sample1<-sample(1:3, n, replace = T, prob=c(jb_vote, dt_vote, else_vote))
freq(sample1)

sample3<-as.data.frame(replicate(times, sample(1:3, n, replace = T, prob=c(jb_vote, dt_vote, else_vote))))
freq(sample3$V100)

population<-sample(1:3, 1580000, replace = T, prob=c(jb_vote, dt_vote, else_vote))
freq(population)



# reps is the number of samples drawn
# n is the number of observations per sample

reps <- 1000
n <- 1000
sample2 <- matrix(data=NA, nrow=n, ncol=reps)
sampling.dist<-matrix(data=NA, nrow=3, ncol=reps)
sampling.dist2<-matrix(data=NA, nrow=3, ncol=reps)
error<-matrix(data=NA, nrow=1, ncol=reps)

for (i in 1:reps){
  sample2[,i] <- sample(population, n, replace=F)
  sampling.dist2[,i] <- table(sample2[,i])
}

for (i in 1:3){
  sampling.dist[i,] <- ((sampling.dist2[i,])/n)  
}

for (i in 1:reps){
  error[,i] <- ((sampling.dist[1,i])-(sampling.dist[2,i]))  
}



#Creates a new variable 
jb_avg<-mean((sampling.dist[1,]))
dt_avg<-mean((sampling.dist[2,]))
ot_avg<-mean((sampling.dist[3,]))

jb_avg
dt_avg

jb_max<-max((sampling.dist[1,]))
jb_min<-min((sampling.dist[1,]))
dt_max<-max((sampling.dist[2,]))
dt_min<-min((sampling.dist[2,]))

jb <- density(sampling.dist[1,])
dt <- density(sampling.dist[2,])
ot <- density(sampling.dist[3,])

##Calculates standard error of the sampling distribution
jb.st.error <- sd(sampling.dist[1,])
dt.st.error <- sd(sampling.dist[2,])

# How confident am I?
jb.ci <- c(mean(sampling.dist[1,])-1.96*jb.st.error, mean(sampling.dist[1,])+1.96*jb.st.error)
jb.ci_low<-mean(sampling.dist[1,])-1.96*jb.st.error
jb.ci_high<-mean(sampling.dist[1,])+1.96*jb.st.error
jb.ci.size=jb.ci_high-jb.ci_low

dt.ci <- c(mean(sampling.dist[2,])-1.96*dt.st.error, mean(sampling.dist[2,])+1.96*dt.st.error)
dt.ci_low<-mean(sampling.dist[2,])-1.96*dt.st.error
dt.ci_high<-mean(sampling.dist[2,])+1.96*dt.st.error
dt.ci.size=dt.ci_high-dt.ci_low

plot(jb, ylim=c(0 , 30), xlim=c(.4, .6), col='blue', xlab='Electoral Support', main='2020 Presidential Election Sampling')
abline(v=jb_avg, col='blue')
legend(x = "topright",          # Position
       legend = c("Biden Poll Results", "Trump Poll Results", "Biden 95% CI", "Trump 95% CI"),  # Legend texts
       lty = c(1, 1, 2, 2),           # Line types
       col = c('blue', 'red', 'blue', 'red'),           # Line colors
       lwd = 2)                 # Line width

abline(v=jb.ci_low, col='blue', lty=2)
abline(v=jb.ci_high, col='blue', lty=3)

lines(dt, col='red')
abline(v=dt_avg, col='red')
abline(v=dt.ci_low, col='red', lty=2)
abline(v=dt.ci_high, col='red', lty=3)

##Counts Number of Times 1 Candidate is Beating the Other
jb_dt <- sum(sampling.dist [1,] > sampling.dist [2,])
dt_jb <- sum(sampling.dist [2,] > sampling.dist [1,])
ot_jb <- sum(sampling.dist [3,] > sampling.dist [1,])
ot_dt <- sum(sampling.dist [3,] > sampling.dist [2,])
jb_dt
dt_jb
ot_jb
ot_dt

jb_dt_tie <- sum(sampling.dist [1,] == sampling.dist [2,])
jb_dt_tie


##Counts Number of Times Estimated support is Outside +/- 2SD of Actual Support
sum(sampling.dist [2,] < dt.ci_low | sampling.dist [2,] > dt.ci_high )
sum(sampling.dist [1,] < jb.ci_low | sampling.dist [1,] > jb.ci_high )
jb.low<-sum(sampling.dist [1,] < jb.ci_low )
jb.low
jb.high<-sum(sampling.dist [1,] > jb.ci_high )
jb.high


#####Plotting Results for n = 500

reps <- 1000
n <- 500
sample2 <- matrix(data=NA, nrow=n, ncol=reps)
sampling.dist<-matrix(data=NA, nrow=3, ncol=reps)
sampling.dist2<-matrix(data=NA, nrow=3, ncol=reps)
error<-matrix(data=NA, nrow=1, ncol=reps)

for (i in 1:reps){
  sample2[,i] <- sample(population, n, replace=F)
  sampling.dist2[,i] <- table(sample2[,i])
}

for (i in 1:3){
  sampling.dist[i,] <- ((sampling.dist2[i,])/n)  
}

for (i in 1:reps){
  error[,i] <- ((sampling.dist[1,i])-(sampling.dist[2,i]))  
}

jb_500 <- density(sampling.dist[1,])
dt_500 <- density(sampling.dist[2,])
ot_500 <- density(sampling.dist[3,])

plot(jb, ylim=c(0 , 30), xlim=c(.4, .6), col='blue', xlab='Electoral Support', main='2020 Presidential Election Sampling')
abline(v=jb_avg, col='blue')

lines(jb_500, col='red')


#####Plotting Results for n = 2500

reps <- 1000
n <- 2500
sample2 <- matrix(data=NA, nrow=n, ncol=reps)
sampling.dist<-matrix(data=NA, nrow=3, ncol=reps)
sampling.dist2<-matrix(data=NA, nrow=3, ncol=reps)
error<-matrix(data=NA, nrow=1, ncol=reps)

for (i in 1:reps){
  sample2[,i] <- sample(population, n, replace=F)
  sampling.dist2[,i] <- table(sample2[,i])
}

for (i in 1:3){
  sampling.dist[i,] <- ((sampling.dist2[i,])/n)  
}

for (i in 1:reps){
  error[,i] <- ((sampling.dist[1,i])-(sampling.dist[2,i]))  
}

jb_2500 <- density(sampling.dist[1,])

lines(jb_2500, col='purple')

plot(jb, ylim=c(0 , 50), xlim=c(.4, .6), col='blue', xlab='Electoral Support', main='2020 Presidential Election Sampling')
abline(v=jb_avg, col='blue')

lines(jb_500, col='red')
lines(jb_2500, col='purple')



#####Plotting Results for n = 10,000

reps <- 1000
n <- 10000
sample2 <- matrix(data=NA, nrow=n, ncol=reps)
sampling.dist<-matrix(data=NA, nrow=3, ncol=reps)
sampling.dist2<-matrix(data=NA, nrow=3, ncol=reps)
error<-matrix(data=NA, nrow=1, ncol=reps)

for (i in 1:reps){
  sample2[,i] <- sample(population, n, replace=F)
  sampling.dist2[,i] <- table(sample2[,i])
}

for (i in 1:3){
  sampling.dist[i,] <- ((sampling.dist2[i,])/n)  
}

for (i in 1:reps){
  error[,i] <- ((sampling.dist[1,i])-(sampling.dist[2,i]))  
}

jb_10k <- density(sampling.dist[1,])

lines(jb_10k, col='orange')

plot(jb, ylim=c(0 , 75), xlim=c(.4, .6), col='blue', xlab='Electoral Support', main='2020 Presidential Election Sampling')
abline(v=jb_avg, col='blue')
legend(x = "topright",          # Position
       legend = c("Biden Results: N=1,000", "Biden Results: N=500", "Biden Results: N=2,500", "Biden Results: N=10,000"),  # Legend texts
       lty = c(1, 1, 1, 1),           # Line types
       col = c('blue', 'red', 'purple', 'orange'),           # Line colors
       lwd = 2)                 # Line width

lines(jb_500, col='red')
lines(jb_2500, col='purple')
lines(jb_10k, col='orange')
