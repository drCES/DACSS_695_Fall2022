
library(poliscidata)
library(sm)

###Coin Flip Example using Simulation
size<-1
flips<-10
p<-.5
trials<-1000
coin.flip <- t(rbinom(flips, size, p))
freq(coin.flip)
coin.flips <- t(replicate(trials, rbinom(flips, size, p)))


head(coin.flips)

sum(apply(coin.flips, 1, sum) == 9)
sum(apply(coin.flips, 1, sum) == 0)


###Coin Flip Example using Simulation

flips<-10
size<-1
times<-10000
p<-.5
coin.flips <- (replicate(times, rbinom(flips, size, p)))
freq(coin.flips)

coin.dist<-matrix(data=NA, nrow=1, ncol=times)

for (i in 1:times){
  coin.dist[,i] <- sum(coin.flips[,i])
}


hist(coin.dist)
mean(coin.dist[1,])
freq(coin.dist[1,])
sd(coin.dist[1,])
7/5900
##### Let's Sample the 2020 Presidential Election Voter Population 

###Draw 1 sample at a time and see 
jb_vote<-.513 #Biden's vote share in 2016 nationally
dt_vote<-.468 #Trump's vote share in 2016 nationally
else_vote<-(1-(jb_vote+dt_vote)) #All other candidates' vote share in 2016 nationally

sample1<-sample(1:3, 1000, replace = T, prob=c(jb_vote, dt_vote, else_vote))
freq(sample1)

sample3<-as.data.frame(replicate(times, sample(1:3, 1000, replace = T, prob=c(jb_vote, dt_vote, else_vote))))
freq(sample3$V100)

population<-sample(1:3, 1580000, replace = T, prob=c(jb_vote, dt_vote, else_vote))
freq(population)

sample2<-sample(population, replace = F, prob=c(jb_vote, dt_vote, else_vote))
freq(sample2)


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

##Calcuates standard error of the sampling distribution
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

##Counts Number of Times Outside +/-2 SD of the Mean
jb.st.error <- sd(sampling.dist[1,])
dt.st.error <- sd(sampling.dist[2,])

# How confident am I?
jb.ci <- c(mean(sampling.dist[1,])-1.96*jb.st.error, mean(sampling.dist[1,])+1.96*jb.st.error)
jb.ci_low<-mean(sampling.dist[1,])-1.96*jb.st.error
jb.ci_high<-mean(sampling.dist[1,])+1.96*jb.st.error

dt.ci <- c(mean(sampling.dist[2,])-1.96*dt.st.error, mean(sampling.dist[2,])+1.96*dt.st.error)
dt.ci_low<-mean(sampling.dist[2,])-1.96*dt.st.error
dt.ci_high<-mean(sampling.dist[2,])+1.96*dt.st.error

##Counts Number of Times Estimated support is Outside +/- 2SD of Actual Support
sum(sampling.dist [2,] < dt.ci_low | sampling.dist [2,] > dt.ci_high )
sum(sampling.dist [1,] < jb.ci_low | sampling.dist [1,] > jb.ci_high )
jb.low<-sum(sampling.dist [1,] < jb.ci_low )
jb.low
jb.high<-sum(sampling.dist [1,] > jb.ci_high )
jb.high

