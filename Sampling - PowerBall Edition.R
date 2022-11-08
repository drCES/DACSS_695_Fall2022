
################################################################################
# DACSS 695: Survey Research Methods                                           #
# Carey E. Stapleton                                                           #
# University of Massachusetts Amherst                                          #
# cstapleton@umass.edu                                                         #
# Sampling - PowerBall Numebers                                                #
# Last update: 11/6/22                                                         #
################################################################################

library(poliscidata)
library(tidyverse)

###Yesterday's winning numbers with PowerBall draw being pb_6
pb1<-10
pb2<-33
pb3<-41
pb4<-47
pb5<-56
pb_6<-10


draws<-30000 #Number of tickets to buy
pb <- (replicate(draws, sample(1:69, 5, replace=FALSE))) #Randomly draws 5 numbers without replacement to replicate the first 5 draws for PB
power <- t(replicate(draws, sample(1:26, 1, replace=FALSE))) #Randomly draws 1 Powerball number from 1 to 26
m2<-t(apply(pb,2,sort)) #Orders the initial 5 number draw from smallest to largest 
m3<-t(power) #Transposes the PB number to merge with the 5 initial draws
final<-as.data.frame(cbind(m2, m3)) #Merges the 5 initial draws with the PowerBall number

#Code calculates the number of correct numbers drawn in that random sample
 final$correct1<-if_else((final$V1==pb1 | final$V1==pb2 |  final$V1==pb3  |  final$V1==pb4  |  final$V1==pb5), 1, 0, missing = NULL)
  final$correct2<-if_else((final$V2==pb1 |final$V2==pb2 |  final$V2==pb3 |  final$V2==pb4 |  final$V1==pb5) , 1, 0, missing = NULL)
  final$correct3<-if_else((final$V3==pb1 | final$V3==pb2 |  final$V3==pb3 |  final$V3==pb4 |  final$V5==pb5), 1, 0, missing = NULL)
  final$correct4<-if_else((final$V4==pb1 | final$V4==pb2 |  final$V4==pb3 |  final$V4==pb4 |  final$V5==pb5) , 1, 0, missing = NULL)
  final$correct5<-if_else((final$V5==pb1 | final$V5==pb2 |  final$V5==pb3 |  final$V5==pb4 |  final$V5==pb5) , 1, 0, missing = NULL)
  final$correct6<-if_else(final$V6==pb_6 , 1, 0, missing = NULL)
  
  #Counts all correct values 6=Won the Powerball
  final$all_correct <- rowSums(cbind(final$correct1, final$correct2, 
final$correct3, final$correct4, final$correct5, final$correct6), na.rm = T )
  #Counts correct values for first 5 initial numberes
   final$reg_correct <- rowSums(cbind(final$correct1, final$correct2, 
                                     final$correct3, final$correct4, final$correct5), na.rm = T )
freq(final$all_correct) # Frequency on all numbers correctly picked
freq(final$reg_correct) #Frequency on number of first 5 numbers correctly picked
correct_powerball<-table(final$reg_correct, final$correct6) #Crosstab between regular numbers correct and PowerBall correct
correct_powerball

