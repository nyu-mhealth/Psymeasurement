install.packages('psych')
install.packages('plyr')

library(psych)
library(plyr)

# set work directory
setwd("~/psymeasurement-scripts")

###################
# data processing #
###################

# read in data file
cfcs<- read.csv("turk_job_cfcs_wave_51_assignments.csv")

# only keep submitted surveys
cfcs_submit<- subset(cfcs, cfcs$AssignmentStatusCode=="Submitted", 
                  select=c(Id, influence:score_dichotomous))
cfcs_submit<- cfcs_submit[, !grepl("q10", names(cfcs_submit))]

# select out raw scores to analyze
cfcs_raw<- cbind(cfcs_submit$Id, cfcs_submit[, grepl("score_raw", names(cfcs_submit))])
# rename item names
for (i in 1:12){
  names(cfcs_raw)[i+1] <- paste0("item",i)
}
cfcs_raw<- rename(cfcs_raw, c("cfcs_submit$Id"="ID"))
# reverse code items
reverse_items<- c(paste0("item",c(3:5, 9:12)))
cfcs_raw[,reverse_items]<- 6 - cfcs_raw[,reverse_items]

describe(cfcs_raw)

#############
# histogram #
#############

item_name<- names(cfcs_raw[2:14])
par(mfrow=c(4,4))
for (i in item_name){
  hist(cfcs_raw[[i]], xlab=i, main=paste("Histogram of",i))
}


#################
# data analysis #
#################

# get alpha and interitem correlation
alpha50<- alpha(cfcs_raw[2:(ncol(cfcs_raw)-1)])$total$raw_alpha
cc50<- alpha(cfcs_raw[2:(ncol(cfcs_raw)-1)])$total$average_r

###### resample ######

# draw 2, 5, 10, 15, 20, 25 samples from the serveys
# repeat for 1000 times and extract mean alpha and interitem correlation
draw_sample<- c(5, 10, 15, 20, 25, 30, 40) # the number of sample being drawn
# define a resample function to keep code clean
resample.function<- function(cfcs_raw, draw_sample){
  n<- nrow(cfcs_raw) # count total number of observations
  N<- 10 # the number of times to resample
  alpha_v<- numeric(N)# create a vector to store the results
  average_r<- numeric(N)
  resample<- NULL
  for (i in draw_sample){
    for (j in 1:N){
      obs<- sample(n, i, replace=F)
      samp<- cfcs_raw[obs,]
      alpha_v[j]<- alpha(samp[2:(ncol(samp)-1)])$total$raw_alpha
      average_r[j]<- alpha(samp[2:(ncol(samp)-1)])$total$average_r
    }
    alpha_mean<- mean(alpha_v)
    alpha_lower<- alpha_mean - qnorm(0.975)*sd(alpha_v)
    alpha_higher<- alpha_mean + qnorm(0.975)*sd(alpha_v)
    average_rmean<- mean(average_r)
    average_rlower<- average_rmean - qnorm(0.975)*sd(average_r)
    average_rhigher<- average_rmean + qnorm(0.975)*sd(average_r)
    alphar<- cbind(i, alpha_mean, alpha_lower, alpha_higher, average_rmean, average_rlower, average_rhigher)
    resample<- rbind(resample, alphar)
  }
  return(resample)
}
resample<- resample.function(cfcs_raw, draw_sample)

###### take out item ######

draw_item<- NULL
for (k in names(cfcs_raw[2:(length(cfcs_raw)-1)])){
  cfcs_item<- cfcs_raw[ , !names(cfcs_raw) %in% c(i)]
  cfcs_item_resample<- resample.function(cfcs_item, draw_sample)
  cfcs_item_resample<- cbind(k, cfcs_item_resample)
  
  item<- cbind(i, alpha_item, average_r_item)
  draw_item<- rbind(draw_item, item)
}


write.table(resample, file="resample.csv", sep=",", row.names=F)
write.table(draw_item, file="draw_item.csv", sep=",", row.names=F)


