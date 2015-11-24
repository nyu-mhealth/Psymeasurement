library(psych)
library(plyr)
library(gtools)
library(foreach)
library(doParallel)
library(ggplot2)
library(jsonlite)
library(httr)
library(survos)
# library(Hmisc)

###################
# Configuration
###################


# set file for csv
setwd("~")

# set configuration for api
projectCode <- "ryan01"
username <- "ryan01"
password <- "password"
endPoint <- paste("https://", projectCode, ".psymeasurement.com/api1.0", sep="")
jobId <- 122

loginSurvos(username, password)
assignments <- assignments(jobId=jobId)

data_columns = c("Id", "abortion","baby","whether_use","gender")
resample_iterations = 10


###################
# data processing #
###################


# import csv file
data<- read.csv(csv_filename, stringsAsFactors = F)

# only keep submitted surveys
data_raw<- subset(data, data$AssignmentStatusCode=="Submitted", 
                  select=data_columns)

# rename item names
#for (i in 1:(ncol(data_raw)-1)){
#  names(data_raw)[i+1] <- paste0("item",i)
#}
# reverse code items
#reverse_items<- c(paste0("item",c(3:5, 9:12)))
#data_raw[,reverse_items]<- 6 - data_raw[,reverse_items]

data_raw$total<- rowSums(data_raw[,2:ncol(data_raw)])
data_raw$mean<- rowMeans(data_raw[,2:(ncol(data_raw)-1)])

###############
# descriptive #
###############

describe(data_raw)

#############
# histogram #
#############

hist(data_raw$total, xlab="total", main="Total", breaks=max(data_raw$total))
# lines(density(data_raw$total))
hist(data_raw$mean, xlab="mean", main="Mean", breaks=as.integer(max(data_raw$mean)))

item_name<- names(data_raw[2:ncol(data_raw)])
par(mfrow=c(3,2)) # c("#rows,#cols")
for (i in item_name){
  hist(data_raw[[i]], xlab=i, main=paste("Histogram of",i))
}

#################
# data analysis #
#################

# variance
var(data_raw[2:(ncol(data_raw)-2)])

# covariance matrix
cov(data_raw[2:(ncol(data_raw)-2)], method="pearson")
cov(data_raw[2:(ncol(data_raw)-2)], method="kendall")
cov(data_raw[2:(ncol(data_raw)-2)], method="spearman")

# correlation matrix
cor(data_raw[2:(ncol(data_raw)-2)], method="pearson")
cor(data_raw[2:(ncol(data_raw)-2)], method="kendall")
cor(data_raw[2:(ncol(data_raw)-2)], method="spearman")


# alpha and interitem correlation
alpha(data_raw[2:(ncol(data_raw)-2)])

alpha50<- alpha(data_raw[2:(ncol(data_raw)-2)])$total$raw_alpha
average_r50<- alpha(data_raw[2:(ncol(data_raw)-2)])$total$average_r



###### resampling ######

# draw 5, 10, 15, 20, 25 samples from the serveys
# repeat for 1000 times and extract mean alpha and interitem correlation
draw_sample<- c(10, 15, 20, 25, 30, 40) # the number of sample being drawn
# define a resample function to keep code clean
resample.function<- function(data_raw, draw_sample, x){
  n<- nrow(data_raw) # count total number of observations
  N<- c(1:x) # the number of times to resample
  resample<- foreach(i=draw_sample, .combine='rbind') %dopar%{
    alpha_v<- foreach(j=N, .combine='rbind') %dopar%{
      obs<- sample(n, i, replace=F)
      samp<- data_raw[obs,]
      alpha(samp[2:(ncol(samp)-1)])$total$raw_alpha
    }
    average_r<- foreach(j=N, .combine='rbind') %dopar%{
      obs<- sample(n, i, replace=F)
      samp<- data_raw[obs,]
      alpha(samp[2:(ncol(samp)-1)])$total$average_r
    }
    alpha_mean<- mean(alpha_v)
    alpha_lower<- alpha_mean - qnorm(0.975)*sd(alpha_v)
    alpha_higher<- alpha_mean + qnorm(0.975)*sd(alpha_v)
    average_rmean<- mean(average_r)
    average_rlower<- average_rmean - qnorm(0.975)*sd(average_r)
    average_rhigher<- average_rmean + qnorm(0.975)*sd(average_r)
    alphar<- cbind(i, alpha_mean, alpha_lower, alpha_higher, average_rmean, average_rlower, average_rhigher)
  }
  return(resample)
}
resample<- resample.function(data_raw, draw_sample, 100)
# combind with the whole sample
i<- 50
alpha_mean<- alpha50
average_rmean<- average_r50
alphar50<- cbind(i, alpha_mean, average_rmean)
resample<- smartbind(resample, alphar50)

###### take out item ######

draw_item<- NULL # create an empty object
draw_item<- foreach(k=names(data_raw[2:(length(data_raw)-2)]), .combine='rbind') %dopar%{
  data_item<- data_raw[ , !names(data_raw) %in% c(i)]
  data_item_resample<- resample.function(data_item, draw_sample, 10)
  data_item_resample<- cbind(k, data_item_resample)
  draw_item<- rbind(draw_item, data_item_resample)
  i<- resample_iterations
  alpha_mean<- alpha(data_item[2:(ncol(data_raw)-1)])$total$raw_alpha
  average_rmean<- alpha(data_raw[2:(ncol(data_raw)-1)])$total$average_r
  alphar50<- cbind(k, i, alpha_mean, average_rmean)
  draw_item<- smartbind(draw_item, alphar50)
}

write.table(resample, file="resample.csv", sep=",", row.names=F)
write.table(draw_item, file="draw_item.csv", sep=",", row.names=F)

############
# graphics #
############

resample<- data.frame(resample)
par(mfrow=c(1,1))
ggplot(resample, aes(i)) + 
  geom_line(aes(y = alpha_mean), size=1.2, col="black") + 
  geom_point(aes(y = alpha_mean), size=5, col="black")+
  geom_line(aes(y = alpha_lower), size=1.2, col="black", linetype="dashed")+
  geom_line(aes(y = alpha_higher), size=1.2, col="black", linetype="dashed")+
  geom_line(aes(y = average_rmean), size=1.2, col="steelblue2")+
  geom_point(aes(y = average_rmean), size=5, shape=17, col="steelblue2")+
  geom_line(aes(y = average_rlower), size=1.2, col="steelblue2", linetype="dashed")+
  geom_line(aes(y = average_rhigher), size=1.2, col="steelblue2", linetype="dashed")+
  coord_cartesian(ylim=c(0,1.5))+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))+
  labs(x="Resample Size", y="Value")

