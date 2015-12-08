#install.packages('psych')
#install.packages('plyr')
#install.packages('gtools')
#install.packages('foreach')
#install.packages('doParallel')
#install.packages('ggplot2')
#install.packages('Hmisc')
#install.packages('reshape2')

library(psych)
library(plyr)
library(gtools)
library(foreach)
library(doParallel)
library(ggplot2)
library(reshape2)
# library(Hmisc)

###################
# Configuration   #
###################

csv_filename = "turk_job_cfcs_wave_51_assignments.csv"
data_columns = c("Id", "q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12")

###################
# data processing #
###################

# set file directory
setwd("~/")
# set image file directory to export images
image_directory<- "~/images/"

# import csv file
data<- read.csv(csv_filename, stringsAsFactors = F)

# only keep submitted surveys
data_raw<- subset(data, AssignmentStatusCode=="Submitted"| AssignmentStatusCode=="Approved", 
                  select=data_columns)
N<- length(data_columns)-1

# rename item names
#for (i in 1:(ncol(data_raw)-1)){
#  names(data_raw)[i+1] <- paste0("item",i)
#}
# reverse code items
#reverse_items<- c(paste0("item",c(3:5, 9:12)))
#data_raw[,reverse_items]<- 6 - data_raw[,reverse_items]

data_raw$total<- rowSums(data_raw[,2:(N+1)])
data_raw$mean<- rowMeans(data_raw[,2:(N+1)])

write.table(data_raw, file="data_raw.csv", sep=",", row.names = F)

###############
# descriptive #
###############

describe(data_raw)

#############
# histogram #
#############

hist(data_raw$total, xlab="Total", main="Histogram of Total Raw Score", breaks=max(data_raw$total))
# lines(density(data_raw$total))
hist(data_raw$mean, xlab="mean", main="Mean", breaks=as.integer(N))

item_name<- data_columns[-1]
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

alpha_raw50<- alpha(data_raw[2:(ncol(data_raw)-2)])$total$raw_alpha
alpha_std50<- alpha(data_raw[2:(ncol(data_raw)-2)])$total$std.alpha
average_r50<- alpha(data_raw[2:(ncol(data_raw)-2)])$total$average_r



###### resampling ######

# draw 5, 10, 15, 20, 25 samples from the serveys
# repeat for 1000 times and extract mean alpha and interitem correlation
draw_sample<- c(10, 15, 20, 25, 30, 40) # the number of sample being drawn
# define a resample function to keep code clean
resample.function<- function(data_raw, draw_sample, x){
  n<- nrow(data_raw) # count total number of observations
  N<- x # the number of times to resample
  alpha_v<- numeric(N)# create a vector to store the results
  alpha_stdv<- numeric(N)
  average_r<- numeric(N)
  resample<- foreach(i=draw_sample, .combine='rbind') %dopar%{
    for (j in 1:N){
      obs<- sample(n, i, replace=F)
      samp<- data_raw[obs,]
      alpha_list<- alpha(samp[2:(ncol(samp)-2)])
      alpha_v[j]<- alpha_list$total$raw_alpha # extract raw alpha value
      alpha_stdv[j]<- alpha_list$total$std.alpha # extract standardized alpha value
      average_r[j]<- alpha_list$total$average_r # extract average r value
    }
    alpha_mean<- mean(alpha_v)
    alpha_lower<- alpha_mean - qnorm(0.975)*sd(alpha_v) # calculate confidence interval
    alpha_higher<- alpha_mean + qnorm(0.975)*sd(alpha_v)
    alpha_stdmean<- mean(alpha_stdv)
    alpha_stdlower<- alpha_stdmean - qnorm(0.975)*sd(alpha_stdv)
    alpha_stdhigher<- alpha_stdmean + qnorm(0.975)*sd(alpha_stdv)
    average_rmean<- mean(average_r)
    average_rlower<- average_rmean - qnorm(0.975)*sd(average_r)
    average_rhigher<- average_rmean + qnorm(0.975)*sd(average_r)
    alphar<- cbind(i, alpha_mean, alpha_lower, alpha_higher, alpha_stdmean, alpha_stdlower, alpha_stdhigher, 
                   average_rmean, average_rlower, average_rhigher)
    
  }
  return(resample) # specify which data frame to return in a function
}
resample<- resample.function(data_raw, draw_sample, 100) # run 100 bootstrap
# combind with the whole sample
i<- 50
alpha_mean<- alpha_raw50
alpha_stdmean<- alpha_std50
average_rmean<- average_r50
alphar50<- cbind(i, alpha_mean, alpha_stdmean, average_rmean)
resample<- smartbind(resample, alphar50)
for (i in 1:ncol(resample)){ # replace missing values to the previous value in the same row
  resample[7,i]<- ifelse(is.na(resample[7,i]), resample[7,i-1], resample[7,i])
}


###### take out item ######

draw_item<- NULL # create an empty object
for (k in names(data_raw[2:(length(data_raw)-2)])){
  data_item<- data_raw[ , !names(data_raw) %in% c(i)] # delete one variable at a time
  data_item_resample<- resample.function(data_item, draw_sample, 10) # 10 bootstrap
  data_item_resample<- cbind(k, data_item_resample)
  draw_item<- rbind(draw_item, data_item_resample)
  i<- 50
  alpha_mean<- alpha(data_item[2:(ncol(data_raw)-2)])$total$raw_alpha
  alpha_stdmean<- alpha(data_raw[2:(ncol(data_raw)-2)])$total$std.alpha
  average_rmean<- alpha(data_raw[2:(ncol(data_raw)-2)])$total$average_r
  alphar50<- cbind(k, i, alpha_mean, alpha_stdmean, average_rmean)
  draw_item<- smartbind(draw_item, alphar50)
}
for (i in 1:ncol(draw_item)){ # replace missing values
  draw_item[,i]<- ifelse(is.na(draw_item[,i]), draw_item[,i-1], draw_item[,i])
}

# export data frame as csv files
write.table(resample, file="resample.csv", sep=",", row.names=F)
write.table(draw_item, file="draw_item.csv", sep=",", row.names=F)

############
# graphics #
############

resample<- data.frame(resample)
par(mfrow=c(1,1))
alpha_r<- melt(resample[,c("i","stdalpha_mean","average_rmean")], id="i")
alpha_rhiger<- melt(resample[,c("i","alpha_stdhigher","average_rhigher")],id="i", value.name = "higher")
alpha_rlower<- melt(resample[,c("i","alpha_stdlower","average_rlower")],id="i", value.name = "lower")
alpha_r<- cbind(alpha_r, alpha_rhiger, alpha_rlower)
plot_alphar<- ggplot(alpha_r, aes(x=i, y=value, color=variable)) + 
  geom_line(aes(y = value), size=1.2) + 
  geom_point(aes(y = value), size=5)+
  geom_line(aes(y = higher, color=variable), size=1.2, linetype="dashed")+
  geom_line(aes(y = lower, color=variable), size=1.2, linetype="dashed")+
  coord_cartesian(ylim=c(-0.1,1.1))+
  theme(legend.key= element_blank(), legend.background= element_rect(color="black"))+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))+
  labs(x="Resample Size", y="Value", color="Parameters")
plot_alphar
  

# save all the graphics
png(file=paste0(image_directory,"Histogram of Total Raw Score.png"), width = 5, height = 4, units = 'in', res = 300)
hist(data_raw$total, xlab="Total", main="Histogram of Total Raw Score", breaks=max(data_raw$total))
dev.off()

png(file=paste0(image_directory,"Histogram of All Items.png"), width = 8, height = 8, units = 'in', res = 200)
par(mfrow=c(3,4)) # c("#rows,#cols")
for (i in item_name){
  hist(data_raw[[i]], xlab=i, main=paste("Histogram of",i))
}
dev.off()

png(file=paste0(image_directory,"alpha_convergence.png"), width = 6, height = 4, units = 'in', res = 300)
plot_alphar
dev.off()
