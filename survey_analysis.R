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

csv_filename = "data/turk_job_reproductive_autonomy_wave_78_assignments.csv"
data_columns = c("Id", "abortion", "baby", "whether_use", "gender")

###################
# data processing #
###################

# set file directory
setwd("~/")


# import csv file
data<- read.csv(csv_filename, stringsAsFactors = F)

# only keep submitted surveys
data_raw<- subset(data, AssignmentStatusCode=="Submitted"| AssignmentStatusCode=="Approved", 
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

item_name<- names(data_raw[2:(ncol(data_raw)-2)])
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
ggplot(resample, aes(i)) + 
  geom_line(aes(y = alpha_stdmean), size=1.2, col="black") + 
  geom_point(aes(y = alpha_stdmean), size=5, col="black")+
  geom_line(aes(y = alpha_stdlower), size=1.2, col="black", linetype="dashed")+
  geom_line(aes(y = alpha_stdhigher), size=1.2, col="black", linetype="dashed")+
  geom_line(aes(y = average_rmean), size=1.2, col="steelblue2")+
  geom_point(aes(y = average_rmean), size=5, shape=17, col="steelblue2")+
  geom_line(aes(y = average_rlower), size=1.2, col="steelblue2", linetype="dashed")+
  geom_line(aes(y = average_rhigher), size=1.2, col="steelblue2", linetype="dashed")+
  coord_cartesian(ylim=c(0,1))+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))+
  labs(x="Resample Size", y="Value")


############
# validity #
############

## create a dichotomous variable for each question ##
all_items<- data_columns[-1]
total_count<- paste0(all_items,"_bin")
for (i in all_items){
  data_raw[[paste0(i,"_bin",sep="")]]<- ifelse(data_raw[[i]]<=2, 0, 1)
}
N<- length(total_count)

# If you don't have subscales, don't run line 205-213
# define subscales
# items raw scored
raw_items<- c("abortion","baby")
s1_count<- paste0(raw_items,"_bin")
a<- length(s1_count)
# items reverse scored
reverse_items<- c("whether_use","gender")
s2_count<- paste0(reverse_items,"_bin")
b<- length(s2_count)

## basic descriptives ##
describe(data_raw[,grep("_bin", names(data_raw))])

## Aggregate scores ##
data_raw$Total_count<- 0
for (i in c(total_count)){
  data_raw$Total_count<- data_raw$Total_count+ data_raw[[i]]
}
# If you don't have subscales, don't run line 224-231
data_raw$s1_count<- 0
for (i in s1_count){
  data_raw$s1_count<- data_raw$s1_count+ data_raw[[i]]
}
data_raw$s2_count<- 0
for (i in s2_count){
  data_raw$s2_count<- data_raw$s2_count+ data_raw[[i]]
}

## standardize total scores ##
data_raw$Total_std<- scale(data_raw$Total_count)
# If you don't have subscales, don't run line 236-237
data_raw$s1_std<- scale(data_raw$s1_count)
data_raw$s2_std<- scale(data_raw$s2_count)

## dx cutpoints ##
cutpoint<- 1:N
for (i in 1:N){
  data_raw[[paste0("SD",i,sep="")]]<- ifelse(data_raw$Total_count>=i, 1, 0)
}
# If you don't have subscales, don't run line 244-247
for (i in 1:N){
  data_raw[[paste0("SD_s1",i,sep="")]]<- ifelse(data_raw$s1_count>=i, 1, 0)
  data_raw[[paste0("SD_s2",i,sep="")]]<- ifelse(data_raw$s2_count>=i, 1, 0)
}

## counts across cut-points ##
for (i in 1:N){
  # Total score
  data_raw[[paste0("TP_t",i,sep="")]]<- ifelse(data_raw$Total_count>=i, data_raw$Total_count, NA)
  data_raw[[paste0("FN_t",i,sep="")]]<- ifelse(data_raw$Total_count>=i, N-data_raw$Total_count, NA)
  data_raw[[paste0("FP_t",i,sep="")]]<- ifelse(data_raw$Total_count<i, data_raw$Total_count, NA)
  data_raw[[paste0("TN_t",i,sep="")]]<- ifelse(data_raw$Total_count<i, N-data_raw$Total_count, NA)
}
for (i in 1:N){
  # Total score
  data_raw[[paste0("q",i,"_true")]]<- ifelse(data_raw$Total_count>=i & data_raw[[paste0("q",i,"_bin")]]==1, 1, 0)
  data_raw[[paste0("q",i,"_true")]]<- ifelse(data_raw$Total_count<i & data_raw[[paste0("q",i,"_bin")]]==0, 
                                             1, data_raw[[paste0("q",i,"_true")]])
}
count_true<- colSums(data_raw[,grep("_true",names(data_raw))])
count_true<- data.frame(cbind(cutpoint, count_true))
qplot(cutpoint, data=count_true, geom="bar", weight=count_true, xlab="item")+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))
# If you don't have subscales, don't run line 270-281
for (i in 1:N){
  # scale 1
  data_raw[[paste0("TP_s1",i,sep="")]]<- ifelse(data_raw$s1_count>=i, data_raw$s1_count, NA)
  data_raw[[paste0("FN_s1",i,sep="")]]<- ifelse(data_raw$s1_count>=i, a-data_raw$s1_count, NA)
  data_raw[[paste0("FP_s1",i,sep="")]]<- ifelse(data_raw$s1_count<i, data_raw$s1_count, NA)
  data_raw[[paste0("TN_s1",i,sep="")]]<- ifelse(data_raw$s1_count<i, a-data_raw$s1_count, NA)
  # scale 2
  data_raw[[paste0("TP_s2",i,sep="")]]<- ifelse(data_raw$s2_count>=i, data_raw$s2_count, NA)
  data_raw[[paste0("FN_s2",i,sep="")]]<- ifelse(data_raw$s2_count>=i, b-data_raw$s2_count, NA)
  data_raw[[paste0("FP_s2",i,sep="")]]<- ifelse(data_raw$s2_count<i, data_raw$s2_count, NA)
  data_raw[[paste0("TN_s2",i,sep="")]]<- ifelse(data_raw$s2_count<i, b-data_raw$s2_count, NA)
}

######### histogram #########
hist(data_raw$Total_count, xlab="Total count", main="Total histogram", breaks=as.integer(N), xlim=c(0,N))
# If you don't have subscales, don't run line 286-287
hist(data_raw$s1_count, xlab="s1 count", main="s1 histogram", breaks=as.integer(a), xlim=c(0,N))
hist(data_raw$s2_count, xlab="s2 count", main="s2 histogram", breaks=as.integer(b), xlim=c(0,N))

######### reshape table for calculating sensspec.. #########
## sum over cutpoint ##
varlist<- c("TP_t","FN_t","FP_t","TN_t")
# If you don't have subscales, don't run line 293
varlist<- c(varlist,"TP_s1","FN_s1","FP_s1","TN_s1","TP_s2","FN_s2","FP_s2","TN_s2")
data_cutpoint<- data.frame(cutpoint)
for (i in varlist){
  i<- colSums(data_raw[grep(i, names(data_raw), value=TRUE)], na.rm=T, dims=1)
  data_cutpoint<- data.frame(data_cutpoint, i) 
}
colnames(data_cutpoint)<- c("cutpoint", varlist)

## define scales ##
scales<- c("t")
# If you don't have subscales, don't run line 304
scales<- c(scales, "s1","s2")

## Sensitivity and Specificity ##
for (i in scales){
  data_cutpoint[[paste0("sens_",i,sep="")]]<- 
    data_cutpoint[[paste0("TP_",i,sep="")]]/
    (data_cutpoint[[paste0("TP_",i,sep="")]]+data_cutpoint[[paste0("FN_",i,sep="")]])
  data_cutpoint[[paste0("spec_",i,sep="")]]<- 
    data_cutpoint[[paste0("TN_",i,sep="")]]/
    (data_cutpoint[[paste0("TN_",i,sep="")]]+data_cutpoint[[paste0("FP_",i,sep="")]])
  data_cutpoint[[paste0("spec_",i,"_1",sep="")]]<- 1-data_cutpoint[[paste0("spec_",i,sep="")]]
}

# Positive and Negative Predictive Power
for (i in scales){
  data_cutpoint[[paste0("PPP_",i,sep="")]]<- 
    data_cutpoint[[paste0("TP_",i,sep="")]]/
    (data_cutpoint[[paste0("TP_",i,sep="")]]+data_cutpoint[[paste0("FP_",i,sep="")]])
  data_cutpoint[[paste0("NPP_",i,sep="")]]<- 
    data_cutpoint[[paste0("TN_",i,sep="")]]/
    (data_cutpoint[[paste0("TN_",i,sep="")]]+data_cutpoint[[paste0("FN_",i,sep="")]])
}


############
# graphics #
############

# sensitivity and specificity
sensspec<- melt(data_cutpoint[,c("cutpoint","sens_t","spec_t")], id="cutpoint")
ggplot(sensspec, aes(x=cutpoint, y=value, color=variable))+
  geom_line(aes(y = value), size=1.2)+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))+
  coord_cartesian(ylim=c(0,1.1))+
  theme(legend.position=c(0.9,0.15), legend.key= element_blank(), legend.background= element_rect(color="black"))

# PPP and NPP
pppnpp<- melt(data_cutpoint[,c("cutpoint","PPP_t","NPP_t")], id="cutpoint")
ggplot(pppnpp, aes(x=cutpoint, y=value, color=variable))+
  geom_line(aes(y = value), size=1.2)+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))+
  coord_cartesian(ylim=c(0,1.1))+
  theme(legend.position=c(0.8,0.15), legend.key= element_blank(), legend.background= element_rect(color="black"))


