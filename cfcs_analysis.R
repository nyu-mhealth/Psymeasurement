#install.packages('psych')
#install.packages('plyr')
#install.packages('gtools')
#install.packages('foreach')
#install.packages('doParallel')
#install.packages('ggplot2')
#install.packages('Hmisc')
install.packages('reshape2')

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
cfcs_columns = c("Id", "q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12")

###################
# data processing #
###################

# set file directory
setwd("~/Documents/projects/Psychometrics/psymeasurement-scripts")


# import csv file
cfcs<- read.csv(csv_filename, stringsAsFactors = F)

# only keep submitted surveys
cfcs_submit<- subset(cfcs, cfcs$AssignmentStatusCode=="Submitted" | AssignmentStatusCode=="Approved", 
                     select=c(Id, influence:q13__score_dichotomous))
cfcs_submit<- cfcs_submit[, !grepl("q10", names(cfcs_submit))]

# select out raw scores to analyze
cfcs_raw<- cbind(cfcs_submit$Id, cfcs_submit[, grepl("score_raw", names(cfcs_submit))])
# rename item names
for (i in 1:12){
  names(cfcs_raw)[i+1] <- paste0("q",i)
}
cfcs_raw<- rename(cfcs_raw, c("cfcs_submit$Id"="ID"))

# reverse code items
reverse_items<- c(paste0("q",c(3:5, 9:12)))
cfcs_raw[,reverse_items]<- 6 - cfcs_raw[,reverse_items]

cfcs_raw$total<- rowSums(cfcs_raw[,2:ncol(cfcs_raw)])
cfcs_raw$mean<- rowMeans(cfcs_raw[,2:(ncol(cfcs_raw)-1)])

###############
# descriptive #
###############

describe(cfcs_raw)

#############
# histogram #
#############

hist(cfcs_raw$total, xlab="total", main="Total", breaks=max(cfcs_raw$total))
# lines(density(cfcs_raw$total))
hist(cfcs_raw$mean, xlab="mean", main="Mean", breaks=as.integer(max(cfcs_raw$mean)))

item_name<- names(cfcs_raw[2:(ncol(cfcs_raw)-2)])
par(mfrow=c(3,2)) # c("#rows,#cols")
for (i in item_name){
  hist(cfcs_raw[[i]], xlab=i, main=paste("Histogram of",i))
}

#################
# data analysis #
#################

# variance
var(cfcs_raw[2:(ncol(cfcs_raw)-2)])

# covariance matrix
cov(cfcs_raw[2:(ncol(cfcs_raw)-2)], method="pearson")
cov(cfcs_raw[2:(ncol(cfcs_raw)-2)], method="kendall")
cov(cfcs_raw[2:(ncol(cfcs_raw)-2)], method="spearman")

# correlation matrix
cor(cfcs_raw[2:(ncol(cfcs_raw)-2)], method="pearson")
cor(cfcs_raw[2:(ncol(cfcs_raw)-2)], method="kendall")
cor(cfcs_raw[2:(ncol(cfcs_raw)-2)], method="spearman")


# alpha and interitem correlation
alpha(cfcs_raw[2:(ncol(cfcs_raw)-2)])

alpha_raw50<- alpha(cfcs_raw[2:(ncol(cfcs_raw)-2)])$total$raw_alpha
alpha_std50<- alpha(cfcs_raw[2:(ncol(cfcs_raw)-2)])$total$std.alpha
average_r50<- alpha(cfcs_raw[2:(ncol(cfcs_raw)-2)])$total$average_r



###### resampling ######

# draw 5, 10, 15, 20, 25 samples from the serveys
# repeat for 1000 times and extract mean alpha and interitem correlation
draw_sample<- c(10, 15, 20, 25, 30, 40) # the number of sample being drawn
# define a resample function to keep code clean
resample.function<- function(cfcs_raw, draw_sample, x){
  n<- nrow(cfcs_raw) # count total number of observations
  N<- x # the number of times to resample
  alpha_v<- numeric(N)# create a vector to store the results
  alpha_stdv<- numeric(N)
  average_r<- numeric(N)
  resample<- foreach(i=draw_sample, .combine='rbind') %dopar%{
    for (j in 1:N){
      obs<- sample(n, i, replace=F)
      samp<- cfcs_raw[obs,]
      alpha_list<- alpha(samp[2:(ncol(samp)-2)])
      alpha_v[j]<- alpha_list$total$raw_alpha
      alpha_stdv[j]<- alpha_list$total$std.alpha
      average_r[j]<- alpha_list$total$average_r
    }
    alpha_mean<- mean(alpha_v)
    alpha_lower<- alpha_mean - qnorm(0.975)*sd(alpha_v)
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
  return(resample)
}
resample<- resample.function(cfcs_raw, draw_sample, 100)
# combind with the whole sample
i<- 50
alpha_mean<- alpha_raw50
alpha_stdmean<- alpha_std50
average_rmean<- average_r50
alphar50<- cbind(i, alpha_mean, alpha_stdmean, average_rmean)
resample<- smartbind(resample, alphar50)
for (i in 1:ncol(resample)){
  resample[7,i]<- ifelse(is.na(resample[7,i]), resample[7,i-1], resample[7,i])
}


###### take out item ######

draw_item<- NULL # create an empty object
for (k in names(cfcs_raw[2:(length(cfcs_raw)-2)])){
  cfcs_item<- cfcs_raw[ , !names(cfcs_raw) %in% c(i)]
  cfcs_item_resample<- resample.function(cfcs_item, draw_sample, 10)
  cfcs_item_resample<- cbind(k, cfcs_item_resample)
  draw_item<- rbind(draw_item, cfcs_item_resample)
  i<- 50
  alpha_mean<- alpha(cfcs_item[2:(ncol(cfcs_raw)-2)])$total$raw_alpha
  alpha_stdmean<- alpha(cfcs_raw[2:(ncol(cfcs_raw)-2)])$total$std.alpha
  average_rmean<- alpha(cfcs_raw[2:(ncol(cfcs_raw)-2)])$total$average_r
  alphar50<- cbind(k, i, alpha_mean, alpha_stdmean, average_rmean)
  draw_item<- smartbind(draw_item, alphar50)
}
for (i in 1:ncol(draw_item)){
  draw_item[,i]<- ifelse(is.na(draw_item[,i]), draw_item[,i-1], draw_item[,i])
}


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

# create a dichotomous variable for each question
for (i in item_name){
  cfcs_raw[[paste0(i,"_bin",sep="")]]<- ifelse(cfcs_raw[[i]]<=3, 0, 1)
}

# 5 item raw scored
raw_items<- c(paste0("q",c(1:2, 6:8)))
s1_count<- paste0(raw_items,"_bin")
a<- length(s1_count)
# 7 item reverse scored
s2_count<- paste0(reverse_items,"_bin")
b<- length(s2_count)
N<- a+b

# basic descriptives 
describe(cfcs_raw[,grep("_bin", names(cfcs_raw))])

# Aggregate scores
cfcs_raw$Total_count<- 0
for (i in c(s1_count,s2_count)){
  cfcs_raw$Total_count<- cfcs_raw$Total_count+ cfcs_raw[[i]]
}
cfcs_raw$s1_count<- 0
for (i in s1_count){
  cfcs_raw$s1_count<- cfcs_raw$s1_count+ cfcs_raw[[i]]
}
cfcs_raw$s2_count<- 0
for (i in s2_count){
  cfcs_raw$s2_count<- cfcs_raw$s2_count+ cfcs_raw[[i]]
}

# standardize total scores
cfcs_raw$Total_std<- scale(cfcs_raw$Total_count)
cfcs_raw$s1_std<- scale(cfcs_raw$s1_count)
cfcs_raw$s2_std<- scale(cfcs_raw$s2_count)

# dx cutpoints
for (i in 1:N){
  cfcs_raw[[paste0("SD",i,sep="")]]<- ifelse(cfcs_raw$Total_count>=i, 1, 0)
  cfcs_raw[[paste0("SD_s1",i,sep="")]]<- ifelse(cfcs_raw$s1_count>=i, 1, 0)
  cfcs_raw[[paste0("SD_s2",i,sep="")]]<- ifelse(cfcs_raw$s2_count>=i, 1, 0)
}

# counts across cut-points
for (i in 1:N){
  # Total score
  cfcs_raw[[paste0("TP_t",i,sep="")]]<- ifelse(cfcs_raw$Total_count>=i, cfcs_raw$Total_count, NA)
  cfcs_raw[[paste0("FN_t",i,sep="")]]<- ifelse(cfcs_raw$Total_count>=i, N-cfcs_raw$Total_count, NA)
  cfcs_raw[[paste0("FP_t",i,sep="")]]<- ifelse(cfcs_raw$Total_count<i, cfcs_raw$Total_count, NA)
  cfcs_raw[[paste0("TN_t",i,sep="")]]<- ifelse(cfcs_raw$Total_count<i, N-cfcs_raw$Total_count, NA)
  # scale 1
  cfcs_raw[[paste0("TP_s1",i,sep="")]]<- ifelse(cfcs_raw$s1_count>=i, cfcs_raw$s1_count, NA)
  cfcs_raw[[paste0("FN_s1",i,sep="")]]<- ifelse(cfcs_raw$s1_count>=i, a-cfcs_raw$s1_count, NA)
  cfcs_raw[[paste0("FP_s1",i,sep="")]]<- ifelse(cfcs_raw$s1_count<i, cfcs_raw$s1_count, NA)
  cfcs_raw[[paste0("TN_s1",i,sep="")]]<- ifelse(cfcs_raw$s1_count<i, a-cfcs_raw$s1_count, NA)
  # scale 2
  cfcs_raw[[paste0("TP_s2",i,sep="")]]<- ifelse(cfcs_raw$s2_count>=i, cfcs_raw$s2_count, NA)
  cfcs_raw[[paste0("FN_s2",i,sep="")]]<- ifelse(cfcs_raw$s2_count>=i, b-cfcs_raw$s2_count, NA)
  cfcs_raw[[paste0("FP_s2",i,sep="")]]<- ifelse(cfcs_raw$s2_count<i, cfcs_raw$s2_count, NA)
  cfcs_raw[[paste0("TN_s2",i,sep="")]]<- ifelse(cfcs_raw$s2_count<i, b-cfcs_raw$s2_count, NA)
}

######### histogram #########
hist(cfcs_raw$Total_count, xlab="Total count", main="Total histogram", breaks=as.integer(N))
hist(cfcs_raw$s1_count, xlab="s1 count", main="s1 histogram", breaks=as.integer(a))
hist(cfcs_raw$s2_count, xlab="s2 count", main="s2 histogram", breaks=as.integer(b))

######### reshape table for calculating sensspec.. #########
# sum over cutpoint
varlist<- c("TP_t","FN_t","FP_t","TN_t","TP_s1","FN_s1","FP_s1","TN_s1","TP_s2","FN_s2","FP_s2","TN_s2")
cutpoint<- 1:N
cfcs_cutpoint<- data.frame(cutpoint)
for (i in varlist){
  i<- colSums(cfcs_raw[grep(i, names(cfcs_raw), value=TRUE)], na.rm=T, dims=1)
  cfcs_cutpoint<- data.frame(cfcs_cutpoint, i) 
}
colnames(cfcs_cutpoint)<- c("cutpoint", varlist)

# Sensitivity and Specificity
for (i in c("t","s1","s2")){
  cfcs_cutpoint[[paste0("sens_",i,sep="")]]<- 
    cfcs_cutpoint[[paste0("TP_",i,sep="")]]/
    (cfcs_cutpoint[[paste0("TP_",i,sep="")]]+cfcs_cutpoint[[paste0("FN_",i,sep="")]])
  cfcs_cutpoint[[paste0("spec_",i,sep="")]]<- 
    cfcs_cutpoint[[paste0("TN_",i,sep="")]]/
    (cfcs_cutpoint[[paste0("TN_",i,sep="")]]+cfcs_cutpoint[[paste0("FP_",i,sep="")]])
  cfcs_cutpoint[[paste0("spec_",i,"_1",sep="")]]<- 1-cfcs_cutpoint[[paste0("spec_",i,sep="")]]
}

# Positive and Negative Likelihood Ratios
for (i in c("t","s1","s2")){
  cfcs_cutpoint[[paste0("LRpos_",i,sep="")]]<- 
    cfcs_cutpoint[[paste0("sens_",i,sep="")]]/
    (1-cfcs_cutpoint[[paste0("spec_",i,sep="")]])
  cfcs_cutpoint[[paste0("LRneg_",i,sep="")]]<- 
    cfcs_cutpoint[[paste0("spec_",i,sep="")]]/
    (1-cfcs_cutpoint[[paste0("sens_",i,sep="")]])
}

# Positive and Negative Predictive Power
for (i in c("t","s1","s2")){
  cfcs_cutpoint[[paste0("PPP_",i,sep="")]]<- 
    cfcs_cutpoint[[paste0("TP_",i,sep="")]]/
    (cfcs_cutpoint[[paste0("TP_",i,sep="")]]+cfcs_cutpoint[[paste0("FP_",i,sep="")]])
  cfcs_cutpoint[[paste0("NPP_",i,sep="")]]<- 
    cfcs_cutpoint[[paste0("TN_",i,sep="")]]/
    (cfcs_cutpoint[[paste0("TN_",i,sep="")]]+cfcs_cutpoint[[paste0("FN_",i,sep="")]])
}

# Bayesian Predictive Power (accounting for prevalence)
for (i in c("t","s1","s2")){
  for (p in c(0.99, 0.90, 0.75, 0.5, 0.25, 0.1, 0.01)){
    cfcs_cutpoint[[paste0("PPPb_",i,p,sep="")]]<- 
      (cfcs_cutpoint[[paste0("sens_",i,sep="")]]*p)/
      ((cfcs_cutpoint[[paste0("sens_",i,sep="")]]*p)+(1-p)*(1-cfcs_cutpoint[[paste0("spec_",i,sep="")]]))
    cfcs_cutpoint[[paste0("NPPb_",i,p,sep="")]]<- 
      cfcs_cutpoint[[paste0("spec_",i,sep="")]]*(1-p)/
      (cfcs_cutpoint[[paste0("spec_",i,sep="")]]*(1-p)+p*(1-cfcs_cutpoint[[paste0("sens_",i,sep="")]]))
  }
}

############
# graphics #
############

# sensitivity and specificity
sensspec<- melt(cfcs_cutpoint[,c("cutpoint","sens_t","spec_t")], id="cutpoint")
ggplot(sensspec, aes(x=cutpoint, y=value, color=variable))+
  geom_line(aes(y = value), size=1.2)+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))+
  coord_cartesian(ylim=c(0,1.1))+
  theme(legend.position=c(0.9,0.15), legend.key= element_blank(), legend.background= element_rect(color="black"))

# PPP and NPP
pppnpp<- melt(cfcs_cutpoint[,c("cutpoint","PPP_t","NPP_t")], id="cutpoint")
ggplot(pppnpp, aes(x=cutpoint, y=value, color=variable))+
  geom_line(aes(y = value), size=1.2)+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))+
  coord_cartesian(ylim=c(0,1.1))+
  theme(legend.position=c(0.8,0.15), legend.key= element_blank(), legend.background= element_rect(color="black"))

# PPPb
pppb<- melt(cbind(cutpoint,cfcs_cutpoint[,grep("PPPb_t", names(cfcs_cutpoint))]), id="cutpoint")
ggplot(pppb, aes(x=cutpoint, y=value, color=variable))+
  geom_line(aes(y = value), size=1.2)+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))+
  coord_cartesian(ylim=c(0,1.1))+
  theme(legend.key= element_blank(), legend.background= element_rect(color="black"))

# NPPb
nppb<- melt(cbind(cutpoint,cfcs_cutpoint[,grep("NPPb_t", names(cfcs_cutpoint))]), id="cutpoint")
ggplot(nppb, aes(x=cutpoint, y=value, color=variable))+
  geom_line(aes(y = value), size=1.2)+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))+
  coord_cartesian(ylim=c(0,1.1))+
  theme(legend.key= element_blank(), legend.background= element_rect(color="black"))

