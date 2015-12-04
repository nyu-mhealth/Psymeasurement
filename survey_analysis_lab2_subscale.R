
# To run this script, make sure you have data_raw from lab 2 in the upper right window under Data

library(plyr)
library(ggplot2)
library(reshape2)

#################
# Configuration #
#################

# How many subscales do you have? Put number after s<- 
s<- 2

# What items are in each subscale? Put names of variables in c() with comma
s1<- c("q1","q2","q6","q7","q8")
s2<- c("q3","q4","q5","q9","q10","q11","q12")
s3<- c("")
s4<- c("")

# Which positions are subscale items located? Put numbers in c()
s1n<- c(1,2,6,7,8)
s2n<- c(3,4,5,9,10,11,12)
s3n<- c()
s4n<- c()

###################
# Data Processing #
###################

# generate subscale variable names
subscale<- paste0("s",1:s)
subscale_count<- paste0(subscale, "_count")
for (i in 1:s){
  assign(subscale_count[i], paste0(get(subscale[i]),"_bin"))
}

# generate sub scale total scores
for (j in subscale_count){
  data_raw[j]<- 0
  for (i in get(j)){
    data_raw[j]<- data_raw[j]+ data_raw[[i]]
  }
}

# generate sub scale counts across cutpoints
for (j in subscale_count){
  for (i in 1:N){
    a<- as.numeric(length(get(j)))
    data_raw[[paste0("TP",j,i)]]<- ifelse(data_raw[j]>=i, data_raw[[j]], NA)
    data_raw[[paste0("FN",j,i)]]<- ifelse(data_raw[j]>=i, a-data_raw[[j]], NA)
    data_raw[[paste0("FP",j,i)]]<- ifelse(data_raw[j]<i, data_raw[[j]], NA)
    data_raw[[paste0("TN",j,i)]]<- ifelse(data_raw[j]<i, a-data_raw[[j]], NA)
  }
}

# reshape table for calculating sensspec.. 
subvarlist<- NULL
for (i in subscale){
  a<- paste(c("TP","FN","FP","TN"),i, sep="")
  subvarlist<- c(subvarlist,a)
}
subvarlist<- c(subvarlist,"q_TP_5")
# sum over cutpoint
sub_cutpoint<- data.frame(cutpoint)
for (i in subvarlist){
  a<- colSums(data_raw[grep(i, names(data_raw), value=TRUE)], na.rm=T, dims=1)
  sub_cutpoint<- data.frame(sub_cutpoint, a) 
}
colnames(sub_cutpoint)<- c("cutpoint", subvarlist)

# generate subscale indicating subscale items
sub_cutpoint$subscale<- NA
for (i in subscale){
  sub_cutpoint$subscale<- ifelse(sub_cutpoint$cutpoint %in% get(paste0(i,"n")),i,sub_cutpoint$subscale)
}

############
# ss & ppp #
############

# Sensitivity and Specificity
for (i in subscale){
  sub_cutpoint[[paste0("sens_",i,sep="")]]<- 
    sub_cutpoint[[paste0("TP",i,sep="")]]/
    (sub_cutpoint[[paste0("TP",i,sep="")]]+sub_cutpoint[[paste0("FN",i,sep="")]])
  sub_cutpoint[[paste0("spec_",i,sep="")]]<- 
    sub_cutpoint[[paste0("TN",i,sep="")]]/
    (sub_cutpoint[[paste0("TN",i,sep="")]]+sub_cutpoint[[paste0("FP",i,sep="")]])
  sub_cutpoint[[paste0("spec_",i,"_1",sep="")]]<- 1-sub_cutpoint[[paste0("spec_",i,sep="")]]
}

# Positive and Negative Predictive Power
for (i in subscale){
  sub_cutpoint[[paste0("PPP_",i,sep="")]]<- 
    sub_cutpoint[[paste0("TP",i,sep="")]]/
    (sub_cutpoint[[paste0("TP",i,sep="")]]+sub_cutpoint[[paste0("FP",i,sep="")]])
  sub_cutpoint[[paste0("NPP",i,sep="")]]<- 
    sub_cutpoint[[paste0("TN",i,sep="")]]/
    (sub_cutpoint[[paste0("TN",i,sep="")]]+sub_cutpoint[[paste0("FN",i,sep="")]])
}

############
# graphics #
############

# subscale total count histogram
for (i in subscale_count){
  a<- as.numeric(length(get(i)))
  hist(data_raw[[i]], xlab=i, main=paste(i,"histogram"), breaks=as.integer(a), xlim=c(0,N))
}

# item by subscale
plot_subscale<- ggplot(sub_cutpoint, aes(x=cutpoint, y=q_TP_5, color=factor(subscale)))+
  geom_point(aes(y = q_TP_5), size=5)+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))+
  labs(x="Item", y="Count", color="Subscale")
plot_subscale


###################
# export graphics #
###################

for (i in subscale_count){
  tiff(paste(i,"histogram.tiff"), width = 4, height = 4, units = 'in', res = 300)
  par(mfrow=c(1,1))
  a<- as.numeric(length(get(i)))
  hist(data_raw[[i]], xlab=i, main=paste(i,"histogram"), breaks=as.integer(a), xlim=c(0,N))
  dev.off()
}

tiff("Item by Subscale.tiff", width = 6, height = 4, units = 'in', res = 300)
plot_subscale
dev.off()


