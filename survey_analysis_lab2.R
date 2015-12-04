
# To run this script, make sure you have data_raw from lab 1 in the upper right window under Data

library(plyr)
library(ggplot2)
library(reshape2)

############
# validity #
############

## create a dichotomous variable for each question ##
all_items<- data_columns[-1]
total_count<- paste0(all_items,"_bin")
for (i in all_items){
  data_raw[[paste0(i,"_bin",sep="")]]<- ifelse(data_raw[[i]]<=3, 0, 1)
}
N<- length(total_count)

# If you don't have subscales, don't run line 21-29
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
# If you don't have subscales, don't run line 40-47
data_raw$s1_count<- 0
for (i in s1_count){
  data_raw$s1_count<- data_raw$s1_count+ data_raw[[i]]
}
data_raw$s2_count<- 0
for (i in s2_count){
  data_raw$s2_count<- data_raw$s2_count+ data_raw[[i]]
}

## Generate TP and TN for each item over set of cutpoints ##
cutpoint<- 1:N
for (i in 1:N){
  for (j in 1:5){
    data_raw[[paste("q_TP",j,i,sep="_")]]<- ifelse(data_raw$Total_count>=j & data_raw[[paste0("q",i,"_bin")]]==1, 1, 0)
  }
}
for (i in 1:N){
  for (j in 7:10){
    data_raw[[paste("q_TN",j,i,sep="_")]]<- ifelse(data_raw$Total_count<j & data_raw[[paste0("q",i,"_bin")]]==0, 1, 0)
  }
}

## counts across cut-points ##
for (i in 1:N){
  # Total score
  data_raw[[paste0("TP_t",i,sep="")]]<- ifelse(data_raw$Total_count>=i, data_raw$Total_count, NA)
  data_raw[[paste0("FN_t",i,sep="")]]<- ifelse(data_raw$Total_count>=i, N-data_raw$Total_count, NA)
  data_raw[[paste0("FP_t",i,sep="")]]<- ifelse(data_raw$Total_count<i, data_raw$Total_count, NA)
  data_raw[[paste0("TN_t",i,sep="")]]<- ifelse(data_raw$Total_count<i, N-data_raw$Total_count, NA)
}
# If you don't have subscales, don't run line 71-82
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
par(mfrow=c(1,1))
hist(data_raw$Total_count, xlab="Total", main="Histogram of Total Dichotomous Score", breaks=as.integer(N), xlim=c(0,N))
# If you don't have subscales, don't run line 88-89
hist(data_raw$s1_count, xlab="s1 count", main="s1 histogram", breaks=as.integer(a), xlim=c(0,N))
hist(data_raw$s2_count, xlab="s2 count", main="s2 histogram", breaks=as.integer(b), xlim=c(0,N))

######### reshape table for calculating sensspec.. #########
## sum over cutpoint ##
varlist<- c("q_TP_5","q_TN_10","TP_t","FN_t","FP_t","TN_t")
# If you don't have subscales, don't run line 95
varlist<- c(varlist,"TP_s1","FN_s1","FP_s1","TN_s1","TP_s2","FN_s2","FP_s2","TN_s2")

data_cutpoint<- data.frame(cutpoint)
for (i in varlist){
  i<- colSums(data_raw[grep(i, names(data_raw), value=TRUE)], na.rm=T, dims=1)
  data_cutpoint<- data.frame(data_cutpoint, i) 
}
colnames(data_cutpoint)<- c("cutpoint", varlist)

## define scales ##
scales<- c("t")
# If you don't have subscales, don't run line 107-109
scales<- c(scales, "s1","s2")
# generate subscale indicating subscale items
data_cutpoint$subscale<- ifelse(data_cutpoint$cutpoint %in% as.numeric(substring(raw_items,2)),1,2)

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

## Positive and Negative Predictive Power ##
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

## item plots ##
# TP when positive responses are rare
TP5<- data_raw[data_raw$Total_count<7,]
TP5<- colSums(TP5[grep("q_TP_5", names(TP5), value=TRUE)], na.rm=T, dims=1)
TP5<- data.frame(cutpoint, TP5) 
plot_TP5<- ggplot(TP5,aes(x=cutpoint,y=TP5,fill=factor(cutpoint)))+
  geom_bar(stat="identity",position="dodge")+
  scale_x_continuous(breaks=1:12)+
  scale_fill_discrete(name="item")+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))+
  xlab("Item")+ylab("Count")
plot_TP5

# TN when negative responses are rare
TN10<- data_raw[data_raw$Total_count>7,]
TN10<- colSums(TN10[grep("q_TN_10", names(TN10), value=TRUE)], na.rm=T, dims=1)
TN10<- data.frame(cutpoint, TN10) 
plot_TN10<- ggplot(TN10,aes(x=cutpoint,y=TN10,fill=factor(cutpoint)))+
  geom_bar(stat="identity",position="dodge")+
  scale_x_continuous(breaks=1:12)+
  scale_fill_discrete(name="item")+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))+
  xlab("Item")+ylab("Count")
plot_TN10

# If you don't have subscale, don't run line 185-190
## item by subscale ##
ggplot(data_cutpoint, aes(x=cutpoint, y=q_TP_5, color=factor(subscale)))+
  geom_point(aes(y = q_TP_5), size=5)+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))+
  labs(x="Item", y="Count", color="Subscale")

## sensitivity and specificity ##
sensspec<- melt(data_cutpoint[,c("cutpoint","sens_t","spec_t")], id="cutpoint")
plot_ss<- ggplot(sensspec, aes(x=cutpoint, y=value, color=variable))+
  geom_line(aes(y = value), size=1.2)+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))+
  scale_x_continuous(breaks=1:12)+
  coord_cartesian(ylim=c(-0.1,1.1))+
  theme(legend.key= element_blank(), legend.background= element_rect(color="black"))
plot_ss

## PPP and NPP ##
pppnpp<- melt(data_cutpoint[,c("cutpoint","PPP_t","NPP_t")], id="cutpoint")
plot_pp<- ggplot(pppnpp, aes(x=cutpoint, y=value, color=variable))+
  geom_line(aes(y = value), size=1.2)+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))+
  scale_x_continuous(breaks=1:12)+
  coord_cartesian(ylim=c(-0.1,1.1))+
  theme(legend.key= element_blank(), legend.background= element_rect(color="black"))
plot_pp


###################
# export graphics #
###################

tiff("Histogram of Total Dichotomous Score.tiff", width = 4, height = 4, units = 'in', res = 300)
par(mfrow=c(1,1))
hist(data_raw$Total_count, xlab="Total", main="Histogram of Total Dichotomous Score", breaks=as.integer(N), xlim=c(0,N))
dev.off()

tiff("True Positive Item Discremination.tiff", width = 6, height = 4, units = 'in', res = 300)
plot_TP5
dev.off()

tiff("True Negative Item Discremination.tiff", width = 6, height = 4, units = 'in', res = 300)
plot_TN10
dev.off()

tiff("Sencitivity and Specificity.tiff", width = 6, height = 4, units = 'in', res = 300)
plot_ss
dev.off()

tiff("PPP and NPP.tiff", width = 6, height = 4, units = 'in', res = 300)
plot_pp
dev.off()


