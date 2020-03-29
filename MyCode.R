install.packages("tydyverse")
install.packages("randomForest")
install.packages("corrplot")
install.packages("dplyr")
install.packages("Metrics")
install.packages("caret")
install.packages("plyr")
install.packages("ggplot2")
install.packages("psych")
install.packages("ggpubr")
install.packages("reshape2")

library(ggpubr)
library(psych)
library(ggplot2)
library(plyr)
library(tidyverse)
library(caret)
library(Metrics)
library(dplyr)
library(corrplot)
library(randomForest)
library(reshape2)


#Merge
train_docket <- read_csv('raw_data/train_dockets.csv')
train_docket <- train_docket %>%
  mutate(statute = as.factor(statute),
         protected_class = as.factor(protected_class),
         jury_demand = as.factor(jury_demand),
         jurisdiction = as.factor(jurisdiction),
         diversity_residence = as.factor(jurisdiction),
         pro_se = as.factor(pro_se),
         informa_pauperis = as.factor(informa_pauperis),
         arbitration_at_filing = as.factor(arbitration_at_filing),
         outcome = as.factor(outcome)
  )
str(train_docket)
train_om <- read_csv('raw_data/train_other_motions.csv')
train_om <- train_om %>%
  mutate(motion_type = as.factor(motion_type),
         filing_party = as.factor(filing_party))
str(train_om)
train_tm <- read_csv('raw_data/train_terminating_motions.csv')
train_tm <- train_tm %>%
  mutate(motion_type = as.factor(motion_type),
         filing_party = as.factor(filing_party))
str(train_tm)
#some entries in train_om does not exist in train_om
anti_join(train_tm, train_om, by = c('mudac_id' = 'mudac_id'))
motions <- full_join(train_om, train_tm, by = c('mudac_id' = 'mudac_id'))
names(motions)
names(motions)[2:4] <- c("motion_type.om", "filing_party.om", "filed_before_joined.om" )
names(motions)[7:11] <- c("proceeding_precentile.om", "motion_type.tm","filing_party.tm", "filed_before_joined.tm", "proceeding_precentile.t,")
names(motions)[5] <- 'decision'
train <- left_join(motions, train_docket, by = c('mudac_id' = 'mudac_id'))

#venues motion to dismiss

train1<-train%>%
  filter(as.character(motion_type.tm)=="Motion to Dismiss")%>%
  group_by(district)%>%
  summarise(countDisMiss=n())

train2<-train%>%
  group_by(district)%>%
  summarise(countAll=n())

train_motion_dismiss <-full_join(train1, train2, by = c('district' = 'district'))
train_motion_dismiss$circuit<-NULL
train_motion_dismiss$circuit[1:9]<-6
train_motion_dismiss$circuit[10:16]<-7
train_motion_dismiss$circuit[17:26]<-8
train_motion_dismiss$circuit<-as.factor(train_motion_dismiss$circuit)
train_motion_dismiss<-train_motion_dismiss[-27,]

train_motion_dismiss$ratio<-train_motion_dismiss$countDisMiss/train_motion_dismiss$countAll

train_motion_dismiss%>%
  group_by(circuit)%>%
  summarise(mean=mean(ratio),variance=var(ratio),counts=n())



ggplot(train_motion_dismiss,aes(x=circuit,y=ratio,fill=circuit))+
  geom_bar(stat="summary",fun.y="mean")

ggplot(train_motion_dismiss,aes(x=district,y=ratio,fill=circuit))+
  geom_bar(stat = 'identity')

ggplot(train_motion_dismiss,aes(x=reorder(district,ratio),y=ratio,fill=circuit))+
  geom_bar(stat = 'identity')

#venues motion to summary judgement

train3<-train%>%
  filter(as.character(motion_type.tm)=="Motion for Summary Judgment")%>%
  group_by(district)%>%
  summarise(countSM=n())

train4<-train%>%
  group_by(district)%>%
  summarise(countAll=n())

train_motion_sm <-full_join(train3, train4, by = c('district' = 'district'))
train_motion_sm$circuit<-NULL
train_motion_sm$circuit[1:9]<-6
train_motion_sm$circuit[10:16]<-7
train_motion_sm$circuit[17:26]<-8
train_motion_sm$circuit<-as.factor(train_motion_sm$circuit)
train_motion_sm<-train_motion_sm[-27,]

train_motion_sm$ratio<-train_motion_sm$countSM/train_motion_sm$countAll

train_motion_sm%>%
  group_by(circuit)%>%
  summarise(mean=mean(ratio),variance=var(ratio),counts=n())


ggplot(train_motion_sm,aes(x=circuit,y=ratio,fill=circuit))+
  geom_bar(stat="summary",fun.y="mean")

ggplot(train_motion_sm,aes(x=district,y=ratio,fill=circuit))+
  geom_bar(stat = 'identity')

ggplot(train_motion_sm,aes(x=reorder(district,ratio),y=ratio,fill=circuit))+
  geom_bar(stat = 'identity')
  
 
#favor plaintiff/defendant

  # Only tm
summary(train_tm$filing_party)


train5<-train%>%
  filter(as.character(filing_party.tm)=="Plaintiff")%>%
  group_by(district)%>%
  summarise(countAll=n())

train6<-train%>%
  filter(as.character(filing_party.tm)=="Defendant")%>%
  group_by(district)%>%
  summarise(countAll=n())

train5$circuit[1:9]<-6
train5$circuit[10:16]<-7
train5$circuit[17:26]<-8
train5$circuit<-as.factor(train5$circuit)
train5<-train5[-27,]

train6$circuit[1:9]<-6
train6$circuit[10:16]<-7
train6$circuit[17:26]<-8
train6$circuit<-as.factor(train6$circuit)
train6<-train6[-27,]

ggplot()+geom_bar(stat="identity",data=train6,aes(x=reorder(district,countAll),y=countAll),fill="orange")+
  geom_bar(stat="identity",data=train5,aes(x=reorder(district,countAll),y=countAll),fill="blue")


df1 <- data.frame(train5$district, train5$countAll, train6$countAll)
names(df1)<-c("district","Plaintiff","Defendant")
df2 <- reshape2::melt(df1, id.vars='district')
head(df2)

ggplot(df2, aes(x=reorder(district,value), y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')








ggplot()+geom_bar(stat="identity",data=train6,aes(x=district,y=countAll,fill=circuit))

train6$dif=(train6$countAll-train5$countAll)/(train6$countAll+train5$countAll)
ggplot()+geom_bar(stat="identity",data=train6,aes(x=district,y=dif,fill=circuit))
ggplot()+geom_bar(stat="identity",data=train6,aes(x=reorder(district,dif),y=dif,fill=circuit))



train6%>%
  group_by(circuit)%>%
  summarise(mean=mean(dif),variance=var(dif),counts=n())





  #only om

summary(train_om$filing_party)


train7<-train%>%
  filter(as.character(filing_party.om)=="Plaintiff")%>%
  group_by(district)%>%
  summarise(countAll=n())

train8<-train%>%
  filter(as.character(filing_party.om)=="Defendant")%>%
  group_by(district)%>%
  summarise(countAll=n())

train7$circuit[1:9]<-6
train7$circuit[10:16]<-7
train7$circuit[17:26]<-8
train7$circuit<-as.factor(train7$circuit)
train7<-train7[-27,]

train8$circuit[1:9]<-6
train8$circuit[10:16]<-7
train8$circuit[17:26]<-8
train8$circuit<-as.factor(train8$circuit)
train8<-train8[-27,]



df3 <- data.frame(train7$district, train7$countAll, train8$countAll)
names(df3)<-c("district","Plaintiff","Defendant")
df4 <- reshape2::melt(df3, id.vars='district')
head(df4)

ggplot(df4, aes(x=reorder(district,value), y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')



train8$dif=(train8$countAll-train7$countAll)/(train8$countAll+train7$countAll)
ggplot()+geom_bar(stat="identity",data=train8,aes(x=district,y=dif,fill=circuit))
ggplot()+geom_bar(stat="identity",data=train8,aes(x=reorder(district,dif),y=dif,fill=circuit))
train8%>%
  group_by(circuit)%>%
  summarise(mean=mean(dif),variance=var(dif),counts=n())


#both tm and om

train9<-train5[,c("district","circuit","countAll")]
train9$countAll<-train9$countAll+train7$countAll
train10<-train6[,c("district","circuit","countAll")]
train10$countAll<-train10$countAll+train8$countAll


df5 <- data.frame(train9$district, train9$countAll, train10$countAll)
names(df5)<-c("district","Plaintiff","Defendant")
df6 <- reshape2::melt(df5, id.vars='district')
head(df6)

ggplot(df6, aes(x=reorder(district,value), y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')



train10$dif=(train10$countAll-train9$countAll)/(train10$countAll+train9$countAll)
train10 %>%
  group_by(circuit)%>%
  summarise(mean=mean(dif),variance=var(dif),counts=n())

ggplot()+geom_bar(stat="identity",data=train10,aes(x=district,y=dif,fill=circuit))
ggplot()+geom_bar(stat="identity",data=train10,aes(x=reorder(district,dif),y=dif,fill=circuit))


#Check

length(which(as.character(train$motion_type.tm)==
               "Motion for Summary Judgment"&as.character(train$outcome)=="Summary Judgment"))

length(intersect(which(as.character(train$motion_type.tm)==
               "Motion for Summary Judgment"&as.character(train$outcome)=="Summary Judgment"),
       which(as.character(train$decision)=="Granted")))


length(intersect(which(as.character(train$motion_type.tm)==
                         "Motion for Summary Judgment"&as.character(train$outcome)=="Summary Judgment"),
                 which(as.character(train$decision)=="Granted in Part")))


#Add NA's in decision
length(which(is.na(train$decision)))

length(intersect(which(as.character(train$motion_type.tm)==
                         "Motion for Summary Judgment"&as.character(train$outcome)=="Summary Judgment"),
                 which(is.na(train$decision))))


View(train[c("9","10","11"),c("motion_type.tm","outcome","decision")])

  #Note: No use


#Motion for summary judgment


train$MoSumGet<-0
train$MoSumGet[which(as.character(train$motion_type.tm)=="Motion for Summary Judgment"&
                       train$summary_judgment==1)]<-1

ggplot(train[train$MoSumGet==1,],aes(x=district,fill=district))+
  geom_bar()


train11<-train%>%
  filter(train$MoSumGet==1)%>%
  group_by(district)%>%
  summarise(countSM=n())

train12<-train%>%
  filter(as.character(train$motion_type.tm)=="Motion for Summary Judgment")%>%
  group_by(district)%>%
  summarise(countAll=n())

train_mo_sm <-full_join(train11, train12, by = c('district' = 'district'))
train_mo_sm$circuit<-NULL
train_mo_sm$circuit[1:9]<-6
train_mo_sm$circuit[10:16]<-7
train_mo_sm$circuit[17:26]<-8
train_mo_sm$circuit<-as.factor(train_mo_sm$circuit)
train_mo_sm<-train_mo_sm[-27,]

train_mo_sm$ratio<-train_mo_sm$countSM/train_mo_sm$countAll

train_mo_sm%>%
  group_by(circuit)%>%
  summarise(mean=mean(ratio),variance=var(ratio),counts=n())


ggplot(train_mo_sm,aes(x=circuit,y=ratio,fill=circuit))+
  geom_bar(stat="summary",fun.y="mean")

ggplot(train_mo_sm,aes(x=district,y=ratio,fill=circuit))+
  geom_bar(stat = 'identity')

ggplot(train_mo_sm,aes(x=reorder(district,ratio),y=ratio,fill=circuit))+
  geom_bar(stat = 'identity')


df7 <- data.frame(train_mo_sm$district, train_mo_sm$countSM, train_mo_sm$countAll)
names(df7)<-c("district","SumJudClosed","SumJudclosedMotion")
df8 <- reshape2::melt(df7, id.vars='district')
head(df8)

ggplot(df8, aes(x=district, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')

ggplot(df8, aes(x=reorder(district,value), y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')


#Motion to dismiss
length(which(as.character(train$outcome)=="Dismissed"&
        as.character(train$motion_type.tm)=="Motion to Dismiss"))

length(which(as.character(train$outcome)!="Dismissed"&
               as.character(train$motion_type.tm)=="Motion to Dismiss"))


train$MoSumDis<-0
train$MoSumDis[which(as.character(train$motion_type.tm)=="Motion to Dismiss"&
                       as.character(train$outcome)=="Dismissed")]<-1

ggplot(train[which(train$MoSumDis==1),],aes(x=district,fill=district))+
  geom_bar()


train13<-train%>%
  filter(train$MoSumDis==1)%>%
  group_by(district)%>%
  summarise(countSM=n())

train14<-train%>%
  filter(as.character(train$motion_type.tm)=="Motion to Dismiss")%>%
  group_by(district)%>%
  summarise(countAll=n())

train_mo_sd <-full_join(train13, train14, by = c('district' = 'district'))
train_mo_sd$circuit<-NULL
train_mo_sd$circuit[1:9]<-6
train_mo_sd$circuit[10:16]<-7
train_mo_sd$circuit[17:26]<-8
train_mo_sd$circuit<-as.factor(train_mo_sd$circuit)
train_mo_sd<-train_mo_sd[-27,]

train_mo_sd$ratio<-train_mo_sd$countSM/train_mo_sd$countAll

train_mo_sd%>%
  group_by(circuit)%>%
  summarise(mean=mean(ratio),variance=var(ratio),counts=n())


ggplot(train_mo_sd,aes(x=circuit,y=ratio,fill=circuit))+
  geom_bar(stat="summary",fun.y="mean")

ggplot(train_mo_sd,aes(x=district,y=ratio,fill=circuit))+
  geom_bar(stat = 'identity')

ggplot(train_mo_sd,aes(x=reorder(district,ratio),y=ratio,fill=circuit))+
  geom_bar(stat = 'identity')


df9 <- data.frame(train_mo_sd$district, train_mo_sd$countSM, train_mo_sd$countAll)
names(df9)<-c("district","SumJudClosed","SumJudclosedMotion")
df10 <- reshape2::melt(df9, id.vars='district')
head(df10)

ggplot(df10, aes(x=district, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')

ggplot(df10, aes(x=reorder(district,value), y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')



#Favor the plaintiff/defendant 

ggplot(train,aes(x=outcome,fill=motion_type.tm))+
  geom_bar()+facet_wrap(~motion_type.om)+
  theme(axis.text.x=element_text(angle=45,hjust=1))


ggplot(train,aes(x=outcome,fill=summary_judgment))+
  geom_bar()+facet_wrap(~motion_type.tm)+
  theme(axis.text.x=element_text(angle=45,hjust=1))

ggplot(train,aes(x=outcome,fill=filing_party.om))+
  geom_bar()+facet_wrap(~motion_type.tm)+
  theme(axis.text.x=element_text(angle=45,hjust=1))


ggplot(train[which(train$decision %in% c("Granted","Granted in Part")),],aes(x=outcome,fill=filing_party.om))+
  geom_bar()+facet_wrap(~motion_type.om)+
  theme(axis.text.x=element_text(angle=45,hjust=1))


ggplot(train[which(train$decision %in% c("Denied","Denied as Moot")),],aes(x=outcome,fill=filing_party.om))+
  geom_bar()+facet_wrap(~motion_type.om)+
  theme(axis.text.x=element_text(angle=45,hjust=1))


ggplot(train,aes(x=outcome,fill=filing_party.om))+
  geom_bar()+facet_wrap(~motion_type.om)+
  theme(axis.text.x=element_text(angle=45,hjust=1))


ggplot(train,aes(x=outcome,fill=decision))+
  geom_bar()+facet_wrap(~motion_type.om)+
  theme(axis.text.x=element_text(angle=45,hjust=1))

ggplot(train,aes(x=decision,fill=filed_before_joined.tm))+geom_bar()+facet_wrap(~outcome)+
  theme(axis.text.x=element_text(angle=45,hjust=1))


#Naive method for checking the favor




