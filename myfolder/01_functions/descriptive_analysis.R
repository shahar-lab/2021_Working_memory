library(lme4)
library(tidylog)
library(lmerTest)
library(purrr)
library("car")
library(tidyverse) # for data manipulation and plots
library(ggplot2)
library(dplyr)
library(plyr)
library(cowplot)
library(tidyverse)

#looking at the data
descriptive=data_cards%>%group_by(reward1)%>%summarise(stay=mean(stay_key_1_2))

df1=data_cards%>%group_by(subj,reward1)%>%summarise(stay=mean(stay_key_1_2))
df1%>%ggplot(aes(x=reward1,y=stay))+geom_point()+geom_smooth(method="lm",se=T)
df2=data_cards%>%group_by(subj,reward1,set_size)%>%summarise(stay=mean(stay_key_1_2))
df2%>%ggplot(aes(x=reward1,y=stay,color=set_size))+geom_point()+geom_smooth(method="lm",se=T)                                        
#model for reward and sex
m1=glmer(formula=stay_key_1_2~1+reward1*sex*avg_capacity+(1|subj),family = binomial(link = "logit"),data=cards)
summary(m1)

df3=data_correct%>%group_by(subj,reward1,set_size)%>%summarise(stay=mean(stay_key_1_2))
df3%>%ggplot(aes(x=reward1,y=stay,color=set_size))+geom_point()+geom_smooth(method="lm",se=T)      
#using only correct trials
m2=glmer(formula=stay_key_1_2~1+reward1*set_size+(1+reward1+set_size+reward1*set_size|subj),data=data_correct)
summary(m2)


cor(data_8_4$'set_size_8',data_8_4$'set_size_4')
scat_8_4=data_8_4%>%ggplot(aes(x=set_size_4,y=set_size_8))+geom_point()+geom_smooth(method="lm",se=T)
scat_8_4

cor(data_4_1$'set_size_1',data_4_1$'set_size_4_comb')
scat_4_1=data_4_1%>%ggplot(aes(x=set_size_1,y=set_size_4_comb))+geom_point()+geom_smooth(method="lm",se=T)
scat_4_1
cor(data_4_1$set_size_4_comb,data_8_4$set_size_4)
scat_4_4_comb=squares_all%>%ggplot(aes(x=set_size_4,y=set_size_4_comb))+geom_point()+geom_smooth(method="lm",se=T)
scat_4_4_comb
m_squares=lm(squares_all$set_size_4~1+squares_all$set_size_4_comb)
summary(m_squares)


#a model with capacity
m3=glmer(formula=stay_key_1_2~1+reward1*avg_capacity+(1+reward1|subj),data=data_female)
summary(m3)
df4=data_cards%>%group_by(subj,reward1,avg_capacity)%>%summarise(stay=mean(stay_key_1_2))
df4%>%ggplot(aes(x=reward1,y=stay,color=avg_capacity,group=avg_capacity))+geom_point()+geom_smooth(method="lm",se=F)                                
#get stay data from subj
data_stay_diff=data_cards%>%group_by(subj,reward1)%>%summarise(stay=mean(stay_key_1_2))
data_stay_diff=data_stay_diff%>%pivot_wider(names_from="reward1",values_from="stay")
colnames(data_stay_diff)[2]='stay_reward_0'
colnames(data_stay_diff)[3]='stay_reward_1'
data_stay_diff$stay_diff=data_stay_diff$stay_reward_1-data_stay_diff$stay_reward_0
data_stay_diff$avg_capacity=squares_all$avg_capacity
g5=data_stay_diff%>%ggplot(aes(x=avg_capacity,y=stay_diff))+geom_point()+geom_smooth(method="lm",se=F)
g5
cor(data_stay_diff$stay_diff,data_stay_diff$avg_capacity)
m3=glmer(formula=stay_key_1_2~1+reward1+avg_capacity+reward1*avg_capacity+reward1*avg_capacity*set_size+(1+reward1|subj),data=data_cards)
summary(m3)

#model with triple interaction
m4=lmer(formula=stay_key_1_2~1+reward1*capacity_high_low*set_size+(1+reward1*capacity_high_low*set_size|subj),data=data_cards)
summary(m4)

#get stay data from subj divided by set_size
data_stay_diff_set=data_cards%>%group_by(subj,set_size,reward1)%>%summarise(stay=mean(stay_key_1_2))
wide_data_stay_diff_set=data_stay_diff_set%>%pivot_wider(names_from=c("reward1","set_size"),values_from="stay")
colnames(wide_data_stay_diff_set)[2]="reward_0_low"
colnames(wide_data_stay_diff_set)[3]="reward_1_low"
colnames(wide_data_stay_diff_set)[4]="reward_0_high"
colnames(wide_data_stay_diff_set)[5]="reward_1_high"
wide_data_stay_diff_set$stay_diff_low=wide_data_stay_diff_set$reward_1_low-wide_data_stay_diff_set$reward_0_low
wide_data_stay_diff_set$stay_diff_high=wide_data_stay_diff_set$reward_1_high-wide_data_stay_diff_set$reward_0_high
wide_data_stay_diff_set$avg_capacity=squares_all$avg_capacity
g6=wide_data_stay_diff_set%>%ggplot(aes(x=avg_capacity,y=stay_diff_low))+geom_point()+geom_smooth(method="lm",se=F)+ylim(c(-.55,.55))
g6
g7=wide_data_stay_diff_set%>%ggplot(aes(x=avg_capacity,y=stay_diff_high))+geom_point()+geom_smooth(method="lm",se=F)+ylim(c(-.55,.55))
g7
cor(wide_data_stay_diff_set$avg_capacity,wide_data_stay_diff_set$stay_diff_high)
cor(wide_data_stay_diff_set$avg_capacity,wide_data_stay_diff_set$stay_diff_low)

#the same procedure for correct trials
data_correct_stay_diff_set=data_correct%>%group_by(subj,set_size,reward1)%>%summarise(stay=mean(stay_key_1_2))
wide_data_correct_stay_diff_set=data_correct_stay_diff_set%>%pivot_wider(names_from=c("reward1","set_size"),values_from="stay")
colnames(wide_data_correct_stay_diff_set)[2]="reward_0_low"
colnames(wide_data_correct_stay_diff_set)[3]="reward_1_low"
colnames(wide_data_correct_stay_diff_set)[4]="reward_0_high"
colnames(wide_data_correct_stay_diff_set)[5]="reward_1_high"
wide_data_correct_stay_diff_set$stay_diff_low=wide_data_correct_stay_diff_set$reward_1_low-wide_data_correct_stay_diff_set$reward_0_low
wide_data_correct_stay_diff_set$stay_diff_high=wide_data_correct_stay_diff_set$reward_1_high-wide_data_correct_stay_diff_set$reward_0_high
wide_data_correct_stay_diff_set$avg_capacity=squares_all$avg_capacity
g6=wide_data_correct_stay_diff_set%>%ggplot(aes(x=avg_capacity,y=stay_diff_low))+geom_point()+geom_smooth(method="lm",se=F)+ylim(c(-.55,.55))
g6
g7=wide_data_stay_diff_set%>%ggplot(aes(x=avg_capacity,y=stay_diff_high))+geom_point()+geom_smooth(method="lm",se=F)+ylim(c(-.55,.55))
g7
cor(wide_data_correct_stay_diff_set$avg_capacity,wide_data_correct_stay_diff_set$stay_diff_high)
cor(wide_data_correct_stay_diff_set$avg_capacity,wide_data_correct_stay_diff_set$stay_diff_low)

#predicting better card choice by set-size
data_better_cards=data_cards%>%group_by(subj,set_size)%>%summarise(better=(mean(is_better_card1)+mean(is_better_card2))/2)
wide_data_better_cards=data_better_cards%>%pivot_wider(names_from="set_size",values_from="better")
data_better_cards%>%ggplot(aes(x=set_size,y=better))+geom_boxplot()+ylab('choose card with higher reward prob')
mean(unlist(wide_data_better_cards[,2]))
mean(unlist(wide_data_better_cards[,3]))

#predicting better card choice by capacity
data_better_cards_capacity=data_cards%>%group_by(subj,avg_capacity)%>%summarise(better=(mean(is_better_card1)+mean(is_better_card2))/2)
data_better_cards_capacity%>%ggplot(aes(x=avg_capacity,y=better))+geom_point()+ylab('choose card with higher reward prob')+geom_smooth(method="lm",se=F)
cor(data_better_cards_capacity$avg_capacity,data_better_cards_capacity$better)
data_cards$better_total=(data_cards$is_better_card1+data_cards$is_better_card2)/2
m4=lmer(formula=better_total~1+avg_capacity+(1|subj),data=data_cards)
summary(m4)

#boxplot of rt1_binned and stay_diff
df3=data_cards%>%group_by(subj,capacity_high_low,reward1)%>%summarise(mean(stay_key_1_2))
df3=df3%>%pivot_wider(names_from="reward1",values_from="stay")
df3$stay_diff=df3$`1`-df3$`0`

#Rainclouds with boxplots
p1 <- ggplot(df3,aes(x=reward1,y=stay,fill=reward1,color=reward1))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =.2)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_boxplot(aes(x = as.numeric(reward1)+0.25, y = stay),outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK") +
  ylab('Stay key press')+xlab('Reward')+coord_flip()+theme_cowplot()+guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Figure 1: Effect of reward on the probability to repeat the same key press")
p1
p2 <- ggplot(df3,aes(x=reward1,y=stay, fill = reward1, colour = reward1))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =.2, trim = F)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_boxplot(aes(x = as.numeric(reward1)+0.25, y = stay),outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK") +
  
  ylab('Stay key press')+xlab('Reward')+coord_flip()+theme_cowplot()+guides(fill = FALSE, colour = FALSE) + facet_wrap(~capacity_high_low)+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Figure 2: Reward by WM capacity group")

p2
