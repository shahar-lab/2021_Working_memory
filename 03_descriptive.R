rm(list=ls())
library(tidylog)
library(ppcor)
library("car")
library(effects)
library(tidyverse) # for data manipulation and plots
library(ggplot2)
library(dplyr)
library(modelr) #for data manipulation
library(tidybayes) #for analysis of posterior draws of a Bayesian model
library(magrittr)
library(ggpubr)

load('myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_cards_analysis.Rdata')
load('myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_cards.Rdata')
load('myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_squares_combined.Rdata')
load('myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_squares.Rdata')
load('myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_demographic.Rdata')
load('myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_capacity.Rdata')
load('myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_capacity_comb.Rdata')
# looking at the data -----------------------------------------------------

hist(capacity$avg_capacity)
hist(capacity$set_size_8)
hist(capacity$set_size_4)
hist(capacity_comb$set_size_comb_4)
hist(capacity_comb$set_size_comb_1)
hist(capacity_comb$set_size_comb_0)

#intercept
mean(cards$stay_key)

#reward main effect
cards%>%filter(subtrial==2)%>%group_by(reward_oneback)%>%summarise(mean(stay_key))

#stay effect by set size
descriptive_set_size=cards%>%filter(subtrial==2)%>%group_by(set_size,reward_oneback)%>%summarise(stay=mean(stay_key))
View(descriptive_set_size)

#stay effect by WM capacity
descriptive_capacity=cards%>%filter(subtrial==2)%>%group_by(capacity_high_low,reward_oneback)%>%summarise(stay=mean(stay_key))
View(descriptive_capacity)

#stay effect by WM capacity and set size
descriptive_capacity_set_size=cards%>%filter(subtrial==2)%>%group_by(set_size,capacity_high_low,reward_oneback)%>%summarise(stay=mean(stay_key))
View(descriptive_capacity_set_size)
descriptive_capacity_set_size%>%ggplot(aes(x=reward_oneback,y=stay,color=set_size,shape=capacity_high_low))+geom_point()+geom_smooth(method='lm')


#Change detection and ICAR
demographic%>%ggplot(aes(x=avg_capacity,y=icar))+geom_point()+geom_smooth(method="lm")+
  xlab('Average Capacity')+ylab('ICAR')+
  theme_cowplot()
cor(demographic$icar,demographic$avg_capacity)
cor.test(demographic$icar,demographic$avg_capacity)

#Change detection and OCIR
demographic%>%ggplot(aes(x=avg_capacity,y=ocir))+geom_point()+geom_smooth(method="lm")+
  xlab('Average Capacity')+ylab('OCI-R')+
  theme_cowplot()
cor(demographic$ocir,demographic$avg_capacity)
cor.test(demographic$ocir,demographic$avg_capacity)

#OCI-R and ICAR
demographic%>%ggplot(aes(x=icar,y=ocir))+geom_point()+geom_smooth(method="lm")
cor(demographic$icar,demographic$ocir)
#OCI-R and WM controlling for ICAR
pcor.test(demographic$ocir,demographic$icar,demographic$avg_capacity)
#OCI-R and ICAR controlling for WM
pcor.test(demographic$ocir,demographic$icar,demographic$avg_capacity)
#OCI-R and diff
demographic%>%ggplot(aes(x=ocir,y=diff))+geom_point()+geom_smooth()+
  ylab('Key-response learning')+xlab('OCI-R')+
  theme_cowplot()
cor(demographic$diff,demographic$ocir)
cor.test(demographic$diff,demographic$ocir)
#No correlation when taking capacity into account
pcor.test(demographic$diff,demographic$ocir,demographic$avg_capacity)

#test re-test reliability of diff
cor(demographic$diff1,demographic$diff2)
demographic%>%ggplot(aes(x=diff1,y=diff2))+geom_point()+geom_smooth(method="lm")
#ICAR and diff
demographic%>%ggplot(aes(x=icar,y=diff))+geom_point()+geom_smooth(method="lm")+
ylab('Key-response learning')+xlab('ICAR')+
  theme_cowplot()
cor(demographic$diff,demographic$icar)
cor.test(demographic$diff,demographic$icar)
pcor.test(demographic$diff,demographic$icar,demographic$avg_capacity)
#WM capacity and diff
p_capacity_diff=demographic%>%ggplot(aes(x=avg_capacity,y=diff))+ylab('Key-response learning')+xlab('WM Capacity')+
  theme_cowplot()+geom_point()+geom_smooth(method="lm")
save(p_capacity_diff, file = 'figures/p_capacity_diff.Rdata')
cor(demographic$diff,demographic$avg_capacity)
cor.test(demographic$diff,demographic$avg_capacity)
pcor.test(demographic$diff,demographic$avg_capacity,demographic$icar)

#Chose better by capacity
cards%>%group_by(capacity_high_low)%>%summarise(better=mean(chose_better))

#Chose better by set_size
cards%>%group_by(set_size)%>%summarise(better=mean(chose_better))

#visualize stay effect by WM capacity and set size
visualize_capacity_set_size=cards%>%
  filter(subtrial==2)%>%
  group_by(set_size,avg_capacity,reward_oneback)%>%
  summarise(stay=mean(stay_key))%>%
  pivot_wider(names_from='reward_oneback',values_from='stay')%>%
  mutate(diff=`1`-`0`)%>%dplyr::select(-c(`0`,`1`))
View(visualize_capacity_set_size)
visualize_capacity_set_size%>%ggplot(aes(x=avg_capacity,y=diff,color=set_size))+geom_point()+geom_smooth()

# card learning by set size
descriptive_card_set_size = cards%>%group_by(set_size)%>%summarise(mean(chose_better))
View(descriptive_card_set_size)
cards%>%group_by(session,trial)%>%summarise(better=mean(chose_better))%>%ggplot(aes(x=trial,y=better,color=session))+geom_point()+geom_smooth(method="lm")

#stay effect by WM capacity and set size and correctness
descriptive_set_size_correct=cards%>%filter(subtrial==2)%>%group_by(square_correct,reward_oneback)%>%summarise(stay=mean(stay_key))
View(descriptive_set_size_correct)

visualize_correct_capacity_set_size=cards%>%
  filter(subtrial==2)%>%
  group_by(set_size,avg_capacity,reward_oneback,square_correct)%>%
  summarise(stay=mean(stay_key))%>%
  pivot_wider(names_from='reward_oneback',values_from='stay')%>%
  mutate(diff=`1`-`0`,square_correct=as.factor(square_correct))%>%
  dplyr::select(-c(`0`,`1`))
View(visualize_correct_capacity_set_size)
visualize_correct_capacity_set_size%>%ggplot(aes(x=avg_capacity,y=diff,color=square_correct))+geom_point()+geom_smooth()

#Is location really irrelevant?
demographic%>%ggplot(aes(x=real_diff))+geom_histogram()
mean(demographic$real_diff)
cor(demographic$real_diff,demographic$diff)
cor(demographic$real_diff1,demographic$diff1)
cor(demographic$real_diff2,demographic$diff2)

#correlation of reward and diff
demographic %<>%mutate(cards%>%group_by(subj)%>%summarise(reward=sum(reward))%>%select(reward))
cor(demographic$diff,demographic$reward)
cor.test(demographic$diff,demographic$reward)
#stay effect by RT bins
cards_analysis%>%
  group_by(reward_oneback,bin_rt)%>%
  summarise(stay=mean(stay_key))%>%
  pivot_wider(names_from=reward_oneback,values_from=stay)%>%
  mutate(diff=`1`-`0`)%>%
  select(-c(`0`,`1`))

#effect of key-response reward on squares stay_key
View(cards%>%filter(subtrial==2)%>%
  group_by(reward,capacity_high_low)%>%
    summarise(mean(stay_key_sqr)))

View(cards%>%filter(subtrial==2)%>%
       group_by(reward,square_correct)%>%
       summarise(mean(stay_key_sqr)))

View(cards%>%filter(subtrial==2)%>%
       group_by(reward,square_correct,set_size)%>%
       summarise(mean(stay_key_sqr)))

View(cards%>%filter(subtrial==2)%>%
       group_by(reward,is_ch_key_correct_sqr)%>%
       summarise(mean(stay_key_sqr)))

View(cards%>%
       group_by(reward,is_ch_key_correct_sqr)%>%
       summarise(mean(stay_key_sqr)))

View(cards%>%filter(subtrial==2)%>%
       group_by(set_size,is_ch_key_correct_sqr,reward)%>%
       summarise(mean(stay_key_sqr)))

View(cards%>%filter(subtrial==2)%>%
       group_by(capacity_high_low,is_ch_key_correct_sqr,reward)%>%
       summarise(mean(stay_key_sqr)))
#show with accuracy
View(cards%>%filter(subtrial==2)%>%
       group_by(is_ch_key_correct_sqr,reward)%>%
       summarise(mean(square_correct)))

View(cards%>%filter(subtrial==2)%>%
       group_by(set_size,is_ch_key_correct_sqr,reward)%>%
       summarise(mean(square_correct)))

View(cards%>%filter(subtrial==2)%>%
       group_by(capacity_high_low,is_ch_key_correct_sqr,reward)%>%
       summarise(mean(square_correct)))

#correlation of inter-task to ocir
View(cards%>%filter(subtrial==2)%>%
       group_by(is_ch_key_correct_sqr,reward,subj)%>%
       summarise(sqr_correct=mean(square_correct)))

#correctness by reward
View(cards%>%filter(subtrial==1)%>%
       group_by(reward)%>%
       summarise(mean(square_correct)))

View(cards%>%filter(subtrial==2)%>%
       group_by(reward)%>%
       summarise(mean(square_correct)))

#learning_for_cards
cards%>%filter(reoffer_pair)%>%group_by(rw_when_ch_last_appeared)%>%summarise(mean(stay_ch_card))%>%View()

#learning for squares
memory%>%group_by(set_size)%>%summarise(mean(acc))
