rm(list=ls())
# effect of reward on unchosen --------------------------------------------
library(lme4)
library(tidylog)
library(lmerTest)
library(purrr)
library("car")
library(effects)
library(tidyverse) # for data manipulation and plots
library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyverse)
library(sjstats) #for calculating intra-class correlation (ICC)
library(ROCR) #for calculating area under the curve (AUC) statistics
library(brms) #for Bayesian (multilevel) generalised linear modelling
library(modelr) #for data manipulation
library(tidybayes) #for analysis of posterior draws of a Bayesian model

load('myfolder/03_data/02_aggregated_data/exp1_01.2021/data_for_analysis/data_cards.Rdata')
load('myfolder/03_data/02_aggregated_data/exp1_01.2021/data_for_analysis/data_squares_combined.Rdata')
load('myfolder/03_data/02_aggregated_data/exp1_01.2021/data_for_analysis/data_squares.Rdata')


# set_size on pstay_key ---------------------------------------------------

#looking at the data
cards%>%filter(subtrial==2)%>%group_by(reward_oneback)%>%summarise(stay=mean(stay_key))
cards%>%filter(subtrial==2)%>%group_by(set_size,reward_oneback)%>%summarise(stay=mean(stay_key))
cards%>%filter(subtrial==2)%>%group_by(capacity_high_low,reward_oneback)%>%summarise(stay=mean(stay_key))

#model for reward and set size
m1=glmer(formula=stay_key~1+reward_oneback*set_size+(1+reward_oneback|subj),family = binomial(link = "logit"),nAGQ=0,data=cards%>%filter(subtrial==2))
summary(m1)

#Bayes model for set size and reward
Bayes_Model_Binary <- brm(formula=stay_key~1+reward_oneback*set_size+(1+reward_oneback|subj),  
                          data=cards%>%filter(subtrial==2), 
                          family = bernoulli(link = "logit"),
                          warmup = 500, 
                         iter = 2000, 
                          chains = 2, 
                          inits= "0", 
                         cores=4,
                         seed = 123)
#summary(Bayes_Model_Binary)
#marginal_effects(Bayes_Model_Binary)
#stanplot(Bayes_Model_Binary, pars = "^b", type = "areas")
plot(Bayes_Model_Binary)
# capacity ----------------------------------------------------------------
#looking at the data
descriptive=cards%>%filter(subtrial==2)%>%group_by(capacity_high_low,reward_oneback)%>%summarise(stay=mean(stay_key))

#model for reward and capacity
m2=glmer(formula=stay_key~1+reward_oneback*avg_capacity+(1+reward_oneback|subj),family = binomial(link = "logit"),nAGQ=0,data=cards%>%filter(subtrial==2))
summary(m2)
m2_prolific=glmer(formula=stay_key~1+reward_oneback*avg_capacity+(1+reward_oneback|subj),family = binomial(link = "logit"),nAGQ=0,data=cards_prolific%>%filter(subtrial==2))
summary(m2_prolific)
#Bayes model for set size and reward
Bayes_Model_Binary1 <- brm(formula=stay_key~1+reward_oneback*capacity_high_low+(1+reward_oneback|subj),  
                          data=cards%>%filter(subtrial==2), 
                          family = bernoulli(link = "logit"),
                          warmup = 500, 
                          iter = 2000, 
                          chains = 2, 
                          inits= "0", 
                          cores=4,
                          seed = 123)
summary(Bayes_Model_Binary1)
#marginal_effects(Bayes_Model_Binary1)
#stanplot(Bayes_Model_Binary1, pars = "^b", type = "areas")


# binned_rt ---------------------------------------------------------------
#looking at the data
descriptive_binned_rt=cards%>%filter(subtrial==2)%>%group_by(bin_rt,reward_oneback)%>%summarise(stay=mean(stay_key))

#model for reward, set_size and rt_binned
colnames(cards)[c(13,18,24)]=c("Reward_previous_trial","Prob_stay_key_response","RT_binned")
m3=glmer(formula=Prob_stay_key_response~1+Reward_previous_trial*RT_binned*avg_capacity+(1+Reward_previous_trial|subj),family = binomial(link = "logit"),nAGQ=0,data=cards%>%filter(subtrial==2))
summary(m3)
effect("reward_oneback:bin_rt",m3)
plot(effect("Reward_previous_trial:RT_binned",m3,xlevels=4),cex.axis=15)
m3_prev=glmer(formula=stay_key~1+reward_oneback*prev_bin_rt*avg_capacity+(1+reward_oneback|subj),family = binomial(link = "logit"),nAGQ=0,data=cards%>%filter(subtrial==2))
summary(m3_prev)
#Bayes model for set size and reward
Bayes_Model_Binary2 <- brm(formula=stay_key~1+reward_oneback*bin_rt*avg_capacity+(1+reward_oneback|subj),  
                           data=cards%>%filter(subtrial==2), 
                           family = bernoulli(link = "logit"),
                           warmup = 500, 
                           iter = 2000, 
                           chains = 2, 
                           inits= "0", 
                           cores=20,
                           seed = 123)
summary(Bayes_Model_Binary2)
save(Bayes_Model_Binary2,file='myfolder/02_models/model_rw_bin_cap.Rdata')
marginal_effects(Bayes_Model_Binary2)

#stanplot(Bayes_Model_Binary2, pars = "^b", type = "areas")

# unchosen ----------------------------------------------------------------

#looking at the data
#chosen
descriptive_ch=cards%>%group_by(set_size,set_size_when_ch_last_appeared,rw_when_ch_last_appeared)%>%summarise(stay=mean(stay_ch_card))
#unchosen
descriptive_unch=cards%>%group_by(set_size,set_size_when_unch_last_appeared,rw_when_unch_last_appeared)%>%summarise(stay=mean(stay_unch_card))

#models for reward and set_sizes
m4=glmer(formula=stay_ch_card~1+rw_when_ch_last_appeared*set_size*set_size_when_ch_last_appeared+(1+rw_when_ch_last_appeared|subj),family = binomial(link = "logit"),nAGQ=0,data=cards)
summary(m4)
m5=glmer(formula=stay_unch_card~1+rw_when_unch_last_appeared*set_size*set_size_when_unch_last_appeared+(1+rw_when_unch_last_appeared|subj),family = binomial(link = "logit"),nAGQ=0,data=cards)
summary(m5)

#Bayes model for set sizes and unchosen
Bayes_Model_Binary3 <- brm(formula=stay_unch_card~1+rw_when_unch_last_appeared*set_size*set_size_when_unch_last_appeared+(1+rw_when_unch_last_appeared|subj),  
                           data=cards, 
                           family = bernoulli(link = "logit"),
                           warmup = 500, 
                           iter = 2000, 
                           chains = 2, 
                           inits= "0", 
                           cores=2,
                           seed = 123)
summary(Bayes_Model_Binary3)
#marginal_effects(Bayes_Model_Binary1)
#stanplot(Bayes_Model_Binary1, pars = "^b", type = "areas")

save(Bayes_Model_Binary,file='myfolder/02_models/model_rw_set.Rdata')
save(Bayes_Model_Binary1,file='myfolder/02_models/model_rw_avg_cap.Rdata')
load('myfolder/02_models/model_rw_set.Rdata')

# Visualization -----------------------------------------------------------

#preparing the dfs
df_stay_key_set_size=cards%>%group_by(subj,set_size,reward_oneback)%>%summarise(stay=mean(stay_key))
df_stay_key_capacity=cards%>%group_by(subj,capacity_high_low,reward_oneback)%>%summarise(mean(stay_key))
df_stay_unch_set_size=cards%>%group_by(set_size,rw_when_unch_last_appeared)%>%summarise(mean(stay_unch_card))
df_stay_key_capacity_binned=cards%>%group_by(subj,capacity_high_low,bin_rt,reward_oneback)%>%summarise(stay=mean(stay_key))
#change to factor scale
df_stay_key_capacity_binned <- df_stay_key_capacity_binned %>%
  mutate(capacity_high_low = factor(capacity_high_low),
         capacity_high_low = if_else(capacity_high_low == 0, "Low", "High"),
         capacity_high_low = factor(capacity_high_low, levels = c("Low", "High")),
         reward_oneback = factor(reward_oneback),
         reward_oneback=if_else(reward_oneback==0, "0", "1"),
         reward_oneback= factor(reward_oneback, levels= c("0","1")),
        bin_rt=factor(bin_rt)
         )
df_stay_key_set_size <- df_stay_key_set_size %>%
  mutate(set_size = factor(set_size),
         set_size = if_else(set_size == 1, "Low", "High"),
         set_size = factor(set_size, levels = c("Low", "High")),
         reward_oneback = factor(reward_oneback),
         reward_oneback=if_else(reward_oneback==0, "0", "1"),
         reward_oneback= factor(reward_oneback, levels= c("0","1")),
  )
colnames(df_stay_key_set_size)[3]="Reward_previous_trial"
#Rainclouds with boxplots

#reward_prev and set_size
p1 <- ggplot(df_stay_key_set_size,aes(x=set_size,y=stay,fill=Reward_previous_trial,color=Reward_previous_trial))+
  geom_boxplot(aes(x = set_size, y = stay), alpha = 0.3, width = 0.5, colour = "BLACK") +
  ylab('Prob_stay_key_response')+xlab('WM load')+theme_cowplot()+
  scale_fill_manual(values=c("#FF0000", "#00FF00"))+ 
  scale_color_manual(values=c("#FF0000", "#00FF00"))+
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.1))+
  theme_bw()+
  theme(legend.position="bottom")+
  theme(text = element_text(size = 25))+
  theme(legend.key.size = unit(3,"line"))
p1
p2 <- ggplot(df_stay_key_capacity_binned,aes(x=bin_rt,y=stay, fill = reward_oneback, colour = reward_oneback))+
  geom_boxplot(aes(x = bin_rt, y = stay),outlier.shape = NA, alpha = 0.3, width = .3, colour = "BLACK") +
  ylab('P(stay_key_response)')+xlab('RT bin')+theme_cowplot()+ facet_wrap(~capacity_high_low)+
  scale_fill_manual(values=c("#FF0000", "#00FF00"))+ 
  scale_color_manual(values=c("#FF0000", "#00FF00"))+
  ggtitle("Figure 2: Reward by WM capacity group")+ 
  scale_x_discrete(breaks=c("1","2"),
  labels=c("Fast","Slow"))+
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.5,1))+
 theme_bw()
p2


