
# logistic regression modeling --------------------------------------------

rm(list=ls())
library(lme4)
library(tidylog)
library(lmerTest)
library(purrr)
library(ppcor)
library("car")
library(effects)
library(tidyverse) # for data manipulation and plots
library(ggplot2)
library(dplyr)
library(cowplot)
library(sjstats) #for calculating intra-class correlation (ICC)
library(ROCR) #for calculating area under the curve (AUC) statistics
library(brms) #for Bayesian (multilevel) generalised linear modelling
library(modelr) #for data manipulation
library(tidybayes) #for analysis of posterior draws of a Bayesian model
library(magrittr)
library(ggeffects)
library(sjmisc)
library(splines)
library(ggpubr)

load('myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_cards_analysis.Rdata')
load('myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_cards.Rdata')
load('myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_squares_combined.Rdata')
load('myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_squares.Rdata')
load('myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_demographic.Rdata')
load('myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_capacity.Rdata')
load('myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_capacity_comb.Rdata')


#model for reward
m1=glmer(formula=stay_key~1+reward_oneback+(1+reward_oneback*set_size|subj),family = binomial(link = "logit"),nAGQ=0,data=cards_analysis)
summary(m1)
save(m1, file = 'myfolder/02_models/glmer/rw_glmer.Rdata')
#model for reward and set size
m2=glmer(formula=stay_key~1+reward_oneback*set_size+(1+reward_oneback*set_size|subj),family = binomial(link = "logit"),nAGQ=0,data=cards_analysis)
summary(m2)
Anova(m2)
save(m2, file = 'myfolder/02_models/glmer/rw_ss_glmer.Rdata')
#model for reward and sex
m3=glmer(formula=stay_key~1+reward_oneback*sex+(1+reward_oneback|subj),family = binomial(link = "logit"),nAGQ=0,data=cards_analysis)
summary(m3)
save(m3, file = 'myfolder/02_models/glmer/rw_sex_glmer.Rdata')
#model for reward and capacity_high_low
m4=glmer(formula=stay_key~1+reward_oneback+reward_oneback:capacity_high_low+(1+reward_oneback|subj),family = binomial(link = "logit"),nAGQ=0,data=cards_analysis)
summary(m4)
Anova(m4)
save(m4, file = 'myfolder/02_models/glmer/rw_capacity_glmer.Rdata')
#model for reward, capacity_high_low and set_size
m5=glmer(formula=stay_key~1+reward_oneback+reward_oneback:set_size+reward_oneback:avg_capacity+reward_oneback:set_size+reward_oneback:avg_capacity:set_size+(1+reward_oneback*set_size|subj),family = binomial(link = "logit"),nAGQ=0,data=cards_analysis)
summary(m5)
Anova(m5)
save(m5, file = 'myfolder/02_models/glmer/rw_capacityHighLow_ss_glmer.Rdata')
#model for reward, and icar
m6=glmer(formula=stay_key~1+reward_oneback*icar+(1+reward_oneback*set_size|subj)+(1+set_size|subj),family = binomial(link = "logit"),nAGQ=0,data=cards_analysis)
summary(m6)
save(m6, file = 'myfolder/02_models/glmer/rw_icar_glmer.Rdata')
#model for reward, set_size and icar
m7=glmer(formula=stay_key~1+reward_oneback*icar*set_size+(1+reward_oneback|subj),family = binomial(link = "logit"),nAGQ=0,data=cards_analysis)
summary(m7)
save(m7, file = 'myfolder/02_models/glmer/rw_icar_ss_glmer.Rdata')
#model for reward, and ocir
m8=glmer(formula=stay_key~1+reward_oneback*ocir+(1+reward_oneback*set_size|subj),family = binomial(link = "logit"),nAGQ=0,data=cards_analysis)
summary(m8)
save(m8, file = 'myfolder/02_models/glmer/rw_ocir_glmer.Rdata')
#model for reward, and avg_capacity
m9=glmer(formula=stay_key~1+reward_oneback*avg_capacity+(1+reward_oneback*set_size|subj),family = binomial(link = "logit"),nAGQ=0,data=cards_analysis)
summary(m9)
save(m9, file = 'myfolder/02_models/glmer/rw_avgcapacity_glmer.Rdata')
#model for reward, and centered_avg_capacity
m10=glmer(formula=stay_key~1+reward_oneback+reward_oneback*centered_avg_capacity+(1+reward_oneback|subj),family = binomial(link = "logit"),nAGQ=0,data=cards_analysis)
summary(m10)
save(m10, file = 'myfolder/02_models/glmer/rw_centeredAvgCapacity_glmer.Rdata')
#model for reward, set_size, centered_avg_capacity 
m11=glmer(formula=stay_key~1+reward_oneback+reward_oneback*set_size*centered_avg_capacity+(1+reward_oneback*set_size|subj),family = binomial(link = "logit"),nAGQ=0,data=cards_analysis)
Anova(m11)
save(m11, file = 'myfolder/02_models/glmer/rw_centered_avg_capacity_ss_glmer.Rdata')
#model for reward, set_size, centered_avg_capacity and correctness
m12=glmer(formula=stay_key~1+reward_oneback*set_size*centered_avg_capacity*correct_response_sqr+(1+reward_oneback*set_size|subj),family = binomial(link = "logit"),nAGQ=0,data=cards%>%filter(subtrial==2,square_correct==0))
Anova(m12)
save(m12, file = 'myfolder/02_models/glmer/rw_centered_avg_capacity_ss_correctSqr_glmer.Rdata')
#model for reward,icar and capacity_high_low
m13=glmer(formula=stay_key~1+reward_oneback*capacity_high_low*icar+(1+reward_oneback|subj),family = binomial(link = "logit"),nAGQ=0,data=cards_analysis)
summary(m13)
Anova(m13)
save(m13, file = 'myfolder/02_models/glmer/rw_capacity_ss_glmer.Rdata')
#model for reward in second trial and stay key for squares
m14=glmer(formula=stay_key_sqr~1+reward+(1+reward|subj),family=binomial(link="logit"),nAGQ=0,data=cards_analysis)
summary(m14)
Anova(m14)
save(m14, file = 'myfolder/02_models/glmer/stayKeySqr_rw_glmer.Rdata')
#model for reward in second trial and stay key for squares with set size
m15=glmer(formula=stay_key_sqr~1+reward*set_size+(1+reward|subj),family=binomial(link="logit"),nAGQ=0,data=cards_analysis)
summary(m15)
Anova(m15)
save(m15, file = 'myfolder/02_models/glmer/stayKeySqr_rw_ss_glmer.Rdata')
#model for reward in second trial and stay key for squares with capacity
m16=glmer(formula=stay_key_sqr~1+reward*capacity_high_low+(1+reward|subj),family=binomial(link="logit"),nAGQ=0,data=cards_analysis)
summary(m16)
Anova(m16)
save(m16, file = 'myfolder/02_models/glmer/stayKeySqr_rw_capacity_glmer.Rdata')
#model for reward in second trial and stay key for squares with square_correct
m17=glmer(formula=stay_key_sqr~1+reward*square_correct+(1+reward|subj),family=binomial(link="logit"),nAGQ=0,data=cards_analysis)
summary(m17)
Anova(m17)
save(m17, file = 'myfolder/02_models/glmer/stayKeySqr_rw_sqrCorrect_glmer.Rdata')
#model for reward and ch_key in the second trial is the correct response as predicting square_correct
m18=glmer(formula=square_correct~1+reward*is_ch_key_correct_sqr+(1+reward|subj),family=binomial(link="logit"),nAGQ=0,data=cards_analysis)
summary(m18)
Anova(m18)
save(m18, file = 'myfolder/02_models/glmer/sqrCorrect_reward_chKeyCorrect_glmer.Rdata')


#model for chose_better by capacity and set_size
m19=glmer(formula=chose_better~1+avg_capacity*set_size+(1+avg_capacity*set_size|subj),family=binomial(link="logit"),nAGQ=0,data=cards)
summary(m19)
Anova(m19)
save(m19, file = 'myfolder/02_models/glmer/chose_better_wm_glmer.Rdata')

#model for stay with unch_card by reward
m20=glmer(formula=stay_unch_card~1+reward_oneback+(1+reward_oneback|subj),family=binomial(link="logit"),nAGQ=0,data=cards_unch)
summary(m20)
save(m20, file = 'myfolder/02_models/glmer/unch_card_rw.Rdata')


#model for stay with unch_card by reward
m20=glmer(formula=stay_unch_card~1+reward_oneback+(1+reward_oneback|subj),family=binomial(link="logit"),nAGQ=0,data=cards_unch)
summary(m20)
save(m20, file = 'myfolder/02_models/glmer/unch_card_rw.Rdata')

#model for relevant_learning
m21=glmer(formula=stay_ch_card~1+rw_when_ch_last_appeared+(1+rw_when_ch_last_appeared*set_size|subj),family=binomial(link="logit"),nAGQ=0,data=cards%>%filter(reoffer_pair))
summary(m21)
save(m21, file = 'myfolder/02_models/glmer/relevant_learning.Rdata')

#model for relevant_learning and set_size
m22=glmer(formula=stay_ch_card~1+rw_when_ch_last_appeared*set_size+(1+rw_when_ch_last_appeared*set_size|subj),family=binomial(link="logit"),nAGQ=0,data=cards%>%filter(reoffer_pair))
summary(m22)
save(m22, file = 'myfolder/02_models/glmer/relevant_learning_ss.Rdata')

#model for relevant_learning and set_size and capacity
m23=glmer(formula=stay_ch_card~1+rw_when_ch_last_appeared*set_size*centered_avg_capacity+(1+rw_when_ch_last_appeared*set_size|subj),family=binomial(link="logit"),nAGQ=0,data=cards%>%filter(reoffer_pair))
summary(m23)
save(m23, file = 'myfolder/02_models/glmer/relevant_learning_ss_cap.Rdata')

#model for irrelevant_learning between trials
m24=glmer(formula=stay_key~1+reward_oneback*set_size*centered_avg_capacity+(1+reward_oneback*set_size|subj),family=binomial(link="logit"),nAGQ=0,data=cards%>%filter(repeat_pair_one_back))
summary(m24)
save(m24, file = 'myfolder/02_models/glmer/irrelevant_learning_trials_ss.Rdata')