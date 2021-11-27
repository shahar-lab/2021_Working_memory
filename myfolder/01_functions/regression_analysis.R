library(sjstats) #for calculating intra-class correlation (ICC)
library(ROCR) #for calculating area under the curve (AUC) statistics
library(brms) #for Bayesian (multilevel) generalised linear modelling
library(modelr) #for data manipulation
library(tidybayes) #for analysis of posterior draws of a Bayesian model


#Bayes model for set size and reward
Bayes_Model_Binary <- brm(formula = stay_key_1_2~reward1*sex*avg_capacity+(1+reward1|subj),  
                          data=data_cards, 
                          family = bernoulli(link = "logit"),
                          warmup = 500, 
                          iter = 2000, 
                          chains = 2, 
                          inits= "0", 
                          cores=2,
                          seed = 123)
summary(Bayes_Model_Binary)
marginal_effects(Bayes_Model_Binary)
stanplot(Bayes_Model_Binary, pars = "^b", type = "areas")
#Bayesian model with sex, and set_size
Bayes_Model_Binary1 <- brm(formula = stay_key_1_2~reward1*sex*set_size+(1+reward1|subj),  
                           data=data_cards, 
                           family = bernoulli(link = "logit"),
                           warmup = 500, 
                           iter = 2000, 
                           chains = 2, 
                           inits= "0", 
                           cores=2,
                           seed = 123)
summary(Bayes_Model_Binary1)
marginal_effects(Bayes_Model_Binary1)
stanplot(Bayes_Model_Binary1, 
         type = "areas",
         prob = 0.95)
stanplot(Bayes_Model_Binary1, pars = "^b", type = "areas")
#triple model
Bayes_Model_Binary2 <- brm(formula = stay_key_1_2~reward1*avg_capacity*set_size+(1+reward1|subj),  
                           data=data_cards, 
                           family = bernoulli(link = "logit"),
                           warmup = 500, 
                           iter = 2000, 
                           chains = 2, 
                           inits= "0", 
                           cores=2,
                           seed = 123)
Bayes_Model_Binary3 <- brm(formula = stay_key_1_2~reward1*sex+(1+reward1|subj),  
                           data=data_cards, 
                           family = bernoulli(link = "logit"),
                           warmup = 500, 
                           iter = 2000, 
                           chains = 2, 
                           inits= "0", 
                           cores=2,
                           seed = 123)
summary(Bayes_Model_Binary3)
Bayes_Model_Binary4 <- brm(formula=pick~1+reward_prev*set_size*set_size_prev*round+(1+reward_prev|subj),  
                           data=df10, 
                           family = bernoulli(link = "logit"),
                           warmup = 500, 
                           iter = 2000, 
                           chains = 2, 
                           inits= "0", 
                           cores=2,
                           seed = 123)
summary(Bayes_Model_Binary4)
Bayes_Model_Binary5 <- brm(formula=pick~1+reward_prev*set_size*set_size_prev*round+(1|subj),  
                           data=df10, 
                           family = bernoulli(link = "logit"),
                           warmup = 500, 
                           iter = 2000, 
                           chains = 2, 
                           inits= "0", 
                           cores=2,
                           seed = 123)
summary(Bayes_Model_Binary5)
add_criterion(Bayes_Model_Binary4,"waic")
add_criterion(Bayes_Model_Binary5,"waic")
loo_compare(Bayes_Model_Binary4, Bayes_Model_Binary5, criterion = "bayes_R2")
save(Bayes_Model_Binary,file='myfolder/02_models/model1.Rdata')
save(Bayes_Model_Binary1,file='myfolder/02_models/model2.Rdata')
save(Bayes_Model_Binary2,file='myfolder/02_models/model3.Rdata')
save(Bayes_Model_Binary3,file='myfolder/02_models/model4.Rdata')
save(Bayes_Model_Binary4,file='myfolder/02_models/model5.Rdata')
save(Bayes_Model_Binary5,file='myfolder/02_models/model6.Rdata')
