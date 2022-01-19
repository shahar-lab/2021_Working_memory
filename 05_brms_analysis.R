# Bayes modeling ----------------------------------------------------------
rm(list = ls())
load(
  'myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_cards_analysis.Rdata'
)
load(
  'myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_cards.Rdata'
)
load(
  'myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_squares_combined.Rdata'
)
load(
  'myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_squares.Rdata'
)
load(
  'myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_demographic.Rdata'
)
load(
  'myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_capacity.Rdata'
)
load(
  'myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_capacity_comb.Rdata'
)

library(tidylog)
library(effects)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(brms) #for Bayesian (multilevel) generalised linear modelling
library(tidybayes) #for analysis of posterior draws of a Bayesian model
library(magrittr)
library(ggeffects)
library(bayestestR)
library(bayesplot)


# Bayes model for reward ------------------------------------

#priors

prior_weak=c(set_prior(
  prior = "normal(0,0.2)",
  class = "b",
  coef = "Intercept"
),
set_prior(
  prior = "normal(0,0.4)", 
  class = "b",
  coef = "reward_oneback1"
))
###
prior_medium=c(set_prior(
  prior = "normal(0,0.2)",
  class = "b",
  coef = "Intercept"
),
set_prior(
  prior = "normal(0,0.2)", 
  class = "b",
  coef = "reward_oneback1"
))
###
prior_strong=c(set_prior(
  prior = "normal(0,0.2)",
  class = "b",
  coef = "Intercept"
),
set_prior(
  prior = "normal(0,0.09)", 
  class = "b",
  coef = "reward_oneback1"
))
###
prior_rw_set_size_strong=c(set_prior(
  prior = "normal(0,0.2)",
  class = "b",
  coef = "Intercept"
),
set_prior(
  prior = "normal(0,0.2)", 
  class = "b",
  coef = "reward_oneback1"
),
set_prior(
  prior = "normal(0,0.2)",
  class = "b",
  coef = "set_sizeMedium"
),
set_prior(
  prior = "normal(0,0.2)",
  class = "b",
  coef = "set_sizeHigh"
),
set_prior(
  prior = "normal(0,0.09)",
  class = "b",
  coef = "reward_oneback1:set_sizeMedium"
),
set_prior(
  prior = "normal(0,0.09)",
  class = "b",
  coef = "reward_oneback1:set_sizeHigh"
))
###
prior_rw_set_size_medium=c(set_prior(
  prior = "normal(0,0.2)", 
  class = "b",
  coef = "Intercept"
),
  set_prior(
  prior = "normal(0,0.2)",
  class = "b",
  coef = "reward_oneback1"
),
set_prior(
  prior = "normal(0,0.2)",
  class = "b",
  coef = "set_sizeMedium"
),
set_prior(
  prior = "normal(0,0.2)",
  class = "b",
  coef = "set_sizeHigh"
),
set_prior(
  prior = "normal(0,0.2)",
  class = "b",
  coef = "reward_oneback1:set_sizeMedium"
),
set_prior(
  prior = "normal(0,0.2)",
  class = "b",
  coef = "reward_oneback1:set_sizeHigh"
))
###
prior_rw_centered_capacity_medium=c(set_prior(
  prior = "normal(0,0.2)",
  class = "b",
  coef = "Intercept"
),
set_prior(
  prior = "normal(0,0.2)", 
  class = "b",
  coef = "reward_oneback1"
),
set_prior(
  prior = "normal(0,0.2)",
  class = "b",
  coef = "centered_avg_capacity"
),
set_prior(
  prior = "normal(0,0.09)",
  class = "b",
  coef = "reward_oneback1:centered_avg_capacity"
))
###
prior_rw_set_size_capacity_medium=c(set_prior(
  prior = "normal(0,0.2)",
  class = "b",
  coef = "Intercept"
),
set_prior(
  prior = "normal(0,0.2)", 
  class = "b",
  coef = "reward_oneback1"
),
set_prior(
  prior = "normal(0,0.2)",
  class = "b",
  coef = "centered_avg_capacity"
),
set_prior(
  prior = "normal(0,0.2)",
  class = "b",
  coef = "set_sizeMedium"
),
set_prior(
  prior = "normal(0,0.2)",
  class = "b",
  coef = "set_sizeHigh"
),
set_prior(
  prior = "normal(0,0.09)",
  class = "b",
  coef = "reward_oneback1:centered_avg_capacity"
),
set_prior(
  prior = "normal(0,0.09)",
  class = "b",
  coef = "reward_oneback1:set_sizeMedium"
),
set_prior(
  prior = "normal(0,0.09)",
  class = "b",
  coef = "reward_oneback1:set_sizeHigh"
),
set_prior(
  prior = "normal(0,0.09)",
  class = "b",
  coef = "reward_oneback1:centered_avg_capacity:set_sizeMedium"
),
set_prior(
  prior = "normal(0,0.09)",
  class = "b",
  coef = "reward_oneback1:centered_avg_capacity:set_sizeHigh"
))

#prior for unch_card
prior_medium_unch=c(set_prior(
  prior = "normal(0,0.2)",
  class = "b",
  coef = "Intercept"
),
set_prior(
  prior = "normal(0,0.2)", 
  class = "b",
  coef = "rw_when_unch_last_appeared"
))

prior_ch_better=c(set_prior(
  prior = "normal(0,0.2)",
  class = "b",
  coef = "Intercept"
))

#weak prior
bayes_rw_weak <-
  brm(
    formula = stay_key ~ 0 + Intercept + reward_oneback + (1 + reward_oneback * set_size |
                                                             subj),
    data = cards_analysis,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    inits = "0",
    cores = 8,
    seed = 123,
    prior=prior_weak
  )

#medium prior
bayes_rw_medium <-
  brm(
    formula = stay_key ~ 0 + Intercept + reward_oneback + (1 + reward_oneback * set_size|
                                                 subj),
    data = cards_analysis,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    inits = "0",
    cores = 4,
    seed = 123,
    prior=prior_medium
    )

#strong prior
bayes_rw_strong <-
  brm(
    formula = stay_key ~ 0+Intercept + reward_oneback + (1 + reward_oneback * set_size |
                                                 subj),
    data = cards_analysis,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    inits = "0",
    cores = 4,
    seed = 123,
    prior=prior_strong
  )

#uniform prior
bayes_rw_uniform <-
  brm(
    formula = stay_key ~ 0+Intercept + reward_oneback + (1 + reward_oneback * set_size|
                                                 subj),
    data = cards_analysis,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    inits = "0",
    cores = 4,
    seed = 123,
  )

#save models

save(bayes_rw_weak, file = 'myfolder/02_models/model_rw_weak.Rdata')
save(bayes_rw_medium, file = 'myfolder/02_models/model_rw_medium.Rdata')
save(bayes_rw_strong, file = 'myfolder/02_models/model_rw_strong.Rdata')
save(bayes_rw_uniform, file = 'myfolder/02_models/model_rw_uniform.Rdata')
save(bayes_rw_medium_prior, file = 'myfolder/02_models/model_rw_weak_prior.Rdata')


# Bayes model for set size and reward -----------------

bayes_rw_set_size <-
  brm(
    formula = stay_key ~ 0 + Intercept + reward_oneback * set_size + (1 + reward_oneback *
                                                            set_size |                                                 subj),
    data = cards_analysis,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    inits = "0",
    cores = 4,
    seed = 123,
    prior=prior_rw_set_size_medium
  )

save(bayes_rw_set_size, file = 'myfolder/02_models/model_rw_ss.Rdata')

# Bayes model for avg_capacity and reward ---------------------------------


bayes_rw_capacity <-
  brm(
    formula = stay_key ~ 0 + Intercept + reward_oneback * avg_capacity + (1 + reward_oneback * set_size |
                                                                subj),
    data = cards_analysis,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    inits = "0",
    cores = 4,
    seed = 123,
    prior = prior_rw_capacity_medium
  )

save(bayes_rw_capacity, file = 'myfolder/02_models/model_rw_cap_weakly_informed_priors.Rdata')


# Bayes model for centered_avg_capacity and reward ---------------------------------


bayes_rw_centered_capacity <-
  brm(
    formula = stay_key ~ 0 + Intercept + reward_oneback * centered_avg_capacity + (1 + reward_oneback * set_size |
                                                                            subj),
    data = cards_analysis,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    inits = "0",
    cores = 4,
    seed = 123,
    prior = prior_rw_centered_capacity_medium
  )

save(bayes_rw_centered_capacity, file = 'myfolder/02_models/model_rw_cent_cap.Rdata')


# #Bayes model for set-size ,centered_avg_capacity and reward ---------------------------

bayes_rw_capacity_set_size <-
  brm(
    formula = stay_key ~ 0 + Intercept + reward_oneback * centered_avg_capacity * set_size + (1 + 
                                                                           reward_oneback * set_size |
                                                                           subj),
    data = cards_analysis,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    inits = "0",
    cores = 4,
    seed = 123,
    prior = prior_rw_set_size_capacity_medium

  )
save(bayes_rw_capacity_set_size, file = 'myfolder/02_models/model_rw_cent_cap_ss.Rdata')

#model for prior predictive check sampling only from prior
bayes_rw_medium_prior <-
  brm(
    formula = stay_key ~ 0+Intercept+ reward_oneback + (1 + reward_oneback * set_size |
                                                          subj),
    data = cards_analysis,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 2,
    inits = "0",
    cores = 4,
    seed = 123,
    prior=prior_medium,
    sample_prior = "only"
  )
save(bayes_rw_medium_prior, file = 'myfolder/02_models/model_rw_medium_prior.Rdata')

bayes_rw_set_size_medium_prior <-
  brm(
    formula = stay_key ~ 0+Intercept+ reward_oneback*set_size + (1 + reward_oneback * set_size |
                                                                   subj),
    data = cards_analysis,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    inits = "0",
    cores = 4,
    seed = 123,
    prior=prior_rw_set_size_medium,
    sample_prior = "only"
  )
save(bayes_rw_set_size_medium_prior, file = 'myfolder/02_models/model_rw_set_size_medium_prior.Rdata')


bayes_rw_set_size_strong_prior <-
  brm(
    formula = stay_key ~ 0+Intercept+ reward_oneback*set_size + (1 + reward_oneback * set_size |
                                                          subj),
    data = cards_analysis,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    inits = "0",
    cores = 4,
    seed = 123,
    prior=prior_rw_set_size_strong,
    sample_prior = "only"
  )
save(bayes_rw_set_size_strong_prior, file = 'myfolder/02_models/model_rw_set_size_strong_prior.Rdata')

# unchosen_card -----------------------------------------------------------

bayes_unch_rw <-
  brm(
    formula=stay_unch_card~0+Intercept+rw_when_unch_last_appeared+(1+rw_when_unch_last_appeared|subj),
    data = cards,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    inits = "0",
    cores = 4,
    seed = 123,
    prior = prior_medium_unch
  )
save(bayes_unch_rw, file = 'myfolder/02_models/model_rw_unch.Rdata')


# chose_better ----------------------------------------------------------------
bayes_ch_better <-
  brm(
    formula=chose_better~0+Intercept+(1|subj),
    data = cards,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    inits = "0",
    cores = 4,
    seed = 123,
    prior = prior_ch_better
  )
save(bayes_ch_better, file = 'myfolder/02_models/brms_agnostic_model/model_ch_better.Rdata')

#guessing_squares
bayes_guess_square_0 <-
  brm(
    formula=acc~0+Intercept+(1|subj),
    data = memory%>%filter(set_size==0),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    inits = "0",
    cores = 4,
    seed = 123,
  )
save(bayes_guess_square_0, file = 'myfolder/02_models/brms_agnostic_model/model_guess_square_0.Rdata')

bayes_guess_square_1 <-
  brm(
    formula=acc~0+Intercept+(1|subj),
    data = memory%>%filter(set_size==1),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    inits = "0",
    cores = 4,
    seed = 123,
  )
save(bayes_guess_square_1, file = 'myfolder/02_models/brms_agnostic_model/model_guess_square_1.Rdata')

bayes_guess_square_4 <-
  brm(
    formula=acc~0+Intercept+(1|subj),
    data = memory%>%filter(set_size==4),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    inits = "0",
    cores = 4,
    seed = 123,
  )
save(bayes_guess_square_4, file = 'myfolder/02_models/brms_agnostic_model/model_guess_square_4.Rdata')
