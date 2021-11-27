rm(list = ls())
# effect of reward on unchosen --------------------------------------------
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
library(tidyverse)
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
library(see)
library(magrittr)
library(ggeffects)
library(bayestestR)
library(bayesplot)

#load data
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

#load models
load('myfolder/02_models/brms_agnostic_model/model_rw_uniform.Rdata')
load('myfolder/02_models/brms_agnostic_model/model_rw_weak.Rdata')
load('myfolder/02_models/brms_agnostic_model/model_rw_medium.Rdata')
load('myfolder/02_models/brms_agnostic_model/model_rw_strong.Rdata')
load('myfolder/02_models/brms_agnostic_model/model_rw_ss.Rdata')
load('myfolder/02_models/brms_agnostic_model/model_rw_cap_weakly_informed_priors.Rdata')
load('myfolder/02_models/brms_agnostic_model/model_rw_cent_cap.Rdata')
load('myfolder/02_models/brms_agnostic_model/model_rw_cap_ss_weakly_informed_priors.Rdata')
load('myfolder/02_models/brms_agnostic_model/model_rw_medium_prior.Rdata')

# Visualization -----------------------------------------------------------

#preparing the dfs
df_stay_key = cards_analysis %>% group_by(subj, reward_oneback) %>%
  summarise(stay = mean(stay_key))
df_stay_key_set_size = cards_analysis %>% group_by(subj, set_size, reward_oneback) %>%
  summarise(stay = mean(stay_key))
df_stay_key_capacity = cards_analysis %>% group_by(subj, capacity_high_low, reward_oneback) %>%
  summarise(stay = mean(stay_key))
df_stay_key_capacity_set_size = cards_analysis %>% group_by(subj, capacity_high_low, set_size, reward_oneback) %>%
  summarise(stay = mean(stay_key))


colnames(df_stay_key)[2] = "Reward_first_offer"
colnames(df_stay_key_set_size)[3] = "Reward_first_offer"
colnames(df_stay_key_capacity)[3] = "Reward_first_offer"
colnames(df_stay_key_capacity_set_size)[4] = "Reward_first_offer"

#define sd
sd = df_stay_key %>% group_by(Reward_first_offer) %>% summarise(sd =
                                                                     sd(stay))

#plots
demographic%>%ggplot(aes(x=avg_capacity,y=diff))+ylab('Key-response learning')+xlab('WM Capacity')+
  theme_cowplot()+geom_point()+geom_smooth(method="lm")
p1 <-
  ggplot(
    df_stay_key,
    aes(
      x = Reward_first_offer,
      y = stay,
      fill = Reward_first_offer,
      color = Reward_first_offer
    )
  ) +
  geom_jitter(alpha = 0.4, width = 0.1) +
  ylab('Prob stay key response') + xlab('First offer outcome') +
  theme_cowplot() +
  scale_fill_manual(values = c("#FF0000", "#00FF00")) +
  scale_color_manual(values = c("#FF0000", "#00FF00")) +
  scale_y_continuous(limits = c(0.1, 0.8), breaks = seq(0, 1, 0.1)) +
  scale_x_discrete(labels=c("Unrewarded","Rewarded"))+
  stat_summary(
    fun.data = mean_sdl,
    fun.args = list(mult = 1),
    geom = "errorbar",
    color = "black",
    width = 0.2
  ) +
  stat_summary(fun.y = mean,
               geom = "point",
               color = "black") +
  stat_summary(
    aes(group = 1),
    geom = "line",
    fun.y = "mean",
    col = "black",
    size = 1,
  ) +
  annotate(
    "text",
    label = "**",
    x = 2,
    y = 0.8,
    size = 10,
    colour = "black"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 20))
p1

p2 <-
  ggplot(
    df_stay_key_set_size,
    aes(
      x = Reward_first_offer,
      y = stay,
      fill = Reward_first_offer,
      color = Reward_first_offer
    )
  ) +
  geom_jitter(alpha = 0.4, width = 0.1) +
  facet_grid(. ~ set_size, space = "free", scales = "free_x") +
  ylab('Prob_stay_key_response') + xlab('First offer outcome') +
  theme_cowplot() +
  scale_fill_manual(values = c("#FF0000", "#00FF00")) +
  scale_color_manual(values = c("#FF0000", "#00FF00")) +
  scale_y_continuous(limits = c(0.03, 0.9), breaks = seq(0, 1, 0.1)) +
  scale_x_discrete(labels=c("Unrewarded","Rewarded"))+
  stat_summary(
    fun.data = mean_sdl,
    fun.args = list(mult = 1),
    geom = "errorbar",
    color = "black",
    width = 0.2
  ) +
  stat_summary(fun.y = mean,
               geom = "point",
               color = "black") +
  stat_summary(
    aes(group = 1),
    geom = "line",
    fun.y = "mean",
    col = "black",
    size = 1,
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
p2
p3 <-
  ggplot(
    df_stay_key_capacity,
    aes(
      x = Reward_first_offer,
      y = stay,
      fill = Reward_first_offer,
      color = Reward_first_offer
    )
  ) +
  geom_jitter(alpha = 0.4, width = 0.1) +
  facet_grid(. ~ capacity_high_low, space = "free", scales = "free_x") +
  ylab('Prob_stay_key_response') + xlab('First offer outcome') +
  theme_cowplot() +
  scale_fill_manual(values = c("#FF0000", "#00FF00")) +
  scale_color_manual(values = c("#FF0000", "#00FF00")) +
  scale_y_continuous(limits = c(0.04, 0.9), breaks = seq(0, 1, 0.1)) +
  scale_x_discrete(labels=c("Unrewarded","Rewarded"))+
  stat_summary(
    fun.data = mean_sdl,
    fun.args = list(mult = 1),
    geom = "errorbar",
    color = "black",
    width = 0.2
  ) +
  stat_summary(fun.y = mean,
               geom = "point",
               color = "black") +
  stat_summary(
    aes(group = 1),
    geom = "line",
    fun.y = "mean",
    col = "black",
    size = 1,
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
p3

p4 <-
  ggplot(
    df_stay_key_capacity_set_size,
    aes(
      x = Reward_first_offer,
      y = stay,
      fill = Reward_first_offer,
      color = Reward_first_offer
    )
  ) +
  geom_jitter(alpha = 0.4, width = 0.1) +
  facet_grid(capacity_high_low ~ set_size,
             space = "free",
             scales = "free_x") +
  ylab('Prob_stay_key_response') + xlab('Reward_previous_trial') +
  theme_cowplot() +
  scale_fill_manual(values = c("#FF0000", "#00FF00")) +
  scale_color_manual(values = c("#FF0000", "#00FF00")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  stat_summary(
    fun.data = mean_sdl,
    fun.args = list(mult = 1),
    geom = "errorbar",
    color = "black",
    width = 0.2
  ) +
  stat_summary(fun.y = mean,
               geom = "point",
               color = "black") +
  stat_summary(
    aes(group = 1),
    geom = "line",
    fun.y = "mean",
    col = "black",
    size = 1,
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
p4

#Bayesian visualisation

# Plot the distribution and add the limits of the two CIs
posteriors = insight::get_parameters(bayes_rw_medium)
#HDI
ci_hdi <- ci(posteriors$b_reward_oneback1, method = "HDI")
#prior
prior = distribution_normal(4000, mean = 0, sd = 0.2)
#support interval
# si_1 = si(posteriors$b_reward_oneback1, prior, BF = 1)
ggplot(posteriors, aes(x = b_reward_oneback1)) +
  theme_classic()+
  geom_density(fill = "orange") +
  # The median in red
  geom_vline(
    xintercept = median(posteriors$b_reward_oneback1),
    color = "red",
    size = 1
  ) +
  geom_vline(xintercept = ci_hdi$CI_low,
             color = "royalblue",
             size = 3) +
  geom_vline(xintercept = ci_hdi$CI_high,
             color = "royalblue",
             size = 3)+
  xlab(label="Reward on first offer")+
  ylab(label="Density")
ggplot(mapping = aes(x = x, y = y)) +
  theme_classic() +
  # The posterior
  geom_area(fill = "orange",
            data = estimate_density(posteriors$b_reward_oneback1, extend = TRUE)) +
  # # The prior
  geom_area(color = "black", fill = NA, size = 1, linetype = "dashed",
            data = estimate_density(prior, extend = TRUE))+
  xlab(label="Reward on first offer")+
  ylab(label="Density")
  # # BF = 1 SI in blue
  # geom_vline(xintercept = si_1$CI_low, color = "royalblue", size = 1) +
  # geom_vline(xintercept = si_1$CI_high, color = "royalblue", size = 1) +
  # # BF = 3 SI in red
  # geom_vline(xintercept = si_3$CI_low, color = "red", size = 1) +
  # geom_vline(xintercept = si_3$CI_high, color = "red", size = 1)

#get BFs
#uniform_bf=bayesfactor_parameters(bayes_rw_uniform) There is no BF for the uniform prior
#plot(uniform_bf)
weak_bf=bayesfactor_parameters(bayes_rw_weak)
weak_bf
plot(weak_bf)
medium_bf=bayesfactor_parameters(bayes_rw_medium)
medium_bf
plot(medium_bf)
strong_bf=bayesfactor_parameters(bayes_rw_strong)
strong_bf
plot(strong_bf)

ss_bf=bayesfactor_parameters(bayes_rw_set_size)
ss_bf
plot(ss_bf)

capacity_bf=bayesfactor_parameters(bayes_rw_centered_capacity)
capacity_bf
plot(capacity_bf)
full_bf=bayesfactor_parameters(bayes_rw_capacity_set_size)
full_bf
plot(full_bf)

effectsize::interpret_bf(exp(weak_bf$log_BF[1]), include_value = TRUE)
effectsize::interpret_bf(exp(weak_bf$log_BF[2]), include_value = TRUE)
effectsize::interpret_bf(exp(strong_bf$log_BF[1]), include_value = TRUE)
effectsize::interpret_bf(exp(strong_bf$log_BF[2]), include_value = TRUE)
#BFs rope
weak_bf_rope=bayesfactor_parameters(bayes_rw_weak,null=c(-0.0132, 0.0132))
weak_bf_rope
plot(weak_bf_rope)
medium_bf_rope=bayesfactor_parameters(bayes_rw_medium,null=c(-0.0132, 0.0132))
medium_bf_rope
plot(medium_bf_rope)
strong_bf_rope=bayesfactor_parameters(bayes_rw_strong,null=c(-0.0132, 0.0132))
strong_bf_rope
plot(strong_bf_rope)
capacity_bf_rope=bayesfactor_parameters(bayes_rw_centered_capacity,null=c(-0.0132, 0.0132))
capacity_bf_rope
plot(capacity_bf_rope)

ss_bf_rope=bayesfactor_parameters(bayes_rw_set_size,null=c(-0.0132, 0.0132))
ss_bf_rope
plot(ss_bf_rope)

bayes_rw_capacity_set_size_rope=bayesfactor_parameters(bayes_rw_capacity_set_size,null=c(-0.0132, 0.0132))
bayes_rw_capacity_set_size_rope
plot(bayes_rw_capacity_set_size_rope)
effectsize::interpret_bf(exp(weak_bf_rope$log_BF[1]), include_value = TRUE)
effectsize::interpret_bf(exp(weak_bf_rope$log_BF[2]), include_value = TRUE)
effectsize::interpret_bf(exp(medium_bf_rope$log_BF[1]), include_value = TRUE)
effectsize::interpret_bf(exp(medium_bf_rope$log_BF[2]), include_value = TRUE)
effectsize::interpret_bf(exp(strong_bf_rope$log_BF[1]), include_value = TRUE)
effectsize::interpret_bf(exp(strong_bf_rope$log_BF[2]), include_value = TRUE)

#bridge sampling comparison
model_sampler=bayesfactor_models(bayes_rw_medium,bayes_rw_set_size,bayes_rw_centered_capacity,bayes_rw_capacity_set_size,denominator =bayes_rw_medium )

#describe results
describe_posterior_rw_uni=describe_posterior(bayes_rw_uniform, rope_range = c(-0.0132, 0.0132))
describe_posterior_rw_weak=describe_posterior(bayes_rw_weak, rope_range = c(-0.0132, 0.0132))
describe_posterior_rw_medium=describe_posterior(bayes_rw_medium, rope_range = c(-0.0132, 0.0132))
describe_posterior_rw_strong=describe_posterior(bayes_rw_strong, rope_range = c(-0.0132, 0.0132))
describe_posterior_rw_capacity=describe_posterior(bayes_rw_capacity, rope_range = c(-0.0132, 0.0132))
describe_posterior_rw_cent_capacity=describe_posterior(bayes_rw_centered_capacity, rope_range = c(-0.0132, 0.0132))
describe_posterior_rw_ss=describe_posterior(bayes_rw_set_size, rope_range = c(-0.0132, 0.0132))
describe_posterior_rw_capacity_ss=describe_posterior(bayes_rw_capacity_set_size, rope_range = c(-0.0132, 0.0132))

save(describe_posterior_rw_uni, file = 'myfolder/02_models/describe_posterior_rw_uni.Rdata')
save(describe_posterior_rw_weak, file = 'myfolder/02_models/describe_posterior_rw_weak.Rdata')
save(describe_posterior_rw_medium, file = 'myfolder/02_models/describe_posterior_rw_medium.Rdata')
save(describe_posterior_rw_strong, file = 'myfolder/02_models/describe_posterior_rw_strong.Rdata')
save(describe_posterior_rw_capacity, file = 'myfolder/02_models/describe_posterior_rw_capacity.Rdata')
save(describe_posterior_rw_cent_capacity, file = 'myfolder/02_models/describe_posterior_rw_cent_capacity.Rdata')
save(describe_posterior_rw_ss, file = 'myfolder/02_models/describe_posterior_rw_ss.Rdata')
save(describe_posterior_rw_capacity_ss, file = 'myfolder/02_models/describe_posterior_capacity_ss.Rdata')

#prior_predictive_check reward model
y=cards_analysis$stay_key
yrep=posterior_predict(bayes_rw_medium_prior)
group_vec=cards_analysis$reward_oneback==1
ppc_stat_grouped(y, yrep, group_vec)

#prior_predictive_check reward_ss_strong model
y=cards_analysis$stay_key
yrep=posterior_predict(bayes_rw_set_size_strong_prior)
group_vec=cards_analysis%>%mutate(set_rw=case_when(
  set_size=="Low"&reward_oneback==0 ~ 0,
  set_size=="Low"&reward_oneback==1 ~ 1,
  set_size=="Medium"&reward_oneback==0 ~ 2,
  set_size=="Medium"&reward_oneback==1 ~ 3,
  set_size=="High"&reward_oneback==0 ~ 4,
  set_size=="High"&reward_oneback==1 ~ 5
))
group_vec=group_vec$set_rw
ppc_stat_grouped(y, yrep, group_vec)

#prior_predictive_check reward_ss_medium model
yrep_medium=posterior_predict(bayes_rw_set_size_medium_prior)
ppc_stat_grouped(y, yrep_medium, group_vec)

#posterior_predictive_check
y=cards_analysis$stay_key
yrep=posterior_predict(bayes_rw_medium)
group_vec=as.factor(cards_analysis$reward_oneback==1)
ppc_stat_grouped(y, yrep, group_vec)

#conditional_effects
plot(conditional_effects(bayes_rw_medium))[[1]]+ggplot2::xlab("Reward on first offer")+ggplot2::ylab("Probability to repeat key-response")
conditional_effects(bayes_rw_capacity)
conditional_effects(bayes_rw_set_size)
conditional_effects(bayes_rw_capacity_set_size)

summary(Bayes_Model_Binary1)
marginal_effects(Bayes_Model_Binary1)
stanplot(Bayes_Model_Binary1, pars = "^b", type = "areas")
plot(Bayes_Model_Binary1)
plot(Bayes_Model_Binary2)
stanplot(Bayes_Model_Binary2, pars = "^b", type = "areas")