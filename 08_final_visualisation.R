rm(list = ls())
library(ggplot2)
library(bayestestR)
library(bayesplot)
library(ggpubr)
library(tidyverse)
library(gridExtra)
library(cowplot)
library(brms)
library(scales)
library(jstable)
library(lme4)

load(
  'myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_cards_analysis.Rdata'
)
load(
  'myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_demographic.Rdata'
)
load('myfolder/02_models/brms_agnostic_model/model_rw_medium.Rdata')
load('myfolder/02_models/brms_agnostic_model/model_rw_ss.Rdata')
load('myfolder/02_models/brms_agnostic_model/model_rw_cent_cap.Rdata')
load('myfolder/02_models/brms_agnostic_model/model_rw_cent_cap_ss.Rdata')


# Figure 3 ----------------------------------------------------------------

#plot descriptive main effect - A
con_rw = conditional_effects(bayes_rw_medium)
plotA = plot(con_rw, plot = FALSE)[[1]] + ggtitle("(A)") + xlab("First offer outcome") +
  ylab("Stay key") + theme_cowplot()+
  scale_x_discrete(labels=c("Unrewarded","Rewarded"))


#plot posterior for main effect - B
posteriors_rw = insight::get_parameters(bayes_rw_medium)
ci_hdi <- ci(posteriors_rw$b_reward_oneback1, method = "HDI")
prior = distribution_normal(4000, mean = 0, sd = 0.2)
plotB = ggplot(posteriors_rw, aes(x = b_reward_oneback1)) +
  theme_classic() +
  geom_density(fill = "orange") +
  ggtitle("(B)") +
  theme(plot.title = element_text(size = 14)) +
  geom_segment(aes(
    x = median(b_reward_oneback1) ,
    y = 0,
    xend = median(b_reward_oneback1),
    yend = 11.59
  ),
  color = "red",
  size = 1) +
  geom_segment(
    aes(
      x = ci_hdi$CI_low ,
      y = 0.8,
      xend = ci_hdi$CI_high,
      yend = 0.8
    ),
    color = "royalblue",
    size = 2
  ) +
  geom_segment(
    aes(
      x = ci_hdi$CI_low ,
      y = 0.5,
      xend = ci_hdi$CI_low,
      yend = 1.1
    ),
    color = "royalblue",
    size = 2
  ) +
  geom_segment(
    aes(
      x = ci_hdi$CI_high ,
      y = 0.5,
      xend = ci_hdi$CI_high,
      yend = 1.1
    ),
    color = "royalblue",
    size = 2
  ) +
  xlab(label = expression(paste(beta," Reward on first offer"))) +
  ylab(label = "Density") + theme_cowplot()
  

#plot descriptive capacity effect - C
demographic=demographic%>%mutate(diff_parameter=coef(bayes_rw_medium,pars="reward_oneback1")$subj[1:169])
plotC = demographic %>% ggplot(aes(x = avg_capacity, y = diff_parameter)) + ylab('Key-response learning') +
  xlab('Working memory capacity') +
  theme_cowplot() + geom_point() + geom_smooth(method = "lm") + ggtitle("(A)")+
  annotation_custom("r=-0.32")

#plot posterior for capacity effect - D
posteriors_rw_cap = insight::get_parameters(bayes_rw_centered_capacity)
ci_hdi_cap <-
  ci(posteriors_rw_cap$`b_reward_oneback1:centered_avg_capacity`,
     method = "HDI")
plotD = ggplot(posteriors_rw_cap,
               aes(x = `b_reward_oneback1:centered_avg_capacity`)) +
  theme_cowplot() +
  geom_density(fill = "orange") +
  ggtitle("(B)") +
  theme(plot.title = element_text(size = 14)) +
  geom_segment(aes(
    x = median(`b_reward_oneback1:centered_avg_capacity`) ,
    y = 0,
    xend = median(`b_reward_oneback1:centered_avg_capacity`),
    yend = 10.4
  ),
  color = "red",
  size = 1) +
  geom_segment(
    aes(
      x = ci_hdi_cap$CI_low ,
      y = 0.7,
      xend = ci_hdi_cap$CI_high,
      yend = 0.7
    ),
    color = "royalblue",
    size = 2
  ) +
  geom_segment(
    aes(
      x = ci_hdi_cap$CI_low ,
      y = 0.4,
      xend = ci_hdi_cap$CI_low,
      yend = 1
    ),
    color = "royalblue",
    size = 2
  ) +
  geom_segment(
    aes(
      x = ci_hdi_cap$CI_high ,
      y = 0.4,
      xend = ci_hdi_cap$CI_high,
      yend = 1
    ),
    color = "royalblue",
    size = 2
  ) +
  xlab(label = expression(paste(beta," Reward X Working memory capacity"))) +
  ylab(label = "Density") + theme_cowplot()

grid.arrange(plotA, plotB, plotC, plotD)


# SI - FIGURES -------------------------------------------------------------

#plot posterior for set_size interaction
posteriors_rw_ss = insight::get_parameters(bayes_rw_set_size)
ci_hdi_ss <-
  ci(posteriors_rw_ss$`b_reward_oneback1:set_sizeHigh`, method = "HDI")
plot1 = ggplot(posteriors_rw_ss, aes(x = `b_reward_oneback1:set_sizeHigh`)) +
  theme_classic() +
  geom_density(fill = "orange") +
  ggtitle("(B)") +
  theme(plot.title = element_text(size = 14)) +
  geom_segment(aes(
    x = median(`b_reward_oneback1:set_sizeHigh`) ,
    y = 0,
    xend = median(`b_reward_oneback1:set_sizeHigh`),
    yend = 9.01
  ),
  color = "red",
  size = 1) +
  geom_segment(
    aes(
      x = ci_hdi_ss$CI_low ,
      y = 0.7,
      xend = ci_hdi_ss$CI_high,
      yend = 0.7
    ),
    color = "royalblue",
    size = 2
  ) +
  geom_segment(
    aes(
      x = ci_hdi_ss$CI_low ,
      y = 0.4,
      xend = ci_hdi_ss$CI_low,
      yend = 1
    ),
    color = "royalblue",
    size = 2
  ) +
  geom_segment(
    aes(
      x = ci_hdi_ss$CI_high ,
      y = 0.4,
      xend = ci_hdi_ss$CI_high,
      yend = 1
    ),
    color = "royalblue",
    size = 2
  ) +
  xlab(label = expression(paste(beta," Reward X Working memory load"))) +
  ylab(label = "Density") + theme_cowplot()
#plot descriptive set_size effect
con_ss <-
  conditional_effects(bayes_rw_set_size, effects = "reward_oneback:set_size")
con_ss <- con_ss$`reward_oneback:set_size`
plot2=ggplot(aes(x = set_size, y = estimate__, colour = reward_oneback), data = con_ss) +
  geom_point(position = position_dodge(width = 0.4),
             alpha = 0.4) +
  geom_pointrange(aes(ymax = upper__, ymin = lower__),
                  position = position_dodge(width = 0.4)) +
  scale_y_continuous(name = "Stay key", limits = c(0.41, 0.5)) +
  ggtitle("(A)") + xlab("Working memory load") + ylab("Stay key") + theme_cowplot()+
  labs(color='Reward first offer')
