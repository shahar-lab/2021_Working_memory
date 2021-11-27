calc_means_sd=function(df){
  cards_trial_means = df %>% filter(subtrial == 2) %>% group_by(subj, reward_oneback) %>%
    summarise(stay = mean(stay_key)) %>% pivot_wider(names_from = 'reward_oneback',
                                                     values_from = 'stay',
                                                     names_prefix = 'stay')
  mean_unrw = mean(cards_trial_means$stay0)
  mean_rw = mean(cards_trial_means$stay1)
  sd_unrw = sd(cards_trial_means$stay0)
  sd_rw = sd(cards_trial_means$stay1)
  return(c(mean_unrw,mean_rw,sd_unrw,sd_rw)) 
}
