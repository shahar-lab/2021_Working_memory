run_power=function(df,sample_sizes,reps){
  power_vector = c()
  for (sample_size in sample_sizes){
    power_by_sample=pwr_anova(df,sample_size, reps)
    power_vector = append(power_vector, power_by_sample)
  }
  size=sample_sizes
  df_results = data.frame(size, power_vector)
  df_results %>% ggplot(aes(x = size, y = power_vector)) + geom_bar(stat = 'identity') +
    xlab('sample_size') + ylab('power')
  return(df_results)
}
