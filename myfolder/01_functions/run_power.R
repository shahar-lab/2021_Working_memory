run_power=function(population_size,sample_sizes, reps,
                   mean_group1,
                   mean_group2,
                   sd_group1,
                   sd_group2,
                   interaction_difference,
                   independent_var_name_1,
                   independent_var_name_2,
                   dependent_var_name){
  power_vector = c()
  for (sample_size in sample_sizes){
    power_by_sample=pwr_anova(population_size,sample_size, reps,
                              mean_group1,
                              mean_group2,
                              sd_group1,
                              sd_group2,
                              interaction_difference,
                              independent_var_name_1,
                              independent_var_name_2,
                              dependent_var_name)
    power_vector = append(power_vector, power_by_sample)
  }
  size=sample_sizes
  df_results = data.frame(size, power_vector)
  g=df_results %>% ggplot(aes(x = size, y = power_vector)) + geom_bar(stat = 'identity') +
    xlab('sample_size') + ylab('power')
  print(g)
  return(df_results)
}
