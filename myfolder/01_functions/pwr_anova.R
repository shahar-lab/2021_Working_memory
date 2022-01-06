rm(list = ls())
pwr_anova = function (population_size,sample_size, reps,
                      mean_group1,
                      mean_group2,
                      sd_group1,
                      sd_group2,
                      interaction_difference,
                      independent_var_name_1,
                      independent_var_name_2,
                      dependent_var_name) {
  
  library(tidyverse)
  p_values_list = c()
  significance_list = rep(0, reps)
  source("/home/shared/2021_Working_memory/myfolder/01_functions/sample_values.R")
  df=sample_values(population_size=population_size,
                   mean_group1=mean_group1,
                   mean_group2=mean_group2,
                   sd_group1=sd_group1,
                   sd_group2=sd_group2,
                   interaction_difference=interaction_difference,
                   independent_var_name_1=independent_var_name_1,
                   independent_var_name_2=independent_var_name_2,
                   dependent_var_name=dependent_var_name)

  for (rep in 1:reps) {

    sampled_subjects = sample(1:population_size,size=sample_size)
    df_sample=df%>%filter(subject %in% sampled_subjects  )
    anova_sample = anova_test(data = df_sample, dv=dependent_var_name,wid=subject,within = c(independent_var_name_1,independent_var_name_2))
    p_val = anova_sample$p[3]
    p_values_list[rep] = p_val
  }
  file_name = sprintf("hist_interaction_%s.jpg", sample_size)
  jpeg(file_name, width = 350, height = 350)
  hist(p_values_list, breaks = 30)
  dev.off()
  significance_list[p_values_list < 0.05 ] = 1
  power = mean(significance_list)
  return (power)
}