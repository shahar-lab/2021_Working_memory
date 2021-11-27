pwr_anova = function (df,sample_size, reps) {
  p_values_list = c()
  for (rep in 1:reps) {
    subjects = sample(1:10000, size = sample_size)
    df_sample = df %>% pivot_wider(names_from = c(load, reward),
                                   values_from = stay) %>% filter(subj %in% subjects) %>% pivot_longer(
                                     cols = c(noload_rw, noload_unrw, load_unrw, load_rw),
                                     names_to = c("load", "reward"),
                                     names_sep = "_",
                                     values_to = "stay"
                                   )
    anova_sample = anova_test(data = df_sample, dv=stay,wid=subj,within = c(reward,load))
    p_val = anova_sample$p[3]
    p_values_list[rep] = p_val
  }
  file_name = sprintf("hist_interaction_%s.jpg", sample_size)
  jpeg(file_name, width = 350, height = 350)
  hist(p_values_list, breaks = 30)
  dev.off()
  p_values_list[p_values_list >= 0.05] = 0
  p_values_list[p_values_list < 0.05 & p_values_list > 0] = 1
  power = mean(p_values_list)
  return (power)
}