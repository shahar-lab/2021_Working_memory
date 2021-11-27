sample_values = function(population_size,
                         interaction_effect,
                         mean_unrw,
                         mean_rw,
                         sd_unrw,
                         sd_rw) {
  noload_unrw = rnorm(population_size, mean_unrw, sd_unrw)
  #Something I thought of:
  #More load=bigger difference between the means.
  #We predict that 1 square was already loading.
  #Therefore, we predict that this difference we got would be smaller in our no load condition.
  #So maybe we should reduce the differences between the means for the noload condition.
  noload_rw = rnorm(population_size, mean_rw, sd_rw)
  #(b) -----------------------------------------------------------------------
  load_unrw = rnorm(population_size, mean_unrw, sd_unrw)
  load_rw = rnorm(population_size, mean_rw + interaction_effect, sd_rw)
  df_anova = data_frame(noload_rw, noload_unrw, load_unrw, load_rw)
  df_anova = df_anova%>%mutate(subj = row_number()) %>% pivot_longer(
    cols = c(noload_rw, noload_unrw, load_unrw, load_rw),
    names_to = c("load", "reward"),
    names_sep = "_",
    values_to = "stay"
  )
  return (df_anova)
}