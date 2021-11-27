find_x_from_pEta=function(interaction_effect_vec,population_size,mean_unrw,mean_rw,sd_unrw,sd_rw){
  partial_etas = c()
  for (interaction_effect in interaction_effect_vec) {
    df = sample_values(population_size,
                       interaction_effect,
                       mean_unrw,
                       mean_rw,
                       sd_unrw,
                       sd_rw)
    partial_eta = calculate_pEta(df)
    partial_etas = append(partial_etas, partial_eta)
  }
  partial_etas_df = data_frame(interaction_effect_vec, partial_etas)
  return(partial_etas_df)
}