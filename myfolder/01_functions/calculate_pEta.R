calculate_pEta = function(df_anova) {
  anova0 = anova_test(data = df_anova, dv=stay,wid=subj,within = c(reward,load),effect.size="pes")
  partial_eta_interaction_effect = anova0$pes[3]
  return (round(partial_eta_interaction_effect,4))
}