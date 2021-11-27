rm(list = ls())
library("bayesplot")
library("loo")
library("brms")

load('myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_cards_analysis.Rdata')

load('myfolder/02_models/brms_agnostic_model/model_rw_uniform.Rdata')
load('myfolder/02_models/brms_agnostic_model/model_rw_weak.Rdata')
load('myfolder/02_models/brms_agnostic_model/model_rw_medium.Rdata')
load('myfolder/02_models/brms_agnostic_model/model_rw_strong.Rdata')
load('myfolder/02_models/brms_agnostic_model/model_rw_ss.Rdata')
load('myfolder/02_models/brms_agnostic_model/model_rw_cap_weakly_informed_priors.Rdata')
load('myfolder/02_models/brms_agnostic_model/model_rw_cent_cap.Rdata')
load('myfolder/02_models/brms_agnostic_model/model_rw_cap_ss_weakly_informed_priors.Rdata')
load('myfolder/02_models/brms_agnostic_model/model_rw_medium_prior.Rdata')

load('myfolder/02_models/loo_model_comparison/loo_bayes_rw_medium.Rdata')
load('myfolder/02_models/loo_model_comparison/loo_bayes_rw_set_size.Rdata')
load('myfolder/02_models/loo_model_comparison/loo_bayes_rw_capacity.Rdata')
load('myfolder/02_models/loo_model_comparison/loo_bayes_rw_centered_capacity.Rdata')
load('myfolder/02_models/loo_model_comparison/loo_bayes_rw_capacity_set_size.Rdata')

loo_rw <- loo(bayes_rw_medium)
save(loo_rw, file = 'myfolder/02_models/loo_model_comparison/loo_bayes_rw_medium.Rdata')
loo_rw_ss <- loo(bayes_rw_set_size)
save(loo_rw_ss, file = 'myfolder/02_models/loo_model_comparison/loo_bayes_rw_set_size.Rdata')
loo_rw_cap <- loo(bayes_rw_capacity)
save(loo_rw_cap, file = 'myfolder/02_models/loo_model_comparison/loo_bayes_rw_capacity.Rdata')
loo_rw_cent_cap <- loo(bayes_rw_centered_capacity)
save(loo_rw_cent_cap, file = 'myfolder/02_models/loo_model_comparison/loo_bayes_rw_centered_capacity.Rdata')
loo_rw_ss_cap <- loo(bayes_rw_capacity_set_size)
save(loo_rw_ss_cap, file = 'myfolder/02_models/loo_model_comparison/loo_bayes_rw_capacity_set_size.Rdata')

loo_cmpr=loo_compare(loo_rw,loo_rw_cent_cap, loo_rw_ss_cap,loo_rw_ss)
save(loo_cmpr, file = 'myfolder/02_models/loo_model_comparison/loo_cmpr.Rdata')
loo_cmpr
lpd_point <- cbind(loo_rw$pointwise[,"elpd_loo"],
                   loo_rw_ss$pointwise[,"elpd_loo"],
                   loo_rw_cent_cap$pointwise[,"elpd_loo"],
                   loo_rw_ss_cap$pointwise[,"elpd_loo"])
stacking_weights(lpd_point)
