rm(list = ls())
library(tidyverse)
library(tidylog)
library(stringr)
library(stringi)
library(dplyr)
library(magrittr)
library(data.table)


load('myfolder/03_data/02_aggregated_data/exp2_07.2021/cards2_raw.Rdata')
load('myfolder/03_data/02_aggregated_data/exp2_07.2021/cards3_raw.Rdata')
load('myfolder/03_data/02_aggregated_data/exp2_07.2021/memory2_raw.Rdata')
load('myfolder/03_data/02_aggregated_data/exp2_07.2021/memory3_raw.Rdata')
load('myfolder/03_data/02_aggregated_data/exp2_07.2021/squares_raw.Rdata')
load('myfolder/03_data/02_aggregated_data/exp2_07.2021/icar_raw.Rdata')
load('myfolder/03_data/02_aggregated_data/exp2_07.2021/oci_raw.Rdata')
demographic=read.csv("myfolder/03_data/03_demographic_data/demographic_07.2021.csv")



# demographic -------------------------------------------------------------
demographic$'icar'=icar$score
demographic$'ocir'=oci$score


# combine sessions --------------------------------------------------------
cards2$session='1'
cards3$session='2'
cards=rbind(cards2,cards3)
cards=cards%>%relocate(session,.after=subj)
memory2$session='1'
memory3$session='2'
memory=rbind(memory2,memory3)
memory=memory%>%relocate(session,.after=subj)

# cards preprocessing ------------------------------------------------------------

#add columns
names(cards)

cards %<>%
  mutate(
    square_correct = rep(memory$acc,each=2),
    reward_oneback = lag(reward) * 1,
    setsize_oneback= lag(set_size, 2),
    unch_card      = if_else(card_right == ch_card, card_left, card_right),
    stay_ch_card   = subj * 0,   #placeholder
    stay_unch_card = subj * 0,  #placeholder
    stay_key       = (ch_key == lag(ch_key)) * 1,
    abort          = (rt < 0.2 | rt > 4 | trial == 0),
    ch_key_sqr=rep(memory$ch_key,each=2),
    stay_key_sqr=(ch_key==ch_key_sqr)*1,
    correct_response_sqr=rep(memory$correct_response,each=2),
    is_ch_key_correct_sqr=(ch_key==correct_response_sqr)*1
  )

#add stay chosen and unchosen card according to subtrial - those two columns look on the last time the currently chosen card appeared.
#If it was previously chosen then stay_ch_card==1, if it was previously unchosen, then stay_unch_card==1.

cards %<>%
  mutate(stay_ch_card   = if_else(subtrial == 1,(ch_card == lag(ch_card, 1) | ch_card == lag(ch_card, 2)) * 1, (ch_card == lag(ch_card, 2) | ch_card == lag(ch_card, 3)) * 1), 
         stay_unch_card = if_else(subtrial == 1,(ch_card == lag(unch_card, 1) | ch_card == lag(unch_card, 2)) * 1, (ch_card == lag(unch_card, 2) | ch_card == lag(unch_card, 3)) * 1))

#get the reward in the last time that a specific card appeared
cards%<>%mutate(rw_when_ch_last_appeared=case_when(
  subtrial==1&(lag(card_left,1)==ch_card|lag(card_right,1)==ch_card) ~ lag(reward,1),
  subtrial==1&(lag(card_left,2)==ch_card|lag(card_right,2)==ch_card) ~ lag(reward,2),
  subtrial==2&(lag(card_left,2)==ch_card|lag(card_right,2)==ch_card) ~ lag(reward,2),
  subtrial==2&(lag(card_left,3)==ch_card|lag(card_right,3)==ch_card) ~ lag(reward,3)
) 
)
cards%<>%mutate(rw_when_unch_last_appeared=case_when(
  subtrial==1&(lag(card_left,1)==unch_card|lag(card_right,1)==unch_card) ~ lag(reward,1),
  subtrial==1&(lag(card_left,2)==unch_card|lag(card_right,2)==unch_card) ~ lag(reward,2),
  subtrial==2&(lag(card_left,2)==unch_card|lag(card_right,2)==unch_card) ~ lag(reward,2),
  subtrial==2&(lag(card_left,3)==unch_card|lag(card_right,3)==unch_card) ~ lag(reward,3)
)
)


cards%<>%mutate(set_size_when_ch_last_appeared=case_when(
  subtrial==1&(lag(card_left,1)==ch_card|lag(card_right,1)==ch_card) ~ lag(set_size,1),
  subtrial==1&(lag(card_left,2)==ch_card|lag(card_right,2)==ch_card) ~ lag(set_size,2),
  subtrial==2&(lag(card_left,2)==ch_card|lag(card_right,2)==ch_card) ~ lag(set_size,2),
  subtrial==2&(lag(card_left,3)==ch_card|lag(card_right,3)==ch_card) ~ lag(set_size,3)
)
)

cards%<>%mutate(set_size_when_unch_last_appeared=case_when(
  subtrial==1&(lag(card_left,1)==unch_card|lag(card_right,1)==unch_card) ~ lag(set_size,1),
  subtrial==1&(lag(card_left,2)==unch_card|lag(card_right,2)==unch_card) ~ lag(set_size,2),
  subtrial==2&(lag(card_left,2)==unch_card|lag(card_right,2)==unch_card) ~ lag(set_size,2),
  subtrial==2&(lag(card_left,3)==unch_card|lag(card_right,3)==unch_card) ~ lag(set_size,3)
)
)

#clear aborted trials and NA
cards %<>% filter(abort == F) %>% na.omit() %>% as.data.frame()

#remove subjects who selected the same response key in more than 80% of the time
subj_to_remove = as.data.frame(c())
subj_to_remove <- rbind(subj_to_remove,cards %>% group_by(subj) %>% summarise(stay = mean(stay_key)) %>% mutate(abort_subj = stay > .8) %>% filter(abort_subj == T) %>% dplyr::select(subj))
#remove subjects for which trial omission is more than 30% after removing missing data and fast/slow reaction-times
subj_to_remove <- rbind(subj_to_remove,cards %>% group_by(subj) %>% summarise(p.trls = length(trial) / (50*2*3*2 - 1)) %>% mutate(abort_subj = p.trls < .70) %>% filter(abort_subj == T) %>% select(subj))
# add RT bin pre subject
# Nbins=4
# mybreaks    =seq(0,1,by=1/Nbins)
# cards$bin_rt=rep(0,dim(cards)[1])
# detach(package:tidylog) #remove tidylog to avoid multiple outputs
# for (t in 1:dim(cards)[1]) {
#   current.rt  =cards$rt[t]
#   current.bins=cards %>% filter(subj==subj[t]) %>% summarise(Q=quantile(rt,mybreaks))%>%unlist()
#   for (bin in 1:Nbins) {
#     if (current.bins[bin]<=current.rt & current.rt<=current.bins[bin+1]){cards$bin_rt[t]=bin}
#   }
# }
# library(tidylog)
# #add prev_bin_rt
# cards%<>%mutate(prev_bin_rt=lag(bin_rt))

#remove first trial
cards %<>% na.omit() %>% as.data.frame() 
# squares(calculate capacity) ----------------------------------------------------------------

#clear aborted trials and NA
squares %<>% na.omit() %>% as.data.frame()
memory %<>% na.omit() %>% as.data.frame()
#calculate capacity
capacity = squares %>%
  group_by(subj, set_size) %>%
  summarise(acc = mean(acc)) %>%
  pivot_wider(names_from = "set_size",
              values_from = "acc",
              names_prefix = "set_size_") %>%
  mutate(capacity_8 = 8 * (set_size_8 * 2 - 1),
         capacity_4 = 4 * (set_size_4 * 2 - 1)) %>%
  mutate(avg_capacity = (capacity_8 + capacity_4) / 2)

capacity_comb=memory%>%
  group_by(subj,set_size)%>%
  summarise(acc = mean(acc)) %>%
  pivot_wider(names_from = "set_size",
              values_from = "acc",
              names_prefix = "set_size_comb_") %>%
  mutate(capacity_comb_4 = 4 * (set_size_comb_4 * 2 - 1),
         capacity_comb_1 = 1 * (set_size_comb_1 * 2 - 1),
         capacity_comb_0 = 1 * (set_size_comb_0 * 2 - 1)) 

#add capacity to demographics
demographic$avg_capacity=capacity$avg_capacity
demographic$avg_comb_capacity=rowMeans(capacity_comb[c("capacity_comb_4","capacity_comb_1","capacity_comb_0")])
#remove subjects who guessed in the squares part
subj_to_remove <- rbind(subj_to_remove,capacity%>%filter(avg_capacity<0|set_size_4<0.5)%>%dplyr::select(subj))

#remove subjects who guessed in the memory part
#subj_to_remove <- rbind(subj_to_remove,capacity_comb%>%filter(capacity_comb_0<0.1)%>%dplyr::select(subj))

#remove subjects from data-frames
cards %<>% filter(subj %in% subj_to_remove$subj == F)
demographic %<>% filter(subj %in% subj_to_remove$subj == F)
memory %<>% filter(subj %in% subj_to_remove$subj == F)
squares %<>% filter(subj %in% subj_to_remove$subj == F)
capacity %<>% filter(subj %in% subj_to_remove$subj == F)
capacity_comb %<>% filter(subj %in% subj_to_remove$subj == F)

#scale capacity
capacity$scaled_avg_capacity=scale(capacity$avg_capacity)
capacity$centered_avg_capacity=scale(capacity$avg_capacity,center=T,scale=F)

#add diff to each subject in demographic
demographic["diff"]=cards%>%filter(subtrial==2)%>%
  group_by(subj,reward_oneback)%>%
  summarise(stay=mean(stay_key))%>%
  pivot_wider(names_from=reward_oneback,values_from=stay)%>%
  mutate(diff=`1`-`0`)%>%pull(diff)

#add diff to each session
demographic=cbind(demographic,cards%>%filter(subtrial==2)%>%
  group_by(subj,session,reward_oneback)%>%
  summarise(stay=mean(stay_key))%>%
  pivot_wider(names_from=c(session,reward_oneback),values_from=stay)%>%
  mutate(diff1=`1_1`-`1_0`,diff2=`2_1`-`2_0`)%>%
  ungroup()%>%
  dplyr::select(diff1,diff2))
#add capacity,sex, ocir and icar to each participant in cards df
for (ind in unique(cards$subj)) {
  cards[cards$subj == ind, "avg_capacity"] = capacity[capacity$subj == ind, "avg_capacity"]
  cards[cards$subj == ind, "centered_avg_capacity"] = as.numeric(capacity[capacity$subj == ind, "centered_avg_capacity"])
  cards[cards$subj == ind, "scaled_avg_capacity"] = as.numeric(capacity[capacity$subj == ind, "scaled_avg_capacity"])
  cards[cards$subj==ind,"capacity_comb_4"]=capacity_comb[capacity_comb$subj == ind, "capacity_comb_4"]
  cards[cards$subj==ind,"sex"]=demographic[demographic$subj==ind,"sex"]
  cards[cards$subj==ind,"icar"]=demographic[demographic$subj==ind,"icar"]
  cards[cards$subj==ind,"ocir"]=demographic[demographic$subj==ind,"ocir"]
  }

#add avg_capacity as a categorical variable
cards%<>%mutate(capacity_high_low = if_else(cards$avg_capacity > median(unlist(capacity$avg_capacity)),"High","Low"))
cards%<>%mutate(capacity_high_low = factor(capacity_high_low, levels = c("High", "Low")))

#add subj as a categorical variable
cards%<>%mutate(subj=as.factor(subj))


#change set_size and reward_oneback to dummy coding
cards <- cards %>%
  mutate(set_size = case_when(
    set_size==0 ~ "Low",
    set_size==1 ~ "Medium",
    set_size==4 ~ "High"
  ),
  set_size = factor(set_size, levels = c("Low","Medium", "High")),
  reward_oneback=as.factor(reward_oneback))

### add prob_reward for each card
reward_matrix_0 = c(0.33772, 0.39326, 0.40412, 0.45749, 0.48324, 0.48626, 0.45786, 0.42262, 0.49357, 0.50731, 0.54289, 0.53135, 0.50034, 0.49953, 0.48937, 0.50251, 0.53868, 0.52203, 0.49471, 0.45876, 0.52796, 0.50268, 0.46898, 0.4837, 0.47281, 0.48038, 0.42808, 0.39279, 0.37639, 0.36649, 0.35582, 0.35915, 0.38079, 0.37371, 0.37682, 0.41413, 0.39547, 0.44378, 0.46997, 0.47091, 0.50357, 0.53194, 0.51394, 0.52503, 0.53825, 0.54097, 0.49772, 0.50029, 0.48817, 0.50105, 0.45351, 0.46345, 0.50613, 0.51335, 0.56239, 0.56169, 0.56903, 0.5471, 0.50874, 0.53747, 0.50208, 0.51419, 0.53481, 0.47552, 0.48684, 0.51724, 0.49535, 0.51149, 0.53235, 0.4978, 0.47696, 0.47291, 0.48077, 0.4425, 0.48969, 0.52356, 0.54573, 0.49781, 0.52555, 0.50405, 0.519, 0.51776, 0.49547, 0.48375, 0.45753, 0.5155, 0.51645, 0.50343, 0.47022, 0.47794, 0.51657, 0.52218, 0.52722, 0.5297, 0.52667, 0.56737, 0.6445, 0.63256, 0.59317, 0.5313)
reward_matrix_1 = c(0.25704, 0.25686, 0.26537, 0.32215, 0.2977, 0.31559, 0.29018, 0.24321, 0.24308, 0.27374, 0.28045, 0.31219, 0.3303, 0.30804, 0.37726, 0.40205, 0.37142, 0.34159, 0.31001, 0.30259, 0.25156, 0.28041, 0.29775, 0.33556, 0.33383, 0.27651, 0.31895, 0.3416, 0.31953, 0.27126, 0.28677, 0.27745, 0.23634, 0.18869, 0.1756, 0.16868, 0.16833, 0.16272, 0.16272, 0.16828, 0.23888, 0.215, 0.2362, 0.2198, 0.25504, 0.24835, 0.23921, 0.25358, 0.28813, 0.29193, 0.30125, 0.26231, 0.25296, 0.28666, 0.26313, 0.2301, 0.21956, 0.20996, 0.25839, 0.28052, 0.32197, 0.35135, 0.36251, 0.35102, 0.37592, 0.41482, 0.46239, 0.46043, 0.50986, 0.56564, 0.59565, 0.62044, 0.56731, 0.55831, 0.58139, 0.58884, 0.54916, 0.5071, 0.51807, 0.54408, 0.56728, 0.52195, 0.55915, 0.53774, 0.53981, 0.52355, 0.46647, 0.4676, 0.45075, 0.48408, 0.55282, 0.54373, 0.53274, 0.53184, 0.53568, 0.49976, 0.50574, 0.552, 0.51679, 0.53681)
reward_matrix_2 = c(0.5917, 0.64769, 0.70574, 0.68713, 0.68242, 0.70379, 0.71255, 0.70272, 0.67069, 0.65692, 0.65933, 0.64338, 0.65085, 0.64829, 0.69846, 0.71015, 0.70442, 0.74258, 0.7554, 0.73006, 0.75518, 0.72853, 0.74084, 0.68069, 0.71271, 0.68525, 0.67073, 0.67068, 0.648, 0.67646, 0.70308, 0.68574, 0.71904, 0.71133, 0.73911, 0.74918, 0.73149, 0.70956, 0.68745, 0.69793, 0.6233, 0.62599, 0.66243, 0.69599, 0.68707, 0.649, 0.60675, 0.59887, 0.63346, 0.69691, 0.66782, 0.69997, 0.72736, 0.70792, 0.67461, 0.6478, 0.65467, 0.6329, 0.6158, 0.60556, 0.64038, 0.63054, 0.58793, 0.56423, 0.55994, 0.56647, 0.54658, 0.49899, 0.50783, 0.57898, 0.54492, 0.48585, 0.48109, 0.48699, 0.46963, 0.48634, 0.46603, 0.47676, 0.45376, 0.40843, 0.39107, 0.40562, 0.35266, 0.33274, 0.31999, 0.34656, 0.37718, 0.37391, 0.38179, 0.40437, 0.4088, 0.33946, 0.33295, 0.31311, 0.32477, 0.37747, 0.36431, 0.35193, 0.33457, 0.32013)
reward_matrix_3 = c(0.37146, 0.29856, 0.30642, 0.2707, 0.25356, 0.25763, 0.1921, 0.20666, 0.19305, 0.19291, 0.18298, 0.18156, 0.19481, 0.15628, 0.18232, 0.21027, 0.20945, 0.22949, 0.20882, 0.23431, 0.24699, 0.19052, 0.2206, 0.21146, 0.19117, 0.21937, 0.23521, 0.27283, 0.33207, 0.33282, 0.3137, 0.32797, 0.33243, 0.33035, 0.37739, 0.38959, 0.39165, 0.43298, 0.4753, 0.45643, 0.46237, 0.46749, 0.47418, 0.47148, 0.46565, 0.48012, 0.51917, 0.56677, 0.551, 0.52118, 0.5035, 0.51614, 0.48258, 0.4852, 0.51321, 0.5236, 0.54053, 0.58072, 0.58066, 0.60732, 0.60788, 0.62424, 0.61417, 0.61129, 0.61249, 0.63833, 0.6153, 0.64763, 0.66878, 0.65734, 0.66195, 0.66565, 0.68452, 0.66885, 0.6788, 0.69809, 0.6875, 0.69827, 0.6978, 0.72358, 0.7295, 0.74277, 0.73044, 0.74494, 0.74108, 0.74222, 0.74686, 0.80182, 0.80182, 0.82339, 0.81468, 0.81121, 0.84829, 0.83158, 0.83158, 0.80201, 0.74137, 0.72785, 0.73865, 0.76278)
reward_matrix=data.frame(reward_matrix_0,reward_matrix_1,reward_matrix_2,reward_matrix_3)
reward_matrix=reward_matrix[c(1:50),]

cards %<>% rowwise()%>% mutate(prob_chosen=reward_matrix[trial,ch_card+1])%>% 
mutate(prob_unchosen=reward_matrix[trial,unch_card+1])%>%
mutate(chose_better=if_else(prob_chosen>=prob_unchosen,1,0))
demographic%<>%mutate(cards%>%group_by(subj,ch_key)%>%summarise(reward=mean(reward))%>%
                        pivot_wider(names_from='ch_key',values_from='reward')%>%
                        mutate(real_diff=`1`-`0`)%>%
                        dplyr::select(-c(`0`,`1`)))

#add prob_diff between cards
cards%<>%mutate(prob_diff=prob_chosen-prob_unchosen)

#add real_diff to demographic to understand if location is really irrelevant?
demographic%<>%mutate(cards%>%group_by(subj,ch_key)%>%summarise(reward=mean(reward))%>%
                        pivot_wider(names_from = 'ch_key', values_from = 'reward') %>%
                        mutate(real_diff = `1` - `0`) %>%
                        dplyr::select(-c(`0`,`1`)))
#add real_diff by session
demographic=cbind(demographic,cards%>%
                    group_by(subj,session,ch_key)%>%
                    summarise(reward=mean(reward))%>%
                    pivot_wider(names_from=c(session,ch_key),values_from=reward)%>%
                    mutate(real_diff1=`1_1`-`1_0`,real_diff2=`2_1`-`2_0`)%>%
                    ungroup()%>%
                    dplyr::select(real_diff1,real_diff2))

#check for repeating pair
cards=as.data.frame.data.frame(cards)
cards=cards%>%mutate(card_pair = paste(card_right,card_left),
                     reoffer_pair=if_else(subtrial==1,
                                          card_pair==lag(card_pair,1)|card_pair==lag(card_pair,2)|stri_reverse(card_pair)==lag(card_pair,1)|stri_reverse(card_pair)==lag(card_pair,2),
                                          card_pair==lag(card_pair,2)|card_pair==lag(card_pair,3)|stri_reverse(card_pair)==lag(card_pair,2)|stri_reverse(card_pair)==lag(card_pair,3))
                     )
#add repeating pair for keys
cards=cards%>%mutate(repeat_pair_one_back=subtrial==1&(card_pair==lag(card_pair,1)|stri_reverse(card_pair)==lag(card_pair,1)))

#cards for analysis - only second subtrial
cards_analysis=cards%>%filter(subtrial==2)



#summary
#one subject was removed due to pressing the same key for more then 80% of the trial
#three subjects were removed due to having more then 30% trials removed due to missing data, fast or slow RTs (changed from, 20% at prereg)
#one subjects was removed due to negative capacity or to less than chance performance in set_size 4

save(cards_analysis, file = 'myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_cards_analysis.Rdata')
save(cards, file = 'myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_cards.Rdata')
save(memory, file = 'myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_squares_combined.Rdata')
save(squares, file = 'myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_squares.Rdata')
save(demographic, file = 'myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_demographic.Rdata')
save(capacity, file = 'myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_capacity.Rdata')
save(capacity_comb, file = 'myfolder/03_data/02_aggregated_data/exp2_07.2021/data_for_analysis/data_capacity_comb.Rdata')

