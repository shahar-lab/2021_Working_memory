rm(list = ls())
library(tidyverse)
library(tidylog)
library(stringr)
library(dplyr)
library(magrittr)
library(data.table)


load('myfolder/03_data/02_aggregated_data/exp2_07.2021/cards_raw.Rdata')
load('myfolder/03_data/02_aggregated_data/exp2_07.2021/memory_raw.Rdata')
load('myfolder/03_data/02_aggregated_data/exp2_07.2021/squares_raw.Rdata')

# cards preprocessing ------------------------------------------------------------

#add columns
names(cards)

cards %<>%
  mutate(
    reward_oneback = lag(reward) * 1,
    setsize_oneback= lag(set_size, 2),
    unch_card      = if_else(card_right == ch_card, card_left, card_right),
    stay_ch_card   = subj * 0,   #placeholder
    stay_unch_card = subj * 0,  #placeholder
    stay_key       = (ch_key == lag(ch_key)) * 1,
    abort          = (rt < 0.2 | rt > 4 | trial == 0)
  )

#add stay chosen and unchosen card according to subtrial 
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
subj_to_remove <- cards %>% group_by(subj) %>% summarise(stay = mean(stay_key)) %>% mutate(abort_subj = stay > .8) %>% filter(abort_subj == T) %>% select(subj)
cards %<>% filter(subj %in% subj_to_remove$subj == F)

#remove subjects for which trial omission is more than 30% after removing missing data and fast/slow reaction-times
subj_to_remove <- cards %>% group_by(subj) %>% summarise(p.trls = length(trial) / (50*4*2 - 1)) %>% mutate(abort_subj = p.trls < .70) %>% filter(abort_subj == T) %>% select(subj)
cards %<>% filter(subj %in% subj_to_remove$subj == F)

#add RT bin pre subject
Nbins=4
mybreaks    =seq(0,1,by=1/Nbins)
cards$bin_rt=rep(0,dim(cards)[1])
detach(package:tidylog) #remove tidylog to avoid multiple outputs
for (t in 1:dim(cards)[1]) {
  current.rt  =cards$rt[t]
  current.bins=cards %>% filter(subj==subj[t]) %>% summarise(Q=quantile(rt,mybreaks))%>%unlist()
     for (bin in 1:Nbins) {
        if (current.bins[bin]<=current.rt & current.rt<=current.bins[bin+1]){cards$bin_rt[t]=bin}
  }
}
library(tidylog)
#add prev_bin_rt
cards%<>%mutate(prev_bin_rt=lag(bin_rt))
cards %<>% na.omit() %>% as.data.frame() #remove first trial
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
         capacity_comb_1 = 1 * (set_size_comb_1 * 2 - 1)) 
#remove subjects who guessed in the squares part
subj_to_remove <- capacity%>%filter(avg_capacity<0|set_size_4<0.5)
cards %<>% filter(subj %in% subj_to_remove$subj == F)
memory %<>% filter(subj %in% subj_to_remove$subj == F)
#scale capacity
capacity %<>% filter(subj %in% subj_to_remove$subj == F)
capacity$scaled_avg_capacity=scale(capacity$avg_capacity)
#add capacity to each participant
for (ind in unique(cards$subj)) {
  cards[cards$subj == ind, "avg_capacity"] = capacity[capacity$subj == ind, "avg_capacity"]
  cards[cards$subj == ind, "scaled_avg_capacity"] = as.numeric(capacity[capacity$subj == ind, "scaled_avg_capacity"])
cards[cards$subj==ind,"capacity_comb_4"]=capacity_comb[capacity_comb$subj == ind, "capacity_comb_4"]
  }

#add avg_capacity as a categorical variable
cards$'capacity_high_low' = (cards$avg_capacity > median(unlist(capacity$avg_capacity)))*1

#summary
#three subjects were removed due sticking with the same key for more then 80% of the trial
#ten subjects were removed due to having more then 30% trials removed due to missing data, fast or slow RTs (changed from, 20% at prereg)
#two subjects were removed due to negative capacity or two less than chance performance in set_size 4

save(cards, file = 'myfolder/03_data/02_aggregated_data/data_for_analysis/data_cards.Rdata')
save(squares, file = 'myfolder/03_data/02_aggregated_data/data_for_analysis/data_squares_combined.Rdata')
save(memory, file = 'myfolder/03_data/02_aggregated_data/data_for_analysis/data_squares.Rdata')
