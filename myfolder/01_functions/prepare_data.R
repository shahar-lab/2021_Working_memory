library(tidyverse)

data_squares=clean_squares_exp #visual working memory task
data_cards=clean_combined_cards_exp #combined_cards
data_squares_combined=clean_combined_squares_exp #combined_squares
#calculate bonus for each subject - for prolific
df_reward=data_cards%>%group_by(subj)%>%summarise(total_reward=sum(reward1,na.rm=T)+sum(reward2,na.rm = T))
df_reward$bonus=df_reward$total_reward*0.003
#write.csv(df_reward,"C:/Users/ido/0004_Ido_Set-size_effects/irrelevant_learning_WM/MyFolder/data/raw_data/bonus_prolific.csv")

#add sex to each participant
data_sex=read.csv('C:/Users/ido/0004_Ido_Set-size_effects/irrelevant_learning_WM/MyFolder/data/raw_data/all_data_sex.csv') #in demographic_data folder in the server
colnames(data_sex)[1]="subj"
#data_sex$subj=unique(data_cards$subj)
for (ind in unique(data_cards$subj)){
  data_cards[data_cards$subj==ind,"sex"]=data_sex[data_sex$subj==ind,"sex"]
}

#add experiment column
for (ind in 1:nrow(data_cards)){
  if (data_cards[ind,"subj"]<99){
    data_cards[ind,"experiment"]=1
    data_squares_combined[ind,"experiment"]=1
  }
  else{
    data_cards[ind,"experiment"]=2
    data_squares_combined[ind,"experiment"]=2
  }
}
for (ind in 1:nrow(data_squares)){
  if (data_squares[ind,"subj"]<99){
    data_squares[ind,"experiment"]=1
  }
  else{
    data_squares[ind,"experiment"]=2
  }
}

#remove first trial
data_cards=data_cards[data_cards$trial_num!=0,]
data_squares_combined=data_squares_combined[data_squares_combined$trial_num!=0,]
rownames(data_cards)=1:nrow(data_cards)
rownames(data_squares_combined)=1:nrow(data_squares_combined)
#add rt_squares and key_press to data_cards
data_cards$rt_squares=data_squares_combined$rt
data_cards$key_press_squares=data_squares_combined$key_press
#organize columns
data_cards=data_cards[c("subj","block","trial_num","trial_name1","rt1","card_left1","card_right1","key_press1","card_selected1","reward1","trial_name2","rt2","card_left2","card_right2","key_press2","card_selected2","reward2","stay_1_2","rt_squares","key_press_squares","set_size","condition","is_correct","sex")]
#change column name
colnames(data_cards)[18]="stay_key_1_2"

#remove no response trials (key press1/2/squares =NA) from data sets
data_squares_combined=data_squares_combined[-which(is.na(unlist(data_cards$key_press1))),]
data_cards=data_cards[-which(is.na(unlist(data_cards$key_press1))),]
rownames(data_cards)=1:nrow(data_cards)
rownames(data_squares_combined)=1:nrow(data_squares_combined)

data_squares_combined=data_squares_combined[-which(is.na(unlist(data_cards$key_press2))),]
data_cards=data_cards[-which(is.na(unlist(data_cards$key_press2))),]
rownames(data_cards)=1:nrow(data_cards)
rownames(data_squares_combined)=1:nrow(data_squares_combined)

data_squares_combined=data_squares_combined[-which(is.na(unlist(data_cards$key_press_squares))),]
data_cards=data_cards[-which(is.na(unlist(data_cards$key_press_squares))),]
rownames(data_cards)=1:nrow(data_cards)
rownames(data_squares_combined)=1:nrow(data_squares_combined)
#remove no response trials
data_squares$key_press[sapply(data_squares$key_press, is.null)] <- NA
data_squares=data_squares[-which(is.na(unlist(data_squares$key_press))),]
rownames(data_squares)=1:nrow(data_squares)
#change Null to NA
data_squares_combined$key_press[sapply(data_squares_combined$key_press, is.null)] <- NA

#add unchosen card
data_cards=data_cards%>%mutate(card_not_selected1=if_else(card_right1==as.character(card_selected1),card_left1,card_right1))
data_cards=data_cards%>%mutate(card_not_selected2=if_else(card_right2==as.character(card_selected2),card_left2,card_right2))
#select only rows in which cards combination was not repeated
data_cards=data_cards%>%mutate(same_combination=if_else(lag(as.numeric(card_right1),1)+lag(as.numeric(card_left1),1)==as.numeric(card_right1)+as.numeric(card_left1)|lag(as.numeric(card_right1),1)+lag(as.numeric(card_left1),1)==as.numeric(card_right2)+as.numeric(card_left2),"Repeat","Non-repeat"))
data_cards_non_repeat=data_cards%>%filter(same_combination=="Non-repeat")
#check if unchosen cards were chosen now
data_cards_non_repeat=data_cards_non_repeat%>%mutate(pick_unchosen_card_1=lag(card_not_selected1,1)==as.character(card_selected1)|lag(card_not_selected1,1)==as.character(card_selected2))
data_cards_non_repeat=data_cards_non_repeat%>%mutate(pick_unchosen_card_2=lag(card_not_selected2,1)==as.character(card_selected1)|lag(card_not_selected2,1)==as.character(card_selected2))
#add reward and set_size in previous trial
data_cards_non_repeat=data_cards_non_repeat%>%mutate(reward1_prev=lag(reward1,1),reward2_prev=lag(reward2,1))
data_cards_non_repeat=data_cards_non_repeat%>%mutate(set_size_prev=lag(set_size,1))
#remove NA trial from data_cards_non_repeat
data_cards_non_repeat=data_cards_non_repeat[-1,]

#remove extremely fast or slow trials (rt<200ms or rt>4000ms)
data_squares_combined=data_squares_combined[unlist(data_cards$rt1)>200 & unlist(data_cards$rt1)<4000,] #remove for rt in first card choice
data_cards=data_cards[unlist(data_cards$rt1)>200 & unlist(data_cards$rt1)<4000,] 
rownames(data_cards)=1:nrow(data_cards)
rownames(data_squares_combined)=1:nrow(data_squares_combined)
data_squares_combined=data_squares_combined[unlist(data_cards$rt2)>200 & unlist(data_cards$rt2)<4000,] #remove for rt in second card choice
data_cards=data_cards[unlist(data_cards$rt2)>200 & unlist(data_cards$rt2)<4000,] 
rownames(data_cards)=1:nrow(data_cards)
rownames(data_squares_combined)=1:nrow(data_squares_combined)
data_squares_combined=data_squares_combined[unlist(data_cards$rt_squares)>200 & unlist(data_cards$rt_squares)<4000,] #remove for rt in square choice
data_cards=data_cards[unlist(data_cards$rt_squares)>200 & unlist(data_cards$rt_squares)<4000,] 
rownames(data_cards)=1:nrow(data_cards)
rownames(data_squares_combined)=1:nrow(data_squares_combined)
#the same for the only_squares part
data_squares=data_squares[unlist(data_squares$rt)>200 & unlist(data_squares$rt)<4000,]
rownames(data_squares)=1:nrow(data_squares)


#change set_size to dummy coding
data_cards <- data_cards %>%
  mutate(set_size = factor(set_size),
         set_size = if_else(set_size == 1, "Low", "High"),
         set_size = factor(set_size, levels = c("Low", "High")))
#change reward1 to dummy coding
data_cards <- data_cards %>%
  mutate(reward1 = factor(reward1),
         reward1 = if_else(reward1 == 0, "0", "1"),
         reward1 = factor(reward1, levels = c("0", "1")))
#remove subject 83 from all the data - as his capacity is negative in the  change detection task
#data_cards=data_cards[data_cards$subj!=83,]
#data_squares_combined=data_squares_combined[data_squares_combined$subj!=83,] #guessing in squares
#data_squares=data_squares[data_squares$subj!=83,]

#remove subjects who selected the same response key in more than 80% of the time
df_repeat=data_cards%>%group_by(subj)%>%summarise(stay=mean(stay_key_1_2))
df_repeat=df_repeat[df_repeat$stay<0.2|df_repeat$stay>0.8,] 
data_cards=data_cards[!(data_cards$subj %in% df_repeat$subj),]
data_squares_combined=data_squares_combined[!(data_squares_combined$subj %in% df_repeat$subj),]
data_squares=data_squares[!(data_squares$subj %in% df_repeat$subj),]
rownames(data_cards)=1:nrow(data_cards)
rownames(data_squares_combined)=1:nrow(data_squares_combined)
rownames(data_squares)=1:nrow(data_squares)

#remove subjects for which trial omission is more than 20% after fast/slow rt removel
df_num_of_trials=data_cards%>%group_by(subj)%>%summarise(num_trials=sum(table(unlist(rt2)))) #df with number of trials for each subject
df_exclude=df_num_of_trials[df_num_of_trials$num_trials<160,]
data_cards=data_cards[!(data_cards$subj %in% df_exclude$subj),]
data_squares=data_squares[!(data_squares$subj %in% df_exclude$subj),]
data_squares_combined=data_squares_combined[!(data_squares_combined$subj %in% df_exclude$subj),]
### add prob_reward for each card
reward_matrix_0 = c(0.33772, 0.39326, 0.40412, 0.45749, 0.48324, 0.48626, 0.45786, 0.42262, 0.49357, 0.50731, 0.54289, 0.53135, 0.50034, 0.49953, 0.48937, 0.50251, 0.53868, 0.52203, 0.49471, 0.45876, 0.52796, 0.50268, 0.46898, 0.4837, 0.47281, 0.48038, 0.42808, 0.39279, 0.37639, 0.36649, 0.35582, 0.35915, 0.38079, 0.37371, 0.37682, 0.41413, 0.39547, 0.44378, 0.46997, 0.47091, 0.50357, 0.53194, 0.51394, 0.52503, 0.53825, 0.54097, 0.49772, 0.50029, 0.48817, 0.50105, 0.45351, 0.46345, 0.50613, 0.51335, 0.56239, 0.56169, 0.56903, 0.5471, 0.50874, 0.53747, 0.50208, 0.51419, 0.53481, 0.47552, 0.48684, 0.51724, 0.49535, 0.51149, 0.53235, 0.4978, 0.47696, 0.47291, 0.48077, 0.4425, 0.48969, 0.52356, 0.54573, 0.49781, 0.52555, 0.50405, 0.519, 0.51776, 0.49547, 0.48375, 0.45753, 0.5155, 0.51645, 0.50343, 0.47022, 0.47794, 0.51657, 0.52218, 0.52722, 0.5297, 0.52667, 0.56737, 0.6445, 0.63256, 0.59317, 0.5313)
reward_matrix_1 = c(0.25704, 0.25686, 0.26537, 0.32215, 0.2977, 0.31559, 0.29018, 0.24321, 0.24308, 0.27374, 0.28045, 0.31219, 0.3303, 0.30804, 0.37726, 0.40205, 0.37142, 0.34159, 0.31001, 0.30259, 0.25156, 0.28041, 0.29775, 0.33556, 0.33383, 0.27651, 0.31895, 0.3416, 0.31953, 0.27126, 0.28677, 0.27745, 0.23634, 0.18869, 0.1756, 0.16868, 0.16833, 0.16272, 0.16272, 0.16828, 0.23888, 0.215, 0.2362, 0.2198, 0.25504, 0.24835, 0.23921, 0.25358, 0.28813, 0.29193, 0.30125, 0.26231, 0.25296, 0.28666, 0.26313, 0.2301, 0.21956, 0.20996, 0.25839, 0.28052, 0.32197, 0.35135, 0.36251, 0.35102, 0.37592, 0.41482, 0.46239, 0.46043, 0.50986, 0.56564, 0.59565, 0.62044, 0.56731, 0.55831, 0.58139, 0.58884, 0.54916, 0.5071, 0.51807, 0.54408, 0.56728, 0.52195, 0.55915, 0.53774, 0.53981, 0.52355, 0.46647, 0.4676, 0.45075, 0.48408, 0.55282, 0.54373, 0.53274, 0.53184, 0.53568, 0.49976, 0.50574, 0.552, 0.51679, 0.53681)
reward_matrix_2 = c(0.5917, 0.64769, 0.70574, 0.68713, 0.68242, 0.70379, 0.71255, 0.70272, 0.67069, 0.65692, 0.65933, 0.64338, 0.65085, 0.64829, 0.69846, 0.71015, 0.70442, 0.74258, 0.7554, 0.73006, 0.75518, 0.72853, 0.74084, 0.68069, 0.71271, 0.68525, 0.67073, 0.67068, 0.648, 0.67646, 0.70308, 0.68574, 0.71904, 0.71133, 0.73911, 0.74918, 0.73149, 0.70956, 0.68745, 0.69793, 0.6233, 0.62599, 0.66243, 0.69599, 0.68707, 0.649, 0.60675, 0.59887, 0.63346, 0.69691, 0.66782, 0.69997, 0.72736, 0.70792, 0.67461, 0.6478, 0.65467, 0.6329, 0.6158, 0.60556, 0.64038, 0.63054, 0.58793, 0.56423, 0.55994, 0.56647, 0.54658, 0.49899, 0.50783, 0.57898, 0.54492, 0.48585, 0.48109, 0.48699, 0.46963, 0.48634, 0.46603, 0.47676, 0.45376, 0.40843, 0.39107, 0.40562, 0.35266, 0.33274, 0.31999, 0.34656, 0.37718, 0.37391, 0.38179, 0.40437, 0.4088, 0.33946, 0.33295, 0.31311, 0.32477, 0.37747, 0.36431, 0.35193, 0.33457, 0.32013)
reward_matrix_3 = c(0.37146, 0.29856, 0.30642, 0.2707, 0.25356, 0.25763, 0.1921, 0.20666, 0.19305, 0.19291, 0.18298, 0.18156, 0.19481, 0.15628, 0.18232, 0.21027, 0.20945, 0.22949, 0.20882, 0.23431, 0.24699, 0.19052, 0.2206, 0.21146, 0.19117, 0.21937, 0.23521, 0.27283, 0.33207, 0.33282, 0.3137, 0.32797, 0.33243, 0.33035, 0.37739, 0.38959, 0.39165, 0.43298, 0.4753, 0.45643, 0.46237, 0.46749, 0.47418, 0.47148, 0.46565, 0.48012, 0.51917, 0.56677, 0.551, 0.52118, 0.5035, 0.51614, 0.48258, 0.4852, 0.51321, 0.5236, 0.54053, 0.58072, 0.58066, 0.60732, 0.60788, 0.62424, 0.61417, 0.61129, 0.61249, 0.63833, 0.6153, 0.64763, 0.66878, 0.65734, 0.66195, 0.66565, 0.68452, 0.66885, 0.6788, 0.69809, 0.6875, 0.69827, 0.6978, 0.72358, 0.7295, 0.74277, 0.73044, 0.74494, 0.74108, 0.74222, 0.74686, 0.80182, 0.80182, 0.82339, 0.81468, 0.81121, 0.84829, 0.83158, 0.83158, 0.80201, 0.74137, 0.72785, 0.73865, 0.76278)
reward_matrix=data.frame(reward_matrix_0,reward_matrix_1,reward_matrix_2,reward_matrix_3)
reward_matrix=reward_matrix[c(1:50),]

for (sub in unique(data_cards$subj)){
  for (block_num in 0:3) {
    for (trial in data_cards$trial_num[data_cards$subj==sub&data_cards$block==block_num]){
      card_left_1=as.integer(data_cards[data_cards$subj==sub&data_cards$block==block_num&data_cards$trial_num==trial,"card_left1"])
      card_right_1=as.integer(data_cards[data_cards$subj==sub&data_cards$block==block_num&data_cards$trial_num==trial,"card_right1"])
      card_left_2=as.integer(data_cards[data_cards$subj==sub&data_cards$block==block_num&data_cards$trial_num==trial,"card_left2"])
      card_right_2=as.integer(data_cards[data_cards$subj==sub&data_cards$block==block_num&data_cards$trial_num==trial,"card_right2"])
      data_cards[data_cards$subj==sub&data_cards$block==block_num&data_cards$trial_num==trial,"left_prob_1"]=reward_matrix[trial,card_left_1-4*(block_num+1)]
      data_cards[data_cards$subj==sub&data_cards$block==block_num&data_cards$trial_num==trial,"right_prob_1"]=reward_matrix[trial,card_right_1-4*(block_num+1)]
      data_cards[data_cards$subj==sub&data_cards$block==block_num&data_cards$trial_num==trial,"left_prob_2"]=reward_matrix[trial,card_left_2-4*(block_num+1)]
      data_cards[data_cards$subj==sub&data_cards$block==block_num&data_cards$trial_num==trial,"right_prob_2"]=reward_matrix[trial,card_right_2-4*(block_num+1)]
    }
  }
}
#add 'better decision'
for (trial in 1:nrow(data_cards)){
  if (data_cards$left_prob_1[trial]>data_cards$right_prob_1[trial]){
    data_cards$better_card1[trial]=data_cards$card_left1[trial]
  }
  else{
    data_cards$better_card1[trial]=data_cards$card_right1[trial]
  }
  if (data_cards$left_prob_2[trial]>data_cards$right_prob_2[trial]){
    data_cards$better_card2[trial]=data_cards$card_left2[trial]
  }
  else{
    data_cards$better_card2[trial]=data_cards$card_right2[trial]
  }
}
#add is_better_choice
data_cards$is_better_card1=data_cards$card_selected1==data_cards$better_card1
data_cards$is_better_card2=data_cards$card_selected2==data_cards$better_card2
#organize columns - with avg_capacity from analyze_data
#data_cards=data_cards[c("subj","block","trial_num","trial_name1","rt1","card_left1","card_right1","left_prob_1","right_prob_1","better_card1","key_press1","card_selected1","is_better_card1","reward1","trial_name2","rt2","card_left2","card_right2","left_prob_2","right_prob_2","better_card2","key_press2","card_selected2","is_better_card2","reward2","stay_key_1_2","rt_squares","key_press_squares","set_size","condition","is_correct","avg_capacity","capacity_high_low")]
#remove incorrect_squares trials from cards data
data_correct=data_cards[data_cards$is_correct==T,]
data_correct=data_cards[data_cards$is_correct==T,]

#add belief in instructions
questions=questions[!(questions$subj %in% df_exclude$subj),c("subj","response")]
questions=questions[!(questions$subj %in% df_repeat$subj),]

#divide dfs to every experiment
#data_cards1=data_cards[data_cards$experiment==1,]
#data_cards2=data_cards[data_cards$experiment==2,]
#data_squares_combined1=data_squares_combined[data_squares_combined$experiment==1,]
#data_squares_combined2=data_squares_combined[data_squares_combined$experiment==2,]
#data_squares1=data_squares[data_squares$experiment==1,]
#data_squares2=data_squares[data_squares$experiment==2,]

#binning the rt
mybinning<-function(.data,var,Nquantiles){
  #this functions is designed for dyplr piping
  #inputs include data.frame (you don't to include that when piping)
  #var to be binned and the number of binns
  x=unlist(.data%>%select({{var}})) #vector to be binned
  y=rep(0,length(x))               #preallocate the vector that wil include the binns
  myquantiles=quantile(x,seq(0,1,by=1/Nquantiles)) 
  for (i in 1:(Nquantiles-1)){
    y=y+(x>=myquantiles[i] & x<myquantiles[i+1])*1*i
  }
  y[y==0]=Nquantiles
  return(as.numeric(y))
}
data_cards$rt1_binned=data_cards%>%mybinning(rt1,5)
data_cards$rt2_binned=data_cards%>%mybinning(rt2,5)
data_cards$rt_squares_binned=data_cards%>%mybinning(rt_squares,5)

### squares accuracy correlations
data_8_4=data_squares%>%group_by(subj,set_size)%>%summarise(acc=mean(unlist(is_correct)))
data_8_4=data_8_4%>%pivot_wider(names_from="set_size",values_from="acc")
colnames(data_8_4)[2]='set_size_4'
colnames(data_8_4)[3]='set_size_8'

data_8_4$capacity_8=8*(data_8_4$set_size_8*2-1)
data_8_4$capacity_4=4*(data_8_4$set_size_4*2-1)

### doing the same for the squares in the combined task
data_4_1=data_squares_combined%>%group_by(subj,set_size)%>%summarise(acc=mean(unlist(is_correct)))
data_4_1=data_4_1%>%pivot_wider(names_from="set_size",values_from="acc")
colnames(data_4_1)[2]='set_size_1'
colnames(data_4_1)[3]='set_size_4_comb'
#combining the accuracy to one df
squares_all=cbind(data_8_4,set_size_1=data_4_1$set_size_1,set_size_4_comb=data_4_1$set_size_4_comb)
squares_all$avg_capacity=(squares_all$capacity_8+squares_all$capacity_4)/2
save(squares_all,file='myfolder/03_data/02_aggregated_data/squares_all.Rdata')
#add capacity to each participant
for (ind in unique(data_cards$subj)){
  data_cards[data_cards$subj==ind,"avg_capacity"]=squares_all[squares_all$subj==ind,"avg_capacity"]
}
#add avg_capacity as a categorical variable
data_cards$'capacity_high_low'=data_cards$avg_capacity>median(unlist(squares_all$avg_capacity))
data_cards <- data_cards %>%
  mutate(capacity_high_low = factor(capacity_high_low),
         capacity_high_low = if_else(capacity_high_low == T, "High", "Low"),
         capacity_high_low = factor(capacity_high_low, levels = c("High", "Low")))
#save dfs
save(data_cards,file='myfolder/03_data/02_aggregated_data/data_cards.Rdata')
save(data_squares_combined,file='myfolder/03_data/02_aggregated_data/data_squares_combined.Rdata')
save(data_squares,file='myfolder/03_data/02_aggregated_data/data_squares.Rdata')
