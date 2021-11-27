library(tidyverse)
library(tidylog)
library(stringr)
###
#How the dfs will look
###
#cards:sub_num,block,trial,rt,set_size,left_card,right_card,card_selected,key_selected,reward,is_correct
#cards.squares:sub_num,block,trial,rt,is_correct,set_size,colors,locations
#cards-> 1:cards.practice, 2:memory.practice, 3:cards.test, 4:memory.test

###
#change nulls to NA
cards[[1]]$rt[sapply(cards[[1]]$rt, is.null)] <- NA
cards[[1]]$key_press[sapply(cards[[1]]$key_press, is.null)] <- NA

cards[[2]]$rt[sapply(cards[[2]]$rt, is.null)] <- NA
cards[[2]]$key_press[sapply(cards[[2]]$key_press, is.null)] <- NA

###
#cards.practice 
#The first 12 lines for each subject are the 'only_cards practice' and are considered as one long trial.
#We fix this and divide the practice trial to two seperate dfs
###
clean_cards_practice=cards[[1]]%>%select(c(1,2,3,4,6,7,22)) #take only relevant columns
number_of_subj=length(unique(clean_cards_practice$subj))
fixed_trial_nums=rep(list(0,0,1,1,2,2,3,3,4,4,5,5),2)
fixed_trial_names=c(rep(c('only_cards1','only_cards2'),6),rep(c('cards1','cards2'),6))
for (ind in 1:number_of_subj){ #I need to check how many times they replicated the practice part
  practice_sessions_of_subj=sum(unlist(clean_cards_practice$trial_num[clean_cards_practice$subj==ind])%%6==1)/2 #this allows me to check how many times they repeated the practice.
  #enter the fixed trial num and name.
  numrows_to_update=24*practice_sessions_of_subj
  clean_cards_practice$trial_num[clean_cards_practice$subj==ind][1:numrows_to_update]=rep(fixed_trial_nums,practice_sessions_of_subj)
  clean_cards_practice$trial_name[clean_cards_practice$subj==ind][1:numrows_to_update]=rep(fixed_trial_names,practice_sessions_of_subj)
}

rownames(clean_cards_practice)=1:nrow(clean_cards_practice)
###
#change key_press from number to character
###
clean_cards_practice$key_press[unlist(clean_cards_practice$key_press)==75]='k'
clean_cards_practice$key_press[unlist(clean_cards_practice$key_press)==83]='s'
###
#get left and right cards
###

get_cards=function(df){
  card_left_list=list()
  card_right_list=list()
  patterns <- c("card deck") 
  string <- df$stimulus #could be stimulus 1 or 2
  matches <- str_locate_all(string, patterns)
  for (ind in 1:dim(df)[1]){
    all <- do.call(rbind, matches[ind])
    stop <- all[, 2]
    card_left_list[ind]=substr(string[ind],stop[1]+4,stop[1]+4) 
    card_right_list[ind]=substr(string[ind],stop[2]+4,stop[2]+4) 

  }
  df$card_left=card_left_list
  df$card_right=card_right_list
  return (df)
}
clean_cards_practice=get_cards(clean_cards_practice)
###
#put every trial in one line cards1...cards2
###
rownames(clean_cards_practice)=1:nrow(clean_cards_practice)
### divide the data to two types of practice
clean_only_cards_practice=clean_cards_practice[(clean_cards_practice$trial_name=='only_cards1'|clean_cards_practice$trial_name=='only_cards2'),]
clean_combined_cards_practice=clean_cards_practice[(clean_cards_practice$trial_name=='cards1'|clean_cards_practice$trial_name=='cards2'),]
rownames(clean_only_cards_practice)=1:nrow(clean_only_cards_practice)
rownames(clean_combined_cards_practice)=1:nrow(clean_combined_cards_practice)
### organize two lines into one line
clean_only_cards1_practice=clean_only_cards_practice[clean_only_cards_practice$trial_name=='only_cards1',]
clean_only_cards2_practice=clean_only_cards_practice[clean_only_cards_practice$trial_name=='only_cards2',]
clean_only_cards1_practice=clean_only_cards1_practice%>%select(-c(3)) #remove stimulus column

#change column names
colnames(clean_only_cards1_practice)[c(2,3,4,6,7,8)]=c('rt1','key_press1','trial_name1','reward1','card_left1','card_right1') 
#remove subj, stimulus and trial_num from mini-trial2
clean_only_cards2_practice=clean_only_cards2_practice%>%select(-c(1,3,6)) 
#change column names
colnames(clean_only_cards2_practice)[c(1,2,3,4,5,6)]=c('rt2','key_press2','trial_name2','reward2','card_left2','card_right2')
#combine dfs
clean_only_cards_practice=cbind(clean_only_cards1_practice,clean_only_cards2_practice)
clean_only_cards_practice=clean_only_cards_practice[c("subj","trial_num","trial_name1","rt1","card_left1","card_right1","key_press1","reward1","trial_name2","rt2","card_left2","card_right2","key_press2","reward2")]
rownames(clean_only_cards_practice)=1:nrow(clean_only_cards_practice)

clean_combined_cards1_practice=clean_combined_cards_practice[clean_combined_cards_practice$trial_name=='cards1',]
clean_combined_cards2_practice=clean_combined_cards_practice[clean_combined_cards_practice$trial_name=='cards2',]
#remove redundant and change column names
clean_combined_cards1_practice=clean_combined_cards1_practice%>%select(-c(3)) #remove stimulus column
colnames(clean_combined_cards1_practice)[c(2,3,4,6,7,8)]=c('rt1','key_press1','trial_name1','reward1','card_left1','card_right1')
clean_combined_cards2_practice=clean_combined_cards2_practice%>%select(-c(1,3,6)) #remove subj, stimulus and trial_num
colnames(clean_combined_cards2_practice)[c(1,2,3,4,5,6)]=c('rt2','key_press2','trial_name2','reward2','card_left2','card_right2')

clean_combined_cards_practice=cbind(clean_combined_cards1_practice,clean_combined_cards2_practice)
clean_combined_cards_practice=clean_combined_cards_practice[c("subj","trial_num","trial_name1","rt1","card_left1","card_right1","key_press1","reward1","trial_name2","rt2","card_left2","card_right2","key_press2","reward2")]
rownames(clean_combined_cards_practice)=1:nrow(clean_combined_cards_practice)
###
#add squares
###
clean_combined_squares_practice=cards[[2]]%>%select(1,2,3,4,6,7,8,13,14)

clean_combined_squares_practice$key_press[unlist(clean_combined_squares_practice$key_press)==75]='k'
clean_combined_squares_practice$key_press[unlist(clean_combined_squares_practice$key_press)==83]='s'
clean_combined_squares_practice$correct_response[unlist(clean_combined_squares_practice$correct_response)==75]='k'
clean_combined_squares_practice$correct_response[unlist(clean_combined_squares_practice$correct_response)==83]='s'
###

###
#memory and test side by side
###
clean_test_combined_squares_practice=clean_combined_squares_practice[clean_combined_squares_practice$trial_name=='test_cards',]
clean_memory_combined_squares_practice=clean_combined_squares_practice[clean_combined_squares_practice$trial_name=='memory_cards',]
clean_memory_combined_squares_practice=clean_memory_combined_squares_practice%>%select(c(3))
colnames(clean_memory_combined_squares_practice)[1]='stimuli'
clean_combined_squares_practice=cbind(clean_test_combined_squares_practice,clean_memory_combined_squares_practice)
#get locations and colors
####
get_locations=function(df,loc_string){
  location_list=list()
  patterns <- c(loc_string) #"class='"
  string <- df$stimuli
  matches <- str_locate_all(string, patterns)
  for (ind in 1:dim(df)[1]){
    all <- do.call(rbind, matches[ind])
    stop <- all[, 2]
    for (location in 1:length(stop)){
      if(df$set_size[ind]==1) {
        location_list[location]=substr(string[ind],stop[location]+1,stop[location]+6) #get memory locations
        df$target_location[ind]=location_list[location] #target location is memory location when set size is 1
      }
      else{
        location_list[location]=substr(string[ind],stop[location]+1,stop[location]+1) #get memory locations
        df$target_location[ind]=substr(df$stimulus[ind], 13, 13) #target location from stimulus
      }
    }
    location_list=toString(location_list)
    df$memory_locations[ind]=location_list
  }
  return (df)
}
get_stimuli=function(df,stim_string){
  stimuli_list=list()
  patterns <- c(stim_string) #'squares/'
  string <- df$stimuli
  matches <- str_locate_all(string, patterns)
  for (ind in 1:dim(df)[1]){
    all <- do.call(rbind, matches[ind])
    stop <- all[, 2]
    for (stimulus in 1:length(stop)){
      stimuli_list[stimulus]=substr(string[ind],stop[stimulus]+1,stop[stimulus]+3) #get memory colors
    }
    if(df$set_size[ind]==1) {
      df$target_color[ind]=stimuli_list[stimulus] #target color is memory color when set size is 1
    }
    else{
      df$target_color[ind]=substr(df$stimulus[ind],  35, 37) #target location from stimulus
    }
    stimuli_list=toString(stimuli_list)
    df$memory_colors[ind]=stimuli_list
  }
  return (df)
}
clean_combined_squares_practice=get_locations(clean_combined_squares_practice,"class='")
clean_combined_squares_practice=get_stimuli(clean_combined_squares_practice,"squares/")
clean_combined_squares_practice=clean_combined_squares_practice%>%select(-c(3,10))
clean_combined_squares_practice=clean_combined_squares_practice[,c("subj","trial_name","trial_num","rt","key_press","correct_response","is_correct","set_size","target_location","memory_locations","target_color","memory_colors"),with=F]
###
#add set size and is_correct in squares to cards df
clean_combined_cards_practice$set_size=clean_combined_squares_practice$set_size
clean_combined_cards_practice$is_correct=clean_combined_squares_practice$is_correct
clean_combined_cards_practice$key_press_squares=clean_combined_squares_practice$key_press
clean_combined_cards_practice$stay_1_2=unlist(clean_combined_cards_practice$key_press1)==unlist(clean_combined_cards_practice$key_press2)
###

###
#clean cards_exp
#
###

#change nulls to NA
cards[[3]]$rt[sapply(cards[[3]]$rt, is.null)] <- NA
cards[[3]]$key_press[sapply(cards[[3]]$key_press, is.null)] <- NA

cards[[4]]$rt[sapply(cards[[4]]$rt, is.null)] <- NA
cards[[4]]$key_press[sapply(cards[[4]]$key_press, is.null)] <- NA
###
clean_combined_cards_exp=cards[[3]]%>%select(c(1,2,3,4,6,7,18,19,20,25,27)) #take only relevant columns

#change 75/83 to k/s
clean_combined_cards_exp$key_press[unlist(clean_combined_cards_exp$key_press)==75]='k'
clean_combined_cards_exp$key_press[unlist(clean_combined_cards_exp$key_press)==83]='s'

#get cards from stimulus

get_cards_exp=function(df){
  card_left_list=list()
  card_right_list=list()
  patterns <- c("card deck") 
  string <- df$stimulus #could be stimulus 1 or 2
  matches <- str_locate_all(string, patterns)
  for (ind in 1:dim(df)[1]){
    all <- do.call(rbind, matches[ind])
    stop <- all[, 2]
    if (df$block[ind]==0){ #fixing the difference between blocks
    card_left_list[ind]=substr(string[ind],stop[1]+4,stop[1]+4) 
    card_right_list[ind]=substr(string[ind],stop[2]+4,stop[2]+4) 
    }
    else{
      card_left_list[ind]=substr(string[ind],stop[1]+4,stop[1]+5) 
      card_right_list[ind]=substr(string[ind],stop[2]+4,stop[2]+5) 
      card_left_list[card_left_list=='9.']='9' #fixing the 9 problem
      card_right_list[card_right_list=='9.']='9'
    }
  }
  df$card_left=card_left_list
  df$card_right=card_right_list
  return (df)
}
clean_combined_cards_exp=get_cards_exp(clean_combined_cards_exp)
#change card selected from 1-4 to 5-8/9-12 and so on, according to the card deck in the block.
clean_combined_cards_exp$card_selected=clean_combined_cards_exp$card_selected+((clean_combined_cards_exp$block+1)*4+1)
#change to wide format 'cards1...cards2' every trial in every row
clean_combined_cards1_exp=clean_combined_cards_exp[clean_combined_cards_exp$trial_name=='cards1',]
clean_combined_cards2_exp=clean_combined_cards_exp[clean_combined_cards_exp$trial_name=='cards2',]
#remove redundent columns and change column names
clean_combined_cards1_exp=clean_combined_cards1_exp%>%select(-c(3)) #remove stimulus column
colnames(clean_combined_cards1_exp)[c(2,3,4,9,10,11,12)]=c('rt1','key_press1','trial_name1','reward1','card_selected1','card_left1','card_right1')
clean_combined_cards2_exp=clean_combined_cards2_exp%>%select(-c(1,3,6,7,8,9)) #remove subj,trial_num,block,condition,set_size columns
colnames(clean_combined_cards2_exp)[c(1,2,3,4,5,6,7)]=c('rt2','key_press2','trial_name2','reward2','card_selected2','card_left2','card_right2')
clean_combined_cards_exp=cbind(clean_combined_cards1_exp,clean_combined_cards2_exp)
#organize columns
clean_combined_cards_exp=clean_combined_cards_exp[c("subj","block","trial_num","trial_name1","rt1","card_left1","card_right1","key_press1","card_selected1","reward1","trial_name2","rt2","card_left2","card_right2","key_press2","card_selected2","reward2","set_size","condition")]
#change condition to same and different
clean_combined_cards_exp$condition[unlist(clean_combined_cards_exp$condition)=='s']='same'
clean_combined_cards_exp$condition[unlist(clean_combined_cards_exp$condition)=='d']='different'
###
#clean squares part of combined task-exp
###
clean_combined_squares_exp=cards[[4]]%>%select(c(1,2,3,4,6,7,8,13,14,15,16,17)) #take only relevant columns
#memory and test parts in wide format
clean_combined_squares_exp_memory=clean_combined_squares_exp[clean_combined_squares_exp$trial_name=='memory_cards',]
colnames(clean_combined_squares_exp_memory)[3]='stimuli' #memory_array stimuli
clean_combined_squares_exp_memory=clean_combined_squares_exp_memory%>%select(c(3))
clean_combined_squares_exp_test=clean_combined_squares_exp[clean_combined_squares_exp$trial_name=='test_cards',]
clean_combined_squares_exp=cbind(clean_combined_squares_exp_test,clean_combined_squares_exp_memory)
#get locations and colors
clean_combined_squares_exp=get_locations(clean_combined_squares_exp,"class='")
clean_combined_squares_exp=get_stimuli(clean_combined_squares_exp,"squares/")
clean_combined_squares_exp=clean_combined_squares_exp%>%select(-c(3,13)) #remove stimulus and stimuli
#change 75/83 to k/s
clean_combined_squares_exp$key_press[unlist(clean_combined_squares_exp$key_press)==75]='k'
clean_combined_squares_exp$key_press[unlist(clean_combined_squares_exp$key_press)==83]='s'
clean_combined_squares_exp$correct_response[unlist(clean_combined_squares_exp$correct_response)==75]='k'
clean_combined_squares_exp$correct_response[unlist(clean_combined_squares_exp$correct_response)==83]='s'
#change condition to same and different
clean_combined_squares_exp$condition[unlist(clean_combined_squares_exp$condition)=='s']='same'
clean_combined_squares_exp$condition[unlist(clean_combined_squares_exp$condition)=='d']='different'
#organize columns
clean_combined_squares_exp=clean_combined_squares_exp[,c("subj","block","trial_num","trial_name","rt","mapping","key_press","correct_response","condition","is_correct","set_size","target_location","memory_locations","target_color","memory_colors"),with=F]
#interpretable column name
colnames(clean_combined_squares_exp)[6]="mapping:left=same;right=different"
#add data to clean_combined_cards_exp
clean_combined_cards_exp$is_correct=clean_combined_squares_exp$is_correct
clean_combined_cards_exp$stay_1_2=unlist(clean_combined_cards_exp$key_press1)==unlist(clean_combined_cards_exp$key_press2)
#save data
save(clean_only_cards_practice,file='myfolder/03_data/02_aggregated_data/practice_only_cards_agg.Rdata')
save(clean_combined_squares_practice,file='myfolder/03_data/02_aggregated_data/practice_combined_squares_agg.Rdata')
save(clean_combined_cards_practice,file='myfolder/03_data/02_aggregated_data/practice_combined_cards_agg.Rdata')
save(clean_combined_squares_exp,file='myfolder/03_data/02_aggregated_data/exp_combined_squares_agg.Rdata')
save(clean_combined_cards_exp,file='myfolder/03_data/02_aggregated_data/exp_combined_cards_agg.Rdata')
