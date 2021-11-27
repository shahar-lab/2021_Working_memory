library(tidyverse)
library(tidylog)
library(stringr)
###
#How the dfs will look
###
#squares:sub_num,trial,rt,mapping,condition,key_press,correct_response,is_correct,set_size,colors(target and memory),locations(target and memory)
#### Clean squares dfs ####
###
#change nulls to NA
squares[[1]]$rt[sapply(squares[[1]]$rt, is.null)] <- NA
squares[[1]]$key_press[sapply(squares[[1]]$key_press, is.null)] <- NA
squares[[2]]$rt[sapply(squares[[2]]$rt, is.null)] <- NA
squares[[2]]$key_press[sapply(squares[[2]]$key_press, is.null)] <- NA
###
colnames(squares[[1]])[16]="stimuli" #change stimulus to stimuli for the memory array
colnames(squares[[2]])[18]="stimuli" 
squares[[1]]$key_press[unlist(squares[[1]]$key_press)==75]='k'
squares[[1]]$key_press[unlist(squares[[1]]$key_press)==83]='s'
squares[[2]]$key_press[unlist(squares[[2]]$key_press)==75]='k'
squares[[2]]$key_press[unlist(squares[[2]]$key_press)==83]='s'
squares[[1]]$correct_response[unlist(squares[[1]]$correct_response)==75]='k'
squares[[1]]$correct_response[unlist(squares[[1]]$correct_response)==83]='s'
squares[[2]]$correct_response[unlist(squares[[2]]$correct_response)==75]='k'
squares[[2]]$correct_response[unlist(squares[[2]]$correct_response)==83]='s'
clean_squares_practice=squares[[1]]%>%select(c(1,2,3,4,7,8,9,14,16)) #we have replicated names so I use numeric index
clean_squares_exp=squares[[2]]%>%select(c(1,2,3,4,7,8,9,10,11,16,18))
####
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

clean_squares_exp=get_locations(clean_squares_exp,"class='")
clean_squares_exp=get_stimuli(clean_squares_exp,"squares/")
clean_squares_practice=get_locations(clean_squares_practice,"class='")
clean_squares_practice=get_stimuli(clean_squares_practice,"squares/")
clean_squares_exp=clean_squares_exp%>%select(-c("stimulus","stimuli"))
clean_squares_practice=clean_squares_practice%>%select(-c("stimulus","stimuli"))
#organize columns
clean_squares_practice=clean_squares_practice[c("subj","trial_num","rt","key_press","correct_response","is_correct","set_size","target_location","memory_locations","target_color","memory_colors")]
clean_squares_exp=clean_squares_exp[c("subj","trial_num","rt","mapping","key_press","correct_response","is_correct","condition","set_size","target_location","memory_locations","target_color","memory_colors")]
#interpretable column name
colnames(clean_squares_exp)[4]="mapping:left=same;right=different"
#change condition to same and different
clean_squares_exp$condition[unlist(clean_squares_exp$condition)=='s']='same'
clean_squares_exp$condition[unlist(clean_squares_exp$condition)=='d']='different'
save(clean_squares_practice,file='myfolder/03_data/02_aggregated_data/practice_only_squares_agg.Rdata')
save(clean_squares_exp,file='myfolder/03_data/02_aggregated_data/exp_only_squares_agg.Rdata')
