rm(list=ls())
# pull data from json -----------------------------------------------------

source('myfolder/01_functions/convert_json_screen_size.R')
source('myfolder/01_functions/convert_json_questions.R')
source('myfolder/01_functions/convert_json_squares.R')
source('myfolder/01_functions/convert_json_cards.R')
source('myfolder/01_functions/convert_json_icar.R')
source('myfolder/01_functions/convert_json_self_reports.R')

mainfolder<-paste(getwd(),'/myfolder/03_data/01_raw_data/exp2_07.2021',sep="")
subfolder <-dir(mainfolder)
squares<-list(data.frame(),data.frame()) #this will hold two df, squares.practice,sqaures.test
cards2<-list(data.frame(),data.frame(),data.frame(),data.frame()) #this will hold four df, cards2.practice, memory.practice, cards2.test, memory.test
cards3<-list(data.frame(),data.frame(),data.frame(),data.frame()) #this will hold four df, cards3.practice, memory.practice, cards3.test, memory.test
print(length(subfolder))

for (i in 1:length(subfolder)){
  print(paste('folder #',i,': ',subfolder[i],sep=""))
  files<-dir(curnfolder<-paste(mainfolder,'/',subfolder[i],sep="", row.names=NULL))
  #screensize <-con_screen_size_json('screen_size',screensize,curnfolder,files,i)
  #questions  <-con_questions_json('questionnaires',questions,curnfolder,files,i)
  squares    <-con_squares_json('squares',squares[[1]],squares[[2]],curnfolder,files,i)
  cards2      <-con_cards_json('cards2',cards2[[1]],cards2[[2]],cards2[[3]],cards2[[4]],curnfolder,files,i)
  cards3      <-con_cards_json('cards3',cards3[[1]],cards3[[2]],cards3[[3]],cards3[[4]],curnfolder,files,i)
}  



# working memory task -----------------------------------------------------
library(tidyverse)

#replace NULL values with NA
df1<-lapply(squares[[1]], function(i) {unlist(na_if(i, 'NULL'))}) #practice trials
df2<-lapply(squares[[2]], function(i) {unlist(na_if(i, 'NULL'))}) #test trials

#merge to one data.frame
#df1$condition<-df1$mapping<-NA
#df<-bind_rows(as.data.frame(df1),as.data.frame(df2))

#find locations and colors of the memory array
memory_location=sapply(1:length(df2[[18]]), function(i) {str_sub(df2[[18]][i],str_locate_all(df2[[18]][i],'<img')[[1]][,1]+12,str_locate_all(df2[[18]][i],'<img')[[1]][,1]+12)})
memory_colors  =sapply(1:length(df2[[18]]), function(i) {str_sub(df2[[18]][i],str_locate_all(df2[[18]][i],'<img')[[1]][,1]+34,str_locate_all(df2[[18]][i],'<img')[[1]][,1]+36)})

#make data.frame
squares=data.frame(subj       =df2$subj,
              mapping         =(df2$mapping=='ks')*1, #left letter is for ...
              trial           =df2$trial_num,
              set_size        =as.numeric(df2$set_size),
              memory_location =as.matrix(memory_location),
              memory_color    =as.matrix(memory_colors),
              target_location =str_sub(df2$stimulus,13,13),
              target_color    =str_sub(df2$stimulus,35,37),
              condition       =factor((df2$condition=='s')*1,levels=c(0,1),labels=c('different','same')),
              ch_key          =(df2$key_press==83)*1, #key press 's' is 1,key press 'k' is 0
              correct_response=(df2$correct_response==83)*1,
              rt              =df2$rt/1000,
              acc             =df2$is_correct*1)


# cards2 trials in the dual task-----------------------------------------------------------------

#replace NULL values with NA
df1<-lapply(cards2[[1]], function(i) {unlist(na_if(i, 'NULL'))}) #practice trials
df2<-lapply(cards2[[2]], function(i) {unlist(na_if(i, 'NULL'))}) #practice trials
df3<-lapply(cards2[[3]], function(i) {unlist(na_if(i, 'NULL'))}) #test trials cards data
df4<-lapply(cards2[[4]], function(i) {unlist(na_if(i, 'NULL'))}) #test trials memory data

#code cards location in each trial
card_left =sapply(1:length(df3$stimulus), function(i) {as.numeric(str_sub(df3$stimulus[i],str_locate_all(df3$stimulus[i],'<img')[[1]][1,1]+47,str_locate_all(df3$stimulus[i],'<img')[[1]][1,1]+48))})
card_right=sapply(1:length(df3$stimulus), function(i) {as.numeric(str_sub(df3$stimulus[i],str_locate_all(df3$stimulus[i],'<img')[[1]][2,2]+45,str_locate_all(df3$stimulus[i],'<img')[[1]][2,2]+46))})

#make data.frame for cards
cards2=data.frame(subj     =df3$subj,
              block    =df3$block+1,
              trial    =df3$trial_num,
              set_size =as.numeric(df3$set_size),
              condition=factor((df3$condition=='s')*1,levels=c(0,1),labels=c('different','same')),
              subtrial =as.numeric(str_sub(df3$trial_name,6,6)),
              card_left=card_left-((df3$block+1)*4+1),
              card_right=card_right-((df3$block+1)*4+1),
              ch_key   =(df3$key_press==83)*1,
              rt       =df3$rt/1000,
              ch_card  =df3$card_selected,
              reward   =df3$reward)

# memory2 trials in the dual task -----------------------------------------------------------------

#prepare index
#(the data has two subtrials for memory and test, we want to merage them into one since most information only appears in the test subtrial)
subtrial =as.numeric(str_sub(df3$trial_name,6,6))
ii=(subtrial==2)

#find locations and colors of the memory array
#(note that this will hold the memory array and the test stim interleaved between subtrials)
memory_location=sapply(1:length(df4$stimulus), function(i) {if (df4$set_size[i]==1){j=12+5}else{j=12}
  str_sub(df4$stimulus[i],str_locate_all(df4$stimulus[i],'<img')[[1]][,1]+12,str_locate_all(df4$stimulus[i],'<img')[[1]][,1]+j)})
memory_colors  =sapply(1:length(df4$stimulus), function(i) {str_sub(df4$stimulus[i],str_locate_all(df4$stimulus[i],'/squares/')[[1]][,1]+9,str_locate_all(df4$stimulus[i],'/squares/')[[1]][,1]+11)})

target_location=as.matrix(memory_location)[ii]
target_color   =as.matrix(memory_colors)[ii]
memory_location=as.matrix(memory_location)[!ii]
memory_color   =as.matrix(memory_colors)[!ii]

memory2=data.frame(subj     =df4$subj[ii],
                  mapping  =((df4$mapping=='ks')*1)[ii],
                  block    =(df4$block+1)[ii],
                  trial    =df4$trial_num[ii],
                  set_size =as.numeric(df4$set_size)[ii],
                  memory_location=as.matrix(memory_location),
                  memory_color   =as.matrix(memory_color),
                  target_location=as.matrix(target_location),
                  target_color   =as.matrix(target_color),
                  condition=(factor((df4$condition=='s')*1,levels=c(0,1),labels=c('different','same')))[ii],
                  ch_key   =((df4$key_press==83)*1)[ii],
                  rt       =(df4$rt/1000)[ii],
                  correct_response=((df4$correct_response==83)*1)[ii],
                  acc      =(df4$is_correct*1)[ii])

# cards3 trials in the dual task-----------------------------------------------------------------

#replace NULL values with NA
df1<-lapply(cards3[[1]], function(i) {unlist(na_if(i, 'NULL'))}) #practice trials
df2<-lapply(cards3[[2]], function(i) {unlist(na_if(i, 'NULL'))}) #practice trials
df3<-lapply(cards3[[3]], function(i) {unlist(na_if(i, 'NULL'))}) #test trials cards data
df4<-lapply(cards3[[4]], function(i) {unlist(na_if(i, 'NULL'))}) #test trials memory data

#code cards location in each trial
card_left =sapply(1:length(df3$stimulus), function(i) {as.numeric(str_sub(df3$stimulus[i],str_locate_all(df3$stimulus[i],'<img')[[1]][1,1]+47,str_locate_all(df3$stimulus[i],'<img')[[1]][1,1]+48))})
card_right=sapply(1:length(df3$stimulus), function(i) {as.numeric(str_sub(df3$stimulus[i],str_locate_all(df3$stimulus[i],'<img')[[1]][2,2]+45,str_locate_all(df3$stimulus[i],'<img')[[1]][2,2]+46))})

#make data.frame for cards
cards3=data.frame(subj     =df3$subj,
                  block    =df3$block+1,
                  trial    =df3$trial_num,
                  set_size =as.numeric(df3$set_size),
                  condition=factor((df3$condition=='s')*1,levels=c(0,1),labels=c('different','same')),
                  subtrial =as.numeric(str_sub(df3$trial_name,6,6)),
                  card_left=card_left-((df3$block+1)*4+1),
                  card_right=card_right-((df3$block+1)*4+1),
                  ch_key   =(df3$key_press==83)*1,
                  rt       =df3$rt/1000,
                  ch_card  =df3$card_selected,
                  reward   =df3$reward)


# memory3 trials in the dual task -----------------------------------------------------------------

#prepare index
#(the data has two subtrials for memory and test, we want to merge them into one since most information only appears in the test subtrial)
subtrial =as.numeric(str_sub(df3$trial_name,6,6))
ii=(subtrial==2)

#find locations and colors of the memory array
#(note that this will hold the memory array and the test stim interleaved between subtrials)
memory_location=sapply(1:length(df4$stimulus), function(i) {if (df4$set_size[i]==1){j=12+5}else{j=12}
  str_sub(df4$stimulus[i],str_locate_all(df4$stimulus[i],'<img')[[1]][,1]+12,str_locate_all(df4$stimulus[i],'<img')[[1]][,1]+j)})
memory_colors  =sapply(1:length(df4$stimulus), function(i) {str_sub(df4$stimulus[i],str_locate_all(df4$stimulus[i],'/squares/')[[1]][,1]+9,str_locate_all(df4$stimulus[i],'/squares/')[[1]][,1]+11)})

target_location=as.matrix(memory_location)[ii]
target_color   =as.matrix(memory_colors)[ii]
memory_location=as.matrix(memory_location)[!ii]
memory_color   =as.matrix(memory_colors)[!ii]

memory3=data.frame(subj     =df4$subj[ii],
                  mapping  =((df4$mapping=='ks')*1)[ii],
                  block    =(df4$block+1)[ii],
                  trial    =df4$trial_num[ii],
                  set_size =as.numeric(df4$set_size)[ii],
                  memory_location=as.matrix(memory_location),
                  memory_color   =as.matrix(memory_color),
                  target_location=as.matrix(target_location),
                  target_color   =as.matrix(target_color),
                  condition=(factor((df4$condition=='s')*1,levels=c(0,1),labels=c('different','same')))[ii],
                  ch_key   =((df4$key_press==83)*1)[ii],
                  rt       =(df4$rt/1000)[ii],
                  correct_response=((df4$correct_response==83)*1)[ii],
                  acc      =(df4$is_correct*1)[ii])

#### icar ----------------------------------------------------
subfolder=dir(mainfolder)
icar<-data.frame()
detach("package:RJSONIO", unload = TRUE)
library(rjson)
for (i in 1:length(subfolder)){
  print(paste('ICAR-folder #',i,': ',subfolder[i],sep=""))
  files<-dir(curnfolder<-paste(mainfolder,'/',subfolder[i],sep="", row.names=NULL))
  icar <-con_icar_json('icar',icar,curnfolder,files,i)
} 


####self-report ----------------------------------------------------
subfolder=dir(mainfolder)
oci <-data.frame()
detach("package:rjson", unload = TRUE)
for (i in 1:length(subfolder)){
  print(paste('OCIR-folder #',i,': ',subfolder[i],sep=""))
  files<-dir(curnfolder<-paste(mainfolder,'/',subfolder[i],sep="", row.names=NULL))
  oci  <-con_ocir_json('questionnaires',oci,curnfolder,files,i) 
  
}  


# demographic -------------------------------------------------------------

#demographic=read.csv('myfolder/03_data/03_demographic_data/all_data_sex_age.csv') #in demographic_data folder in the server
# save --------------------------------------------------------------------
save(icar,file='myfolder/03_data/02_aggregated_data/exp2_07.2021/icar_raw.Rdata')
save(oci,file='myfolder/03_data/02_aggregated_data/exp2_07.2021/oci_raw.Rdata')
save(cards2,file='myfolder/03_data/02_aggregated_data/exp2_07.2021/cards2_raw.Rdata')
save(memory2,file='myfolder/03_data/02_aggregated_data/exp2_07.2021/memory2_raw.Rdata')
save(cards3,file='myfolder/03_data/02_aggregated_data/exp2_07.2021/cards3_raw.Rdata')
save(memory3,file='myfolder/03_data/02_aggregated_data/exp2_07.2021/memory3_raw.Rdata')
save(squares,file='myfolder/03_data/02_aggregated_data/exp2_07.2021/squares_raw.Rdata')
#save(questions,file='myfolder/03_data/02_aggregated_data/exp2_07.2021/questions_raw.Rdata')
#save(screensize,file='myfolder/03_data/02_aggregated_data/exp2_07.2021/screensize_raw.Rdata')
#save(demographic,file='myfolder/03_data/02_aggregated_data/exp2_07.2021/demographic.Rdata')


