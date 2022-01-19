

con_cards_json<-function(myname,cards.practice,memory.practice,cards.test,memory.test,curnfolder,files,subnum) {
  if (sum(grepl(myname,files))>0) {
    library(RJSONIO)
    #load json
    x<-fromJSON(paste(curnfolder,'/',files[grepl(myname,files)],sep=""))
    x<-fromJSON(x[[1]])
    
    #####practice------------------------------------------------------------
    #take only choices 
    i<-sapply(1:length(x), function(i) {exists('trial_num',x[[i]]) & x[[i]]['phase']=='practice' & ( x[[i]]['trial_name']=='cards1' |  x[[i]]['trial_name']=='cards2')})
    x.ch<-x[i]
    x.ch<-as.data.frame(do.call(rbind,x.ch))
    
    #take only rewards
    i<-sapply(1:length(x), function(i) {exists('trial_num',x[[i]]) & x[[i]]['phase']=='practice' & ( x[[i]]['trial_name']=='reward1' |  x[[i]]['trial_name']=='reward2')})
    x.rw<-x[i]
    library(data.table)
    x.rw<-rbindlist(x.rw, fill = TRUE)
    
    #take only memory 
    i<-sapply(1:length(x), function(i) {exists('trial_num',x[[i]]) & x[[i]]['phase']=='practice' & ( x[[i]]['trial_name']=='test_cards' |x[[i]]['trial_name']=='memory_cards')})
    x.memory<-x[i]
    x.memory<-rbindlist(x.memory, fill = TRUE)
    memory=cbind(subj=rep(subnum,dim(x.memory)[1]),x.memory)
 
    
    #merge and save to file choices and rewards, and memory
    cards.practice=rbind(cards.practice,cbind(subj=rep(subnum,dim(x.ch)[1]),x.ch,x.rw))
    memory.practice=rbind(memory.practice,memory)
    
    #####test------------------------------------------------------------
    #take only choices 
    i<-sapply(1:length(x), function(i) {exists('trial_num',x[[i]]) & x[[i]]['phase']=='exp' & ( x[[i]]['trial_name']=='cards1' |  x[[i]]['trial_name']=='cards2')})
    x.ch<-x[i]
    x.ch<-as.data.frame(do.call(rbind,x.ch))
    
    #take only rewards
    i<-sapply(1:length(x), function(i) {exists('trial_num',x[[i]]) & x[[i]]['phase']=='exp' & ( x[[i]]['trial_name']=='reward1' |  x[[i]]['trial_name']=='reward2')})
    x.rw<-x[i]
    library(data.table)
    x.rw<-rbindlist(x.rw, fill = TRUE)
    
    #take only memory 
    i<-sapply(1:length(x), function(i) {exists('trial_num',x[[i]]) & x[[i]]['phase']=='exp' & ( x[[i]]['trial_name']=='test_cards' |x[[i]]['trial_name']=='memory_cards')})
    x.memory<-x[i]
    x.memory<-rbindlist(x.memory, fill = TRUE)
    memory=cbind(subj=rep(subnum,dim(x.memory)[1]),x.memory)
    
    
    #merge and save to file choices and rewards, and memory
    cards.test=rbind(cards.test,cbind(subj=rep(subnum,dim(x.ch)[1]),x.ch,x.rw))
    memory.test=rbind(memory.test,memory)
    return(list(cards.practice,memory.practice,cards.test,memory.test))
  }
  else
  {
    return(datafile)
  }
}
