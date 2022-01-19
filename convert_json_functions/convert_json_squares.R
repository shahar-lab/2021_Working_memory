

con_squares_json<-function(myname,squares.practice,squares.exp,curnfolder,files,subnum) {
  if (sum(grepl(myname,files))>0) {
    library(RJSONIO)
    x<-fromJSON(paste(curnfolder,'/',files[grepl(myname,files)],sep=""))
    x<-fromJSON(x[[1]])
    i<-sapply(1:length(x), function(i) {exists('trial_num',x[[i]]) & x[[i]]['phase']=='practice' &  x[[i]]['trial_name']=='test'}) #practice_part_test
    practice_test<-x[i]
    practice_test<-as.data.frame(do.call(rbind,practice_test))
    i<-sapply(1:length(x), function(i) {exists('trial_num',x[[i]]) & x[[i]]['phase']=='practice' &  x[[i]]['trial_name']=='memory'}) #practice_part_memory_array
    practice_memory<-x[i]
    practice_memory<-as.data.frame(do.call(rbind,practice_memory))
    squares.practice=rbind(squares.practice,cbind(subj=rep(subnum,dim(practice_test)[1]),practice_test,practice_memory))
    
    i<-sapply(1:length(x), function(i) {exists('trial_num',x[[i]]) & x[[i]]['phase']=='exp' &  x[[i]]['trial_name']=='test_squares'}) #exp_part_test
    exp_test<-x[i]
    exp_test<-as.data.frame(do.call(rbind,exp_test))
    i<-sapply(1:length(x), function(i) {exists('trial_num',x[[i]]) & x[[i]]['phase']=='exp' &  x[[i]]['trial_name']=='memory'}) #exp_part_memory_array
    exp_memory<-x[i]
    exp_memory<-as.data.frame(do.call(rbind,exp_memory))
    squares.exp=rbind(squares.exp,cbind(subj=rep(subnum,dim(exp_test)[1]),exp_test,exp_memory))
    return(list(squares.practice,squares.exp))
  }
  else
  {
    return(datafile)
  }
}
