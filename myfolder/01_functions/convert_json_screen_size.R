

con_screen_size_json<-function(myname,datafile,curnfolder,files,subnum) {
  if (sum(grepl(myname,files))>0) {
    library(RJSONIO)
    x<-fromJSON(paste(curnfolder,'/',files[grepl(myname,files)],sep=""))
    x<-fromJSON(x[[1]])[[1]]
    x<-as.data.frame(do.call(cbind,x))
    x<-data.frame(subj=subnum,x)
    
    return(rbind(datafile,x))
  }
  else
  {
    return(datafile)
  }
}
