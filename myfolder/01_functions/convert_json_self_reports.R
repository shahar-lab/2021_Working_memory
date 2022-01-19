con_ocir_json<-function(myname,datafile,curnfolder,files,subnum) {
  if (sum(grepl(myname,files))>0) {
    library(RJSONIO)
    x<-fromJSON(paste(curnfolder,'/',files[grepl(myname,files)],sep=""))
    x<-fromJSON(x[[1]])[[2]]
    x<-as.data.frame(do.call(cbind,x))
    x<-data.frame(subj=subnum,x$responses)
    num_questions=str_count(x$x.responses,":")
    values_locations=str_locate_all(x$x.responses,":")[[1]][,1]+2
    
    for (question in 1:num_questions)
    {
      location=values_locations[question]
      x[paste("q",question,sep="")]=strtoi(substr(x$x.responses,location,location))
    }
    x$score=apply(x[,3:20],1,sum)
    x=x%>%relocate(x.responses,.after=score)
    x=x%>%relocate(score,.after=subj)
    return(rbind(datafile,x))
  }
  else
  {
    return(datafile)
  }
}