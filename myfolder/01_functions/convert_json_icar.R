########################################################################
################ convert json to Rdata  ################################
####################################################################


con_icar_json<-function(icar_name,datafile,curnfolder,files,subnum) {
  
  #create icar_cr which hold the mapping for the correct responses
  icar_item <- c('ICAR_3_options','ICAR_5_options','ICAR_7_options','ICAR_9_options','ICAR_10_options',
                 'ICAR_11_options','ICAR_12_options','ICAR_13_options','ICAR_15_options','ICAR_17_options',
                 'ICAR_19_options','ICAR_21_options','ICAR_22_options','ICAR_23_options','ICAR_24_options',
                 'ICAR_25_options')
  correct_response <- c('C','B','F','G','X','G','X','N','D','B','B','E','5',"It's impossible to tell",'47','Sunday')
  item <- c(3,5,7,9,10,11,12,13,15,17,19,21,22,23,24,25)
  icar_cr <- data.frame(icar_item,correct_response,item)
  
  #convert icar's json
  if (sum(grepl(icar_name,files))>0) {
    library(stringr)
    temp       <-as.data.frame(fromJSON(file = paste(curnfolder,'/',files[grepl('icar',files)],sep="")))
    
    #take item, response and item order from temp
    item        <-temp[,grepl(c('name'), colnames(temp))==1]
    item        <- as.integer(sub("(?i).*?\\ICAR_\\s*(\\d+).*", "\\1", item))
    resp        <-temp[,grepl(c('value'), colnames(temp))==1]
    
    #keep a vector telling which item was given at the 1st position, 2nd position, etc.
    item_order<-item
    names(item_order)<-sapply(1:16, function(i) {paste('position',i,sep="")})
    
    #create a vector of acc
    acc<-vector()
    for (i in 1:16){
      cr=icar_cr[icar_cr[,3]==item[i],2]
      acc[i]=(resp[i]==cr)*1  
    }
    
    #order according to item
    names(resp)<-paste('item_',item,'_resp',sep="")
    names(acc)<-paste('item_',item,'_acc',sep="")
    resp<-resp[ ,order(names(resp))]
    acc <-acc [order(names(acc))]
    
    x<-as.data.frame(c(acc,resp,item_order))
    x<-data.frame(subj=subnum,x)
    x$score=apply(x[,2:17],1,sum)
    x=x%>%relocate(score,.after=subj)
    
    return(rbind(datafile,x))
  }
  else
  {
    return(datafile)
  }
}