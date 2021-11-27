my_binrt<-function(.data,var,Nquantiles){
  #this functions is designed for dyplr piping
  #inputs include data.frame (you don't to include that when piping)
  #var to be binned and the number of binns
  browser()
  x=unlist(.data%>%select({{var}})) #vector to be binned
  y=rep(0,length(x))               #preallocate the vector that wil include the binns
  myquantiles=quantile(x,seq(0,1,by=1/Nquantiles)) 
  for (i in 1:(Nquantiles-1)){
    y=y+(x>=myquantiles[i] & x<myquantiles[i+1])*1*i
  }
  y[y==0]=Nquantiles
  return(as.numeric(y))
}
