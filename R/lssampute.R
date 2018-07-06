lss.ampute<-function(n, time, mean, sd, sd.e,perc.up, perc.zero, perc.down, perc.ampute, ampute="mcar", set.seed=NULL){
  if(ampute=="mcar"){
    data=lss(n,time, mean, sd, sd.e,perc.up,perc.zero,perc.down,set.seed)
    mcar=runif(dim(data)[1]*dim(data)[2], min=0, max=1)
    data.ampute=as.vector(data)
    data.ampute=ifelse(mcar<perc.ampute, NA, data)
    data.ampute=matrix(data.ampute,ncol=time) 
  }else if(ampute=="mar"){
    data=lss(n,time, mean, sd, sd.e,perc.up,perc.zero,perc.down)
    data.ampute<-data
    for(i in 1:dim(data)[1]){
      for(j in 4:dim(data)[2]){
        dif1 = data[i,j-2]-data[i,j-3]
        dif2 = data[i,j-1]-data[i,j-2]
        if(dif1>0 && dif2>0){  # if weight goes up twice, drops out
          data.ampute[i,j:dim(data)[2]] = NA;  break
        }
      }
    }
  }else if(ampute=="mnar"){
    data=lss(n,time, mean, sd, sd.e,perc.up,perc.zero,perc.down)
    data.mnar<-as.vector(data)
    sort.data.mnar = sort(data.mnar, decreasing=TRUE)
    nmar   = sort.data.mnar[ceiling(perc.ampute*length(sort.data.mnar))] #Limit to NA
    data.ampute = ifelse(data.mnar>nmar, NA, data.mnar)  # doesn't show up when heavier
    data.ampute=matrix(data.ampute,ncol=time)
  }
  return(list(data=data, data.ampute=data.ampute))
}