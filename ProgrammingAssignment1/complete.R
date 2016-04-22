str_pad<-function(string,length,pad="0"){
  numpad<-length-nchar(string)
  output<-string
  if (numpad > 0) {
    for (i in 1:numpad) {
      output<-paste(pad,string,sep="")
      string<-output
    }
  }
  output
}

complete<-function(directory,id=1:332){
  data<-read.csv(paste(directory,"/",str_pad(id[1],3),".csv",sep=""))
  good<-complete.cases(data)
  nobs<-length(data[1][good, ])
  i<-2
  while (i <= length(id)){
    data<-read.csv(paste(directory,"/",str_pad(id[i],3),".csv",sep=""))
    good<-complete.cases(data)
    nobs<-c(nobs, length(data[1][good, ]))
    i<-i+1
  }
  out<-data.frame("id"=id,"nobs"=nobs)
  out
}