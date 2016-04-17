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

corr<-function(directory, threshold = 0){
  ## 'directory' is a character of length 1 indicating
  ## the location of the CSV files
  com<-complete(directory)
  
  ## 'threshold' is a numeric vector of length 1 indicating
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  for (i in 1:nrow(com)) {
    if (com[i,"nobs"]>threshold) {
      data<-read.csv(paste(com[i,"id"],"/",str_pad(id[1],3),".csv",sep=""))
      sulfate<-data[['sulfate']]
      ##bad<-is.na(subset)
      ##s<-sulfate[!bad]
    }
  }
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result! 
}