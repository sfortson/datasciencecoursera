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

pollutantmean<-function(directory,pollutant,id=1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate"
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values) 
  ## NOTE: do not round the result!

  data<-read.csv(paste(directory,"/",str_pad(id[1],3),".csv",sep=""))
  subset<-data[[pollutant]]
  bad<-is.na(subset)
  d<-subset[!bad]
  i<-2
  while (i <= length(id)){
    data<-read.csv(paste(directory,"/",str_pad(id[i],3),".csv",sep=""))
    subset<-data[[pollutant]]
    bad<-is.na(subset)
    d<-c(d,subset[!bad])
    i<-i+1
  }
  
  round(mean(d),digits=3)
}