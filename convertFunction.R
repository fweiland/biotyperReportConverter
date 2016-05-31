
library(XML)
library(stringr)

convertBioTyperReportToCSV = function(fname){
  
  doc <- htmlParse(fname)
  
  # top n ranked matches reported
  n = 3
  
  # Number of Analytes
  tmp = xpathSApply(doc, "//h3", xmlValue)
  N = length(tmp)-3
  o = data.frame(Analyte=rep(tmp[4:length(tmp)],rep(3,N)))
  
  # All Table entries.
  vals = xpathSApply(doc, "//table/tbody/tr/td", xmlValue)
  
  # Analyte Name
  tmp = which(vals == "Analyte Name:")+1
  if(length(tmp) != N){
    print("ERROR: length mismatch: nam")
  } else {
    o$nam = rep(vals[tmp],rep(3,N))
  }
  
  # # Analyte ID
  # tmp = which(vals == "Analyte ID:")+1
  # if(length(tmp) != N){
  #   print("ERROR: length mismatch: id")
  # } else {
  #   o$id = rep(vals[tmp],rep(3,N))
  # }
  
  # Spot
  tmp = which(vals == "Analyte Description:")+1
  if(length(tmp) != N){
    print("ERROR: length mismatch: spot")
  } else {
    tmp = strsplit(vals[tmp],'\\\\')
    tmp2 = sapply(tmp,function(x){return(length(x))})
    o$spot = ""
    for (i in 1:N){
      o[3*i-2,"spot"] = tmp[[i]][tmp2[i]-2]
      o[3*i-1,"spot"] = tmp[[i]][tmp2[i]-2]
      o[3*i  ,"spot"] = tmp[[i]][tmp2[i]-2]
    }
  }
  
  tmp = which(vals == "Applied Taxonomy Tree:")
  if(length(tmp) != N){
    print("ERROR: length mismatch: table entries!?")
  } else {
    tmp = c(t(matrix(c(tmp+2,tmp+6,tmp+10),nrow=length(anal))))
    
    o$rnk = as.numeric(substring(vals[tmp],1,2))
    o$score_sym = substring(vals[tmp],3)
    o$pattern = vals[tmp+1]
    o$score = vals[tmp+2]
    o$ncbi_id = vals[tmp+3]
  }
  
  # URLS
  tmp = str_subset(xpathSApply(doc, "//table/tbody/tr/td/a", xmlGetAttr,"href"),"http")
  if (length(tmp) != 3*N){
    print("ERROR: length mismatch: ncbi_url")  
  } else{
    o$ncbi_url = tmp  
  }
  
  nams = unique(o$nam)
  if (length(nams) == 1){
    write.csv(o,paste(nams,"csv",sep="."),row.names=FALSE)  
  } else {
    write.csv(o,paste(substring(fname,1,nchar(fname)-5),"csv",sep="."),row.names=FALSE)  
  }
  
}



