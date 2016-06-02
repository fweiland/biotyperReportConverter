
library(XML)

convertBioTyperReportToCSV = function(fname){
  
  doc <- htmlParse(fname)
    
  # Number of Analytes
  h3s = xpathSApply(doc, "//h3", xmlValue)
  N = length(h3s)-3
  
  # URLS
  urls = xpathSApply(doc, "//tr/td/a[starts-with(@href,'http')]", xmlGetAttr,"href")
  
  if ((length(urls) %% N) != 0){
    print("ERROR: length mismatch: ncbi_url -- aborting!")
    return(data.frame())
  }

  # Top n hits are reported
  n = length(urls)/N  
  
  # Initialise output data.frame o
  o = data.frame(Analyte=rep(h3s[4:length(h3s)],rep(n,N)))
  
  # All Table entries.
  vals = xpathSApply(doc, "//tr/td", xmlValue)

  # Analyte Name
  tmp = which(vals == "Analyte Name:")+1
  if(length(tmp) != N){
    print("ERROR: length mismatch: nam")
  } else {
    o$nam = rep(vals[tmp],rep(n,N))
  }
  
  # # Analyte ID
  # tmp = which(vals == "Analyte ID:")+1
  # if(length(tmp) != N){
  #   print("ERROR: length mismatch: id")
  # } else {
  #   o$id = rep(vals[tmp],rep(n,N))
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
      for (j in 1:n){
        o[3*i - j + 1,"spot"] = tmp[[i]][tmp2[i]-2]        
      }
    }
  }
  
  tmp = which(vals == "Applied Taxonomy Tree:")
  if(length(tmp) != N){
    print("ERROR: length mismatch: table entries!?")
  } else {
    tmp2 = tmp + 2
    for (j in 2:n){
      tmp2 = c(tmp2, tmp + 2 + 4*(j-1))
    }
    tmp = c(t(matrix(tmp2,nrow=length(N))))
    
    o$rnk = as.numeric(substring(vals[tmp],1,2))
    o$score_sym = substring(vals[tmp],3)
    o$pattern = vals[tmp+1]
    o$score = vals[tmp+2]
    o$ncbi_id = vals[tmp+3]
  }
    
  o$ncbi_url = urls
  
  return(o)

}



