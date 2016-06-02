
source('convertFunction.R')

i.fold = '.\\'
i.file = 'example'
i.path = paste(i.fold,i.file,'.html',sep="")

o = convertBioTyperReportToCSV(i.path)

o.path = paste(i.fold,i.file,'.csv',sep="")
write.csv(o,o.path,row.names=FALSE)  

