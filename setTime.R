##6/20/2017
##Converting the time variable 

#make sure cdf file was already read, if not, run this code
#source("readCDF.R") 

#split time units strings into fields 
tustr <- strsplit(tunits$value," ") 
tdstr <- strsplit(unlist(tustr)[3],"-")
tmonth = as.integer(unlist(tdstr)[2])
tday = as.integer(unlist(tdstr)[3])
tyear = as.integer(unlist(tdstr)[1])
chron(t,origin=c(tmonth,tday,tyear))