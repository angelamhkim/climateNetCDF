##6/20/2017
##reading NetCDF files from ESGF CMIP5 data 

#download packages 
library(ncdf4)
library(chron)
library(RColorBrewer)
library(lattice)

#set the working directory 



readfile <- function(folder, fname) {
	ncname <- fname
	lastthree <- substr(ncname, nchar(ncname)-2,nchar(ncname))
	if(lastthree!=".nc"){
		ncfname <- paste(ncname, ".nc", sep="") 
	} else {
		ncfname<-ncname
	}
	dname <- "tasmax" #name of the temperature variable

	#open NetCDF file 
	cname <- paste(folder,"/",ncfname,sep="")
	ncin <- nc_open(cname) 

	#get longitutes and latitudes 
	lon <<- ncvar_get(ncin,"lon") 
	nlon <- dim(lon)

	lat <<- ncvar_get(ncin,"lat", verbose=F) 
	nlat <- dim(lat) 

	#get the time variable and attributes 
	tori <<- ncvar_get(ncin,"time") #for time original 
	tunits <<- ncatt_get(ncin,"time","units") 
	nt <- dim(tori) 

	#get the variable and its attributes 
	tmp.array <<- ncvar_get(ncin,dname) #dimensions (lon,lat,time) 
	dlname <- ncatt_get(ncin,dname,"long_name") 
	dunits<- ncatt_get(ncin,dname,"units") 
	fillvalue <- ncatt_get(ncin,dname,"_FillValue") 

	#get global attributes 
	title <- ncatt_get(ncin,0,"title") 
	institution <- ncatt_get(ncin,0,"institution") 
	datasource <- ncatt_get(ncin,0,"source") 
	references <- ncatt_get(ncin,0,"references") 
	history <- ncatt_get(ncin,0,"history") 
	Conventions <- ncatt_get(ncin,0,"Conventions") 

	#close the NetCDF file
	nc_close(ncin)
	

	#make new time
	tustr <- strsplit(tunits$value," ") 
	tdstr <- strsplit(unlist(tustr)[3],"-")
	tmonth = as.integer(unlist(tdstr)[2])
	tday = as.integer(unlist(tdstr)[3])
	tyear = as.integer(unlist(tdstr)[1])
	tnew <<- chron(tori,origin=c(tmonth,tday,tyear))

	#only for growing season (months 5-9)
	nyears <- dim(tori)/12 #number of years in a monthly data set 
	m <- rep(1:12, nyears)
	a <- which(m==5) 
	b <- which(m==6) 
	c <- which(m==7) 
	d <- which(m==8)
	e <- which(m==9) 
	a <- c(a,b,c,d,e)
	a <- sort(a)
	gtori <<- tori[a]
	gtnew <<- tnew[a]

	#select the tmp.array for the growing season and change to oC 
	if(mean(tmp.array)>100){
		gtmp.array <<- tmp.array[,,a]-273.15
	}
	else(gtmp.array<<-tmp.array)

	print("new variables defined: lat, lon, tori, tunits, tmp.array, gtori, gtnew, gtmp.array")
}


#-----------------------------------------------------------------------
#the functions below were used in the building of the final product

#changes time from "days since 01-01-01 to actual dates"
#settime <- function(units){
	#tustr <- strsplit(units$value," ") 
	#tdstr <- strsplit(unlist(tustr)[3],"-")
	#tmonth = as.integer(unlist(tdstr)[2])
	#tday = as.integer(unlist(tdstr)[3])
	#tyear = as.integer(unlist(tdstr)[1])
	#tnew <<- chron(tori,origin=c(tmonth,tday,tyear))
#}

#takes a time slice of the grid 
#mapslice <- function(tmp.array,mmon) {
	#tmp.slice <- tmp.array[,,m]
	#image(lon,lat,tmp.slice,col=rev(brewer.pal(10,"RdBu")))
#}


#make a function to index growing season
#gseason <- function (nyears) { #nyears should be the number of years in a file
	#m <- rep(1:12, nyears)
	#a <- which(m==5) 
	#b <- which(m==6) 
	#c <- which(m==7) 
	#d <- which(m==8)
	#e <- which(m==9) 
	#a <- c(a,b,c,d,e)
	#a <<- sort(a)  
	#outputs a vector of the position of the months 5-9 for nyears years
#}