#take the threshold function for each grid point and apply it to the whole data 
#outputs a 2D matrix with dimensions lon,lat and year values (output of thresholdYear function)
source("C:/Users/angel/Documents/UoT/Research/data/code/threshold.R") 

#tarray is tmp.array (full data set)
#t has to be in tnew units (ie. NOT in days after...)
setPoint <- function(tarray, tvalue, t, perc){ #from readCDF.R, tarray is tmp.array
	var1 <- dim(tarray)[1] #get the length of longitudes, returns a number
	var2 <- dim(tarray)[2] #get the length of latitudes, returns a number
	empty.matrix <- matrix(nrow=var1,ncol=var2)
	for (i in 1:var1) { #i is moving from 1 to 192 (longitude)
		
		for (j in 1:var2) { #j moves from 1 to 145 (latitude)
			#check if data is in C or K before setting tvalue
			empty.matrix[i,j] <- tYear(tarray[i,j,],tvalue,t,perc)
		}
	}
	alarm()
	return(empty.matrix)
}

fixdim <- function(cmatrix){
	if(length(dim(cmatrix))==3){
		cmatrix <- cmatrix[c(1:13,187:192), 79:86,]
		return(cmatrix)
	}
	#else if (length(dim(cmatrix))==2){
		#cmatrix <- cmatrix[c(1:13,187:192), 79:86]
		#return(cmatrix)
	#}
	
}

rNA <- function(tout) {
	tout <- replace(tout, tout==-9999, NA)
	return(tout) 
}

tcsv <- function(fmatrix, fname){
	fmatrix <- t(fmatrix) 
	write.csv(fmatrix, fname)
}


#---------------------------------------------------------------------------
#TEST
#the code below was used to develop setPoint
getMean <- function(vector){
	if(mean(vector)>0){mean(vector)}
	if(mean(vector)<0){-9999}
}
test.array <- array(rnorm(10,mean=0,sd=1), dim=c(2,3,4))

testPoint <- function(test.array){
	var1 <- dim(test.array)[1]
	var2 <- dim(test.array)[2]
	empty.matrix <- matrix(nrow=var1, ncol=var2) 
	for (i in 1:var1) {
		for (j in 1:var2) {
			tmp.point <- test.array[i,j,]
			temp <- getMean(tmp.point) 
			empty.matrix[i,j] <- temp
		}
	}
	empty.matrix
}