

#function takes a single grid point through time and finds the year when threshold value is passed by more than perc%
thresholdYear <- function(dat, value, t, perc){ #value is the threshold temperature
	#**** dat is the data, value is the temperature threshold, t is time
	#dat is a vector of temperatures of one grid point through time ie. tmp.point <- tmp.array[1,2,a]
	#** pay attention to units of temperature (ie. C or K) 
	vec <- dat>value #example: vec38 <- tmp.point>38
	vec <- replace(vec, vec==TRUE, 1) 
	vec <<- replace(vec, vec==FALSE, 0)
	for (i in c(1:475)){
		if (mean(vec[i:475])>perc){
			break
		}
		a <<- append(1,i)
	}
	tyear <- a[2]+1
	#print(t[tyear]) #returns a factor
	nyear <- years(t[tyear])
	nyear <- as.numeric(levels(nyear)) #returns a numeric year 
	#print(nyear)
	if(length(nyear)==0){
		nyear <- -9999
	}
	else{nyear}
	return(nyear)
}


tYear <- function(dat, value, t, perc){ #value is the threshold temperature
	#**** dat is the data, value is the temperature threshold, t is time
	#dat is a vector of temperatures of one grid point through time ie. tmp.point <- tmp.array[1,2,a]
	#** pay attention to units of temperature (ie. C or K) 
	vec <- dat>value #example: vec38 <- tmp.point>38
	vec <- replace(vec, vec==TRUE, 1) 
	vec <<- replace(vec, vec==FALSE, 0)
	for (i in c(1:475)){
		if (mean(vec[i:475])>perc){break}
	}
	a <<- append(1,i)
	tyear <- a[2]+1
	#print(t[tyear]) #returns a factor
	nyear <- years(t[tyear])
	nyear <- as.numeric(levels(nyear)) #returns a numeric year 
	#print(nyear)
	if(length(nyear)==0){
		nyear <- -9999
	}
	else{nyear}
	return(nyear)
}

#example: thresholdYear(tmp.point,37,tnew1,0.7)

#-------------------------------------------------------------------
#the functions below were written in the building of the final code above
#threshold takes a threshold temperature (value) and a vector of temperatures
#outputs a new vector of values 1 if threshold is crossed and 0 if threshold is not crossed 
threshold <- function(value, dat) {
	vec <- dat>value #example: vec38 <- tmp.point>38
	vec <- replace(vec, vec==TRUE, 1) 
	vec <<- replace(vec, vec==FALSE, 0) #vec is outputted back to console 
}

#this function takes the vec outputted from above and outputs the year when thresholds are exceeded more than n%
findTYear <- function(vec, t, perc){
	for (i in c(1:475)){
		if (mean(vec[i:475])>perc){
			break
		}
		a <- append(1,i)
	}
	tyear <- a[2]+1
	print(t[tyear])
}