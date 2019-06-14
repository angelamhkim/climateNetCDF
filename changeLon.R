#the function below was written to change the longitude indexing
#not needed anymore because the mapping function already does this

#changing longitude from 0:360 to -180:180
rotateLon <- function(lon) {
	for (i in c(1:length(lon))){
		if (lon[i]>180){
			lon[i] <- lon[i]-360
		}
		else {
			lon[i] <- lon[i]
		}
	}
}

#ncin <- nc_open("filename", write=TRUE) 
#ncvar_put(ncin, "lon", lon) 