#map with country outlines 
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)
library(viridis)
library(weathermetrics)

#need to define tas, lat, and lon first
#tas is the temperature by lat,lon at one point in time 
mapTempCountry <- function(lat,lon,tas, model="model", perc="%") #model and perc should be a string
{
	titletext <- paste("Modeled year after which", perc, "of temperatures are outside optimum")
	expand.grid(lon, lat) %>%
	        rename(lon = Var1, lat = Var2) %>%
	        mutate(lon = ifelse(lon > 180, -(360 - lon), lon),
	               tas = as.vector(tas)) %>% 
	        #mutate(tas = convert_temperature(tas, "k", "c")) %>%
	        
	        ggplot() + 
	        geom_point(aes(x = lon, y = lat, color = tas),
	                   size = 0.8) + 
	        borders("world", colour="black", fill=NA) + 
	        scale_color_viridis(na.value="white",name = "Year", limits=c(2006,2100), breaks=c(2010,2020, 2030,2040,2050, 2060,2070, 2080,2090, 2100)) + 
	        theme(legend.direction="vertical", legend.position="right", legend.key.width=unit(0.4,"cm"), legend.key.heigh=unit(2,"cm")) + 
	        coord_quickmap() + 
	        ggtitle(titletext,
	                subtitle = model) 
}