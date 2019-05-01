##dependent packages
#install.packages(c("dismo", "maptools", "sp", "raster", "ncdf4", "mgcv", "rJava", "ROCR"))

library(ncdf4)
library(raster)
library(mgcv)
library(dismo)
library(maptools)
library(sp)
library(rJava)
library(ROCR)

data(wrld_simpl)

months<- c("January", "February","March", "April", "May", "June", "July", "August", "September", "October", "November","December")
bin_sizes<- c(10,20, 30)
mean_sizes1030<- c(6.961597, 16.640155, 22.320416)

#Step 1: create metabolic rate raster for target size class
source("metabolic_rate_functions.R")
for(i in 1:length(bin_sizes)){
	calc_RMR(temp_nc="../Data/sst/monthly_means/", timeframes=months, input_mass=mean_sizes1030[i], mass_bin=bin_sizes[i]) 
	print(paste("finished with size",mean_sizes1030[i], sep=" "))
}


#Step 2: calculation consumption rate
source("ingestion_functions.R")
for(j in 1:length(bin_sizes)){ 
	calc_HIF(size=mean_sizes1030[j], data=uwkdata, plot.months=months, mass_bin= bin_sizes[j])
	calc_HIF_hr(mass_bin=bin_sizes[j])
	print(paste("finished with size", mean_sizes1030[j], sep=" "))
}


#step 3: calculate energetic cost and gain
source("cost_gain_functions.R")
for(i in bin_sizes){
	calc_cost(bin_size=i, timeframe=months)
	calc_gain(bin_size=i, timeframe=months)
}

#step 4: calculate the physiological niche
source("physio_niche_function.R")
for(i in bin_sizes){
  mod<-physio_niche(i)
  print(paste("finished with size class", i, sep=" "))
}

#step 5: calculate the ecologically defined niche
source("eco_niche.R")
for(i in bin_sizes){
  mod<-eco_niche(i)
  print(paste("finished with size class", i, sep=" "))
}



