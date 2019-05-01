
#dependent packages
library(dismo)
library(maptools)
library(sp)
library(raster)
library(mgcv)

#dependent scripts
source("size_to_mass.R")

uwkdata<- get(load("../Data/WeeklyHIFdata.RData"))
uwkdata$kg<- SFL_to_kg(uwkdata$length2)
uwkdata$sst<-uwkdata$temp2+18  #temp was centred

uwkdata$calday<- as.Date(uwkdata$jday, origin=as.Date("2002-01-01"))
uwkdata$month<- as.integer(uwkdata$month2)
uwkdata$lons<- uwkdata$long2+360
uwkdata$lats<- uwkdata$lat2
uwkdata$mixed_layer_depth<- uwkdata$tcline2
uwkdata$log_chla_conc<- uwkdata$logchl2
uwkdata$eddy_kinetic_energy<- uwkdata$eke2
uwkdata$ftag<-factor(uwkdata$tag2)

uwkdata<- uwkdata[!uwkdata$hifmed2<10,]


##creates a gam on a sibset of data for each size class and each month

get_gam<- function(data=uwkdata){
	gcontrol<-gam.control(maxit=100)
	
	gam_mod<- gamm(hifmed2~ month+s(lons,lats, bs="ts",k=50)+s(sst,bs="ts",k=10)+s(mixed_layer_depth,bs="ts",k=10)+
		s(log_chla_conc,bs="ts",k=10)+s(kg,bs="ts",k=10)+s(eddy_kinetic_energy,bs="ts",k=10), random=list(ftag=~1),
	cor=corAR1(form=~1|ftag), data=data,control=gcontrol, fit=TRUE)	
	
	
#~ 	gam_mod<- gamm(hifmed2~ month+s(lons,lats, bs="ts",k=50)+s(sst,bs="ts",k=10)+s(mixed_layer_depth,bs="ts",k=10)+
#~ 		s(log_chla_conc,bs="ts",k=10)+s(kg,bs="ts",k=10)+s(eddy_kinetic_energy,bs="ts",k=10), data=data,control=gcontrol, fit=TRUE)	

return(gam_mod$gam)
}

#~ test_gam<- get_gam()
#~ gam.check(test_gam, old.style=TRUE)
#~ summary(test_gam)
#~ par(mfrow=c(2,3))
#~ plot.gam(test_gam, scale=0)


months<- c("January", "February","March", "April", "May", "June", "July", "August", "September", "October", "November","December")
mon<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

calc_HIF<- function(size, data=uwkdata, plot.months=months, mass_bin) {
	for(i in 1:length(plot.months)){

		sst_raster<- raster(paste("../Data/new_sst/monthly_means/", mon[i], ".nc", sep=""))
#~ 		sst_raster<- raster(paste("../Data/sst_test/monthly_means/", plot.months[i], ".nc", sep=""))
		mld_raster<- raster(paste("../Data/mld_test/disagg_mld/", plot.months[i], i,".nc", sep="")) #put thermocline depth file in here
		chloro_raster<- raster(paste("../Data/chla_test/log_chla/", plot.months[i], ".nc", sep="")) #chloro a levels file 
		eddy_raster<- raster(paste("../Data/eddy_test/EKE/monthly_means/", plot.months[i], ".nc", sep="")) #eddy kinetic energy files
		lats_raster<- raster("../Data/new_nplats.nc") #latitude
		lats_resample<- resample(lats_raster, sst_raster, method="bilinear")
		lons_raster<- raster("../Data/new_nplons.nc") #longitude
		lons_resample<- resample(lons_raster, sst_raster, method="bilinear")
		
		kg_raster<- raster(ncol=ncol(sst_raster), nrow=nrow(sst_raster), ext=extent(sst_raster), vals=rep(size, ncell(sst_raster)))
		names(kg_raster)<- "kg"
		
		month_raster<- raster(ncol=ncol(sst_raster), nrow=nrow(sst_raster), ext=extent(sst_raster), vals=rep(i, ncell(sst_raster)))
		names(month_raster)<- "month"
		
		env_raster<- stack(sst_raster, mld_raster, chloro_raster, eddy_raster, lats_resample, 
			lons_resample, month_raster,kg_raster)
		
		
		use_gam<- get_gam(data=data)

		p<- raster::predict(env_raster, use_gam, inf.rm=T)

#~ 		writeRaster(p, filename=paste("../Data/consumption_rates/HIF_day/Size_", mass_bin, "/", plot.months[i], sep=""), overwrite=T, 
		writeRaster(p, filename=paste("../sandbox/Data/consumption_rates2/HIF_day/Size_", mass_bin, "/", plot.months[i], sep=""), overwrite=T, 
			format="CDF", varname="HIF_day", varunit="kcal_day", xname="long", yname="lat")
}}


calc_HIF_hr<- function(mass_bin=10){
	for(i in 1:length(months)){
#~ 		day<- raster(paste("../Data/consumption_rates/HIF_day/Size_", mass_bin,"/", months[i], ".nc", sep=""))
		day<- raster(paste("../sandbox/Data/consumption_rates2/HIF_day/Size_", mass_bin,"/", months[i], ".nc", sep=""))
		hr<- calc(day, function(x) {x*4.184/24})
		values(hr)[values(hr)<0] <- 0

#~ 		writeRaster(hr, filename=paste("../Data/consumption_rates/HIF_hr/Size_", mass_bin,"/", months[i], sep=""), overwrite=T, 
		writeRaster(hr, filename=paste("../sandbox/Data/consumption_rates2/HIF_hr/Size_", mass_bin,"/", months[i], sep=""), overwrite=T, 
			format="CDF", varname="HIF_hr", varunit="kJ_hr", xname="long", yname="lat")
}}
		










