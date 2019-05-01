
#~ library(c("dismo", "maptools", "sp", "raster", "ncdf4", "rJava"))


#~ months<- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November","December")

mon<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


eco_niche<- function(bin_size, timeframe=months, index){
	means<- list(mean_training=c(), mean_test=c())
	for(i in 1:length(timeframe)){
#~ 		arch_data<- read.csv(paste("../Data/arch_tag/monthly_tagging/Size_", bin_size, "/", timeframe[i], ".csv", sep=""), 
		arch_data<- read.csv(paste("../sandbox/Data/arch_tag/monthly_tagging/Size_", bin_size, "/", timeframe[i], ".csv", sep=""), 
					header=T, skip=1, colClasses=c("numeric", "numeric", "Date"))
		lat_lon<- arch_data[-1,1:2]
		colnames(lat_lon)<- c("lons", "lats")
		for(k in 1:nrow(lat_lon)){
			if(!is.na(lat_lon$lons[k]) && lat_lon$lons[k]<0){lat_lon$lons[k]<- lat_lon$lons[k]+360}
		}	
			
#~ 		sst_raster<- raster(paste("../Data/sst_test/monthly_means/", timeframe[i], ".nc", sep=""))
		sst_raster<- raster(paste("../Data/new_sst/monthly_means/", mon[i], ".nc", sep=""))
		mld_raster<- raster(paste("../Data/mld_test/disagg_mld/", timeframe[i],index,".nc", sep="")) #put thermocline depth file in here
		chloro_raster<- raster(paste("../Data/chla_test/log_chla/", timeframe[i], ".nc", sep="")) #chloro a levels file 
		eddy_raster<- raster(paste("../Data/eddy_test/EKE/monthly_means/", timeframe[i], ".nc", sep="")) #eddy kinetic energy files
		env_raster<- stack(sst_raster,  mld_raster, chloro_raster, eddy_raster)

		m1_spat<- maxent(env_raster, p=lat_lon, args=c('replicates=5', 'threshold=false', 'hinge=false', 'betamultiplier=3','responsecurves=true'))

		p<- raster::predict(m1_spat, env_raster)

#~ 		writeRaster(p, filename=paste("../Data/eco_niche/Size_", bin_size, "/", timeframe[i], sep=""), 
#~ 		writeRaster(p, filename=paste("../sandbox/Data/eco_niche/Size_", bin_size, "/", timeframe[i], sep=""), 
#~ 					overwrite=T, format="CDF", varname="monthly_suitability", varunit="", xname="long", yname="lat")
		

		m1_df<- as.data.frame(m1_spat@results, rownames=T)
		means$mean_training<- c(means$mean_training, m1_df[5,6] )
		means$mean_test<- c(means$mean_test, m1_df[8,6])
		means$standard_deviation<- c(means$standard_deviation, m1_df[9,6])

	}
#~ 	print(corr_matrix)
	return(m1_spat)
#~ 	return(means)
}
