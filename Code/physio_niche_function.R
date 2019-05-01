
plot.months<- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November","December")


#####physiological

mon<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


physio_niche<- function(bin_size, timeframe=months){
	means<- list(mean_training=c(), mean_test=c(), standard_deviation=c())
	index=0
	for(i in timeframe){
		index=index+1
		arch_data<- read.csv(paste("../sandbox/Data/arch_tag/monthly_tagging/Size_", bin_size, "/", i, ".csv", sep=""), 
					header=T, skip=1, colClasses=c("numeric", "numeric", "Date"))
		lat_lon<- arch_data[-1,1:2]
		colnames(lat_lon)<- c("lons", "lats")
		for(k in 1:nrow(lat_lon)){
			if(!is.na(lat_lon$lons[k]) && lat_lon$lons[k]<0){lat_lon$lons[k]<- lat_lon$lons[k]+360}
		}	
			
		data(wrld_simpl)		
		
		rmr_raster<- raster(paste("../sandbox/Data/RMR/Size_", bin_size, "/", mon[index], ".nc", sep=""))
		gain_raster<- raster(paste("../sandbox/Data/gain/Size_", bin_size, "/", i, ".nc", sep=""))

		
		env_raster<- stack(rmr_raster, gain_raster)
		
		
		m1_spat<- maxent(env_raster, p=lat_lon, args=c('replicates=5', 'threshold=false', 'hinge=false', 'betamultiplier=3','responsecurves=true'))#, path="../Results/physio_resp")

		m1_df<- as.data.frame(m1_spat@results, rownames=T)
		means$mean_training<- c(means$mean_training, m1_df[5,6] )
		means$mean_test<- c(means$mean_test, m1_df[8,6])
		means$standard_deviation<- c(means$standard_deviation, m1_df[9,6])
		

		
		p<- raster::predict(m1_spat, env_raster)
#~ 		writeRaster(p, filename=paste("../sandbox/Data/physio_niche/Size_", bin_size, "/", i, sep=""), 
#~ 					overwrite=T, format="CDF", varname="monthly_suitability", varunit="", xname="long", yname="lat")

	}
#~ 	print(corr_matrix)
	return(m1_spat)
#~ 	return(means)
}
		


