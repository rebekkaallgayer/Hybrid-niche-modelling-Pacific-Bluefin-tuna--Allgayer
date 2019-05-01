mon<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


calc_cost<- function(bin_size, timeframe=months){
	index=0
	for(k in timeframe){
		index=index+1
#~ 			ingestion<- raster(paste("../Data/consumption_rates/HIF_hr/Size_", bin_size, "/", k, ".nc", sep=""), varname="HIF_hr")
			ingestion<- raster(paste("../sandbox/Data/consumption_rates/HIF_hr/Size_", bin_size, "/", k, ".nc", sep=""), varname="HIF_hr")
#~ 			RMR_raster<- raster(paste("../Data/RMR/Size_", bin_size, "/", k, ".nc", sep=""), varname="RMR")
			RMR_raster<- raster(paste("../sandbox/Data/RMR/Size_", bin_size, "/",mon[index] , ".nc", sep=""), varname="RMR")
			SDA<- calc(ingestion, function(x){x*.092})
#~ 			writeRaster(SDA, filename=paste("../Data/SDA/Size_", bin_size, "/", k, sep=""), overwrite=T, 
#~ 				format="CDF", varname="SDA", varunit="kJ_hr", xname="long", yname="lat")
			EE<- calc(ingestion, function(x){x*.27})
#~ 			writeRaster(SDA, filename=paste("../Data/EE/Size_", bin_size, "/", k, sep=""), overwrite=T, 
#~ 				format="CDF", varname="EE", varunit="kJ_hr", xname="long", yname="lat")

			cost<- overlay(SDA,EE,RMR_raster, fun=sum)
#~ 			writeRaster(cost, filename=paste("../Data/cost/Size_", bin_size, "/", k, sep=""), overwrite=T, 
			writeRaster(cost, filename=paste("../sandbox/Data/cost/Size_", bin_size, "/", k, sep=""), overwrite=T, 
				format="CDF", varname="energetic_cost", varunit="kJ_hr", xname="long", yname="lat")
	}
}


calc_gain<- function(bin_size, timeframe=months){
	for (k in timeframe){
#~ 		ingestion<- raster(paste("../Data/consumption_rates/HIF_hr/Size_", bin_size, "/", k, ".nc", sep=""), varname="HIF_hr")
		ingestion<- raster(paste("../sandbox/Data/consumption_rates/HIF_hr/Size_", bin_size, "/", k, ".nc", sep=""), varname="HIF_hr")
		cost<- raster(paste("../sandbox/Data/cost/Size_", bin_size, "/", k, ".nc", sep=""))
#~ 		cost<- raster(paste("../Data/cost/Size_", bin_size, "/", k, ".nc", sep=""))

		gain<- overlay(x=ingestion, y=cost, fun=function(x,y){return(x-y)})
#~ 		writeRaster(gain, filename=paste("../Data/gain/Size_", bin_size, "/", k, sep=""), overwrite=T, 
		writeRaster(gain, filename=paste("../sandbox/Data/gain/Size_", bin_size, "/", k, sep=""), overwrite=T, 
			format="CDF", varname="energetic_gain", varunit="kJ_hr", xname="long", yname="lat")
	}		
}

