###compare to Blank et al 2007
# 8deg = 331
# 15deg= 175
# 20deg= 193
# 25deg = 256


calc_E<- function(k=8.6173303*10^(-5), T1, T2, Q10){ #k= Bolztman's constant in eV/Kelvin
	E=(k*T1*T2*log(Q10, base=exp(1))/10)
	return(E)
}

calc_UTD<- function(k=8.6173303*10^(-5), T1, T2, E){
	Tc=T2-T1
	UTD<- exp(E*Tc/(k*T2*T1))
	return(UTD)
}

calc_mass_corr_R0<- function(k=8.6173303*10^(-5), T1, T2, MR0_T1, E){
	MR0_T2= MR0_T1*exp(E*(T2-T1)/(k*T2*T1))
	return(MR0_T2)
}


MR_temp_mass_BA<- function(MRT1, T1, T2, Rexp=.698, k=8.6173303*10^(-5), M){
	MR0_T1=MRT1/M^Rexp
	Q10=0
	if (293.15<=T1&T1 <=298.15 & 293.15<= T2&T2<= 298.15){Q10=1.8} #if they're both 20-25 deg
	else if (288.15<= T1&T1<= 293.15 & 288.15<= T2&T2 <=293.15){Q10=1.25} #both 15-20 deg
	else if (281.15<= T1&T1 <= 288.15 & 281.15<= T2&T2<=288.15){Q10=.41} #both 8-15 deg
	
	else if (281.15<= T1&T1<= 288.15 & T2<=281.15){Q10=.41} #if T2 is less than 8deg and 8<T1<15
	else if (281.15<= T2&T2<= 288.15 & T1<=281.15){Q10=.41} #if T1 is less tan 8deg and 8<T2<15
	else if (293.15<=T1&T1 <=298.15 &T2> 298.15){Q10=1.8} #if T2 is more than 25 deg and 20<T1<25
	else if (293.15<=T2&T2 <=298.15 &T1> 298.15){Q10=1.8} #if T1 is more than 25deg and 20<T2<25
	if (Q10 != 0){
		E<- calc_E(T1=T1, T2=T2, Q10=Q10)
		UTD= calc_UTD(T1=T1,T2=T2, E=E)
		MRT2= MR0_T1*UTD*M^Rexp
		return (MRT2)}
	if (T2<T1){	#T2 is less than T1
		if (293.15<=T1&T1<=298.15 & 288.15<=T2&T2<=293.15){ #20<T1<25, 15<T2<20
			E_a<- calc_E(T1=T1, T2=293.15, Q10=1.8)
			UTD_a<- calc_UTD(T1=T1, T2=293.15, E=E_a)
			MR0_T2_a= calc_mass_corr_R0(T1=T1, T2=293.15, MR0_T1=MR0_T1, E=E_a)
			
			E_b<- calc_E(T1=293.15, T2=T2, Q10=1.25)
			UTD_b<- calc_UTD(T1=293.15, T2=T2, E=E_b)		
			MRT2_b= MR0_T2_a*UTD_b*M^Rexp
			return(MRT2_b)
		}

		if (288.15<=T1&T1<=293.15 & 281.15<=T2&T2<=288.15){ #15<T1<20, 8<T2<15
			E_a<- calc_E(T1=T1, T2=288.15, Q10=1.25)
			UTD_a<- calc_UTD(T1=T1, T2=288.15, E=E_a)
			MR0_T2_a= calc_mass_corr_R0(T1=T1, T2=288.15, MR0_T1=MR0_T1, E=E_a)
			
			E_b<- calc_E(T1=288.15, T2=T2, Q10=.41)
			UTD_b<- calc_UTD(T1=288.15, T2=T2, E=E_b)		
			MRT2_b= MR0_T2_a*UTD_b*M^Rexp
			return(MRT2_b)
		}

		if (293.15<=T1&T1<=298.15 & 281.15<=T2&T2<=288.15){ #20<T1<25, 8<T2<15
			E_a<- calc_E(T1=T1, T2=293.15, Q10=1.8)
			UTD_a<- calc_UTD(T1=T1, T2=293.15, E=E_a)
			MR0_T2_a= calc_mass_corr_R0(T1=T1, T2=293.15, MR0_T1=MR0_T1, E=E_a)
			
			E_b<- calc_E(T1=293.15, T2=288.15, Q10=1.25)
			UTD_b<- calc_UTD(T1=293.15, T2=288.15, E=E_b)
			MR0_T2_b= calc_mass_corr_R0(T1=293.15, T2=288.15, MR0_T1=MR0_T2_a, E=E_b)
			
			E_c<- calc_E(T1=288.15, T2=T2, Q10=.41)
			UTD_c<- calc_UTD(T1=288.15, T2=T2, E=E_c)		
			MRT2_c= MR0_T2_b*UTD_c*M^Rexp
			return(MRT2_c)
		}
		
		if(293.15<=T1&T1<=298.15 & T2<281.15){ #20<T1<25, T2<8
			E_a<- calc_E(T1=T1, T2=293.15, Q10=1.8)
			UTD_a<- calc_UTD(T1=T1, T2=293.15, E=E_a)
			MR0_T2_a= calc_mass_corr_R0(T1=T1, T2=293.15, MR0_T1=MR0_T1, E=E_a)
			
			E_b<- calc_E(T1=293.15, T2=288.15, Q10=1.25)
			UTD_b<- calc_UTD(T1=293.15, T2=288.15, E=E_b)
			MR0_T2_b= calc_mass_corr_R0(T1=293.15, T2=288.15, MR0_T1=MR0_T2_a, E=E_b)
			
			E_c<- calc_E(T1=288.15, T2=T2, Q10=.41)
			UTD_c<- calc_UTD(T1=288.15, T2=T2, E=E_c)		
			MRT2_c= MR0_T2_b*UTD_c*M^Rexp
			return(MRT2_c)
		}
		
		if (288.15<=T1&T1<=293.15 & T2<281.15){ #15<T1<20, T2<8
			E_a<- calc_E(T1=T1, T2=288.15, Q10=1.25)
			UTD_a<- calc_UTD(T1=T1, T2=288.15, E=E_a)
			MR0_T2_a= calc_mass_corr_R0(T1=T1, T2=288.15, MR0_T1=MR0_T1, E=E_a)
			
			E_b<- calc_E(T1=288.15, T2=T2, Q10=.41)
			UTD_b<- calc_UTD(T1=288.15, T2=T2, E=E_b)		
			MRT2_b= MR0_T2_a*UTD_b*M^Rexp
			return(MRT2_b)
		} 
	}
	
	else if (T1<T2){ #if T1 is less than T2
		if (288.15<=T1&T1<=293.15 & 293.15<=T2&T2<=298.15){ #15<T1<20, 20<T2<25
			E_a<- calc_E(T1=T1, T2=293.15, Q10=1.25)
			UTD_a<- calc_UTD(T1=T1, T2=293.15, E=E_a)
			MR0_T2_a= calc_mass_corr_R0(T1=T1, T2=293.15, MR0_T1=MR0_T1, E=E_a)
			
			E_b<- calc_E(T1=293.15, T2=T2, Q10=1.8)
			UTD_b<- calc_UTD(T1=293.15, T2=T2, E=E_b)		
			MRT2_b= MR0_T2_a*UTD_b*M^Rexp
			return(MRT2_b)
		}

		if (281.15<=T1&T1<=288.15 & 288.15<=T2&T2<=293.15){ #8<T1<15, 15<T2<20
			E_a<- calc_E(T1=T1, T2=288.15, Q10=.41)
			UTD_a<- calc_UTD(T1=T1, T2=288.15, E=E_a)
			MR0_T2_a= calc_mass_corr_R0(T1=T1, T2=288.15, MR0_T1=MR0_T1, E=E_a)
			
			E_b<- calc_E(T1=288.15, T2=T2, Q10=1.25)
			UTD_b<- calc_UTD(T1=288.15, T2=T2, E=E_b)		
			MRT2_b= MR0_T2_a*UTD_b*M^Rexp
			return(MRT2_b)
		}
		
		if (281.15<=T1&T1<=288.15 & 293.15<=T2&T2<=298.15){ #8<T1<15, 20<T2<25
			E_a<- calc_E(T1=T1, T2=288.15, Q10=.41)
			UTD_a<- calc_UTD(T1=T1, T2=288.15, E=E_a)
			MR0_T2_a= calc_mass_corr_R0(T1=T1, T2=288.15, MR0_T1=MR0_T1, E=E_a)
			
			E_b<- calc_E(T1=288.15, T2=293.15, Q10=1.25)
			UTD_b<- calc_UTD(T1=288.15, T2=293.15, E=E_b)
			MR0_T2_b= calc_mass_corr_R0(T1=288.15, T2=293.15, MR0_T1=MR0_T2_a, E=E_b)
			
			E_c<- calc_E(T1=293.15, T2=T2, Q10=1.8)
			UTD_c<- calc_UTD(T1=293.15, T2=T2, E=E_c)		
			MRT2_c= MR0_T2_b*UTD_c*M^Rexp
			return(MRT2_c)
		}
		
		if (281.15<=T1&T1<=288.15 & T2>298.15){ #8<T1<15, 25<T2
			E_a<- calc_E(T1=T1, T2=288.15, Q10=.41)
			UTD_a<- calc_UTD(T1=T1, T2=288.15, E=E_a)
			MR0_T2_a= calc_mass_corr_R0(T1=T1, T2=288.15, MR0_T1=MR0_T1, E=E_a)
			
			E_b<- calc_E(T1=288.15, T2=293.15, Q10=1.25)
			UTD_b<- calc_UTD(T1=288.15, T2=293.15, E=E_b)
			MR0_T2_b= calc_mass_corr_R0(T1=288.15, T2=293.15, MR0_T1=MR0_T2_a, E=E_b)
			
			E_c<- calc_E(T1=293.15, T2=T2, Q10=1.8)
			UTD_c<- calc_UTD(T1=293.15, T2=T2, E=E_c)		
			MRT2_c= MR0_T2_b*UTD_c*M^Rexp
			return(MRT2_c)
		}
		
		if (288.15<=T1&T1<=293.15 & 293.15<=T2&T2<=298.15){ #15<T1<20, 25<T2
			E_a<- calc_E(T1=T1, T2=293.15, Q10=1.25)
			UTD_a<- calc_UTD(T1=T1, T2=293.15, E=E_a)
			MR0_T2_a= calc_mass_corr_R0(T1=T1, T2=293.15, MR0_T1=MR0_T1, E=E_a)
			
			E_b<- calc_E(T1=293.15, T2=T2, Q10=1.8)
			UTD_b<- calc_UTD(T1=293.15, T2=T2, E=E_b)		
			MRT2_b= MR0_T2_a*UTD_b*M^Rexp
			return(MRT2_b)
		}
	}
}


MR_B0mass<- function(B0, M, Rexp=.698){
	MR_M<- B0*M^(Rexp)
	return(MR_M)
}

####takes input temp and mass for target individual
take_mass_temp<- function(input_temp, input_mass, Rexp=.698, ref_temp=281.15, ref_MR=331, ref_mass=8.8){ #temp in Kelvin, mass in kg
	new_MR_at_temp<- MR_temp_mass_BA(MRT1=ref_MR, T1=ref_temp, T2=input_temp, M=ref_mass)
	B0<- new_MR_at_temp/ref_mass^Rexp
	new_MR_at_mass<- MR_B0mass(B0=B0,M=input_mass)
	return(new_MR_at_mass)
}

#function for calculating metabolic rate
calc_RMR<- function(temp_nc="../Data/sst/monthly_means/", timeframes=months, input_mass, mass_bin=10){ #mass_bin is which folder do you want the RMR in, so round up to nearest 10 in my case
	for(k in timeframes){
		temp<- raster(paste(temp_nc, k, ".nc", sep=""), varname="sst")
		RMR_raster<- raster(nrows=nrow(temp), ncols=ncol(temp), extent(temp), crs(temp))
		
		for(i in 1:ncell(temp)){
			K_temp<- temp[i] +273.15
			if(!is.na(K_temp)){
				RMR<- take_mass_temp(K_temp, input_mass=input_mass)
				#RMR is in mgO2/kg/h 
				#1 mg = 13.59J=.01359 kJ
				RMR_whole<- RMR*input_mass 		##ADD IN *MASS BECAUSE RMR IS mgO2/kg/h!! 
				RMR_kJ<- RMR_whole*.01359 #kJ/h
				RMR_raster[i]<- RMR_kJ #now RMR_with_temp is the hourly cost of RMR in kJ
			}
		}
		print("calcs done")
		writeRaster(RMR_raster, filename=paste("../sandbox/Data/RMR/Size_", mass_bin, "/", k, sep=""), overwrite=T, 
			format="CDF", varname="RMR", varunit="kJ_hr", xname="long", yname="lat")
			print(paste("Done with", k, sep=" "))
}}
