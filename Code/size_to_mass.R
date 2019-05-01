#these are the size classes from Estess 2014
#65-74
#75-96
#103-140
#146-209

#i need to fill the gaps
CFL_to_SFL<- function(CFL){
	if(65<=CFL && CFL<75){
		SFL<- 1.0381*CFL-5.7719
	}
	else if(75<= CFL && CFL<100){
		SFL<- .9206*CFL + 4.0604
	}
	else if(100 <=CFL && CFL< 140){
		SFL<- .9738*CFL -.256
	}
	else if(140<= CFL && CFL<=209){
		SFL<- .8942*CFL +5.442
	}
return(SFL)
}

#these size classes were established by using the calc values from CFL_to_SFL's boundaries
SFL_to_CFL<- function(SFL){
	if(60<=SFL && SFL<73){
		CFL<- (SFL+5.7719)/1.0381
	}
	else if(73<= SFL && SFL<100){
		CFL<- (SFL-4.0604)/.9206
	}
	else if(100 <=SFL && SFL< 140){
		CFL<- (SFL+.256)/.9738
	}
	else if(140<= SFL && SFL<=209){
		CFL<- (SFL-5.442)/.8942
	}
return(CFL)
}

SFL_to_kg<- function(SFL){
	M<- 4.98*10^(-6)*SFL^(3.3186)
	return(M)
}

CFL_to_kg<- function(CFL){
	SFL<- CFL_to_SFL(CFL)
	M<- SFL_to_kg(SFL)
	return(M)
}

#need to do a kg to SFL 
