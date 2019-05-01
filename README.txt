Written by: Rebekka Allgayer (corresponding author r.allgayer.17@abdn.ac.uk)

Useful information to running the ode and data provided in this repository, to create the results found in "Hybrid niche modelling as an alternative to correlative species distribution models in a top marine predator, Pacific Bluefin tuna"
RL Allgayer, B Block, RE Whitlock, JMJ Travis, S Pawar

The script 'run_all.R' calls all functions necessary to provide the results shown in the manuscript.
This script depends on the following workflow:

Code
	- run_all.R
	- cost_gain_functions.R
	- eco_niche.R
	- ingestion_functions.R
	- metabolic_rate_functions.R
	- physio_niche_functions.R
	- size_to_mass.R
Data
	- arch_tag
		- Size10
			- January- December.nc
		- Size20
			- January- December.nc
		- Size30
			- January- December.nc
	- chla
		- January - December.nc
	- consumption_rates
		- Size10
			- January- December.nc
		- Size20
			- January- December.nc
		- Size30
			- January- December.nc
	
Results

Dependent packages: 'ncdf4', 'raster', 'mgcv', 'dismo', 'maptools', 'sp', 'rJava', 'ROCR'

Running MaxEnt:
NOTE: in order to run this code, you need to have the 'dismo' package installed as well as Java. This is required for the maxent function to work, as it runs the analysis through java. I used the 'rJava' package to achieve this. You then have to make sure the maxent.jar file is in the correct folder. Excerpt from dismo documentation for the maxent function: " This function uses the MaxEnt species distribution model software, which is a java program that you can download here. Put the file 'maxent.jar' in the 'java' folder of this package."

 

Functions on how to get environmental data into the correct format and how to subset the tagging data is not included. This script assumes your data is already in the format you are wanting to run the model on

Information on creating the figures is also not provided but the function call 'raster()' will plot whichever raster you wish to view