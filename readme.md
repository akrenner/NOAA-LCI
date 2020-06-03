R-Scripts in this folder were developed to plot SWMP data of the year previous to the 
current year against the long-term mean. Separate scripts cover wind, rain, and SST. A 
number of utility functions were stuck into the file "annualPlotFct.R". These scripts 
were developed by Martin Renner with input from Steve Baird, Karyn DeCino, Chris Guo, 
Kris Holdereid, and Ben Weitzman

To run them locally, it may be necessary to the set working directory or adjust the 
folder structure, to reflect location of these scritps. 

salinityAnnual.R relies on SeldoviaTemp.R to run first! 

## known issues ##
Salinity patterns need to be verified. 

## missing features ##
Ultimately, it is hoped to host these scripts online in a Shiny, Jupyter, or similar 
environment. 
