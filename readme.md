Files in this script will eventually be organized in folders. For now,
they represent three overlapping projects, using data from pelagic and
shore-based stations in Kachemak Bay and lower Cook Inlet. There is
some inter-dependence of these files: runAll.R is the place to start. 

Files preparing plots for the annual State of the Bay report are
prefixed "annual-" and include "annualPlotFct.R", the later containing
functions for standardized plots used in the report. These functions
mostly use SWMP data, which has to be updated manuall. 

These scripts were developed by Martin Renner with input from Steve
Baird, Karyn DeCino, Chris Guo, Kris Holdereid, and Ben Weitzman. To
run them locally, it may be necessary to the set working directory or
adjust the folder structure, to reflect location of these scritps. 


## known issues ##
Salinity patterns need to be verified. 

## missing features ##
Ultimately, it is hoped to host these scripts online in a Shiny,
Jupyter, or similar environment. 
