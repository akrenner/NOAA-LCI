## Processing of KBL physical and biological oceanographic data

Files in these script represent three overlapping projects, using data from 
pelagic and shore-based stations in Kachemak Bay and lower Cook Inlet. There is
some inter-dependence of these files: runAll.R is the place to start. 

Running all steps in this script will accomplish the following tasks:
- Process all CTD data from edited hex files to annual aggregated files. 
- Generate and update plots for each cast, for each transect/section
- If connected to VPN, fetch recent SWMP data and update plots for monthly 
reports. For an initial installation, this has to be done manually from 
https://cdmo.baruch.sc.edu/get/landing.cfm

Location matters: some of these scripts require a specific directory structure.
These scripts were developed by Martin Renner with input from Steve
Baird, Paul Cziko, Karyn DeCino, Chris Guo, Kris Holdereid, and Ben Weitzman.
A complete documentation of the workflow, from sampling to analysis, can be 
found in the file "Manual.md". A version of this manual is also mirrored to the 
Google Drive: Kasitsna Bay Lab/Administration/SOPs
