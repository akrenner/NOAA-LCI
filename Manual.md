---
title: "Kachemak Bay Sampling Manual"
author: "Martin Renner"
date: "2023-04-06"
output: pdf_document
keywords: "NOAA, NCCOS, Kasitsna Bay Lab, Kachemak Bay, Cook Inlet, CTD, nutrients
---

## Kachhemak Bay Ecological Sampling Protocols

NOAA's Kasitsna Bay Lab, in collaboration with the Kachemak Bay Estuarine Research Reserve, has been 
conducting oceanographhic and ecological long-term monitoring in Kachemak Bay since 2012. This document 
is based on previous versions compiled by Kim Schuster and James Schloemer. Please direct question to 
Martin Renner martin.renner@noaa.gov or Kris Holderied  kris.holderied@noaa.gov.

"Printed on `r format(Sys.time(), '%d %B, %Y')`"

Previous versions included zooplankton, and OA sampling, projects that have been suspended for now.


## TOC

[//]: # checked Jim's printed pages -- done 
[//]: # incorporated Kim's manual XXX


## Sampling preparations

### CTD
Prior to sampling, the CTD status must be checked to ensure that there is sufficient battery power, logging space, and that settings are correct. Connect to the CTD using SeaTerm v.159 (not SeatermV2), then click 'Connect", then 'Status'. Check that date and time are correct. [//]: # if not, correct like this...XXX **code** adjust time and date in seaterm: 
- vbatt: battery must be greater than 12 V. If lower, change batteries.
- vlith: replace lithium battery if vlith is < 7 (https://rts.as/wp-content/uploads/2018/09/Seabird-SBE-19plus-Profiler-CTD-manual.pdf page 113).
- casts: CTD stores up to 299 casts. If approaching 200 casts, download all data and clear CTD memory by clicking 'Init Log' button. DO NOT click this button until all data has been downloaded and confirmed to be adequate. mode: MUST be 'profile'. 
[//]: # XXX set time and date -- adjust XXX insert screenshot

### iPad
FileMaker database: sync to iPad. Write-down last ctd-cast number. 

### staging gear Have the following items handy to bring to the boat:
- note book
- pencils
- CTD
- Niskin bottle 
- phytoplankton bottles (small, white): 6 for monthly 
- nutrient bottles (0.5l brown nalgene): 12 for monthly 
- Otter-bag with:
  - iPad + charging cable 
  - zip ties 
  - this protocol 
  - clip-board with data sheets 
  - MESSANGER WEIGHT 
  - stop-watch 
- Toolbox with: 
  - multimeter 
  - crescent wrench 
  - zip ties 
  - needlenose pliers 
  - electrical tape 
  - phillips screw driver 
  - regular screw driver 
  - leatherman tool


## Monthly and quarterly sampling

Monthly samplinng covers transects AlongBay and T9. CTD casts at each station. Time permitting, also do an eBird point count. At stations AB-3, AB-6(=T9-6), and AB-10 also sample nutrients, chlorophyll, and phytoplankton. Quarterly sampling extends AlongBay transect to T7-22 and also covers T6-3, AB-POGR, and AB-POPT and adds Transect 4. Additional water samples: XXX

### CTD
Verbalize turning on the CTD at the beginning of each cast. Lower the instrument to about 3m, raise it up to the surface (leaving the water intake port well submerged) and let it soak for at least 1 minute (use stop-watch). The lower it to about 5 m above the sounded depth (avoid touching the bottom). On retrieving the instrument, verbalize switching it off.

### Water sampling
Niskin bottle water samples as well as bucket surface water samples are taken at pre-determined stations. XXX details?

### Phytoplankton
Pour 10 l, 20 l, or 40 l (depending on season) of seawater through the 20 μm, 20 cm diameter plankton net. Wash the ouside of net down with ambient sea water Collect samples in white plastic bottles. Preserve with 8 drops of Lugal's solution, of practical.

### Notes
Pay attention to times (local time) and station names.

Open FileMakerMoblile on iPad, then open LTMdatabase At each station, press "new station".


## Post-cruise cleanup

-   Rinse CTD, Niskin-bottle, nets, everything that was in the water, with freshwater.
-   Flush CTD with DI water: Attach syringe to lower water-intake port and flush with at least 3 full syringes.
-   Store water samples in fridge overnight, if not processing right away. Watersamples need to be processed and frozen within XX hours of collecting. XXX


## Sample processing

### Nutrients
Piston-sampler syringes stored by pump. Rinse syringe with DI water. Filter onto syringe (use one for all samples, then discard). Press 30 ml from each water sample (brown bottle) through filter into labeled centrifuge plastic vial. Label: date (YYYY-MM-DD), station, deep/surface, replicate 1/2 Freeze in sample freezer.

### Chlorophyll
Use nitril gloves. 25 mm filter paper onto vacuum pump attach 200 ml funnel, fill to 200 ml line turn on vacuum pump bleed vacuum when samples have passed put filter paper into labeled screw-top glass tube freeze dump any seawater outside, not into sink.

### Phytoplankton
Add 8 drops of Lugol's solution to each white sample bottle, if not already treated on board. Leave in Dom's marked drawer.


## Data QAQC, analysis, and archiving
Before CTD data can be used, files have to be downloaded from the instrument, matched with the data from notebooks, and converted from hex to .csv files. Most of these steps ave now been automated. 

### Download CTD
Connect to CTD, as before.
[//]: # Copy from Jim's instructions. 
Two options: batch-downloading and attended download. 
#### Batch download and processing with FileMaker and R
Copy/sync filemaker database back to computer. 
Connect to CTD, as before and display headers. Compare headers to recorded times in FileMaker database, and enter cast numbers. 
Export notesTable (script) and close FileMaker. Open R and run hexEdit.R. Manually inspect resultant edited hex files and copy 
them with containing folder to the appropriate place in 2_edited_hex_files. 

Generate NoteBook pdf from FileMaker and upload it to the WorkSpace. 
https://researchworkspace.com/campaingn/2562960/evos-gulf-watch-2017-ongoing Environmental Drivers: Oceanographic monitoring in Cook Inlet and Kachemak Bay > Data, 2017-ongoing > Fiel notes > "year"
[//]: # Producing this pdf needs details.

#### Attended download
If working manually: scan handwritten notes to pdf. Then upload that pdf to the WorkSpace, as above. 

#### Process and archive CTD files
See Appendix below for the necessary set-up and software installation. To process new CTD files, enter the following commands in R: 

```
rFolder <- "~/myDocs/R-scripts"
## set up folder for R scripts and pull scripts from github
setwd (rFolder)
system ("git update")
# source ("runAll.R")
source ("ctd_processing.R")
````
This will batch-process CTD files using SEABIRD software and do some basic QAQC. This may take a while. After a successful run, plots can be found in *~/tmp/LCI_noaa/media/*

#### Run a small example dataset
Produce a section plot from an individual survey and transect. [//]: #XXX fill in details



# Appendix: 

## Set-up new computer to communicate with CTD

All installations may need to be done from an admin-account. 
- Install driver PL2303_Prolific_DriverInstaller_v1200 from https://www.prolific.com.tw/US/ShowProduct.aspx?p_id=225&pcid=41 -- this may or may not be necessary?
- Connect serial cable with USB-to-serial adapter. In device manager, install driver for USB-to-serial adapter (should show up under "other" and be marked with a yellow warning triangle). 
- Install the Seasoft 2.3 software suite from seabird.com:  https://www.seabird.com/cms-view-en.jsa?page=/cms/list-items/seasoft-2-3-0-en.jsp  

Open Seaterm v 1.59. Configure *SBE 19 plus ...* Communication settings: 
* No parity 
* 8 Data bits 
* 9600 boud 
* Mode: RS-232 full duplex
Depending on your RS-232 to USB adapter, your COM-port may be configured on port-6 or on another. Select "Connect" and see whether communications to the instrument can be established. 


## Setup of local environment for data analysis

### Windows

The following instructions work under Windows 11. Install the following software: 
<!--- [##]: Install required software. Estimated time: --->
- R, version 4.0.0 or later https://cran.r-project.org/bin/windows/base/release.html
- git https://git-scm.com/download/win
Both of these packages can be installed without administrator privileges in the user directory. To work with R, it is recommended to use a IDE, like RStudio (admin rights required for installation) https://www.rstudio.com/categories/rstudio-ide/
In addition, a number of add-on R packages, data-files, and folder-structure are required. These will be automatically set-up with the instructions given below. 

Open R and paste the following lines of code to pull CTD data, configuration files, and put them in the appropriate places. Location of the data is hard-coded, so the scripts can find them, but if you like, you can modify the location where R-scripts are stored (first line). 
````
rFolder <- "~/myDocs/R-scripts"
## set up folder for R scripts and pull scripts from github
dir.create (rFolder , recursive = TRUE)
setwd (rFolder)
system ("git clone https://github.com/akrenner/NOAA-LCI.git")
## set up folder for data and pull data from github
dir.create ("~/GISdata/", recursive = TRUE)
setwd ("~/GISdata/")
system ("git clone https://github.com/akrenner/LCI.git")
````
Advanced: In order to push changes to code or data back into the repository and that way share them, you may have to generate a token on the githup.com website. It is recommended to generated a ssh key for passwordless communication.  


### macOS or gnu/linux

All R code for this project is cross-platform compatible. While it is possible to run all of this on a macOS or gnu/linux platform as well, proceeding here is only recommended for advanced users. The main stumbling block is processing hex-files using seabird, Inc. software, which is only available as a windows executable. It is possible to run this software on macOS or gnu/linux platforms, either within a virtual environment, like VirtualDesktop, or using wine. 
<!---
The following approach has worked for me (macOS 13.3, Ventura, x86). Your mileage may vary, especially 
if running Apple silicone.
[//]: # Following https://github.com/Gcenx/wine-on-mac
[//]: # also check out https://www.sysnettechsolutions.com/en/install-wine-macos/
If not present already, install homebrew: https://brew.sh/ Install wine, using homebrew: brew install --no-quarantine gcenx/wine/wine-crossover
[//]: # show this code, but do not execute -- how?
Configure wine setup in your user account: provide access to the relevant folders.
To create a new pure 32-bit prefix, you can run: \$ WINEARCH=win32 WINEPREFIX=\~/.wine32 winecfg

Copy seabird executables.
--->
## Acknowledgments: 
This would not have worked out without the help of Jim Schloemer. I also like to thank Kim Schuster for paving the way and Kris Holderied for guiding the way.  


## Outlook
Eventually, it would be desirable to implement this workflow on the workspace itself. This would require running SBEBatch.exe via ‘wine’ inside a docker container. Whether this setup would be of practical use remains to be seen. It would have the advantage that all my verbiage above is then obsolete and all processing could be done online. Whether that’s worth the trouble and a blessing or curse remains to be seen. 
There are also several open source projects to do the same calculations, which has the potential of providing performance benefits and the ability to check what is actually being calculated: 
* https://github.com/gunnarvoet/ctdproc
* https://github.com/wmruef/sumpis 
While promising, I did not evaluate these two options further at this time. Especially ctdproc appears to be under active development – watch this space. 

