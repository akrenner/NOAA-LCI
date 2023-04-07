---
title: "KachemakBaySampling-Manual"
author: "Martin Renner"
date: "2023-04-06"
output: pdf_document
keywords: "NOAA, NCCOS, Kasitsna Bay Lab, Kachemak Bay, Cook Inlet, CTD, nutrients
---

## Kachhemak Bay Ecological Sampling Protocols

NOAA's Kasitsna Bay Lab, in collaboration with the Kachemak Bay Estuarine Research Reserve, has been conducting oceanographhic and ecological long-term monitoring in Kachemak Bay since 2012. This document is based on previous versions compiled by Kim Schuster and James Schloemer. Please direct question to Martin Renner martin.renner@noaa.gov or Kris Holderied  kris.holderied@noaa.gov.

"Printed on `r format(Sys.time(), '%d %B, %Y')`"

Previous versions included zooplankton, and OA sampling, projects that have been suspended for now.

## TOC

[//]: # checked Jim's printed pages -- done 
[//]: # incorporated Kim's manual XXX

## Sampling preparations

### CTD

Prior to sampling, the CTD status must be checked to ensure that there is sufficient battery power, logging space, and that settings are correct. Connect to the CTD using SeaTerm v.159 (not SeatermV2), then click 'Connect", then 'Status'. Check that date and time are correct. [//]: # if not, correct like this...XXX **code** adjust time and date in seaterm: 
- vbatt: battery must be greater than 12 V. If lower, change batteries.
- casts: CTD stores up to 299 casts. If approaching 200 casts, download all data and clear CTD memory by clicking 'Init Log' button. DO NOT click this button until all data has been downloaded and confirmed to be adequate. mode: MUST be 'profile'. Ensure that CTD battery power is > X V. Lithium battery: > X V 
[//]: # XXX set time and date -- adjust XXX insert screenshot

### iPad

FileMaker database: sync to iPad. Write-down last ctd-cast number. 

### staging gear Have the following items handy to bring to the boat: 

- note books 
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

### Clean gear and stow samples
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

### Download CTD

Connect to CTD, as before.

### FileMaker database
sync iPad database back to desktop (iCloud?). Share database with collaborators.

Enter cast name into database (in lab or in field?) Produce edited files.

### QAQC, data analysis, and archive 
#### Field Notes, FileMaker
Generate NoteBook pdf from FileMaker and upload to: 
[//]: # Producing this pdf needs details.

If working manually: scan handwritten notes to pdf.

https://researchworkspace.com/campaingn/2562960/evos-gulf-watch-2017-ongoing Environmental Drivers: Oceanographic monitoring in Cook Inlet and Kachemak Bay > Data, 2017-ongoing > Fiel notes > "year"


#### R
git checkout main 
git update

Open runAll.R in RStudio. Press "Source". Batch processing everything should take about 3 hours. XXX


#### Run a small example dataset
Produce a section plot from an individual survey and transect. [//]: #XXX fill in details


# Appendix: 

## Set-up new computer to communicate with CTD
- Install driver PL2303_Prolific_DriverInstaller_v1200 from https://www.prolific.com.tw/US/ShowProduct.aspx?p_id=225&pcid=41 -- this may or may not be necessary?
- In device manager, install driver for USB-to-serial adapter (this may need an admin account). 
- Depending on your RS-232 to USB adapter, your com-port may be configured on port-5 or on another. 

- Install SEABIRD software suite:  https://www.seabird.com/cms-view-en.jsa?page=/cms/list-items/seasoft-2-3-0-en.jsp  Communication settings: 
* No parity 
* 8 Data bits 
* 9600 boud 
* Mode: RS-232 full duplex


## Setup of local environment for data analysis

### Windows

The following instructions work under Windows 11.

Install required software. Estimated time:

### macOS or gnu/linux

Most code used here is cross-platform compatible. While it is possible to run all of this on a macOS or gnu/linux platform as well, proceeding here is only recommended for advanced users. The main stumbling block is processing hex-files using seabird, Inc. software, which is only available as a windows executable. It is possible to run this software on macOS or gnu/linux platforms, either within a virtual environment, like VirtualDesktop, or using wine. The following approach has worked for me (macOS 13.3, Ventura, x86). Your mileage may vary, especially if running Apple silicone.

[//]: # Following https://github.com/Gcenx/wine-on-mac
[//]: # also check out https://www.sysnettechsolutions.com/en/install-wine-macos/

If not present already, install homebrew: https://brew.sh/ Install wine, using homebrew: brew install --no-quarantine gcenx/wine/wine-crossover
[//]: # show this code, but do not execute -- how?
Configure wine setup in your user account: provide access to the relevant folders.
To create a new pure 32-bit prefix, you can run: \$ WINEARCH=win32 WINEPREFIX=\~/.wine32 winecfg

Copy seabird executables.

