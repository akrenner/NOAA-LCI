## sync all section plots and wall to GoogleDrive useing rclone

## requires rclone installation
## cross-platform somehow? -- easy in bash!

## NOT working yet!
## failing to invoke rclone from within R on windows
##

rm (list=ls())
rcloneDir <- "/Users/Martin.Renner/Applications/rclone-v1.59.0-windows-amd64/"

wd <- getwd()
setwd (rcloneDir)
system2(".\rclone.exe copy ~/tmp/LCI_noaa/media/CTDsections/ remote:GulfWatch/plots/CTDsections/")
shell ("

       ./rclone sync ~/My\ Documents/tmp/LCI_noaa/media/CTDsections/ remote:GulfWatch/plots/CTDsections/

       ")


setwd (wd)


## paste this into Git-bash:
cd Applications/rclone-v1.59.0-windows-amd64/
./rclone sync ~/My\ Documents/tmp/LCI_noaa/media/CTDsections/ remote:GulfWatch/plots/CTDsections/ -P

## same for state-of-the-bay
./rclone sync ~/My\ Documents/tmp/LCI_noaa/media/StateOfBay/ remote:GulfWatch/plots/StateOfBay/ -P


## on occasion, update rclone:
./rclone selfupdate
