## sync all section plots and wall to GoogleDrive useing rclone

## requires rclone installation
## need to make sure that rclone is correctly installed and configured
## works cross-platform

## allow deletion of files?

rm (list=ls())




if (.Platform$OS.type == "unix"){
  rcloneDir <- ""
  hm <- "~/"
}else{
  rcloneDir <- "C:/Users/Martin.Renner/Applications/rclone-v1.59.0-windows-amd64/"
  hm <- "C:/Users/Martin.Renner/Documents/"
}

## unified cross-platform rclone commands
system (paste0 (rcloneDir, "rclone dedupe remote:GulfWatch/plots --dedupe-mode newest -P"))
system (paste0 (rcloneDir, "rclone sync ", hm, "tmp/LCI_noaa/media/CTDsections/ remote:GulfWatch/plots/CTDsections/ -P"))
system (paste0 (rcloneDir, "rclone sync ", hm, "tmp/LCI_noaa/media/StateOfTheBay/ remote:GulfWatch/plots/StateOfBay/ -P"))
system (paste0 (rcloneDir, "rclone sync ", hm, "tmp/LCI_noaa/data-products/ remote:GulfWatch/data-products/ -P"))

## office docs
if (grep ("artin", getwd())){ # portability
  if (.Platform$OS.type=="windows"){
    system (paste0 (rcloneDir, "rclone sync ", hm, "myDocs/amyfiles/NOAA/currentDocs/ remote:NOAA-laptop/amyfiles/currentDocs/ -P"))
  }else{
    system ("rclone sync ~/Documents/amyfiles/NOAA/currentDocs/ remote:NOAA-laptop/amyfiles/currentDocs/ -P")
  }
}

## sync documents back and forth -- manually!
# if (.Platform$OS.type=="unix"){
if (0){
#  system ("rclone sync remote:tmp/LCI_noaa/cache/ ~/tmp/LCI_noaa/cache/ -P")
#  system ("rclone sync remote:LCIcache/CTD-cache/ ~/tmp/LCI_noaa/CTD-cache/ -P")
  system ("rclone sync remote:NOAA-laptop/amyfiles/currentDocs/ ~/Documents/amyfiles/NOAA/currentDocs/ -P")
  system ("rclone sync remote:GulfWatch/data-products/ ~/tmp/LCI_noaa/data-products/")
}

