## sync all section plots and wall to GoogleDrive useing rclone

## requires rclone installation
## need to make sure that rclone is correctly installed and configured
## works cross-platform

## allow deletion of files?

rm (list = ls())




if (.Platform$OS.type == "unix") {
  rcloneDir <- ""
  hm <- "~/"
  docs <- paste0 (hm, "Documents/")
  excludeF = ".DS_Store"
} else {
  rcloneDir <- "C:/Users/Martin.Renner/Applications/rclone-v1.65.0-windows-amd64/"
  hm <- "C:/Users/Martin.Renner/Documents/"
  docs <- paste0 (hm, "myDocs/")
}
GD <- "remote:GulfWatch/"
## accessing shared kasitsna bay lab folder directly: currently not allowed
## use shortcut/link from GulfWatch on personal drive to shared drive for now


## unified cross-platform rclone commands
system (paste0 (rcloneDir, "rclone dedupe remote:GulfWatch/plots --dedupe-mode newest -P"))
system (paste0 (rcloneDir, "rclone sync ", hm, "tmp/LCI_noaa/media/CTDsections/ ", GD, "plots/CTDsections/ -P"))
system (paste0 (rcloneDir, "rclone sync ", hm, "tmp/LCI_noaa/media/StateOfTheBay/ ", GD, "plots/StateOfBay/ -P"))
system (paste0 (rcloneDir, "rclone sync ", hm, "tmp/LCI_noaa/data-products/ ", GD, "data-products/ -P"))
## clone cache for convenience (e.g. SWMP and noaar  downloads)?
system (paste0 (rcloneDir, "rclone sync ", hm, "tmp/LCI_noaa/cache/SWMP/ remote:NOAA-laptop/cache/SWMP/ -P"))
system (paste0 (rcloneDir, "rclone sync ", hm, "tmp/LCI_noaa/cache/ remote:NOAA-laptop/cache -P"))


## office docs -- specific to Martin Renner's computer!
## from laptop to GoogleDrive
if (1) {
  if (grep ("artin", getwd())) { # portability
    system (paste0 (rcloneDir, "rclone sync ", docs, "amyfiles/NOAA/currentDocs/ remote:NOAA-laptop/amyfiles/currentDocs/ -P"))
  }
}



## sync documents back and forth -- manually!
# if (.Platform$OS.type=="unix"){
if (0) {
  #  system ("rclone sync remote:tmp/LCI_noaa/cache/ ~/tmp/LCI_noaa/cache/ -P")
  #  system ("rclone sync remote:LCIcache/CTD-cache/ ~/tmp/LCI_noaa/CTD-cache/ -P")
  system ("rclone sync remote:NOAA-laptop/amyfiles/currentDocs/ ~/Documents/amyfiles/NOAA/currentDocs/ -P")
  system (paste0 ("rclone sync ", GD, "data-products/ ~/tmp/LCI_noaa/data-products/ -P"))
  system ("rclone sync remote:NOAA-laptop/cache/ ~/tmp/LCI_noaa/cache/ -P")
  system (paste0 ("rclone sync ", GD, "plots/CTDsections/ ~/tmp/LCI_noaa/media/CTDsections/ -P"))
  #  system (paste0 ("rclone sync ", GD, "plots/CTDcasts/ ~/tmp/LCI_noaa/media/CTDcasts/ -P"))
  system (paste0 ("rclone sync ", GD, "plots/StateOfBay/ ~/tmp/LCI_noaa/media/StateOfBay/ -P"))
}



## sync from GoogleDrive back to local -- careful!!
if (1) {
  ## any more recent files on GoogleDrive?
  system (paste0 (rcloneDir, "rclone check remote:NOAA-laptop/amyfiles/currentDocs/ ", docs, "amyfiles/NOAA/currentDocs/"))
  if (0) {
    system (paste0 (rcloneDir, "rclone sync remote:NOAA-laptop/amyfiles/currentDocs/ ", docs, "amyfiles/NOAA/currentDocs/"))
  }
}


## one-off: upload drifterVideo
# system (paste0 (rcloneDir, "rclone sync ", hm, "tmp/LCI_noaa/media/drifter/drifterVideo/ ", GD, "plots/drifterVideo/ -P"))
