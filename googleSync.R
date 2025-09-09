## sync working directory to GoogleDrive for ease of telework -- when not yet ready to commit to GitHub

## requires rclone installation
## need to make sure that rclone is correctly installed and configured
## works cross-platform

## allow deletion of files?

q()  ## run this manually, not automatically!

rm (list = ls())




if (.Platform$OS.type == "unix") {
  rcloneDir <- ""
  hm <- "~/"
  docs <- paste0 (hm, "Documents/")
} else {
  rcloneDir <- "C:/Users/Martin.Renner/Applications/rclone-v1.59.0-windows-amd64/"
  hm <- "C:/Users/Martin.Renner/Documents/"
  docs <- paste0 (hm, "myDocs/")
}

## sync current directory to Google Drive
system (paste0 ("rclone dedupe remote:NOAA-laptop/amyfiles/NOAA-LCI/ -P"))
system (paste0 ("rclone sync --exclude .git/ --exclude renv/ --exclude .Rproj.user/ --exclude=.DS_Store ", docs, "amyfiles/NOAA/NOAA-LCI/ remote:NOAA-laptop/amyfiles/NOAA-LCI/"
  , " -P"))



## copy Google Drive back to local
system (paste0 ("rclone sync remote:NOAA-laptop/amyfiles/NOAA-LCI/ ", docs, "amyfiles/NOAA/NOAA-LCI-gdcopy/ -P"))





#
# system (paste0 (rcloneDir, "rclone dedupe remote:GulfWatch/plots --dedupe-mode newest -P"))
# system (paste0 (rcloneDir, "rclone sync ", hm, "tmp/LCI_noaa/media/CTDsections/ remote:GulfWatch/plots/CTDsections/ -P"))
# system (paste0 (rcloneDir, "rclone sync ", hm, "tmp/LCI_noaa/media/StateOfTheBay/ remote:GulfWatch/plots/StateOfBay/ -P"))
# system (paste0 (rcloneDir, "rclone sync ", hm, "tmp/LCI_noaa/data-products/ remote:GulfWatch/data-products/ -P"))
#
# ## office docs -- specific to Martin Renner's computer!
# if (grep ("artin", getwd())){ # portability
#     system (paste0 (rcloneDir, "rclone sync ", docs, "amyfiles/NOAA/currentDocs/ remote:NOAA-laptop/amyfiles/currentDocs/ -P"))
# }
#
# ## sync documents back and forth -- manually!
# # if (.Platform$OS.type=="unix"){
# if (0){
# #  system ("rclone sync remote:tmp/LCI_noaa/cache/ ~/tmp/LCI_noaa/cache/ -P")
# #  system ("rclone sync remote:LCIcache/CTD-cache/ ~/tmp/LCI_noaa/CTD-cache/ -P")
#   system ("rclone sync remote:NOAA-laptop/amyfiles/currentDocs/ ~/Documents/amyfiles/NOAA/currentDocs/ -P")
#   system ("rclone sync remote:GulfWatch/data-products/ ~/tmp/LCI_noaa/data-products/")
# }
#
