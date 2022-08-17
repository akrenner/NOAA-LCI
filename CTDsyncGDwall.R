## sync all section plots and wall to GoogleDrive useing rclone

## requires rclone installation
## cross-platform somehow? -- easy in bash!

## NOT working yet on windows
## failing to invoke rclone from within R on windows
##

rm (list=ls())


## need to make sure that rclone is correctly installed and configured!
rcloneDir <- "C:/Users/Martin.Renner/Applications/rclone-v1.59.0-windows-amd64/"


if (.Platform$OS.type == "unix"){
  ## system ("rclone dedupe")
  system ("rclone dedupe remote:GulfWatch/plots --dedupe-mode newest -P")
  system ("rclone sync ~/tmp/LCI_noaa/media/CTDsections/ remote:GulfWatch/plots/CTDsections/")
  system ("rclone sync ~/Documents/amyfiles/NOAA/currentDocs/ remote:NOAA-laptop/amyfiles/currentDocs/")
  system ("rclone sync ~/tmp/LCI_noaa/data-products remote:NOAA-laptop/tmp/LCI_noaa/")
}else{
  ## more elegant to do this all in R, but failed to get this to work.
  ## can also cut&paste from GoogleDriveRclone.sh into git-bash
  ## works from bash shell -- and windows comd shell will call bash
  shell (shell="cmd.exe", cmd="GoogleDriveRclone.sh") ## no error, but not yet working -- some path in script not found
}


## sync documents back and forth
# if (.Platform$OS.type=="unix"){
if (0){
#  system ("rclone sync remote:tmp/LCI_noaa/cache/ ~/tmp/LCI_noaa/cache/ -P")
#  system ("rclone sync remote:LCIcache/CTD-cache/ ~/tmp/LCI_noaa/CTD-cache/ -P")
  system ("rclone sync remote:NOAA-laptop/amyfiles/currentDocs/ ~/Documents/amyfiles/NOAA/currentDocs/ -P")
  system ("rclone sync remote:GulfWatch/data-products/ ~/tmp/LCI_noaa/data-products/")
}

