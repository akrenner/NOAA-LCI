## sync all section plots and wall to GoogleDrive useing rclone

## requires rclone installation
## cross-platform somehow? -- easy in bash!

## NOT working yet!
## failing to invoke rclone from within R on windows
##

rm (list=ls())


## need to make sure that rclone is correctly installed and configured!
rcloneDir <- "/Users/Martin.Renner/Applications/rclone-v1.59.0-windows-amd64/"


if (.Platform$OS.type == "unix"){
  ## system ("rclone dedupe")
  system ("rclone dedupe remote:GulfWatch/plots --dedupe-mode newest -P")
  system ("rclone sync ~/tmp/LCI_noaa/media/CTDsections/ remote:GulfWatch/plots/CTDsections/")
}else{
    wd <- getwd()
    setwd (rcloneDir)
    ## shell or system2 ??
    shell ("./rclone sync ~/My\ Documents/tmp/LCI_noaa/media/CTDsections/ remote:GulfWatch/plots/CTDsections/")
    setwd (wd)
}





if (0){
  ## on Windows: paste this into Git-bash:
  cd Applications/rclone-v1.59.0-windows-amd64/
  ./rclone dedupe remote:GulfWatch/plots --dedupe-mode newest -P
  ./rclone sync ~/My\ Documents/tmp/LCI_noaa/media/CTDsections/ remote:GulfWatch/plots/CTDsections/ -P
  ## same for state-of-the-bay
  ./rclone sync ~/My\ Documents/tmp/LCI_noaa/media/StateOfBay/ remote:GulfWatch/plots/StateOfBay/ -P

  ## on occasion, update rclone:
  ./rclone selfupdate
}
