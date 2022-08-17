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
# system ("rclone sync ~/tmp/LCI_noaa/data-products remote:NOAA-laptop/tmp/LCI_noaa/")
}else{
  ## more elegant to do this all in R, but failed to get this to work.
  ## if this isn't working, copy and paste from GoogleDriveRclone.sh
    shell ("GoogleDriveRclone.sh")




    #    shell (shell="C\\:/Program\\ Files/Git/git-bash.exe", cmd="ls")
    #    shell (shell="git-bash.exe", cmd="ls")

## get rclone to work with something like this?
#    system("cmd.exe"
#           , input = paste('"C:/Users/Martin.Renner/Applications/R-4.1.3/bin/i386/Rscript.exe" C:/Users/Martin.Renner/Documents/myDocs/amyfiles/NOAA-LCI/ctd_odbc-export.R'))
    # system ("cmd.exe"
    #         , input=paste0 (rcloneDir
    #                         , "rclone.exe sync "
    #                         , "~/Documents/tmp/LCI_noaa/media/CTDsections/ "
    #                         , "remote:GulfWatch/plots/CTDsections/"))
    # system (shell="cmd.exe"
    #         , cmd=paste0 (rcloneDir
    #                         , "rclone.exe sync "
    #                         , '"C\:/Users/Martin Renner/Documents/My Documents/tmp/LCI_noaa/media/CTDsections/" '
    #                         , "remote:GulfWatch/plots/CTDsections/"))
    # shell (shell="cmd.exe"
    #         , cmd=paste0 (rcloneDir
    #                         , "rclone.exe sync "
    #                         , '"~/tmp/LCI_noaa/media/CTDsections/" '
    #                         , "remote:GulfWatch/plots/CTDsections/"))

    # shell (shell="cmd.exe", cmd=paste0 (rcloneDir
    #                                     , "rclone.exe sync "
    #                                     , "~/Documents/tmp/LCI_noaa/media/CTDsections/ "
    #                                     , "remote:GulfWatch/plots/CTDsetions/"))


    # ## shell or system2 ??
    # setwd (wd)
    # shell (paste0 (rcloneDir, "rclone.exe sync ~/Documents/tmp/LCI_noaa/media/CTDsections/ "
    #                , "remote:GulfWatch/plots/CTDsections/"), translate=TRUE)
}


## sync documents back and forth
# if (.Platform$OS.type=="unix"){
if (0){
#  system ("rclone sync remote:tmp/LCI_noaa/cache/ ~/tmp/LCI_noaa/cache/ -P")
#  system ("rclone sync remote:LCIcache/CTD-cache/ ~/tmp/LCI_noaa/CTD-cache/ -P")
  system ("rclone sync remote:NOAA-laptop/amyfiles/currentDocs/ ~/Documents/amyfiles/NOAA/currentDocs/ -P")
  system ("rclone sync remote:GulfWatch/data-products/ ~/tmp/LCI_noaa/data-products/")
}

