#!/bin/sh

  cd ~/Applications/rclone-v1.59.0-windows-amd64/
  ./rclone.exe dedupe remote:GulfWatch/plots --dedupe-mode newest -P
  ## plots and data products
  ./rclone.exe sync ~/Documents/tmp/LCI_noaa/media/CTDsections/ remote:GulfWatch/plots/CTDsections/ -P
  ./rclone.exe sync ~/Documents/tmp/LCI_noaa/media/StateOfTheBay/ remote:GulfWatch/plots/StateOfBay/ -P
  ./rclone.exe sync ~/Documents/tmp/LCI_noaa/data-products/ remote:GulfWatch/data-products/ -P

  ## office docs
  ./rclone.exe sync ~/Documents/myDocs/amyfiles/NOAA/currentDocs/ remote:NOAA-laptop/amyfiles/NOAA/currentDocs/ -P

  ## tmp files
#  ./rclone.exe sync ~/Documents/tmp/LCI_noaa/cache/ remote:NOAA-laptop/LCIcache/cache/ -P
#  ./rclone.exe sync ~/Documents/tmp/LCI_noaa/CTD-cache/ remote:NOAA-laptop/LCIcache/CTD-cache/ -P
  ## on occasion, update rclone:
  # ./rclone.exe selfupdate

# eof