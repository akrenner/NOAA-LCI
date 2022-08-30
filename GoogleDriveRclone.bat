:: Windows bat file to sync CTD plots, data products, and current office work to GoogleDrive

:: rclone location
set home=C:\Users\Martin.Renner
set location=%home%\Applications\rClone-v1.59.0-windows-amd64\


%location%rclone.exe dedupe remote:GulfWatch/plots/ --dedupe-mode newest -P
::  ## plots and data products
%location%rclone.exe sync %home%\Documents\tmp\LCI_noaa\media\CTDsections\ remote:GulfWatch/plots/CTDsections/ -P
%location%rclone.exe sync %home%\Documents\tmp\LCI_noaa\media\StateOfTheBay\ remote:GulfWatch/plots/StateOfBay/ -P
:: %location%rclone.exe sync %home\Documents\tmp\LCI_noaa\data-products\ remote:GulfWatch/data-products/ -P

:: ## office docs
%location%rclone.exe sync %home%\Documents\myDocs\amyfiles\NOAA\currentDocs\ remote:NOAA-laptop/amyfiles/currentDocs/ -P

::  ## tmp files
:: #  ./rclone.exe sync ~/Documents/tmp/LCI_noaa/cache/ remote:NOAA-laptop/LCIcache/cache/ -P
:: #  ./rclone.exe sync ~/Documents/tmp/LCI_noaa/CTD-cache/ remote:NOAA-laptop/LCIcache/CTD-cache/ -P
::  ## on occasion, update rclone:
::  # ./rclone.exe selfupdate

:: # eof
