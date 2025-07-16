##########
# Author: Asmita Shukla
# Version: V.01
# Date: 04/10/2024
# Updated: 2024/08/22 by Ross Whippo

##### Packages Used######
library(tools)
library(exiftoolr)
library(dplyr)
library(stringi)
###################

## File structure for renaming:

## RootDirectory > FolderA > FolderB > images
##
## FolderA = Area code (ex: JAK01)
## FolderB = platform code and deployment number (ex: A00012  = A is AUV, 00012 is depoyment 12)

###### Parameters####Need to be specified by the user ## Notice the double slashes in the path name - either use two forward slashes or one back slash
RootDirectory <- "~/GISdata/LCI/Archimandritof_Shoals-DropCam/HERO4SilverVideos/" # Specify root directory. This root directory should have subfolders with location name/ID
DestinationDirectory <- "~/GISdata/LCI/Archimandritof_Shoals-DropCam/rename/" # This is where you want to write the renamed images. The script doesn't not alter or move original images in anyway so they can be preserved
CameraTimezone <- "AKDT"  # AKDT
####################

###### Get a list of original files and read Exif metadata to get the timestamp of the image######
Files <- data.frame(list.files(RootDirectory, recursive = TRUE,
  full.names = TRUE, pattern = ".JPG|.jpg|.jpeg|.JPEG|.png|.PNG|.TIF|.tif|.TIFF|.tiff|.MP4|.mp4")) # specifies image file extensions so it leaves any other files untouched
colnames(Files) <- "FileName"

Metadata <- exif_read(Files$FileName)

# Depending on the camera, sometime createdate may be missing - if that is the case it uses modifydate
if ("CreateDate" %in% names(Metadata)) {
  Metadata <- Metadata[, c("SourceFile", "CreateDate")]
} else {
  Metadata <- Metadata[, c("SourceFile", "FileModifyDate")]
}

colnames(Metadata) <- c("orig_filepath", "coltime")
Metadata$fileExt <- paste0(".", file_ext(Metadata$orig_filepath))
Metadata$LocationID <- sapply(strsplit(Metadata$orig_filepath, "/"), `[`, 7)
Metadata$Equipment <- sapply(strsplit(Metadata$orig_filepath, "/"), `[`, 8)
# As long as the deployment folder name ends in a number, we are good with the next line. 012, 0012, 12 - doesn't matter - it will format it to four digits
Metadata$DeploymentID <- sprintf("%04d", as.numeric(stri_extract_last_regex(Metadata$Equipment, "[0-9]+")))
Metadata$EquipmentID <- toupper(substr(Metadata$Equipment, 1, 1))
Metadata$coltime <- format(as.POSIXct(Metadata$coltime, "%Y:%m:%d %H:%M:%S", tz = ""), "%Y%m%dT%H%M%S")
Metadata$NewFilename <- paste0(Metadata$LocationID, "_", Metadata$EquipmentID, Metadata$DeploymentID, "_", Metadata$coltime, CameraTimezone, Metadata$fileExt)

# Creates the destination directory specified in the parameters section if it doesn't already exist
if (!dir.exists(DestinationDirectory)) {
  dir.create(DestinationDirectory)
}

# Creates a progress bar so you are not wondering how fast or slow it is going

pb <- winProgressBar(title = "Image Renaming and Copying Progress Bar", # Window title
  label = "Percent completion", # Window label
  min = 0,      # Minimum value of the bar
  max = nrow(Metadata), # Maximum value of the bar
  initial = 0,  # Initial value of the bar
  width = 300L) # Width of the window

for (i in seq_len(nrow(Metadata))) {
  LocationDir <- paste0(DestinationDirectory, "\\", Metadata$LocationID[i])
  if (!dir.exists(LocationDir)) {
    dir.create(LocationDir)
  }
  EquipmentDir <- paste0(LocationDir, "\\", Metadata$EquipmentID[i], Metadata$DeploymentID[i])
  if (!dir.exists(EquipmentDir)) {
    dir.create(EquipmentDir)
  }
  file.copy(from = Metadata$orig_filepath[i], to = paste0(EquipmentDir, "\\", Metadata$NewFilename[i]))

  pctg <- paste0(round((i / nrow(Metadata)) * 100, 0), "% completed")
  setWinProgressBar(pb, i, label = pctg)
}

close(pb) # cLOSES THE PROGRESS BAR

#### All Done######
