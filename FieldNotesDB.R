## query acess DB
rm (list=ls())

# if (.Platform$OS.type == "unix"){
#   require (tidyr)
#   adb <- list()
#   AT <- system ("mdb-tables ~/GISdata/LCI/EVOS_LTM_tables/EVOS_LTM.accdb",intern=TRUE) %>%
#     strsplit(" +") %>%
#     unlist()
#   for (i in 1:length (AT)){
#     adb [[i]] <-
#       system (paste0 ("mdb-export ~/GISdata/LCI/EVOS_LTM_tables/EVOS_LTM.accdb ", AT [i])) %>%
#       read.csv(file=stdin())
#   }
# }else{
# }

atra <- read.csv("~/GISdata/LCI/EVOS_LTM_tables/manualExport/tblAllTransectsCTD.txt")
tra <- read.csv("~/GISdata/LCI/EVOS_LTM_tables/manualExport/tblTransectEvent.txt") ## trans
sta <- read.csv("~/GISdata/LCI/EVOS_LTM_tables/manualExport/tblStationEvent.txt") ## station and trans
sam <- read.csv("~/GISdata/LCI/EVOS_LTM_tables/manualExport/tblSampleEvent.txt")  ## station and sample
# ctd <- read.csv("~/GISdata/LCI/EVOS_LTM_tables/manualExport/tblCTDall.txt") ## sampleEv


sam$Type <- toupper (sam$Type)
sam$Type <- ifelse (sam$Type=="OCEAN ACIDIFICATION", "OA", sam$Type)
sam$Type <- ifelse (sam$Type=="CD", "CTD", sam$Type)
sam$Type <- factor (sam$Type)
summary (sam$Type)

##
sta$Transect <- tra$Transect [match (sta$TransectEvent, tra$TransectEvent)]
sam$Transect <- sta$Transect  [match (sam$StationEvent, sta$StationEvent)]
sam$Station <- sta$Station [match (sam$StationEvent, sta$StationEvent)]

## ----- QAQC -----
sam <- subset (sam, !is.na (Transect)) %>%
  subset (nchar (Date) > 3)  ## StationEvent and SampleEvent may have dates in here?? broken?

ax <- strsplit(sam$Date, " ", fixed=TRUE)
ax <- do.call(rbind, ax)[,1]
ay <- strsplit(sam$Time, " ", fixed=TRUE)
ay <- do.call (rbind, ay)[,2]
sam$DateTimeStamp <- as.POSIXct(paste (ax,ay))
rm (ax, ay)


sam$month <- as.integer (format (sam$DateTimeStamp, "%m"))
sam$year <- as.integer (format (sam$DateTimeStamp, "%Y"))
sam$SampleID <- with (sam, paste0 (Transect, "_", Station, " "
                                   , format (DateTimeStamp, format="%Y-%m-%d")))


save (sam, file="~/tmp/LCI_noaa/cache/FieldNotes.RData")
##
# z <- subset(sam, Type=="ZOOPLANKTON")



## --- summarize sampling schedule -----
# sam <- subset (sam, year > 2016)
sType <- c("CTD","NUTRIENTS","OA","PHYTOPLANKTON","ZOOPLANKTON")
sam <- subset (sam, Type %in% sType)

if (0){
## reshape sam to wide
samC <- with (sam, data.frame (year=factor (year), month=factor (month)
                               , Transect=factor (Transect), Type, Depth))
reshape (samC, idvar=c ("year", "month", "Transect"), timevar="Type", direction="wide")
reshape (samC, idvar=c ("Transect"), timevar="Type", direction="wide")


require ("tidyr")
# spread (samC, key=Type, value=Depth)
# samC %>% pivot_wider(names_from=c(month,year,Transect) ,values_from=Depth)
}


# -----------------------------------
# Build and populate sampling matrix
# -----------------------------------

require ("tidyr")
# sampleDF <- data.frame (expand.grid(month=1:12, year=2015:format (Sys.time(), "%Y")))
sampleDF <- data.frame (expand.grid(month=1:12, year=2017:2021))  ## for GWA report
row.names (sampleDF) <- apply (sampleDF, 2, as.character) %>%
  apply (MARGIN=1, FUN=paste, collapse="-")

colN <- expand.grid (transect=levels (factor (sam$Transect)), type=sType) %>%
  as.data.frame ()
row.names (colN) <- apply(colN, 2,as.character) %>%
  as.data.frame %>%
  mutate (across(everything(), gsub, pattern="^AlongBay", replacement="AB")) %>%
  apply (MARGIN=1, FUN=paste, collapse="-")

sampleM <- matrix (0, nrow=nrow (sampleDF), ncol=nrow (colN)
                   , dimname=list (row.names(sampleDF), row.names(colN)))
# rm (colN, sampleDF)

## populate sampleDF with db records
## XXX fails when nrow < 1 XXX -- better to revert to reshape?
for (i in 1:nrow (sampleM)){
  for (j in 1:ncol (sampleM)){
    sC <- subset (sam, year==sampleDF$year [i]) %>%
      subset (month==sampleDF$month [i]) %>%
      subset (Transect==colN$transect [j]) %>%
      subset (Type==colN$type [j])
      nrow()
    sampleM [i,j] <- sC
  }
}
xT <- sampleM

xAxis <- function (side=3, Cx=1,...){
  axis (side=side, at = ((1:ncol (xT))-1)/(ncol (xT)-1)
        , labels = colnames(xT), tick = FALSE, cex.axis = Cx, ...) # axis is 0:1
}

yAxis <- function (lab, mx, CA=1, ...){
  axis (2, at = ((1:nrow (mx))-1)/(nrow (mx)-1)
        , labels = rev (lab)
        , tick = FALSE, cex.axis = CA # any bigger and labels will skip
        , ...)
}

png ("~/tmp/LCI_noaa/media/sampleTable.png", height=11*100, width=5*100, res=100)
par (las=2, mar=c(1,8,8,1))
image (t (ifelse (xT>0, 1,0)), axes=FALSE)
xAxis ()
yAxis (rownames(xT), xT)
# axis (1); axis (4)
abline (v=1:ncol(xT)/ncol(xT))
# abline (h=1:nrow (xT)/nrow(xT))
dev.off()





sam$Type <- factor (sam$Type)
for (i in 1:length (levels (sam$Type))){
  cat ("\n\n", levels (sam$Type)[i], "\n")
  sT <- subset (sam, Type == levels (sam$Type)[i])
  print (aggregate(StationEvent~month+year+Transect, sT, FUN=length))
}



## --- Jim's layout: ---------------------
##
## y-axis: year and month
## x-axis: transect x measure (e.g. CTD for transect 3-AB, then OA for transects)


