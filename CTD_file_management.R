#!/bin/sh

## automate movement and reoganization of CTD hex and CNV files
## still need Jim to reprocess files for alignment


rm (list = ls())



## define new destinations
nD <- "~/GISdata/LCI/CTD-startover/allCTD/edited_hex/"
instL <- c ("4141", "5028")
## clean slate -- do this by hand!
x <- unlink (nD, recursive = TRUE, force = TRUE) # not working on Win??
print (x)
for (i in 1:length (instL)){
 dir.create (paste0 (nD, instL [i]), recursive = TRUE)
}
## mkdir?


rL <- function (f, p = NULL){
  x <- list.files(paste0 ("~/GISdata/LCI/CTD-startover/Workspace/", f)
             , pattern = p, ignore.case = TRUE, recursive = TRUE
             , full.names = TRUE)
  print (length (x))
  return (x)
  }


## move about CNV files -- not bother?
# fL <- rL ("ctd-data2012-16/3_Processed data (CSV files)/")
# fL <- c (fL, rL ("ctd-data2017-21/3_Processed data (CSV files)/"))



## move about HEX files
fL <- rL("ctd-data_2012-2016/2_Edited\ HEX/") #, p = ".hex")
fL <- c(fL, rL ("ctd-data_2017-21/2_Edited\ .hex\ files/"))
fL <- c(fL, rL ("ctd-data-KBL_Interns_and_Partners/Updated\ Text\ Files\ CTD\ 2012-2013", p = ".txt"))
fL <- c(fL, rL ("YSI-2016", p = ".hex")) # Steve Kibler
print (length (fL))
rm (rL)

## manually remove duplicates -- if any -- none found


## check duplicates by name or sha256
fLs <- gsub ("^.*/", "", fL)
# any (duplicated(fLs))
fDB <- data.frame (file = fL #gsub (".*files", "", fL)
            , isD = duplicated (fLs)
)
fDB <- fDB[order (fLs, fL),]
require (openssl)
fDB$sha <- sapply (1:nrow (fDB), function (i){
  require (openssl)
  hF <- file (fL [i], open = "r", raw = TRUE)
  x <- sha256 (hF)
  close (hF)
  return (as.character (x))
})
if (any (duplicated(fDB$sha))){
  print (sum (duplicated (fDB$sha)), "files are duplicate")
  stop ("fix duplicates first")
}
# tail (fDB)
##



fDB$instN <- character (length (fL))
fDB$copy <- logical (length (fL))

for (i in 1:length (fL)){
  hF <- file (fL [i], open = "r", raw = TRUE)
  hx <- readLines(hF, n = 10)
  close (hF)
  # hx <- read.fwf(fL [i], width = 100, header = FALSE, skip = 4, n = 5)
  # better to use readLines?!
  fDB$instN [i] <- ifelse (length (grep (instL [1], hx)) > 0, instL [1]
                   , ifelse (length (grep (instL [2], hx)) > 0, instL [2], NA))
  fDB$copy [i] <- file.copy (from = fL [i]
                              , to = paste0 (nD, "/", fDB$instN [i], "/")
                              , recursive = FALSE
             , overwrite = FALSE, copy.date = TRUE)
}
print (head (fDB))
print (tail (fDB))
rm (fL, instL, fLs, fDB)



## rename any .txt files to .hex
iN <- list.files (nD, pattern = ".txt$", ignore.case = TRUE
                  , recursive = TRUE, full.names = TRUE)
file.copy (from = iN, to = gsub (".txt", ".hex", iN, fixed = TRUE)
           , copy.date = TRUE, recursive = FALSE)
unlink (iN)

## weed out undesirables, bad/air casts
iN <- list.files (nD, pattern = "bad", ignore.case = TRUE, recursive = TRUE, full.names = TRUE)
iN <- c (iN, list.files (nD, pattern = "air", ignore.case = TRUE, recursive = TRUE, full.names = TRUE))
unlink (iN)
# zip up result manually to email to Jim
# zip ("~/GISdata/LCI/CTD-startover/edited_hex.zip",
#      list.files (nD, pattern = ))

rm (hx, instN)