txt <- read.table ("~/../Downloads/kachemakBayBirder list.txt", sep = "")


txt <- read.csv ("~/../Downloads/kachemakBayBirder list.txt")
t2 <- gsub ("(.)", "", txt)
t2 <- gsub (" U$", "", t2)
t2 <- gsub ("\\", "", t2)




gsub ("\\t.*$", "", txt [128, 1])
as.data.frame (gsub ("\\t.*$", "", txt))




t2 <- gsub ("[ \\t]*$", "", txt [19])




birds <- read.csv ("~/GISdata/LCI/derived-speciesLists/kachemakBayBirderlist.txt"
  , header = FALSE, col.names = "common", strip.white = TRUE)
nacc <-  read.csv ("~/GISdata/LCI/derived-speciesLists/NACC_list_species.csv")

## check
birds$common [which (is.na (match (birds$common, nacc$common_name)))]
## Rosy-Finch, but Hawk Owl

birds$latin <- nacc$species[match (birds$common, nacc$common_name)]
write.table (birds, file = "~/GISdata/LCI/derived-speciesLists/kachemakBayBirderLatin.txt"
  , row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE)


zoop <- read.csv ("~/GISdata/LCI/Kachemak Bay Zooplankton.csv")
zoopS <- subset (zoop, Transect %in% c("4", "9", "Bear", "KB", "Peterson Bay", "TKBay"))
sp <- levels (factor (zoopS$Species))
write(sp, file = "~/../Desktop/zoopSpecies.txt")


phyto <- read.csv ("~/GISdata/LCI/phytoplankton/phytoplankton.csv", skip = 1)
# levels (factor (phyto$Transect))
phytoS <- subset (phyto, Transect %in% c("", "4", "9", "AlongBay", "subbay"))
phytoS <- phytoS [, 8:ncol (phytoS)]
phySum <- colSums(phytoS, na.rm = TRUE)
pName <- names (subset (phySum, phySum > 0))
pName <- trimws (gsub (".", " ", pName, fixed = TRUE))
pName <- gsub ("sp|spp", "spp.", pName)
write (pName, file = "~/../Desktop/phytoSpecies.txt")
