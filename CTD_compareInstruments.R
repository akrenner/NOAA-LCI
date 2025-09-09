## test new CTD
## compare parallel CTD casts: KBNERR 4141 and 5028
## 2024-05: 5028 and 8138

## get physOc

# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/cachePO1.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/sampleTable.RData")

rm (list = ls()); base::load ("~/tmp/LCI_noaa/cache/CNV1.RData")  ## have latest version of physOc from CTD_cleanup.R, but also have functions from datasetup.R
# rm (list = ls()); base::load ("~/tmp/LCI_noaa/cache/CNV2.RData")  # from CTD_cnv-Import.R

## from CTD_cnv-Import
# rm (list = ls()); base::load ("~/tmp/LCI_noaa/cache/CNVx0.RData")


summary (factor (physOc$CTD.serial))

## pick out 8138 and nearest 5028 casts
ctd <- subset (physOc, Date == "2024-05-14")
ctd <- subset (ctd, Match_Name %in% ctd$Match_Name [which (ctd$CTD.serial == "8138")])
ctd$Match_Name <- factor (ctd$Match_Name, ordered = TRUE)
levels (ctd$Match_Name) <- levels (ctd$Match_Name)[c(2, 3, 1)]

newC <- subset (ctd, CTD.serial == "5028")
oldC <- subset (ctd, CTD.serial != "5028")
names (oldC) <- paste0 (names (oldC), "_o")
nC <- data.frame(newC, oldC [match (paste (newC$Match_Name, newC$Depth.saltwater..m.),
  paste (oldC$Match_Name, oldC$Depth.saltwater..m.)), ])
nC <- subset (nC, !is.na (latitude_DD_o))



require ("RColorBrewer")
nC$cV <- brewer.pal (3, "Accent")[as.factor (nC$Match_Name)]

dir.create("~/tmp/LCI_noaa/media/CTDcomparison/", showWarnings = FALSE, recursive = TRUE)
pdf ("~/tmp/LCI_noaa/media/CTDcomparison/5028vs8138.pdf")
for (i in which (names (ctd) == "Density_sigma.theta.kg.m.3"):ncol (ctd)) {
  if ((nrow (subset (nC, !is.na (nC[, i]))) > 1) &&    # skip all-NA variables
    (nrow (subset (nC, !is.na (nC[, i + ncol(ctd)])))) &&
    !(names (ctd)[i] %in% c("Depth.saltwater..m.", "Pressure..Strain.Gauge..db."))) {
    plot (nC [, i], nC[, i + ncol (ctd)], main = names (nC)[i], xlab = "8138", ylab = "5028"
      # , col=brewer.pal (3, "Accent")[as.factor (nC$MatchName)] ## brew color
      # , col=c("black", "pink")
      , col = nC$cV # factor (nC$Match_Name)
      , pch = 19
      , asp = 1
    )
    abline (a = 0, b = 1)

    legend ("topleft", pch = 19, col = levels (factor (nC$cV))
      , legend = levels (factor (nC$Match_Name)), bty = "n")
    legend ("bottomright"
      , legend = c (paste0 ("r^2=", round (cor (nC [, i], nC[, i + ncol(ctd)],
        use = "p", method = "p")^2, 4))
      , paste0 ("mean (5028) = ",
        round (mean (nC [, i + ncol(ctd)], na.rm = TRUE) /
          mean (nC [, i], na.rm = TRUE), 4)
        , " x mean (8138)")
      , paste0 ('mean (5028) = mean (8138) + ',
        round (mean (nC [, i], na.rm = TRUE) -
          mean (nC [, i + ncol(ctd)], na.rm = TRUE)
        , 4)
    )), bty = "n")
  }
}
dev.off()
