## CTD data QAQC

rm (list = ls()); load ("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")

## check histograms
## IDentify records behow/above 99%-tile

Xsig <- function(x, n = 4) {
  sH <- mean (x, na.rm = TRUE) + n * sd (x, na.rm = TRUE)
  sL <- mean (x, na.rm = TRUE) - n * sd (x, na.rm = TRUE)
  c (sL, sH)
}

## names (poAll)
pdf ("~/tmp/LCI_noaa/media/qaqc-CTC.pdf", width = 9, height = 6)
par (mfrow = c(1, 2))
for (i in which (names (poAll) == "Density_sigma.theta.kg.m.3"):
which (names (poAll) == "O2perc")) {
  cat ("\n\n", i, names (poAll)[i], "\n")
  # print (class (poAll [,i]))
  hist (poAll [, i], main = names (poAll)[i])
  box()
  boxplot(poAll [, i], main = names (poAll)[i])

  # xH <- subset (poAll, poAll [,i] > quantile (poAll [,i], 0.99, na.rm = TRUE))
  # xL <- subset (poAll [,i] < quantile (poAll [,i], 0.01, na.rm = TRUE))
  xH <- subset (poAll, poAll [, i] > Xsig (poAll [, i])[2])
  xL <- subset (poAll, poAll [, i] < Xsig (poAll [, i])[1])
  if (nrow (xH) > 0) {cat ("high\n"); print (summary (factor (xH$DateISO)))}
  if (nrow (xL) > 0) {cat ("low\n"); print (summary (factor (xL$DateISO)))}
}
dev.off()




## run seldovia/homer data through the same loop
load ("~/tmp/LCI_noaa/cache/SeldTemp.RData")
# sldvia$station <- "sldviaH"
# sldviaS$station <- "sldviaL"
# homer$station <- "homerL"
# homerS$station <- "homerH"
SWMP <- rbind (sldvia, sldviaS, homer, homerS)
rm (sldvia, sldviaS, homer, homerS)

## names (SWMP)
pdf ("~/tmp/LCI_noaa/media/qaqc-SWMP.pdf", width = 9, height = 6)
par (mfrow = c(1, 2))
for (i in which (names (SWMP) %in% c ("temp", "sal", "do_pct", "do_mgl", "ph"
  , "turb", "chlfluor"))) {
  cat ("\n\n", i, names (SWMP)[i], "\n")
  # print (class (poAll [,i]))
  hist (SWMP [, i], main = names (SWMP)[i])
  box()
  boxplot(SWMP [, i], main = names (SWMP)[i])

  # xH <- subset (poAll, poAll [,i] > quantile (poAll [,i], 0.99, na.rm = TRUE))
  # xL <- subset (poAll [,i] < quantile (poAll [,i], 0.01, na.rm = TRUE))
  xH <- subset (SWMP, SWMP [, i] > Xsig (SWMP [, i])[2])
  xL <- subset (SWMP, SWMP [, i] < Xsig (SWMP [, i])[1])
  if (nrow (xH) > 0) {cat ("high\n"); print (summary (factor (xH$DateISO)))}
  if (nrow (xL) > 0) {cat ("low\n"); print (summary (factor (xL$DateISO)))}
}
dev.off()
