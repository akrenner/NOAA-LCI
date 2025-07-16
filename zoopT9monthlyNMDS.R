#!/usr/bin/env Rscript

## snipet to recreate Zoop_nMDS-T9monthly.pdf
rm (list = ls())

path2csv <- "~/tmp/LCI_noaa/cache/zoopT9monthly.csv"
require ("colorspace")




cH <- function(fac, colC, pts = nMScores [, 1:2], hull = FALSE) {
  X <- subset (pts, fac)
  # X <- subset (X, envX$Transect != 9)
  hpts <- chull (X)
  hpts <- c(hpts, hpts [1])
  require (mixtools)
  ## collect points of ellipse and draw filled polygon
  if (hull) {
    ## lines (X [hpts,], col = colC, lwd = 2)
    polygon (X [hpts, ], col = adjustcolor (colC, alpha.f = 0.4))
  } else {
    elpt <- ellipse (mu = colMeans (X), sigma = cov (X), alpha = 0.05
      , col = colC, lwd = 2
      , draw = FALSE
    )
    polygon (elpt, col = adjustcolor (colC, alpha.f = 0.4))
  }
}

T9 <- read.csv (path2csv)

## plot T9 only, by month, using nMDS as calculated above -> find best seasonal cut-offs
# pdf ("~/tmp/LCI_noaa/media/2019/Zoop_nMDS-T9monthly.pdf")
pdf ("Zoop_nMDS-T9monthly.pdf")
mCol <- rainbow_hcl (12)
T9$month <- factor (T9$month)
plot (T9 [, 1:2], col = adjustcolor (mCol, 0.7)[T9$month]
  , pch = 19
  , cex = (10 * (T9$zoopSum / T9$volSample) / max (T9$zoopSum / T9$volSample)) + 0.2
)
for (i in 1:length (levels (T9$month))) {
  cH (T9$month == levels (T9$month)[i], mCol [i], pts = T9 [, 1:2], hull = TRUE)
}
# legend ("topleft", legend = month.abb, pch = 19, col = mCol)
text (aggregate (as.matrix (T9 [, 1:2]) ~ T9$month, FUN = mean)[, 2:3], month.abb)
dev.off()

## EOF
