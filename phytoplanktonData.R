#!/usr/bin/env Rscript

## phytoplankton data update
## translate taxa
## aggregate taxa
## reshape to wide
## merge with existing table

phyt <- read.csv("~/GISdata/LCI/phytoplankton/2019-2021 phyto xiuning.csv")

## QAQC
levels (factor (phyt$Taxon))
levels (factor (phyt$Taxon.1))


ph2 <- aggregate (Abundance..cells.L.~Taxon.1+Station+Date, phyt, FUN=sum)
reshape (ph2, direction="wide", timevar=Abundance..cells.L., idvar=Taxon.1)

require ("tidyverse")
##
pivot_wider(ph2, id_cols=c (Taxon.1, Station, Date))
