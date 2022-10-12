#!/usr/bin/env Rscript

## phytoplankton data update
## translate taxa
## aggregate taxa
## reshape to wide
## merge with existing table

rm (list = ls())
phyt <- read.csv("~/GISdata/LCI/phytoplankton/2019-2021 phyto xiuning.csv")
tax <- read.csv ("~/GISdata/LCI/phytoplankton/phytoTaxonomy.csv")
phyt$newTax <- tax$lowresTax [match (phyt$Taxon, tax$hiresTax)]

if (0){
  ## QAQC
  levels (factor (phyt$Taxon))
  levels (factor (phyt$Taxon.1))
  ## generate taxonomy lookup table
  x <- levels (factor (phyt$Taxon))
  y <- phyt$Taxon.1 [match (x, phyt$Taxon)]
  tax <- data.frame (hiresTax=x, lowresTax=y)
  write.csv (tax, file="~/GISdata/LCI/phytoplankton/phytoTaxonomy.csv"
               , row.names=FALSE, quote=FALSE)
  rm (x, y)
}

ph2 <- aggregate (Abundance..cells.L.~Taxon.1+Station+Date, phyt, FUN=sum)
reshape (ph2, direction="wide", timevar=Abundance..cells.L., idvar=Taxon.1)



bigP <- read.csv ("~/GISdata/LCI/phytoplankton/phytodata-kbl-Oct2022.csv")

levels (factor (tax$lowresTax)) [is.na (match (gsub ("[\ \\(\\)]", ".", levels (factor (tax$lowresTax)))
                                                     , names (bigP)))]


gsub (" ", ".", tax$lowresTax, fixed=TRUE)
