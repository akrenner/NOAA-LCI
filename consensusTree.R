#! /usr/bin/env Rscript

## find consensus seasonal cluster tree from zoo- and phyto-plankton (and physical oceanography)

if (!requireNamespace("BiocManager", quietly = TRUE)){
  install.packages("BiocManager")
}

BiocManager::install("ConsensusClusterPlus")
require ("ConsensusClusterPlus")


## load trees

## claculate consensus

## not suitable, I think
# ConsensusClusterPlus(d = dataXX, distance = 'manhatten')



## better?
# Bioconductor:maanova
## consensus (macluster, level - 0.8, draw = TRUE)
BiocManager::install("maanova")
require ("maanova")


## bootstrap trees


# EOF
