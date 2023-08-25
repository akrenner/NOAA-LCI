#! /usr/bin/env Rscript

## find consensus seasonal cluster tree from zoo- and phyto-plankton (and physical oceanography)



## load trees

## claculate consensus

library ("ape")
# consensus ()




if (!requireNamespace("BiocManager", quietly = TRUE)){
  install.packages("BiocManager")
}

BiocManager::install("ConsensusClusterPlus")
require ("ConsensusClusterPlus")



## not suitable, I think
# ConsensusClusterPlus(d = dataXX, distance = 'manhatten')
ConsensusClusterPlus (d=distanceMatrix, clusterAlg = "hc")  # hclust
ConsensusClusterPlus (d=distanceMatrix, clusterAlg = "km")  # k-means


## better?
# Bioconductor:maanova
## consensus (macluster, level - 0.8, draw = TRUE)
# BiocManager::install("maanova")
# require ("maanova")


## bootstrap trees


# EOF
