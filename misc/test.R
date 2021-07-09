
library(aqp)
library(soilDB)

chz <- fetchNASIS_components()
res <- component_AASHTO_GIN(chz)
