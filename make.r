# folders

dir.create("data", showWarnings = FALSE)
dir.create("plots", showWarnings = FALSE)

# data packages

library(ggmap)
library(plyr)
library(qdap)
library(reshape)
library(stringr)
library(XML)

# network packages

library(ergm)
library(GGally)
library(network)
library(RColorBrewer)
library(rgexf)
library(sna)

# run

source("data.r") # retrieve MPs and amendments
source("ergm.r") # cosponsorship network model
source("gexf.r") # export network graph

# have a nice day
