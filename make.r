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

# roll call packages

library(pscl)
library(anominate)
library(oc)

# run

source("code/data.r") # retrieve MPs and amendments
source("code/ergm.r") # cosponsorship network model
source("code/gexf.r") # export network graph
source("code/vote.r") # get, model and plot votes

# have a nice day
