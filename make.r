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

# colors

colors = brewer.pal(9, "Set1")
colors[6] = colors[2] # replace yellow by blue
colors[2] = "#AAAAAA" # dark grey (no bloc)
colors[9] = "#EEEEEE" # light grey (missing)
names(colors) = c("Alliance Démocratique", "Aucun bloc", "Bloc Démocrates",
                  "Congrès Pour La République", "Ettakatol",
                  "Fidélité à La Révolution", "Mouvement Nahdha",
                  "Transition Démocratique", "NA")

# run

source("code/data.r") # retrieve MPs and Constitution amendments
source("code/elec.r") # get, model and plot electoral amendments
source("code/ergm.r") # model Constitution cosponsorship network
source("code/gexf.r") # export Constitution cosponsorship network
source("code/vote.r") # get, model and plot roll call votes

# have a nice day
