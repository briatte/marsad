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

library(GGally)
library(RColorBrewer)
library(rgexf)
library(sna)
library(network)
library(ergm)

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

source("code/data.r")     # scrape MPs and amendments
source("code/networks.r") # build amendment networks
source("code/ergm.r")     # bloc cohesion in amendment cosponsorships
source("code/votes.r")    # bloccohesion in roll call votes

# have a nice day
