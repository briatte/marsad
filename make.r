# scraper
library(readr)
library(rvest)
library(dplyr)
library(lubridate)

# roll calls
library(pscl)
library(oc)
library(anominate)

# networks
library(rgexf) # keep first
library(network)
library(sna)

# plots
library(ggplot2)
library(grid)
library(scales)
library(tidyr)

# folders
dir.create("data"           , showWarnings = FALSE)
dir.create("gexf"           , showWarnings = FALSE)
dir.create("gexf/photos"    , showWarnings = FALSE)
dir.create("plots"          , showWarnings = FALSE)
dir.create("plots/networks" , showWarnings = FALSE)
dir.create("raw"            , showWarnings = FALSE)
dir.create("raw/indexes"    , showWarnings = FALSE)
dir.create("raw/amendments" , showWarnings = FALSE)
dir.create("raw/mps"        , showWarnings = FALSE)
dir.create("raw/votes"      , showWarnings = FALSE)
dir.create("raw/votes/2011" , showWarnings = FALSE)
dir.create("raw/votes/2014" , showWarnings = FALSE)

# colors
blocs_2011 = c(
  "Aucun bloc" = "#AAAAAA",
  "Bloc Nahdha" = "#377EB8", # blue
  "Bloc Wafa" = "#B3DE69", # light green
  "Transition Démocratique" = "#FF7F00", # orange
  "Bloc Démocrates" = "#FFFF33", # yellow
  "Bloc CPR" = "#4DAF4A", # green
  "Bloc Ettakatol" = "#B2182B", # dark red
  "Alliance Démocratique" = "#80B1D3" # light blue
)

blocs_2014 = c(
  "Aucun bloc" = "#AAAAAA",
  "Bloc Nahdha" = "#377EB8", # blue
  "Nidaa Tounes" = "#E41A1C", # red
  "Afek Tounes" = "#FDB462", # light orange
  "Bloc Social-Démocrate" = "#A65628", # brown
  "Front Populaire" = "#C51B7D", # magenta
  "Union Patriotique Libre" = "#4DAF4A" # green
)

# geo = c("Allemagne", "Amérique et reste de l'Europe", "France 1", "France 2",
#         "Italie", "Pays arabe et reste du monde")

vot = c(
  "yea" = "#00A08A",
  "nay" = "#FF0000",
  "abstain" = "#F2AD00",
  "absent" = "#F98400",
  "excused" = "#5BBCD6",
  "missing" = "#AAAAAA"
)

# make
source("code/01-get-mps.r")
source("code/02-get-amendments.r")
source("code/03-get-networks.r")
source("code/04-get-votes.r")
source("code/05-get-rollcall-matrixes.r")
source("code/06-get-rollcall-objects.r")
source("code/07-get-idealpoints.r")
source("code/08-plot-idealpoints.r")

# done
