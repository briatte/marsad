# set to FALSE to build complete network, or use a chapter value
sample = FALSE

file = ifelse(is.character(sample),
              paste0("constitution_network_", sample, ".gexf"),
              "constitution_network.gexf")

data = ifelse(is.character(sample),
              paste0("data/network_", sample, ".rda"),
              "data/network.rda")

if(!file.exists(file)) {

  entropize <- function(x, n) {
    by = diff(range(x, na.rm = TRUE)) / 20
    return(round(x + runif(n, min = by * -1, max = by), 2))
  }
  
  load("data/marsad.rda")
  load(data)
  
  deputes$naissance = NULL

  # plot nodes with no coordinates at means
  deputes$lon[ is.na(deputes$lon) ] = mean(deputes$lon, na.rm = TRUE)
  deputes$lat[ is.na(deputes$lat) ] = mean(deputes$lat, na.rm = TRUE)
  
  # add 5% random noise to avoid overplotting
  deputes$lon = entropize(deputes$lon, network.size(net))
  deputes$lat = entropize(deputes$lat, network.size(net))
  
  network::delete.vertices(net, which(!network.vertex.names(net) %in% deputes$nom))
  rownames(deputes) = deputes$nom
  deputes = deputes[ network.vertex.names(net), ]

  deputes$degree = degree(as.sociomatrix(net))
  deputes$distance = rowMeans(geodist(as.sociomatrix(net))$gdist) # average path length

  colors = brewer.pal(9, "Set1")
  colors[6] = colors[2] # remove yellow, replace by blue
  colors[2] = "#AAAAAA" # dark grey
  colors[9] = "#EEEEEE" # light grey
  names(colors) = c("Alliance Démocratique", "Aucun bloc", "Bloc Démocrates", "Congrès Pour La République",
                    "Ettakatol", "Fidélité à La Révolution", "Mouvement Nahdha", "Transition Démocratique", "NA")
  
  colors = t(col2rgb(colors))
  
  rownames(deputes) = deputes$uid
  net %e% "source" = deputes[ net %e% "source", "nom" ]
  net %e% "target" = deputes[ net %e% "target", "nom" ]
  
  relations = data.frame(
    source = as.numeric(factor(net %e% "source", levels = levels(factor(deputes$nom)))),
    target = as.numeric(factor(net %e% "target", levels = levels(factor(deputes$nom)))),
    weight = round(net %e% "weight", 4)
  )
  relations = na.omit(relations)
  
  nodes = data.frame(id = 1:nrow(deputes), label = network.vertex.names(net))
  net = as.matrix.network.adjacency(net)
  
  position = do.call("gplot.layout.fruchtermanreingold", list(net, NULL))
  position = as.matrix(cbind(position, 1))
  colnames(position) = c("x", "y", "z")
  
  # compress floats
  position[, "x"] = round(position[, "x"], 2)
  position[, "y"] = round(position[, "y"], 2)
  
  # strong ties (upper quartile)
  q = (relations[, 3] >= quantile(relations[, 3], .75))
    
  nodecolors = lapply(deputes$bloc, function(x)
    data.frame(r = colors[x, 1], g = colors[x, 2], b = colors[x, 3], a = .3 ))
  nodecolors = as.matrix(rbind.fill(nodecolors))
  
  names(deputes)[ which(names(deputes) == "uid") ] = "url"
  deputes$url = paste0("http://www.marsad.tn/fr/deputes/", deputes$url)

  write.gexf(nodes = nodes,
             edges = relations[, -3],
             edgesWeight = 1 + (relations[, 3] >= quantile(relations[, 3], .75)),
             nodesAtt = deputes[, c("url", "pic", "circo", "sexe", "bloc", "liste", "parti",
                                    "lon", "lat", "degree", "distance") ],
             nodesVizAtt = list(position = position, color = nodecolors, size = deputes$degree),
             edgesVizAtt = list(size = relations[, 3]),
             defaultedgetype = "undirected",
             meta = list(creator = "rgexf",
                         description = "Tunisian Constitution amendment cosponsorships.",
                         keywords = "Constitution, Parliament, Tunisia"),
             output = file)

}

# kthxbye
