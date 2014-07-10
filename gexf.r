if(!file.exists("constitution_network.gexf")) {
  
  load("data/marsad.rda")
  load("data/network.rda")
  
  network::delete.vertices(net, which(!network.vertex.names(net) %in% deputes$nom))
  rownames(deputes) = deputes$nom
  deputes = deputes[ network.vertex.names(net), ]
  
  colors = t(col2rgb(brewer.pal(9, "Set1")))
  rownames(colors) = c(unique(deputes$bloc), NA)
  
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
             nodesAtt = deputes[, c(1:8, 12, 14:15) ],
             nodesVizAtt = list(position = position, color = nodecolors),
             edgesVizAtt = list(size = relations[, 3]),
             defaultedgetype = "undirected",
             meta = list(creator = "rgexf",
                         description = "Tunisian Constitution amendment cosponsorships.",
                         keywords = "Constitution, Parliament, Tunisia"),
             output = "constitution_network.gexf")

}

# kthxbye
