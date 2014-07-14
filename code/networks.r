splits = c("full", "constit", "constit_Préambule", paste0("constit_ch", 1:7),
           "elect", paste0("elect_ch", 1:7))

for(sample in splits) {

  file = ifelse(sample != "full", paste0("data/network_", sample, ".rda"), "data/network.rda")
  plot = ifelse(sample != "full", paste0("plots/network_", sample, ".pdf"), "plots/network.pdf")
  
  load("data/marsad.rda")
  
  if(sample != "full") {
    
    if(sample == "elect")
      amendements = subset(amendements, type == "Loi électorale") # n = 391
    else if(sample == "constit")
      amendements = subset(amendements, type == "Constitution") # n = 251
    else if(grepl("elect_", sample))
      amendements = subset(amendements, type == "Loi électorale" & ch == gsub("elect_", "", sample))
    else if(grepl("constit_", sample))
      amendements = subset(amendements, type == "Constitution" & ch == gsub("constit_", "", sample))
    
    cat(sample, ":", nrow(amendements), "amendments")
    
    deputes = subset(deputes, uid %in% unique(unlist(strsplit(amendements$aut, ";"))))
    
  } else {
    
    cat("Full data:", nrow(amendements), "amendments")
    
  }
  
  cat("", nrow(deputes), "MPs\n")
  
  if(!file.exists(file)) {
    
    # edge list
    edges = lapply(unique(amendements$uid), function(x) {
      
      y = amendements$aut[ amendements$uid == x ]
      y = unlist(strsplit(y, ";"))
      
      w = length(y) # number of sponsors on amendment

      y = subset(expand.grid(y, y), Var1 != Var2)
      y = apply(y, 1, function(x) { paste0(sort(x), collapse = ";") })

      if(length(y))
        data.frame(uid = unique(y), w, stringsAsFactors = FALSE) # undirected ties
      
    })
    
    edges = rbind.fill(edges)
    
    # raw counts as weights
    edges = aggregate(w ~ uid, length, data = edges)
    
    # Newman-Fowler weights
    # edges = aggregate(w ~ uid, function(x) sum(1 / x), data = edges)
    
    edges$i = gsub("(.*);(.*)", "\\1", edges$uid)
    edges$j = gsub("(.*);(.*)", "\\2", edges$uid)
    edges = edges[, c("i", "j", "w") ]
    
    # Gross-Shalizi weights
    # counts = table(c(edges$i, edges$j))
    # edges$w = edges$w / (counts[ edges$i ] + counts[ edges$j ])
    
    # network
    
    net = network(edges[, 1:2], directed = FALSE)
    network::set.edge.attribute(net, "source", edges[, 1])
    network::set.edge.attribute(net, "target", edges[, 2])
    network::set.edge.attribute(net, "weight", as.vector(edges[, 3]))
    
    wgt = unique(quantile(edges[, 3], c(0, 1/2, 3/4, 1)))
    if(length(wgt) > 1)
      wgt = cut(edges[, 3], breaks = unique(wgt))
    else
      wgt = 5/2
    network::set.edge.attribute(net, "alpha", as.numeric(wgt) / 5)
    
    net %v% "sexe" = deputes[ network.vertex.names(net), "sexe" ]
    net %v% "naissance" = deputes[ network.vertex.names(net), "naissance" ]
    net %v% "bloc" = deputes[ network.vertex.names(net), "bloc" ]
    net %v% "liste" = deputes[ network.vertex.names(net), "liste" ]
    net %v% "circo" = deputes[ network.vertex.names(net), "circo" ]
    net %v% "parti" = deputes[ network.vertex.names(net), "parti" ]
    net %v% "lon" = deputes[ network.vertex.names(net), "lon" ]
    net %v% "lat" = deputes[ network.vertex.names(net), "lat" ]
    net %v% "geo" = deputes[ network.vertex.names(net), "geo" ]
    network.vertex.names(net) = deputes[ network.vertex.names(net), "nom" ]
    
    na = is.na(network.vertex.names(net))
    if(sum(na)) {
      cat("Deleting", sum(na), "unrecognized nodes\n")
      network::delete.vertices(net, which(is.na(network.vertex.names(net))))
    }
    
    save(edges, net, file = file)
    
  }
  
  load(file)
  
  if(plot == "plots/network_constit.pdf" & !file.exists("constitution_network.gexf")) {
    
    entropize <- function(x, n) {
      by = diff(range(x, na.rm = TRUE)) / 20
      return(round(x + runif(n, min = by * -1, max = by), 2))
    }
    
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
    
    nodecolors = t(col2rgb(colors))
    nodecolors = lapply(deputes$bloc, function(x)
      data.frame(r = nodecolors[x, 1], g = nodecolors[x, 2], b = nodecolors[x, 3], a = .3 ))
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
               output = "constitution_network.gexf")
    
  }
  
  if(!file.exists(plot)) {
    
    rownames(deputes) = deputes$uid
    
    same = deputes[ net %e% "source", "bloc"] == deputes[ net %e% "target", "bloc"]
    bloc = deputes[ net %e% "source", "bloc"]
    bloc[ !same ] = NA
    
    bloc[ is.na(bloc) ] = "NA"
    bloc = colors[ bloc ]
    
    # plot
    
    plotcolors = colors[ names(colors) %in% unique(net %v% "bloc") ]
    g = ggnet(net, node.group = net %v% "bloc", node.color = colors, # mode = "kamadakawai",
              segment.alpha = net %e% "alpha", size = 0,
              segment.color = bloc) +
      scale_color_manual("", values = plotcolors) +
      geom_point(size = 9, alpha = 1/3) +
      geom_point(size = 6, alpha = 1/2) +
      guides(size = FALSE)
    
    ggsave(plot, g, width = 12, height = 9)
    
  }
  
}
