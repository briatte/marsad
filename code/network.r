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
    
    if(!file.exists("plots/counts_per_electoral_article.pdf")) {
      
      y = subset(amendements, type == "Loi électorale")
      blocs = data.frame()
      
      for(j in 1:nrow(y)) {
        
        d = unlist(strsplit(y$aut[ j ], ";"))
        d = deputes[ d, "bloc" ]
        if(length(d)) {
          d = data.frame(art = y$art[ j ], d)
          blocs = rbind(blocs, d)
        }
        
      }
      
      blocs$art = factor(blocs$art, levels = 1:170)
      blocs = merge(blocs, unique(elec[, c("art", "ch") ]), by = "art", all.x = TRUE)
      
      g = qplot(data = blocs, x = art, fill = d, alpha = I(2 / 3), geom = "bar") + 
        scale_x_discrete(breaks = c("Préambule", 4, 18, 47, 98, 147, 164)) + 
        scale_fill_manual("", values = colors, na.value = "grey") + 
        labs(y = "number of amendment sponsors\n", x = "\narticle")
      
      ggsave("plots/counts_per_electoral_article.pdf", g, width = 16, height = 9)
      
    }
    
    if(!file.exists("plots/counts_per_constitution_article.pdf")) {
      
      y = subset(amendements, type == "Constitution")
      blocs = data.frame()
      
      for(j in 1:nrow(y)) {
        
        d = unlist(strsplit(y$aut[ j ], ";"))
        d = deputes[ d, "bloc" ]
        d = data.frame(art = y$art[ j ], d)
        blocs = rbind(blocs, d)
        
      }
      
      blocs$art = factor(blocs$art, levels = c("Préambule", 1:146))
      blocs = merge(blocs, unique(y[, c("art", "ch") ]), by = "art", all.x = TRUE)
      
      g = qplot(data = blocs, x = art, fill = d, alpha = I(2 / 3), geom = "bar") + 
        scale_x_discrete(breaks = c("Préambule", 21, 51, 71, 102, 125, 131, 143, 145, 148)) + 
        scale_fill_manual("", values = colors) + 
        labs(y = "number of amendment sponsors\n", x = "\narticle")
      
      ggsave("plots/counts_per_constitution_article.pdf", g, width = 16, height = 9)
      
    }
    
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
