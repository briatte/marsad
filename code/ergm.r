sample = FALSE

file = ifelse(is.character(sample),
              ifelse(sample == "elect", "data/network_elect.rda",
                     ifelse(sample == "constit", "data/network_constit.rda",
                            paste0("data/network_constit_", sample, ".rda"))),
              "data/network.rda")

plot = ifelse(is.character(sample),
              ifelse(sample == "elect", "plots/network_elect.pdf",
                     ifelse(sample == "constit", "plots/network_constit.pdf",
                            paste0("plots/network_constit_", sample, ".pdf"))),
              "plots/network.pdf")

load("data/marsad.rda")

if(is.character(sample)) {
  
  if(sample == "elect")
    amendements = subset(amendements, type == "Loi électorale") # n = 391
  else if(sample == "constit")
    amendements = subset(amendements, type == "Constitution") # n = 251
  else
    amendements = subset(amendements, type == "Constitution" & ch == sample)
  
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

file = ifelse(is.character(sample),
              ifelse(sample == "elect", "data/ergmm_elect.rda",
                     ifelse(sample == "constit", "data/ergmm_constit.rda",
                            paste0("data/ergmm_constit_", sample, ".rda"))),
              "data/ergmm.rda")

# plot = ifelse(is.character(sample),
#               ifelse(sample == "elect", "ergm_elect",
#                      ifelse(sample == "constit", "ergm_constit",
#                             paste0("data/ergm_constit_", sample))),
#               "ergm")

if(!file.exists(file)) {
  
  ERGMM = ergmm(net ~ edges + euclidean(d = 2) + rsociality +
                  nodefactor("bloc") +
                  nodematch("bloc", diff = TRUE) + 
                  nodefactor("sexe") +
                  nodematch("sexe"),
                family = "Bernoulli.logit",
                response = "weight",
                verbose = TRUE,
                control = ergmm.control(sample = 5000, burnin = 50000))

#     ERGM = ergm(net ~ edges +
#                 gwdegree(decay = 1, fixed = TRUE) +
#                 nodefactor("bloc") +
#                 nodematch("bloc", diff = TRUE) + 
#                 nodefactor("sexe") +
#                 nodematch("sexe"),
#               control = control.ergm(MCMLE.maxit = 20))
  
#   save(ERGM, file = file)

save(ERGMM, file = file)
  
}

# if(length(dir("plots", paste0(plot, "_(controls|homophilies)"))) < 2) {
#   
#   load(file)
#   print(summary(ERGM))
#   
#   coefs = summary(ERGM)$coefs
#   names(coefs) = c("b", "se", "mcmc", "p")
#   coefs$v = rownames(coefs)
#   
#   g = qplot(data = subset(coefs, grepl("nodematch|nodecov|absdiff", v)),
#             y = b, ymin = b - se, ymax = b + se,
#             x = reorder(v, b), geom = "pointrange") + 
#     coord_flip() + 
#     geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
#     theme_bw(18) +
#     labs(y = NULL, x = NULL)
#   
#   ggsave(paste0("plots/", plot, "_homophilies.pdf"), g, width = 12, height = 9)
#   
#   g = qplot(data = subset(coefs, grepl("edges|nodefactor", v)),
#             y = b, ymin = b - se, ymax = b + se,
#             x = reorder(v, b), geom = "pointrange") + 
#     coord_flip() + 
#     geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
#     theme_bw(18) +
#     labs(y = NULL, x = NULL)
#   
#   ggsave(paste0("plots/", plot, "_controls.pdf"), g, width = 12, height = 9)
#   
# }

# kthxbye
