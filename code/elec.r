if(!file.exists("data/electoral_law.rda")) {
  
  elec = data.frame()
  for(i in 1:170) {
    
    cat("Article", i)
    h = htmlParse(paste0("http://www.marsad.tn/fr/lois/loi_electorale/article/", i))
    
    uid = gsub("/fr/lois/loi_electorale/amendement/", "",
               xpathSApply(h, "//div[@id='elec']/div/a[@class='permalink']/@href"))
    aut = lapply(xpathSApply(h, "//div[@id='elec']/div/div/div[contains(@id, 'deps-')]"),
                 xpathSApply, "a/@href")
    aut = sapply(aut, function(x) { paste0(gsub("/fr/deputes/", "", x), collapse = ";") } )
    
    cat("", length(uid), "amendments\n")
    if(length(uid))
      elec = rbind(data.frame(art = i, uid, aut, stringsAsFactors = FALSE), elec)
    
  }
  
  elec$ch = 1 # dispo. générales
  elec$ch[ elec$art %in% 4:17 ] = 2    # électeur
  elec$ch[ elec$art %in% 18:46 ] = 3   # candidat
  elec$ch[ elec$art %in% 47:97 ] = 4   # campagne
  elec$ch[ elec$art %in% 98:146 ] = 5  # scrutin
  elec$ch[ elec$art %in% 147:163 ] = 6 # infractions électorales
  elec$ch[ elec$art %in% 164:170 ] = 7 # dispo. finales et transitoires
  
  load("data/marsad.rda") # MPs
  
  elec$nblocs = sapply(unique(elec$uid), function(x) {
    
    y = elec$aut[ elec$uid == x ]
    y = unlist(strsplit(y, ";"))
    
    length(table(deputes[ y, "bloc" ]))
    
  })
  
  elec$nlists = sapply(unique(elec$uid), function(x) {
    
    y = elec$aut[ elec$uid == x ]
    y = unlist(strsplit(y, ";"))
    
    length(table(deputes[ y, "liste" ]))
    
  })
  
  elec$nconstituencies = sapply(unique(elec$uid), function(x) {
    
    y = elec$aut[ elec$uid == x ]
    y = unlist(strsplit(y, ";"))
    
    length(table(deputes[ y, "circo" ]))
    
  })
  
  elec$nparties = sapply(unique(elec$uid), function(x) {
    
    y = elec$aut[ elec$uid == x ]
    y = unlist(strsplit(y, ";"))
    
    length(table(deputes[ y, "parti" ]))
    
  })
  
  if(!file.exists("plots/counts_per_electoral_amendment.pdf")) {
    
    counts = melt(elec[, which(grepl("^n", names(elec))) ])
    counts$variable = substring(counts$variable, 2)
    
    g = qplot(data = counts, x = factor(value), fill = I("grey25"), geom = "bar") +
      scale_x_discrete(breaks = c(2, 5, 10, 20, 30, 60)) +
      facet_wrap(~ variable) +
      labs(x = "\ncount per amendment", y = "number of amendments\n") +
      guides(fill = FALSE) +
      theme_linedraw(16) +
      theme(panel.grid = element_blank())
    
    ggsave("plots/counts_per_electoral_amendment.pdf", width = 11, height = 9)
    
  }
  
  save(elec, file = "data/electoral_law.rda")

}

load("data/electoral_law.rda")

if(!file.exists("plots/counts_per_electoral_article.pdf")) {
  
  blocs = data.frame()
  for(j in 1:nrow(elec)) {
    
    d = unlist(strsplit(elec$aut[ j ], ";"))
    d = deputes[ d, "bloc" ]
    if(length(d)) {
      d = data.frame(art = elec$art[ j ], d)
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

# edge list

if(!file.exists("data/electoral_network.rda")) {
  
  edges = lapply(unique(elec$uid), function(x) {
    
    y = elec$aut[ elec$uid == x ]
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
  
  save(edges, net, file = "data/electoral_network.rda")

}

load("data/electoral_network.rda")

if(!file.exists("plots/electoral_network.pdf")) {
  
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
  
  ggsave("plots/electoral_network.pdf", g, width = 12, height = 9)
  
}

ERGM = ergm(net ~ edges +
              gwdegree(decay = 1, fixed = TRUE) +
              nodefactor("bloc") +
              nodematch("bloc", diff = TRUE) + 
              nodefactor("sexe") +
              nodematch("sexe"),
            control = control.ergm(MCMLE.maxit = 100))

print(summary(ERGM))

save(ERGM, file = "data/ergm_elec.rda")

coefs = summary(ERGM)$coefs
names(coefs) = c("b", "se", "mcmc", "p")
coefs$v = rownames(coefs)

g = qplot(data = subset(coefs, grepl("nodematch", v)),
          y = b, ymin = b - se, ymax = b + se,
          x = reorder(v, b), geom = "pointrange") + 
  coord_flip() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  theme_bw(18) +
  labs(y = NULL, x = NULL)

ggsave(paste0("plots/ergm_elec_homophilies.pdf"), g, width = 12, height = 9)

g = qplot(data = subset(coefs, grepl("edges|nodefactor", v)),
          y = b, ymin = b - se, ymax = b + se,
          x = reorder(v, b), geom = "pointrange") + 
  coord_flip() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  theme_bw(18) +
  labs(y = NULL, x = NULL)

ggsave(paste0("plots/ergm_elec_controls.pdf"), g, width = 12, height = 9)
