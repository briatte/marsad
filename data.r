if(!file.exists("data/marsad.rda")) {
  
  root = "http://www.marsad.tn"
  
  html = htmlParse("http://www.marsad.tn/fr/assemblee")
  urls = xpathSApply(html, "//div[@id='liste']//a[contains(@href, 'depute')]/@href")
  
  amendements = data.frame()
  deputes = data.frame()
  
  for(i in urls) {
    
    page = htmlParse(paste0(root, i))
    
    name = gsub(" \\| Marsad", "", xpathApply(page, "//title", xmlValue))
    # name = xpathSApply(page, "//div[@id='content']/h1/span[2]", xmlValue)
    
    cat(sprintf("%3g", length(urls) - which(i == urls)),
        "Parsing:", i, ":", name, "\n")
    
    bio = scrubber(xpathSApply(page, "//div[@id='bio']", xmlValue), fix.space = FALSE)
    nfo = xpathSApply(page, "//div[@id='infos']/div/span[2]", xmlValue)
    pic = as.vector(xpathSApply(page, "//img[@class='photo']/@src"))

    # commissions
    
    page = htmlParse(paste0(root, i, "/commissions"), encoding = "UTF-8")
    
    comm = xpathSApply(page, "//a[@class='commission']/span[not(@*)]", xmlValue)
    comm = paste0(comm, collapse = ";")
    
    deputes = rbind(deputes,
                    data.frame(uid = gsub("/fr/deputes/", "", i),
                               nom = name,
                               sexe = ifelse(grepl("[N|n]ée", bio), "F", "H"),
                               naissance = as.numeric(str_extract(bio, "[0-9]{4}")),
                               bloc = nfo[1],
                               liste = nfo[2],
                               circo = nfo[3],
                               parti = nfo[4],
                               bureau = ifelse(length(nfo) > 5, nfo[5], NA),
                               comm,
                               votes = ifelse(length(nfo) > 4, as.numeric(gsub("%", "", nfo[5])), NA),
                               pic, bio, stringsAsFactors = FALSE))
    
    # amendements
    
    page = htmlParse(paste0(root, i, "/amendements"), encoding = "UTF-8")
    
    amids = xpathSApply(page, "//p[contains(@class, 'small')]/@id")
    
    amdts = xpathSApply(page, "//p[contains(@class, 'small')]")
    amdts = lapply(amdts, xpathSApply, "a/@href")
    amdts = sapply(amdts, function(x) {
      x = x[ grepl("deputes", x) ]
      x = gsub("/fr/deputes/", "", x)
      return(paste0(x, collapse = ";"))
    })
    
    texte = xpathSApply(page, "//div[@class='rubrique-amendements']/div")
    
    # texte = lapply(texte, xpathSApply, "(p|ul/li)[not(@id) and not(@class)]", xmlValue)
    # texte = lapply(texte, paste0, collapse = " ")
    # texte = sapply(texte, scrubber, fix.space = FALSE)
    
    art = sapply(texte, xpathSApply, "p[1]/a[contains(@href, 'constitution')]", xmlValue)
    art = gsub("l'article(\\s)?", "", scrubber(art))
    art[ !grepl("\\d", art) ] = "Préambule"
    
    amendements = rbind(amendements, data.frame(uid = amids,
                                                art,
                                                aut = amdts,
                                                # txt = texte,
                                                stringsAsFactors = FALSE))
    
  }
  
  #TODO: fix birth years
  
  # geocodes
  
  geo = c("Allemagne", "Amérique et reste de l'Europe", "France 1", "France 2",
          "Italie", "Pays arabe et reste du monde")
  geo = unique(deputes$circo[ !deputes$circo %in% geo ])
  geo = data.frame(geo, geocode(paste(gsub("\\d", "", geo), "tunisie"), output = "latlona"))
  
  deputes = merge(deputes, geo[, -4 ], by.x = "circo", by.y = "geo", all.x = TRUE)
  
  amendements = unique(amendements)
  amendements$nsponsors = 1 + str_count(amendements$aut, ";")
  
  rownames(deputes) = deputes$uid

  amendements$nblocs = sapply(unique(amendements$uid), function(x) {
    
    y = amendements$aut[ amendements$uid == x ]
    y = unlist(strsplit(y, ";"))
    
    length(table(deputes[ y, "bloc" ]))
    
  })
  
  amendements$nlists = sapply(unique(amendements$uid), function(x) {
    
    y = amendements$aut[ amendements$uid == x ]
    y = unlist(strsplit(y, ";"))
    
    length(table(deputes[ y, "liste" ]))
    
  })
  
  amendements$nconstituencies = sapply(unique(amendements$uid), function(x) {
    
    y = amendements$aut[ amendements$uid == x ]
    y = unlist(strsplit(y, ";"))
    
    length(table(deputes[ y, "circo" ]))
    
  })
  
  amendements$nparties = sapply(unique(amendements$uid), function(x) {
    
    y = amendements$aut[ amendements$uid == x ]
    y = unlist(strsplit(y, ";"))
    
    length(table(deputes[ y, "parti" ]))
    
  })
  
  counts = melt(amendements[, which(grepl("^n", names(amendements))) ])
  counts$variable = substring(counts$variable, 2)
  
  qplot(data = counts, x = factor(value), fill = I("grey25"), geom = "bar") +
    scale_x_discrete(breaks = c(2, 5, 10, 20, 30, 60)) +
    facet_wrap(~ variable) +
    labs(x = "\ncount per amendment", y = "number of amendments\n") +
    guides(fill = FALSE) +
    theme_linedraw(16) +
    theme(panel.grid = element_blank())
  
  ggsave("plots/counts_per_amendment.pdf", width = 11, height = 9)
  
  # note: 2 unrecognized sponsors in the data
  # aut = amendements$aut
  # aut = strsplit(aut, ";")
  # unique(unlist(aut)) [ !unique(unlist(aut)) %in% deputes$uid ]
  
  # deputes[ apply(deputes, 1, function(x) { sum(is.na(x)) - 1 }), ]
  # deputes[ grepl("Ennahdha", deputes$bio) & deputes$parti != "Mouvement Nahdha", ]
  
  save(deputes, amendements, file = "data/marsad.rda")
  
}

load("data/marsad.rda")

if(!file.exists("data/network.rda")) {
  
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
  
  wgt = cut(edges[, 3], breaks = quantile(edges[, 3], c(0, 1/2, 3/4, 1)))
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
  
  save(edges, net, file = "data/network.rda")
  
}

load("data/network.rda")

if(!file.exists("plots/constitution_network.pdf")) {
  
  rownames(deputes) = deputes$uid
  same = deputes[ net %e% "source", "bloc"] == deputes[ net %e% "target", "bloc"]
  bloc = deputes[ net %e% "source", "bloc"]
  bloc[ !same ] = NA
  
  # color links by party when both cosponsors come from the same bloc
  
  colors = brewer.pal(9, "Set1")
  names(colors) = sort(unique(bloc))
  
  bloc[ is.na(bloc) ] = "NA"
  names(colors)[9] = "NA"
  
  colors[9] = "#EEEEEE"
  bloc = colors[ bloc ]
  
  # plot
  
  g = ggnet(net, node.group = net %v% "bloc", # mode = "kamadakawai",
            segment.alpha = net %e% "alpha", size = 0,
            segment.color = bloc) +
    scale_color_manual("", values = colors) +
    geom_point(size = 9, alpha = 1/3) +
    geom_point(size = 6, alpha = 1/2) +
    scale_color_brewer("", palette = "Set1") +
    guides(size = FALSE)
  
  ggsave("plots/constitution_network.pdf", g, width = 12, height = 9)
  
}

# kthxbye
