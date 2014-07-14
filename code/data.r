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
  
  # erase wrong birth years (identified by reading through bios)
  deputes$naissance[ deputes$uid %in% c("52bdd52412bdaa7f9b90f157",
                                        "4f4fbcf3bd8cb561570000a1",
                                        "4f4fbcf3bd8cb561570000ae",
                                        "4f4fbcf3bd8cb5615700009a",
                                        "4f4fbcf3bd8cb5615700002b",
                                        "4f4fbcf3bd8cb5615700005e",
                                        "4f4fbcf3bd8cb56157000010",
                                        "4f4fbcf3bd8cb5615700009f",
                                        "514507f4b197de08259e59c2",
                                        "4f4fbcf3bd8cb561570000d3",
                                        "4f4fbcf3bd8cb5615700004b",
                                        "51caff6d7ea2c47c3f3672ac",
                                        "535fc27e12bdaa078ab824d7") ] = NA
  # fixes
  deputes$naissance[ deputes$uid == "4f4fbcf3bd8cb5615700000e" ] = 1961
  deputes$naissance[ deputes$uid == "4f4fbcf3bd8cb5615700000c" ] = 1983
  deputes$naissance[ deputes$uid == "4f4fbcf3bd8cb56157000099" ] = 1980
  deputes$naissance[ deputes$uid == "4f4fbcf3bd8cb5615700009a" ] = 1963
  deputes$naissance[ deputes$uid == "4f4fbcf3bd8cb5615700009a" ] = 1963
  deputes$naissance[ deputes$uid == "4f4fbcf3bd8cb56157000010" ] = 1953
  deputes$naissance[ deputes$uid == "4f4fbcf3bd8cb56157000028" ] = 1977
  
  # approximate (sometimes very approximate) fixes
  deputes$naissance[ deputes$uid == "52bdd52412bdaa7f9b90f157" ] = 1960 # undergrad in 1978
  deputes$naissance[ deputes$uid == "4f4fbcf3bd8cb561570000a1" ] = 1964 # Google Images, looks like 50...
  deputes$naissance[ deputes$uid == "4f4fbcf3bd8cb561570000ae" ] = 1963 # graduated in 1984
  deputes$naissance[ deputes$uid == "4f4fbcf3bd8cb561570000b2" ] = 1964 # Google Images, looks like 50...
  deputes$naissance[ deputes$uid == "535fc27e12bdaa078ab824d7" ] = 1974 # Google Images, looks like 40...
  deputes$naissance[ deputes$uid == "4f4fbcf3bd8cb5615700005e" ] = 1974 # Google Images, looks like 50...
  deputes$naissance[ deputes$uid == "4f4fbcf3bd8cb5615700002b" ] = 1960 # graduated in 1981
  deputes$naissance[ deputes$uid == "4f4fbcf3bd8cb5615700009f" ] = 1966 # finished high school in 1984
  deputes$naissance[ deputes$uid == "514507f4b197de08259e59c2" ] = 1964 # Google Images, looks like 50...
  deputes$naissance[ deputes$uid == "4f4fbcf3bd8cb561570000d3" ] = 1966 # went to uni in 1984 (?)
  deputes$naissance[ deputes$uid == "4f4fbcf3bd8cb5615700004b" ] = 1958 # PhD in Pharmacy in 1984
  deputes$naissance[ deputes$uid == "51caff6d7ea2c47c3f3672ac" ] = 1958 # guesstimated close to above

  table(deputes$naissance, exclude = NULL)
  
  # geocodes
  
  geo = c("Allemagne", "Amérique et reste de l'Europe", "France 1", "France 2",
          "Italie", "Pays arabe et reste du monde")
  geo = unique(deputes$circo[ !deputes$circo %in% geo ])
  geo = data.frame(geo, geocode(paste(gsub("\\d", "", geo), "tunisie"), output = "latlona"))
  
  deputes = merge(deputes, geo[, -4 ], by.x = "circo", by.y = "geo", all.x = TRUE)
  
  geo = deputes$circo
  # geo[ grepl("France \\d+", geo) ] = "France"
  # geo[ grepl("Tunis \\d+", geo) ] = "Tunis"
  # geo[ grepl("Nabeul \\d+", geo) ] = "Nabeul"
  geo[ geo %in% c("Allemagne", "Amérique et reste de l'Europe", "France", "Italie",
                  "Pays arabe et reste du monde") ] = NA
  geo[ !is.na(geo) ] = "Tunisie"
  geo[ is.na(geo) ] = "Étranger"
  deputes$geo = geo
  
  table(deputes$circo, exclude = NULL)
  table(deputes$geo, exclude = NULL)
  
  rownames(deputes) = deputes$uid

  amendements = unique(amendements)
  
  # constitutional chapters

  # ch = data.frame(table(amendements$art))
  amendements$ch = ifelse(amendements$art == "Préambule", "Préambule", NA)
  amendements$ch[ amendements$art %in% 1:20 ] = "ch1" # principes généraux
  amendements$ch[ amendements$art %in% 21:49 ] = "ch2" # droits et libertés
  amendements$ch[ amendements$art %in% 50:70 ] = "ch3" # pouvoir législatif
  amendements$ch[ amendements$art %in% 71:101 ] = "ch4" # pouvoir exécutif
  amendements$ch[ amendements$art %in% 102:124 ] = "ch5" # pouvoir judiciaire
  # amendements$ch[ amendements$art %in% 106:117 ] = "ch5_1" # titre 1, justice
  # amendements$ch[ amendements$art %in% 118:124 ] = "ch5_2" # titre 2, cour constit.
  amendements$ch[ amendements$art %in% 125:130 ] = "ch6" # instances indép.
  amendements$ch[ amendements$art %in% 131:142 ] = "ch7" # pouvoir local
  # amendements$ch[ amendements$art %in% 143:144 ] = "ch8" # révision constit.
  # amendements$ch[ amendements$art %in% 145:147 ] = "ch9" # dispo. finales
  # amendements$ch[ amendements$art %in% 148:149 ] = "ch10" # dispo. transitoires
  print(table(amendements$ch))
  
  # deputes[ apply(deputes, 1, function(x) { sum(is.na(x)) - 1 }), ]

  amendements$type = "Constitution"

  elec = data.frame()
  for(i in 170:1) {
    
    cat("Article", i)
    h = htmlParse(paste0("http://www.marsad.tn/fr/lois/loi_electorale/article/", i))
    
    uid = gsub("/fr/lois/loi_electorale/amendement/", "",
               xpathSApply(h, "//div[@id='amendements']/div/a[@class='permalink']/@href"))
    aut = lapply(xpathSApply(h, "//div[@id='amendements']/div/div/div[contains(@id, 'deps-')]"),
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
  elec$ch = paste0("ch", elec$ch)
  elec$type = "Loi électorale"

  amendements = rbind(amendements, elec)
  
  n_count <- function(x, attr) {
    
    y = amendements$aut[ amendements$uid == x ]
    y = unlist(strsplit(y, ";"))
    
    length(table(deputes[ y, attr ]))
    
  }
  
  amendements$nsponsors = 1 + str_count(amendements$aut, ";")
  amendements$nblocs = sapply(unique(amendements$uid), n_count, attr = "bloc")
  amendements$nlists = sapply(unique(amendements$uid), n_count, attr = "liste")  
  amendements$nconstituencies = sapply(unique(amendements$uid), n_count, attr = "circo")
  amendements$nparties = sapply(unique(amendements$uid), n_count, attr = "parti")
  
  counts = melt(amendements[, which(grepl("^n|type", names(amendements))) ], "type")
  counts$variable = substring(counts$variable, 2)
  
  qplot(data = counts, x = factor(value), fill = I("grey25"), geom = "bar") +
    scale_x_discrete(breaks = c(2, 5, 10, 20, 30, 60)) +
    facet_grid(type ~ variable, scales = "free_y") +
    labs(x = "\ncount per amendment", y = "number of amendments\n") +
    guides(fill = FALSE) +
    theme_linedraw(16) +
    theme(panel.grid = element_blank())
  
  ggsave("plots/counts_per_amendment.pdf", width = 18, height = 6)
  
  save(deputes, amendements, file = "data/marsad.rda")
  
}

if(!file.exists("plots/demographics.pdf")) {
    
  d = data.frame(
    bloc = deputes$bloc,
    geo = deputes$geo,
    age = 2014 - deputes$naissance,
    sexe = deputes$sexe)
  d$age = cut(d$age, quantile(d$age, c(0, 1/3, 2/3, 3/3)), include.lowest = TRUE)
  
  g = qplot(data = d, x = geo, fill = sexe, geom = "bar") + 
    facet_grid(age ~ bloc) +
    scale_fill_brewer("", palette = "Set2") +
    labs(y = "count\n", x = NULL) +
    theme_linedraw(16) +
    theme(panel.grid = element_blank())
  
  ggsave("plots/demographics.pdf", g, width = 18, height = 9)
  
}

# kthxbye
