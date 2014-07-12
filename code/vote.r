# scraper

if(!file.exists("data/votes.csv")) {
  
  get_votes <- function(x) {  

    u = xpathSApply(p, paste0("//div[contains(@class, 'votants-", x, "')]/a[contains(@href, '/deputes/')]/@href"))
    return(paste0(gsub("/fr/deputes/", "", u), collapse = ";"))
    
  }
  
  h = htmlParse("http://www.marsad.tn/fr/votes") # /constitution
  n = gsub("\"", "'", scrubber(xpathSApply(h, "//a[contains(@href, '/vote/')]", 
                                           xmlValue), 
                               rm.quote = FALSE, fix.space = FALSE)) # titles
  h = xpathSApply(h, "//a[contains(@href, '/vote/')]/@href") # links
  votes = data.frame()
  
  for(i in length(h):1) {
    
    cat(sprintf("%5g", i), n[i])
    p = htmlParse(paste0("http://www.marsad.tn", h[i]))
    
    d = data.frame(
      uid = gsub("/fr/vote/", "", h[i]),
      date = xpathSApply(p, "//div[@id='main']/*/h3", xmlValue)[1],
      titre1 = n[i],
      titre2 = xpathSApply(p, "//title", xmlValue),
      pour = get_votes("pour"),
      contre = get_votes("contre"),
      abstenu = get_votes("abstenu"),
      absent = get_votes("absent"),
      excuse = get_votes("excuse"),
      stringsAsFactors = FALSE
    )
    cat(" [", d$date, "]\n")
    votes = rbind(votes, d)
    
  }
  
  write.csv(votes, file = "data/votes.csv", row.names = FALSE)
  
}

votes = read.csv("data/votes.csv")

load("data/marsad.rda") # MPs

m = unlist(strsplit(as.matrix(votes[, 5:9]), ";")) # unique ids

cat("Voters:",
    n_distinct(m[ m %in% deputes$uid ]), "matched",
    n_distinct(m[ !m %in% deputes$uid ]), "unmatched\n")

# vote matrix

if(!file.exists("data/votes_matrix.csv")) {
  
  cat("Building vote matrix...")
  
  V = lapply(deputes$uid, function(x) { 
    n = str_count(v$pour, x) + 
      2 * str_count(v$contre, x) +
      3 * str_count(v$abstenu, x) +
      4 * str_count(v$absent, x) +
      5 * str_count(v$excuse, x)
    return(matrix(n, nrow = 1))
  })
  V = rbind.fill.matrix(V)
  
  cat(" done [", nrow(V), "MPs x", ncol(V), "bills ]\n")
  
  # rownames(V) = deputes$uid
  # colnames(V) = v$uid
  
  write.csv(V, file = "data/votes_matrix.csv")
  
}

plot_idealpoints <- function(x, plot) {
  
  l = x$legislators         # legislators
  w = x$wnom.result         # W-NOMINATE
  d = length(x$legislators) # dimensions
  a = matrix(NA, nrow = ncol(x$legislators[[1]]), ncol = d) # alpha-NOMINATE
  
  for(i in 1:d)
    a[, i] <- summary(as.mcmc(l[[i]]))[[1]][, "Mean"]
  
  W = na.omit(w$legislators[, grepl("coord", colnames(w$legislators))])
  W = as.matrix(na.omit(W), ncol = d)
  
  coords = lapply(1:d, function(i) {
    
    data.frame(d = paste("Dimension", i),
               b = w$legislators$bloc,
               x = W[, i],
               y = a[, i],
               y0 = summary(l[[i]])[[2]][, 1],
               y1 = summary(l[[i]])[[2]][, 5]
    )
    
  })
  
  coords = rbind.fill(coords)
  
  g = qplot(data = coords, y = y, x = x, ymin = y0, ymax = y1, color = b,
            alpha = I(2/3), geom = "pointrange") +
    scale_color_manual("", values = colors) +
    facet_wrap(~ d) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    labs(y = "A-NOMINATE\n", x = "\nW-NOMINATE") +
    theme_linedraw(16) +
    theme(panel.grid = element_blank(), legend.key = element_blank())
  
  ggsave(plot, g, height = 9, width = 18)
  
  g = qplot(data = subset(coords, b != "Aucun bloc"),
            x = y, color = b, fill = b, alpha = I(1/2), geom = "density") +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_rug(size = 1) +
    facet_wrap(~ d) +
    scale_color_manual("", values = colors) +
    scale_fill_manual("", values = colors) +
    labs(x = "\nA-NOMINATE", y = "density\n") +
    theme_linedraw(16) +
    theme(panel.grid = element_blank(), legend.key = element_blank())
  
  ggsave(gsub("_(\\d+d)", "_\\1_density", plot), g, height = 9, width = 18)
  
  if(d == 2) {
    
    d = data.frame(
      id = deputes$nom,
      bl = deputes$bloc,
      an = a[, 1],
      lb = summary(l[[1]])[[2]][, 1],
      ub = summary(l[[1]])[[2]][, 5]
    )
    d$id = factor(d$id, levels = d$id[ order(d$an) ])
    
    g = qplot(data = d, x = id, y = an, ymin = lb, ymax = ub, color = bl, geom = "pointrange") + 
      geom_point(aes(y = a[, 2][ order(d$an) ]), alpha = 1/2) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
      scale_color_manual("", values = colors) +
      coord_flip() + 
      labs(y = "\nA-NOMINATE", x = NULL) +
      theme_linedraw(12) +
      theme(panel.grid = element_blank(),
            legend.key = element_blank(),
            axis.text.y = element_text(color = colors[ d$bl[ order(d$an) ] ], size = 6))
    
    ggsave("plots/idealpoints.pdf", g, width = 9, height = 18)
    
  }
  
}

if(!file.exists("data/votes.rda")) {
  
  V = read.csv("data/votes_matrix.csv", header = TRUE)[, -1]
  
  # roll call
  
  deputes$party = deputes$bloc # required for loyalty scores
  deputes$pic = NULL
  deputes$bio = NULL

  RC = rollcall(V, yea = 1, nay = 2, missing = c(0, 3), notInLegis = 4:5,
                legis.names = deputes$nom, legis.data = deputes,
                vote.names = votes$uid, vote.data = votes)
  
  RC = dropUnanimous(RC)

  deputes$loyalty = summary(RC, verbose = TRUE)$partyLoyalty

  t = tapply(deputes$loyalty, deputes$bloc, mean)
  t = data.frame(bloc = names(t), mu = t)
  
  g = qplot(data = deputes, x = loyalty, alpha = I(1/2), fill = bloc, color = bloc, geom = "density") +
    geom_density(data = transform(deputes, bloc = "All"), fill = "grey75") +
    geom_vline(data = t, aes(xintercept = mu), linetype = "dotted") +
    scale_color_manual("", values = colors) +
    scale_fill_manual("", values = colors) +
    facet_wrap(~ bloc) +
    labs(y = NULL, x = "\nloyalty score") +
    theme_linedraw(16) +
    theme(legend.position = "none", panel.grid = element_blank(), legend.key = element_blank())
  
  ggsave("plots/loyalty.pdf", g, width = 12, height = 12)
  
  OC1 = oc(RC, dims = 1, polarity = 1          , verbose = TRUE)
  OC2 = oc(RC, dims = 2, polarity = c(1, 1)    , verbose = TRUE)
  OC3 = oc(RC, dims = 3, polarity = c(1, 1, 1) , verbose = TRUE)
  
  g = qplot(data = OC2$legislators, x = coord1D, y = coord2D, color = bloc,
            label = gsub("(\\w)(\\w+) (.*)", "\\1 \\3", nom), size = I(4), 
            alpha = I(2/3), geom = "text") + 
    scale_color_manual("", values = colors) + 
    geom_point(data = transform(OC2$legislators, bloc = "All"), color = "black", alpha = 1/3) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    facet_wrap(~ bloc) +
    guides(color = FALSE) +
    labs(x = "\nDimension 1", y = "Dimension 2\n") +
    theme_linedraw(16) +
    theme(legend.position = "right",
          panel.grid = element_blank(),
          legend.key = element_blank())
  
  ggsave("plots/oc_2d.pdf", g, width = 12, height = 12)
    
  # alpha-NOMINATE (slow; right-wing ref. is Fathi Ayadi, Nahdha)
  
  AN1 = anominate(RC, dims = 1, polarity = 1, nsamp = 1000, thin = 1,
                  burnin = 500, random.starts = FALSE, verbose = TRUE)
  
  plot_idealpoints(AN1, "plots/idealpoints_1d.pdf")
  
  AN2 = anominate(RC, dims = 2, polarity = c(1, 1), nsamp = 1000, thin = 1,
                  burnin = 500, random.starts = FALSE, verbose = TRUE)
  
  plot_idealpoints(AN2, "plots/idealpoints_2d.pdf")
  
  AN3 = anominate(RC, dims = 2, polarity = c(1, 1, 1), nsamp = 1000, thin = 1,
                  burnin = 500, random.starts = FALSE, verbose = TRUE)

  plot_idealpoints(AN3, "plots/idealpoints_3d.pdf")
  
  save(RC, AN1, AN2, AN3, OC1, OC2, OC3, file = "data/votes.rda")
  
}

load("data/votes.rda")

cat("Optimal classification:\n",
    "  Nahdha positive-positive identification:",
    sum(OC2$legislators$coord1D > 0 & OC2$legislators$coord2D > 0 & grepl("Nahdha", OC2$legislators$bloc)),
    "out of", sum(grepl("Nahdha", OC2$legislators$bloc)), "\n",
    "  Non-Nahdha positive-positive identification:",
    sum(OC2$legislators$coord1D > 0 & OC2$legislators$coord2D > 0 & !grepl("Nahdha", OC2$legislators$bloc)),
    "out of", sum(!grepl("Nahdha", OC2$legislators$bloc)), "\n")

cat("alpha-NOMINATE scores (2 dimensions):\n",
    "  Nahdha > 0:",
    sum(summary(as.mcmc(AN2$legislators[[1]]))[[1]][, "Mean"] > 0 & 
          grepl("Nahdha", AN2$wnom.result$legislators$bloc)),
    "out of", sum(grepl("Nahdha", AN2$wnom.result$legislators$bloc)), "\n",
    "  Non-Nahdha > 0:",
    sum(summary(as.mcmc(AN2$legislators[[1]]))[[1]][, "Mean"] > 0 &
          !grepl("Nahdha", AN2$wnom.result$legislators$bloc)),
    "out of", sum(!grepl("Nahdha", AN2$wnom.result$legislators$bloc)))

# kthxbye
