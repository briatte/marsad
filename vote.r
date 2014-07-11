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

plot_idealpoints <- function(x, plot = "plots/idealpoints.pdf") {
  
  legislators <- x$legislators
  wnom.result <- x$wnom.result
  dims <- length(x$legislators)
  nlegis <- ncol(x$legislators[[1]])
  anom.means <- matrix(NA, nrow = nlegis, ncol = dims)
  for(i in 1:dims) {
    anom.means[, i] <- summary(as.mcmc(legislators[[i]]))[[1]][, "Mean"]
  }
  wnom.coords <- na.omit(wnom.result$legislators[, grepl("coord", colnames(wnom.result$legislators))])
  wnom.coords <- as.matrix(na.omit(wnom.coords), ncol = dims)
  
  coords = lapply(1:dims, function(i) {
    data.frame(d = paste("Dimension", i),
               b = wnom.result$legislators$bloc,
               x = wnom.coords[, i],
               y = anom.means[, i],
               y0 = summary(legislators[[i]])[[2]][, 1],
               y1 = summary(legislators[[i]])[[2]][, 5]
    )
    # main = "W-NOMINATE vs. a-NOMINATE\nIdeal Points (w/ 95% CIs)"
    # xlab = paste("W-NOM (", dimension.name[i], " Dimension)", sep = "")
    # ylab = paste("a-NOM (", dimension.name[i], " Dimension)", sep = "")
    
  })
  
  coords = rbind.fill(coords)
  # coords = melt(coords, "d")
  
  colors = brewer.pal(9, "Set1")
  colors[6] = colors[2] # remove yellow, replace by blue
  colors[2] = "#AAAAAA" # dark grey
  colors[9] = "#EEEEEE" # light grey
  names(colors) = c("Alliance Démocratique", "Aucun bloc", "Bloc Démocrates", "Congrès Pour La République",
                    "Ettakatol", "Fidélité à La Révolution", "Mouvement Nahdha", "Transition Démocratique", "NA")
  
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
    facet_wrap(~ d) +
    scale_color_manual("", values = colors) +
    scale_fill_manual("", values = colors) +
    labs(x = "\nA-NOMINATE", y = "density\n") +
    theme_linedraw(16) +
    theme(panel.grid = element_blank(), legend.key = element_blank())
  
  ggsave(gsub("_(\\d+d)", "_\\1_density", plot), g, height = 9, width = 18)
  
}

if(!file.exists("data/votes.rda")) {
  
  V = read.csv("data/votes_matrix.csv", header = TRUE)[, -1]
  
  # roll call
  
  RC = rollcall(V, yea = 1, nay = 2, missing = c(0, 3), notInLegis = 4:5,
                legis.names = deputes$nom, legis.data = deputes,
                vote.names = votes$uid, vote.data = votes)
  
  # alpha-NOMINATE (slow; right-wing ref. is Fathi Ayadi, Nahdha)
  
  AN1 = anominate(RC, dims = 1, polarity = 1, nsamp = 1000, thin = 1,
                  burnin = 500, random.starts = FALSE, verbose = TRUE)
  
  plot_idealpoints(AN1, "plots/idealpoints_1d.pdf")
  
  AN2 = anominate(RC, dims = 2, polarity = c(1, 1), nsamp = 1000, thin = 1,
                  burnin = 500, random.starts = FALSE, verbose = TRUE)
  
  plot_idealpoints(AN2, "plots/idealpoints_2d.pdf")

  AN3 = anominate(RC, dims = 3, polarity = c(1, 1, 1), nsamp = 1000, thin = 1,
                  burnin = 500, random.starts = FALSE, verbose = TRUE)
  
  plot_idealpoints(AN3, "plots/idealpoints_3d.pdf")

  save(AN1, AN2, AN3, RC, V, votes, deputes, file = "data/votes.rda")
  
}

# kthxbye
