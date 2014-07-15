sample = FALSE

plot = ifelse(is.character(sample), paste0("_", sample), "")
scores = paste0("data/scores", plot, ".rda")

load("data/votes.rda")  # roll calls
load("data/marsad.rda") # MPs

if(sample == "constit") {
  votes = subset(votes, constit)
} else if(sample == "rest") {
  votes = subset(votes, !constit)
}

m = unlist(strsplit(as.matrix(votes[, 6:10]), ";")) # unique ids

cat("Voters", ifelse(sample %in% c("[constit] :", "[rest] :"), sample, ":"),
    n_distinct(m[ m %in% deputes$uid ]), "matched",
    n_distinct(m[ !m %in% deputes$uid ]), "unmatched\n")

deputes = deputes[ deputes$uid %in% unique(m), ]

if(!file.exists(scores)) {
  
  # vote matrix
  
  cat("Building vote matrix...")
  
  V = lapply(deputes$uid, function(x) { 
    n = str_count(votes$pour, x) + 
      2 * str_count(votes$contre, x) +
      3 * str_count(votes$abstenu, x) +
      4 * str_count(votes$absent, x) +
      5 * str_count(votes$excuse, x)
    return(matrix(n, nrow = 1))
  })
  V = rbind.fill.matrix(V)
  
  cat(" done [", nrow(V), "MPs x", ncol(V), "bills ]\n")
  
  rownames(V) = deputes$uid
  colnames(V) = votes$uid
  
  plot_idealpoints <- function(x, plot) {
    
    l = x$legislators         # legislators
    w = x$wnom.result         # W-NOMINATE
    d = length(x$legislators) # dimensions
    a = matrix(NA, nrow = ncol(x$legislators[[1]]), ncol = d) # alpha-NOMINATE
    
    for(i in 1:d)
      a[, i] <- summary(as.mcmc(l[[i]]))[[1]][, "Mean"]
    
    W = na.omit(w$legislators[, grepl("coord", colnames(w$legislators)) ])
    W = as.matrix(na.omit(W), ncol = d)
    
    coords = lapply(1:d, function(i) {
      
      data.frame(d = paste("Dimension", i),
                 b = w$legislators$bloc[ w$legislators$nom %in% colnames(as.mcmc(x$legislators[[1]])) ],
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
    
  }
  
  # roll call
  
  deputes$party = deputes$bloc # required for loyalty scores
  deputes$pic = NULL
  deputes$bio = NULL

  # drop 264 votes where yea/nay minority is under 2.5%
  mino = apply(V, 2, function(x) { any(table(x[ x %in% c(1, 2)]) / length(x[ x %in% c(1, 2) ]) < .025) })
  if(sum(mino)) {
    cat("Deleting", sum(mino), "minority votes \n")
    V = V[, !mino ]
  }
  
  deputes = deputes[ deputes$uid %in% rownames(V), ]
  
  # 1,067 votes
  RC = rollcall(V, yea = 1, nay = 2, missing = c(0, 3), notInLegis = 4:5,
                legis.names = deputes$nom, legis.data = deputes,
                vote.names = votes$uid[ !mino ], vote.data = votes[ !mino, ])

  # drops 347 unanimous votes; drops 0/1 legislator(s) and 720 votes
  RC = dropRollCall(RC, dropList = list(lop = 0, legisMin = 24))

  deputes = subset(deputes, nom %in% RC$legis.data$nom)
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
  
  ggsave(paste0("plots/loyalty", plot, ".pdf"), g, width = 12, height = 12)
  
  pol = c( which(deputes$nom == "Jawhara Tiss"), # conservative
           which(deputes$nom == "Amel Azzouz"),  # moderate
           1) # random third dimension
  
  OC1 = oc(RC, dims = 1, polarity = pol[1]   , verbose = TRUE)
  OC2 = oc(RC, dims = 2, polarity = pol[1:2] , verbose = TRUE)
  OC3 = oc(RC, dims = 3, polarity = pol[1:3] , verbose = TRUE)
  
  # alpha-NOMINATE (slow; right-wing ref. is Fathi Ayadi, Nahdha)
  
  AN1 = anominate(RC, dims = 1, polarity = pol[1], nsamp = 1000, thin = 1,
                  burnin = 500, random.starts = FALSE, verbose = TRUE)
  
  plot_idealpoints(AN1, paste0("plots/idealpoints_1d", plot, ".pdf"))
  
  AN2 = anominate(RC, dims = 2, polarity = pol[1:2], nsamp = 1000, thin = 1,
                  burnin = 500, random.starts = FALSE, verbose = TRUE)
  
  plot_idealpoints(AN2, paste0("plots/idealpoints_2d", plot, ".pdf"))
  
  AN3 = anominate(RC, dims = 3, polarity = pol[1:3], nsamp = 1000, thin = 1,
                  burnin = 500, random.starts = FALSE, verbose = TRUE)

  plot_idealpoints(AN3, paste0("plots/idealpoints_3d", plot, ".pdf"))
  
  save(RC, AN1, AN2, AN3, OC1, OC2, OC3, file = scores)
  
}

# two-dimensional results

load(scores)

cat("Optimal classification:\n",
    "  Nahdha positive-positive identification:",
    sum(OC2$legislators$coord1D > 0 & OC2$legislators$coord2D > 0 & grepl("Nahdha", OC2$legislators$bloc),
        na.rm = TRUE), "out of", sum(grepl("Nahdha", OC2$legislators$bloc)), "\n",
    "  Non-Nahdha positive-positive identification:",
    sum(OC2$legislators$coord1D > 0 & OC2$legislators$coord2D > 0 & !grepl("Nahdha", OC2$legislators$bloc),
        na.rm = TRUE), "out of", sum(!grepl("Nahdha", OC2$legislators$bloc)), "\n")

cat("alpha-NOMINATE scores (2 dimensions):\n",
    "  Nahdha > 0:",
    sum(summary(as.mcmc(AN2$legislators[[1]]))[[1]][, "Mean"] > 0 & 
          grepl("Nahdha", AN2$wnom.result$legislators$bloc), na.rm = TRUE),
    "out of", sum(grepl("Nahdha", AN2$wnom.result$legislators$bloc)), "\n",
    "  Non-Nahdha > 0:",
    sum(summary(as.mcmc(AN2$legislators[[1]]))[[1]][, "Mean"] > 0 &
          !grepl("Nahdha", AN2$wnom.result$legislators$bloc), na.rm = TRUE),
    "out of", sum(!grepl("Nahdha", AN2$wnom.result$legislators$bloc)), "\n")

if(!file.exists(paste0("plots/idealpoints", plot, ".pdf"))) {
  
  d = data.frame(
    id = deputes$nom[ deputes$nom %in% colnames(as.mcmc(AN2$legislators[[1]])) ],
    bl = deputes$bloc[ deputes$nom %in% colnames(as.mcmc(AN2$legislators[[1]])) ],
    an = summary(as.mcmc(AN2$legislators[[1]]))[[1]][, "Mean"],
    lb = summary(AN2$legislators[[1]])[[2]][, 1],
    ub = summary(AN2$legislators[[1]])[[2]][, 5]
  )
  d$id = factor(d$id, levels = d$id[ order(d$an) ])
  
  g = qplot(data = d, x = id, y = an, ymin = lb, ymax = ub, color = bl, geom = "pointrange") + 
    geom_point(aes(y = summary(as.mcmc(AN2$legislators[[1]]))[[2]][ order(d$an) ]), alpha = 1/2) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
    scale_color_manual("", values = colors) +
    coord_flip() + 
    labs(y = "\nA-NOMINATE", x = NULL) +
    theme_linedraw(16) +
    theme(panel.grid = element_blank(),
          legend.key = element_blank(),
          axis.text.y = element_text(color = colors[ d$bl[ order(d$an) ] ], size = 6))
  
  ggsave(paste0("plots/idealpoints", plot, ".pdf"), g, width = 9, height = 18)
  
}

if(!file.exists(paste0("plots/idealpoints", plot, "_an.pdf"))) {
  
  d = data.frame(
    id = deputes$nom[ deputes$nom %in% colnames(as.mcmc(AN2$legislators[[1]])) ],
    bl = deputes$bloc[ deputes$nom %in% colnames(as.mcmc(AN2$legislators[[1]])) ],
    d1 = summary(as.mcmc(AN2$legislators[[1]]))[[1]][, "Mean"],
    d2 = summary(as.mcmc(AN2$legislators[[2]]))[[1]][, "Mean"])
  
  g = qplot(data = d, x = d1, y = d2, color = bl,
            label = gsub("(\\w)(\\w+) (.*)", "\\1 \\3", id), size = I(4), 
            alpha = I(2/3), geom = "text") + 
    scale_color_manual("", values = colors) + 
    geom_point(data = transform(d, bl = "All"), color = "black", alpha = 1/3) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    facet_wrap(~ bl) +
    guides(color = FALSE) +
    labs(x = "\nalpha-NOMINATE dimension 1", y = "alpha-NOMINATE dimension 2\n") +
    theme_linedraw(16) +
    theme(legend.position = "right",
          panel.grid = element_blank(),
          legend.key = element_blank())
  
  ggsave(paste0("plots/idealpoints", plot, "_an.pdf"), g, width = 10, height = 10)
  
}

if(!file.exists(paste0("plots/idealpoints", plot, "_oc.pdf"))) {
  
  g = qplot(data = OC2$legislators, x = coord1D, y = coord2D, color = bloc,
            label = gsub("(\\w)(\\w+) (.*)", "\\1 \\3", nom), size = I(4), 
            alpha = I(2/3), geom = "text") + 
    scale_color_manual("", values = colors) + 
    geom_point(data = transform(OC2$legislators, bloc = "All"), color = "black", alpha = 1/3) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    facet_wrap(~ bloc) +
    guides(color = FALSE) +
    labs(x = "\nOC dimension 1", y = "OC dimension 2\n") +
    theme_linedraw(16) +
    theme(legend.position = "right",
          panel.grid = element_blank(),
          legend.key = element_blank())
  
  ggsave(paste0("plots/idealpoints", plot, "_oc.pdf"), g, width = 10, height = 10)
  
}

# kthxbye
