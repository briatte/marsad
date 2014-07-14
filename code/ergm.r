bootMPLE <- function(Y, X, nboot){
  
  coefs = NULL
  ut = unique(unique(X$j))
  x = X[, 1:(ncol(X) - 1)]
  
  for(i in 1:nboot) {
    
    cat(nboot - i, "... ")
    b = sample(ut, length(ut), replace = TRUE)
    
    indic = NULL
    for(j in 1:length(b)) {
      indic = c(indic, which(X$j == b[j]))
    }
    
    xi = as.matrix(x[indic, ])
    yi = Y[indic]
    esti = glm(yi ~ -1 + xi, family = binomial)
    coefs = rbind(coefs, coef(esti))
  }
  cat("done.\n")
  return(coefs)
}

splits = c("data/ergm_boot.rda", "data/ergm_constit_boot.rda", "data/ergm_elect_boot.rda")
for(raw in splits) {
  
  if(!file.exists(raw)) {
    
    N = dir("data", "network_(constit|elect)_(.*).rda", full.names = TRUE)
    
    if(grepl("constit", raw))
      N = N[ grepl("constit", N) ]
    else if(grepl("elect", raw))
      N = N[ grepl("elect", N) ]
    
    n = length(N)
    
    Y = NULL
    X = NULL
    
    for(j in n:1) {
      
      cat(sprintf("%3g", j), "Adding:", N[j], "\n")
      load(N[j])
      
      if(N[j] == "data/network_elect_ch7.rda")
        m = ergmMPLE(net ~ edges +
                       # structure
                       isolates +
                       kstar(2) +
                       triangle +
                       # main effects
                       nodefactor("bloc") +
                       nodefactor("sexe") +
                       nodecov("naissance") +
                       # homophilies
                       nodematch("bloc", diff = TRUE) + 
                       nodematch("sexe") +
                       absdiff("naissance"))
      else
        m = ergmMPLE(net ~ edges +
                       # structure
                       isolates +
                       kstar(2) +
                       triangle +
                       # main effects
                       nodefactor("bloc") +
                       nodefactor("sexe") +
                       nodefactor("geo") +
                       nodecov("naissance") +
                       # homophilies
                       nodematch("bloc", diff = TRUE) + 
                       nodematch("sexe") +
                       nodematch("geo", diff = TRUE) +
                       absdiff("naissance"))#,
      #            control = control.ergm(MCMLE.maxit = 20))
      
      colnames(m$predictor) = gsub("\\s", ".", colnames(m$predictor))
      
      # empty nodal factor sets
      if(!"nodefactor.geo.Tunisie" %in% colnames(m$predictor))
        m$predictor = cbind(nodefactor.geo.Tunisie = 0, m$predictor)
      if(!"nodefactor.geo.Étranger" %in% colnames(m$predictor))
        m$predictor = cbind(nodefactor.geo.Étranger = 0, m$predictor)
      if(!"nodefactor.bloc.Congrès.Pour.La.République" %in% colnames(m$predictor))
        m$predictor = cbind(nodefactor.bloc.Congrès.Pour.La.République = 0, m$predictor)
      if(!"nodefactor.bloc.Bloc.Démocrates" %in% colnames(m$predictor))
        m$predictor = cbind(nodefactor.bloc.Bloc.Démocrates = 0, m$predictor)
      if(!"nodefactor.bloc.Mouvement.Nahdha" %in% colnames(m$predictor))
        m$predictor = cbind(nodefactor.bloc.Mouvement.Nahdha = 0, m$predictor)
      if(!"nodefactor.bloc.Fidélité.à.La.Révolution" %in% colnames(m$predictor))
        m$predictor = cbind(nodefactor.bloc.Fidélité.à.La.Révolution = 0, m$predictor)
      if(!"nodefactor.bloc.Transition.Démocratique" %in% colnames(m$predictor))
        m$predictor = cbind(nodefactor.bloc.Transition.Démocratique = 0, m$predictor)
      if(!"nodefactor.bloc.Ettakatol" %in% colnames(m$predictor))
        m$predictor = cbind(nodefactor.bloc.Ettakatol = 0, m$predictor)
      if(!"nodefactor.bloc.Alliance.Démocratique" %in% colnames(m$predictor))
        m$predictor = cbind(nodefactor.bloc.Alliance.Démocratique = 0, m$predictor)
      if(!"nodefactor.bloc.Aucun.bloc" %in% colnames(m$predictor))
        m$predictor = cbind(nodefactor.bloc.Aucun.bloc = 0, m$predictor)
      
      # empty differential homophily sets]
      if(!"nodematch.geo.Tunisie" %in% colnames(m$predictor))
        m$predictor = cbind(nodematch.geo.Tunisie = 0, m$predictor)
      if(!"nodematch.geo.Étranger" %in% colnames(m$predictor))
        m$predictor = cbind(nodematch.geo.Étranger = 0, m$predictor)
      if(!"nodematch.bloc.Congrès.Pour.La.République" %in% colnames(m$predictor))
        m$predictor = cbind(nodematch.bloc.Congrès.Pour.La.République = 0, m$predictor)
      if(!"nodematch.bloc.Bloc.Démocrates" %in% colnames(m$predictor))
        m$predictor = cbind(nodematch.bloc.Bloc.Démocrates = 0, m$predictor)
      if(!"nodematch.bloc.Mouvement.Nahdha" %in% colnames(m$predictor))
        m$predictor = cbind(nodematch.bloc.Mouvement.Nahdha = 0, m$predictor)
      if(!"nodematch.bloc.Fidélité.à.La.Révolution" %in% colnames(m$predictor))
        m$predictor = cbind(nodematch.bloc.Fidélité.à.La.Révolution = 0, m$predictor)
      if(!"nodematch.bloc.Transition.Démocratique" %in% colnames(m$predictor))
        m$predictor = cbind(nodematch.bloc.Transition.Démocratique = 0, m$predictor)
      if(!"nodematch.bloc.Ettakatol" %in% colnames(m$predictor))
        m$predictor = cbind(nodematch.bloc.Ettakatol = 0, m$predictor)
      if(!"nodematch.bloc.Alliance.Démocratique" %in% colnames(m$predictor))
        m$predictor = cbind(nodematch.bloc.Alliance.Démocratique = 0, m$predictor)
      if(!"nodematch.bloc.Aucun.bloc" %in% colnames(m$predictor))
        m$predictor = cbind(nodematch.bloc.Aucun.bloc = 0, m$predictor)
      
      m$predictor = m$predictor[, c("edges", "isolates", "kstar2", "triangle", "nodefactor.sexe.H", "nodematch.sexe", "nodecov.naissance", "absdiff.naissance", "nodefactor.geo.Tunisie", "nodefactor.geo.Étranger", "nodematch.geo.Tunisie", "nodematch.geo.Étranger", "nodefactor.bloc.Congrès.Pour.La.République", "nodefactor.bloc.Bloc.Démocrates", "nodefactor.bloc.Mouvement.Nahdha", "nodefactor.bloc.Fidélité.à.La.Révolution", "nodefactor.bloc.Transition.Démocratique", "nodefactor.bloc.Ettakatol", "nodefactor.bloc.Alliance.Démocratique", "nodefactor.bloc.Aucun.bloc", "nodematch.bloc.Congrès.Pour.La.République", "nodematch.bloc.Bloc.Démocrates", "nodematch.bloc.Mouvement.Nahdha", "nodematch.bloc.Fidélité.à.La.Révolution", "nodematch.bloc.Transition.Démocratique", "nodematch.bloc.Ettakatol", "nodematch.bloc.Alliance.Démocratique", "nodematch.bloc.Aucun.bloc") ]
      
      Y = c(Y, m$response)
      X = rbind(X, cbind(m$predictor, j))
      
    }
    
    bsample = bootMPLE(Y, data.frame(X), nboot = 1000)
    
    message("Fitting bootstrapped TERGM...")
    B = as.matrix(X[, 1:(ncol(X) - 1)])
    m = glm(Y ~ B - 1, family = binomial)
    summary(m)
    
    save(B, Y, m, bsample, file = raw)
    
  }
  
  load(raw)
  
  coefs = data.frame(summary(m)[['coefficients']])
  coefs = cbind(rownames(coefs), coefs)
  
  names(coefs) = c("term", "b", "se", "z", "p")
  write.csv(coefs, file = gsub(".rda", ".csv", raw), row.names = FALSE)
  
  p = subset(coefs, !grepl("factor|nodecov", term))
  p$term = gsub("(.*).bloc.|^B", "", p$term)
  p$term = factor(p$term, levels = c("edges", "isolates", "kstar2", "triangle", "nodematch.sexe", 
                                     "absdiff.naissance", "nodematch.geo.Tunisie", "nodematch.geo.Étranger", 
                                     "Congrès.Pour.La.République", "Bloc.Démocrates", "Mouvement.Nahdha", 
                                     "Fidélité.à.La.Révolution", "Transition.Démocratique", "Ettakatol", 
                                     "Alliance.Démocratique", "Aucun.bloc"))
  levels(p$term) = c("Edges", "Isolates", "k-star(2)", "Triangles", "Same gender", 
                     "Age difference", "Both elected at home", "Both elected abroad", 
                     "Congrès.Pour.La.République", "Bloc.Démocrates", "Mouvement.Nahdha", 
                     "Fidélité.à.La.Révolution", "Transition.Démocratique", "Ettakatol", 
                     "Alliance.Démocratique", "Aucun.bloc")
  p$type = ifelse(grepl("\\.|Ettakatol", p$term), as.character(p$term), "Network controls")
  
  m = p[ order(as.numeric(p$term)), ]
  m$p = ifelse(m$p < .001, "***", ifelse(m$p < .01, "**", ifelse(m$p < .05, "*", "")))
  m$term = gsub("\\.", " ", as.character(m$term))
  m[, 2:4] = round(m[, 2:4], 2)
  m[, 3] = paste0("(", m[, 3], ")")
  #   m[, 5] = paste0("(", m[, 5], ")")
  #   m[, 7] = paste0("(", m[, 7], ")")
  m = rbind(c("\\textit{Network structure}", rep("", ncol(m) - 1)),
            m[ 1:4, ],
            c("\\textit{Balancing effects}", rep("", ncol(m) - 1)),
            m[ 5:8, ],
            c("\\textit{Party homophily}", rep("", ncol(m) - 1)),
            m[10:16, ])
  
  colnames(m)[ c(1:3, 5) ] = c("ERGM term", "$\\Beta$", "SE", "\\emph{p}")
  
  print(xtable(m[, c(1:3, 5) ], align = "lrrrr"),
        sanitize.colnames.function = as.character,
        sanitize.text.function = as.character,
        include.rownames = FALSE, file = gsub(".rda", ".tex", raw))
  
  #   p = subset(p, grepl("\\.|Ettakatol", term))
  #   p$term = gsub("\\.", " ", p$term)
  #   p$term = reorder(p$term, p$b)
  # 
  #   g = qplot(data = ph, color = term,
  #         y = b, x = term, ymin = b - 3 * se, ymax = b + 3 * se,
  #         geom = "pointrange") +
  #     geom_pointrange(color = "black", alpha = .5) +
  #     scale_y_continuous(breaks = 0:4) +
  #     scale_color_manual("", values = colors) +
  #     geom_hline(yintercept = 0, color = "grey25", linetype = "dashed") +
  #     coord_flip() +
  #     labs(x = NULL, y = NULL) +
  #     theme_linedraw(16) +
  #     theme(legend.position = "none",        
  #           panel.grid = element_blank())
  #   
  #   ggsave("plots/ergm_boot.pdf", g, height = 9, width = 9)
  
  #   g = qplot(data = subset(coefs, grepl("factor", term)),
  #         y = b, x = term, ymin = b - 3 * se, ymax = b + 3 * se,
  #         geom = "pointrange") +
  #     geom_pointrange(color = "black", alpha = .25) +
  #     scale_y_continuous(minor_breaks = NULL) +
  #     geom_hline(yintercept = 0, color = "grey25", linetype = "dashed") +
  #     coord_flip() +
  #     labs(x = NULL, y = NULL) +
  #     theme_grey(16) +
  #     theme(legend.position = "none",
  #           axis.text = element_text(color = "black"),
  #           panel.grid.minor = element_blank(),
  #           panel.grid.major.x = element_blank())
  
}
