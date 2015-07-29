#===============================================================================
# NETWORK PLOTTING FUNCTION
#===============================================================================

#' Lighter version of the ggnet function, co-authored with Moritz Marbach
#' @seealso GGally package, by Barret Schloerke, for the full version
save_plot <- function(n, file, i, j, q, colors, order) {
  
  party = as.vector(i)
  party[ i != j ] = "#AAAAAA"
  
  print(table(n %v% "party", exclude = NULL))
  stopifnot(all(unique(n %v% "party") %in% names(colors)))
  
  m = network::as.matrix.network.adjacency(n)
  m = do.call(paste0("gplot.layout.", "fruchtermanreingold"), list(m, NULL))
  
  ij = network::as.matrix.network.edgelist(n)
  xy = data.frame(m[ij[, 1], ], m[ij[, 2], ])
  colnames(xy) = c("X1", "Y1", "X2", "Y2")
  
  g = ggplot(data.frame(m), aes(X1, X2)) + 
    geom_segment(data = xy, aes(x = X1, y = Y1, xend = X2, yend = Y2),
                 size = 0.25, colour = party, alpha = 0.5) +
    geom_point(alpha = 1/3, aes(size = q, color = n %v% "party")) +
    geom_point(alpha = 1/2, aes(size = min(q), color = n %v% "party")) +
    scale_color_manual("", values = colors, breaks = order) +
    scale_size_continuous(range = c(6, 12)) +
    scale_x_continuous(breaks = NULL) + 
    scale_y_continuous(breaks = NULL) +
    theme(panel.background = element_rect(fill = "white"), 
          panel.grid = element_blank(), axis.title = element_blank(), 
          legend.key = element_blank(),
          legend.key.size = unit(1, "cm"),
          legend.position = "right",
          legend.text = element_text(size = 16)) +
    guides(color = guide_legend(override.aes = list(alpha = 1/3, size = 6)),
           size = FALSE)
  
  print(g)
  
  ggsave(paste0(file, ".pdf"), g, width = 11, height = 9)
  
  ggsave(paste0(file, ".jpg"), g + theme(legend.position = "none"),
         width = 9, height = 9, dpi = 150)
  
}

#===============================================================================
# NETWORK EXPORT FUNCTION
#===============================================================================

#' Wrapper around the write.gexf function
#' @seealso rgexf package, by George Vega Yon and Jorge Fabrega Lacoa
save_gexf <- function(id, n, colors, extra = NULL) {
  
  rgb = t(col2rgb(colors[ names(colors) %in% as.character(n %v% "party") ]))
  meta = list(creator = "rgexf",
              description = paste("legislative cosponsorship network,",
                                  "fruchtermanreingold placement,", n %n% "n_amendments", "amendments"),
              keywords = "parliament, tunisia")
  
  node.att = data.frame(url = n %v% "url",
                        party = n %v% "party",
                        n_amendments = n %v% "n_amendments",
                        photo = n %v% "photo",
                        stringsAsFactors = FALSE)
  
  for(i in extra)
    node.att[, i ] = n %v% i
  
  people = data.frame(id = as.numeric(factor(network.vertex.names(n))),
                      label = network.vertex.names(n),
                      stringsAsFactors = FALSE)
  
  relations = data.frame(
    source = as.numeric(factor(n %e% "source", levels = levels(factor(people$label)))),
    target = as.numeric(factor(n %e% "target", levels = levels(factor(people$label)))),
    weight = n %e% "raw")
  relations = na.omit(relations)
  
  # check all weights are positive after rounding
  stopifnot(all(relations$weight > 0))
  
  nodecolors = lapply(n %v% "party", function(x)
    data.frame(r = rgb[x, 1], g = rgb[x, 2], b = rgb[x, 3], a = .5))
  nodecolors = as.matrix(bind_rows(nodecolors))
  
  # node placement
  position = do.call("gplot.layout.fruchtermanreingold",
                     list(as.matrix.network.adjacency(n), NULL))
  position = as.matrix(cbind(round(position, 1), 1))
  colnames(position) = c("x", "y", "z")
  
  write.gexf(nodes = people, nodesAtt = node.att,
             edges = relations[, 1:2 ], edgesWeight = relations[, 3],
             nodesVizAtt = list(position = position, color = nodecolors,
                                size = round(n %v% "degree", 1)),
             # edgesVizAtt = list(size = relations[, 4]),
             defaultedgetype = "directed", meta = meta,
             output = paste0(id, ".gexf"))
  
}

#===============================================================================
# NETWORK CONSTRUCTION
#===============================================================================

s = read_csv("data/mps_2011.csv") %>% data.frame
rownames(s) = s$name

a = read_csv("data/amendments_electoral_law.csv", col_types = "cccc") %>%
  mutate(type = "elec")

a = rbind(a,read_csv("data/amendments_constitution.csv", col_types = "cccc") %>%
            mutate(type = "cons"))

a$type = paste0(a$type, "_", gsub("é", "e", a$chapter))
table(a$type, grepl(";", a$sponsors))

# clean up sponsor ids
a$sponsors = gsub("fr/deputes|/", "", a$sponsors)
s$url = gsub("fr/deputes|/", "", s$url)

for(ii in c(unique(a$type), "cons", "elec", "all")) {
  
  cat(ii)
  if(grepl("_", ii))
    data = subset(a, type == ii & grepl(";", sponsors))
  else if(nchar(ii) > 3)
    data = subset(a, grepl(ii, type) & grepl(";", sponsors))
  else
    data = a
  
  # missing sponsors (only a few)
  n = strsplit(data$sponsors, ";") %>% unlist
  cat(" (missing", n_distinct(n[ !n %in% s$url ]), "sponsors):",
      nrow(data), "amendments, ")
  
  #
  # directed edge list
  #
  
  edges = lapply(data$sponsors, function(d) {
    
    w = unlist(strsplit(d, ";"))
    w = w[ w %in% s$url ]
    
    if(length(w) > 1) {
      
      d = expand.grid(i = s$name[ s$url %in% w ],
                      j = s$name[ s$url == w[1]], stringsAsFactors = FALSE)
      
      return(data.frame(d, w = length(w) - 1)) # number of cosponsors
      
    }
    
  }) %>% bind_rows
  
  #
  # edge weights
  #
  
  # first author self-loops, with counts of cosponsors
  self = subset(edges, i == j)
  
  # count number of amendments per first author
  n_au = table(self$j)
  
  # remove self-loops from directed edge list
  edges = subset(edges, i != j)
  
  # count number of amendments cosponsored per sponsor
  n_co = table(edges$i)
  
  # identify directed ties
  edges$ij = apply(edges[, 1:2 ], 1, paste0, collapse = "///")
  
  # raw edge counts
  raw = table(edges$ij)
  
  # Newman-Fowler weights (weighted quantity of amendments cosponsored)
  edges = aggregate(w ~ ij, function(x) sum(1 / x), data = edges)
  
  # expand to edge list
  edges = data.frame(i = gsub("(.*)///(.*)", "\\1", edges$ij),
                     j = gsub("(.*)///(.*)", "\\2", edges$ij),
                     raw = as.vector(raw[ edges$ij ]), # raw edge counts
                     nfw = edges$w, stringsAsFactors = FALSE)
  
  # Gross-Shalizi weights (weighted propensity to cosponsor)
  edges = merge(edges, aggregate(w ~ j, function(x) sum(1 / x), data = self))
  edges$gsw = edges$nfw / edges$w
  
  # sanity check
  stopifnot(edges$gsw <= 1)
  
  # final edge set: cosponsor, first author, weights
  edges = edges[, c("i", "j", "raw", "nfw", "gsw") ]
  
  cat(nrow(edges), "edges, ")
  
  #
  # directed network
  #
  
  n = network(edges[, 1:2 ], directed = TRUE)
  
  n %n% "n_amendments" = nrow(a)
  # n %n% "n_sponsors" = table(subset(b, chamber == jj & legislature == ii)$n_au)
  
  n_au = as.vector(n_au[ network.vertex.names(n) ])
  n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)
  
  n_co = as.vector(n_co[ network.vertex.names(n) ])
  n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)
  
  n %v% "n_amendments" = n %v% "n_au" + n %v% "n_co"
  
  cat(network.size(n), "nodes\n")
  
  n %v% "party" = s[ network.vertex.names(n), "bloc" ]
  n %v% "list" = s[ network.vertex.names(n), "list" ]
  n %v% "sex" = s[ network.vertex.names(n), "sex" ]
  n %v% "age" = s[ network.vertex.names(n), "age" ]
  n %v% "region" = s[ network.vertex.names(n), "region" ]
  n %v% "photo" = s[ network.vertex.names(n), "photo" ]
  n %v% "url" = s[ network.vertex.names(n), "url" ]
  
  # export photos
  n %v% "photo" = paste0("photos/", n %v% "photo")
  
  p = paste0("gexf/", n %v% "photo")
  p = p[ !file.exists(p) ]
  p = file.copy(gsub("gexf/photos", "raw/mps", p), gsub("_2011", "", p))
  
  n %v% "photo" = gsub("_2011", "", n %v% "photo")
  
  # unweighted degree
  n %v% "degree" = degree(n)
  q = n %v% "degree"
  q = as.numeric(cut(q, unique(quantile(q)), include.lowest = TRUE))
  
  set.edge.attribute(n, "source", as.character(edges[, 1]))
  set.edge.attribute(n, "target", as.character(edges[, 2]))
  
  set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
  set.edge.attribute(n, "target", as.character(edges[, 2])) # first author
  
  set.edge.attribute(n, "raw", edges$raw) # raw edge counts
  set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
  set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights
  
  save_plot(n, file = paste0("plots/networks/net_", ii),
            i = blocs_2011[ s[ n %e% "source", "bloc" ] ],
            j = blocs_2011[ s[ n %e% "target", "bloc" ] ],
            q, blocs_2011, names(blocs_2011))

  save_gexf(paste0("gexf/net_", ii), n, blocs_2011, extra = "region")

  assign(paste0("net_",  ii), n)

}

save(list = ls(pattern = "net_"), file = "data/networks.rda")
