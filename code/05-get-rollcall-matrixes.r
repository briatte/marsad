#===============================================================================
# VOTE MATRIX 2011
#===============================================================================

h = read_csv("data/votes_2011.csv")$uid
V = read_csv("data/mps_2011.csv")

V = matrix(0, nrow = nrow(V), ncol = n_distinct(h),
           dim = list(V$url, unique(h)))

cat("2011: building roll call matrix of", ncol(V), "votes x",
    nrow(V), "legislators...\n")

pb = txtProgressBar(0, n_distinct(h), style = 3)
for(i in unique(h)) {
  
  f = paste0("raw/votes/2011/", basename(i), ".html") %>% html
  
  V[ which(rownames(V) %in% html_attr(html_nodes(f, "div.votants-pour a"),
                                      "href")), i ] = 1
  V[ which(rownames(V) %in% html_attr(html_nodes(f, "div.votants-contre a"),
                                      "href")), i ] = 2
  V[ which(rownames(V) %in% html_attr(html_nodes(f, "div.votants-abstenu a"),
                                      "href")), i ] = 3
  V[ which(rownames(V) %in% html_attr(html_nodes(f, "div.votants-absent a"),
                                      "href")), i ] = 4
  V[ which(rownames(V) %in% html_attr(html_nodes(f, "div.votants-excuse a"),
                                      "href")), i ] = 5
  
  setTxtProgressBar(pb, which(unique(h) == i))
  
}

stopifnot(V %in% 0:5)
cat("\n")

#===============================================================================
# VOTE MATRIX 2011, CONSTITUTION ONLY OR ALL EXCEPT IT
#===============================================================================

f = html("http://www.marsad.tn/fr/votes/constitution") %>%
  html_nodes("div#main a") %>%
  html_attr("href")

V_cons = V[,  which(colnames(V) %in% f) ]
V_rest = V[, -which(colnames(V) %in% f) ]

save(V, V_cons, V_rest, file = "data/votes_2011_matrix.rda")

#===============================================================================
# VOTE MATRIX 2014
#===============================================================================

h = read_csv("data/votes_2014.csv")$uid
V = read_csv("data/mps_2014.csv")
V = matrix(0, nrow = nrow(V), ncol = n_distinct(h),
           dim = list(V$url, unique(h)))

cat("2014: building roll call matrix of", ncol(V), "votes x",
    nrow(V), "legislators...\n")

pb = txtProgressBar(0, n_distinct(h), style = 3)
for(i in unique(h)) {
  
  f = paste0("raw/votes/2014/", basename(i), ".html") %>% html

  V[ which(rownames(V) %in% html_attr(html_nodes(f, xpath = "//a[@data-vote='pour']"),
                                      "href")), i ] = 1
  V[ which(rownames(V) %in% html_attr(html_nodes(f, xpath = "//a[@data-vote='contre']"),
                                      "href")), i ] = 2
  V[ which(rownames(V) %in% html_attr(html_nodes(f, xpath = "//a[@data-vote='abstenu']"),
                                      "href")), i ] = 3
  V[ which(rownames(V) %in% html_attr(html_nodes(f, xpath = "//a[@data-vote='absent']"),
                                      "href")), i ] = 4
  V[ which(rownames(V) %in% html_attr(html_nodes(f, xpath = "//a[@data-vote='excuse']"),
                                      "href")), i ] = 5
  
  setTxtProgressBar(pb, which(unique(h) == i))
  
}

stopifnot(V %in% 0:5)
cat("\n")

save(V, file = "data/votes_2014_matrix.rda")
