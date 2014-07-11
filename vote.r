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

V = read.csv("data/votes_matrix.csv", header = TRUE)[, -1]

# roll call

RC = rollcall(V, yea = 1, nay = 2, missing = c(0, 3), notInLegis = 4:5,
              legis.names = deputes$nom, legis.data = deputes,
              vote.names = votes$uid, vote.data = votes)

# alpha-NOMINATE (slow; right-wing ref. is Fathi Ayadi, Nahdha)

AN1 = anominate(RC, dims = 1, polarity = 1, nsamp = 1000, thin = 1,
                burnin = 500, random.starts = FALSE, verbose = TRUE)

AN2 = anominate(RC, dims = 2, polarity = c(1, 1), nsamp = 1000, thin = 1,
                burnin = 500, random.starts = FALSE, verbose = TRUE)

AN3 = anominate(RC, dims = 3, polarity = c(1, 1, 1), nsamp = 1000, thin = 1,
                burnin = 500, random.starts = FALSE, verbose = TRUE)

save(AN1, AN2, AN3, RC, V, votes, deputes, file = "data/votes.rda")

# kthxbye
