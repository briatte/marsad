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
  v = data.frame()
  
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
    v = rbind(v, d)
    
  }
  
  write.csv(v, file = "data/votes.csv", row.names = FALSE)
  
}

v = read.csv("data/votes.csv")
m = unlist(strsplit(as.matrix(v[, 5:9]), ";")) # unique ids

load("data/marsad.rda") # MPs

cat("Voters:", n_distinct(m[ m %in% deputes$uid ]), "matched",
    n_distinct(m[ !m %in% deputes$uid ]), "unmatched\n")

cat("Building vote matrix...")

l = lapply(deputes$uid, function(x) { 
  n = str_count(v$pour, x) + 
    2 * str_count(v$contre, x) +
    3 * str_count(v$abstenu, x) +
    4 * str_count(v$absent, x) +
    5 * str_count(v$excuse, x)
  return(matrix(n, nrow = 1))
})
l = rbind.fill.matrix(l)
rownames(l) = deputes$nom

cat(" done [", nrow(l), "MPs x", ncol(l), "bills ]\n")
