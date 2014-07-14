sample = FALSE

if(is.character(sample)) {
  plot = paste0("_", sample)
} else {
  plot = ""
}

scores = paste0("data/scores", plot, ".rda")

# roll call uids from Constitution votes only
constit = c("52c712ee12bdaa7f9b90f3e9", "52c7195112bdaa7f9b90f3ea", "52c71e4e12bdaa7f9b90f3eb", 
            "52c726e412bdaa7f9b90f3ec", "52c72c0312bdaa7f9b90f3ed", "52c822bf12bdaa7f9b90f3f6", 
            "52c822bf12bdaa7f9b90f3f7", "52c830e312bdaa7f9b90f3ff", "52c838c312bdaa7f9b90f400", 
            "52c8404a12bdaa7f9b90f401", "52c846c912bdaa7f9b90f402", "52c84e9812bdaa7f9b90f406", 
            "52c856d512bdaa7f9b90f408", "52c85e9112bdaa7f9b90f40f", "52c8661e12bdaa7f9b90f410", 
            "52c86f9912bdaa7f9b90f414", "52c8794012bdaa7f9b90f418", "52c87fe412bdaa7f9b90f41b", 
            "52c8a13212bdaa7f9b90f41c", "52c8a95112bdaa7f9b90f41d", "52c8ade412bdaa7f9b90f41e", 
            "52c928c412bdaa7f9b90f420", "52c929df12bdaa7f9b90f421", "52c92e9c12bdaa7f9b90f422", 
            "52c92fa112bdaa7f9b90f423", "52c9b9f212bdaa7f9b90f435", "52c9ba0712bdaa7f9b90f436", 
            "52c9ba8c12bdaa7f9b90f437", "52c9baae12bdaa7f9b90f438", "52c9baca12bdaa7f9b90f439", 
            "52c9bb0012bdaa7f9b90f43a", "52c9bb4612bdaa7f9b90f43b", "52c9bd6412bdaa7f9b90f43c", 
            "52cac7d412bdaa7f9b90f43f", "52cad5d612bdaa7f9b90f44f", "52cadedb12bdaa7f9b90f451", 
            "52caea0112bdaa7f9b90f457", "52caefeb12bdaa7f9b90f45d", "52caf7b612bdaa7f9b90f464", 
            "52cafbea12bdaa7f9b90f468", "52cb128d12bdaa7f9b90f473", "52cbe96112bdaa7f9b90f48b", 
            "52cbeda712bdaa7f9b90f48c", "52cbf04212bdaa7f9b90f48d", "52cbf58612bdaa7f9b90f48e", 
            "52cbf83212bdaa7f9b90f490", "52cc775c12bdaa57018b76f3", "52cc7b2912bdaa57018b76f4", 
            "52cc820312bdaa57018b76f6", "52cc89b012bdaa57018b76f7", "52cc8e7012bdaa57018b76f8", 
            "52cc926212bdaa57018b76f9", "52cc956b12bdaa57018b76fa", "52cc991212bdaa57018b76fb", 
            "52cc9d1f12bdaa57018b76fc", "52cca15b12bdaa57018b76fd", "52cca41a12bdaa57018b76fe", 
            "52cca71312bdaa57018b76ff", "52ccafbb12bdaa57018b7700", "52ccb32c12bdaa57018b7701", 
            "52ccb93c12bdaa57018b7702", "52ccbc4912bdaa57018b7703", "52ccc0f712bdaa57018b7704", 
            "52ccc57712bdaa57018b7705", "52ccc7a812bdaa57018b7706", "52ccca8412bdaa57018b7707", 
            "52cde39b12bdaa09ac3f39bf", "52cde3a412bdaa09ac3f39c1", "52cde95112bdaa09ac3f39c2", 
            "52cdef4a12bdaa09ac3f39c3", "52cdf59a12bdaa09ac3f39c4", "52cdf8b212bdaa09ac3f39c5", 
            "52ce019b12bdaa09ac3f39c6", "52ce03c312bdaa09ac3f39c7", "52ce080512bdaa09ac3f39c8", 
            "52ce0bfb12bdaa09ac3f39ca", "52ce11fa12bdaa09ac3f39cb", "52ce801a12bdaa09ac3f39cd", 
            "52ce8bcf12bdaa09ac3f39d0", "52cebef212bdaa09ac3f3a6e", "52ced46d12bdaa09ac3f3a7b", 
            "52cef37f12bdaa77218c8812", "52cefce312bdaa77218c8814", "52ceff9912bdaa77218c8815", 
            "52cf16bf12bdaa77218c881a", "52cf1f1e12bdaa77218c881b", "52cf4c2f12bdaa77218c881c", 
            "52cf4e4312bdaa77218c881d", "52cf516312bdaa77218c881e", "52cf532b12bdaa77218c881f", 
            "52cf560e12bdaa77218c8820", "52cfd50612bdaa77218c8822", "52d00d9812bdaa77218c8823", 
            "52d05d4212bdaa77218c88d4", "52d0621912bdaa77218c88d6", "52d06b0012bdaa77218c88d7", 
            "52d06c2f12bdaa77218c88d8", "52d0a0a312bdaa77218c88d9", "52d0a24912bdaa77218c88da", 
            "52d0a9f912bdaa77218c88db", "52d0aa9f12bdaa77218c88dc", "52d0ab8712bdaa77218c88dd", 
            "52d0abfb12bdaa77218c88de", "52d0acc012bdaa77218c88df", "52d0ad6b12bdaa77218c88e0", 
            "52d0aeea12bdaa77218c88e1", "52d0af7e12bdaa77218c88e2", "52d12b8812bdaa77218c88e4", 
            "52d12c4712bdaa77218c88e5", "53876edc12bdaa5f5421ac20", "52d12d8712bdaa77218c88e6", 
            "52d130fa12bdaa77218c88e7", "52d1321512bdaa77218c88e8", "52d1328112bdaa77218c88e9", 
            "52d135c012bdaa77218c88ea", "52d13e0112bdaa77218c88eb", "52d13e0112bdaa77218c88ec", 
            "52d13f3b12bdaa77218c88ed", "52d13fda12bdaa77218c88ee", "52d142c012bdaa77218c88ef", 
            "52d1449012bdaa77218c88f0", "52d1458112bdaa77218c88f1", "52d146f412bdaa77218c88f2", 
            "52d149c712bdaa77218c88f3", "52d5ddc012bdaa635b715ec0", "52d14a3f12bdaa77218c88f4", 
            "52d1933612bdaa635b715e34", "52d1965612bdaa635b715e35", "52d1de7512bdaa635b715e37", 
            "52d1df0e12bdaa635b715e38", "52d1e1e812bdaa635b715e39", "52d1e3be12bdaa635b715e3a", 
            "52d1e4f412bdaa635b715e3b", "52d1e73012bdaa635b715e3c", "52d1ee1912bdaa635b715e3d", 
            "52d1eef912bdaa635b715e3e", "52d1efbb12bdaa635b715e3f", "52d1f04d12bdaa635b715e40", 
            "52d1f20612bdaa635b715e41", "52d1f4fa12bdaa635b715e42", "52ce94f312bdaa09ac3f39d1", 
            "52cf062a12bdaa77218c8818", "52d1fc6212bdaa635b715e43", "52d3f02212bdaa635b715e69", 
            "52d1fcf812bdaa635b715e44", "52d26a8112bdaa635b715e46", "52d26c7a12bdaa635b715e47", 
            "52d26cd812bdaa635b715e48", "52d276cc12bdaa635b715e49", "52d2775612bdaa635b715e4a", 
            "52d2b6f712bdaa635b715e55", "52d0188b12bdaa77218c8824", "52d6092512bdaa635b715ec8", 
            "52d6098912bdaa635b715ec9", "52d609d112bdaa635b715eca", "52d60a7e12bdaa635b715ecb", 
            "52d6798112bdaa635b715ecc", "52d67a4712bdaa635b715ecd", "52d67ce612bdaa635b715ece", 
            "52d67e0112bdaa635b715ecf", "52d680a312bdaa635b715ed0", "52d6813412bdaa635b715ed1", 
            "52d0266612bdaa77218c8825", "52d0307212bdaa77218c88c6", "52d038b112bdaa77218c88c7", 
            "52d6824a12bdaa635b715ed2", "52d6847e12bdaa635b715ed3", "52d687a312bdaa635b715ed4", 
            "52d6884812bdaa635b715ed5", "52d688c312bdaa635b715ed6", "52d68a3312bdaa635b715ed7", 
            "52d68bca12bdaa635b715ed8", "52d68cbf12bdaa635b715ed9", "52d68eb312bdaa635b715eda", 
            "52d68f3f12bdaa635b715edb", "52d6912f12bdaa635b715edc", "52d6947e12bdaa635b715edd", 
            "52d6969b12bdaa635b715ede", "52d6985b12bdaa635b715edf", "52d69a4912bdaa635b715ee0", 
            "52d69b3812bdaa635b715ee1", "52d69c2212bdaa635b715ee2", "52d69cc012bdaa635b715ee3", 
            "52d16ab912bdaa635b715e1c", "52d5d37c12bdaa635b715ebf", "52d5d14512bdaa635b715ebe", 
            "52d5cfab12bdaa635b715ebd", "52d5c9ef12bdaa635b715ebb", "52d5c42012bdaa635b715eba", 
            "52d5c10f12bdaa635b715eb9", "52d5c01e12bdaa635b715eb8", "52d5bfa412bdaa635b715eb7", 
            "52d5bf0612bdaa635b715eb6", "52d57dd612bdaa635b715eb4", "52d57c8012bdaa635b715eb3", 
            "52d57b9912bdaa635b715eb2", "52d5797f12bdaa635b715eb1", "52d5777112bdaa635b715eb0", 
            "52d576bf12bdaa635b715eaf", "52d574d412bdaa635b715eae", "52d573bf12bdaa635b715ead", 
            "52d5704712bdaa635b715eab", "52d56f5512bdaa635b715eaa", "52d1a55512bdaa635b715e36", 
            "52d2fa0e12bdaa635b715e58", "52d2f3d112bdaa635b715e57", "52d56dfa12bdaa635b715ea9", 
            "52d56c9a12bdaa635b715ea8", "52d56c6612bdaa635b715ea7", "52d56acd12bdaa635b715ea6", 
            "52d5697712bdaa635b715ea5", "52d567a112bdaa635b715ea4", "52d5663b12bdaa635b715ea3", 
            "52d5611412bdaa635b715ea2", "52d55f7712bdaa635b715ea1", "52d55e9312bdaa635b715ea0", 
            "52d4aed812bdaa635b715e9e", "52d4ae1212bdaa635b715e9d", "52d4ac3712bdaa635b715e9c", 
            "52d4ab4612bdaa635b715e9b", "52d4a9bd12bdaa635b715e9a", "52d4a8ab12bdaa635b715e99", 
            "52d4a47412bdaa635b715e98", "52d4a3f012bdaa635b715e97", "52d49de612bdaa635b715e96", 
            "52d49d3012bdaa635b715e95", "52d49ca812bdaa635b715e94", "52d49c0412bdaa635b715e93", 
            "52d499d812bdaa635b715e92", "52d4996c12bdaa635b715e91", "52d498c312bdaa635b715e90", 
            "52d4974212bdaa635b715e8f", "52d4965212bdaa635b715e8e", "52d4953512bdaa635b715e8d", 
            "52d4946912bdaa635b715e8c", "52d2bb6412bdaa635b715e56", "52d48c8f12bdaa635b715e8b", 
            "52d48ad112bdaa635b715e8a", "52d487d912bdaa635b715e89", "52d4865412bdaa635b715e88", 
            "52d485bf12bdaa635b715e87", "52d4838e12bdaa635b715e86", "52d481f412bdaa635b715e85", 
            "52d480cf12bdaa635b715e84", "52d47fad12bdaa635b715e83", "52d47ee012bdaa635b715e82", 
            "52d47e0412bdaa635b715e81", "52d47d6c12bdaa635b715e80", "52d47be412bdaa635b715e7f", 
            "52d478d212bdaa635b715e7e", "52d470b912bdaa635b715e77", "52d4716712bdaa635b715e78", 
            "52d4729912bdaa635b715e79", "52d473b712bdaa635b715e7a", "52d4745412bdaa635b715e7b", 
            "52d4752b12bdaa635b715e7c", "52d4761612bdaa635b715e7d", "52d444a112bdaa635b715e75", 
            "52d445aa12bdaa635b715e76", "52d5f8bc12bdaa635b715ec7", "52d5f84b12bdaa635b715ec6", 
            "52d5f78f12bdaa635b715ec5", "52d5f6a212bdaa635b715ec4", "52d5f61612bdaa635b715ec3", 
            "52d5f59e12bdaa635b715ec2", "52d5f40c12bdaa635b715ec1", "52d811e712bdaa635b715ee4", 
            "52d8171e12bdaa635b715ee5", "52d81d4212bdaa635b715ee6", "52db37ff12bdaa5cd4a07cfe", 
            "52d9663112bdaa635b715fbd", "52d96a5c12bdaa635b715fbe", "52d96ee912bdaa635b715fc2", 
            "52d975cc12bdaa635b715fc5", "52d97a0812bdaa635b715fc7", "52d980ed12bdaa635b715fc8", 
            "52d984b912bdaa635b715fca", "52db38b512bdaa5cd4a07cff", "52db3a7512bdaa5cd4a07d00", 
            "52db3aff12bdaa5cd4a07d01", "52db3bad12bdaa5cd4a07d02", "52db3d0e12bdaa5cd4a07d03", 
            "52db3dd912bdaa5cd4a07d05", "52db3e8e12bdaa5cd4a07d06", "52db3f4f12bdaa5cd4a07d08", 
            "52db409112bdaa5cd4a07d09", "52db41e912bdaa5cd4a07d0a", "52db429212bdaa5cd4a07d0b", 
            "52db434b12bdaa5cd4a07d0c", "52db448512bdaa5cd4a07d0d", "52db45b912bdaa5cd4a07d0e", 
            "52db468112bdaa5cd4a07d0f", "52db481f12bdaa5cd4a07d10", "52db492f12bdaa5cd4a07d11", 
            "52db49ed12bdaa5cd4a07d12", "52dbb6bb12bdaa5cd4a07d13", "52dbb7d012bdaa5cd4a07d14", 
            "52dbb85512bdaa5cd4a07d15", "52dbb8c912bdaa5cd4a07d16", "52d9403412bdaa635b715fb7", 
            "52dbb95812bdaa5cd4a07d17", "52dbbca012bdaa5cd4a07d18", "52dbbd2f12bdaa5cd4a07d1a", 
            "52dbbe8012bdaa5cd4a07d1b", "52dbbf8712bdaa5cd4a07d1c", "52dbc0a012bdaa5cd4a07d1d", 
            "52dbc1d012bdaa5cd4a07d1e", "52dbc2aa12bdaa5cd4a07d1f", "52dbc49512bdaa5cd4a07d20", 
            "52dbc89812bdaa5cd4a07d21", "52dbf75412bdaa5cd4a07d22", "52dbf8af12bdaa5cd4a07d23", 
            "52dbf98d12bdaa5cd4a07d24", "52dbfa2012bdaa5cd4a07d25", "52dbfaa212bdaa5cd4a07d26", 
            "52dbfc4a12bdaa5cd4a07d27", "52dbfd1b12bdaa5cd4a07d28", "52dbfe8c12bdaa5cd4a07d29", 
            "52dbff6612bdaa5cd4a07d2a", "52dc000e12bdaa5cd4a07d2b", "52dc00fc12bdaa5cd4a07d2c", 
            "52dc031012bdaa5cd4a07d2d", "52dc03cf12bdaa5cd4a07d2e", "52dc088a12bdaa5cd4a07d2f", 
            "52dc09b612bdaa5cd4a07d31", "52dc0ad112bdaa5cd4a07d32", "52dc0c0612bdaa5cd4a07d33", 
            "52dc0d2012bdaa5cd4a07d34", "52dc0f7512bdaa5cd4a07d35", "52dc100712bdaa5cd4a07d36", 
            "52dc135512bdaa5cd4a07d37", "52dc142712bdaa5cd4a07d38", "52dc148c12bdaa5cd4a07d39", 
            "52dc15f612bdaa5cd4a07d3a", "52dc168312bdaa5cd4a07d3b", "52dc189612bdaa5cd4a07d3c", 
            "52dc1a3512bdaa5cd4a07d3d", "52dc1afd12bdaa5cd4a07d3e", "52dc1bb412bdaa5cd4a07d3f", 
            "52dc1e9612bdaa5cd4a07d40", "52dc20eb12bdaa5cd4a07d41", "52dc234c12bdaa5cd4a07d42", 
            "52dc23dd12bdaa5cd4a07d43", "52dc25ac12bdaa5cd4a07d44", "52dc27ae12bdaa5cd4a07d45", 
            "52dc285012bdaa5cd4a07d46", "52dc295412bdaa5cd4a07d47", "52dc2a0112bdaa5cd4a07d48", 
            "52dc2b5612bdaa5cd4a07d49", "52e127ad12bdaa593ad565d7", "52e127f912bdaa593ad565d8", 
            "52e1388812bdaa593ad565d9", "52e138be12bdaa593ad565da", "52e138ff12bdaa593ad565db", 
            "52e1393e12bdaa593ad565dc", "52e1397212bdaa593ad565dd", "52e13a1d12bdaa593ad565de", 
            "52e13a8312bdaa593ad565e0", "52e13abe12bdaa593ad565e1", "52e1577b12bdaa593ad565e5", 
            "52e157b212bdaa593ad565e6", "52e1584712bdaa593ad565e7", "52e1588712bdaa593ad565e8", 
            "52e1598b12bdaa593ad565e9", "52e159c212bdaa593ad565eb", "52e159f712bdaa593ad565ec", 
            "52e15a3312bdaa593ad565ee", "52e15a9512bdaa593ad565f2", "52e15b5812bdaa593ad565f3", 
            "52e15c7012bdaa593ad565f5", "52e15d4912bdaa593ad565f6", "52e15da512bdaa593ad565f9", 
            "52e15e2812bdaa593ad565fa", "52e15e7f12bdaa593ad565fb", "52e15ee412bdaa593ad565fd", 
            "52e15f6e12bdaa593ad565fe", "52e15fac12bdaa593ad565ff", "52e1603012bdaa593ad56600", 
            "52e1607012bdaa593ad56601", "52e160e112bdaa593ad56603", "52e161c512bdaa593ad56604", 
            "52e161fc12bdaa593ad56605", "52e1628312bdaa593ad56606", "52e1631312bdaa593ad56607", 
            "52e163d812bdaa593ad56608", "52e1655412bdaa593ad56609", "52e1660112bdaa593ad5660a", 
            "52e1672d12bdaa593ad5660b", "52e167d512bdaa593ad5660c", "52e168e812bdaa593ad5660d", 
            "52e16a4d12bdaa593ad5660e", "52e16af312bdaa593ad5660f", "52e16b4112bdaa593ad56610", 
            "52e16b7612bdaa593ad56611", "52e25aed12bdaa593ad566af", "52e25c8312bdaa593ad566b0", 
            "52e266c712bdaa593ad566b3", "52e2686b12bdaa593ad566b4", "52e26e5c12bdaa593ad566b5", 
            "52e26f4012bdaa593ad566b6", "52e273a712bdaa593ad566b8", "52e274f312bdaa593ad566ba", 
            "52e284de12bdaa593ad566bc", "52e2881512bdaa593ad566bd", "52e2898812bdaa593ad566be", 
            "52e28a9312bdaa593ad566bf", "52e28b4f12bdaa593ad566c0", "52e28c8c12bdaa593ad566c1", 
            "52e28e7912bdaa593ad566c2", "52e28fd012bdaa593ad566c3", "52e2918112bdaa593ad566c4", 
            "52e2928912bdaa593ad566c5", "52e2936812bdaa593ad566c6", "52e294d412bdaa593ad566c7", 
            "52e2954312bdaa593ad566c8", "52e29a8312bdaa593ad566cb", "52e29b5b12bdaa593ad566ce", 
            "52e29db212bdaa593ad566cf", "52e29e8c12bdaa593ad566d0", "52e2a21e12bdaa593ad566d1", 
            "52e2a4f612bdaa593ad566d2", "52e2a5e712bdaa593ad566d3", "52e26fff12bdaa593ad566b7", 
            "52e2fed112bdaa593ad566d6", "52e598b212bdaa593ad566f2")

# scraper

if(!file.exists("data/votes.rda")) {
  
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
  
  save(votes, file = "data/votes.rda")
  
}

load("data/votes.rda")  # roll calls
load("data/marsad.rda") # MPs

if(sample == "constit") {
  votes = subset(votes, uid %in% constit)
} else if(sample == "rest") {
  votes = subset(votes, !uid %in% constit)
}

m = unlist(strsplit(as.matrix(votes[, 5:9]), ";")) # unique ids

cat("Voters:",
    n_distinct(m[ m %in% deputes$uid ]), "matched",
    n_distinct(m[ !m %in% deputes$uid ]), "unmatched\n")

deputes = deputes[ deputes$uid %in% m, ]

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
    
    if(d == 2) {
      
      d = data.frame(
        id = deputes$nom[ deputes$nom %in% colnames(as.mcmc(x$legislators[[1]])) ],
        bl = deputes$bloc[ deputes$nom %in% colnames(as.mcmc(x$legislators[[1]])) ],
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
  
  # 1,067 votes
  RC = rollcall(V, yea = 1, nay = 2, missing = c(0, 3), notInLegis = 4:5,
                legis.names = deputes$nom, legis.data = deputes,
                vote.names = votes$uid[ !mino ], vote.data = votes[ !mino, ])
  
  # drops 347 unanimous votes; keeps all legislators and 720 votes
  RC = dropRollCall(RC, dropList = list(lop = 0, legisMin = 24))

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
  
  ggsave(paste0("plots/oc_2d", plot, ".pdf"), g, width = 12, height = 12)
    
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

# kthxbye
