#===============================================================================
# SET ROLL CALL OBJECT 2011
#===============================================================================

# legislators
d = read_csv("data/mps_2011.csv") %>%
  rename(party = bloc)

# votes
v = read_csv("data/votes_2011.csv")

# roll call votes
load("data/votes_2011_matrix.rda")

# sanity checks
stopifnot(!duplicated(colnames(V)))
stopifnot(!duplicated(rownames(V)))

cat("\n2011: matrix has", ncol(V), "roll call votes and", nrow(V), "legislators\n")

# votes where yea/nay minority is under 2.5%
min = apply(V, 2, function(x) {
  any(table(x[ x %in% c(1, 2)]) / length(x[ x %in% c(1, 2) ]) < .025)
})

cat("2011: removed", sum(min), "votes where minority y/n < 2.5% of y/n\n")
V = V[, !min ]

cat("2011: removed", sum(!d$url %in% rownames(V)), "legislators with only minority votes\n")
d = d[ d$url %in% rownames(V), ]

# subset votes to selected
v = v[ v$uid %in% colnames(V), ]

# sanity checks
stopifnot(v$uid == colnames(V))
stopifnot(d$url == rownames(V))

# roll call object
R = rollcall(V, yea = 1, nay = 2, missing = c(0, 0), notInLegis = 3:5,
             legis.names = d$nom, legis.data = data.frame(d),
             vote.names = v$uid, vote.data = data.frame(v))

# lose votes with zero legislators and legislators with < 20 votes
RC = dropRollCall(R, dropList = list(lop = 0, legisMin = 20))

cat("2011: removed", R$m - RC$m, "votes with < 1 legislator in minority\n")
cat("2011: removed", R$n - RC$n, "legislators with < 20 votes\n")
cat("2011: final matrix has", RC$m, "roll call votes and", RC$n, "legislators\n")

save(RC, file = "data/rollcalls_2011.rda")

#===============================================================================
# SET ROLL CALL OBJECT 2014
#===============================================================================

# legislators
d = read_csv("data/mps_2014.csv") %>%
  rename(party = bloc)

# votes
v = read_csv("data/votes_2014.csv")

# roll call votes
load("data/votes_2014_matrix.rda")

# sanity checks
stopifnot(!duplicated(colnames(V)))
stopifnot(!duplicated(rownames(V)))

cat("\n2014: matrix has", ncol(V), "roll call votes and", nrow(V), "legislators\n")

# votes where yea/nay minority is under 2.5%
min = apply(V, 2, function(x) {
  any(table(x[ x %in% c(1, 2)]) / length(x[ x %in% c(1, 2) ]) < .025)
})

cat("2014: removed", sum(min), "votes where minority y/n < 2.5% of y/n\n")
V = V[, !min ]

cat("2014: removed", sum(!d$url %in% rownames(V)), "legislators with only minority votes\n")
d = d[ d$url %in% rownames(V), ]

# subset votes to selected
v = v[ v$uid %in% colnames(V), ]

# sanity checks
stopifnot(v$uid == colnames(V))
stopifnot(d$url == rownames(V))

# roll call object
R = rollcall(V, yea = 1, nay = 2, missing = c(0, 0), notInLegis = 3:5,
             legis.names = d$nom, legis.data = data.frame(d),
             vote.names = v$uid, vote.data = data.frame(v))

# lose votes with zero legislators and legislators with < 20 votes
RC = dropRollCall(R, dropList = list(lop = 0, legisMin = 20))

cat("2014: removed", R$m - RC$m, "votes with < 1 legislator in minority\n")
cat("2014: removed", R$n - RC$n, "legislators with < 20 votes\n")
cat("2014: final matrix has", RC$m, "roll call votes and", RC$n, "legislators\n")

save(RC, file = "data/rollcalls_2014.rda")