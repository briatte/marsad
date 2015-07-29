#===============================================================================
# IDEAL POINTS 2011
#===============================================================================

load("data/rollcalls_2011.rda")

p = RC$legis.data$name
p = c(
  which(p == "Jawhara Tiss"), # conservative
  which(p == "Amel Azzouz"),  # moderate
  1 # random third dimension
)

stopifnot(length(p) == 3)

# optimal classification

OC1 = oc(RC, dims = 1, polarity = p[1]   , verbose = TRUE)
OC2 = oc(RC, dims = 2, polarity = p[1:2] , verbose = TRUE)
OC3 = oc(RC, dims = 3, polarity = p[1:3] , verbose = TRUE)

# alpha-NOMINATE

AN1 = anominate(RC, dims = 1, polarity = p[1], nsamp = 1000, thin = 1,
                burnin = 500, random.starts = FALSE, verbose = TRUE)

AN2 = anominate(RC, dims = 2, polarity = p[1:2], nsamp = 1000, thin = 1,
                burnin = 500, random.starts = FALSE, verbose = TRUE)

AN3 = anominate(RC, dims = 3, polarity = p[1:3], nsamp = 1000, thin = 1,
                burnin = 500, random.starts = FALSE, verbose = TRUE)

save(RC, AN1, AN2, AN3, OC1, OC2, OC3, file = "data/rollcalls_2011.rda")

#===============================================================================
# IDEAL POINTS 2014
#===============================================================================

load("data/rollcalls_2014.rda")

p = RC$legis.data$name
p = c(
  which(p == "Mahmoud Gouiaa"),   # conservative
  which(p == "Meherzia Laabidi"), # moderate
  1 # random third dimension
)

stopifnot(length(p) == 3)

OC1 = oc(RC, dims = 1, polarity = p[1]   , verbose = TRUE)
OC2 = oc(RC, dims = 2, polarity = p[1:2] , verbose = TRUE)
OC3 = oc(RC, dims = 3, polarity = p[1:3] , verbose = TRUE)

# alpha-NOMINATE

AN1 = anominate(RC, dims = 1, polarity = p[1], nsamp = 1000, thin = 1,
                burnin = 500, random.starts = FALSE, verbose = TRUE)

AN2 = anominate(RC, dims = 2, polarity = p[1:2], nsamp = 1000, thin = 1,
                burnin = 500, random.starts = FALSE, verbose = TRUE)

AN3 = anominate(RC, dims = 3, polarity = p[1:3], nsamp = 1000, thin = 1,
                burnin = 500, random.starts = FALSE, verbose = TRUE)

save(RC, AN1, AN2, AN3, OC1, OC2, OC3, file = "data/rollcalls_2014.rda")
