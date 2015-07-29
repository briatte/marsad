#===============================================================================
# SUMMARY OF ALL ROLL CALLS
#===============================================================================

v = read_csv("data/votes_2011.csv")
load("data/votes_2011_matrix.rda")

p_2011 = data_frame(uid = colnames(V),
                    missing = apply(V, 2, function(x) sum(x == 0)) / nrow(V),
                    yea = apply(V, 2, function(x) sum(x == 1)) / nrow(V),
                    nay = apply(V, 2, function(x) sum(x == 2)) / nrow(V),
                    abstain = apply(V, 2, function(x) sum(x == 3)) / nrow(V),
                    absent = apply(V, 2, function(x) sum(x == 4)) / nrow(V),
                    excused = apply(V, 2, function(x) sum(x == 5) / nrow(V))
)

p_2011 = gather(p_2011, key, value, -uid) %>%
  left_join(v, by = "uid") %>%
  mutate(year = year(date))

v = read_csv("data/votes_2014.csv")
load("data/votes_2014_matrix.rda")

p_2014 = data_frame(uid = colnames(V),
                    missing = apply(V, 2, function(x) sum(x == 0)) / nrow(V),
                    yea = apply(V, 2, function(x) sum(x == 1)) / nrow(V),
                    nay = apply(V, 2, function(x) sum(x == 2)) / nrow(V),
                    abstain = apply(V, 2, function(x) sum(x == 3)) / nrow(V),
                    absent = apply(V, 2, function(x) sum(x == 4)) / nrow(V),
                    excused = apply(V, 2, function(x) sum(x == 5) / nrow(V))
)

p_2014 = gather(p_2014, key, value, -uid) %>%
  left_join(v, by = "uid") %>%
  mutate(year = year(date))

p = rbind(p_2011, p_2014)

g = qplot(data = p, x = reorder(uid, date), y = value, fill = key,
          stat = "identity", geom = "bar") +
  scale_y_continuous(label = percent) +
  scale_fill_manual("", values = vot) +
  facet_grid(. ~ year, scale = "free_x", space = "free_x") +
  labs(x = NULL, y = NULL) +
  theme_bw(18) +
  theme(strip.background = element_rect(fill = "grey90"),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())

ggsave("plots/rc_summary.pdf", width = 18, height = 9)

#===============================================================================
# PARTY LOYALTY 2011
#===============================================================================

load("data/rollcalls_2011.rda")

RC$legis.data$partyLoyalty = summary(RC, verbose = TRUE)$partyLoyalty

t = tapply(RC$legis.data$partyLoyalty, RC$legis.data$party, mean)
t = data_frame(party = names(t), mu = t)
t = rbind(t, data_frame(party = "All", mu = mean(RC$legis.data$partyLoyalty)))

g = qplot(data = RC$legis.data, x = partyLoyalty, alpha = I(1/2),
          fill = party, color = party, geom = "density") +
  geom_density(data = transform(RC$legis.data, party = "All"),
               color = "grey50", fill = "grey75") +
  geom_rug() +
  geom_vline(data = t, aes(xintercept = mu), linetype = "dotted") +
  scale_color_manual("", values = blocs_2011) +
  scale_fill_manual("", values = blocs_2011) +
  facet_wrap(~ party) +
  labs(y = "Density\n", x = "\nLoyalty score") +
  theme_bw(18) +
  theme(strip.background = element_rect(fill = "grey90"),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.margin = unit(10, "points"),
        legend.key = element_blank())

ggsave("plots/rc_2011_loyalty.pdf", g, width = 9, height = 9)

#===============================================================================
# OPTIMAL CLASSIFICATION 2011
#===============================================================================

cat(
  "Optimal classification (2 dimensions):\n",
  "- Nahdha positive-positive identification:",
  sum(OC2$legislators$coord1D > 0 & OC2$legislators$coord2D > 0 &
        grepl("ahdha$", OC2$legislators$party), na.rm = TRUE),
  "out of", sum(grepl("ahdha$", OC2$legislators$party)), "\n",
  "- Non-Nahdha positive-positive identification:",
  sum(OC2$legislators$coord1D > 0 & OC2$legislators$coord2D > 0 &
        !grepl("ahdha$", OC2$legislators$party),
      na.rm = TRUE),
  "out of", sum(!grepl("ahdha$", OC2$legislators$party)), "\n"
)

g = qplot(data = OC2$legislators, x = coord1D, y = coord2D, color = party,
          label = gsub("(\\w)(\\w+) (.*)", "\\1 \\3", name), size = I(4), 
          alpha = I(2/3), geom = "text") + 
  scale_color_manual("", values = blocs_2011) + 
  scale_x_continuous(breaks = pretty_breaks(3)) +
  scale_y_continuous(breaks = pretty_breaks(3)) +
  geom_point(data = transform(OC2$legislators, party = "All"), color = "black", alpha = 1/3) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(~ party) +
  guides(color = FALSE) +
  labs(x = "\nOC dimension 1", y = "OC dimension 2\n") +
  theme_bw(18) +
  theme(strip.background = element_rect(fill = "grey90"),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.margin = unit(10, "points"))

ggsave("plots/oc_2011.pdf", g, width = 9, height = 9)

#===============================================================================
# A-NOMINATE 2011
#===============================================================================

r = AN2$wnom.result$legislators
r$legislator = rownames(r)

dim1 = summary(as.mcmc(AN2$legislators[[1]]))[[2]]
dim1 = data_frame(legislator = rownames(dim1), d1 = dim1[, "50%"],
                  lo = dim1[, "2.5%"], hi = dim1[, "97.5%"])

dim2 = summary(as.mcmc(AN2$legislators[[2]]))[[2]]
dim2 = data_frame(legislator = rownames(dim2), d2 = dim2[, "50%"])

dims = inner_join(dim1, dim2, by = "legislator")

r = inner_join(r, dims, by = "legislator")

g = qplot(data = r, y = d2, x = d1, color = party) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
  scale_color_manual("", values = blocs_2011) +
  facet_wrap(~ party) +
  labs(x = "\nA-NOMINATE dimension 1", y = "A-NOMINATE dimension 2\n") +
  theme_bw(18) +
  theme(strip.background = element_rect(fill = "grey90"),
        panel.grid = element_blank(),
        legend.position = "none")

ggsave(paste0("plots/an_2011.pdf"), g, width = 9, height = 9)

g = qplot(data = r, y = reorder(name, d1), x = d1, color = party) +
  geom_segment(aes(yend = reorder(name, d1), x = lo, xend = hi)) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
  scale_color_manual("", values = blocs_2011) +
  labs(y = "\nA-NOMINATE dimension 1", x = NULL) +
  theme_bw(18) +
  theme(panel.grid = element_blank(),
        legend.position = c(0, 1), 
        legend.justification = c(0, 1), 
        legend.key = element_blank(),
        axis.text.y = element_text(size = rel(.5),
                                   color = blocs_2011[ r$party[ order(r$d1) ] ]))

ggsave(paste0("plots/an_2011_points.pdf"), g, width = 9, height = 18)

g = qplot(data = r, x = d1, fill = party, color = party, alpha = I(1/2),
          geom = "density") +
  geom_density(data = transform(r, party = "All"),
               color = "grey50", fill = "grey75") +
  geom_rug() +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
  scale_color_manual("", values = blocs_2011) +
  scale_fill_manual("", values = blocs_2011) +
  facet_wrap(~ party) +
  labs(x = "\nA-NOMINATE dimension 1", y = "Density\n") +
  theme_bw(18) +
  theme(strip.background = element_rect(fill = "grey90"),
        panel.grid = element_blank(),
        legend.position = "none")

ggsave(paste0("plots/an_2011_density.pdf"), g, width = 9, height = 9)

#===============================================================================
# PARTY LOYALTY 2014
#===============================================================================

load("data/rollcalls_2014.rda")

RC$legis.data$partyLoyalty = summary(RC, verbose = TRUE)$partyLoyalty

t = tapply(RC$legis.data$partyLoyalty, RC$legis.data$party, mean)
t = data_frame(party = names(t), mu = t)
t = rbind(t, data_frame(party = "All", mu = mean(RC$legis.data$partyLoyalty)))

g = qplot(data = RC$legis.data, x = partyLoyalty, alpha = I(1/2),
          fill = party, color = party, geom = "density") +
  geom_density(data = transform(RC$legis.data, party = "All"),
               color = "grey50", fill = "grey75") +
  geom_rug() +
  geom_vline(data = t, aes(xintercept = mu), linetype = "dotted") +
  scale_color_manual("", values = blocs_2014) +
  scale_fill_manual("", values = blocs_2014) +
  facet_wrap(~ party) +
  labs(y = "Density\n", x = "\nLoyalty score") +
  theme_bw(18) +
  theme(strip.background = element_rect(fill = "grey90"),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.margin = unit(10, "points"),
        legend.key = element_blank())

ggsave("plots/rc_2014_loyalty.pdf", g, width = 9, height = 9)

#===============================================================================
# OPTIMAL CLASSIFICATION 2014
#===============================================================================

cat(
  "Optimal classification (2 dimensions):\n",
  "- Nahdha positive-positive identification:",
  sum(OC2$legislators$coord1D > 0 & OC2$legislators$coord2D > 0 &
        grepl("ahdha$", OC2$legislators$party), na.rm = TRUE),
  "out of", sum(grepl("ahdha$", OC2$legislators$party)), "\n",
  "- Non-Nahdha positive-positive identification:",
  sum(OC2$legislators$coord1D > 0 & OC2$legislators$coord2D > 0 &
        !grepl("ahdha$", OC2$legislators$party),
      na.rm = TRUE),
  "out of", sum(!grepl("ahdha$", OC2$legislators$party)), "\n"
)

g = qplot(data = OC2$legislators, x = coord1D, y = coord2D, color = party,
      label = gsub("(\\w)(\\w+) (.*)", "\\1 \\3", name), size = I(4), 
      alpha = I(2/3), geom = "text") + 
  scale_color_manual("", values = blocs_2014) + 
  scale_x_continuous(breaks = pretty_breaks(3)) +
  scale_y_continuous(breaks = pretty_breaks(3)) +
  geom_point(data = transform(OC2$legislators, party = "All"), color = "black", alpha = 1/3) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(~ party) +
  guides(color = FALSE) +
  labs(x = "\nOC dimension 1", y = "OC dimension 2\n") +
  theme_bw(18) +
  theme(strip.background = element_rect(fill = "grey90"),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.margin = unit(10, "points"))

ggsave("plots/oc_2014.pdf", g, width = 9, height = 9)

#===============================================================================
# A-NOMINATE 2014
#===============================================================================

r = AN2$wnom.result$legislators
r$legislator = rownames(r)

dim1 = summary(as.mcmc(AN2$legislators[[1]]))[[2]]
dim1 = data_frame(legislator = rownames(dim1), d1 = dim1[, "50%"],
                  lo = dim1[, "2.5%"], hi = dim1[, "97.5%"])

dim2 = summary(as.mcmc(AN2$legislators[[2]]))[[2]]
dim2 = data_frame(legislator = rownames(dim2), d2 = dim2[, "50%"])

dims = inner_join(dim1, dim2, by = "legislator")

r = inner_join(r, dims, by = "legislator")

g = qplot(data = r, y = d2, x = d1, color = party) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
  scale_color_manual("", values = blocs_2014) +
  facet_wrap(~ party) +
  labs(x = "\nA-NOMINATE dimension 1", y = "A-NOMINATE dimension 2\n") +
  theme_bw(18) +
  theme(strip.background = element_rect(fill = "grey90"),
        panel.grid = element_blank(),
        legend.position = "none")

ggsave(paste0("plots/an_2014.pdf"), g, width = 9, height = 9)

g = qplot(data = r, y = reorder(name, d1), x = d1, color = party) +
  geom_segment(aes(yend = reorder(name, d1), x = lo, xend = hi)) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
  scale_color_manual("", values = blocs_2014) +
  labs(y = "\nA-NOMINATE dimension 1", x = NULL) +
  theme_bw(18) +
  theme(panel.grid = element_blank(),
        legend.position = c(0, 1), 
        legend.justification = c(0, 1), 
        legend.key = element_blank(),
        axis.text.y = element_text(size = rel(.5),
                                   color = blocs_2014[ r$party[ order(r$d1) ] ]))

ggsave(paste0("plots/an_2014_points.pdf"), g, width = 9, height = 18)

g = qplot(data = r, x = d1, fill = party, color = party, alpha = I(1/2),
      geom = "density") +
  geom_density(data = transform(r, party = "All"),
               color = "grey50", fill = "grey75") +
  geom_rug() +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
  scale_color_manual("", values = blocs_2014) +
  scale_fill_manual("", values = blocs_2014) +
  facet_wrap(~ party) +
  labs(x = "\nA-NOMINATE dimension 1", y = "Density\n") +
  theme_bw(18) +
  theme(strip.background = element_rect(fill = "grey90"),
        panel.grid = element_blank(),
        legend.position = "none")

ggsave(paste0("plots/an_2014_density.pdf"), g, width = 9, height = 9)
