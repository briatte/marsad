if(!file.exists("data/ergm.rda")) {
  
  # excluding buggy birth year variable
  ERGM = ergm(net ~ edges +
                gwdegree(decay = 1, fixed = TRUE) +
                nodefactor("bloc") +
                nodematch("bloc", diff = TRUE) + 
                nodefactor("sexe") +
                nodematch("sexe"),
              control = control.ergm(MCMLE.maxit = 100))
  
  save(ERGM, file = "data/ergm.rda")
  
}

load("data/ergm.rda")

print(summary(ERGM))

coefs = summary(ERGM)$coefs
names(coefs) = c("b", "se", "mcmc", "p")
coefs$v = rownames(coefs)

g = qplot(data = subset(coefs, grepl("nodematch", v)),
          y = b, ymin = b - se, ymax = b + se,
          x = reorder(v, b), geom = "pointrange") + 
  coord_flip() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  theme_bw(18) +
  labs(y = NULL, x = NULL)

ggsave("plots/ergm_homophilies.pdf", g, width = 12, height = 9)

g = qplot(data = subset(coefs, grepl("edges|nodefactor", v)),
          y = b, ymin = b - se, ymax = b + se,
          x = reorder(v, b), geom = "pointrange") + 
  coord_flip() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  theme_bw(18) +
  labs(y = NULL, x = NULL)

ggsave("plots/ergm_controls.pdf", g, width = 12, height = 9)

# kthxbye
