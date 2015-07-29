#===============================================================================
# DOWNLOAD ELECTORAL LAW AMENDMENTS 2011
#===============================================================================

cat("2011: downloading 171 electoral law amendments...\n")

pb = txtProgressBar(0, 170, style = 3)
for(i in 1:170) {
  
  f = paste0("raw/amendments/loi_electorale_", i, ".html")
  
  setTxtProgressBar(pb, as.numeric(i))
  
  if(!file.exists(f))
    download.file(paste0("http://www.marsad.tn/fr/lois/loi_electorale/article/", i), f,
                  mode = "wb", quiet = TRUE)
  
  if(!file.info(f)$size)
    file.remove(f)
  
}

if(!file.exists("raw/amendments/loi_electorale_ajout_articles.html"))
  download.file("http://www.marsad.tn/fr/lois/loi_electorale/ajout_articles",
                "raw/amendments/loi_electorale_ajout_articles.html",
                mode = "wb", quiet = TRUE)

cat("\n")

#===============================================================================
# DOWNLOAD CONSTITUTION AMENDMENTS 2011
#===============================================================================

h = read_csv("data/mps_2011.csv")$url

cat("2011: downloading", length(h), "constitution amendment lists...\n")

pb = txtProgressBar(0, length(h), style = 3)
for(i in h) {
  
  f = paste0("raw/amendments/constitution_", basename(i), ".html")
  
  setTxtProgressBar(pb, which(h == i))
  
  if(!file.exists(f))
    download.file(paste0("http://www.marsad.tn", i, "/amendements"), f,
                  mode = "wb", quiet = TRUE)
  
  if(!file.info(f)$size)
    file.remove(f)
    
}

if(!file.exists("raw/amendments/constitution_ajout_articles.html"))
  download.file("http://majles.marsad.tn/fr/constitution/amendements",
                "raw/amendments/constitution_ajout_articles.html",
                mode = "wb", quiet = TRUE)

cat("\n")

#===============================================================================
# PARSE ELECTORAL LAW AMENDMENTS 2011
#===============================================================================

cat("2011: parsing electoral law amendments...\n")

n = data_frame()

for(i in list.files("raw/amendments", pattern = "loi_electorale", full.names = TRUE)) {
  
  h = html(i)
  
  s = html_nodes(h, xpath = "//div[starts-with(@id, 'deps')]") %>%
    lapply(html_nodes, "a") %>%
    lapply(html_attr, "href") %>%
    sapply(paste0, collapse = ";")
  
  if(length(s) > 0)
    n = rbind(n, data_frame(
      chapter = html_node(h, "div.titre") %>% html_text,
      article = ifelse(!grepl("\\d", i), "ajouts",
                       html_node(h, "div.titre span") %>%
                         html_text),
      uid = html_nodes(h, xpath = "//div[starts-with(@id, 'deps')]") %>%
        html_attr("id"),
      sponsors = s
    ))
  
}

n$article = gsub("Article ", "", n$article)

n$chapter[ n$article %in% 1:3 ] =     1 # dispo. générales
n$chapter[ n$article %in% 4:17 ] =    2 # électeur
n$chapter[ n$article %in% 18:46 ] =   3 # candidat
n$chapter[ n$article %in% 47:97 ] =   4 # campagne
n$chapter[ n$article %in% 98:146 ] =  5 # scrutin
n$chapter[ n$article %in% 147:163 ] = 6 # infractions électorales
n$chapter[ n$article %in% 164:170 ] = 7 # dispo. finales et transitoires
n$chapter = paste0("ch", n$chapter)
n$chapter[ n$article == "ajouts" ] = "ajouts"
table(n$chapter, exclude = NULL)

n$uid = gsub("deps-", "", n$uid)
stopifnot(!duplicated(n$uid))

write_csv(n, "data/amendments_electoral_law.csv")

#===============================================================================
# PARSE CONSTITUTION AMENDMENTS 2011
#===============================================================================

cat("2011: parsing constitution amendments...\n")

n = data_frame()

for(i in list.files("raw/amendments", pattern = "constitution", full.names = TRUE)) {
  
  h = html(i, encoding = "UTF-8")

  s = html_nodes(h, xpath = "//p[starts-with(@class, 'deps')]") %>%
    lapply(html_nodes, "a") %>%
    lapply(html_attr, "href") %>%
    sapply(paste0, collapse = ";")
  
  if(length(s) > 0)
    n = rbind(n, data_frame(
      chapter = NA,
      article = html_nodes(h, xpath = "//a[contains(@href, 'constitution')]") %>%
        html_text,
      uid = html_nodes(h, xpath = "//p[starts-with(@class, 'deps')]") %>%
        html_attr("id"),
      sponsors = s
    ))
  
}

n$article = gsub("(.*)(Préambule|article \\d+)(.*)", "\\2", n$article)
n$article = gsub("article ", "", n$article)

n$chapter[ n$article == "Préambule" ] = "préambule"
n$chapter[ n$article %in% 1:20 ] = "ch1" # principes généraux
n$chapter[ n$article %in% 21:49 ] = "ch2" # droits et libertés
n$chapter[ n$article %in% 50:70 ] = "ch3" # pouvoir législatif
n$chapter[ n$article %in% 71:101 ] = "ch4" # pouvoir exécutif
n$chapter[ n$article %in% 102:124 ] = "ch5" # pouvoir judiciaire
# n$chapter[ n$article %in% 106:117 ] = "ch5_1" # titre 1, justice
# n$chapter[ n$article %in% 118:124 ] = "ch5_2" # titre 2, cour constit.
n$chapter[ n$article %in% 125:130 ] = "ch6" # instances indép.
n$chapter[ n$article %in% 131:142 ] = "ch7" # pouvoir local
n$chapter[ n$article %in% 143:144 ] = "ch8" # révision constit.
n$chapter[ n$article %in% 145:147 ] = "ch9" # dispo. finales
n$chapter[ n$article %in% 148:149 ] = "ch10" # dispo. transitoires
table(n$chapter, exclude = NULL)

n = unique(n) # since parser is inefficient
stopifnot(!duplicated(n$uid))

write_csv(n, "data/amendments_constitution.csv")

cat("\n")
