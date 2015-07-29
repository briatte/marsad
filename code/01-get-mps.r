#===============================================================================
# DOWNLOAD MPs 2011
#===============================================================================

f = "raw/indexes/mps_2011.html"

if(!file.exists(f))
  download.file("http://www.marsad.tn/fr/assemblee", f,
                mode = "wb", quiet = TRUE)

h = html(f) %>%
  html_nodes("a.depute")

h = data_frame(
  url = html_attr(h, "href"),
  seat = html_attr(h, "data-siege"),
  name = html_attr(h, "data-nom"),
  sex = html_attr(h, "data-sexe"),
  age = html_attr(h, "data-age"),
  bloc = html_attr(h, "data-groupe"),
  list = html_attr(h, "data-liste"),
  # party = html_attr(h, "data-parti"),
  photo = NA, # html_attr(h, "data-photo"),
  region = html_attr(h, "data-region")
)

stopifnot(!duplicated(h$name))

cat("2011: downloading", nrow(h), "MPs...\n")

pb = txtProgressBar(0, nrow(h), style = 3)
for(i in 1:nrow(h)) {
  
  # profile
  f = paste0("raw/mps/", gsub("\\s", "_", h$name[ i ]), "_2011.html") %>%
    tolower

  setTxtProgressBar(pb, i)
  
  if(!file.exists(f))
    download.file(paste0("http://www.marsad.tn", h$url[ i ]), f,
                  mode = "wb", quiet = TRUE)
  
  # photo
  u = html(f) %>%
    html_node(xpath = "//meta[@property='og:image']") %>%
    html_attr("content") %>%
    gsub("\\s", "%20", .)
  
  f = paste0("raw/mps/", gsub("\\s", "_", h$name[ i ]), "_2011.",
             gsub("(.*)\\.", "", basename(u))) %>%
    tolower

  if(!file.exists(f))
    download.file(u, f,
                  mode = "wb", quiet = TRUE)
  
  if(file.info(f)$size > 0)
    h$photo[ i ] = basename(f)
  else
    h$photo[ i ] = NA
  
}

cat("\n")
h$sex = ifelse(h$sex == "Femmes", "F", "M")
h$age = ifelse(h$age == 0, NA, as.integer(h$age))

stopifnot(h$bloc %in% names(blocs_2011))
write_csv(h, "data/mps_2011.csv")

## might be added:
## party, bureau, comm, votes, bio

#===============================================================================
# DOWNLOAD MPs 2014
#===============================================================================

f = "raw/indexes/mps_2014.html"

if(!file.exists(f))
  download.file("http://majles.marsad.tn/2014/fr/assemblee", f,
                mode = "wb", quiet = TRUE)

h = html(f) %>%
  html_nodes("a.depute")

h = data_frame(
  url = html_attr(h, "href"),
  seat = html_attr(h, "data-siege"),
  name = html_attr(h, "data-nom"),
  sex = html_attr(h, "data-sexe"),
  age = html_attr(h, "data-age"),
  bloc = html_attr(h, "data-bloc"),
  list = html_attr(h, "data-liste"),
  # profession = html_attr(h, "data-profession"),
  photo = NA, # html_attr(h, "data-photo"),
  region = html_attr(h, "data-region")
)

stopifnot(!duplicated(h$name))

cat("2014: downloading", nrow(h), "MPs...\n")

pb = txtProgressBar(0, nrow(h), style = 3)
for(i in 1:nrow(h)) {
  
  # profile
  f = paste0("raw/mps/", gsub("\\s", "_", h$name[ i ]), "_2014.html") %>%
    tolower
  
  setTxtProgressBar(pb, i)
  
  if(!file.exists(f))
    download.file(paste0("http://majles.marsad.tn", h$url[ i ]), f,
                  mode = "wb", quiet = TRUE)
  
  # photo
  u = html(f) %>%
    html_node("img.photo") %>%
    html_attr("src") %>%
    gsub("\\s", "%20", .)
  
  f = paste0("raw/mps/", gsub("\\s", "_", h$name[ i ]), "_2014.",
             gsub("(.*)\\.", "", basename(u))) %>%
    tolower
  
  if(!file.exists(f))
    download.file(paste0("http://majles.marsad.tn", u), f,
                  mode = "wb", quiet = TRUE)
  
  if(file.info(f)$size > 0)
    h$photo[ i ] = basename(f)
  else
    h$photo[ i ] = NA
  
}

cat("\n")
h$sex = ifelse(h$sex == "Femmes", "F", "M")
h$age = ifelse(h$age == 0, NA, as.integer(h$age))

h$bloc[ h$bloc == "Mouvement Ennahdha" ] = "Bloc Nahdha"
h$bloc[ h$bloc == "Mouvement Nidaa Tounes" ] = "Nidaa Tounes"

stopifnot(h$bloc %in% names(blocs_2014))
write_csv(h, "data/mps_2014.csv")

## might be added:
## profession, comm, votes, discipline, presence_plen, presence_comm
