#===============================================================================
# DOWNLOAD VOTES 2011
#===============================================================================

f = "raw/indexes/votes_2011.html"

if(!file.exists(f))
  download.file("http://www.marsad.tn/fr/votes", f,
                mode = "wb", quiet = TRUE)

h = html(f) %>%
  html_nodes("div#main a") %>%
  html_attr("href")

h = h[ grepl("/vote/\\d+", h) ] %>%
  unique

cat("2011: downloading", length(h), "votes...\n")

pb = txtProgressBar(0, length(h), style = 3)
for(i in h) {
  
  f = paste0("raw/votes/2011/", basename(i), ".html")
  
  setTxtProgressBar(pb, which(h == i))
  
  if(!file.exists(f))
    download.file(paste0("http://www.marsad.tn", i), f,
                  mode = "wb", quiet = TRUE)
  
  if(!file.info(f)$size)
    file.remove(f)
  
}

cat("\n")

#===============================================================================
# EXPORT VOTES 2011
#===============================================================================

cat("2011: exporting votes metadata...\n")
h = data_frame()

for(i in list.files("raw/votes/2011", full.names = TRUE)) {
  
  f = html(i)
  h = rbind(h, data_frame(
    uid = i,
    title = html_node(f, xpath = "//meta[@property='og:title']") %>%
      html_attr("content"),
    date = html_node(f, xpath = "//div[@id='main']/div/h3[1]") %>%
      html_text
  ))
  
}

h$uid = gsub("\\.html", "", basename(h$uid))
h$date = parse_date_time(h$date, "%d %m %Y", locale = "fr_FR") %>% as.Date

write_csv(arrange(h, date), "data/votes_2011.csv")

#===============================================================================
# DOWNLOAD VOTES 2014
#===============================================================================

f = "raw/indexes/votes_2014_page_1.html"

if(!file.exists(f))
  download.file("http://majles.marsad.tn/2014/fr/votes", f,
                mode = "wb", quiet = TRUE)

h = html(f)
n = html_nodes(h, "div#main a") %>%
  html_attr("href")

v = n[ grepl("/votes$", n) ]
p = n[ grepl("?before", n) ]
i = 1

while(length(p) > 0) {
  
  f = paste0("raw/indexes/votes_2014_page_", i + 1, ".html")
  i = i + 1
  
  if(!file.exists(f))
    download.file(paste0("http://majles.marsad.tn", p), f,
                  mode = "wb", quiet = TRUE)
  
  h = html(f)
  n = html_nodes(h, "div#main a") %>% html_attr("href")
  v = c(v, n[ grepl("/votes$", n) ])
  p = n[ grepl("?before", n) ]
  
}

v = unique(v)
h = c()

cat("2014: downloading", length(v), "bills...\n")

for(i in v) {
  
  f = paste0("raw/bills/", gsub("/votes$", "", i) %>% basename, ".html")
  
  if(!file.exists(f))
    download.file(paste0("http://majles.marsad.tn", i), f,
                  mode = "wb", quiet = TRUE)

  f = html(f) %>%
    html_nodes("div#main a") %>%
    html_attr("href")
  
  h = c(h, f)
  
}

cat("\n")

h = h[ grepl("/vote/", h) ] %>% unique

cat("2014: downloading", length(h), "votes...\n")

pb = txtProgressBar(0, length(h), style = 3)
for(i in h) {
  
  f = paste0("raw/votes/2014/", basename(i), ".html")
  
  setTxtProgressBar(pb, which(h == i))
  
  if(!file.exists(f))
    download.file(paste0("http://majles.marsad.tn", i), f,
                  mode = "wb", quiet = TRUE)
  
  if(!file.info(f)$size)
    file.remove(f)
  
}

cat("\n")

#===============================================================================
# EXPORT VOTES 2014
#===============================================================================

cat("2014: exporting votes metadata...\n")
h = data_frame()

for(i in list.files("raw/votes/2014", full.names = TRUE)) {
  
  f = html(i)
  h = rbind(h, data_frame(
    uid = i,
    title = html_node(f, xpath = "//div[@id='main']/h1") %>%
      html_text,
    date = html_node(f, xpath = "//div[@id='main']/h5") %>%
      html_text
  ))
  
}

h$uid = gsub("\\.html", "", basename(h$uid))
h$date = parse_date_time(h$date, "%d %m %Y", locale = "fr_FR") %>% as.Date

#===============================================================================
# APPEND BILL TITLES
#===============================================================================

p = data_frame()

for(i in list.files("raw/bills", full.names = TRUE)) {
  
  f = html(i)
  
  p = rbind(p, data_frame(
    bill = html_node(f, "h1") %>%
      html_text,
    uid = html_nodes(f, "div#main a") %>%
      html_attr("href")
  ))
  
}

h = filter(p, grepl("/vote/", uid)) %>%
  mutate(uid = basename(uid)) %>%
  inner_join(h, by = "uid") %>%
  mutate(title = paste0(bill, ". ", title)) %>%
  dplyr::select(uid, title, date)

write_csv(arrange(h, date), "data/votes_2014.csv")
