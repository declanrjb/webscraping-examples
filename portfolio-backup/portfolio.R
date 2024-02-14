source("functions.R")
source("package.R")

url <- "https://reedquest.org/2023/10/19/author-declan-bradley/"

page <- read_html(url)

articles <- page %>% html_nodes(".m-a-posts-list-item-title") %>% html_children() %>% html_attr("href")

lapply(articles,package_page)