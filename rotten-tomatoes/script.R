library(rvest)
library(tidyverse)
library(data.table)   # Required for rbindlist
library(dplyr)        # Required to use the pipes %>% and some table manipulation commands
library(magrittr)     # Required to use the pipes %>%
library(lubridate)
library(stringr)

for (i in 1:967) {

  message(i)
  
  movieTitle <- df[i,]$Title
  movieTitle <- tolower(movieTitle)
  movieTitle <- gsub("&", "and", str_trim(movieTitle))
  movieTitle <- str_replace_all(movieTitle, "[[:punct:]]", "")
  movieTitle <- gsub("\\s+", " ", str_trim(movieTitle))
  movieTitle <- gsub(" ","_",movieTitle)
  
  movieAddress <- paste(movieTitle,"_",df[i,]$Year,sep="")
  
  url <- paste("https://www.rottentomatoes.com/m/",movieAddress,sep="")
  page <- GET(url, add_headers('user-agent' = 'Student data scraper (declanrjb@gmail.com)'))
  
  if ("404" %in% page) {
    url <- paste("https://www.rottentomatoes.com/m/",movieTitle,sep="")
    page <- GET(url, add_headers('user-agent' = 'Student data scraper (declanrjb@gmail.com)'))
  }
  
  if (("404" %in% page) && grepl("iii",movieTitle,fixed=TRUE)) {
    movieTitle <- gsub("iii","3",str_trim(movieTitle))
  }
  
  if (("404" %in% page) && grepl("ii",movieTitle,fixed=TRUE)) {
    movieTitle <- gsub("ii","2",str_trim(movieTitle))
  }
  
  movieAddress <- paste(movieTitle,"_",df[i,]$Year,sep="")
  
  url <- paste("https://www.rottentomatoes.com/m/",movieAddress,sep="")
  page <- GET(url, add_headers('user-agent' = 'Student data scraper (declanrjb@gmail.com)'))
  
  if ("404" %in% page) {
    url <- paste("https://www.rottentomatoes.com/m/",movieTitle,sep="")
    page <- GET(url, add_headers('user-agent' = 'Student data scraper (declanrjb@gmail.com)'))
  }
  
  if (("404" %in% page) && grepl("the_",movieTitle,fixed=TRUE)) {
    movieTitle <- sub("the_","",str_trim(movieTitle))
    
    movieAddress <- paste(movieTitle,"_",df[i,]$Year,sep="")
    
    url <- paste("https://www.rottentomatoes.com/m/",movieAddress,sep="")
    page <- GET(url, add_headers('user-agent' = 'Student data scraper (declanrjb@gmail.com)'))
    
    if ("404" %in% page) {
      url <- paste("https://www.rottentomatoes.com/m/",movieTitle,sep="")
      page <- GET(url, add_headers('user-agent' = 'Student data scraper (declanrjb@gmail.com)'))
    }
  }
  
  if ("404" %in% page) {
    movieTitle <- df[i,]$Title
    movieTitle <- tolower(movieTitle)
    movieTitle <- gsub("&", "and", str_trim(movieTitle))
    movieTitle <- gsub("-", " ", str_trim(movieTitle))
    movieTitle <- str_replace_all(movieTitle, "[[:punct:]]", "")
    movieTitle <- gsub("\\s+", " ", str_trim(movieTitle))
    movieTitle <- gsub(" ","_",movieTitle)
    movieAddress <- paste(movieTitle,"_",df[i,]$Year,sep="")
    
    url <- paste("https://www.rottentomatoes.com/m/",movieAddress,sep="")
    page <- GET(url, add_headers('user-agent' = 'Student data scraper (declanrjb@gmail.com)'))
    
    if ("404" %in% page) {
      url <- paste("https://www.rottentomatoes.com/m/",movieTitle,sep="")
      page <- GET(url, add_headers('user-agent' = 'Student data scraper (declanrjb@gmail.com)'))
    }
  }
  
  scoreboard <- page %>% read_html() %>% html_nodes(".scoreboard")
  audiencescore <- scoreboard %>% html_attr('audiencescore')
  tomatometer <- scoreboard %>% html_attr('tomatometerscore')
  rating <- scoreboard %>% html_attr('rating')
  
  df[i,]$audienceScore <- audiencescore
  df[i,]$tomatoMeter <- tomatometer
  df[i,]$Rating <- rating

}

writeRow <- function(i,df,url) {
  page <- GET(url, add_headers('user-agent' = 'Student data scraper (declanrjb@gmail.com)'))
  scoreboard <- page %>% read_html() %>% html_nodes(".scoreboard")
  audiencescore <- scoreboard %>% html_attr('audiencescore')
  tomatometer <- scoreboard %>% html_attr('tomatometerscore')
  rating <- scoreboard %>% html_attr('rating')
  
  df[i,]$audienceScore <- audiencescore
  df[i,]$tomatoMeter <- tomatometer
  df[i,]$Rating <- rating
  return(df)
}

for (i in 1:length(problems$Title)) {
  message(i)
  df <- writeRow(problems$Rank[i],df,problems$Link[i])
}




