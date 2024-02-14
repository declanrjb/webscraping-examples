library(tidyverse)
library(rvest)
library(tools)
library(stringr)
library(png)

clean_video_obj <- function(elem) {
  elem <- elem %>% html_children() %>%
    html_children() %>%
    html_children()
  if (get_class(elem) == "youtube-player") {
    return(elem)
  } else {
    return(FALSE)
  }
}

retrieve_image <- function(elem,dir) {
  if (grepl("wp-block-image",elem,fixed=TRUE) | grepl("wp-block-post-featured-image",elem,fixed=TRUE)) {
    children <- elem %>% html_children()
    img <- children[1]
    src <- img %>% html_attr("data-orig-file")
    message(src)
    file_path <- paste(dir,"/",basename(src),sep="")
    
    if (grepl(".jpg",file_path,fixed=TRUE)) {
      ext_index <- first_instance(file_path,".jpg") + 3
      file_path <- substr(file_path,1,ext_index)
    } else if (grepl(".webp",file_path,fixed=TRUE)) {
      ext_index <- first_instance(file_path,".webp") + 4
      file_path <- substr(file_path,1,ext_index)
    } else if (grepl(".png",file_path,fixed=TRUE)) {
      ext_index <- first_instance(file_path,".png") + 3
      file_path <- substr(file_path,1,ext_index)
    }
    
    download.file(src,file_path)
    return(basename(src))
  }
}

get_class <- function(elem) {
  return(elem %>% html_attr("class"))
}

retrieve_image_caption <- function(elem,dir) {
  if (grepl("wp-block-image",elem,fixed=TRUE)) {
    children <- elem %>% html_children()
    classes <- children %>% html_attr("class")
    caption_index <- which(classes == "wp-element-caption")
    caption_elem <- children[caption_index]
    return(html_text(caption_elem))
  }
}

get_image_src_url <- function(elem) {
  if (grepl("wp-block-image",elem,fixed=TRUE)) {
    children <- elem %>% html_children()
    img <- children[1]
    src <- img %>% html_attr("data-orig-file")
    return(src)
  } 
}

first_instance <- function(string,substring) {
  window_size <- str_length(substring)
  i <- 1
  j <- i + window_size - 1
  while (j < str_length(string)) {
    curr_window <- substr(string,i,j)
    
    if (curr_window == substring) {
      return(i)
    }
    
    i <- i + 1
    j <- i + window_size - 1
  }
  return(FALSE)
}

next_instance <- function(string,substring,start_index) {
  window_size <- str_length(substring)
  i <- start_index
  j <- i + window_size - 1
  while (j < str_length(string)) {
    curr_window <- substr(string,i,j)
    
    if (curr_window == substring) {
      return(i)
    }
    
    i <- i + 1
    j <- i + window_size - 1
  }
  return(FALSE)
}

replace_attr <- function(text_obj,attr,new_value) {
  attr_name_start <- first_instance(text_obj,attr)
  attr_start <- next_instance(text_obj,'="',attr_name_start) + 2
  attr_end <- next_instance(text_obj,'"',attr_start) - 2
  substr(text_obj,attr_start,attr_end) <- new_value
  return(text_obj)
}

clean_image_obj <- function(elem,dir) {
  local_file <- retrieve_image(elem,dir)
  elem_text <- create_new_image_obj(local_file)
  caption_elem <- create_caption_obj(retrieve_image_caption(elem))
  elem_text <- paste(elem_text,caption_elem,sep="\n")
  return(elem_text)
}

create_new_image_obj <- function(local_addr) {
  text <- paste('<img src="',local_addr,'">',sep="")
  return(text)
}

create_caption_obj <- function(caption_text) {
  obj_text <- paste('<p class="caption">',caption_text,"</p>",sep="")
  return(obj_text)
}

create_object <- function(anchor,class,inner_content) {
  obj_text <- paste('<',anchor,' class="',class,'">',inner_content,'</',anchor,'>',sep="")
  return(obj_text)
}

retrieve_article_title <- function(url) {
  page <- read_html(url)
  title <- page %>% html_nodes(".post-header-title") %>% html_text()
  if (length(title) == 1) {
    return(title)
  } else {
    title <- url
    title <- gsub("https://reedquest.org/","",title)
    title <- gsub("/","_",title)
    return(title)
  }
}

get_category <- function(page) {
  category <- page %>% html_nodes(".post-categories") %>% html_text()
  category <- unique(category)
  pasted_form <- str_squish(paste(category,collapse=", "))
  return(pasted_form)
}

retrieve_date <- function(page) {
  return(page %>% html_nodes(".wp-block-post-date") %>% html_text())
}