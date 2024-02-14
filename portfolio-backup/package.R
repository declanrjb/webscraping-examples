source("functions.R")

package_page <- function(url) {
  message(url)
  page <- read_html(url)
  
  title <- retrieve_article_title(url)
  
  directory <- str_to_lower(title)
  directory <- gsub('[[:punct:] ]+',' ',directory)
  directory <- str_squish(directory)
  directory <- gsub(" ","_",directory)
  
  if (!dir.exists(directory)) {
    dir.create(directory)
  }
  
  title_obj <- create_object("h1","page-title",title)
  
  category <- get_category(page)
  date <- retrieve_date(page)
  category_obj <- create_object("h3","page-category",paste(category," (",date,")",sep=""))
  
  featured_image <- page %>% html_nodes(".post-header-featured-image")
  if (length(featured_image) > 0) {
    featured_image <- featured_image[1]
    featured_obj <- clean_image_obj(featured_image,directory)
  } else {
    featured_obj <- ""
  }
  
  body <- page %>% html_nodes(".entry-content")
  
  code <- html_children(body)
  
  class_list <- unlist(lapply(code,get_class))
  
  video_indices <- which(grepl("wp-block-embed-youtube",class_list))
  for (index in video_indices) {
    code[index] <- clean_video_obj(code[index])
  }
  
  text_code <- as.character(code)
  
  image_indices <- which(grepl("wp-block-image",class_list))
  
  for (index in image_indices) {
    text_code[index] <- clean_image_obj(code[index],directory)
  }
  
  text_code <- text_code[-c(which(grepl("sharedaddy",class_list)))]
  
  text_code <- c(featured_obj,category_obj,title_obj,text_code)
  
  margin <- paste('<','div',' class="','column','"','style="width: 20%; height: 100px;"','>','</','div','>',sep="")
  
  text_code <- paste(text_code, collapse = "\n")
  
  wrapped_code <- paste('<','div',' class="','column','"','style="width: 60%;"','>',
                        text_code,
                        '</','div','>',
                        sep="")
  
  inner_row <- c(margin,wrapped_code,margin)
  
  inner_row <- paste(inner_row, collapse = "\n")
  
  wrapped_row <- paste('<','div',' class="','row','"','style="height: fit-content;"','>',
                       inner_row,
                       '</','div','>',
                       sep="")
  
  wrapped_row <- paste('<link rel="stylesheet" href="general_styles.css">','\n',wrapped_row,sep="")
  
  final_file_path <- paste(directory,"/","index.html",sep="")
  
  write.table(wrapped_row, 
              file=final_file_path, 
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
  
  file.copy(from="general_styles.css",to=paste(directory,"/","general_styles.css",sep=""))
  
  folder_files <- list.files(directory)
  folder_files <- paste(directory,"/",folder_files,sep="")
  zip_name <- paste(directory,".zip",sep="")
  zip(zip_name,folder_files)
  
  unlink(directory,recursive=TRUE)
  
  return(zip_name)
}