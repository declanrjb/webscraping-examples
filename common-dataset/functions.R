library(tidyverse)
library(RSelenium)
library(rvest)
library(RCurl)
library(pdftools)
library(stringr)
library(stringi)
library(plyr)
library(ggbeeswarm)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)

vec_paste <- function(vec) {
  result <- ""
  for (x in vec) {
    result <- paste(result,x,sep="")
  }
  return(unlist(result))
}

first_split <- function(string,sep) {
  vec <- unlist(str_split(string,sep))
  front <- vec[1]
  vec <- vec[-1]
  back <- vec_paste(vec)
  result <- c(front,back)
  result <- unlist(result)
  result <- result[str_length(result) > 0]
  return(result)
}

row_clean <- function(row) {
  row <- first_split(row,":")
  row <- str_squish(row)
  return(row)
}

list_filter <- function(list,filter) {
  i <- 1
  while (i <= length(list)) {
    if (!filter(list[[i]])) {
      list <- list[-i]
    } else {
      i <- i + 1
    }
  }
  return(list)
}

transform_vectorized_list <- function(list) {
  max_cols <- 0
  for (x in list) {
    if (length(x) > max_cols) {
      max_cols <- length(x)
    }
  }
  
  df <- as.data.frame(matrix(nrow=length(list),ncol=max_cols))
  
  for (i in 1:length(list)) {
    vec <- list[[i]]
    for (j in 1:length(vec)) {
      df[i,j] <- vec[j]
    }
  }
  
  return(df)
}

table_from_text <- function(text) {
  rows <- unlist(str_split(text,"\n"))
  rows <- rows[str_length(rows) > 0]
  rows <- rows[grepl(":",rows)]
  row_list <- lapply(rows,row_clean)
  
  evaluator <- function(x) {
    return(length(x) == 2)
  }
  
  row_list <- list_filter(row_list,evaluator)
  
  if (length(row_list) > 0) {
    df <- transform_vectorized_list(row_list)
    colnames(df) <- c("Val_Type","Val")
    
    return(df)
  } else {
    return(FALSE)
  }
}

retrieve_college_name <- function(full_text) {
  index <- which(grepl("Name of College/University",full_text,fixed=TRUE))
  name_sec <- full_text[index]
  name_table <- table_from_text(name_sec)
  return(name_table[['Val']][which(name_table[['Val_Type']] == "Name of College/University")])
}

retrieve_name_table <- function(full_text) {
  index <- which(grepl("Name of College/University",full_text,fixed=TRUE))
  
  if (length(index > 1)) {
    index <- index[1]
  } else if (length(index) == 0) {
    return(FALSE)
  }
  
  name_sec <- full_text[index]
  name_table <- table_from_text(name_sec)
  return(name_table)
}

name_from_name_table <- function(name_table) {
  index <- which(name_table[['Val_Type']] == "Name of College/University")
  if (length(index) > 1) {
    index <- index[1]
  } else if (length(index) == 0) {
    return(FALSE)
  } else {
    return(name_table[['Val']][index])
  }
}

string_index <- function(string,index) {
  return(substr(string,index,index))
}

consec_space_counter <- function(string) {
  i <- 1
  while ((i <= (str_length(string)-1)) & (substr(string,i,i+1) != "  ")) {
    i <- i + 1
  }
  if (i == str_length(string)) {
    # terminate
    return(0)
  } else {
    j <- i
    while ((j <= str_length(string)) & (substr(string,j,j) == " ")) {
      j <- j + 1
    }
    return(j-i)
  }
}

strip_before_consec_space <- function(string) {
  i <- 1
  while ((i <= (str_length(string)-1)) & (substr(string,i,i+1) != "  ")) {
    i <- i + 1
  }
  return(str_trim(substr(string,1,i)))
}

strip_before_consec_space_ending_x <- function(string) {
  string <- gsub(" X","",string,fixed=TRUE)
  return(str_squish(string))
}

generate_priority_table <- function(file_address) {
  text <- pdf_text(file_address)
  merge_text <- vec_paste(text)
  search_string <- "Relative importance of each of the following academic and nonacademic factors"
  termination_string <- "C8"
  splice <- gregexpr(search_string,merge_text,fixed=TRUE)[[1]][1] + str_length(search_string)
  end_splice <- gregexpr(termination_string,merge_text,fixed=TRUE)[[1]][1]
  weight_sec <- substr(merge_text,splice,end_splice)
  
  if (str_length(weight_sec) == 0) {
    return(FALSE)
  }
  
  rows <- str_split(weight_sec,"\n")
  rows <- unlist(rows)
  rows <- str_trim(rows)
  rows <- rows[str_length(rows) > 0]
  rows <- rows[grepl(" X",rows,fixed=TRUE) | grepl(" x",rows,fixed=TRUE)]
  
  priority_table <- as.data.frame(matrix(ncol=2,nrow=length(rows)))
  colnames(priority_table) <- c("Priority","Weight")
  
  priority_table$Priority <- unlist(lapply(rows,strip_before_consec_space_ending_x))
  priority_table$Weight <- str_length(rows)
  
  unique_weights <- unique(priority_table$Weight)
  for (x in unique_weights) {
    count <- length(which(priority_table$Weight == x))
    message(paste(x,": ",count,sep=""))
  }
  
  return(priority_table)
}

bind_backup_csvs <- function(start_path) {
  all_csvs <- list.files(start_path)
  all_csvs <- paste(start_path,"/",all_csvs,sep="")
  result <- read_csv(all_csvs[1])
  for (i in 2:length(all_csvs)) {
    temp_df <- read_csv(all_csvs[i])
    result <- rbind(result,temp_df)
  }
  return(result)
}

vec_count <- function(vector,x) {
  count <- 0
  for (y in vector) {
    if (y == x)
      count <- count + 1
  }
  return(count)
}

freq_table <- function(vector) {
  values <- unique(vector)
  result <- as.data.frame(matrix(ncol=2,nrow=length(values)))
  colnames(result) <- c("Value","N")
  result$Value <- values
  for (i in 1:length(result$Value)) {
    result[i,]$N <- vec_count(vector,result[i,]$Value)
  }
  result <- as.data.frame(result)
  return(result)
}

very <- 41:49
important <- 60:70
considered <- 76:89
not_considered <- 96:106

ranges_values <- list(very,important,considered,not_considered)
ranges_names <- c("Very Important","Important","Considered","Not Considered")

spaces_to_name <- function(x) {
  i <- 1
  while (i <= length(ranges_values)) {
    if (x %in% ranges_values[[i]]) {
      return(ranges_names[i])
    } else {
      i <- i + 1
    }
  }
  return("SCRIPT FAILURE")
}

importance_to_numeric <- function(importance) {
  if (importance %in% ranges_names) {
    index <- which(ranges_names == importance)
    return(4-index)    
  } else {
    return(NA)
  }
}

whittle <- function(df,uni_col,fail_col) {
  units <- unique(df[[{{uni_col}}]])
  for (sort in units) {
    temp_df <- df[(which(df[[{{uni_col}}]] == sort)),]
    if ("SCRIPT FAILURE" %in% temp_df[[{{fail_col}}]]) {
      df <- df[(which(df[[{{uni_col}}]] != sort)),]
    }
  }
  message(paste("Original Unique: ",length(units),sep=""))
  message(paste("Now Unique: ",length(unique(df[[{{uni_col}}]])),sep=""))
  message(length(unique(df[[{{uni_col}}]]))/length(units))
  return(df)
}

double_filter_lookup <- function(df,x_col,y_col,x,y,output_col) {
  first_indices <- which(df[[{{x_col}}]] == x)
  if (length(first_indices) == 0) {
    return(NA)
  }
  df <- df[first_indices,]
  second_indices <- which(df[[{{y_col}}]] == y)
  if (length(second_indices) == 0) {
    return(NA)
  }
  df <- df[second_indices,]
  results <- df[[{{output_col}}]]
  return(results)
}

url_from_result <- function(result_obj) {
  anchor <- result_obj$findChildElements("css","a")
  if (length(anchor) >= 1) {
    anchor <- anchor[[1]]
    url <- unlist(anchor$getElementAttribute("href"))
    return(url)
  } else {
    return(NA)
  }
}

download_from_url <- function(url,dir_path) {
  file_name <- gsub("https://","",url)
  file_name <- gsub("/","_",file_name)
  file_name <- paste(dir_path,"/",file_name,sep="")
  if (file.exists(file_name)) {
    return(FALSE)
  } else if (url.exists(url)) {
    download.file(url,file_name)
    return(TRUE)
  } else {
    return(FALSE)
  }
}

get_page_height <- function() {
  doc_body <- remDr$findElement("css","body")
  return(parse_number(unlist(doc_body$getElementAttribute("scrollHeight"))))
}

scroll_to_bottom <- function() {
  webElem <- remDr$findElement("css", "body")
  past_height <- 0
  curr_height <- parse_number(unlist(webElem$getElementAttribute("scrollHeight")))
  while ((curr_height - past_height) > 0) {
    past_height <- curr_height
    webElem$sendKeysToElement(list(key = "end"))
    Sys.sleep(1)
    curr_height <- parse_number(unlist(webElem$getElementAttribute("scrollHeight")))
  }
}

scrape_page <- function() {
  results <- remDr$findElements("css",".MjjYud")
  
  urls <- unlist(lapply(results,url_from_result))
  urls <- urls[!is.na(urls)]
  urls <- urls[grepl(".edu",urls,fixed=TRUE)]
  
  lapply(urls,download_from_url,"cds_all")
}

expand_results <- function() {
  expand_button <- remDr$findElement("css",".GNJvt.ipz2Oe")
  if (length(expand_button) >= 1) {
    expand_button$clickElement()
    return(TRUE)
  } else {
    return(FALSE)
  }
}

generate_search_url_from_name <- function(college_name) {
  college_name <- str_to_lower(college_name)
  college_name <- gsub('[[:punct:] ]+',' ',college_name)
  college_name <- str_squish(college_name)
  college_name <- gsub(" ","+",college_name)
  
  search_url <- paste("https://www.google.com/search?q=site:.edu+filetype:pdf+",college_name,"+common+data+set+2022-2023+admission&start=0",sep="")
  return(search_url)
}

generate_search_url <- function(college_url) {
  college_url <- gsub("https://","",college_url,fixed=TRUE)
  
  search_url <- paste("https://www.google.com/search?q=site:",college_url,"+filetype:pdf+common+data+set+2022-2023&start=0",sep="")
  return(search_url)
}

download_cds_guess <- function(college_name,college_url) {
  search_url <- generate_search_url(college_url)
  
  remDr$navigate(search_url)
  
  result <- remDr$findElements("css",".MjjYud")[[1]]
  
  if (length(result) > 0) {
    url <- url_from_result(result)
    if (!is.na(url)) {
      college_name <- str_to_lower(college_name)
      college_name <- gsub('[[:punct:] ]+',' ',college_name)
      college_name <- str_squish(college_name)
      college_name <- gsub(" ","_",college_name)
      return(download_from_url(url,"cds_all"))    
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

go_forth_and_scrape <- function(j) {
  search_url <- "https://www.google.com/search?q=site:.edu+filetype:pdf+common+data+set+2022-2023&start=0"
  
  remDr$navigate(search_url)
  
  i <- 0
  while (i < j) {
    scroll_to_bottom()
    scrape_page()
    if (expand_results()) {
      i <- i + 1 
    } else {
      break
    }
  }
}

randFloat <- function(min,max) {
  difference <- max - min
  result <- min + (runif(1) * difference)
  return(result)
}

randInt <- function(min,max) {
  difference <- max - min
  result <- min + (runif(1) * difference)
  result <- as.integer(result)
  return(result)
}

shared_vec <- function(vec1,vec2) {
  combined_vec <- c(vec1,vec2)
  combined_vec <- unique(combined_vec)
  result <- c()
  
  for (x in combined_vec) {
    if ((x %in% vec1) & (x %in% vec2)) {
      result <- c(result,x)
    }
  }
  
  return(result)
}

standard_clean <- function(vec) {
  vec <- gsub('[[:punct:] ]+',' ',vec)
  vec <- str_to_lower(vec)
  vec <- str_squish(vec)
  vec <- stri_trans_general(vec,"Latin-ASCII")
  return(vec)
}

mean_by_filter <- function(df,filter_column,val_column,filter_value) {
  indices <- which(df[[{{filter_column}}]] == filter_value)
  df <- df[indices,]
  return(mean(df[[{{val_column}}]],na.rm=TRUE))
}

stat_by_filter <- function(df,filter_column,val_column,filter_value,stat_func) {
  indices <- which(df[[{{filter_column}}]] == filter_value)
  df <- df[indices,]
  indices <- which(!is.na(df[[{{val_column}}]]) & (df[[{{val_column}}]] != Inf))
  removed_vals <- dim(df)[1] - length(indices)
  message(paste(removed_vals," values removed",sep=""))
  df <- df[indices,]
  return(stat_func(df[[{{val_column}}]]))
}

build_means_table <- function(df,filter_column,val_column) {
  sorts <- unique(df[[{{filter_column}}]])
  result <- as.data.frame(matrix(ncol=4,nrow=length(sorts)))
  colnames(result) <- c("Filter_Col","Val_Col","Filter_Val","MEAN")
  result$Filter_Col <- filter_column
  result$Val_Col <- val_column
  for (i in 1:length(sorts)) {
    curr_sort <- sorts[i]
    result[i,]$MEAN <- mean_by_filter(df,filter_column,val_column,curr_sort)
    result[i,]$Filter_Val <- curr_sort
  }
  result <- result %>% arrange(Filter_Val)
  return(result)
}

build_stat_table <- function(df,filter_column,val_column,stat_func) {
  sorts <- unique(df[[{{filter_column}}]])
  result <- as.data.frame(matrix(ncol=4,nrow=length(sorts)))
  colnames(result) <- c("Filter_Col","Val_Col","Filter_Val","STAT")
  result$Filter_Col <- filter_column
  result$Val_Col <- val_column
  for (i in 1:length(sorts)) {
    curr_sort <- sorts[i]
    result[i,]$STAT <- stat_by_filter(df,filter_column,val_column,curr_sort,stat_func)
    result[i,]$Filter_Val <- curr_sort
  }
  result <- result %>% arrange(Filter_Val)
  return(result)
}

means_table_original_colnames <- function(df,filter_column,val_column) {
  result <- build_means_table(df,filter_column,val_column)
  result <- result %>% select(!Filter_Col) %>% select(!Val_Col)
  colnames(result) <- c(filter_column,val_column)
  return(result)
}

stat_table_original_colnames <- function(df,filter_column,val_column,stat_func) {
  result <- build_stat_table(df,filter_column,val_column,stat_func)
  result <- result %>% select(!Filter_Col) %>% select(!Val_Col)
  colnames(result) <- c(filter_column,val_column)
  return(result)
}

stat_table_across_range <- function(df,filter_column,val_columns,stat_func) {
  result <- stat_table_original_colnames(df,filter_column,val_columns[1],stat_func)
  for (i in 2:length(val_columns)) {
    temp_table <- stat_table_original_colnames(df,filter_column,val_columns[i],stat_func)
    result <- left_join(result,temp_table,by=filter_column)
  }
  return(result)
}

count_instances <- function(vec,value) {
  return(length(vec[vec == value]))
}

perc_instances <- function(vec,value) {
  return(length(vec[vec == value]) / length(vec))
}

build_means_table_over_range <- function(df,filter_range,val_column) {
  result <- build_means_table(df,filter_range[1],val_column)
  for (i in 2:length(filter_range)) {
    temp <- build_means_table(df,filter_range[i],val_column)
    result <- rbind(temp,result)
  }
  return(result)
}

parse_percent <- function(dec) {
  dec <- dec * 100
  dec <- round(dec,1)
  dec <- paste(dec,"%",sep="")
  return(dec)
}

title_wrapper <- function(x,wrap) {
  x <- paste(strwrap(x,wrap),collapse="\n")
  return(x)
}

assemble_combined_plot <- function(df,filter_col,val) {
  means_table <- build_means_table(df,filter_col,val)
  means_table <- means_table %>% filter(!is.na(Filter_Val))
  graph_df <- df %>% select({{filter_col}},{{val}})
  colnames(graph_df) <- c("Filter_Col","Val")
  graph_df <- graph_df %>% filter(!is.na(Filter_Col)) %>% filter(!is.na(Val))
  p <- ggplot() +
    geom_point(data=graph_df, aes(x=Filter_Col,y=Val)) +
    geom_line(data=means_table, aes(x=Filter_Val,y=MEAN), color="blue") +
    geom_point(data=means_table, aes(x=Filter_Val,y=MEAN), color="blue") +
    ggrepel::geom_label_repel(data = means_table,
                              mapping = aes(x = Filter_Val, y = MEAN, label = parse_percent(MEAN))) +
    geom_line(data=means_table, aes(x=Filter_Val,y=0), color="black", linetype="dashed") +
    theme_bw() +
    ylab(gsub("admDiff","Difference in Admission Rates",val)) +
    xlab(paste("Importance of: ",filter_col,sep="")) +
    scale_y_continuous(labels = scales::percent) +
    ggtitle(title_wrapper(paste("Gendered Difference in Admissions Rates by Importance of ",str_to_title(filter_col),sep=""),85)) +
    xlim(0,3)
  return(p)
}

concat_df_list <- function(ls) {
  result <- as.data.frame(matrix(ncol=0,nrow=0))
  for (x in ls) {
    if (is.data.frame(x)) {
      result <- rbind.fill(result,x)
    }
  }
  return(result)
}

vec_replace <- function(vec,original,new) {
  if (!is.na(original) & !is.na(new)) {
    for (i in 1:length(vec)) {
      if (!is.na(vec[i]) & vec[i] == original) {
        vec[i] <- new
      }
    }
    return(vec)
  }
}

prepare_flourish <- function(target_cons,df) {
  columns <- which(colnames(df) %in% c("College_Name",target_cons,"admDiff"))
  df <- df[,columns]
  df <- cbind(Consideration=target_cons,df)
  colnames(df) <- c("Consideration","College_Name","Priority","admDiff")
  df <- df %>% filter(!is.na(Priority)) %>% filter(!is.na(admDiff)) %>% arrange(Priority)
  df$Priority <- vec_replace(df$Priority,0,"Not Considered")
  df$Priority <- vec_replace(df$Priority,1,"Considered")
  df$Priority <- vec_replace(df$Priority,2,"Important")
  df$Priority <- vec_replace(df$Priority,3,"Very Important")
  df[['admDiff']] <- df[['admDiff']] * 100
  return(df)
}

apply_arbitrary_ordering <- function(df,column,ordering) {
  result <- df[which(!(df[[{{column}}]] %in% ordering)),]
  ordering <- unique(ordering)
  for (i in length(ordering):1) {
    temp <- df[which(df[[{{column}}]] == ordering[i]),]
    result <- rbind(temp,result)
  }
  return(result)
}

produce_consideration_boxplot <- function(consideration,df) {
  df <- apply_arbitrary_ordering(df,consideration,c("Required","Considered","Not Considered"))
  
  factor(df[[{{consideration}}]], labels=c(1,2,3), levels=c("Required","Considered","Not Considered"), ordered=TRUE) %>% 
    factor(levels=c(1,2,3), labels=c("Required","Considered","Not Considered"), ordered=TRUE) -> 
    df[[{{consideration}}]]
  
  colnames(df)[which(colnames(df) == consideration)] <- "Adm_Consideration"
  
  q <- ggplot(df, aes(x=admDiff,y=Adm_Consideration)) +
    geom_boxplot() +
    theme_bw() +
    scale_y_discrete(limits=rev) +
    theme_bw() +
    ylab(consideration)
  
  file_name <- paste("ipeds_plots/",consideration,".png",sep="")
  
  ggsave(file_name,q,units="px",height=1600,width=2400)
}

soft_parse_number <- function(x) {
  if (is.character(x)) {
    return(parse_number(x))
  } else {
    return(x)
  }
}

codebook2022 <- read_csv("ipeds/codebooks/ic2022.csv")
codebook2022 <- codebook2022 %>% filter(CNTLAFFI %in% c(1,3))
valid_inst <- unique(codebook2022$UNITID)

process_ipeds_file <- function(file_address) {
  
  message(file_address)
  
  adm <- read_csv(file_address)
  colnames(adm) <- str_to_upper(colnames(adm))
  
  adm_considerations <- colnames(adm)[which(grepl("ADMCON",colnames(adm),fixed=TRUE))]
  applicants_cols <- c('APPLCN','APPLCNM','APPLCNW','APPLCNAN','APPLCNUN')
  adm_cols <- c('ADMSSN','ADMSSNM','ADMSSNW','ADMSSNAN','ADMSSNUN')
  enrl_cols <- c('ENRLT','ENRLM','ENRLW','ENRLAN','ENRLUN')
  testing_cols <- c('SATNUM','SATPCT')
  
  applicants_cols <- applicants_cols[applicants_cols %in% colnames(adm)]
  adm_cols <- adm_cols[adm_cols %in% colnames(adm)]
  enrl_cols <- enrl_cols[enrl_cols %in% colnames(adm)]
  testing_cols <- testing_cols[testing_cols %in% colnames(adm)]
  
  adm <- adm %>% select(UNITID,all_of(adm_considerations),all_of(applicants_cols),all_of(adm_cols),all_of(enrl_cols),all_of(testing_cols))
  
  ipeds <- adm
  
  ipeds <- ipeds %>% filter(UNITID %in% valid_inst)
  
  for (adm_cons in adm_considerations) {
    ipeds[[{{adm_cons}}]] <- vec_replace(ipeds[[{{adm_cons}}]],1,"Required")
    ipeds[[{{adm_cons}}]] <- vec_replace(ipeds[[{{adm_cons}}]],5,"Considered")
    ipeds[[{{adm_cons}}]] <- vec_replace(ipeds[[{{adm_cons}}]],3,"Not Considered")
  }
  
  for (numeric_col in c(applicants_cols,adm_cols,testing_cols,enrl_cols)) {
    ipeds[[{{numeric_col}}]] <- soft_parse_number(ipeds[[{{numeric_col}}]])
  }
  
  if (all(c("ADMSSN","APPLCN") %in% colnames(ipeds))) {
    ipeds <- ipeds %>% filter(!is.na(ADMSSN)) %>% filter(!is.na(APPLCN))
    ipeds <- cbind(ipeds,ADM_RATE=NA)
    ipeds$ADM_RATE <- ipeds$ADMSSN / ipeds$APPLCN
  }
  
  if (all(c("ADMSSNM","APPLCNM") %in% colnames(ipeds))) {
    ipeds <- ipeds %>% filter(!is.na(ADMSSNM)) %>% filter(!is.na(APPLCNM))
    ipeds <- cbind(ipeds,admRateMale=NA)
    ipeds$admRateMale <- ipeds$ADMSSNM / ipeds$APPLCNM
  }
  
  if (all(c("ADMSSNW","APPLCNW") %in% colnames(ipeds))) {
    ipeds <- ipeds %>% filter(!is.na(ADMSSNW)) %>% filter(!is.na(APPLCNW))
    ipeds <- cbind(ipeds,admRateFemale=NA)
    ipeds$admRateFemale <- ipeds$ADMSSNW / ipeds$APPLCNW
  }

  if (all(c("admRateFemale","admRateMale") %in% colnames(ipeds))) {
    ipeds <- cbind(ipeds,admDiff=NA)
    ipeds$admDiff <- ipeds$admRateFemale - ipeds$admRateMale
  }
  
  ipeds <- cbind(Year=NA,ipeds)
  ipeds$Year <- parse_number(file_address)
  
  write_out_address <- paste("ipeds/processed_data/",gsub("ipeds/raw_data","",file_address),sep="")
  write.csv(ipeds,write_out_address,row.names=FALSE)
  
  return(ipeds)
}

sort_by_evaluator <- function(vec,numeric_evaluator) {
  sort_df <- as.data.frame(matrix(ncol=2,nrow=length(vec)))
  colnames(sort_df) <- c("Original","Numeric")
  sort_df$Original <- vec
  sort_df$Numeric <- numeric_evaluator(vec)
  sort_df <- sort_df %>% arrange(Numeric)
  return(sort_df$Original)
}

count_required <- function(vec) {
  return(count_instances(vec,"Required"))
}

perc_required <- function(vec) {
  return(perc_instances(vec,"Required"))
}

count_unique <- function(vec) {
  return(length(unique(vec)))
}

perc_ignored <- function(vec) {
  return(perc_instances(vec,"Not Considered"))
}

perc_required_table_by_cons <- function(df,cons) {
  table <- stat_table_original_colnames(df,"Year",cons,perc_required)
  return(table)
}

perc_required_chart_by_cons <- function(df,cons) {
  table <- perc_required_table_by_cons(df,cons)
  colnames(table) <- c("Year","Consideration")
  p <- ggplot(table, aes(x=Year,y=Consideration)) +
    geom_line() +
    theme_bw() +
    ylab(cons)
  return(p)
}

filter_dataframe <- function(df,col,value) {
  indices <- which(df[[{{col}}]] == value)
  df <- df[indices,]
  return(df)
}

meta_stat_table <- function(df,wrap_col,filter_col,val_col,stat_func) {
  sorts <- unique(df[[{{wrap_col}}]])
  temp_df <- filter_dataframe(df,wrap_col,sorts[1])
  result <- build_stat_table(temp_df,filter_col,val_col,stat_func)
  result <- cbind(WRAP=sorts[1],result)
  for (i in 2:length(sorts)) {
    curr_sort <- sorts[i]
    temp_df <- filter_dataframe(df,wrap_col,sorts[i])
    temp_result <- build_stat_table(temp_df,filter_col,val_col,stat_func)
    temp_result <- cbind(WRAP=curr_sort,temp_result)
    result <- rbind(result,temp_result)
  }
  return(result)
}