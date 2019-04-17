# this script takes a goodreads users id # and profile name and scrapes their 'read' bookshelf
library(stringr)
library(rvest)
library(tidyverse)

a <- read_html("https://www.goodreads.com/review/list/4187841-ryan-houlette?shelf=read")%>%
  html_nodes(".selectedShelf")%>%
  html_text(trim = TRUE)%>%
  gsub("\\(([^()]+)\\)", "\\1", str_extract_all(., "\\(([^()]+)\\)")[[1]])
  str_ex

number_of_books <- function(user = NULL){
  if(is.null(user)){
    stop("No user supplied. User must be of the form 'goodreadsid#-goodreadsprofilename'")
  }
  
  baseUrl <- "http://www.goodreads.com/review/list/"
  user_url <- str_c(baseUrl, user, "?page=1&per_page=30&shelf=read")
  
  get_number_books <- read_html(user_url)%>%
    html_nodes(".selectedShelf")%>%
    html_text(trim = TRUE)
  
  return(as.numeric(gsub("\\(([^()]+)\\)", "\\1", str_extract_all(get_number_books, "\\(([^()]+)\\)")[[1]])))
  
}  
#helper function to trim off stuff in paraentheses in scraped titles
trim_title <- function(title){
  return(str_trim(str_remove(title, " \\([^()]+\\)")))
}

first_last_name <- function(name){
  # converts 'last, first' name format to 'first last'
  split <- str_split_fixed(name, ", ", 2)
  return(paste(split[,2], split[,1], sep = " "))
}

# function to get books; This is close to an exact copy of Mara Avericks code from her scraping of goodreads
# https://maraaverick.rbind.io/2017/10/goodreads-part-2/#fn1
getBooks <- function(i, user = NULL) {
  cat(i, "\n")
  
  if(is.null(user)){
    stop("No user supplied. User must be of the form 'goodreadsid#-goodreadsprofilename'")
  }
  # concatenate the url for the users read books shelf
  baseUrl <- "http://www.goodreads.com/review/list/"
  user_url <- str_c(baseUrl, user, "?page=", i, "&per_page=30", "&shelf=read")
  
  # parse user_url html
  html <- read_html(user_url)
  
  # extract the title of read books; then remove any title information in parentheses(generally a series title)
  title <- html %>%
    html_nodes(".title a") %>%
    html_text(trim = TRUE) %>%
    map_chr(trim_title)
  
  # extract the author of read books
  author <- html %>%
    html_nodes(".author a") %>%
    html_text(trim = TRUE)%>%
    map_chr(first_last_name)

  
  return(tibble(
    title = title,
    author = author
  ))
}

test_getBooks <- function(testuser){
  getBooks(1, user = testuser)
}

scrape_all_title_authors <- function(user = NULL){
  if(is.null(user)){
    stop("No user supplied. User must be of the form 'goodreadsid#-goodreadsprofilename'")
  }
  
  num_pages <- ceiling(number_of_books(user)/30)
  
  c(1:num_pages)%>%
    map2_dfr(user, getBooks)
  
}
goodreads <- c(1:8)%>%
  map_dfr2("getBooks)




