# Script takes a goodreads users id # and profile name and scrapes their 'read' bookshelf
library(stringr)
library(rvest)
library(tidyverse)

#### This goodreads scraping functionality will NOT work if the user info is set to private. Even if
#### you are friends with that user, you will not be able to scrape their book data.


number_of_books <- function(user = NULL){
  # Gets the number of books on the 'read' shelf of the user.
  
  # Parameters: user -- the goodreads id # and profile name of a non-private user.
  # Returns: a single numeric value.
  if(is.null(user)){
    stop("No user supplied. User must be of the form 'goodreadsid#-goodreadsprofilename'")
  }
  
  # concatenate the base goodreads api url with the specific user 
  # also make sure specify pagination amount and start at page 1
  baseUrl <- "http://www.goodreads.com/review/list/"
  user_url <- str_c(baseUrl, user, "?page=1&per_page=30&shelf=read")
  
  # This determines how many books user has read. Used to calculate how many pages 
  # need to be scraped.
  get_number_books <- read_html(user_url)%>%
    html_nodes(".selectedShelf")%>%
    html_text(trim = TRUE)
  
  # scraping returns a character string like "(101)". This removes the parentheses and converts to a numeric value
  return(as.numeric(gsub("\\(([^()]+)\\)", "\\1", str_extract_all(get_number_books, "\\(([^()]+)\\)")[[1]])))
  
} 



trim_title <- function(title){
  #helper function to trim off stuff in parentheses in scraped titles
  return(str_trim(str_remove(title, " \\([^()]+\\)")))
}

trim_author <- function(author){
  # remove whitespace from start and end of author
  return(str_trim(author))
}

first_last_name <- function(name){
  # converts 'last, first' name format to 'first last'
  split <- str_split_fixed(name, ", ", 2)
  return(paste(split[,2], split[,1], sep = " "))
}

trim_date <- function(date){
  # trims date and converts from 'Jan 1, 2019' to '2019-01-01'
  date <- str_trim(str_remove(date, "date added "))
  lubridate::mdy(date)
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
    map_chr(trim_author)
    
  
  # first last form of author name
  firstlast <- author%>%
    map_chr(first_last_name)
  
  # extract the date book was added
  date <- html %>%
    html_nodes("#booksBody .date_added")%>%
    html_text(trim = TRUE)
  # when I used map to apply the trim_date function, all my dates were returned as 5 digit numbers. They would print out as the correct Year-Month-Day
  # format, but would display 12345. Something to do with how purrr and map handle types, but I'm just fixing it by breaking into 2 steps.
  date <- trim_date(date)

  
  return(tibble(
    title = title,
    author = author,
    firstlast = stringi::stri_trans_general(firstlast,
                                            id = "Latin-ASCII"),
    date = date 
  ))
}


scrape_all <- function(user = NULL){
  #paramenter: user - goodreads id # and profile name
  #returns: tibble of user's read books containing:
            # title
            # author
            # firstlast
            # date added
  
  if(is.null(user)){
    stop("No user supplied. User must be of the form 'goodreadsid#-goodreadsprofilename'")
  }
  
  # determine how many pages function will have to scrape
  num_pages <- ceiling(number_of_books(user)/30)
  
  # user purrr to map number of pages and user info to the getBooks function. Returns a tibble df
  c(1:num_pages)%>%
    map2_dfr(user, getBooks)
  
}





