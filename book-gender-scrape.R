# Script takes a tibble df of users read books and attempts to gender tag the authors using A) goodreads api
# or if A fails B) scraping viaf.org pages for gender.
library(httr)
library(xml2)
library(rvest)
library(RSelenium)
library(purrr)
library(dplyr)
library(rgoodreads)

source("readbooks_scrape.R")
# Scraping Viaf website requires RSelenium, so 1) make sure docker is up and running
# 2) make sure R and the server are connected
# can't use remoteServerAddr = "localhost" for windows. Needs to be the ip of the VM 
# that is running docker

# Run this in docker quickstart terminal
# docker run -d -p 4445:4444 selenium/standalone-chrome
remDr <- remoteDriver(
  remoteServerAddr = "192.168.99.100",
  port = 4445L,
  browserName = "chrome"
)

# don't think I want to instantiate the browser every time, so I do it once here
remDr$open()


# First I need to get the wi numbers for each author/title. I can do this with basic tidyverse
# scraping functionality
get_wi_number <- function(author = NULL, title = NULL, .verbose = TRUE){
  # wi #'s are found in two different places in the classify.oclc.org website: 
  #         - Most often in the 'work' element under the 'wi' attribute 
  #         - Occasionally in the xml text of the 'work' element (generally in non-fiction books it seems like)
  # parameters: author - character
  #             title - character
  # returns: wi value of author as a character
  
  # Make sure an author and title are given
  if(is.null(author)){
    if(.verbose) message("Author not supplied")
    return(NA)
  }
  
  if(is.null(title)){
    if(.verbose) message("Title not supplied")
    return(NA)
  }
  
  # Generate encoded url
  base_url <- "http://classify.oclc.org/classify2/Classify?"
  
  encoded_url <- paste0(base_url, "author=", URLencode(author), "&title=", URLencode(title))
  
  # wrap the GET httr function with purrr::safely which gives it exception handling code
  sGET <- purrr::safely(GET)
  res <- sGET(encoded_url)
  
  # If the classify api doesn't find a result for the author+title, it returns an xml page with response code 102, otherwise it will be 2 or 4(I think).
  # I want to grab this response code to check if my call returned a scrapable xml page
  response_code <- read_html(res$result)%>%
    html_nodes("response")%>%
    map(html_attrs)%>%
    simplify()%>%
    first()
  
  # checks 1) if the httr call returned NULL
  #        2) if the httr call came back OK (wasn't a 404 error code or something like that)
  #        3) if the xml has a 102 response code(meaning it didn't find the book and is basically empty xml)
  if(is.null(res$result) | (status_code(res$result) !=200) | response_code == 102){
    # if any of these conditions return TRUE, warn that somethings wrong with the url and return an NA value
    if(.verbose) message("URL invalid or results returned NULL")
    return(NA)
  } else{
    
    # read the valid xml page
    requested_url <- content(res$result, as = 'text')
    raw_html <- read_html(requested_url)
    

    # This handles scraping wi # from the two locations it can be found
    # Get a list of the work attributes found in the html
    work_attributes <- html_nodes(raw_html, "work")%>%
      map(html_attrs)
    
    # check to see if the xml page found is for the correct author. If it isn't, return NA
    wi_author <- work_attributes%>%
      map_df(~as.list(.))%>%
      select(author)%>%
      pull(author)%>%
      first()
    
    if(str_detect(tolower(wi_author), str_extract(tolower(author), pattern = "[a-z]*$")) == FALSE){
      if(.verbose) message("Input Author/Title does not match Author/Title of WI match")
      return(NA)
    }
    
    # if wi is an attribute, I get the wi # from there and return it as a character vector
    if("wi" %in% attributes(work_attributes[[1]])$names){
      # scrape the WI number from the title, author input. 
      works_tdf <- work_attributes%>%
        # create a tibble out of the list you get
        map_df(~as.list(.))%>%
        # only need the WI column
        select(wi)%>%
        pull(wi)
      # return a single wi number as a character
      works_tdf[1]
      
    } else {
      # if wi is not an attribute for the xml file, then wi is listed as text in the work element
      html_nodes(raw_html, "work")%>%
        map(xml_text)%>%
        # extract the character vector containing the wi number
        simplify()%>%
        first()
    }
  }
  
}


# with the wi numbers, I can scrap the VIAF numbers I'll use to tag gender
get_viaf_number <- function(author = NULL, title = NULL, .verbose = TRUE){
  # Make sure an author and title are given
  if(is.null(author)){
    if(.verbose) message("Author not supplied")
    return(NA)
  }
  
  if(is.null(title)){
    if(.verbose) message("Title not supplied")
    return(NA)
  }
  
  # Need to check if wi # is NA. If so, return NA value
  wi_number <- get_wi_number(author, title)
  if(is.na(wi_number)){
    if(.verbose) message("NA WI number. Book probably not found.")
    return(NA)
  }
  
  # Also need to make sure I'm getting the right author--translators and multiple authors can be picked by accident if I'm not careful.
  base_url <- "http://classify.oclc.org/classify2/ClassifyDemo?wi="
  
  # scrape the viaf table: May contain multiple authors/translators. Need to check and get the viaf # associated with the actual author
  viaf_table <- read_html(paste0(base_url, wi_number))%>%
    html_nodes("#display-V-tbl a")%>%
    html_text()%>%
    matrix(., ncol = 2, 
           byrow = TRUE,
           dimnames = list(NULL, c("name", "viaf")))%>%
    as_tibble()
  
  # matching author based on last name. Extract last name here--author is assumed to be in first middle last format  
  name_split <- str_split(author, " ")[[1]]
  first_name <- name_split[1]
  last_name <- name_split[length(name_split)]
  
  # Filter the scraped author viaf table. If they table has multiple rows, check first and last name
  # If the table is a single row, just check the last name
  result <- if(nrow(viaf_table) > 1){
                viaf_table%>%
                  filter(str_detect(tolower(name), tolower(first_name)),
                         str_detect(tolower(name), tolower(last_name)))%>%
                  pull(viaf)
              } else{
                viaf_table %>%
                  filter(str_detect(tolower(name), tolower(last_name)))%>%
                  pull(viaf)
              }
  if(is_empty(result)){
    return(NA)
  } else{
    return(result)
  }

}

# function uses get_wi_number and get_viaf_number to scrape gender from the viaf website.

#!!!!!!! this returns an empty character vector if the 'about me' section of the viaf page is empty or it otherwise 
# can't access it. This needs to be fixed--map_chr doesn't like the empty character vector and throws an error
get_gender_from_viaf <- function(author = NULL, title = NULL, .verbose = FALSE){
  # This is limited to binary genders at the moment. The goodreads api is also hit or miss on correctly tagging trans/nonbinary gender.
  # I am not sure how to fix this. If goodreads misses it, then the oclassify method is probably missing it too.
  
  #Make sure an author and title are given
  if(is.null(author)|is.null(title)){
    if(.verbose) message("Author/Title not supplied")
    return(NA)
  }
  
  # try not to spam the scraping websites too hard
  Sys.sleep(sample(seq(0, 1, .25), 1))
  
  # get the viaf number
  viaf <- get_viaf_number(author, title)
  # Handle NA viaf numbers. Right now just return NA for gender and moves on.
  if(is.na(viaf)| is_empty(viaf) | is.null(viaf)){
    if(.verbose) message("Missing viaf value. Returning NA gender.")
    return(NA)
  }
  
  base_url <- "http://viaf.org/viaf/"
  # navigate to the correct url
  remDr$navigate(paste0(base_url, viaf))
  # scrape the gender for that author
  gender <- read_html(remDr$getPageSource()[[1]])%>%
    html_nodes(".langHi0+ .langHi0")%>%
    html_text()
  
  gender
  
}

scrape_genders <- function(df){
  # scraping is slow--make sure I'm not doing one author multiple times. This is also nice to the websites.
  distinct_authors <- df %>%
    distinct(author, .keep_all = TRUE)
  
  print("Scraping genders available from Goodreads API")
  print(Sys.time())
  results <- map_df(distinct_authors$firstlast, author_by_name)%>%
    select(name, gender)%>%
    # white space is present after scraping that causes issues with later joins. Clean that up here
    mutate(name = str_squish(str_trim(name)))%>%
    mutate(gender = ifelse(gender == "", NA, tolower(gender))) #%>%
  
  distinct_authors$gender <- results$gender
  
  second_pass <- distinct_authors%>%
    filter(is.na(gender))
  # scrape those genders not found on goodreads api from the oclassify method
  print("Scraping genders not found through Goodreads API")
  print(Sys.time())
  second_pass$gender <- tolower(map2_chr(second_pass$firstlast, second_pass$title, get_gender_from_viaf))
  print(Sys.time())

  final <- distinct_authors%>%
    filter(!is.na(gender))%>%
    bind_rows(second_pass)%>%
    select(author, firstlast, gender)%>%
    right_join(df, by = c("author", "firstlast"))

  final
  
}



