library(tidyverse)
library(magrittr) # better handling of pipes
library(purrr) # to work with lists and map functions
library(glue) # to paste strings
library(stringr) # to hand strings
library(rvest)
library(dplyr)# rvest makes scraping easier

#FUNCTION 
#got a good structure, let's scrap all the movies now

url_base <- "https://www.imdb.com/search/title/?title_type=feature&release_date=2015-01-01,2020-10-01&count=250"

map_df(1:10, function(i) {
  
  #progress indicator
  cat(".")
  
  pg <- read_html(url_base)
  
  #fetch all the movie names 
  movie_all <- pg %>%
    html_nodes("h3.lister-item-header a")    
  movie_name = html_text(movie_all)
  
  #fetch_all_years
  movie_year_all <- pg %>%
    html_nodes("h3.lister-item-header span.lister-item-year.text-muted.unbold")
  
  movie_years <- as.numeric(gsub(")","",
                                 gsub("- ","",             
                                      gsub("\\(","",gsub("-","",
                                                         html_text(movie_year_all)
                                      )))))
  
  #fetch all the ratings
  movie_ratings_all <-  pg %>%
    html_nodes("div.ratings-bar div.inline-block.ratings-imdb-rating strong") %>% 
    html_text() %>% 
    as.double()
  
  #getting all the links of the movies from the html
  movie_link = lapply(html_attrs(movie_all),`[[`,'href')
  
  movie_gross <- pg %>%
    html_nodes("p.sort-num_votes-visible span:nth-child(5)")  %>% 
    html_text()
  
  movie_gross_new <- gsub("M","",   
                          movie_gross)
  
  data.frame(Movie_name = movie_name,
             Years = movie_years
             #Ratings = movie_ratings_all
             #Gross = movie_gross_new
             #Link = movie_link
  ) %>% 
    mutate(Link = movie_link)
  
}) -> imdb_feature_films



movie_urls <- glue("https://www.imdb.com{imdb_feature_films$Link[1]}")

#Function to get the Genres of the movie
get_genre <- function(link){
  
  movie_genre <- glue("https://www.imdb.com{link}") %>%
    read_html() %>%
    html_nodes(".subtext :nth-child(4)") %>%
    html_text()
  
  return(movie_genre)
}

#Ratings for the movies
get_rating  <- function(link){
  
  movie_ratings <- glue("https://www.imdb.com{link}") %>%
    read_html() %>%
    html_nodes("div.ratingValue strong") %>%
    html_text()
  return(movie_ratings)
}

#Director of the movie
get_director  <- function(link){
  movie_director <- glue("https://www.imdb.com{link}") %>%
    read_html() %>%
    html_nodes(".credit_summary_item a") %>%
    html_text()
  
  return(movie_director[1])
}

#number of users who rated for the movie
get_number_of_users_rated <- function(link){
  
  users_rated_numbers <- glue("https://www.imdb.com{link}") %>%
    read_html() %>%
    html_nodes(".imdbRating a") %>%
    html_text()
  
  users_rated_numbers <- as.numeric(gsub(",","",users_rated_numbers))
  
  return(users_rated_numbers)
}

#Budget for movies
get_budget <- function(link){
  
  budget_movies <- glue("https://www.imdb.com{link}") %>%
    read_html() %>%
    html_nodes("div.article .txt-block:nth-child(14)") %>%
    html_text()
  
  budget_movies <- as.numeric(gsub(",","",substr(budget_movies,14,22)))
  return(budget_movies)
  
}

#Runtime for movies
get_runtime <- function(link){
  
  movie_runtime <- glue("https://www.imdb.com{link}") %>%
    read_html() %>%
    html_nodes("div.article .txt-block:nth-child(23)") %>%
    html_text()
  
  movie_runtime <- as.numeric(substr(movie_runtime,25,27))
  return(movie_runtime)
  
}

imdb_new_all <- imdb_feature_films %>%
  slice(1:10)  %>%
  mutate(Genre = map(Link, get_genre),
         Ratings = map(Link,get_rating),
         Director = map(Link, get_director),
         Users_involved = map(Link, get_number_of_users_rated),
         Budget = map(Link, get_budget),
         Runtime = map(Link, get_runtime)
  )
imdb_new_all

movie_runtime <- glue("https://www.imdb.com{imdb_feature_films$Link[3]}") %>%
  read_html() %>%
  html_nodes("div.article .txt-block:nth-child(23)") %>%
  html_text()

movie_runtime <- as.numeric(substr(movie_runtime,25,27))
movie_runtime