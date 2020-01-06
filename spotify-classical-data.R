library(spotifyr)
library(tidyverse)

# Minor alterations of code from https://www.kaylinpavlik.com/classifying-songs-genres/. She did 99% of the work.
# I just want to see what some of the classical I listen to looks like so I need to get some classical albums.

# generates the access token necessary to interact with the spotify api
access_token <- get_spotify_access_token(client_id = Sys.getenv("SPOTIFY_CLIENT_ID"), 
                                         client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"))

# composers 
composers <- c("Johann Sebastian Bach", "Wolfgang Amadeus Mozart", 
               "Ludwig van Beethoven", "Gustav Mahler", 
               "Sergei Rachmaninoff", "Frédéric Chopin")

# get 25 album ids for each composer
# tibble with name, id, composer
album_ids <- pmap_dfr(list(q = composers, type = 'album', limit = 25),  search_spotify)%>%
  select(name, id)%>%
  rename(album_id = id,
         album_name = name)%>%
  mutate(composer = rep(composers, each = 25))
  
# use album ids to get individual track ids on each album; bind into one df
album_songs <- tibble()

for(i in seq_along(album_ids$album_id)){
  raw_track_info <- get_album_tracks(album_ids$album_id[i])
  
  output_track_info <- raw_track_info%>%
    select(id, name)%>%
    mutate(album_id = album_ids$album_id[i],
           album_name = album_ids$album_name[i],
           composer = album_ids$composer[i])%>%
    as_tibble()
  
  album_songs <- rbind(album_songs, output_track_info)
}
  

#spotifyr get_track_audio_features will accept a max of 100 ids to get track features on. Need to loop through
# to get all ~1600 tracks

get_track_audio_features_all <- function(track_ids) {
  # create intervals of width 100 to loop through. +1 is needed to make sure you grab the last row of the data
  intervals <- c(seq(from = 0, to = length(track_ids), by = 100), length(track_ids) + 1)
  
  track_features <- tibble()
  for(i in seq_along(intervals)){

    start <- intervals[i]
    end <- intervals[i + 1] - 1
    if(is.na(end)) break
    
    # use spotifyr to access spotify api and pull track features
    raw_features <- get_track_audio_features(ids = track_ids[start:end])
    track_features <- rbind(track_features, raw_features)
    
  }
  
  return(track_features)
  
}

# get all track features
album_features <- get_track_audio_features_all(track_ids = album_songs$id)

# join the track features tibble to the album_songs tibble
full_album_songs <- album_songs%>%
  left_join(track_features, by = 'id')%>%
  select(-type, -uri, -track_href, -analysis_url )%>%
  # check to make sure the same track isn't on some albums
  distinct(id, .keep_all = TRUE)
  
# export to csv file
write_csv(full_album_songs, 'classical.songs.csv')
