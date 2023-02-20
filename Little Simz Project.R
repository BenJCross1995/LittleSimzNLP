#------Little Simz Project------#

#------LOAD LIBRARIES------# ####

suppressWarnings(
  suppressPackageStartupMessages({
    library(gridExtra)  # Facet Wrap
    library(ggplot2)
    library(tidyr)      # Seperate columns
    library(stringr)
    library(rvest)      # Album Awards
    library(dplyr)
    library(lubridate)
    library(tidytext)   # unnest_tokens
    library(stopwords)  # Get stopwords
  })
)

options(dplyr.summarise.inform = FALSE)
#------LOCATIONS------# ####

# Below we have the locations for the lyrics, albums and songs
lyric.location <- "C:/Users/benjc/Documents/Little Simz Project/Lyrics/"
album.info.location <- "C:/Users/benjc/Documents/Little Simz Project/Info/Album Info.csv"
song.info.location <- "C:/Users/benjc/Documents/Little Simz Project/Info/Song Info.csv"

#------FUNCTIONS------# ####

read.lyric.files <- function(lyric.loc, album){
  #' A function which takes in a filepath and an album folder which stores
  #' the lyric .txt files as functions and returns a dataframe containing
  #' the album name, song title, and a lyric column with one row of lyrics
  #' per row in the .txt file.
  #' 
  #' @param lyric.loc - The filepath to the main lyrics folder
  #' @param album - The name of the album.
  #' 
  #' @return - A dataframe containing the album, song, and lyrics.
  #' 

  # Create a blank dataframe
  lyrics.df <- data.frame()
  
  # Create a string containing the lyric location and album name
  loc <- paste0(lyric.loc, album, "/")
  
  # Search this filepath for the songs in the album
  songs <- list.files(loc)
  
  # Now we read in the files
  for(song in songs){
    # When reading the data below, we want no header so that the first line
    # isn't used as a column header, create own column name, choose the 
    # encoding as UTF-8 to change characters to English then there are some
    # quotes in the lyrics so set quotation marks to "".
    lyrics <- read.delim(paste0(loc, song),
                         header = FALSE,
                         col.names = "Lyrics",
                         encoding = 'UTF-8',
                         quote = "")
    # Bind the album and song data together.
    lyrics <- cbind("Album" = album,
                    "Song" = song,
                    "Lyrics" = lyrics)
    
    # Now remove the .txt from the song title and filter out 
    # full unknown rows.
    lyrics <- as.data.frame(lyrics) %>%
      mutate(Song = strsplit(Song,
                             split = '.txt',
                             fixed = TRUE)) %>%
      filter(!(Lyrics == "[?]"))
    
    # Bind the new rows to the old dataframe.
    lyrics.df <- rbind(lyrics.df, lyrics)
  }
  # Return the new dataframe
  return(lyrics.df)
}

#------LOAD SUPPORTING DATA------# ####

# Load the album information
album.info <- read.csv(album.info.location)
album.info$AlbumReleaseDate <- as.Date(album.info$AlbumReleaseDate)

# Load the song information
song.info <- read.csv(song.info.location)
song.info$SongReleaseDate <- as.Date(song.info$SongReleaseDate)

# Combine the song and album information
song.info <- song.info %>%
  left_join(album.info, by = 'AlbumID')

#------LOAD LYRICS------# ####

# Get a list of albums from the lyrics folder
albums <- list.files(lyric.location)

# Create an empty dataframe
lyrics <- data.frame()

# Loop across all albums in the folder and cmplete the
# read.lyric.files function for each album. Bind the results
# of each album to the dataframe.
for(album in albums){
  lyric <- read.lyric.files(lyric.location, album)
  lyrics <- rbind(lyrics, lyric)
}

# Remove the lyric dataframe from the environment
rm(lyric)

#------GET ALBUM AWARDS------# ####

# Wiki page
wiki.page <- "https://en.wikipedia.org/wiki/Little_Simz#Awards_and_nominations"

# Read in the table, you get the xpath by right clicking element in inspection and
# clicking copy XPath
awards <- wiki.page %>%
  read_html() %>%
  html_node(xpath = "//*[@id='mw-content-text']/div[1]/table[10]") %>%
  html_table(fill = T)


awards <- awards %>%
  filter(`Nominated work` %in% albums) %>%
  select(- Ref.) %>%
  rename("Album" = "Nominated work") %>%
  mutate(Album = factor(Album,
                           levels = c("A Curious Tale Of Trials + Persons",
                                      "Stillness in Wonderland",
                                      "Grey Area",
                                      "Sometimes I Might Be Introvert",
                                      "No Thank You")))



awards %>%
  ggplot(aes(x = Album, fill = Result)) +
  geom_bar() +
  ggtitle("Awards by Album and Result") +
  labs(y = "Total Awards") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 0.5,
                               vjust = 0.5)
  )
#------MANIPULATE LYRICS------# ####

lyrics <- lyrics %>%
  mutate(Info = ifelse(substr(Lyrics, 1, 1) == "[" &
                         substr(Lyrics, 1, 2) != "[?"  ,
                       substring(Lyrics, 2, nchar(Lyrics) - 1),
                       NA),
         SongPart = ifelse(str_detect(Info, ":") == TRUE,
                           sub("\\:.*","",Info),
                           Info),
         Artist = ifelse(str_detect(Info, ":") == TRUE,
                         sub(".*\\:","",Info),
                         ifelse(
                           str_detect(Info, ":") == FALSE &
                             !(is.na(Info)),
                           "Little Simz",
                           NA)),
         Album = factor(Album,
                        levels = c("A Curious Tale Of Trials + Persons",
                                   "Stillness in Wonderland",
                                   "Grey Area",
                                   "Sometimes I Might Be Introvert",
                                   "No Thank You"))) %>%
  fill(SongPart, Artist) %>%
  filter(substr(Lyrics, 1, 1) != "[" &
             substr(Lyrics, 1, 2) != "[?") %>%
  select(-Info)
lyrics

#------WORDS PER SECOND------# ####

words.per.second <- lyrics %>%
  unnest_tokens(word, Lyrics) %>%
  group_by(Album, Song) %>%
  summarise("Words" = n()) %>%
  unnest(Song) %>%
  left_join(song.info[,c('AlbumTitle', 'Song', 'SongLength')],
            by = c('Album' = 'AlbumTitle', 'Song')) %>%
  mutate(WordsPerSecond = Words / SongLength,
         Album = factor(Album,
                        levels = c("A Curious Tale Of Trials + Persons",
                                   "Stillness in Wonderland",
                                   "Grey Area",
                                   "Sometimes I Might Be Introvert",
                                   "No Thank You"))) %>%
  arrange(desc(WordsPerSecond))

#------WORDS PER SONG------# ####

words.per.second %>%
  ggplot(aes(x = Words, y = reorder(Song, Words),
             fill = Album)) +
  geom_col() +
  facet_wrap(Album~., dir = "v", scales = "free") +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

#------WORDS PER SECOND GRAPH------# ####
words.per.second %>%
  head(25) %>%
  ggplot(aes(x = reorder(Song, -WordsPerSecond), y = WordsPerSecond, fill = Album)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = round(WordsPerSecond, 2)),
            position = position_stack(vjust = 0.75),
            angle = 45) +
  ggtitle("Top 25 Songs by Words per Second, Coloured by Album")+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1,
                               vjust = 0.5),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)) +
  coord_polar(theta = "x")


#------LITTLE SIMZ LYRICS------# ####

lyrics %>%
  unnest_tokens(word, Lyrics) %>%
  filter(!(word %in% stopwords(source = "snowball"))) %>%
  filter(Artist =="Little Simz") %>%
  count(Album, word) %>%
  group_by(Album) %>%
  arrange(desc(n)) %>%
  as.data.frame()



