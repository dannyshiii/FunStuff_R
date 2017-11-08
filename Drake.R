# This project scrap data of Drake's lyrics.
# And compare it with Eminem's lyrics.
install.packages("rvest")
install.packages("RCurl")
library(rvest, RCurl)
url.drake <- "http://www.azlyrics.com/d/drake.html"
song.list.drake <- read_html(url.drake)
links.drake <- html_attr(html_nodes(html_nodes(song.list.drake, "#listAlbum"), "a"), "href")
links.drake <- links.drake[which(sapply(links.drake, substr, 0, 2) == "..")]
add.url <- function(link){
  gsub("..", "http://www.azlyrics.com", link, fixed = T)
}
full.links.drake <- unname(sapply(links.drake, add.url))
site.to.lyrics <- function(url){
  song <- read_html(url)
  # grab the raw lyrics from the source code
  lyrics.raw <- html_text(html_nodes(song, "div")[23])
  # remove the line breaks
  lyrics.intermediate <- gsub("\n", " ", lyrics.raw, fixed = T ) 
  lyrics.intermediate <- gsub("\r", " ", lyrics.intermediate, fixed = T )
  # Remove all parenthesis
  lyrics.intermediate <- gsub("(", "", lyrics.intermediate, fixed = T )
  lyrics.intermediate <- gsub(")", "", lyrics.intermediate, fixed = T )
  # remove punctuation marks
  lyrics.intermediate <- gsub("[[:punct:]]", "", lyrics.intermediate) 
  # Remove verse/chorus tags
  lyrics.intermediate <- gsub("\\[[^\\]]*\\]", "", lyrics.intermediate, perl=TRUE) 
  # split into single words based on spaces
  word.tokens <- unlist(strsplit(lyrics.intermediate, " ")) 
  # remove empty space tokens
  word.tokens <- word.tokens[word.tokens != ""] 
  # make all lower case
  word.tokens <- tolower(word.tokens)
  return(word.tokens)
}

set.seed(24)
subsample.drake <- sample(full.links.drake, 50)
complete.list.drake <- c()
for(url in subsample.drake) {
  complete.list.drake <- c(complete.list.drake, site.to.lyrics(url))
  Sys.sleep(5)
}
word.counts.drake <- table(complete.list.drake)
head(sort(word.counts.drake, decreasing = T))
unique(complete.list.drake)
"honey" %in% complete.list.drake #or subsample.drake ?
honey.drake <- rep(0, length(subsample.drake))
for(i in 1:length(subsample.drake)){
  Sys.sleep(5)
  song.lyrics <- site.to.lyrics(subsample.drake[i])
  #honey.drake[i] <- "honey" %in% song.lyrics
  honey.drake[i] <- length(unique(song.lyrics))
}


url.eminem <- "http://www.azlyrics.com/e/eminem.html" # Pull the site's source code into R
song.list.eminem <- read_html(url.eminem)
links.eminem <- html_attr(html_nodes(html_nodes(song.list.eminem, "#listAlbum"), "a"), "href")
links.eminem <- links.eminem[which(sapply(links.eminem, substr, 0, 2) == "..")]
full.links.eminem <- unname(sapply(links.eminem, add.url))
set.seed(42)
subsample.eminem <- sample(full.links.eminem, 200)
subsample.eminem
summary.statistics.eminem <- rep(0, length(subsample.eminem))
for(i in 1:length(subsample.eminem)){
  Sys.sleep(5)
  song.lyrics <- site.to.lyrics(subsample.eminem[i])
  summary.statistics.eminem[i] <- length(unique(song.lyrics))
}