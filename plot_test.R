library(sentimentr)
library(dplyr)
library(ggplot2)

midnight01 <- c("The liquid scenery cast an indigo complexion.
To drift so carefree, moonlight reflects in the pool of darker times.
And to sink to darker measures.",
"Submariner seeks pride from depths hard to find, forever wading.
A pool of strange things.
Its guilt, is craving, this comfort, it’s quilt.
And outside I think it’s raining.")
midnight02 <- as_tibble(data.frame(midnight01, c(1,2))) %>% rename(
  lyrics = midnight01, verse = c.1..2.)

sentences <- get_sentences(midnight02) 
d <- sentiment_by(sentences$lyrics, by = sentences$verse)
e <- sentiment(sentences$lyrics, by = sentences$verse)
plot(d) + labs(x = "Verse")
ggplot(e, aes(element_id, y = sentiment)) + geom_line()


#integrating genius lyrics
library(genius)
m <- genius::genius_lyrics(artist = "King Krule", song = "Midnight 01 (Deep Sea Diver)")
genius::possible_lyrics(artist = "King Krule", song = "Midnight 01")
