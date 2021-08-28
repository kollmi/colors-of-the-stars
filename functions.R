library(readtext)
library(tidyverse)
library(tidytext)
library(tokenizers)
library(sentimentr)
library(wordcloud)

freq_story_words <- function(story, name) {
  text <- readtext::readtext(story)$text
  
  `%!in%` = Negate("%in%")
  
  clean_tokens <- text %>%
    tokenize_ngrams(n = 1) %>%
    unlist() %>%
    tolower()
  freq_words <- clean_tokens %>%
    table() %>%
    data.frame() %>%
    rename(word = ".") %>%
    filter(word %!in% stop_words$word) %>%
    arrange(-Freq) %>%
    mutate(Story = name)
  freq_words
}

sentiment_df <- function(story, name){
  text <- readtext::readtext(story)$text
  sentences <- text %>% 
    tokenize_sentences() %>% 
    unlist() 
  
  sentence_df <- data.frame(sentence = 1:length(sentences), text = sentences) %>% 
    inner_join(sentiment(sentences), by = c("sentence" = "element_id")) %>% 
    arrange(-sentiment) %>% 
    select(-sentence_id) %>% 
    mutate(Story = name)
  
  sentence_df
}

sentiment_trends <- function(sentiment_df, name){
  ggplot(sentiment_df, aes(x = sentence, y = sentiment)) + geom_line() + theme_minimal() +
    stat_smooth(method = 'nls', formula = y ~ a * exp(b * x),aes(color = 'exponential'), se = FALSE, method.args = list(start = list(a = 1, b = 1))) +
    stat_smooth(method = 'nls', formula = y ~ a * log(x) + b, aes(color = 'logarithmic'), se = FALSE, method.args = list(start = list(a = 1, b = 1))) +
    stat_smooth(method = 'lm', aes(color = 'linear'), se = FALSE) +
    stat_smooth(method = 'lm',formula = y ~ poly(x,2), aes(color = 'polynomial'), se = FALSE) +
    scale_color_brewer(name = "Trendline", palette = "Set1") +
    labs(title = name) +
    ggeasy::easy_center_title()
}

sentiment_smooth <- function(sentiment_df, name){
  ggplot(sentiment_df, aes(x = sentence, y = sentiment)) + geom_smooth() + theme_minimal() +
    labs(title = name) +
    ggeasy::easy_center_title()
}


#outpost
outpost1 = freq_story_words("stories2/the outpost.docx", "The Outpost")
outpost2 = sentiment_df("stories2/the outpost.docx", "The Outpost")

sentiment_trends(outpost2, "The Outpost")
sentiment_smooth(outpost2, "The Outpost")
wordcloud(outpost1$word,outpost1$Freq, min.freq = 10, scale=c(4.25,0.25), random.order = FALSE)

#amazing grace
amazing1 = freq_story_words("stories2/amazing grace.docx", "Amazing Grace")
amazing2 = sentiment_df("stories2/amazing grace.docx", "Amazing Grace")

wordcloud(amazing1$word,amazing1$Freq, min.freq = 5, scale=c(3.5,0.25), random.order = FALSE)
sentiment_trends(amazing2, "Amazing Grace")
sentiment_smooth(amazing2, "Amazing Grace")

#quiet gods
quiet1 = freq_story_words("stories2/The Quiet Gods.docx", "The Quiet Gods")
quiet2 = sentiment_df("stories2/The Quiet Gods.docx", "The Quiet Gods")

wordcloud(quiet1$word,quiet1$Freq, min.freq = 4, scale=c(4,0.25), random.order = FALSE)
sentiment_trends(quiet2, "The Quiet Gods")
sentiment_smooth(quiet2, "The Quiet Gods")

#candle in the dark
candle1 = freq_story_words("stories2/candle in the dark.docx", "Candle in the Dark")
candle2 = sentiment_df("stories2/candle in the dark.docx", "Candle in the Dark")

wordcloud(candle1$word,candle1$Freq, min.freq = 4, scale=c(3,0.25), random.order = FALSE)
sentiment_trends(candle2, "Candle in the Dark")
sentiment_smooth(candle2, "Candle in the Dark")

#synthesis
synthesis1 = freq_story_words("stories2/synthesis.docx", "Synthesis")
synthesis2 = sentiment_df("stories2/synthesis.docx", "Synthesis")

wordcloud(synthesis1$word,synthesis1$Freq, min.freq = 3, scale=c(2,0.25), random.order = FALSE)
sentiment_trends(synthesis2, "Synthesis")
sentiment_smooth(synthesis2, "synthesis")
#also add most positive/negative/neutral sentence
synthesis2 %>% select(text) %>% 
#terrarium
terrarium1 = freq_story_words("stories2/terrarium.docx", "Terrarium")
terrarium2 = sentiment_df("stories2/terrarium.docx", "Terrarium")

wordcloud(terrarium1$word,terrarium1$Freq, min.freq = 6, scale=c(3.25,0.25), random.order = FALSE)
sentiment_trends(terrarium2, "Terrarium")
sentiment_smooth(terrarium2, "Terrarium")

#facet wrap
all_stories <- bind_rows(outpost2, amazing2, quiet2, candle2, synthesis2, terrarium2)
ggplot(all_stories, aes(x = sentence, y = sentiment)) + geom_smooth() + theme_minimal() +
  labs(title = "Story Sentiment Distribution") +
  ggeasy::easy_center_title() +
  facet_wrap(~Story)

ggplot(all_stories, aes(x = sentence, y = sentiment)) + geom_line() + theme_minimal() +
  labs(title = "Story Sentiment Distribution") +
  ggeasy::easy_center_title() +
  facet_wrap(~Story)

ggplot(all_stories, aes(x = sentiment)) + geom_boxplot() +
  labs(title = "Story Sentiment Distribution") +
  ggeasy::easy_center_title() +
  facet_wrap(~Story)

