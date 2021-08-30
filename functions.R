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
    filter(word %!in% c(stop_words$word, "you’re", "i’m", "don’t","can’t","it’s",
                        "that’s")) %>%
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
  sentence_df$text <- str_remove_all(sentence_df$text, '“|”')
  
  sentence_df
}

sentiment_dist <- function(sentiment_df, name){
  #identifying most positive and negative sentences (6 words or more)
  most_pos <- sentiment_df %>% filter(word_count > 6) %>% 
    slice(1)
  most_pos_all <- sentiment_df %>% slice(1)
  most_neg <- sentiment_df %>% arrange(sentiment) %>% filter(word_count > 6) %>% 
    slice(1)
  most_neg_all <- sentiment_df %>% arrange(sentiment) %>% slice(1)
  ggplot(sentiment_df, aes(x = sentence, y = sentiment)) + geom_line() + theme_minimal() +
    labs(title = name) +
    ggeasy::easy_center_title() +
    geom_text(data = most_neg, 
              aes(x = sentence, y = sentiment - 0.03, 
                  label = text, family = "serif", size = 20,
                  vjust = "inward", hjust = "inward")) +
    geom_point(data = most_neg, aes(x = sentence, y = sentiment)) +
    geom_text(data = most_neg_all, 
              aes(x = sentence, y = sentiment - 0.03,
                  label = text, family = "serif", size = 20,
                  vjust = "inward", hjust = "inward")) +
    geom_point(data = most_neg_all, aes(x = sentence, y = sentiment)) +
    geom_text(data = most_pos, 
              aes(x = sentence, y = sentiment + 0.03, 
                 label = text, family = "serif", size = 20,
                 vjust = "inward", hjust = "inward")) +
    geom_point(data = most_pos, aes(x = sentence, y = sentiment)) +
    geom_text(data = most_pos_all,
              aes(x = sentence, y = sentiment + 0.03, 
                  label = text, family = "serif", size = 20,
                  vjust = "inward", hjust = "inward")) +
    geom_point(data = most_pos_all, aes(x = sentence, y = sentiment)) +
    theme(text=element_text(size=24,  family="serif"),legend.position = "none")
  
}

test_pt <- amazing2 %>% arrange(sentiment) %>% filter(word_count > 6) %>% 
  slice(1) 
geom_text(data = test_pt, aes(x = sentence * 1.05, y = sentiment, label = text))

sentiment_smooth <- function(sentiment_df, name){
  ggplot(sentiment_df, aes(x = sentence, y = sentiment)) + 
    geom_smooth(method = "loess") + 
    theme_minimal() +
    labs(title = name) +
    ggeasy::easy_center_title() +
    theme(text=element_text(size=24,  family="serif"), legend.position = "none")
}

#amazing grace ----
amazing1 = freq_story_words("stories2/amazing grace.docx", "Amazing Grace")
amazing2 = sentiment_df("stories2/amazing grace.docx", "Amazing Grace")

png("plots/amazinggracecloud.png",width = 1200, height = 800, res = 300)
wordcloud(amazing1$word,amazing1$Freq, min.freq = 5, 
          scale=c(4,0.5), random.order = FALSE, family = "serif")
dev.off()

png("plots/amazinggracedist.png", width = 1200, height = 800)
sentiment_dist(amazing2, "Amazing Grace - Sentiment Distribution")
dev.off()

png("plots/amazinggracesmooth.png", width = 1200, height = 800)
sentiment_smooth(amazing2, "Amazing Grace - Smoothed Sentiment Curve")
dev.off()

#quiet gods ----
quiet1 = freq_story_words("stories2/The Quiet Gods.docx", "The Quiet Gods")
quiet2 = sentiment_df("stories2/The Quiet Gods.docx", "The Quiet Gods")

png("plots/quietgodscloud.png",width = 1200, height = 800, res = 300)
wordcloud(quiet1$word,quiet1$Freq, min.freq = 4, scale=c(4.5,1), 
          random.order = FALSE, family = "serif", res = 300)
dev.off()

png("plots/quietgodsdist.png", width = 1200, height = 800)
sentiment_dist(quiet2, "The Quiet Gods - Sentiment Distribution")
dev.off()

png("plots/quietgodssmooth.png", width = 1200, height = 800)
sentiment_smooth(quiet2, "The Quiet Gods - Smoothed Sentiment Curve")
dev.off()

#candle in the dark ----
candle1 = freq_story_words("stories2/candle in the dark.docx", "Candle in the Dark")
candle2 = sentiment_df("stories2/candle in the dark.docx", "Candle in the Dark")

png("plots/candlecloud.png",width = 1200, height = 800, res = 300)
wordcloud(candle1$word,candle1$Freq, min.freq = 4, scale=c(3,0.25), 
          random.order = FALSE, family = "serif")
dev.off()

png("plots/candledist.png", width = 1200, height = 800)
sentiment_dist(candle2, "Candle in the Dark - Sentiment Distribution")
dev.off()

png("plots/candlesmooth.png", width = 1200, height = 800)
sentiment_smooth(candle2, "Candle in the Dark - Smoothed Sentiment Curve")
dev.off()

#outpost ----
outpost1 = freq_story_words("stories2/the outpost.docx", "The Outpost")
outpost2 = sentiment_df("stories2/the outpost.docx", "The Outpost")

png("plots/outpostcloud.png", width = 1200, height = 800, res = 300)
wordcloud(outpost1$word,outpost1$Freq, min.freq = 10, scale=c(4.25,0.25),
          random.order = FALSE, family = "serif")
dev.off()

png("plots/outpostdist.png", width = 1200, height = 800)
sentiment_dist(outpost2, "The Outpost - Sentiment Distribution")
dev.off()

png("plots/outpostsmooth.png", width = 1200, height = 800)
sentiment_smooth(outpost2, "The Outpost - Smoothed Sentiment Curve")
dev.off()

#terrarium ----
terrarium1 = freq_story_words("stories2/terrarium.docx", "Terrarium")
terrarium2 = sentiment_df("stories2/terrarium.docx", "Terrarium")

png("plots/terrariumcloud.png", width = 1200, height = 800, res = 300)
wordcloud(terrarium1$word,terrarium1$Freq, min.freq = 6, scale=c(3.25,0.25), 
          random.order = FALSE, family = "serif")
dev.off()

png("plots/terrariumdist.png", width = 1200, height = 800)
sentiment_dist(terrarium2, "Terrarium - Sentiment Distribution")
dev.off()

png("plots/terrariumsmooth.png", width = 1200, height = 800)
sentiment_smooth(terrarium2, "Terrarium - Smoothed Sentiment Curve")
dev.off()


#synthesis ------
synthesis1 = freq_story_words("stories2/synthesis.docx", "Synthesis")
synthesis2 = sentiment_df("stories2/synthesis.docx", "Synthesis")

png("plots/synthesiscloud.png", width = 1200, height = 800, res = 300)
wordcloud(synthesis1$word,synthesis1$Freq, min.freq = 3, scale=c(2,0.25), 
          random.order = FALSE, family = "serif")
dev.off()

png("plots/synthesisdist.png", width = 1200, height = 800)
sentiment_dist(synthesis2, "Synthesis - Sentiment Distribution")
dev.off()

png("plots/synthesissmooth.png", width = 1200, height = 800)
sentiment_smooth(synthesis2, "Synthesis - Smoothed Sentiment Curve")
dev.off()

#also add most positive/negative/neutral sentence
synthesis2 %>% select(text)

#all stories ----

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

