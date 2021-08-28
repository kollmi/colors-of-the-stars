library(readtext)
library(tidyverse)
library(tidytext)
library(tokenizers)
library(sentimentr)

outpost <- readtext::readtext("stories/amazing grace.docx")$text

`%!in%` = Negate("%in%")

clean_tokens <- outpost %>% 
  tokenize_ngrams(n = 1) %>% 
  unlist() %>% 
  tolower() 
freq_words <- clean_tokens %>% 
  table() %>% 
  data.frame() %>% 
  rename(word = ".") %>% 
  filter(word %!in% stop_words$word) %>% 
  arrange(-Freq)

#sentiment of sentences
sentences <- outpost %>% 
  tokenize_sentences() %>% 
  unlist() 

sentence_df <- data.frame(sentence = 1:length(sentences), text = sentences) %>% 
  inner_join(sentiment(sentences), by = c("sentence" = "element_id")) %>% 
  arrange(-sentiment) %>% 
  select(-sentence_id)

ggplot(sentence_df, aes(x = sentence, y = sentiment)) + geom_line() + theme_minimal() +
  stat_smooth(method = 'nls', formula = y ~ a * exp(b * x),aes(color = 'exponential'), se = FALSE, method.args = list(start = list(a = 1, b = 1))) +
  stat_smooth(method = 'nls', formula = y ~ a * log(x) + b, aes(color = 'logarithmic'), se = FALSE, method.args = list(start = list(a = 1, b = 1))) +
  stat_smooth(method = 'lm', aes(color = 'linear'), se = FALSE) +
  stat_smooth(method = 'lm',formula = y ~ poly(x,2), aes(color = 'polynomial'), se = FALSE) +
  scale_color_brewer(name = "Trendline", palette = "Set1")

ggplot(sentence_df, aes(x = sentence, y = sentiment)) + geom_smooth() + theme_minimal()
  
