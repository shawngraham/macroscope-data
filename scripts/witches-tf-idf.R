# tf-idf with salem witch trials

# start with the reports.df created after scraping
# following this https://mohitatgithub.github.io/2018-04-28-Learning-tf-idf-with-tidytext/

library(readr)
library(tidytext)
library(tidyverse)

# tidy the reports.df
witches  <- read_csv("witches-all-reports.csv")

# we bring the data in from our earlier scrape, where it was known as `reports.df`
# put the data into a tibble (data structure for tidytext)
# we are also telling R what kind of data is in the 'text',
# 'line', and 'data' columns in our original csv.
# we are also stripping out all the digits from the text column

# do some tidying up
witches <- tibble (case = witches$id, text = (str_remove_all(witches$X.text., "[0-9]|[[:punct:]]")), year = witches$year, month = witches$month)
witches$id <- row.names(witches)


# we're also going to try to put an indication of which case we're dealing with

#witch_reports <- witches %>%
#  mutate(case = case) %>%
#  unnest_tokens(word, text)

head(witch_reports)

# most frequent word?

witch_reports %>% count(word, sort = TRUE)

# remove stop words

data(stop_words)

# remove (anti-join) the stopwords
witch_reports <- witch_reports %>%
  anti_join(stop_words)

# this line might take a few moments to run btw
witch_words <- witch_reports %>%
  count(year, word, sort = TRUE)

head(witch_words)

# let's do some tf_idf and plot things out!

p1 <- witch_words %>%
  count(year, word, sort = TRUE) %>%
  bind_tf_idf(word, year, n) %>%
  arrange(-tf_idf) %>%
  group_by(year) %>%
  top_n(5) %>%
  ungroup %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = year)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ year, scales = "free") +
  coord_flip()

