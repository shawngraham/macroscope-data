# tf-idf with salem witch trials

# start with the reports.df created after scraping

library(readr)
library(tidytext)
library(tidyverse)
library(forcats) # this is for the visualization

# tidy the witch reports
# either load witches-all-reports.csv from your machine or from online
witches  <- read_csv("https://raw.githubusercontent.com/shawngraham/macroscope-data/master/data/witches-all-reports.csv")

# we bring the data in from our earlier scrape
# put the data into a tibble (data structure for tidytext)
# we are also telling R what kind of data is in the 'text',
# 'line', and 'data' columns in our original csv.
# we are also stripping out all the digits from the text column

# do some tidying up
witches <- tibble (case = witches$id, text = (str_remove_all(witches$X.text., "[0-9]|[[:punct:]]")), year = witches$year, month = witches$month)
witches$id <- row.names(witches)


# turn into tidy format

tidy_witches <- witches %>%
  unnest_tokens(word, text)


# load up the default list of stop_words that comes
# with the tidyverse

data(stop_words)

# remove (anti-join) the stopwords
tidy_witches <- tidy_witches %>%
  anti_join(stop_words)

# you could try everything that follows by swapping 'case' for year in all subsequent lines
witch_words <- tidy_witches %>%
  count(year, word, sort = TRUE)

# take a look at what you've just done
# by examining the first few lines of `witches_words`

head(witch_words)

# already, you start to get a sense of what's in this dataset...

# but a word like 'aforesaid' or 'court' isn't going to be much use, so let's filter those out and look again.

# custom stopwords if necessary, add as desired

custom_stop_words <- bind_rows(tibble(word = c("aforesaid", "court", "archives","lord","salem","king","queen",
                                               "sarah","mary","john","wife","elizabeth","county","massachusetts",
                                               "john","county","essex","boston","witchcraft","lady","she","shee",
                                               "itt","joseph","george","jacobs","putnam","william","james","martin",
                                               "thomas","abigail"),  
                                      lexicon = c("custom")), 
                               stop_words)

# delete custom stopwords from our data
tidy_witches <- tidy_witches %>%
  anti_join(custom_stop_words)

witch_words <- tidy_witches %>%
  count(year, word, sort = TRUE)

head(witch_words)


######
# following https://www.tidytextmining.com/tfidf.html

# for visualization, it helps to also know
# the total counts of the various words
# by year

total_words <- witch_words %>% 
  group_by(year) %>% 
  summarize(total = sum(n))

witch_words <- left_join(witch_words, total_words)

witch_words

# and we can then also take a look at their frequency too
freq_by_rank <- witch_words %>% 
  group_by(year) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

freq_by_rank

# so now let's calculate the tf-idf
witches_tf_idf <- witch_words %>%
  bind_tf_idf(word, year, n)

witches_tf_idf

# we can a look at the result, organized in descending order
witches_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# so let's visualize the result by years
# you can choose how many top words to show
# by adjusting 'n', but if words have the same
# score, you'll sometimes end up with more 
witches_tf_idf %>%
  group_by(year) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = year)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~year, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)


