# slightly modified version of
# https://tm4ss.github.io/docs/Tutorial_6_Topic_Models.html
# by Andreas Niekler, Gregor Wiedemann

# libraries
library(tidyverse)
library(tidytext)


witches  <- read_csv("witches-all-reports.csv")

# we bring the data in from our earlier scrape, where it was known as `reports.df`
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

# this line might take a few moments to run btw
witch_words <- tidy_witches %>%
  count(id, word, sort = TRUE)

# take a look at what you've just done
# by examining the first few lines of `cb_words`

head(witch_words)

# already, you start to get a sense of what's in this dataset...

# but a word like 'aforesaid' or 'court' isn't going to be much use, so let's filter those out and look again.

# custom stopwords if necessary, add as desired

custom_stop_words <- bind_rows(tibble(word = c("aforesaid", "court", "archives","lord","salem","king","queen",
                                               "sarah","mary","john","wife","elizabeth","county","massachusetts",
                                               "john","county","essex","boston","witchcraft","lady","she","shee",
                                               "itt","joseph","george","jacobs","putnam","william","james","martin",
                                               "thomas","abigail"),  
                                      lexicon = c("art")), 
                               stop_words)

# delete custom stopwords from our data
tidy_witches <- tidy_witches %>%
  anti_join(custom_stop_words)

witch_words <- tidy_witches %>%
  count(id, word, sort = TRUE)

head(witch_words)

# do the custom list again if you see more that should be removed, then

# turn that into a matrix
dtm <- witch_words %>%
  cast_dtm(id, word, n)


require(topicmodels)
# number of topics
K <- 20
# set random number generator seed
# for purposes of reproducibility
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel <- LDA(dtm, K, method="Gibbs", control=list(iter = 500, verbose = 25))

# have a look a some of the results (posterior distributions)
tmResult <- posterior(topicModel)

# format of the resulting object
attributes(tmResult)

# lengthOfVocab
ncol(dtm)

# topics are probability distributions over the entire vocabulary
beta <- tmResult$terms   # get beta from results
dim(beta)

# for every document we have a probability distribution of its contained topics
theta <- tmResult$topics
dim(theta)        

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")
topicNames

# this is another point where you might want to go back to the custom stop words; perhaps add personal names etc

#---
# load libraries for visualization
library("reshape2")
library("ggplot2")

# select some documents for the purposes of
# sample visualizations
# in our corpus

exampleIds <- c(1,2) #ie, the first and second rows of data from 'witches'

N <- length(exampleIds)
# get topic proportions form example documents
topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames

# put the data into a dataframe just for our visualization
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  

# specify the geometry, aesthetics, and data for a plot
ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)


#topics over time

# collapse years into specific decades for aggregation
# here we rewrite the year column to become the decade

witches$year <- paste0(substr(witches$year, 0, 3), "0" )

# get mean topic proportions per decade
topic_proportion_per_decade <- aggregate(theta, by = list(decade = witches$year), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_decade)[2:(K+1)] <- topicNames

# reshape data frame, for when I get the topics over time thing sorted
vizDataFrame <- melt(topic_proportion_per_decade, id.vars = "decade")

# plot topic proportions per deacde as bar plot
require(pals)
ggplot(vizDataFrame, aes(x=decade, y=value, fill=variable)) +
  geom_bar(stat = "identity") + ylab("proportion") +
  scale_fill_manual(values = paste0(alphabet(25), "FF"), name = "decade") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

