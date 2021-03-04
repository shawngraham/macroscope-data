# Salem Witchcraft Papers
# http://salem.lib.virginia.edu/swp-intro.html


library(rvest)
library(dplyr)
library(magrittr)

base_url <- "http://salem.lib.virginia.edu/"
main_page <- read_html(x = "http://salem.lib.virginia.edu/category/swp.html")	

urls <- main_page %>% # feed `main.page` to the next step
  html_nodes("a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

links <- main_page %>% 		# feed `main.page` to the next step
  html_nodes("a") %>% 	# get the CSS nodes
  html_text() 		# extract the text

reports <- data.frame(links = links, urls = paste(base_url,urls, sep=""), stringsAsFactors = FALSE)
View(reports)

reports <- reports %>% slice(10:150)

dir.create("reports")

#---
# Loop over each row in `reports`
# and write the result to a dataframe
#---

reports.df <- data.frame()

for(i in seq(nrow(reports))) { 
  
  # we're going to loop over each row in 'reports', extracting the entries from the pages and then writing them to file.
  
  text <- read_html(reports$urls[i]) %>% # load the page
    html_nodes(".doc") %>% # isloate the text
    html_text() # get the text
  
  reports.df <- rbind(reports.df, data.frame((text), row.names=NULL), stringsAsFactors = FALSE)
  
  } #end the loop



# in order to topic model, we'd like to extract some metadata into new columns
# if there was a separate div or class in the original html for date, this would be more straightforward.
# but there isn't, so we'll have to use some pattern matching.

library(stringr)

# str_extract will grab the first match
# reports.df[,1] is necessary so that R knows to search through the first column (the only column) of data; it's because of a data type issue
month <- (str_extract(reports.df[,1], 
                          "January|February|bMarch|April|May|June|July|August|September|October|November|December"))

# this will grab the year; we could get days similarly
year <- str_extract(reports.df[,1], "\\d{4}")

# this one was tricky. there are groups within groups. Basically, it says, find two or one digit numbers separated by a point.
id <- str_extract(reports.df[,1], "(([0-9]{2})|([0-9]{1})).(([0-9]{2})|([0-9]{1}))")

# this appends these new columns to our original column
reports.df$year <- year
reports.df$month <- month
reports.df$id <- id  # this is the original case number, not the row id

#get rid of newlines and commas in the text
reports.df$X.text. <- str_replace_all(reports.df$X.text., "[\r\n]", "")
reports.df$X.text. <- str_replace_all(reports.df$X.text., ",", "")

install.packages('writexl')
library(writexl)
write_xlsx(reports.df, "all-reports.xlsx")

# we wrote this to excel, so that we could filter these columns to fix problems.

# now just going to go and fix by hand the dates, months that didn't come across
# tedious, but it could be worse.
# in excel, on the date column, if you hit ctrl+down arrow or cmd+down arrow, you'll jump to the first blank; you can see
# that the document is in reference to the one above it, and so can put the proper year in. You'll also see that
# there might be a blank row altogether; delete that if you come across it, around about row 801.
# you might have to read the other documents in the case to determine the appropriate date.
# now lets go see if the topic model script can work on all this.
# SWP 173.83 has no date. So delete it.

# SAVE AS CSV, and then proceed to the witches-tm.R script.
