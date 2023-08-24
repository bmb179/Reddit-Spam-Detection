#Libraries####

library(RedditExtractoR)
library(tidyverse)

#Reddit-Scrape####

#Search topic is a stock ticker symbol
symbol <- 'WIMI'

#Retrieves threads from the past month containing the search topic & formats dates
reddit <- find_thread_urls(keywords = symbol, sort_by = 'new', period = 'month')
reddit$ticker <- symbol
reddit$date_utc <- as.Date(reddit$date_utc)
reddit$timestamp <- as.POSIXct(reddit$timestamp, origin = '1970-01-01', tz = 'UTC')

#Author-Scraping####

#Creates a vector that is the same size as the reddit thread dataframe
author <- as.character(1:length(reddit$url))

#A series of loops that will write the names of thread authors to the author vector
#Pauses written in due to API limits in the RedditExtractoR library
Sys.sleep(10*60)
for (i in seq_along(reddit$url)[1:90]) {
  thread <- get_thread_content(reddit$url[i])$threads$author
  author[i] <- thread
}
Sys.sleep(10*60)
for (i in seq_along(reddit$url)[91:180]) {
  thread <- get_thread_content(reddit$url[i])$threads$author
  author[i] <- thread
}
Sys.sleep(10*60)
for (i in seq_along(reddit$url)[181:length(reddit$url)]) {
  thread <- get_thread_content(reddit$url[i])$threads$author
  author[i] <- thread
}
rm(thread)
rm(i)

#Anonymized author information created as a key between the reddit and user_key dataframes
username_masked <- inner_join(data.frame(author = author),
                              data.frame(author = unique(author), author_masked = paste('User', 1:length(unique(author)))))
reddit$author_masked <- username_masked$author_masked
user_key <- inner_join(data.frame(author = unique(author), author_masked = paste('User', 1:length(unique(author)))),
                       count(username_masked, author_masked, sort = TRUE))
rm(username_masked)

#Post-History####

#Retrieves post history for each post author named in the reddit dataframe
user.content <- get_user_content(user_key$author)

#for loop calculates the incidence rate of the search topic in the user's post history
post.frequency <- data.frame(author = c(),incidence = c())
for (i in names(user.content)){
  df <- data.frame(
    author = i,
    incidence = ifelse(is.na(user.content[[i]]$threads) == TRUE, 0,
                       (count(user.content[[i]]$threads %>% 
                                filter(grepl(symbol, text, ignore.case = TRUE)|
                                         grepl(symbol, url, ignore.case = TRUE)|
                                         grepl(symbol, title, ignore.case = TRUE)))/
                          count(user.content[[i]]$threads)))[[1]]
  )
  post.frequency <- rbind(post.frequency,df, deparse.level = 2)
}

#Incidence calculated by the for loop is joined to the user_key dataframe 
user_key <- inner_join(user_key, post.frequency)
rm(post.frequency)
rm(df)
rm(i)

#Creating bins using the incidence column for visualization
user_key <- mutate(user_key, incidence_bins = 
                     ifelse(incidence >= .90, 'Incidence is 90% or Greater',
                            ifelse(incidence >= .80, 'Incidence is Between 80% and 90%', 
                                   ifelse(incidence >= .10, 'Incidence is Between 10% and 80%', 'Incidence is Less Than 10%')
                            )
                     )
)

#Visuals####

#A blog theme consistent with my Substack's branding.
blog_theme <- theme(plot.background = element_rect(fill = '#14101b', color = '#14101b'),
                    panel.background = element_rect(fill = '#14101b', color = '#14101b'),
                    legend.background = element_rect(fill = '#14101b', color = '#14101b'),
                    legend.title = element_text(family = 'serif', color = 'White', face = 'bold', size = 12),
                    legend.text = element_text(family = 'serif', color = 'White', size = 12),
                    axis.title = element_text(family = 'serif', color = 'White', face = 'bold', size = 12),
                    axis.text = element_text(family = 'serif', color = 'White', size = 12),
                    plot.title= element_text(family = 'serif', color = 'White', face = 'bold', size = 18),
                    plot.subtitle= element_text(family = 'serif', color = 'White', size = 16),
                    panel.grid.major.y = element_blank()
)

#Visual that measures number of posts about the search topic by user.
#Represents which bin of incidence each user is a part of, helping to identify spammers.
user_key %>% ggplot(aes(fill = incidence_bins, x = n, y = reorder(author_masked,n))) + 
  geom_col() + blog_theme +
  labs(x = 'Number of Posts About the Search Topic', y = 'User Key',
       title = 'Comparing User Posting History About the Search Topic',
       subtitle = 'One Month Interval, Usernames and Post Content Anonymized',
       fill = 'User Posting History About the Search Topic')

#Visual that measures number of posts about the search topic by subreddit.
#Filters out noise by not including posts from users that are not frequently discussing the search topic.
#Hides the symbol name in the visual.
inner_join(
    mutate(reddit, subreddit = gsub(symbol, '[The official subreddit for this stock]', reddit$subreddit, ignore.case = TRUE)), 
    user_key) %>% 
  filter(incidence_bins != 'Incidence is Less Than 10%') %>% count(subreddit) %>% 
  ggplot(aes(x = n, y = reorder(subreddit,n))) + 
  geom_col(fill = 'White') + blog_theme +
  labs(x = 'Number of Posts About the Search Topic', y = 'Subreddit',
       title = 'Subreddits With the Most Posts About the Search Topic',
       subtitle = 'One Month Interval')
