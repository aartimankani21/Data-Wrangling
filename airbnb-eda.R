
##Loading libraries

library(dplyr)
library(ggplot2)
library(tidyr)
library(ggmap)
library(wordcloud)
library(tidyverse)
library(tidytext)
library(magrittr)
library(lubridate)
library(data.table)
library(ggrepel)
library(rgdal)
library(sp)
library(maptools)
library(broom)
library(httr)
library(readr)
library(tidyr)
library(grid)
library(gridExtra)


## Loading Data

## Download the data and enter the path here

listings <- read.csv("../listingsnyc.csv", comment.char="#")
reviews <- read.csv("../reviews.csv", comment.char="#")

# Understanding the data

nrow(reviews)
nrow(listings)
ncol(listings)
names(listings)
names(reviews)
str(reviews)
is.na(listings)
is.na(reviews)

# Removing data with missing values

listings<- na.omit(listings)
reviews<-na.omit(reviews)

# Joining Reviews and Listings

joined1 <- reviews %>% 
  left_join(select(listings,id,neighbourhood,neighbourhood_group),  by = c("listing_id" = "id"))

joined1 <- na.omit(joined1)
joined1$comments <- as.character(joined1$comments)
joined1$comments <- tolower(joined1$comments)


################### SENTIMENT GRAPH #####################


# Tokenization and removal of stop words and special characters

joined1_tidy <- tibble(text = joined1$comments) %>%
  group_by(neighbourhood_group = joined1$neighbourhood_group, neighbourhood= joined1$neighbourhood) %>%
  ungroup() %>%
  unnest_tokens(word, text)%>% 
  anti_join(stop_words) %>%
  filter(str_detect(word, "^[a-z']+$"))

#Finding top 5 most reviewed neighbourhoods in each boroughs

top5 <-joined1_tidy %>% 
  group_by(neighbourhood_group,neighbourhood)%>%
  summarise(n=n())%>% 
  arrange(desc(n))%>%
  top_n(n=5, wt = neighbourhood)

#Selecting data with top 5 neighbourhoods in each boroughs

joined1_tidy <-joined1_tidy %>% 
  filter(neighbourhood %in% top5$neighbourhood)

# Finding sentiment of a word

joined1_temp <- joined1_tidy %>% 
  filter(word != "stay") %>% 
  inner_join(get_sentiments("bing"))

# finding Overall sentiment in each neighbourhood

joined1_sentiment <- joined1_temp %>%
  count(ng = joined1_temp$neighbourhood_group, ne= joined1_temp$neighbourhood, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Overal sentiment analysis plot

ggplot(joined1_sentiment, aes(ne, sentiment, fill = ng)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ng, scales = "free_x") +
  scale_y_continuous(trans='log2')+
  ggtitle("Sentiment Graph of Most Reviewed Neighbourhoods of NYC")+ 
  xlab("Neighbourhoods")+ 
  ylab("Sentiments")


# Negative sentiment analysis plot


ggplot(joined1_sentiment, aes(ne, negative, fill = ng)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ng, scales = "free_x") + 
  scale_y_continuous(trans='log2')+
  ggtitle("Negative Sentiment Graph of Most Reviewed Neighbourhoods of NYC")+
  xlab("Neighbourhoods")+
  ylab("Negative Sentiments")




################ WORD CLOUD ############################


# separating data based on borough

manhattan <- joined1%>% filter(neighbourhood_group=='Manhattan')
brooklyn <- joined1%>% filter(neighbourhood_group=='Brooklyn')
bronx <- joined1%>% filter(neighbourhood_group=='Bronx')
queens <- joined1%>% filter(neighbourhood_group=='Queens')
staten_island <- joined1%>% filter(neighbourhood_group=='Staten Island')

### Manhattan ###

# Tokenization and removal of stop words and special characters

manhattan_tidy <- tibble(text = manhattan$comments) %>%
                    ungroup() %>%
                    unnest_tokens(word, text)%>% 
                    anti_join(stop_words) %>%
                    filter(str_detect(word, "^[a-z']+$"))

# Finding frequency of each word

manhattan_tidy <- manhattan_tidy %>% 
                         group_by(word) %>%
                         summarise(n = n())
            
#wordcloud for manhattan reviews

wordcloud(words = manhattan_tidy$word, freq = manhattan_tidy$n, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))


### Brooklyn ###

# Tokenization and removal of stop words and special characters

brooklyn_tidy <- tibble(text = brooklyn$comments) %>%
                  ungroup() %>% 
                  unnest_tokens(word, text)%>% 
                  anti_join(stop_words) %>%
                  filter(str_detect(word, "^[a-z']+$"))

# Finding frequency of each word

brooklyn_tidy <- brooklyn_tidy %>% 
                  group_by(word) %>%
                  summarise(n = n())

#wordcloud for brooklyn reviews

wordcloud(words = brooklyn_tidy$word, freq = brooklyn_tidy$n, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))


### Queens ###

# Tokenization and removal of stop words and special characters

queens_tidy <- tibble(text = queens$comments) %>%
                ungroup() %>%
                unnest_tokens(word, text)%>% 
                anti_join(stop_words) %>%
                filter(str_detect(word, "^[a-z']+$"))

# Finding frequency of each word

queens_tidy <- queens_tidy %>% 
                  group_by(word) %>%
                  summarise(n = n())

#wordcloud for queens reviews

wordcloud(words = queens_tidy$word, freq = queens_tidy$n, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))

### Bronx ###

# Tokenization and removal of stop words and special characters

bronx_tidy <- tibble(text = bronx$comments) %>%
                ungroup() %>% 
                unnest_tokens(word, text)%>% 
                anti_join(stop_words) %>% 
                filter(str_detect(word, "^[a-z']+$"))

# Finding frequency of each word

bronx_tidy <- bronx_tidy %>% 
                group_by(word) %>%
                summarise(n = n())

#wordcloud for bronx reviews

wordcloud(words = bronx_tidy$word, freq = bronx_tidy$n, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))

### Staten Island ###

# Tokenization and removal of stop words and special characters

staten_island_tidy <- tibble(text = staten_island$comments) %>%
                        ungroup() %>%
                        unnest_tokens(word, text)%>% 
                        anti_join(stop_words) %>% 
                        filter(str_detect(word, "^[a-z']+$"))

# Finding frequency of each word

staten_island_tidy <- staten_island_tidy %>% 
                        group_by(word) %>%
                        summarise(n = n())

#wordcloud for staten island reviews

wordcloud(words =staten_island_tidy$word, freq =staten_island_tidy$n, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))


#####################Reviews per month ###################################

#converting reviews into date format

reviews$date <-ymd(reviews$date)

# Adding a column Month from date

reviews <- reviews %>% mutate(month=month(reviews$date))

# Grouping by month and counting the total reviews

monthwise <- reviews %>% group_by(month) %>% summarise(revs = n())

# Plotting Total Reviews throughout the year to find out when people rent the most

ggplot(monthwise, aes((month), revs)) +
  geom_point(color = "blue") +geom_smooth(color = "red")+
  ggtitle("Total Reviews throughout the year") + 
  xlab("Months")+ ylab("Total reviews")+
  scale_x_discrete(limits = c(1,2,3,4,5,6,7,8,9,10,11,12))

#### REVIEWS NEIGHBOURHOOD WISE ###

# Joining reviews and listings 

joined2 <- reviews %>% 
     inner_join(select(listings,id,neighbourhood,neighbourhood_group),  by = c("listing_id" = "id"))

# Removing Null Values
joined2 <- na.omit(joined2)

# Grouping by month and counting the total reviews

month_area_wise <- joined2 %>% group_by(neighbourhood_group, month) %>% summarise(revs = n())

# Plotting Reviews Neighbourhood wise

ggplot(month_area_wise, aes((month), revs, color=neighbourhood_group)) +
  geom_point() +geom_smooth()+
  ggtitle("Total Reviews throughout the year in NYC Neighbourhoods") + 
  xlab("Months")+ ylab("Total reviews")+
  scale_x_discrete(limits = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  facet_wrap(~neighbourhood_group, scales = "free") + 
  theme(legend.position = "none")



### Roomtype by neighbourhood ###

# Grouping by borough and room type 
listing_type <- listings %>% group_by(neighbourhood_group, room_type)%>% summarise(n=n())

# Grouping by borugh
total_listing<-  listings %>% group_by(neighbourhood_group) %>% summarize(sum = n())
# Finding ratio of room_type by total rooms in a borough
listing_ratio <- merge(listing_type, total_listing, by="neighbourhood_group")
listing_ratio <- listing_ratio %>% mutate(ratio = n/sum)

# Plot

ggplot(listing_ratio, aes(x=neighbourhood_group, y=ratio, fill = room_type)) +
  geom_bar(position = "dodge",stat="identity") + xlab("Neighbourhoods") + ylab("Count")+
  scale_fill_discrete(name = "Property Type") + 
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Percentage of Listing Type by Neighbourhood") +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_gradient(low="#d3cbcb", high="#852eaa")+
  scale_fill_manual("Room Type", values=c("#e06f69","#357b8a", "#f6c458")) +
  xlab("Neighborhood") + ylab("Percentage")


### room type map of nyc

# Using ggmap to plot data roomwise in NYC, Manhattan and Brooklyn

ggmap::register_google(key = "ENTER THE API KEY HERE")

nyc_map <- get_map(location = c(lon = -74.00, lat = 40.71), maptype = "terrain", zoom = 11)

man_listings <- listings%>% filter(neighbourhood_group == "Manhattan")
man_map <- get_map(location = c(lon = -73.97, lat = 40.7831), maptype = "terrain", zoom = 12)

brook_listings <- listings%>% filter(neighbourhood_group == "Brooklyn")
brooklyn_map <- get_map(location = c(lon = -73.9442, lat = 40.6782), maptype = "terrain", zoom = 12)

# Whole NYC
ggmap(nyc_map)+
  geom_point(data=listings, aes(longitude, latitude, color = factor(room_type)), alpha=0.4, size=0.2)+
  scale_colour_brewer(type="qual", palette="Dark2", name = "Room type")+
  ggtitle("Different types of listings in NYC") + 
  theme(legend.text=element_text(size=13))+labs(x="",y="")

# Manhattan
ggmap(man_map)+
  geom_point(data=man_listings, aes(longitude, latitude, color = factor(room_type)), alpha=0.4, size=0.2)+
  scale_colour_brewer(type="qual", palette="Dark2", name = "Room type")+
  ggtitle("Different types of listings in Manhattan") +
  theme(legend.text=element_text(size=13))+labs(x="",y="")


# Brooklyn
ggmap(brooklyn_map)+
  geom_point(data=brook_listings, aes(longitude, latitude, color = factor(room_type)), alpha=0.4, size=0.2)+
  scale_colour_brewer(type="qual", palette="Dark2", name = "Room type")+
  ggtitle("Different types of listings in Brooklyn") +
  theme(legend.text=element_text(size=13))+labs(x="",y="")




### PRICE WISE MAP of NYC

ggmap(nyc_map) +
  stat_density2d(data=listings, aes(longitude, latitude, fill=..level.., 
                               alpha=..level..), geom="polygon", size=0.01, bins=30)+
  scale_fill_gradient(low="#ff9999", high="#990000",breaks=seq(0,400,50),
                      limits=c(0, 450), guide = "legend", name = "Price")+
  scale_alpha(range=c(0.2, 0.4), guide=FALSE)+
  ggtitle("Heat Map of Listings by Price of NYC")+labs(x="",y="")



#manhattan

ggmap(man_map) +
  stat_density2d(data=man_listings, aes(longitude, latitude, fill=..level.., 
                                    alpha=..level..), geom="polygon", size=0.01, bins=30)+
  scale_fill_gradient(low="#ff9999", high="#990000",breaks=seq(0,1000,50),
                      limits=c(0, 1000), guide = "legend", name = "Price")+
  scale_alpha(range=c(0.2, 0.4), guide=FALSE)+
  ggtitle("Heat Map of Listings by Price of Manhattan")+labs(x="",y="")

#brooklyn


ggmap(brooklyn_map) +
  stat_density2d(data=brook_listings, aes(longitude, latitude, fill=..level.., 
                                    alpha=..level..), geom="polygon", size=0.01, bins=30)+
  scale_fill_gradient(low="#ff9999", high="#990000",breaks=seq(0,600,50),
                      limits=c(0, 600), guide = "legend", name = "Price")+
  scale_alpha(range=c(0.2, 0.4), guide=FALSE)+
  ggtitle("Heat Map of Listings by Price of Brooklyn")+labs(x="",y="")




### PRICE AND ROOM WISE
mean(listings$price)
median(listings$price)

#manhattan

rich1 <- man_listings %>% filter(price > 500)

m1 <- ggmap(man_map) +
  geom_point(data = rich1, aes(longitude, latitude, size = price, color = factor(room_type)), alpha=0.9)+
  scale_colour_brewer(type="qual", palette="Dark2", name = "Room type")+
  labs(x = "", y = "", title = "Most Expensive Listings in Manhattan")

cheap1 <- man_listings %>% filter(price >0 ) %>%filter(price < 100)

m2 <- ggmap(man_map) +
  geom_point(data =cheap1, aes(longitude, latitude, size = price, color = factor(room_type)), alpha=0.9)+
  scale_colour_brewer(type="qual", palette="Dark2", name = "Room type")+
  labs(x = "", y = "", title = "Cheapest Listings in Manhattan")+scale_size_continuous(range = c(1, 3))
grid.arrange(m1, m2, ncol=2)


#Brooklyn

rich2 <- brook_listings %>% filter(price > 300) %>% filter(price < 5000)

b1 <-ggmap(brooklyn_map) +
  geom_point(data = rich2, aes(longitude, latitude, size = price, color = factor(room_type)), alpha=0.9)+
  scale_colour_brewer(type="qual", palette="Dark2", name = "Room type")+
  labs(x = "", y = "", title = "Most Expensive Listings in Brooklyn")

cheap2 <- brook_listings %>% filter(price >0 ) %>%filter(price < 100)

b2<- ggmap(brooklyn_map) +
  geom_point(data =cheap2, aes(longitude, latitude, size = price, color = factor(room_type)), alpha=0.9)+
  scale_colour_brewer(type="qual", palette="Dark2", name = "Room type")+
  labs(x = "", y = "", title = "Cheapest Listings in Brooklyn")+scale_size_continuous(range = c(1, 3))
grid.arrange(b1, b2, ncol=2)
