library(dplyr)
library(janitor)
library(tidyverse)
library(ggplot2) 
library(lubridate)
library(tidyr)
library(data.table)
library(scales)
library(grid)
library(gridExtra)
library(gghighlight)
library(wordcloud2)

#import file 
movies <- read.csv("movies.csv", na.strings = c('')) %>% clean_names()
glimpse(movies)

#mengubah tipe data release_date (char) menjadi date
movies$release_date <- ymd(movies$release_date)

#membuang kolom yang tidak dibutuhkan dan memfilter movies dari tahun 2000 sampai 2021

movies <- movies %>% 
  select(-c(keywords, credits, poster_path, backdrop_path)) %>% 
  mutate(year = year(release_date))%>% 
  filter(year>=2000, status=='Released')
nrow(movies)


#pengecekan duplikat data
movies$id[duplicated(movies$id)]
movies <- movies %>% distinct(id, title, .keep_all=TRUE)

nrow(movies)

#melihat N.A value
map(movies, ~sum(is.na(.)))


#memecah genre menjadi genre 1,2,3

movies_genre <- as.data.frame(movies$genres, stringsAsFactors = FALSE)
movies_genre2 <- as.data.frame(tstrsplit(movies_genre[,1], '[-]', type.convert=TRUE), 
                                    stringsAsFactors=FALSE)
colnames (movies_genre2) <- c("genre1", "genre2")

#hanya membutuhkan 2 genre, membuang lebihnya
movies_genre <-  movies_genre2 %>% select(c("genre1", "genre2"))

#menggabungkan ke genre yang di pecah ke movies dan membuang kolom genres
movies <- cbind (movies, movies_genre) 

#menghilangkan . di kolom popularity
movies$popularity <- as.numeric(gsub(".", "", movies$popularity, fixed=TRUE))    

#movie berdasarkan genre 1 & genre 2

genre1 <-  
  movies %>% 
  select(genre1) %>% 
  drop_na() %>% 
  group_by(genre1) %>% 
  summarise (total = n()) %>% 
  arrange(desc(genre1)) %>% 
  ggplot(aes(x = reorder(genre1,total), y=total, fill = total))+
  geom_bar(stat='identity')+
  coord_flip()+
  scale_fill_gradient2(mid = "black", high = "purple")+
  labs(x = "Genres",
       y = " Total",
       title = "Movies by Genre 1")+
  theme_minimal()+
  theme(legend.position = "none")

genre2 <- 
  movies%>% select(genre2) %>% 
  drop_na() %>% 
  group_by(genre2) %>% 
  summarise (total = n()) %>% 
  ggplot(aes(x = reorder(genre2,total), y=total, fill = total))+
  geom_bar(stat='identity')+
  scale_fill_gradient2(mid = "black", high = "purple")+
  coord_flip()+
  labs(x = "Genres",
       y = " Total",
       title = "Movies by Genre 2")+
  theme_minimal()+theme(legend.position = "none")

grid.arrange(genre1, genre2, ncol=2)

#jumlah movies per tahun

movies %>%
  drop_na() %>% 
  group_by(year, genre1) %>% 
  summarise(total = n()) %>% 
  ggplot(aes(year, total))+
  geom_line(aes(color = genre1), linewidth = 2)+
  labs(x = "Year",
       y = "Total Movies",
       title = "Movies From 2012 to 2022")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  gghighlight(max(total)>=30, label_key = genre1)

#Vote average berdasarkan genre 1 dan genre 2

vote_genre <- movies %>% 
  drop_na() %>% 
  ggplot(aes(genre1, year, fill=vote_average))+
  geom_tile()+
  coord_flip()+
  scale_fill_gradient2(mid ="purple", high = "black")+
  labs ( x = "Genre 1",
         y = "Year",
         title = "Vote Average by Genre 1")+
  theme_minimal()

vote_genre2 <- movies %>% 
  drop_na() %>% 
  ggplot(aes(genre2, year, fill=vote_average))+
  geom_tile()+
  coord_flip()+
  scale_fill_gradient2(mid ="purple", high = "black")+
  labs ( x = "Genre 2",
         y = "Year",
         title = "Vote Average by Genre 2")+
  theme_minimal()

grid.arrange(vote_genre+theme(legend.position = 'hidden'), vote_genre2+theme(legend.position = 'top'), ncol=2)



# Top Genre1 "Documentary" Overview Words


library(tidytext)
library(textclean)

documentary <- movies %>%
  select (genre1, overview) %>% 
  filter (genre1 == "Documentary") %>% 
  drop_NA()

overviews <- documentary$overview

# cleaning
overviews <- overviews %>% 
  str_to_lower() %>% #Change word to lower case
  replace_contraction() %>%  #Replace contractions with both words
  replace_word_elongation() %>%   #Replace word elongations with shortened form
  strip()

#tokenize &remove stopwords
documentary <- enframe(overviews, value = "word", name=NULL) %>% #vector to data frame
  unnest_tokens(word, word) %>% #changing 1 word to 1 coloumn
  count(word, sort = T) %>% #counting word sorting by desc
  anti_join(stop_words) #anti join stop words (a, is, of, the..)

#creating wordcloud viz
library(RColorBrewer)
library(wordcloud)
documentary %>% 
  with(wordcloud(words = word,
                 freq = n,
                 max.words = 250,
                 random.order = F,
                 colors = brewer.pal(name="Set1", n=8)))

?wordcloud
#wordlcloud 2
fil_doc <- documentary %>% 
  arrange(desc(n)) %>% 
  slice(1:100)
  
wordcloud2(fil_doc, size=1, color = 'random-light', backgroundColor = "grey", shape = 'star')

#top 10 movies by popularity
movies %>% select(title, popularity) %>%
  arrange(desc(popularity)) %>% 
  slice(1:10) %>% 
  ggplot(aes(reorder(title,popularity),popularity, fill=title))+
  geom_bar(stat='identity')+
  coord_flip()+
  theme_minimal()+
  labs (x= "Title",
        y= "Popularity",
        title = "TOP 10 Movies by Popularity")+theme(legend.position = "none")

#top 10 movies by revenue
movies %>% select(title, revenue) %>%
  arrange(desc(revenue)) %>% 
  slice(1:10) %>% 
  ggplot(aes(reorder(title,revenue), revenue, fill = revenue))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(x = "Title",
       y = "Revenue $",
       title = "TOP 10 movies by Revenue")


#top 10 movies by budget
movies %>% select(title, budget) %>%
  arrange(desc(budget)) %>% 
  slice(1:10) %>% 
  ggplot(aes(reorder(title,budget), budget, fill = budget))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(x = "Title",
       y = "Budget $", 
       title = "TOP 10 movies by Budget")

#type of orignal language (10)

movies %>% group_by(original_language) %>% 
  summarise(total = n()) %>%
  arrange(desc(total)) %>% 
  slice(1:10) %>%  
  ggplot(aes(reorder(original_language,total), total, fill=total))+
  geom_bar(stat = 'identity')+
  coord_flip()+
  scale_fill_gradient2(mid = muted("purple"), high = "black")+
  labs(x= "Original Language",
       y= "Total",
       title = "TOP 10 movies by original language")


#korelasi antara budget, popularity, dan revenue

cor_data <- movies %>% select(revenue, budget, popularity, vote_average, vote_count, runtime) %>% 
  filter(revenue>0, budget>0) %>% drop_na(runtime)

cor(cor_data$revenue, cor_data$budget)
cor(movies$budget, movies$revenue)

pairs(cor_data)


# Function to add correlation coefficients
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- abs(cor(x, y)) # Remove abs function if desired
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
}

# Plotting the correlation matrix
pairs(cor_data,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth) # Smoothed regression lines



