# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.

library(plyr)
library(ggthemes)
dat <- read.csv('../input/movie_metadata.csv', header=TRUE)
df <- as.data.frame(dat)
head(df)

# first we use plyr to calculate the mean rating and SE for each main actor
ratingdat <- ddply(df, c("actor_1_name"), summarise,
                   M = mean(imdb_score, na.rm=T),
                   SE = sd(imdb_score, na.rm=T)/sqrt(length(na.omit(imdb_score))),
                   N = length(na.omit(imdb_score)))
ratings<-ratingdat[which(ratingdat$N>=15),]

# make actor into an ordered factor, ordering by mean rating:
ratings$actor_1_name <- factor(ratings$actor_1_name)
ratings$actor_1_name <- reorder(ratings$actor_1_name, ratings$M)

ggplot(ratings, aes(x = M, xmin = M-SE, xmax = M+SE, y = actor_1_name )) +
  geom_point() + 
  geom_segment( aes(x = M-SE, xend = M+SE,
              y = actor_1_name, yend=actor_1_name)) +
  theme(axis.text=element_text(size=8)) +
  xlab("Mean rating") + ylab("First Actor") 


# then we use plyr to calculate the mean rating and SE for each director
ratingdat <- ddply(df, c("director_name"), summarise,
                   M = mean(imdb_score, na.rm=T),
                   SE = sd(imdb_score, na.rm=T)/sqrt(length(na.omit(imdb_score))),
                   N = length(na.omit(imdb_score)))
ratings<-ratingdat[which(ratingdat$N>=10 & !(ratingdat$director_name=='')),]

# make director into an ordered factor, ordering by mean rating:
ratings$director_name <- factor(ratings$director_name)
ratings$director_name <- reorder(ratings$director_name, ratings$M)

ggplot(ratings, aes(x = M, xmin = M-SE, xmax = M+SE, y = director_name )) +
  geom_point() + 
  geom_segment( aes(x = M-SE, xend = M+SE,
                    y = director_name, yend=director_name)) +
  theme(axis.text=element_text(size=8)) +
  xlab("Mean rating") + ylab("Director") 