##### Movie Project
### step 1 : cleanig 
m<- read.csv('movie_metadata.csv')
movie.usa<-m[which(m[,'country']=='USA'),] # choose all usa
movie.df= data.frame(movie.usa)
mm<-movie.df[, -which(names(movie.df)=='movie_imdb_link')] 
# or:
# which(colnames(movie.df)=='movie_imdb_link')
# movies<-movie.df[,-18]

## check for missing values
library(Amelia)
missmap(mm, main = "Missing values vs observed")
sapply(mm,function(x) sum(is.na(x))) 
movie<-na.omit(mm)
sapply(movie,function(x) sum(is.na(x)))

## check for overall correlation
library(psych)
library(car)
library(RColorBrewer) 
library(corrplot)
pairs.panels(movie[c('director_name','duration','facenumber_in_poster','imdb_score','genres')])
# from the plot, only duration and IMBD score has a high correlation.
# face number in posters has a negative correaltion with IMBD score.
# genre has little correlatin with score
# interesting, director name has no correlation with IMDB score

pairs.panels(movie[c('color','actor_1_name','title_year','imdb_score','aspect_ratio','gross')])
# color and title year has highly positive correlation
# color and aspect ratia,gross has smaller positive correlations
# actor 1 namem has very small positive correlation with gross, meaning who plays the movies does not have impact on the gross
# title year and aspect ratio and color are highly positively correlated
# IMDB score has very small positive correlation with actor 1 name ,which means who was the actor 1 does not make the movie has a higher score
# Interestingly, IMDB score has a negative correlation with title year,which means the old movies seems to have a higher score
# IMDB and aspect ratio has  small positive correlation
# IMDB and has a strong positive correlation with gross.

# corplot for all numerical variables
nums<- sapply(movie,is.numeric) # select numeric columns
movie.num<- movie[,nums]
corrplot(cor(movie.num),method='ellipse') 
# note: corrplot cannot use data.frame, use cor() to change it to matrix.

# find the pairs of correlations (x must be numeic)
corr.test(movie.num,y=NULL,use='pairwise',method='pearson',adjust='holm',alpha=0.05)


         
         
         