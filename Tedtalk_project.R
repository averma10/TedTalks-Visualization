#-------1a---------------
library(tidyverse)
library(anytime)
library(reshape)
library(colorspace)
library(gplots)       #Used for heatmap.2
library(tm)           #Used for wordcloud
library(wordcloud)    #Used for wordcloud
library(SnowballC)    #Used for wordcloud
library(igraph)       #For network plots
library(streamgraph)  #for ThemeRiver/Streamgraph Viz

#------1b--------------
#set working directory path and load data
path <- '/Users/Gurpreet/Documents/Coding practices/Viz (R)'
setwd(path)

#load data
ted_o <- read.csv('ted_main.csv',header=T) #keep original copy as it is
#View(ted_o)

#-----1c-----------
#data cleaning

#Convert film_date and published_date from UNIX date format to normal date format
ted_o$film_date <- anydate(ted_o$film_date)
ted_o$published_date <- anydate(ted_o$published_date)
ted_o$talk_id <- 1:dim(ted_o)[1]

#select only the useful columns
ted <- ted_o[c("talk_id","title","main_speaker","duration","film_date",
               "published_date","speaker_occupation","comments",
               "views","ratings","related_talks","tags","url","languages")]

#write.csv(ted,'ted_talk_clean.csv',row.names = F) #To create ratings table using python

ratings <- read.csv('rating.csv',header = T)
head(ratings)

ted_final <- merge(ted,ratings)
#write.csv(ted_final,'Ted_talk_final.csv')

#create custom theme for plots
mytheme <- theme_minimal() + 
          theme(axis.line = element_line(color = 'darkgray',size = 0.5),axis.ticks = element_line())

#-------2---
#Month vs Freq of talks as barchart
#create new df
ted_2 <- data.frame('film_date'=ted$film_date)
ted_2$month <- as.numeric(format(ted_2$film_date,'%m'))

#create labels for months
ted_2$month <- factor(ted_2$month, 
                   levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                   labels=c('Jan','Feb','March','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec') 
                   )

#generate frequency table for months using dplyr library function count()
ted_2.months <- count(ted_2,month)

#create histogram
p1 <- ggplot(data = ted_2.months, aes(x=month,y=n))
p1 <- p1 + geom_histogram(stat = 'identity')
p1 <- p1 + scale_y_continuous('No. of talks',breaks = seq(0,700,by=100))
p1 <- p1 + xlab('Month') + ggtitle('Monthly Distribution of Ted Talks')
p1 <- p1 + mytheme

plot(p1)


#-------3--------
#talks over the years

ted_2$yr <- factor(as.numeric(format(ted_2$film_date,'%Y')))


#create frequency table using dplyr library function count()
ted_2.years <- count(ted_2,yr)


#Create a line/bar plot
p2 <- ggplot(data=ted_2.years,aes(x=yr,y=n)) 
p2 <- p2 + geom_line(aes(group = 'none')) + geom_point(size=2)  #geom_bar(stat='identity')
p2 <- p2 + scale_y_continuous(breaks = seq(0,300,by=50))
p2 <- p2 + labs(x='Year',y='No. of talks', title = 'Yearly Distribution of Ted Talks')
p2 <- p2 + mytheme

plot(p2)


#------4------------
#Create Heat map for number of talks over months and years

ted_2$mon_yr <- format(ted_2$film_date,'%m%Y')
#create frequency table for number of talks per month every year
ted_2.talks <- count(ted_2,mon_yr)

#split mon_yr to create month and year columns

ted_2.talks$month <- factor(as.integer(substring(ted_2.talks$mon_yr,1,2)),
                            levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                            labels=c('January','February','March','April','May','June','July','August','September','October','November','December'),
                            ordered = T
                            )
ted_2.talks$year <- as.integer(substring(ted_2.talks$mon_yr,3))

ted_2.talks <- ted_2.talks[order(ted_2.talks$year,ted_2.talks$month,decreasing = F),]
ted_2.talks <- ted_2.talks[-1]

#create pivot table for month-year using cast() from reshape lib
pivot <- as.matrix(cast(ted_2.talks,formula = month ~ year,fill = 0,value='n'))

color2 <- colorRampPalette(c('white','darkgreen'))(256)
color1 <- colorspace::heat_hcl(12,h=c(20,-100), l= c(100,30), c = c(100,40))#,power=1)

#heatmap to summarize the distribution of talks each month each year

heatmap.2(pivot,scale = 'none',dendrogram ='none' ,Colv = NA,Rowv = NA,
          trace='none',density.info = 'none',denscol = 'white',
          sepcolor = 'white',sepwidth = c(0.05,0.05),
          cellnote = pivot, notecol = 'black',
          key.xlab = 'Talks',key.title = NA,key.ylab = NA,
          key.par = list(mar=c(4,0,7,5.5)),
          lmat=rbind(c(5, 4, 2), c(3, 1, 6)), lhei=c(1.5, 4), lwid=c(1.5, 12, 0.5),
          #lhei = c(2,7),margins = c(5,7), offsetRow = 0.05,offsetCol = 0.05,
          col=color1,
          xlab = 'Year', ylab = 'Month',margins = c(5,6))
          #,main = 'Distribution of Talks each year')

#-------5---------------
#word cloud for most popular topics i.e. tags

tags <- as.character(ted$tags)

tags.doc <- Corpus(VectorSource(tags))
#inspect(tags.doc)

#text cleaning for word cloud
#remove all punctuation marks from text
tags.doc <- tm_map(tags.doc,removePunctuation)
#inspect(tags.doc)
#remove any numbers from text
tags.doc <- tm_map(tags.doc,removeNumbers)
#convert all text to lower case
tags.doc <- tm_map(tags.doc,content_transformer(tolower))
#remove english lang common stopwords
tags.doc <- tm_map(tags.doc,removeWords,stopwords('english'))
#remove extra white spaces
tags.doc <- tm_map(tags.doc,stripWhitespace)

#inspect(tags.doc)

#create a term document matrix
tags.tm <- TermDocumentMatrix(tags.doc)
#inspect(tags.tm)
tags.m <- as.matrix(tags.tm)
tags.v <- sort(rowSums(tags.m),decreasing = T)
tags.df <- data.frame(word = names(tags.v),freq = tags.v)

#head(tags.df,10)
#tail(tags.df)

#generate word cloud

set.seed(1011)
wordcloud(words = tags.df$word,freq = tags.df$freq,min.freq = 200,max.words = 50,
          random.order = F, rot.per=0,colors = 'gray30') #brewer.pal(12,'Set3'))


#------------5a----------
tags.df[which(tags.df$freq >= 200),]

#####This section will not be used in the Project Report####
#-------------6----------------
#word cloud for different decades data to see trend change over the time
ted_wc <- ted[c('film_date','tags')]
ted_wc$year <- as.numeric(format(ted_wc$film_date,'%Y'))
ted_wc$film_date <- NULL

#create function to setup data for word cloud, takes dataframe and returns a dataframe ready to use with wordcloud()
fun.setup <- function(df){
  tags <- as.character(df$tags)
  
  tags.doc <- Corpus(VectorSource(tags))
  #inspect(tags.doc)
  
  #text cleaning for word cloud
  #remove all punctuation marks from text
  tags.doc <- tm_map(tags.doc,removePunctuation)
  #remove any numbers from text
  tags.doc <- tm_map(tags.doc,removeNumbers)
  #convert all text to lower case
  tags.doc <- tm_map(tags.doc,content_transformer(tolower))
  #remove english lang common stopwords
  tags.doc <- tm_map(tags.doc,removeWords,stopwords('english'))
  #remove extra white spaces
  tags.doc <- tm_map(tags.doc,stripWhitespace)
  
  #create a term document matrix
  tags.tm <- TermDocumentMatrix(tags.doc)
  #inspect(tags.tm)
  tags.m <- as.matrix(tags.tm)
  tags.v <- sort(rowSums(tags.m),decreasing = T)
  tags.df <- data.frame(word = names(tags.v),freq = tags.v)
  
  tags.df #return value
} 

#partition the data
v1 <- c(1972:2000)
v2 <- c(2001:2010)
v3 <- c(2011:2017)
#seperate the dataframe based on years
df1 <- ted_wc[which(ted_wc$year %in% v1),]
df2 <- ted_wc[which(ted_wc$year %in% v2),]
df3 <- ted_wc[which(ted_wc$year %in% v3),]

#setup data for above dataframes to make them ready for wordcloud()
tags.df1 <- fun.setup(df1)
tags.df2 <- fun.setup(df2)
tags.df3 <- fun.setup(df3)

#create word cloud
set.seed(1011)
wordcloud(words = tags.df1$word,freq = tags.df1$freq,scale = c(4,.5),min.freq = 1,max.words = 100,
          random.order = F, rot.per=0.35,colors = brewer.pal(8,'Dark2'))

wordcloud(words = tags.df2$word,freq = tags.df2$freq,scale = c(4,.5),min.freq = 1,max.words = 100,
          random.order = F, rot.per=0.35,colors = brewer.pal(8,'Dark2'))

wordcloud(words = tags.df3$word,freq = tags.df3$freq,scale = c(4,.5),min.freq = 2,max.words = 100,
          random.order = F, rot.per=0.35,colors = brewer.pal(8,'Dark2'))

#-------7-----------
#Create ThemeRiver for Popular Tags/Topics

#data setup
df<- ted_final %>%
  select(film_date,tags)
df$year <- as.numeric(format(df$film_date,'%Y'))
#df$film_date <- NULL

#select popular tags from tags.df dataset
tag_list = as.character(head(tags.df[,1],10))
#add tags from tag_list to dataframe as new columns
df[tag_list] <- NA

#fill values in the individual tag columns
for (column in tag_list) {
  df[column] <- 0
  df[column][which(!is.na(str_extract(df$tags,column))),] <-1
}

#create Streamgraph using github::streamgraph library
df %>%
  select(year, technology,science,global,design,issues,health,culture,business,change) %>% 
  tidyr::gather(tags,value,-year) %>%
  group_by(year,tags) %>%
  tally(wt=value) %>%
  ungroup %>%
  streamgraph("tags", "n", "year") %>%
  sg_axis_x(tick_interval =5) %>%
  sg_axis_y(tick_count=12) %>%
  sg_fill_brewer("Spectral") %>%
  sg_annotate(label='Business',x=as.Date('2010-02-05'),y = 20,color='black') %>%
  sg_annotate(label='Change',x=as.Date('2016-02-05'),y = 100,color='black') %>%
  sg_annotate(label='Culture',x=as.Date('2010-02-05'),y = 110,color='black') %>%
  sg_annotate(label='Design',x=as.Date('2009-02-05'),y = 150,color='black') %>%
  sg_annotate(label='Global',x=as.Date('2009-02-05'),y = 210,color='black') %>%
  sg_annotate(label='Health',x=as.Date('2016-02-05'),y = 260,color='black') %>%
  sg_annotate(label='Issues',x=as.Date('2011-02-05'),y = 300,color='black') %>%
  sg_annotate(label='Science',x=as.Date('2010-02-05'),y = 370,color='black') %>%
  sg_annotate(label='Technology',x=as.Date('2010-02-05'),y = 450,color='black') %>%
  #sg_title(title = 'Popular Topics over the Years') %>%  
  sg_legend(show=T, label="Popular Tags: ") 


###########################
###Not Included in Final Report
#---------8-------------

#Create a graph for ted topics
#Topics are nodes and are connected to eachother if they have main_rating = Inspiring and minimum 5000 votes

#filter data
df<- ted_final[ted_final$Inspiring >=5000 & ted_final$main_rating == 'Inspiring',]

#create cartesian product of talk_id
edge_list <- expand.grid(df$talk_id,df$talk_id)

node_list <- df[c("talk_id","title","main_speaker","duration","comments","views","Inspiring","main_rating")]

#create a graphml from edge_list and node_list

gr <- graph_from_data_frame(edge_list,directed=FALSE,vertices = node_list)

summary(gr)
write_graph(gr,'ted_Inspiring_network.graphml',format = 'graphml')