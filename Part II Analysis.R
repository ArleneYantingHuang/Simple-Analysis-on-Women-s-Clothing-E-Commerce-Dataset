library(readr)
library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggcorrplot)
library(reshape2)
library(tidytext)
library(wordcloud)

data0 = read_csv('/Users/arlenehuang/OneDrive - Columbia University/Spring 2020/APAN 5205/5205 case/deliverable 1/rawdata.csv')
data0 = data0[-1]
colnames(data0) = c('C_ID', 'Age', 'Title', 'Review', 'Rating', 
                    'Recommend', 'PostiveFB', 'Division', 'Department', 'Class')
#View(data0)
str(data0)
summary(data0)

# Data Cleaning Part
# Check missing value 
sapply(data0, function(x) sum(is.na(x)))

# Delete tuples without reviews, division, dept, class
data = data0 %>% 
  filter(!is.na(Review)) %>% 
  filter(!is.na(Division)) %>% 
  filter(!is.na(Department)) %>% 
  filter(!is.na(Class))
 sapply(data, function(x) sum(is.na(x)))

# Combine the text of review & title for the further text mining
notitle = data %>% filter(is.na(Title)) %>% select(-Title)
data = data %>% 
  filter(!is.na(Title)) %>% 
  unite(Review, c(Title, Review), sep = ' ') %>%
  bind_rows(notitle)
sapply(data, function(x) sum(is.na(x)))

# Data Type Transformation
data <- data %>% 
  mutate(Department = factor(Department), Division = factor(Division), Class = factor(Class))
str(data)

# Export cleaned data
# write.csv(data, file = "~/Desktop/Cleaned_data.csv")

# Exploration among variables
# correlationship among recommended ind, rating and existing of title, length of reviews, Division, Department, Class 
temp0 = data %>% mutate(Length_of_Review = nchar(Review), 
                        Div = as.numeric(Division), 
                        Dept = as.numeric(Department), 
                        C_Class = as.numeric(Class)) 
temp0 = temp0[,c(2,4:6,10:13)]

# Correlation matrix
correlation_matrix = cor(temp0)
ggcorrplot(correlation_matrix,method = 'square',type = 'lower',
           lab = T,lab_size = 2.5,title = "Correlation Matrix")

# Rating Count  
data%>%
  group_by(Rating) %>%
  summarize(n = n()) %>%
  mutate(n) %>%
  ggplot(aes(Rating, n))+
  geom_bar(stat = 'identity',width = 0.5, fill = 'lightblue')+
  labs(x= 'Rating Score', y='Number of Rating', 
       title= 'Rating Distribution') +
  geom_text(aes(label=round(n,2)), vjust=-0.2) 

# Departemet
# Percentage of reviews group by dept
data%>%
  group_by(Department) %>%
  summarize(n = n()) %>%
  ggplot(aes(x='',y = n, fill = factor(Department))) + 
  geom_bar(stat = 'identity',width = 0.5)+
  geom_text(aes(label = paste(round(n / sum(n) * 100, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "Department",
       x = NULL,
       y = NULL,
       title = "Pie Chart") + 
  coord_polar("y") + 
  scale_fill_brewer(palette="Blues")+
  theme_minimal()
# tops, dresses, bottoms

# Rating scores group by department
data %>% 
  mutate(Rating = factor(Rating)) %>% 
  group_by(Department) %>% 
  count(Rating) %>% 
  data.frame %>% 
  ggplot(aes(Department, n)) +
  geom_bar(aes(fill = Rating), stat="identity", 
           width = 0.4, position = position_dodge(width=0.5)) +  
  labs(x = 'Department', y ='Number of Ratings', 
       title= 'Rating Distribution for Each Department') +
  theme(legend.position = "bottom")

# Rating Distribution for Each Department
data %>% 
  mutate(Rating = factor(Rating)) %>% 
  group_by(Department) %>% 
  count(Rating) %>% 
  data.frame %>% 
  ggplot(aes(Department, n, fill = Rating)) +
  geom_bar(stat="identity", position="fill", 
           width = 0.4) +
  labs(x = 'Department', y ='Number of Ratings', 
       title= 'Rating Distribution for Each Department') +
  theme(legend.position = "bottom")

# PositiveFB
# Total number of positiveFB by Ratings
data %>% 
  mutate(Rating = factor(Rating)) %>% 
  group_by(Rating) %>%
  summarise(n = sum(PostiveFB)) %>%
  data.frame %>%
  ggplot(aes(Rating, n)) +
  geom_bar(aes(fill = Rating), stat="identity", 
           width = 0.4, position = position_dodge(width=0.5)) +
  labs(x = 'Rating', y ='Numbers of customers found the review positive', 
       title= 'Total number of positiveFB by Ratings') +
  theme(legend.position = "bottom")

# Percentage of positiveFB by Ratings
data %>% 
  mutate(Rating = factor(Rating)) %>% 
  group_by(Rating) %>%
  summarise(n = (sum(PostiveFB>=1)/sum(PostiveFB))) %>%
  data.frame %>%
  ggplot(aes(Rating, n)) +
  geom_bar(aes(fill = Rating), stat="identity", 
           width = 0.4, position = position_dodge(width=0.5)) +
  labs(x = 'Rating', y ='Percentage of customers found the review positive', 
       title= 'Percentage of positiveFB by Ratings') +
  theme(legend.position = "bottom") +
  scale_y_continuous(limits = c(0,0.25))

# Age analysis
# Age density 
ggplot(data,aes(x = Age)) + 
  geom_density(size = 1, fill = 'lightblue') + 
  ggtitle('Age Distribution')

# Department by age group    
# (remove the dept named trend since it just has a few data and the categories insides are mixed and can be included in other departments)
temp1 <- data %>% 
  filter(Department != 'Trend') %>% 
  select(C_ID, Age, Department, Class) %>% 
  mutate(AgeGrp = ifelse(Age < 25, '18-24', ifelse(Age < 35, '25-34', ifelse(Age < 45, '35-44', ifelse(Age < 55, '45-54', ifelse(Age < 70, '54-69', ifelse(Age < 100, '70+' ))))))) %>% 
  mutate(AgeGrp = factor(AgeGrp)) 

temp1 %>% group_by(AgeGrp) %>% 
  count(Department) %>% 
  ggplot(aes(Department, n, fill = AgeGrp)) + 
  geom_bar(stat = 'identity') + 
  facet_wrap(~AgeGrp) + 
  labs(x = 'Department', y = 'Number of Reviews',
       title = 'Department Preference for Each Age Group') + 
  geom_text(aes(label = n), hjust = .5) 

# Class by age group
temp1 %>% group_by(AgeGrp) %>% 
  count(Class) %>% 
  ggplot(aes(x = AgeGrp, y = n, fill = Class)) +
  geom_bar(stat = 'identity', width = 0.5, position = "fill") +
  labs(x = 'Age Groups', y = 'Number of Reviews',
       title = 'Reviews for Each Classes by Age Groups') +
  coord_flip()

# Popularity analysis 
# Top 10 popular clothes
temp2 = data %>% 
  mutate(id = factor(C_ID)) %>% 
  group_by(id) %>% 
  summarise(Num =n()) %>%
  arrange(desc(Num)) %>%
  ungroup() %>% 
  head(10)

temp2 %>% ggplot(aes(x = reorder(id, Num), y = Num)) +
  geom_bar(stat='identity', width = 0.5, fill = 'lightblue') +
  labs(x = 'Clothing ID', y = 'Count Number', 
       title = 'Top 10 Popular Clothes') +
  geom_text(aes(label = Num), vjust=-0.2) +
  coord_flip()

temp3 = merge(temp2, data, by.x = 'id', by.y = 'C_ID' )
ggplot(temp3, aes(factor(id), Age))+
  geom_boxplot()
ggplot(temp3, aes(factor(id), Rating))+
  geom_boxplot()

#Text Handling 
# mean word length of reviews
mean(str_count(string = data$ Review, pattern = '\\S+'))
#63.11848

# tokenization
data %>%
  unnest_tokens(input = Review,output = word) %>%
  select(word)%>%
  group_by(word)%>%
  summarize(count=n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(10)
# the, i, and.... all meaningless words are the most common words

# remove stop words & Visualize the top 10 common words
tokened = data %>%
  unnest_tokens(input = Review,output = word) %>%
  select(word) %>%
  anti_join(stop_words) %>%
  group_by(word) %>%
  count(word, sort = TRUE)%>%
  ungroup() %>%
  data.frame()

tokened %>%
  head(10) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_bar(stat = 'identity', width = 0.5, fill = 'lightblue') +
  labs(x = 'Words', y = 'Word Count', title = 'Most Used Words in Reviews') +
  geom_text(aes(label = n), hjust = -.3) +
  coord_flip()

# word Cloud Visulization 
set.seed(200)
wordcloud(words = tokened$word, tokened$n, scale=c(2,0.5),
          max.words = 100, colors = brewer.pal(9,"Spectral"))

# sentiment analysis
data %>%
  select(Review,Rating)%>%
  unnest_tokens(output=word,input=Review)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(Rating,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))%>%
  ggplot(aes(x=Rating,y=proportion,fill=sentiment))+
  geom_col()+
  theme_economist()+
  coord_flip()

wordcloudData = 
  data%>%
  unnest_tokens(output=word,input=Review)%>%
  anti_join(stop_words)%>%
  inner_join(get_sentiments('bing'))%>%
  count(sentiment,word,sort=T)%>%
  spread(key=sentiment,value = n,fill=0)%>%
  data.frame()

rownames(wordcloudData) = wordcloudData[,'word']
wordcloudData = wordcloudData[,c('positive','negative')]

set.seed(610)
comparison.cloud(term.matrix = wordcloudData,scale = c(2,0.5),max.words = 200, rot.per=0)

# Terms for different ratings
# bigram token (since hard to find single word frequency differences among ratings but sentimental differences indeed exist)
bigramming = function(data){
  bigram0 = data %>%
    unnest_tokens(bigram, Review, token = 'ngrams', n = 2) %>% 
    separate(bigram, c('first', 'second'), sep = ' ') %>% 
    filter(!first %in% stop_words$word, !second %in% stop_words$word, 
           !str_detect(first, '\\d'), !str_detect(second, '\\d')) %>% 
    unite(bigram, c(first, second), sep = ' ') 
  return(bigram0)
}

bigramming(data) %>% 
  mutate(Rating = factor(Rating)) %>% 
  group_by(Rating) %>% 
  count(bigram, sort=TRUE) %>% 
  top_n(8, n) %>% 
  ungroup() %>% 
  ggplot(aes(bigram, n, fill = Rating)) + 
  geom_col() + 
  labs(x=NULL, y = 'frequency', title = 'Top 8 Bigram Terms for Each Rating') + 
  coord_flip() +
  facet_wrap(~Rating, scales = 'free') 
# can find main features for clothes in each rating, especially the issues for lower ratings

# bigrams by department
data %>% 
  filter(Department != 'Trend') %>% 
  bigramming() %>% 
  group_by(Department) %>% 
  count(bigram, sort=TRUE) %>% 
  top_n(8, n) %>% 
  ungroup() %>% 
  ggplot(aes(bigram, n, fill = Department)) + 
  geom_col() + 
  labs(x=NULL, y = 'frequency', title = 'Top 8 Bigram Terms for Each Department') + 
  coord_flip() +
  facet_wrap(~Department, scales = 'free') 

# bigrams by recommend
bigramming(data) %>% 
  group_by(Recommend) %>% 
  count(bigram, sort=TRUE) %>% 
  top_n(5, n) %>% 
  ungroup() %>% 
  ggplot(aes(bigram, n, fill = factor(Recommend))) + 
  geom_col() + 
  labs(x=NULL, y = 'frequency', title = 'Top 5 Bigram Terms by Recommend 0/1') + 
  coord_flip() +
  facet_wrap(~Recommend, scales = 'free') 
# not recommend reasons may be: (cute but) poor quality, body type, arm holes

# For lower ratings
low_rating_term = bigramming(data[data$Rating<=3,]) %>% 
  count(bigram, sort = TRUE)

low_rating_term %>% 
  head(20) %>%
  ggplot(aes(reorder(bigram, n), n)) +
  geom_bar(stat = 'identity', width = 0.5, fill = 'lightblue') +
  labs(x = 'Words', y = 'Word Count', title = 'Top 30 Words in Reviews with Low Rating (1-3)') +
  geom_text(aes(label = n), hjust = -.3) +
  coord_flip()

set.seed(100)
wordcloud(words = low_rating_term$bigram, low_rating_term$n, scale=c(2,0.5),
          max.words = 100, colors = brewer.pal(9,"Spectral"))
# main problems: quality, arm holes, type, fit, fabric, ...

# proption of upper case letter in review 
prop_upper = str_count(data$Review,pattern = '[A-Z]')/nchar(data$Review)
cor(prop_upper,data$Rating)

# corpus 
library(tm)
#create a corpus
corpus = Corpus(VectorSource(data$Review))

# lowercase
corpus = tm_map(corpus,FUN = content_transformer(tolower))

# remove punctuation
corpus = tm_map(corpus,FUN = removePunctuation)

# remove stopwords
corpus = tm_map(corpus,FUN = removeWords,c(tm::stopwords('english')))
corpus[[111]][1]

# remove whitespace
corpus = tm_map(corpus,FUN = stripWhitespace)
corpus[[111]][1]

# create a dictionary
dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(data$Review))),lowfreq=0)
dict_corpus = Corpus(VectorSource(dict))

# stem document
corpus = tm_map(corpus,FUN = stemDocument)
corpus[[111]][1]

# create document term matrix
dtm = DocumentTermMatrix(corpus)
dtm
dim(dtm)

# tdm and word cluster (to find the word groups used together)
tdm = TermDocumentMatrix(corpus)
tdm
tdm2 = removeSparseTerms(tdm, sparse = 0.95)
tdm2
hc <- hclust(d = dist(tdm2, method = "euclidean"), method = "complete")
plot(hc)

# remove sparse Terms (keep terms that appear in at least 5% of ducument)
xdtm = removeSparseTerms(dtm,sparse = 0.95)
xdtm

# complete stems with the most frequent match
xdtm = as.data.frame(as.matrix(xdtm))
colnames(xdtm) =stemCompletion(x= colnames(xdtm),
                               dictionary = dict_corpus,
                               type = 'prevalent')
colnames(xdtm) = make.names(colnames(xdtm))
sort(colSums(xdtm),decreasing =T)

# document term - inverse document frequency weighting
dtm_tfidf = DocumentTermMatrix(x=corpus,
                               control = list(weighting=function(x) weightTfIdf(x,normalize = F)))
xdtm_tfidf = removeSparseTerms(dtm,sparse = 0.95)
xdtm_tfidf = as.data.frame(as.matrix(xdtm_tfidf))

colnames(xdtm_tfidf) =stemCompletion(x= colnames(xdtm_tfidf),
                                     dictionary = dict_corpus,
                                     type = 'prevalent')
colnames(xdtm_tfidf) = make.names(colnames(xdtm_tfidf))
sort(colSums(xdtm_tfidf),decreasing =T)

# visualize top 20  ty and inverse documeny frequency
library(ggthemes)
data.frame(term=colnames(xdtm),tf=colMeans(xdtm), tfidf=colMeans(xdtm_tfidf))%>%
  arrange(desc(tf))%>%
  top_n(20)%>%
  gather(key = weighting_method,value = weight,2:3)%>%
  ggplot(aes(x = term,y = weight,fill = weighting_method))+
  geom_col(position = 'dodge')+
  coord_flip()+
  theme_economist()

# add rating to dataframe
colthing = cbind(Rating = data$Rating,xdtm)
colthing_tfidf = cbind(Rating = data$Rating,xdtm_tfidf)

# frequently used word in review of rating 5
sort(colSums(colthing[colthing$Rating==5,-colthing$Rating]),decreasing = T)

sort(colSums(colthing_tfidf[colthing_tfidf$Rating==5,-colthing_tfidf$Rating]),decreasing = T)

# predictive model 1 for rating
set.seed(14367)
split = sample(1:nrow(colthing),size =0.8*nrow(colthing))
train = colthing[split,]
test = colthing[-split,]

library(rpart);library(rpart.plot)
set.seed(43621)
tree = rpart(Rating~.,train)
rpart.plot(tree)
pred_tree = predict(tree,newdata=test)
rmse_tree = sqrt(mean((pred_tree - test$Rating)^2));rmse_tree

# predictive model 2 for rating
set.seed(657868)
split = sample(1:nrow(colthing_tfidf),size =0.8*nrow(colthing_tfidf))
train2 = colthing_tfidf[split,]
test2 = colthing_tfidf[-split,]

set.seed(89356)
tree2 = rpart(Rating~.,train2)
rpart.plot(tree2)
pred_tree2 = predict(tree2,newdata=test2)
rmse_tree2 = sqrt(mean((pred_tree2 - test2$Rating)^2));rmse_tree2

# check predictive power of Rating for Recommendation
set.seed(11)
split = sample(1:nrow(colthing),size =0.8*nrow(colthing))
train3 = data[split,]
test3 = data[-split,]

set.seed(88)
tree3 = rpart(Recommend~ Rating + Age + Class + PostiveFB, train3)
rpart.plot(tree3)
pred_tree3 = predict(tree3, newdata=test3)
rmse_tree3 = sqrt(mean((pred_tree3 - test2$Recommend)^2)); rmse_tree3

# model 2
library(randomForest)
set.seed(88)
rf = randomForest(as.factor(Recommend)~ Rating + Age + Class, train3)
rf




