library(readr); library(rvest); library(dplyr); library(tidyr);library(stringr)
library(ggplot2); library(ggcorrplot); library(reshape2)
library(tidytext); library(wordcloud)

data0 = read_csv('/Users/zhanglanzhen/Desktop/rawdata.csv')
data0 = data0[-1]
colnames(data0) = c('C_ID', 'Age', 'Title', 'Review', 'Rating', 
                    'Recommend', 'PostiveFB', 'Division', 'Department', 'Class')
View(data0)
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
View(data)

# Export cleaned data
write.csv(data, file = "~/Desktop/Cleaned_data.csv")

# Exploration among variables
# correlationship among recommended ind, rating and existing of title, length of reviews, Division, Department, Class 
temp0 = data %>% mutate(Length_of_Review = nchar(Review), 
         Div = as.numeric(Division), 
         Dept = as.numeric(Department), 
         C_Class = as.numeric(Class)) 
temp0 = temp0[,c(2,5:7,12:15)]

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
  mutate(pct = n/sum(n)) %>%
  ggplot(aes(Department, pct)) +
  geom_bar(stat = 'identity',width = 0.5, fill = 'lightblue')+
  labs(x= 'Department', y='Percentage of Reviews (%)', 
      title= 'Department Distribution') +
  geom_text(aes(label=round(pct*100,2)), vjust=-0.2) 

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
  geom_bar(stat = 'identity', width = 0.5) +
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


#Text Part 

# mean word length of reviews
mean(str_count(string = data$ Review, pattern = '\\S+'))

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

# Remove stop words & Visualize the top 10 common words
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

# Word Cloud Visulization 
set.seed(200)
wordcloud(words = tokened$word, tokened$n, scale=c(2,0.5),
          max.words = 200, colors = brewer.pal(9,"Spectral"))

# proption of upper case letter in review 
prop_upper = str_count(data$Review,pattern = '[A-Z]')/nchar(data$Review)
cor(prop_upper,data$Rating)

# corpus 
library(tm)
#create a corpus
corpus = Corpus(VectorSource(data$Review))

# lowercase
corpus = tm_map(corpus,FUN = content_transformer(tolower))
corpus[[111]][1]

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
