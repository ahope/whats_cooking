
data_folder = '~/Documents/data/kaggle/whats-cooking/'

histo_colors = c('darkorange1', 
                 'darkorchid', 
                 'deepskyblue1', 
                 'gold1', 
                 'deeppink1', 
                 'yellowgreen', 
                 'cadetblue', 
                 'maroon1', 
                 'navy', 
                 'tan4', 
                 'black')

library(gdata)
library(ggplot2)
library(plyr)
library(dplyr)
library(class)
library(gmodels)
library(jsonlite)
library(tm)
library(tau)

## Load all training data
df <- fromJSON(paste0(data_folder, 'train.json'))

## Some exploration

### Plot cuisine freqs
qplot(df$cuisine)

#### 40k recipes
nrow(df)

str(df)

table(df$cuisine)
class(df$ingredients)



recipe.corpus <- VCorpus(VectorSource(df$ingredients))

## Allows each ingredient to be a phrase, and not chunked into separate words.
tokenize_phrase <- function(x) return (x)
recipe.dtm <- DocumentTermMatrix(recipe.corpus, control = list(tokenize=tokenize_phrase))
## Want to explore some text characteristics
length(findFreqTerms(recipe.dtm))
# 39774 recipes, 6701 ingredients
foo <- as.data.frame(inspect(recipe.dtm))
foo$id <- df$id
foo$cuisine <- df$cuisine

## A reasonable first pass seems to be what does knn do? 

## Split my data into test/train
set.seed(123)
train_inds <- sample(1:nrow(df), nrow(df)*0.5, replace = FALSE)

train <- foo[train_inds,]
test <- foo[-train_inds,]

knn.out <- knn(train[,!names(train) %in% c('id', 'cuisine')], 
               test[1:1000, !names(test) %in% c('id', 'cuisine')], 
               k=5, 
               cl = train$cuisine)


small.test <- test[1:1000, c('id', 'cuisine')]
small.test$pred <- knn.out
small.test$correct <- small.test$cuisine == small.test$pred
ggplot(small.test, aes(x=cuisine, y = pred, fill = cuisine)) + 
  geom_jitter() + 
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))

table(small.test$cuisine)


# 7838 recipes
italian.corpus <- VCorpus(VectorSource(df[df$cuisine == 'italian', 'ingredients']))
italian.dtm <- DocumentTermMatrix(italian.corpus, control = list(tokenize=tokenize_phrase))

# What are the most common italian terms? 
findFreqTerms(italian.dtm, lowfreq=780)

# 7838 rows, 2927 cols (each col is a unique ingredient)
foo <- as.data.frame(inspect(italian.dtm))




dtm <- DocumentTermMatrix(recipe.corpus, control = list(tokenize=tokenize_phrase))
findFreqTerms(dtm, 15)

## Pass in "dictionary" to the control list to restrict the DTM. 
# inspect(DocumentTermMatrix(reuters,
#                            + list(dictionary = c("prices", "crude", "oil"))))

small.df <- df[1:10, ]
small.corpus <- VCorpus(VectorSource(df[1:10, 'ingredients']))
small.dtm <- DocumentTermMatrix(small.corpus, control = list(tokenize = tokenize_phrase))

z <- as.data.frame(inspect(small.dtm))

# quick hack to make keywords - which got stuck in row.names - into a variable
#z$docs = rownames(z)
z$doc.id = small.df$id
#z$cuisine = small.df$cuisine

# "melt" the data frame ; ?melt at R console for info
z.melted = melt(z, id = 'doc.id')

# not necessary, but I like decent column names
colnames(z.melted) = c("doc","ingredient","Freq")
z.full <- merge(z.melted, small.df, by.x = 'doc.id', by.y = 'id')
z.full$doc.id <- as.factor(z.full$doc.id)

zplot <- ggplot(z.full, aes(x=doc.id, y = variable, fill = value)) + geom_tile()


ggplot(data = unique(action_log), aes(x = studyDay, y = pptId, 
                                      fill = actionName, 
                                      color = actionName)) + 
  scale_color_manual( values = histo_colors) +
  scale_x_continuous(breaks = seq(0, 29), 
                     labels= minToDayLabels) +
  #geom_jitter(binwidth = 60 ) + 
  geom_vline(xintercept = 0) + 
  geom_vline(xintercept =  7) + 
  geom_vline(xintercept =  14) + 
  geom_vline(xintercept =  21) + 
  theme(axis.text.x = element_text(angle=90, vjust = 1, hjust = 1), 
        axis.text.y = element_blank()) +
  geom_point() + 
  facet_grid(actionName ~ .) + 
  ggtitle('All Ppts, All Actions')


print(PlotDocTerms(df[df$cuisine == 'indian',]))
indian.corpus <- VCorpus(VectorSource(df[df$cuisine == 'indian', 'ingredients']))
indian.dtm <- DocumentTermMatrix(indian.corpus, control = list(tokenize = tokenize_phrase))

findFreqTerms(indian.dtm, 300)
inspect(indian.corpus)

PlotDocTerms <- function(df){
  corpus <- VCorpus(VectorSource(df[, 'ingredients']))
  dtm <- DocumentTermMatrix(corpus, control = list(tokenize = tokenize_phrase))
  
  z <- as.data.frame(inspect(dtm))
  
  # quick hack to make keywords - which got stuck in row.names - into a variable
  #z$docs = rownames(z)
  z$doc.id = df$id
  #z$cuisine = small.df$cuisine
  
  # "melt" the data frame ; ?melt at R console for info
  z.melted = melt(z, id = 'doc.id')
  
  # not necessary, but I like decent column names
  #colnames(z.melted) = c("doc","ingredient","Freq")
  #z.full <- merge(z.melted, small.df, by.x = 'doc.id', by.y = 'id')
  z.melted$doc.id <- as.factor(z.melted$doc.id)
  
  zplot <- ggplot(z.melted, aes(x=doc.id, y = variable, fill = value)) + geom_tile()
  return(zplot)
}

small.tdm <- TermDocumentMatrix(small.corpus, control = list(tokenize = tokenize_phrase))
tdm.df <- as.data.frame(inspect(small.tdm))
