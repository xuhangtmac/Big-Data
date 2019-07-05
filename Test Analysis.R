library(tidyverse)
library(tm)
library(igraph)
library(ggraph)
library(ggthemes)
library(wordcloud)
library(textcat)

rev.df <- read.csv("~/Desktop/reviews.csv", encoding="latin-1", stringsAsFactors=FALSE)
set.seed(123)
rev <- rev.df[sample(c(1:dim(rev.df)[1]), 1400),]
rev$language<-textcat(rev[,6])
rev<-subset(rev,language=="english")
review <- as.character(rev$comments)
head(data.frame(review))
review_source<-VectorSource(review)
review_corpus<-VCorpus(review_source)


head(review_corpus)
# Alter the function code to match the instructions
# Get Customized Stopwords
exceptions <- grep(pattern = "not|n't", x = stopwords(), value = TRUE)
my_stopwords <- setdiff(stopwords("en"), exceptions)

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, 
                   c(my_stopwords, "boston","stay","place","really","everything","home","definitely","also","just","made","back","get","one"))
  return(corpus)
}
clean_corp<-clean_corpus(review_corpus)

rev_df_new<-data.frame(text = sapply(clean_corp, as.character), stringsAsFactors = FALSE)
rev_df_new$document<-c(1:nrow(rev_df_new))

# Create the tdm from the corpus: review_tdm
review_tdm <- TermDocumentMatrix(clean_corp)
# Print out review_tdm data
print(review_tdm)
# Convert coffee_dtm to a matrix: review_m
review_m <- as.matrix(review_tdm)
# Print the dimensions of review_m
dim(review_m)
# Review a portion of the matrix
review_m[100:102, 100:102]

# Calculate the rowSums: term_frequency
term_frequency <- rowSums(review_m)
# Sort term_frequency in descending order
term_frequency <- sort(term_frequency,decreasing=TRUE)
# View the top 20 most common words
head(term_frequency,20)
# Plot a barchart of the 10 most common words
barplot(term_frequency[1:20], col = "tan",las=2)

# WordCloud
word_freqs <- data.frame(term = names(term_frequency),num = term_frequency)
wordcloud(word_freqs$term,word_freqs$num, max.words=100,colors="red")

# Bigrams
library(scales)
library(tidytext)
stop_words<- c(my_stopwords, "boston","stay","place","really","everything","home","definitely","also","just","made","back","get","one")
bigrams <- unnest_tokens(rev, bigram, review, token="ngrams", n=2)
bigrams %>%
  count(bigram, sort = TRUE)
bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words) %>%
  filter(!word2 %in% stop_words)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_counts
bigram_graph <- bigram_counts %>%
  filter(n > 15000) %>%
  graph_from_data_frame()
bigram_graph

# Plot bigrams
set.seed(1995)
a <- grid::arrow(type = "closed", length = unit(.05, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.05, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# Word Network
library(igraph)  #load the igraph package
library(micEcon)
associations <- findAssocs(review_tdm,"location",0.1)
g1<-data.frame(associations$location)
colnames(g1)<-"distance"
g2<-data.frame(0)
rownames(g2)<-"location"
colnames(g2)<-"distance"
g3<-data.frame(rbind(g2,g1))
g3[,2]<-g3[,1]
g3[,3]<-g3[,2]
g3[,3]<-1/g3[,3]
g3[1,3]<-0
g3[,1]<-"location"
g3[,2]<-rownames(g3)
rownames(g3)<-c(1:117)
g4<-graph_from_data_frame(g3, directed = F, vertices = g3[,2])
coords <- layout.fruchterman.reingold(g4, niter=5000)
plot(g4, layout = coords, vertex.label.font=2, vertex.size=13, vertex.label.cex=0.65)


# Bin
library(tidytext)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotrix)
rev_tidy<-tidy(review_tdm)
bing <- get_sentiments("bing")
head(bing)
moby_sents <- inner_join(rev_tidy, bing, by = c("term" = "word"))
head(moby_sents)
moby_tidy_sentiment <- moby_sents %>% 
  count(document, sentiment, wt = count) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative)
head(moby_tidy_sentiment)

moby_tidy_small <- moby_tidy_sentiment %>% 
  filter(polarity >= 15 |polarity <=0 ) 
  
moby_tidy_pol <- moby_tidy_small %>% 
  mutate(
    pol = ifelse(polarity>0, "positive", "negative"))
head(moby_tidy_pol)


pos<-subset(moby_tidy_pol,moby_tidy_pol$pol=="positive")

neg<-subset(moby_tidy_pol,pol=="negative")

pos<-rev_df_new[rev_df_new$document %in% pos$document,]
neg<-rev_df_new[rev_df_new$document %in% neg$document,]

all_pos <- paste(pos$text, collapse = " ")
all_neg <- paste(neg$text, collapse = " ")
all <- c(all_pos, all_neg)

all_source <- VectorSource(all)
all_corpus <- VCorpus(all_source)
all_tdm <- TermDocumentMatrix(all_corpus)
colnames(all_tdm) <- c("positive review", "negative review")
all_m <- as.matrix(all_tdm)
all_m
common_words <- subset(all_m, all_m[, 1] > 0 & all_m[, 2] > 0)
difference <- abs(common_words[, 1] - common_words[, 2])
common_words <- cbind(common_words, difference)
common_words <- common_words[order(common_words[, 3], decreasing = TRUE), ]

top25_df <- data.frame(x = common_words[1:25, 2], 
                       y = common_words[1:25, 1], 
                       labels = rownames(common_words[1:25, ]))

# Create the pyramid plot
pyramid.plot(top25_df$x, top25_df$y, labels = top25_df$labels, 
             gap = 8, top.labels = c("Negative Review", "Words", "Positive Review"), 
             main = "Words in Common", laxlab = NULL, 
             raxlab = NULL, unit = NULL)


#FINN
afinn <- get_sentiments("afinn")
rev_afinn <- rev_tidy %>% 
  # Inner Join to AFINN lexicon
  inner_join(afinn, by = c("term" = "word"))%>% 
  # Count by score and term
  count(score, document)
head(rev_afinn)

rev_afinn$t_score<-rev_afinn$score*rev_afinn$n
rev_afinn[,1]<-NULL
rev_afinn[,2]<-NULL
rev_afinn_agg<-aggregate(rev_afinn$t_score, by=list(rev_afinn$document), FUN=sum)
colnames(rev_afinn_agg)<-c("document","total_score")
head(rev_afinn_agg)

moby_tidy_small2 <- rev_afinn_agg %>% 
  filter(total_score >= 30 |total_score <=0 ) 

moby_tidy_pol2 <- moby_tidy_small2 %>% 
  mutate(
    pol = ifelse(total_score>0, "positive", "negative"))
head(moby_tidy_pol2)


pos2<-subset(moby_tidy_pol2,pol=="positive")

neg2<-subset(moby_tidy_pol2,pol=="negative")

pos2<-rev_df_new[rev_df_new$document %in% pos2$document,]
neg2<-rev_df_new[rev_df_new$document %in% neg2$document,]

all_pos2 <- paste(pos2$text, collapse = " ")
all_neg2 <- paste(neg2$text, collapse = " ")
all2 <- c(all_pos2, all_neg2)

all_source2 <- VectorSource(all2)
all_corpus2 <- VCorpus(all_source2)
all_tdm2 <- TermDocumentMatrix(all_corpus2)
colnames(all_tdm2) <- c("positive review", "negative review")
all_m2 <- as.matrix(all_tdm2)
common_words2 <- subset(all_m2, all_m2[, 1] > 0 & all_m2[, 2] > 0)
difference2 <- abs(common_words2[, 1] - common_words2[, 2])
common_words2 <- cbind(common_words2, difference2)
common_words2 <- common_words2[order(common_words2[, 3], decreasing = TRUE), ]

top25_df2 <- data.frame(x = common_words2[1:25, 2], 
                       y = common_words2[1:25, 1], 
                       labels = rownames(common_words2[1:25, ]))

# Create the pyramid plot
pyramid.plot(top25_df2$x, top25_df2$y, labels = top25_df2$labels, 
             gap = 8, top.labels = c("Negative Review", "Words", "Positive Review"), 
             main = "Words in Common", laxlab = NULL, 
             raxlab = NULL, unit = NULL)



#Comparison Cloud
library(wordcloud)
comparison.cloud(all_m,rot.per=0.1,
  max.words = 100,
  colors = c("darkgreen", "darkred")
)
