article <- c(
  "apples and oranges and pears and bananas",
  "pineapples and mangos and guavas"
)

str_split(fruits, " and ")

library(tidyverse)
df <- read.csv("author_train.csv")




##################################################################
wordspersentence <- 0
wlength <- 0

for(x in 1:dim(df)[1]){
  
  sentence <- str_replace_all(df$article[x], pattern = '\"', replacement = "") # Remove slashes
  sentence <- str_replace_all(sentence, pattern = '\n', replacement = "") # Remove \n
  sentence <- str(sentence)
  sentence = str_split(sentence,".") #list of split sentences
  df$sentence.count[x] = length(sentence) #how many sentences in the article
  for (y in length(sentence)){
    sentence.words = str_split(sentence[y], " ") #split sentence into words
    wordspersentence = c(wordspersentence, length(sentence.words))
    df$avg_sentencelength[x] = mean(wordspersentence) #average sentence length 
    for (z in sentence.words){
      wordlength = length(sentence.words[z])
      wlength = c(wlength, wordlength)
      df$avg_wordlength[x] = mean(wlength)
    }#end of for loop
  }#end of for loop
}#end of for loop
##################################################################

sentence = strsplit(df$article[3],"a")
as.list(sentence)[2]
df$article




as.list(scan(text=df$article[x], what="."))




sentence = str_split(df$article[1]," ")
sentence


df$article[3]
sentence <- str_replace_all(df$article[3], pattern = '\"', replacement = "") # Remove slashes
sentence <- str_replace_all(sentence, pattern = '\n', replacement = "") # Remove \n
sentence

cnt=0
for (x in length(df)){
  cnt <- cnt+1
  df$sentence.count[x] <- cnt 
}










##################################################################
wordspersentence <- 0
wlength <- 0

for(x in df$article){
  
  sentence <- str_replace_all(x, pattern = '\"', replacement = "") # Remove slashes
  sentence <- str_replace_all(sentence, pattern = '\n', replacement = "") # Remove \n
  sentence <- str(sentence)
  sentence = str_split(sentence,".") #list of split sentences
  df$sentence.count = length(sentence) #how many sentences in the article
  for (y in length(sentence)){
    sentence.words = str_split(sentence[y], " ") #split sentence into words
    wordspersentence = c(wordspersentence, length(sentence.words))
    df$avg_sentencelength = mean(wordspersentence) #average sentence length 
    for (z in sentence.words){
      wordlength = length(sentence.words[z])
      wlength = c(wlength, wordlength)
      df$avg_wordlength = mean(wlength)
    }#end of for loop
  }#end of for loop
}#end of for loop
##################################################################
articles <- c(df$article)
df$sentence.count <- 0
df$word.count <- 0
cnt <- 0
for (x in 1:length(articles)){
  sentence<- str_replace_all(articles[x],pattern='\"',replacement = "")
  sentence<- str_replace_all(sentence,pattern='\n',replacement = "")
  SENTENCE<- strsplit(sentence,".",fixed=TRUE)
  for (y in 1:length(SENTENCE)){
    s[y] <- nchar(strsplit(SENTENCE[y]," ",fixed=TRUE))
    df$avg_sentencelength[x] <- mean(s)
  }#end of for avg sentence length
  WORDS<- str_split(sentence," ")
  df$word.count[x] <- nchar(WORDS)
  
}
articles[3]


sentence<- str_replace_all(articles[3],pattern='\"',replacement = "")
sentence<- str_replace_all(sentence,pattern='\n',replacement = "")
sentence<- strsplit(sentence,".",fixed=TRUE)
sentence<- strsplit(sentence," ",fixed=TRUE)
(sentence)
sapply(sentence,length)
