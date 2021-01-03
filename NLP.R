# Librerias
library(dplyr)
library(tidyr)
library(stopwords)
library(corpus)
library(SentimentAnalysis)

# Datos
tweets = read.csv("financial-tweets.csv")
head(tweets)

# 1. Se eliminan todas las columnas menos "text" y "timestamp" que son las que nos interesan
tweets$id <- NULL
tweets$source <- NULL
tweets$symbols <- NULL
tweets$company_names <- NULL
tweets$url <- NULL
tweets$verified <- NULL

# 2. Se eliminan las filas que contengan datos vacíos y NA
tweets <- filter_all(tweets,any_vars(!(tweets == "")))
tweets <- na.omit(tweets)
dim(tweets)

# 3. Se cambia el formato de la fecha a dd-mm-yyyy
#   3.1. Se separa la columna "timestamp" en varias columnas
tweets <- separate(tweets,timestamp,into = c("WeekDay","Month","Day","Hour","UTC","Year"),sep = "\\s")

#   3.2. Nos quedamos con las columnas "text", "Day", "Month" y "Year"
tweets$WeekDay <- NULL
tweets$Hour <- NULL
tweets$UTC <- NULL

#   3.3. Se eliminan las filas que contienen valores NA
tweets <- na.omit(tweets)

#   3.4. Se escriben los meses en formato numérico mm
tweets$Month[tweets$Month == "Feb"] = "02"
tweets$Month[tweets$Month == "Jul"] = "07"

#   3.5. Se une la fecha con formato dd-mm-yyyy
tweets <- unite(tweets,"date",c("Day","Month","Year"),sep = "-")
tweets$date
unique(tweets$date)

# 4. Se limpia el texto de los tweets
#  4.1. Se transforma el texto a minúsculas
tweets_txt <- tweets
tweets_txt$text <- tolower(tweets_txt$text)

#  4.2. Se eliminan las palabras con @, las urls y cualquier otro caracter no alfabético
tweets_clean <- tweets_txt
tweets_clean$text <- gsub('@[a-z0-9]+','',tweets_clean$text)
tweets_clean$text <- gsub('https?://[A-Za-z0-9./]+','',tweets_clean$text)
tweets_clean$text <- gsub('[^a-zA-Z ]','',tweets_clean$text)

#  4.3. Se eliminan las "stopwords"
tweets_stopwords <- tweets_clean
for(i in 1:nrow(tweets_stopwords)){
  x <- tweets_stopwords$text[i]
  if(length(x) > 0){
    x <- unlist(strsplit(x," "))
    x <- x[!x %in% stopwords()]
    x <- paste(x, collapse = " ")
    tweets_stopwords$text[i] <- x
  }
}

#  4.4. Se lematizan las palabras
tweets_stem <- tweets_stopwords
for(i in 1:nrow(tweets_stem)){
  x <- tweets_stem$text[i]
  if(length(x) > 0){
    x <- text_tokens(x, stemmer = "en")
    x <- paste(unlist(x), collapse = " ")
    tweets_stem$text[i] <- x
  }
}

# 5. Se encuentra la polaridad de los tweets
tweets_sentiment <- tweets_stem
sentiment <- analyzeSentiment(tweets_sentiment$text)
sentiment_string <- convertToBinaryResponse(sentiment)$SentimentQDAP
summary(sentiment_string)

sentiment_values <- convertToDirection(sentiment$SentimentQDAP)
summary(sentiment_values)
sentiment_values[sentiment_values = NA]

# 6. Se convierte la polaridad a valores numéricos:
#     - Positivo = 1
#     - Neutro = 0
#     - Negativo = -1
tweets_complete <- data.frame(matrix(nrow=16087,ncol = 3))
colnames(tweets_complete) <- c("Date","Text","Sentiment")
tweets_complete$Date <- tweets_stem$date
tweets_complete$Text <- tweets_stem$text
for(i in 1:nrow(tweets_complete)){
  switch (sentiment_values[i],
    positive = {tweets_complete$Sentiment[i] = 1},
    neutral = {tweets_complete$Sentiment[i] = 0},
    negative = {tweets_complete$Sentiment[i] = -1}
  )
}
tweets_complete <- na.omit(tweets_complete)

# 7. Se suman los valores en la misma fecha
tweets_final <- data.frame(matrix(nrow = length(unique(tweets_complete$Date)), ncol = 2))
colnames(tweets_final) <- c("Date","Total")
tweets_final$Date = unique(tweets_complete$Date)
for(i in 1:nrow(tweets_final)){
  tweets_final$Total[i] = 0
}

for(i in 1:nrow(tweets_final)){
  for(j in 1:nrow(tweets_complete)){
    if(tweets_complete$Date[j] == tweets_final$Date[i]){
      tweets_final$Total[i] <- tweets_final$Total[i] + tweets_complete$Sentiment[j]
    }
  }
}
tweets_final

