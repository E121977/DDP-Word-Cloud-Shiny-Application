library(tm)
library(wordcloud)
library(memoise)
require(shiny)
require(httr)
require(markdown)

#setwd("C:/Users/E121977/Desktop/Coursera - Data Science/(7) Developing Data Products/Shiny APP/")

# The list of valid books.
books <<- list("The Fall of the House of Usher" = "Usher.txt",
               "The Raven" = "Raven.txt",
               "The Masque of the Red Death" = "Death.txt",
               "The Works of Edgar Allan Poe" = "Works.txt")

# We will use "memoise" to cache the resutls.
getTermMatrix <- memoise(function(book) {
  # Caution should be used not to let just any name slip in here; 
  # Qualify from known list of books
  if (!(book %in% books))
    stop("Unknown book")
  
  text <- readLines(sprintf("%s", book), encoding="UTF-8")
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})
