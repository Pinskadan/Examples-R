require(tm)
require(slam)
require(NLP)

pathToData <- "C:\\Users\\Oliver\\Desktop\\LIS590AG_Assignment6\\NSF_Part1"

abstracts <- Corpus(DirSource(pathToData, recursive = TRUE), 
                    readerControl = list(reader=readPlain, language = "en"))


tdm <- TermDocumentMatrix(abstracts,
                          control = list(removePunctuation = TRUE,
                                         removeNumbers = TRUE,
                                         stopwords = TRUE))
tdm <- weightBin(tdm)


table <- as.matrix(tdm)
tableau <- as.data.frame(table)

idfFinal <- apply(tableau, 1, function(x) (log10(51979/sum (x))))
idfFinal1 <- as.data.frame(idfFinal)

write.csv(idfFinal1, row.names=TRUE, file="IDF.csv")