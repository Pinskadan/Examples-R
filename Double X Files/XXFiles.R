setwd("/home/oliver/Documents")

###install.packages("readODS")

XXFiles <- read.csv("Double X Files.csv")
XXFiles$Episode.Title <- NULL
XXFiles[XXFiles==9999] <- NA

rage <- table(XXFiles$Rage)
barplot(rage, main="Alison's Rage Scale",
        xlab="Ranking", ylab="Number of Episdoes per Ranking") 

spooky <- table(XXFiles$Spooky)
barplot(spooky, main = "Courtney's Spooky Scale",
        xlab="Ranking", ylab="Number of Episodes per Ranking")



