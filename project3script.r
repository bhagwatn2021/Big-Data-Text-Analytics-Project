#install and open libraries
install.packages("readr")

library(readr)

#load the text into a data frame
text <- read_lines("TwentyThousandLeagues.txt")
textdf <- data.frame(text)
