# README

This blog post will show you how to make some analysis on your whatsapp messages. 

Whatsapp lets you export your messages as a txt file. You can export your whatsapp history with a group or person when you go to the person's or group's whatsapp chat. Then, go to the extras (Three dots in a vertical line, top right), click on "more" and then "Email chat" without the media to your email address. The provided txt file is then used as an input for this tutorial.

The script was tested for English and German versions of whatsapp on Android phones.

You can find a shiny app where you can upload your txt file and check out the code under the following [link] (https://cgrossbaier.shinyapps.io/WhatsAppShiny). 

The txt files are only saved on the server for the duration of the analysis and will be deleted as soon as you close the tab. If you are more comfortable with running the code locally, use the code as provided in the code below.

***

The tutorial is structured as follows.

1. Libraries
2. Import whatsapp history txt file
3. Data cleaning
4. Data exploration
5. Text exploration
6. Appendix

***




## Libraries

These are all the libraries you need.


```r
library(plyr) # for data manipulation
library(dplyr) # for data manipulation
library(rCharts) # for interactive plotting
library(tm) # for text analysis
```

## Import whatsapp history txt file

First step is to import the data, that the txt file contains into a format that R can work with. Make sure you have your r scipt in the same folder than your txt file. 

Make sure, your timestamps are in one of the following formats, otherwise the script will not detect it.

1. 28/01/2015, 22:06
2. 2015-01-28, 10:06 PM
3. 28.01.2015, 22:06


```r
# One line of the file looks like this:
# "28/01/2015, 22:06 - PERSON: Good morning!"

InputFile <- "WhatsApp Chat with Person.txt"
Connection <- file(InputFile, open = "r")

Messages <- NULL

Iterator <- 0

while (length(Line <- readLines(Connection, n = 1, warn = FALSE)) > 0) {
  
  Timestamp.Raw = strsplit(Line, " - ")[[1]][1]
  
  if (Iterator == 0){
    Timestamp.Format <- "%d/%m/%Y, %H:%M"
    Timestamp = as.POSIXct(strptime(Timestamp.Raw, Timestamp.Format, tz = "GMT"))
    if (is.na(Timestamp)) {
      Timestamp.Format <- "%Y-%m-%d, %I:%M %p"
      Timestamp = as.POSIXct(strptime(Timestamp.Raw, Timestamp.Format, tz = "GMT"))
      if (is.na(Timestamp)) {
        Timestamp.Format <- "%d.%m.%Y, %H:%M"
        Timestamp = as.POSIXct(strptime(Timestamp.Raw, Timestamp.Format, tz = "GMT"))
        assertthat::noNA(Timestamp)
      }
    }
  }
  
  Timestamp <- as.POSIXct(strptime(Timestamp.Raw, Timestamp.Format, tz = "GMT"))
  
  if (is.na(Timestamp)) {
    Messages$Text[Iterator] <- paste(Messages$Text[Iterator], Line)
  } else {
    Person = strsplit(strsplit(Line, ": ")[[1]][1], " - ")[[1]][2]
    
    Text <- strsplit(Line, ": ")[[1]]
    Text <- strsplit(Line, ": ")[[1]][2:length(Text)]
    
    Message <- data.frame(Timestamp = Timestamp, Person = Person, Text = Text)
    
    Messages <- rbind(Messages, Message)
    Iterator <- Iterator + 1
    
  }
}
close(Connection)

Messages$Text <- as.character(Messages$Text)
```

## Data cleaning

Now we have the messages in a data frame. Let's do some data cleaning.


```r
# Flag if message contained image or media
Messages$MediaOmitted <- FALSE

Messages$Text.Cleaned <- Messages$Text

# Flag messages that contain media
Filter.Media <- grep("<Media omitted>", Messages$Text.Cleaned)
Messages$MediaOmitted[Filter.Media] <- TRUE
Messages$Text.Cleaned[Filter.Media] <- gsub("<Media omitted>","", Messages$Text.Cleaned[Filter.Media])

Filter.Media <- grep("<Medien weggelassen>", Messages$Text.Cleaned)
Messages$MediaOmitted[Filter.Media] <- TRUE
Messages$Text.Cleaned[Filter.Media] <- gsub("<Medien weggelassen>","", Messages$Text.Cleaned[Filter.Media])

# Remove Smileys
Messages$Text.Cleaned <- sapply(Messages$Text.Cleaned, 
                                function(row) iconv(row, "latin1", "ASCII", sub = ""))

# Tidy Person in case we are looking at a group chat
Filter.Person <- rbind(grep(" changed ", Messages$Person), 
                       grep(" geändert", Messages$Person),
                       grep(" added", Messages$Person),
                       grep(" hinzugefügt", Messages$Person),
                       grep(" removed", Messages$Person),
                       grep(" entfernt", Messages$Person),
                       grep(" left", Messages$Person), 
                       grep(" verlassen", Messages$Person),
                       grep(" created", Messages$Person),
                       grep(" erstellt", Messages$Person))

if (length(Filter.Person) > 0) Messages <- Messages[-Filter.Person, ]
```

## Data exploration

Let's get a general overview about how many messages everyone send.


```r
Messages.Aggregated <- Messages %>% 
  group_by(Person) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count))

Plot.All <- nPlot(Count ~ Person, data = Messages.Aggregated, type = "multiBarChart")
Plot.All$chart(reduceXTicks = FALSE)
Plot.All$xAxis(staggerLabels = TRUE)

Plot.All$yAxis(tickFormat = "#!\n function(d) {return d3.format('.')(d)}\n!#")
```


```r
Plot.All
```

Let's have a look how the Messages by everyone were distributed across the months.


```r
Messages$MonthYear <- format(Messages$Timestamp, format = "%y-%m")
Messages.Aggregated <- Messages %>% group_by(MonthYear, Person) %>% summarise(Count = n())
```

NVD3 does not handle "holes" in the data well. So one way to get things working is to plug these "holes" by adding data. 


```r
fillDataHoles <- function(Messages.Aggregated, Parameter1, Parameter2) {
  Messages.Aggregated.Full <- expand.grid(unique(Messages.Aggregated[[Parameter1]]), 
                                          unique(Messages.Aggregated[[Parameter2]]))
  colnames(Messages.Aggregated.Full) <- c(Parameter1, Parameter2)
  
  Messages.Aggregated <- join(Messages.Aggregated.Full, Messages.Aggregated, 
                              by = c(Parameter1, Parameter2))
  
  Messages.Aggregated$Count[is.na(Messages.Aggregated$Count)] <- 0
  
  Messages.Aggregated
}

Messages.Aggregated <- fillDataHoles(Messages.Aggregated, "MonthYear", "Person")


Plot.MonthYear <- nPlot(Count ~ MonthYear, group = "Person", data = Messages.Aggregated, 
                        type = "multiBarChart")
Plot.MonthYear$yAxis(tickFormat = "#!\n        function(d) {return d3.format('.')(d)}\n!#")
Plot.MonthYear$chart(reduceXTicks = FALSE)
Plot.MonthYear$xAxis(staggerLabels = TRUE)
```


```r
Plot.MonthYear
```

Per Hour of the day.


```r
Messages$Hour <- as.numeric(format(Messages$Timestamp, format = "%H"))
Messages.Aggregated <- Messages %>% group_by(Hour, Person) %>% summarise(Count = n())
Messages.Aggregated <- fillDataHoles(Messages.Aggregated, "Hour", "Person")


Plot.Hour <- nPlot(Count ~ Hour, group = "Person", data = Messages.Aggregated, 
                   type = "stackedAreaChart")
Plot.Hour$yAxis(tickFormat = "#!\nfunction(d) {return d3.format('.')(d)}\n!#")
```


```r
Plot.Hour
```

## Text exploration

Okay, that was fun for a general overview. Let's dig more into the message body and look, if we can find some interesting patterns in terms of the choice of words. We use the tm package to prepare the message body.


```r
Text.Corpus <- Corpus(VectorSource(Messages$Text.Cleaned))

Corpus.Clean <- tm_map(Text.Corpus, content_transformer(tolower))
Corpus.Clean <- tm_map(Corpus.Clean, content_transformer(removeNumbers))
Corpus.Clean <- tm_map(Corpus.Clean, content_transformer(removeWords), 
                       stopwords("en"))
Corpus.Clean <- tm_map(Corpus.Clean, content_transformer(removeWords), 
                       stopwords("german"))
Corpus.Clean <- tm_map(Corpus.Clean, content_transformer(removePunctuation))
Corpus.Clean <- tm_map(Corpus.Clean, content_transformer(stripWhitespace))

Messages$Text.Cleaned <- as.vector(data.frame(
  text = unlist(sapply(Corpus.Clean,`[`, "content")), stringsAsFactors = F))

Corpus.Clean.Dtm <- DocumentTermMatrix(Corpus.Clean)
Dictionary <- findFreqTerms(Corpus.Clean.Dtm, 2)

Dictionary.Sorted <- NULL

for (Word in Dictionary) {
  Word.Count <- data.frame(Word = Word, Count = length(grep(Word, Messages$Text.Cleaned$text)))
  Dictionary.Sorted <- rbind(Dictionary.Sorted, Word.Count)
}

Dictionary.Sorted <- Dictionary.Sorted[order(Dictionary.Sorted$Count, decreasing = TRUE),]
Dictionary.Sorted.Top20 <- head(Dictionary.Sorted, 20)

Table.WordFrequencies <- NULL

for (Word in Dictionary.Sorted.Top20$Word) {
  for (Person in unique(Messages$Person)) {
    Count = length(grep(Word, Messages$Text.Cleaned$text[Messages$Person == Person]))
    Word.Person.Count <- data.frame(Word = Word, Person = Person, Count = Count)
    Table.WordFrequencies <- rbind(Table.WordFrequencies, Word.Person.Count)
  }
}

Plot.Words <- nPlot(Count ~ Word, group = "Person", data = Table.WordFrequencies, 
                    type = "multiBarChart")
Plot.Words$yAxis(tickFormat = "#!\nfunction(d) {return d3.format('.')(d)}\n!#")
Plot.Words$xAxis(staggerLabels = TRUE)
Plot.Words$chart(reduceXTicks = FALSE)
```


```r
Plot.Words
```

## Appendix

R Session Info


```r
sessionInfo()
```

```
## R version 3.2.0 (2015-04-16)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.3 (Yosemite)
## 
## locale:
## [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] tm_0.6-2      NLP_0.1-8     rCharts_0.4.5 dplyr_0.4.3   plyr_1.8.1   
## [6] knitr_1.11   
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.1     lattice_0.20-31 assertthat_0.1  slam_0.1-32    
##  [5] grid_3.2.0      R6_2.0.1        DBI_0.3.1       formatR_1.1    
##  [9] magrittr_1.5    evaluate_0.7.2  stringi_0.5-5   lazyeval_0.1.10
## [13] whisker_0.3-2   RJSONIO_1.3-0   tools_3.2.0     stringr_1.0.0  
## [17] parallel_3.2.0  yaml_2.1.13
```
