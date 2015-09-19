rm(list = ls())

library(shiny)
library(plyr)  # for data manipulation
library(dplyr)  # for data manipulation
library(rCharts)  # for interactive plotting
library(tm)  # for text analysis

fillDataHoles <- function(Messages.Aggregated, Parameter1, Parameter2) {
  Messages.Aggregated.Full <- expand.grid(unique(Messages.Aggregated[[Parameter1]]),
                                          unique(Messages.Aggregated[[Parameter2]]))
  colnames(Messages.Aggregated.Full) <- c(Parameter1, Parameter2)

  Messages.Aggregated <- join(Messages.Aggregated.Full, Messages.Aggregated,
                              by = c(Parameter1, Parameter2))

  Messages.Aggregated$Count[is.na(Messages.Aggregated$Count)] <- 0

  Messages.Aggregated
}

shinyServer(function(input, output) {

  dataInput <- reactive({
    # input$file1 will be NULL initially. After the user selects and
    # uploads a file, it will be a data frame with 'name', 'size', 'type',
    # and 'datapath' columns. The 'datapath' column will contain the local
    # filenames where the data can be found.

    # One line of the file looks like this: '28/01/2015, 22:06 - PERSON:
    # Good morning!'

    inFile <- input$file1

    if (is.null(inFile)) {
      return(NULL)
    } else {

      InputFile <- inFile$datapath

      Connection <- file(InputFile, open = "r")

      Messages <- NULL

      Iterator <- 0

      while (length(Line <- readLines(Connection, n = 1, warn = FALSE)) >
             0) {

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
          Messages$Text[Iterator] <- paste(Messages$Text[Iterator],
                                           Line)
        } else {
          Person = strsplit(strsplit(Line, ": ")[[1]][1], " - ")[[1]][2]

          Text <- strsplit(Line, ": ")[[1]]
          Text <- strsplit(Line, ": ")[[1]][2:length(Text)]

          Message <- data.frame(Timestamp = Timestamp, Person = Person,
                                Text = Text)

          Messages <- rbind(Messages, Message)
          Iterator <- Iterator + 1

        }
      }
      close(Connection)

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
      Messages$Text.Cleaned <- sapply(Messages$Text.Cleaned, function(row) iconv(row,
                                                                                 "latin1", "ASCII", sub = ""))

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

      if (length(Filter.Person) > 0)
        Messages <- Messages[-Filter.Person, ]

      return(Messages)
    }

  })

  ### PLOT ALL #####
  output$chart1 <- renderChart({

    Messages <- dataInput()

    if (is.null(Messages)) {

      data("baseball")

      Plot.All <- nPlot(g ~ year, data = baseball[1:100, ], type = "scatterChart")
      return(Plot.All)
    } else {


      Messages.Aggregated <- Messages %>% group_by(Person) %>% summarise(Count = n()) %>%
        arrange(desc(Count))

      Plot.All <- nPlot(Count ~ Person, data = Messages.Aggregated,
                        type = "multiBarChart")
      Plot.All$chart(reduceXTicks = FALSE)
      Plot.All$xAxis(staggerLabels = TRUE)

      Plot.All$yAxis(tickFormat = "#!\n function(d) {return d3.format('.')(d)}\n        !#")

      Plot.All$addParams(height = 300, dom = "chart1", title = "Percentage of Employed who are Senior Managers")

      return(Plot.All)
    }
  })

  ### Plot.MonthYear #####
  output$chart2 <- renderChart({

    Messages <- dataInput()

    if (is.null(Messages)) {

      data("baseball")

      Plot.All <- nPlot(g ~ year, data = baseball[1:100, ], type = "scatterChart")
      return(Plot.All)
    } else {

      Messages$MonthYear <- format(Messages$Timestamp, format = "%y-%m")
      Messages.Aggregated <- Messages %>% group_by(MonthYear, Person) %>%
        summarise(Count = n())

      Messages.Aggregated <- fillDataHoles(Messages.Aggregated, "MonthYear",
                                           "Person")


      Plot.MonthYear <- nPlot(Count ~ MonthYear, group = "Person",
                              data = Messages.Aggregated, type = "multiBarChart")
      Plot.MonthYear$yAxis(tickFormat = "#!\n        function(d) {return d3.format('.')(d)}\n        !#")
      Plot.MonthYear$chart(reduceXTicks = FALSE)
      Plot.MonthYear$xAxis(staggerLabels = TRUE)
      Plot.MonthYear$print("plotMonthYear", include_assets = TRUE)

      Plot.MonthYear$addParams(height = 300, dom = "chart2", title = "Percentage of Employed who are Senior Managers")
      return(Plot.MonthYear)
    }
  })

  ### Plot.Hour #####
  output$chart3 <- renderChart({

    Messages <- dataInput()

    if (is.null(Messages)) {

      data("baseball")

      Plot.All <- nPlot(g ~ year, data = baseball[1:100, ], type = "scatterChart")
      return(Plot.All)
    } else {

      Messages$Hour <- as.numeric(format(Messages$Timestamp, format = "%H"))
      Messages.Aggregated <- Messages %>% group_by(Hour, Person) %>%
        summarise(Count = n())
      Messages.Aggregated <- fillDataHoles(Messages.Aggregated, "Hour",
                                           "Person")


      Plot.Hour <- nPlot(Count ~ Hour, group = "Person", data = Messages.Aggregated,
                         type = "stackedAreaChart")
      Plot.Hour$yAxis(tickFormat = "#!\n        function(d) {return d3.format('.')(d)}\n        !#")

      Plot.Hour$addParams(height = 300, dom = "chart3", title = "Percentage of Employed who are Senior Managers")

      return(Plot.Hour)
    }
  })

  ### Plot.Words #####
  output$chart4 <- renderChart({

    Messages <- dataInput()

    if (is.null(Messages)) {

      data("baseball")

      Plot.All <- nPlot(g ~ year, data = baseball[1:100, ], type = "scatterChart")
      return(Plot.All)
    } else {

      Text.Corpus <- Corpus(VectorSource(Messages$Text.Cleaned))

      Corpus.Clean <- tm_map(Text.Corpus, content_transformer(tolower))
      Corpus.Clean <- tm_map(Corpus.Clean, content_transformer(removeNumbers))
      Corpus.Clean <- tm_map(Corpus.Clean, content_transformer(removeWords),
                             stopwords("en"))
      Corpus.Clean <- tm_map(Corpus.Clean, content_transformer(removeWords),
                             stopwords("german"))
      Corpus.Clean <- tm_map(Corpus.Clean, content_transformer(removePunctuation))
      Corpus.Clean <- tm_map(Corpus.Clean, content_transformer(stripWhitespace))

      Messages$Text.Cleaned <- as.vector(data.frame(text = unlist(sapply(Corpus.Clean,
                                                                         `[`, "content")), stringsAsFactors = F))

      Corpus.Clean.Dtm <- DocumentTermMatrix(Corpus.Clean)
      Dictionary <- findFreqTerms(Corpus.Clean.Dtm, 5)

      Dictionary.Sorted <- NULL

      for (Word in Dictionary) {
        Word.Count <- data.frame(Word = Word, Count = length(grep(Word,
                                                                  Messages$Text.Cleaned$text)))
        Dictionary.Sorted <- rbind(Dictionary.Sorted, Word.Count)
      }

      Dictionary.Sorted <- Dictionary.Sorted[order(Dictionary.Sorted$Count,
                                                   decreasing = TRUE), ]

      Dictionary.Sorted.Top20 <- head(Dictionary.Sorted, 20)

      Table.WordFrequencies <- NULL

      for (Word in Dictionary.Sorted.Top20$Word) {
        for (Person in unique(Messages$Person)) {
          Count = length(grep(Word, Messages$Text.Cleaned$text[Messages$Person ==
                                                                 Person]))
          Word.Person.Count <- data.frame(Word = Word, Person = Person,
                                          Count = Count)
          Table.WordFrequencies <- rbind(Table.WordFrequencies,
                                         Word.Person.Count)
        }
      }

      Plot.Words <- nPlot(Count ~ Word, group = "Person", data = Table.WordFrequencies,
                          type = "multiBarChart")
      Plot.Words$yAxis(tickFormat = "#!\n        function(d) {return d3.format('.')(d)}\n        !#")
      Plot.Words$xAxis(staggerLabels = TRUE)
      Plot.Words$chart(reduceXTicks = FALSE)
      Plot.Words$addParams(height = 300, dom = "chart4", title = "Percentage of Employed who are Senior Managers")

      return(Plot.Words)
    }
  })


})
