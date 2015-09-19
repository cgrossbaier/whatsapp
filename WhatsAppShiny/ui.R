library(shiny)
library(rCharts)

shinyUI(fluidPage(
  titlePanel("Whatsapp Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose whatsapp txt file',
                accept=c('.txt')),
      tags$hr()
    ),
    mainPanel(
      showOutput("chart1", "nvd3"),
      showOutput("chart2", "nvd3"),
      showOutput("chart3", "nvd3"),
      showOutput("chart4", "nvd3")
    )
  )
))
