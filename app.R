library(plotly)
library(shiny)
library(shinybrowser)
library(shinyjs)
library(tmap)
library(yaml)

options(shiny.sanitize.errors = FALSE, encoding='utf-8')

shinyApp(ui = ui, server = server, enableBookmarking = "url")
