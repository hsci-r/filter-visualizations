library(shiny)
library(shinyjs)
library(tmap)
library(yaml)

message('DB_HOST: ', Sys.getenv('DB_HOST'))
message('DB_USER: ', Sys.getenv('DB_USER'))

options(shiny.sanitize.errors = FALSE, encoding='utf-8')

shinyApp(ui = ui, server = server, enableBookmarking = "url")
