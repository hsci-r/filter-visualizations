library(dplyr)
library(ggplot2)
library(RCurl)
library(RMariaDB)
library(sf)
library(stringi)

query_db <- function(q) {
  con <- dbConnect(MariaDB(),
                   host=Sys.getenv('DB_HOST'),
                   port=as.integer(Sys.getenv('DB_PORT')),
                   user=Sys.getenv('DB_USER'),
                   password=Sys.getenv('DB_PASS'),
                   dbname=Sys.getenv('DB_NAME'),
                   bigint='integer')
  dbExecute(con, 'SET NAMES utf8;')
  res <- dbSendQuery(con, q)
  data <- dbFetch(res)
  dbClearResult(res)
  dbDisconnect(con)
  data
}

query_octavo <- function(endpoint, query, fields, grouping.var, limit=20, offset=0) {
  URL <- paste0(
    endpoint,
    'search??pretty&endpoint=',
    URLencode(endpoint, reserved=T),
    '&fieldEnricher=&offsetDataConverter=&query=',
    URLencode(query, reserved=T),
    paste0('&field=', fields, collapse=''),
    '&offset=', offset, '&limit=', limit,
    '&contextLevel=Sentence&contextExpandLeft=0&contextExpandRight=0',
    '&level=LINE&sort=score&sortDirection=D&format=csv')
  result <- getURL(URL, .encoding='UTF-8')
  read.csv(text = result, stringsAsFactors=F, encoding='utf-8') %>%
    group_by_at(grouping.var) %>%
    summarize(y = n()) %>%
    rename(x = grouping.var)
}

read_areas <- function() {
  con <- dbConnect(MariaDB(),
                   host=Sys.getenv('DB_HOST'),
                   port=as.integer(Sys.getenv('DB_PORT')),
                   user=Sys.getenv('DB_USER'),
                   password=Sys.getenv('DB_PASS'),
                   dbname=Sys.getenv('DB_NAME'),
                   bigint='integer')
  dbExecute(con, 'SET NAMES utf8;')
  q <- 'select name as parish_name, ST_AsBinary(geometry) as geometry from polygons;'
  df <- st_read(con, query=q, geometry_column='geometry')
  st_crs(df) <- 'urn:ogc:def:crs:EPSG::3857'
  dbDisconnect(con)
  st_make_valid(df)
}

server <- function(input, output, session) {
  config <- read_yaml('config.yaml')
  tmap_options(check.and.fix=T)
  areas <- read_areas()
  is.interactive <- reactive(as.logical(input$interactive))
  df <- reactive({
    req(input$vis)
    v <- config$visualizations[[input$vis]]
    query <- ifelse(
      is.null(v$params),
      v$query,
      stri_replace_all_regex(
        v$query,
        paste0('@', sapply(v$params, function(x) x$name)),
        sapply(v$params, function(x) input[[x$name]]),
        vectorize_all=F
      )
    )
    switch(v$source,
      'sql' = query_db(query),
      'octavo' = query_octavo(config$global$octavo_endpoint, query, v$fields, v$group_by, limit=-1)
    )
  }) %>%
  bindEvent(input$refresh, ignoreNULL=F)
  tmap <- reactive({
    tm_shape(
      areas %>% left_join(df(), by=c('parish_name' = 'x'))
    ) + tm_polygons(col="y", id="parish_name",
                    palette=input$map_palette, style=input$map_style)
  })
  dl_filename <- reactive({
    params <- config$visualizations[[input$vis]]$params
    values <- sapply(params, function(x) as.character(input[[x$name]]))
    stri_replace_all_charclass(
      paste(c(input$vis, values), collapse='_'),
      '[*:\\p{WHITE_SPACE}]', '_'
    )
  })
  output$tmap <- renderTmap({ tmap() })
  output$plot <- renderPlot({
    ggplot(df(), aes(x, y)) + geom_bar(stat='identity') + coord_flip()
  })
  output$dt <- DT::renderDataTable(df())
  output$link <- renderUI({
    params <- config$visualizations[[input$vis]]$params
    url <- paste0(
      '/?',
      paste(
        c('vis', sapply(params, function(x) x$name)),
        c(input$vis, sapply(params, function(x) URLencode(as.character(input[[x$name]])))),
        sep = '=', collapse = '&'
      )
    )
    tags$a(href=url, 'permalink')
  }) %>%
    bindEvent(input$refresh, ignoreNULL=T)
  output$dlData <- downloadHandler(
    filename = function() paste0(dl_filename(), '.csv'),
    content = function(file) {
      write.csv(df(), file, row.names=F)
    },
    contentType = 'text/csv'
  )
  output$dlMap <- downloadHandler(
    filename = function() paste0(dl_filename(), '.png'),
    content = function(file) {
      tmap_save(tmap(), filename = file)
    },
    contentType = 'image/png'
  )
}
