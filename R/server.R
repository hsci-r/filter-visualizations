library(dplyr)
library(ggplot2)
library(RCurl)
library(RMariaDB)
library(sf)
library(stringi)

connect_to_db <- function() {
  con <- dbConnect(MariaDB(),
                   host=Sys.getenv('DB_HOST'),
                   port=as.integer(Sys.getenv('DB_PORT')),
                   user=Sys.getenv('DB_USER'),
                   password=Sys.getenv('DB_PASS'),
                   dbname=Sys.getenv('DB_NAME'),
                   bigint='integer')
  dbExecute(con, 'SET NAMES utf8;')
  con
}

query_db <- function(q) {
  con <- connect_to_db()
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
  con <- connect_to_db()
  q <- 'select name as parish_name, ST_AsBinary(geometry) as geometry from polygons;'
  df <- st_read(con, query=q, geometry_column='geometry')
  st_crs(df) <- 'urn:ogc:def:crs:EPSG::3857'
  dbDisconnect(con)
  st_make_valid(df)
}

make_query <- function(input, visualization) {
  ifelse(
    is.null(visualization$params),
    visualization$query,
    stri_replace_all_regex(
      visualization$query,
      paste0('@', sapply(visualization$params, function(x) x$name)),
      sapply(visualization$params, function(x) input[[x$name]]),
      vectorize_all=F
    )
  )
}

plot_timeline <- function(df, min, max, by) {
  time.seq <- as.Date(
    as.character(seq(min, max, by = switch(by, 'year' = 1, 'decade' = 10))),
    format='%Y')
  decades <- as.Date(as.character(seq(min, max, 10)), format='%Y')
  df <- df %>%
    mutate(x = if (by == 'decade') { paste0(substring(x, 1, 3), '0') } else { x })# %>%
  df <- df %>%
    group_by(x) %>%
    summarize(y = sum(y))
  df$x <- as.Date(as.character(df$x), format='%Y')
  df <- df %>%
    right_join(data.frame(x = time.seq), by = 'x')
  df[is.na(df$y),]$y <- 0
  ggplot(df, aes(x=x, y=y)) +
    geom_line() +
    xlab("") +
    scale_x_date(breaks=decades, date_labels = '%Y')
}

server <- function(input, output, session) {
  config <- read_yaml('config.yaml')
  tmap_options(check.and.fix=T)
  areas <- read_areas()
  is.interactive <- reactive(as.logical(input$interactive))

  # data
  df <- reactive({
    req(input$vis)
    v <- config$visualizations[[input$vis]]
    q <- make_query(input, v)
    switch(v$source,
      'sql' = query_db(q),
      'octavo' = query_octavo(config$global$octavo_endpoint, q, v$fields, v$group_by, limit=-1)
    )
  }) %>%
  bindEvent(input$refresh, ignoreNULL=F)

  # map
  tmap <- reactive({
    breaks <- sapply(unlist(stri_split_fixed(input$map_breaks, ',')), as.numeric)
    tm_shape(
      areas %>% left_join(df(), by=c('parish_name' = 'x'))
    ) + tm_polygons(col="y", id="parish_name",
                    palette=input$map_palette, style=input$map_style,
                    breaks=breaks)
  })
  output$tmap <- renderTmap({ tmap() })

  # other outputs
  output$tree <- renderPlotly({
    # compute totals for categories higher in the hierarchy
    df <- df()
    df2 <- df %>%
      left_join(df, by=c('x' = 'parent'), suffix=c('', '.1'), na_matches='never') %>%
      left_join(df, by=c('x.1' = 'parent'), suffix=c('', '.2'), na_matches='never') %>%
      left_join(df, by=c('x.2' = 'parent'), suffix=c('', '.3'), na_matches='never') %>%
      group_by(x, label, parent) %>%
      summarize(y = sum(y, na.rm=T) + sum(y.1, na.rm=T) + sum(y.2, na.rm=T) + sum(y.3, na.rm=T)) %>%
      filter(y > 0)
    plot_ly(df2, ids=~x, labels=~label, parents=~parent, values=~y,
            type=input$tree_type, branchvalues='total')
  })
  output$plot <- renderPlot({
    v <- config$visualizations[[input$vis]]
    switch(v$type,
      'barplot' = ggplot(df(), aes(x, y))
                  + geom_bar(stat='identity') + coord_flip(),
      'timeline' = plot_timeline(df(), input$tl_min, input$tl_max, input$tl_by)
    )
  }) %>% bindEvent(input$refresh, ignoreNULL=T)
  output$dt <- DT::renderDataTable(df())
  output$link <- renderUI({
    params <- config$visualizations[[input$vis]]$params
    url <- paste0(
      '/?',
      paste(
        c('vis', sapply(params, function(x) x$name)),
        c(input$vis,
          sapply(params, function(x) URLencode(as.character(input[[x$name]]),
                                               reserved=T)
          )
        ),
        sep = '=', collapse = '&'
      )
    )
    tags$a(href=url, 'permalink')
  }) %>%
    bindEvent(input$refresh, ignoreNULL=T)

  # download handlers
  dl_filename <- reactive({
    params <- config$visualizations[[input$vis]]$params
    values <- sapply(params, function(x) as.character(input[[x$name]]))
    stri_replace_all_charclass(
      paste(c(input$vis, values), collapse='_'),
      '[*:\\p{WHITE_SPACE}]', '_'
    )
  })
  output$dlData <- downloadHandler(
    filename = function() paste0(substr(dl_filename(), 1, 50), '.csv'),
    content = function(file) {
      write.csv(df(), file, row.names=F)
    },
    contentType = 'text/csv'
  )
  output$dlMap <- downloadHandler(
    filename = function() paste0(substr(dl_filename(), 1, 50), '.png'),
    content = function(file) {
      tmap_save(tmap(), filename = file)
    },
    contentType = 'image/png'
  )

  observeEvent(input$vis, toggleState('dlMap', grepl('map_', input$vis)))

  chq <- lapply(config$choices_queries, query_db)
  observeEvent(input$vis, {
    v <- config$visualizations[[input$vis]]
    for (p in v$params) {
      if (!is.null(p$choices_query)) {
        if (input[[p$name]] == '') {
          updateSelectizeInput(
            session, p$name, server=T,
            choices = c('Select...' = '', setNames(chq[[p$choices_query]]$value,
                                                   chq[[p$choices_query]]$label)))
        }
      }
    }
  })
}