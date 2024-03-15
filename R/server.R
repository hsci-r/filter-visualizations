library(dplyr)
library(ggplot2)
library(RCurl)
library(RMariaDB)
library(sf)
library(stringi)
library(tidyr)
library(yaml)
library(whisker)
library(wordcloud2)

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

write_log_to_db <- function(url, rtime, user_agent) {
  con <- connect_to_db()
  if (nchar(url) > 2000) {
    url <- paste0(substr(url, 1, 1997), '...')
  }
  dbAppendTable(con, 'visualizations_log',
                data.frame(url=url, rtime=as.double(rtime), user_agent=user_agent))
  dbDisconnect(con)
}

source("R/visualizations/map.R", local=T)
source("R/visualizations/timeline.R", local=T)
source("R/visualizations/typetree.R", local=T)
source("R/visualizations/wordcloud.R", local=T)

get_csv_from_url <- function(url, grouping.var) {
  df <- read.csv(url) %>%
    separate_rows(grouping.var, sep='; ') %>%
    group_by_at(grouping.var) %>%
    summarize(y = n()) %>%
    filter(grouping.var != "")
  # workaround for place names: "County — Parish" -> "Parish"
  if (grouping.var == 'place') {
    df <- df %>% rename(place_name = place)
    df$place_name <- stri_replace_all_regex(df$place_name, '.* — ', '')
  }
  df
}

# FIXME this is clumsy -- rewrite so that the data and visualization params
# are combined earlier
make_url <- function(input, params, vistype, visparams) {
  paste0(
    '/?',
    paste(
      c('vis', sapply(params, function(x) x$name), sapply(visparams, function(x) x$name)),
      c(input$vis,
        sapply(params,
               function(x) URLencode(
                   as.character(input[[paste0(input$vis, '__', x$name)]]),
                   reserved=T)
        ),
        sapply(visparams,
               function(x) URLencode(
                   as.character(input[[paste0(vistype, '__', x$name)]]),
                   reserved=T)
        )
      ),
      sep = '=', collapse = '&'
    )
  )
}

query_octavo <- function(endpoint, query, level, fields, extractor, varname, snippets, limit=20, offset=0) {
  URL <- paste0(
    endpoint,
    'search??pretty&endpoint=',
    URLencode(endpoint, reserved=T),
    '&fieldEnricher=&offsetDataConverter=&query=',
    URLencode(query, reserved=T),
    ifelse(length(fields) > 0, paste0('&field=', fields, collapse=''), ""),
    '&offset=', offset, '&limit=', limit,
    '&contextLevel=Sentence&contextExpandLeft=0&contextExpandRight=0',
    '&level=', level, '&sort=score&sortDirection=D',
    ifelse(snippets, '&snippetLimit=20', ''))
  # did you know: JSON is also YAML
  r <- yaml.load(getURL(URL, .encoding='UTF-8')) 
  x <- unlist(lapply(r$result$docs, eval(parse(text=extractor))))
  # for place names: "Parish, County" -> "Parish"
  if (varname == 'place_name') {
    x <- stri_replace_all_regex(x, ', .*', '')
  }
  df <- data.frame(x = x) %>%
    group_by(x) %>%
    summarize(y = n())
  names(df) <- c(varname, 'y')
  df
}

# Gets the parameter values from input.
# - `params` are a list of parameter specifications from the YAML config,
# - `input` are the input values from the user interface.
# Returns a list of the form: list(parameter_name = value)
get_params <- function(params, input) {
  setNames(
    lapply(params, function(x) input[[paste0(input$vis, '__', x$name)]]), 
    sapply(params, function(x) x$name))
}

server <- function(input, output, session) {
  config <- read_yaml('config.yaml')
  tmap_options(check.and.fix=T)
  maps <- read_maps()
  types <- read_types()
  place.poly <- read_place.poly()
  
  # data
  make_query <- reactive({
    v <- config$visualizations[[input$vis]]
    whisker.render(v$query, get_params(v$params, input))
  }) %>%
    bindEvent(input$refresh, ignoreNULL=T)

  get_data <- reactive({
    req(input$vis)
    t1 <- Sys.time()
    v <- config$visualizations[[input$vis]]
    q <- make_query()
    url <- make_url(input, v$params, v$type, config$visualization_types[[v$type]]$params)
    if (v$source == "octavo") {
      lvl <- whisker.render(v$level, get_params(v$params, input))
    }
    data <- switch(v$source,
      'csv' = read.csv(text = q),
      'sql' = query_db(q),
      'url' = get_csv_from_url(q, v$group_by),
      'octavo' = query_octavo(config$global$octavo_endpoint, q,
                              lvl, v$fields, v$extractor, v$varname, v$snippets, limit=-1)
    )
    t2 <- Sys.time()
    if (nchar(Sys.getenv('ENABLE_LOGGING_TO_DB')) > 0) {
      write_log_to_db(url, t2-t1, shinybrowser::get_user_agent())
    }
    data
  }) %>%
    bindEvent(input$refresh, ignoreNULL=T)

  # output visualizations
  tmap <- reactive(make_map(get_data(), input, maps))
  output$tmap <- renderTmap(tmap())
  output$tree <- renderPlotly(make_type_tree(get_data(), input, types))
  output$plot <- renderPlot({
    v <- config$visualizations[[input$vis]]
    switch(v$type,
      'barplot' = ggplot(get_data(), aes(x, y))
                  + geom_bar(stat='identity') + coord_flip(),
      'timeline' = plot_timeline(get_data(), input$timeline__min,
                                 input$timeline__max, input$timeline__by)
    )
  }) %>% bindEvent(input$refresh, ignoreNULL=T)
  output$wordcloud <- renderWordcloud2(make_wordcloud(get_data(), input))

  output$dt <- DT::renderDataTable(get_data())
  output$query <- renderText(make_query())
  output$link <- renderUI({
    v <- config$visualizations[[input$vis]]
    url <- make_url(input, v$params, v$type, config$visualization_types[[v$type]]$params)
    tags$a(href=url, 'permalink')
  }) %>%
    bindEvent(input$refresh, ignoreNULL=T)

  # download handlers
  dl_filename <- reactive({
    params <- config$visualizations[[input$vis]]$params
    values <- sapply(params, function(x) as.character(input[[paste0(input$vis, '__', x$name)]]))
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
  output$dlMapPNG <- downloadHandler(
    filename = function() paste0(substr(dl_filename(), 1, 50), '.png'),
    content = function(file) {
      tmap_save(tmap(), filename = file)
    },
    contentType = 'image/png'
  )
  output$dlMapSVG <- downloadHandler(
    filename = function() paste0(substr(dl_filename(), 1, 50), '.svg'),
    content = function(file) {
      tmap_save(tmap(), filename = file)
    },
    contentType = 'image/svg'
  )

  observeEvent(input$vis, toggleState('dlMapSVG', grepl('map_', input$vis)))
  observeEvent(input$vis, toggleState('dlMapPNG', grepl('map_', input$vis)))

  chq <- lapply(config$choices_queries, query_db)
  observeEvent(input$vis, {
    v <- config$visualizations[[input$vis]]
    for (p in v$params) {
      if (!is.null(p$choices_query)) {
        name = paste0(input$vis, '__', p$name)
        if (input[[name]] == '') {
          updateSelectizeInput(
            session, name, server=T,
            choices = c('Select...' = '', setNames(chq[[p$choices_query]]$value,
                                                   chq[[p$choices_query]]$label)))
        }
      }
    }
  })

  # If non-interactive -> simulate a click on the refresh button
  # to trigger the data loading.
  observe({
    if (!input$interactive) {
      click('refresh')
    }
  })
}