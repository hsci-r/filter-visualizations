library(dplyr)
library(ggplot2)
library(sf)
library(stringi)
library(tidyr)
library(yaml)
library(whisker)
library(wordcloud2)

source("R/data.R")
source("R/visualizations/map.R", local=T)
source("R/visualizations/timeline.R", local=T)
source("R/visualizations/typetree.R", local=T)
source("R/visualizations/wordcloud.R", local=T)

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
  # initialization: load config and necessary data from the DB
  config <- read_yaml('config.yaml')
  tmap_options(check.and.fix=T)
  con <- connect_to_db()
  maps <- read_maps(con)
  types <- read_types(con)
  place.poly <- read_place.poly(con)
  chq <- lapply(config$choices_queries, function(q) query_db(con, q))
  dbDisconnect(con)
  
  # data
  make_query <- reactive({
    v <- config$visualizations[[input$vis]]
    req(v$query)
    whisker.render(v$query, get_params(v$params, input))
  }) %>%
    bindEvent(input$refresh, ignoreNULL=T)

  get_data <- reactive({
    req(input$vis)
    t1 <- Sys.time()
    v <- config$visualizations[[input$vis]]
    url <- make_url(input, v$params, v$type, config$visualization_types[[v$type]]$params)
    if (v$source %in% c('csv', 'json')) {
      if ('url' %in% names(v)) {
      params_enc <- lapply(get_params(v$params, input),
          function(x) URLencode(as.character(x)))
      data_url <- whisker.render(v$url, params_enc)
      content <- getURL(data_url, .encoding='UTF-8')
      } else {
        content <- whisker.render(v$content, get_params(v$params, input))
      }
    }
    data <- switch(v$source,
      'csv'  = { df <- read.csv(text = content)
                 if (('group_by') %in% names(v)) {
                   if (('separate_by') %in% names(v))
                     df <- df %>% separate_rows(v$group_by, sep=v$separate_by)
                   df <- df %>% group_by_at(v$group_by) %>% summarize(y = n())
                 }
                 df
               },
      'json' = data.frame(jqr::jq(content, v$query) %>% yaml.load()),
      'sql'  = connect_and_query_db(make_query()),
    )
    t2 <- Sys.time()
    if (nchar(Sys.getenv('ENABLE_LOGGING_TO_DB')) > 0) {
      write_log_to_db(url, t2-t1, shinybrowser::get_user_agent())
    }
    data
  }) %>%
    bindEvent(input$refresh, ignoreNULL=T)

  # output visualizations
  tmap <- reactive(make_map(get_data(), input, maps, place.poly))
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
    values <- sapply(
      params,
      function(x) paste0(
        as.character(input[[paste0(input$vis, '__', x$name)]]),
        collapse=","))
    stri_replace_all_charclass(
      paste(c(input$vis, values), collapse='_'),
      '["*:\\p{WHITE_SPACE}]', '_'
    )
  })
  output$dlData <- downloadHandler(
    filename = function() paste0(substr(dl_filename(), 1, 50), '.csv'),
    content = function(file) {
      write.csv(get_data(), file, row.names=F)
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