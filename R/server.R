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