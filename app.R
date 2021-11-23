library(dplyr)
library(ggplot2)
library(RCurl)
library(RMariaDB)
library(sf)
library(shiny)
library(shinyjs)
library(stringi)
library(tmap)
library(yaml)

message('DB_HOST: ', Sys.getenv('DB_HOST'))
message('DB_USER: ', Sys.getenv('DB_USER'))

options(shiny.sanitize.errors = FALSE)

config <- read_yaml('config.yaml')

query_db <- function(q) {
  con <- dbConnect(MariaDB(),
                   host=Sys.getenv('DB_HOST'),
                   port=as.integer(Sys.getenv('DB_PORT')),
                   user=Sys.getenv('DB_USER'),
                   password=Sys.getenv('DB_PASS'),
                   dbname=Sys.getenv('DB_NAME'),
                   bigint='integer')
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
  read.csv(text = result, stringsAsFactors=F) %>%
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
  q <- 'select name as parish_name, ST_AsBinary(geometry) as geometry from polygons;'
  df <- st_read(con, query=q, geometry_column='geometry')
  st_crs(df) <- 'urn:ogc:def:crs:EPSG::3857'
  dbDisconnect(con)
  st_make_valid(df)
}

disableIf <- function(condition, w) {
  if (condition) {
    disabled(w)
  } else {
    w
  }
}

hiddenIf <- function(condition, w) {
  if (condition) {
    hidden(w)
  } else {
    w
  }
}

submitIf <- function(condition) {
  if (condition) {
    list(
      actionButton(inputId = 'refresh', label='Refresh',
                   icon = icon('sync'), class='btn-primary')
    )
  } else {
    list()
  }
}

make_input <- function(x) {
  w <- switch(x$widget,
    'numericInput' = numericInput(inputId = x$name, label = x$description,
                                  value = x$value),
    'textInput' = textInput(inputId = x$name, label = x$description,
                            value = x$value),
    'textAreaInput' = textAreaInput(inputId = x$name, label = x$description,
                                    rows=x$rows, value=x$value)
  )
  disableIf(!is.null(x$value), w)
}

make_params_panel <- function(x) {
  do.call(
    conditionalPanel,
    c(list(
      condition = paste0('input.vis == "', x$name, '"')
      ),
      lapply(x$params, make_input)
    )
  )
}

resizeDetection <- function() {
    tags$head(tags$script('
        $(document).on("shiny:connected", function(e) {
            var window_dimensions = [1, 0];
            window_dimensions[0] = window.innerWidth;
            window_dimensions[1] = window.innerHeight;
            Shiny.onInputChange("window_dimensions", window_dimensions);
         });
         $(window).resize(function(e) {
             var window_dimensions = [0, 0];
             window_dimensions[0] = window.innerWidth;
             window_dimensions[1] = window.innerHeight;
             Shiny.onInputChange("window_dimensions", window_dimensions);
         });
    '))
}

ui <- function(request) {
    qp <- parseQueryString(request$QUERY_STRING)
    route = if (is.null(qp$route)) '' else qp$route
    interactive <- is.null(qp$vis)

    sb_widgets_lst <- config$visualizations
    for (n in names(sb_widgets_lst)) {
      sb_widgets_lst[[n]]$name <- n
    }
    if (!interactive) {
      for (i in 1:(length(sb_widgets_lst[[qp$vis]]$params))) {
        sb_widgets_lst[[qp$vis]]$params[[i]]$value <-
          qp[[sb_widgets_lst[[qp$vis]]$params[[i]]$name]]
      }
    }
    names(sb_widgets_lst) <- NULL
    sidebar_widgets = c(
      list(
        disableIf(!interactive,
          selectInput(
            inputId = 'vis',
            label = 'Visualization', 
            choices = c('Select...' = '',
                        setNames(sapply(sb_widgets_lst, function(x) x$name),
                                 sapply(sb_widgets_lst, function(x) x$description))),
            selected = qp$vis
          )
        )
      ),
      lapply(sb_widgets_lst, make_params_panel),
      list(
        conditionalPanel('input.vis.startsWith("map_")',
          selectInput(
            inputId = 'map_style',
            label = 'Style',
            choices = c('equal', 'fisher'),
            selected = 'fisher'
          ),
          selectInput(
            inputId = 'map_palette',
            label = 'palette',
            choices = c('YlOrRd', 'plasma'),
            selected = 'YlOrRd'
          )
        ),
        hiddenIf(!interactive,
          actionButton(inputId = 'refresh', label='Refresh',
                       icon = icon('sync'), class='btn-primary')
        ),
        hiddenIf(!interactive,
          uiOutput('link')
        )
      )
    )
    fluidPage(
      useShinyjs(),
      resizeDetection(), 
      titlePanel("FILTER Shiny App"),

      sidebarLayout(

        do.call(sidebarPanel, sidebar_widgets),

        mainPanel(
          tabsetPanel(
            tabPanel('Visualization',
              conditionalPanel('input.vis.startsWith("map_")',
                tagList(
                  tmapOutput('tmap'),
                  tags$script(HTML(paste0('
                      $(document).on("shiny:connected",
                                     function(e) {
                                       $("#',"tmap",'").height(window.innerHeight-125)
                                     })
                      $(window).resize(function(e) {
                        $("#',"tmap",'").height(window.innerHeight-125)
                      })
                    ')))
                )
              ),
              conditionalPanel('input.vis.startsWith("plot_")',
                tagList(
                  plotOutput('plot'),
                  tags$script(HTML(paste0('
                      $(document).on("shiny:connected",
                                     function(e) {
                                       $("#',"plot",'").height(window.innerHeight-125)
                                     })
                      $(window).resize(function(e) {
                        $("#',"plot",'").height(window.innerHeight-125)
                      })
                    ')))
                )
              )
            ),
            tabPanel('Data', DT::dataTableOutput('dt')),
            type = 'tabs'
          )
        )
      )
    )
}

server <- function(input, output, session) {
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
  output$tmap <- renderTmap({
    tm_shape(
      areas %>% left_join(df(), by=c('parish_name' = 'x'))
    ) + tm_polygons(col="y", id="parish_name",
                    palette=input$map_palette, style=input$map_style)
  })
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
        c(input$vis, sapply(params, function(x) input[[x$name]])),
        sep = '=', collapse = '&'
      )
    )
    tags$a(href=url, 'permalink')
  }) %>%
    bindEvent(input$refresh, ignoreNULL=T)
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
