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
    'radioButtons' = radioButtons(inputId = x$name, label = x$description,
                                  choices = x$choices, selected = x$default),
    'selectInput' = selectInput(inputId = x$name, label = x$description,
                                choices=x$choices, selected=x$default),
    'selectizeInput' = selectizeInput(inputId = x$name, label = x$description,
                                      choices=c(x$value, x[['choices']]),
                                      selected=x$value),
    'textInput' = textInput(inputId = x$name, label = x$description,
                            value = x$value),
    'textAreaInput' = textAreaInput(inputId = x$name, label = x$description,
                                    rows=x$rows, value=x$value)
  )
  disableIf(!is.null(x$value), w)
}

make_vis_params_panel <- function(x) {
  do.call(
    conditionalPanel,
    c(list(
      condition = paste0('input.vis == "', x$name, '"')
      ),
      lapply(x$params, make_input)
    )
  )
}

make_vistype_params_panel <- function(x) {
  do.call(
    conditionalPanel,
    c(list(
      condition = paste0('input.vis.startsWith("', x$name, '")')
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
    config <- read_yaml('config.yaml')
    qp <- parseQueryString(request$QUERY_STRING)
    route = if (is.null(qp$route)) '' else qp$route
    interactive <- is.null(qp$vis)

    sb_vis_widgets_lst <- config$visualizations
    for (n in names(sb_vis_widgets_lst)) {
      sb_vis_widgets_lst[[n]]$name <- n
    }
    if (!interactive) {
      for (i in 1:(length(sb_vis_widgets_lst[[qp$vis]]$params))) {
        sb_vis_widgets_lst[[qp$vis]]$params[[i]]$value <-
          qp[[sb_vis_widgets_lst[[qp$vis]]$params[[i]]$name]]
      }
    }
    names(sb_vis_widgets_lst) <- NULL

    sb_vistype_widgets_lst <- config$visualization_types
    for (n in names(sb_vistype_widgets_lst)) {
      sb_vistype_widgets_lst[[n]]$name <- n
    }
    names(sb_vistype_widgets_lst) <- NULL

    sidebar_widgets = c(
      list(
        disableIf(!interactive,
          selectInput(
            inputId = 'vis',
            label = 'Visualization', 
            choices = c('Select...' = '',
                        setNames(sapply(sb_vis_widgets_lst, function(x) x$name),
                                 sapply(sb_vis_widgets_lst, function(x) x$description))),
            selected = qp$vis
          )
        )
      ),
      lapply(sb_vis_widgets_lst, make_vis_params_panel),
      lapply(sb_vistype_widgets_lst, make_vistype_params_panel),
      list(
        downloadButton('dlData', 'Data'),
        disabled(downloadButton('dlMap', 'Map')),
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
      titlePanel("FILTER Visualizations"),

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
              ),
              conditionalPanel('input.vis.startsWith("tree_")',
                tagList(
                  plotlyOutput('tree'),
                  tags$script(HTML(paste0('
                      $(document).on("shiny:connected",
                                     function(e) {
                                       $("#',"tree",'").height(window.innerHeight-125)
                                     })
                      $(window).resize(function(e) {
                        $("#',"tree",'").height(window.innerHeight-125)
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
