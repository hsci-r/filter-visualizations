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

getvalue <- function (x) {
  if (!is.null(x$value)) {
    x$value
  } else {
    x$default
  }
}


make_input <- function(x, prefix='') {
  name = paste0(prefix, x$name)
  w <- switch(x$widget,
    'checkboxInput' = checkboxInput(inputId = name, label = x$description,
                                    value = ifelse(is.null(x$value),
                                                   FALSE, as.logical(x$value))),
    'numericInput' = numericInput(inputId = name, label = x$description,
                                  value = getvalue(x),
                                  step = ifelse('step' %in% names(x), x$step, 1)),
    'radioButtons' = radioButtons(inputId = name, label = x$description,
                                  choices = x$choices, selected = getvalue(x)),
    'selectInput' = selectInput(inputId = name, label = x$description,
                                choices=x$choices, selected=getvalue(x)),
    'selectizeInput' = selectizeInput(inputId = name, label = x$description,
                                      choices=c(x$value, x[['choices']]),
                                      selected=getvalue(x)),
    'textInput' = textInput(inputId = name, label = x$description,
                            value = getvalue(x)),
    'textAreaInput' = textAreaInput(inputId = name, label = x$description,
                                    rows=x$rows, value=getvalue(x))
  )
  w <- disableIf(!is.null(x$value), w)
  if (!is.null(x$condition)) {
    w <- conditionalPanel(x$condition, w)
  }
  w
}

make_vis_params_panel <- function(x) {
  do.call(
    conditionalPanel,
    c(list(
      condition = paste0('input.vis == "', x$name, '"'),
      helpText(x$helptext)
      ),
      lapply(x$params, function(y) make_input(y, prefix=paste0(x$name, '__')))
    )
  )
}

make_vistype_params_panel <- function(x) {
  do.call(
    conditionalPanel,
    c(list(
      condition = paste0('input.vis.startsWith("', x$name, '")')
      ),
      lapply(x$params, function(y) make_input(y, prefix=paste0(x$name, '__')))
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

resizeScript <- function(output) {
  tags$script(HTML(paste0('
      $(document).on("shiny:connected",
                     function(e) {
                       $("#',output,'").height(window.innerHeight-125)
                     })
      $(window).resize(function(e) {
        $("#',output,'").height(window.innerHeight-125)
      })
    ')))
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
      if (length(sb_vis_widgets_lst[[qp$vis]]$params) > 0) {
        for (i in 1:(length(sb_vis_widgets_lst[[qp$vis]]$params))) {
          sb_vis_widgets_lst[[qp$vis]]$params[[i]]$value <-
            qp[[sb_vis_widgets_lst[[qp$vis]]$params[[i]]$name]]
        }
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
        ),
        hidden(checkboxInput(inputId = 'interactive', label = "", value = interactive))
      ),
      lapply(sb_vis_widgets_lst, make_vis_params_panel),
      lapply(sb_vistype_widgets_lst, make_vistype_params_panel),
      list(
        downloadButton('dlData', 'Data'),
        disabled(downloadButton('dlMapPNG', 'Map (PNG)')),
        disabled(downloadButton('dlMapSVG', 'Map (SVG)')),
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
      shinybrowser::detect(),
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
                  resizeScript('tmap')
                )
              ),
              conditionalPanel('input.vis.startsWith("plot_")
                                || input.vis.startsWith("timeline_")',
                tagList(
                  plotOutput('plot'),
                  resizeScript('plot')
                )
              ),
              conditionalPanel('input.vis.startsWith("wordcloud_")',
                tagList(
                  wordcloud2Output('wordcloud', width="100%", height="100%"),
                  resizeScript('wordcloud')
                )
              ),
              conditionalPanel('input.vis.startsWith("tree_")',
                tagList(
                  plotlyOutput('tree'),
                  resizeScript('tree')
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
