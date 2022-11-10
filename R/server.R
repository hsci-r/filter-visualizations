library(dplyr)
library(ggplot2)
library(RCurl)
library(RMariaDB)
library(sf)
library(stringi)
library(yaml)

tree.colors <- rbind(
  c('#4169E1', '#5D7FE5', '#7A96EA', '#96ACEE'),
  c('#FFA500', '#FFB226', '#FFC04C', '#FFCD72'),
  c('#6CA6CD', '#82B3D4', '#98C0DC', '#AECEE3'),
  c('#22BB44', '#43C560', '#64CF7C', '#85D998'),
  c('#DDA0DD', '#E2AEE2', '#E7BCE7', '#ECCAEC'),
  c('#DD0000', '#E22626', '#E74C4C', '#EC7272'),
  c('#8B4C39', '#9C6656', '#AD8174', '#BF9C92'),
  c('#FA8072', '#FA9387', '#FBA69C', '#FCB9B1'),
  c('#D02090', '#D741A0', '#DE62B1', '#E584C1'),
  c('#556B2F', '#6E814E', '#88976D', '#A1AD8C'),
  c('#8B8989', '#9C9A9A', '#ADACAC', '#BFBEBE')
)
tree.fontcolors <- c(
  'white', 'black', 'white', 'white',
  'black', 'white', 'white', 'black',
  'white', 'white', 'white'
)

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

query_octavo <- function(endpoint, query, level, fields, grouping.var, limit=20, offset=0) {
  URL <- paste0(
    endpoint,
    'search??pretty&endpoint=',
    URLencode(endpoint, reserved=T),
    '&fieldEnricher=&offsetDataConverter=&query=',
    URLencode(query, reserved=T),
    paste0('&field=', fields, collapse=''),
    '&offset=', offset, '&limit=', limit,
    '&contextLevel=Sentence&contextExpandLeft=0&contextExpandRight=0',
    '&level=', level, '&sort=score&sortDirection=D')
  # did you know: JSON is also YAML
  r <- yaml.load(getURL(URL, .encoding='UTF-8')) 
  x <- unlist(lapply(r$result$docs, function(d) d[[grouping.var]]))
  # for place names: "Parish, County" -> "Parish"
  if (grouping.var == 'place_name') {
    x <- stri_replace_all_regex(x, ', .*', '')
  }
  data.frame(x = x) %>%
    group_by(x) %>%
    summarize(y = n())
}

read_areas <- function() {
  con <- connect_to_db()
  q <- 'select name as parish_name, lang, ST_AsBinary(geometry) as geometry from polygons;'
  df <- st_read(con, query=q, geometry_column='geometry')
  st_crs(df) <- 'urn:ogc:def:crs:EPSG::3857'
  dbDisconnect(con)
  st_make_valid(df)
}

read_themes <- function() {
  query_db(paste0('select t.theme_id, t.name, tp.theme_id as parent ',
                  'from themes t left join themes tp on t.par_id = tp.t_id;'))
}

insert_params <- function(string, input, params) {
  ifelse(
    is.null(params),
    string,
    stri_replace_all_regex(
      string,
      paste0('@', sapply(params, function(x) x$name)),
      sapply(params, function(x) input[[x$name]]),
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
  themes <- read_themes()
  is.interactive <- reactive(as.logical(input$interactive))

  # data
  df <- reactive({
    req(input$vis)
    v <- config$visualizations[[input$vis]]
    q <- insert_params(v$query, input, v$params)
    if (v$source == "octavo") {
      lvl <- insert_params(v$level, input, v$params)
    }
    switch(v$source,
      'csv' = read.csv(text = q),
      'sql' = query_db(q),
      'octavo' = query_octavo(config$global$octavo_endpoint, q,
                              lvl, v$fields, v$group_by, limit=-1)
    )
  }) %>%
  bindEvent(input$refresh, ignoreNULL=F)

  # map
  tmap <- reactive({
    breaks <- sapply(unlist(stri_split_fixed(input$map_breaks, ',')), as.numeric)
    langs <- unlist(stri_split_fixed(input$map_region, ' '))
    tm_shape(
      areas %>%
        filter(lang %in% langs) %>%
        left_join(df(), by=c('parish_name' = 'x'))
    ) + tm_polygons(col="y", id="parish_name",
                    palette=input$map_palette, style=input$map_style,
                    n = input$map_classes, breaks=breaks,
                    title = input$map_var, textNA="\u2014"
    ) + tm_layout(title = input$map_title,
                  legend.format = list(text.separator='\u2013'))
  })
  output$tmap <- renderTmap({ tmap() })

  # type tree
  output$tree <- renderPlotly({
    v <- config$visualizations[[input$vis]]
    df <- df()
    if (v$source == "octavo") {
      # Remove non-leaf types.
      # Frequencies of upper levels need to be to be recomputed because
      # Octavo counts a higher-level category only once, no matter
      # how many of its subcategories occur in an entry.
      df <- df %>%
        anti_join(themes %>% filter(!is.na(parent)) %>%
                  select(parent) %>% rename(x = parent) %>% unique())
    }
    # join with the table of all types (to have the info about the hierarchy)
    df2 <- themes %>%
      left_join(df, by=c('theme_id' = 'x'), suffix=c('', '.y'))
    # compute totals for categories higher in the hierarchy
    df3 <- df2 %>%
      left_join(df2, by=c('theme_id' = 'parent'),
                suffix=c('', '.1'), na_matches='never') %>%
      left_join(df2, by=c('theme_id.1' = 'parent'),
                suffix=c('', '.2'), na_matches='never') %>%
      left_join(df2, by=c('theme_id.2' = 'parent'),
                suffix=c('', '.3'), na_matches='never') %>%
      mutate(category = case_when(
        grepl('skvr_t01.*', theme_id) ~ 1,
        grepl('skvr_t02.*', theme_id) ~ 2,
        grepl('skvr_t03.*', theme_id) ~ 3,
        grepl('skvr_t04.*', theme_id) ~ 4,
        grepl('skvr_t05.*', theme_id) ~ 5,
        grepl('skvr_t06.*', theme_id) ~ 6,
        grepl('skvr_t07.*', theme_id) ~ 7,
        grepl('skvr_t08.*', theme_id) ~ 8,
        grepl('erab_001.*', theme_id) ~ 1,
        grepl('erab_002.*', theme_id) ~ 2,
        grepl('erab_003.*', theme_id) ~ 3,
        grepl('erab_004.*', theme_id) ~ 4,
        grepl('erab_005.*', theme_id) ~ 5,
        grepl('erab_006.*', theme_id) ~ 6,
        grepl('erab_007.*', theme_id) ~ 7,
        grepl('erab_008.*', theme_id) ~ 10,
        grepl('erab_011.*', theme_id) ~ 8,
        grepl('erab_013.*', theme_id) ~ 9,
        TRUE ~ 11
      ),
      level = 1 + (!is.na(theme_id.1))
              + (!is.na(theme_id.2)) + (!is.na(theme_id.3))) %>%
      group_by(theme_id, name, parent) %>%
      summarize(y = sum(y, na.rm=T) + sum(y.1, na.rm=T)
                + sum(y.2, na.rm=T) + sum(y.3, na.rm=T),
                level=max(level), category=min(category)) %>%
      mutate(color = tree.colors[cbind(category, level)],
             fontcolor = tree.fontcolors[category]) %>%
      filter(y > 0)
    plot_ly(df3, ids=~theme_id, labels=~name, parents=~parent, values=~y,
            type=input$tree_type, branchvalues='total',
            marker=list(colors=~color),
            textfont=list(color=~fontcolor),
            hoverlabel=list(font=list(color=~fontcolor)))
  })

  # other outputs
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