library(dplyr)
library(ggplot2)
library(RCurl)
library(RMariaDB)
library(sf)
library(stringi)
library(tidyr)
library(yaml)
library(wordcloud2)

tree.colors <- rbind(
  c('#4169E1', '#5D7FE5', '#7A96EA', '#96ACEE'),
  c('#FFA500', '#FFB226', '#FFC04C', '#FFCD72'),
  c('#5D478B', '#75629C', '#8D7EAD', '#A599BF'),
  c('#008B45', '#269C60', '#4CAD7C', '#72BF98'),
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

write_log_to_db <- function(url, rtime, user_agent) {
  con <- connect_to_db()
  if (nchar(url) > 2000) {
    url <- paste0(substr(url, 1, 1997), '...')
  }
  dbAppendTable(con, 'visualizations_log',
                data.frame(url=url, rtime=as.double(rtime), user_agent=user_agent))
  dbDisconnect(con)
}

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

make_url <- function(input, params) {
  paste0(
    '/?',
    paste(
      c('vis', sapply(params, function(x) x$name)),
      c(input$vis,
        sapply(params,
               function(x) URLencode(
                   as.character(input[[paste0(input$vis, '__', x$name)]]),
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

read_areas <- function() {
  con <- connect_to_db()
  q <- 'select pol_id, name as parish_name, lang, ST_AsBinary(geometry) as geometry from polygons;'
  df <- st_read(con, query=q, geometry_column='geometry')
  st_crs(df) <- 'urn:ogc:def:crs:EPSG::3857'
  dbDisconnect(con)
  st_make_valid(df)
}

read_place.poly <- function() {
  query_db('SELECT pol_id, place_orig_id AS place_id FROM pol_pl NATURAL JOIN places;')
}

read_types <- function() {
  types <- query_db(paste(
    'SELECT ',
    '  t1.type_orig_id AS type_id_1, t1.name AS name_1, t2.type_orig_id as parent_1, ',
    '  t2.type_orig_id AS type_id_2, t2.name AS name_2, t3.type_orig_id as parent_2, ',
    '  t3.type_orig_id AS type_id_3, t3.name AS name_3, t4.type_orig_id as parent_3, ',
    '  t4.type_orig_id AS type_id_4, t4.name AS name_4 ',
    'FROM ',
    '  types t1',
    '  LEFT JOIN types t2 ON t1.par_id = t2.t_id',
    '  LEFT JOIN types t3 ON t2.par_id = t3.t_id',
    '  LEFT JOIN types t4 ON t3.par_id = t4.t_id',
    'WHERE t1.type_orig_id NOT LIKE "kt_%"',
    ';'
  ))
  types <- types %>%
    anti_join(types %>%
      filter(!is.na(parent_1)) %>%
      select(parent_1) %>%
      rename(type_id_1 = parent_1) %>%
      unique()) %>%
    mutate(
      parent_4 = NA_character_,
      tlc = case_when(
        !is.na(type_id_4) ~ type_id_4,
        !is.na(type_id_3) ~ type_id_3,
        !is.na(type_id_2) ~ type_id_2,
        grepl('erab_orig', type_id_1) | nchar(type_id_1) > 8 ~ NA_character_,
        !is.na(type_id_1) ~ type_id_1
      ),
      cat = case_when(
        tlc == 'skvr_t01' ~  1,
        tlc == 'skvr_t02' ~  2,
        tlc == 'skvr_t03' ~  3,
        tlc == 'skvr_t04' ~  4,
        tlc == 'skvr_t05' ~  5,
        tlc == 'skvr_t06' ~  6,
        tlc == 'skvr_t07' ~  7,
        tlc == 'skvr_t08' ~  8,
        tlc == 'erab_001' ~  1,
        tlc == 'erab_002' ~  2,
        tlc == 'erab_003' ~  3,
        tlc == 'erab_004' ~  4,
        tlc == 'erab_005' ~  5,
        tlc == 'erab_006' ~  6,
        tlc == 'erab_007' ~  7,
        tlc == 'erab_008' ~  10,
        tlc == 'erab_011' ~  8,
        tlc == 'erab_013' ~  9,
        TRUE              ~  11
      ),
    )
  types
}

insert_params <- function(string, input, params, prefix='') {
  ifelse(
    is.null(params),
    string,
    stri_replace_all_regex(
      string,
      paste0('@', sapply(params, function(x) x$name)),
      sapply(params, function(x) stri_replace_all_regex(
                                     input[[paste0(prefix, x$name)]],
                                     '\\$', '\\\\\\$')),
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
  types <- read_types()
  place.poly <- read_place.poly()
  
  # data
  make_query <- reactive({
    v <- config$visualizations[[input$vis]]
    insert_params(v$query, input, v$params, prefix=paste0(input$vis, '__'))
  }) %>%
    bindEvent(input$refresh, ignoreNULL=T)

  get_data <- reactive({
    req(input$vis)
    t1 <- Sys.time()
    v <- config$visualizations[[input$vis]]
    q <- make_query()
    url <- make_url(input, v$params)
    if (v$source == "octavo") {
      lvl <- insert_params(v$level, input, v$params, prefix=paste0(input$vis, '__'))
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

  # map
  tmap <- reactive({
    breaks <- sapply(unlist(stri_split_fixed(input$map__breaks, ',')), as.numeric)
    langs <- unlist(stri_split_fixed(input$map__region, ' '))
    df <- get_data()
    if (!('pol_id' %in% names(df))) {
      # if no polygon ID -- add them based on place IDs and regroup
      if ('place_id' %in% names(df)) {
        df <- df %>%
          inner_join(place.poly, by='place_id') %>%
          group_by(pol_id) %>%
          summarize(y = sum(y))
      } else {
        # if no place IDs -- match based on names as last resort
        df <- df %>%
          inner_join(areas %>% select(pol_id, parish_name),
                     by=c(place_name = 'parish_name')) %>%
          group_by(pol_id) %>%
          summarize(y = sum(y))
      }
    }
    tm_shape(
      areas %>%
        filter(lang %in% langs) %>%
        left_join(df, by='pol_id')
    ) + tm_polygons(col="y", id="parish_name",
                    palette=input$map__palette, style=input$map__style,
                    n = input$map__classes, breaks=breaks,
                    title = input$map__var, textNA="\u2014"
    ) + tm_layout(title = input$map__title,
                  legend.format = list(text.separator='\u2013'))
  })
  output$tmap <- renderTmap({ tmap() })

  # type tree
  output$tree <- renderPlotly({
    v <- config$visualizations[[input$vis]]
    df <- get_data()
    # FIXME workaround -- in Octavo the field is still called "theme_id"
    if ('theme_id' %in% names(df)) {
      df <- df %>% rename(type_id = theme_id)
    }
    # join with the type hierarchy
    df2 <- df %>%
      inner_join(types, by = c(type_id = 'type_id_1'), suffix=c('.x', ''))
    # compute sums for the higher hierarchy levels
    df3 <- df2 %>%
      select(type_id, name = name_1, parent = parent_1,
             y = y, cat = cat) %>%
      mutate(level = 1) %>%
      union(df2 %>%
        filter(!is.na(type_id_2)) %>%
        group_by(type_id = type_id_2) %>%
        summarize(name = first(name_2), parent = first(parent_2),
                  y = sum(y), cat = first(cat), level = 2)) %>%
      union(df2 %>%
        filter(!is.na(type_id_3)) %>%
        group_by(type_id = type_id_3) %>%
        summarize(name = first(name_3), parent = first(parent_3),
                  y = sum(y), cat = first(cat), level = 3)) %>%
      union(df2 %>%
        filter(!is.na(type_id_4)) %>%
        group_by(type_id = type_id_4) %>%
        summarize(name = first(name_4), parent = first(parent_4),
                  y = sum(y), cat = first(cat), level = 4)) %>%
      group_by(type_id) %>%
      summarize(name = first(name), parent = first(parent),
                y = sum(y), cat = first(cat), level = max(level)) %>%
      arrange(desc(level), type_id) %>%
      mutate(color = tree.colors[cbind(cat, level)],
             fontcolor = tree.fontcolors[cat])
    # plot
    plot_ly(df3, ids=~type_id, labels=~name, parents=~parent, values=~y,
            type=input$tree__type, branchvalues='total',
            marker=list(colors=~color),
            textfont=list(color=~fontcolor),
            hoverlabel=list(font=list(color=~fontcolor)))
  })

  # other outputs
  output$plot <- renderPlot({
    v <- config$visualizations[[input$vis]]
    switch(v$type,
      'barplot' = ggplot(get_data(), aes(x, y))
                  + geom_bar(stat='identity') + coord_flip(),
      'timeline' = plot_timeline(get_data(), input$timeline__min,
                                 input$timeline__max, input$timeline__by)
    )
  }) %>% bindEvent(input$refresh, ignoreNULL=T)
  output$wordcloud <- renderWordcloud2({
    wordcloud2(get_data() %>%
      filter(y >= input$wordcloud__min_freq) %>%
      mutate(y = as.integer(exp(input$wordcloud__scale*log(y))))
    )
  })

  output$dt <- DT::renderDataTable(get_data())
  output$query <- renderText(make_query())
  output$link <- renderUI({
    params <- config$visualizations[[input$vis]]$params
    url <- make_url(input, params)
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