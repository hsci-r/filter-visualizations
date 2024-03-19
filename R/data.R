library(RCurl)
library(RMariaDB)

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

