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

query_db <- function(con, q) {
  res <- dbSendQuery(con, q)
  data <- dbFetch(res)
  dbClearResult(res)
  data
}

connect_and_query_db <- function(q) {
  con <- connect_to_db()
  result <- query_db(con, q)
  dbDisconnect(con)
  result
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

