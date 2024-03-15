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

