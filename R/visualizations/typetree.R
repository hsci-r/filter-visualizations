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

read_types <- function(con) {
  types <- query_db(con, paste(
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

make_type_tree <- function(df, input, types) {
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
}
