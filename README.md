# FILTER Visualizations

This repository contains the code of the
[FILTER Visualizations](https://filter-visualizations.rahtiapp.fi)
Web application. Currently the app supports mainly maps and treemap
type index visualizations, with some other types (barplots, timelines,
word clouds) being in early stages of development.

## Running or deploying

For quick testing, the application can be started from the R console
by executing:
```{r}
library(shiny)
runApp('.')
```

For deployment (e.g. on [Rahti](https://rahti.csc.fi)), the `Dockerfile`
should be used to build a Docker image.

### Runtime configuration

The application needs access to the FILTER database, which should be set
using the following environment variables:
* `DB_HOST` -- hostname,
* `DB_PORT` -- port (must be given even if it's the default `3306`),
* `DB_USER` -- username,
* `DB_PASS` -- password,
* `DB_NAME` -- name of the database.

In addition, if the variable `ENABLE_LOGGING_TO_DB` is set to any
non-empty value, the app will write logs about each executed query to
the table `visualizations_log`.

## `config.yaml`: Specifying available queries

The available visualizations are specified in a declarative way in the
file `config.yaml`. The idea is that concrete visualizations
can be added without touching the R code, just by specifying
the data source (e.g. an SQL query, or a URL from which to fetch data
in JSON/CSV format) and parameters, for which input widgets will be
generated in the user interface.

### Section `visualizations`

This section defines the concrete visualizations, consisting of a
visualization type (e.g. map) and data query. Below is an example:

```
visualizations:
  ...
  map_collector:
    type: map
    description: 'map: Number of poems by collector'
    source: sql
    query: |
      SELECT pol_pl.pol_id, pol.name, count(*) AS y
      FROM p_col
        JOIN collectors c ON p_col.col_id = c.col_id
        JOIN poems p ON p.p_id = p_col.p_id
        JOIN p_pl ON p_col.p_id = p_pl.p_id
        JOIN pol_pl ON p_pl.pl_id = pol_pl.pl_id
        JOIN polygons pol ON pol_pl.pol_id = pol.pol_id
      WHERE c.name = "{{{collector}}}"
        AND p.collection IN ({{{ collection }}})
      GROUP BY pol_pl.pol_id;
    params:
      - name: collector
        description: 'Collector name'
        widget: selectizeInput
        choices_query: collectors
      - name: collection
        description: 'Collection'
        widget: checkboxGroupInput
        choices: { skvr: '"skvr"', erab: '"erab"', jr: '"jr"', literary: '"literary"' }
        default: ['"skvr"', '"erab"', '"jr"']
        inline: true
  ...
```

The entry consists of a name of the visualization `map_collector` and
following attributes:
* `type`
* `description`
* `source` might be one of:
  * `sql` -- an SQL query to the FILTER database,
  * `json` -- data in JSON format, either provided directly in the property `content`, or fetched from an online source provided in the property `url`
  * `csv` -- data in CSV format, with source specified in the same way as for `json` (either by `content` or `url`)
* `query`
  * if `source: sql`, a [{{mustache}}](https://mustache.github.io/mustache.5.html) template for an SQL query, using the parameters defined below
  * if `source: json`, a [jq](https://manpages.org/jq) query for transforming the JSON data to a form needed by the visualization (e.g. for maps, aggregating the data by place)
* `url` - if `source: json` or `source: csv`, this is a [{{mustache}}](https://mustache.github.io/mustache.5.html) template producing an URL from which to fetch the data
* `content` - if `source: json` or `source: csv`, this is a [{{mustache}}](https://mustache.github.io/mustache.5.html) template the string that contains the data directly (e.g. for inputting a CSV table as a parameter); only used if `url` is not given
* `group_by`, `separate_by` - if `source: csv`, the content of the column specified by `separate_by` can be split on a given separator (e.g. if rows give a list of place IDs separated by `;`), and then aggregated by the column `group_by` (e.g. place ID)
* `params` - the list of parameters used in {{mustache}} templates; for each parameter, a corresponding input widget will be generated in the app; parameters have the following properties:
  * `name` - a short name used in the code
  * `description` - a longer name shown in the user interface
  * `widget` - widget type; currently one of: `checkboxInput`, `checkboxGroupInput`, `numericInput`, `radioButtons`, `selectInput`, `selectizeInput`, `textInput`, `textAreaInput`
  * `default` - default value
  * `helptext` - a longer text explaining what the parameter does (useful e.g. for SQL queries to describe the format of the results)
  * `choices`, `inline`, `rows` etc. - widget type-specific parameters (see Shiny documentation)
  * `choices_query` - see the section on `choices_queries` below.

### Section `visualization_types`

This section describes the available visualization types and their parameters.
Note that it is only used to specify input widgets for the parameters
as the implementation of the visualization itself is coded in the R code.
Thus, unlike the `visualizations` section, changes the content of this
section will likely require corresponding changes in the R code.

The parameters are specified in the same way as in the `visualizations` section.

### Section `choices_queries`

The widget `selectizeInput` allows for dynamic loading of the list of
choices on the server side. In the visualization app, there is a
possibility to specify an SQL query to load the choices from. The query
should return two columns: `label` (optional) and `value`.

For example, the following choices query is used to load the type index names:
```
types: 'SELECT CONCAT(name, " (", type_orig_id, ")") as label, type_orig_id as value
        FROM types;'
```

When specifying the input widget (in the `visualizations` section)
setting `choices_query: types` will cause the widget of type
`selectizeInput` to use the result of this query as list of choices. In
this case, the widget would allow picking a type from the type index
and return its ID.

## License

MIT (see [LICENSE](./LICENSE))

## Publications

The visualizations generated by this app have been used in various
FILTER-related publications, but the app itself hasn't been described
anywhere. In order to cite it, it is best to cite one of the articles
with a general description of the project, e.g.:
* Kati Kallio, Maciej Janicki, Eetu Mäkelä, Jukka Saarinen, Mari Sarv and Liina Saarlo (2023). Eteneminen omalla vastuulla: Lähdekriittinen laskennallinen näkökulma sähköisiin kansanrunoaineistoihin. *Elore* 30(1): 59-90.

Note that both the visualizations app and the underlying data is work
in progress and should not be relied on without consulting the FILTER
project members.
