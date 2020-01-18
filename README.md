dbreport
================

`dbreport` is a R-package that let’s you create automated reports of
database tables or R-objects (data.table, data.frame, tibble), by
summarising table meta data and counting distinct entries for all
(selected) columns. Therefore you can keep track of your ever growing
database tables in your project and avoid constantly runing queries such
as the one below, checking which distinct entries your columns have (at
least, this was my motivation for this package 😄).

``` sql
SELECT var1, count(var1) AS n
FROM tab1
GROUP BY var1
ORDER BY n DESC
```

For now [PostgreSQL](https://www.postgresql.org),
[MySQL](https://www.mysql.com) and
[SQLite](https://www.sqlite.org/index.html) databases are supported.
Adding methods for additional database libraries are welcomed\!

## Installation

``` r
remotes::install_github('andschar/dbreport') # not yet on CRAN
```

## Quickstart

We load dbreport and connect to our database (`con`).

``` r
require(dbreport)
con = DBI::dbConnect(drv = RPostgres::Postgres(),
                     dbname = 'myname',
                     host = 'host',
                     port = 'port',
                     user = 'user',
                     password = 'mysecretpassword',
                     bigint = 'integer')
```

Now we can create a report of any table therein. Let’s just do this for
the `information_schema.tables` table which is created by default for
any Postgres or MySQL database. In doing so, we choose to provide an
output directory (`output_dir =`), to provide a super meaningful title
(`title =`) and to add some introductory text (`text =`) to the report.
As our output format (`output_format =`) we choose `'html_document'`. We
could also take other [rmarkdown
formats](https://rmarkdown.rstudio.com/lesson-9.html), such as
`'pdf_document'`, `'github_document'`, `'html_vignette'` (for a very
lightweight output) etc.

``` r
dbreport(con = con,
         schema = 'information_schema',
         tbl = 'tables',
         output_dir = '/home/user/project1',
         title = 'Why not create an information_schem.tables report?!',
         text = 'Introductory text.', # you can alsp provide a file
         output_format = 'html_document')
```

Done\! The report together with a .csv file summarising the individual
columns is created in the output directory and we can examine it.

## More options

### Specific columns

We can also tweek our report a little more and choose only specific
columns (`col =`) to be included in the report and to use the
[treemap](https://github.com/mtennekes/treemap) instead of the
[ggplot2](https://github.com/tidyverse/ggplot2) package for ploting
(`plot_type =`). For this we access the publicly avaible Read-Only
[MySQL](https://www.mysql.com) [RNA-sequence
database](https://docs.rfam.org/en/latest/database.html) and run
`dbreport()` only on the *ncbi\_id* and *species* column of the
*taxonomy* table. In doing so we also output the report to a .pdf
(`output_format =`).

``` r
con = DBI::dbConnect(drv = RMySQL::MySQL(),
                     host = 'mysql-rfam-public.ebi.ac.uk',
                     user = 'rfamro',
                     port = 4497,
                     dbname = 'Rfam')
```

``` r
dbreport(con,
         schema = 'Rfam',
         tbl = 'taxonomy',
         col = c('ncbi_id', 'species'),
         plot_type = 'tree',
         output_dir = file.path(tempdir(), 'test_mysql'),
         output_format = 'pdf_document')
```

By default `dbreport()` closes the connection to a database and you
would have to reopen it for every new report you want to create. You can
turn off this behavior by setting `exit = FALSE`. This can be especially
useful if you want to create reports for multiple tables automatically,
for instance by using `mapply()`. After having created the reports
however, you should run `DBI::dbDisconnect(con)` to close the open
database connections.

``` r
mapply(dbreport::dbreport,
       tbl = c('table1', 'table2'), # vector of table names
       MoreArgs = list(con = con,
                       schema = 'schema1',
                       output_dir = tempdir(),
                       output_format = 'html_vignette',
                       exit = FALSE)
DBI::dbDisconnect(con)
```

### Column entries

We could as well check how often the entry `versiocolor` occurs (case is
ignored) in the *Species* column of the *iris* table by specifying the
`entry =` parameter.

``` r
dbreport(con = iris,
         entry = 'versicolor',
         output_dir = file.path(tempdir(), 'report_iris'),
         output_format = 'html_vignette',
         title = 'Iris report')
```
