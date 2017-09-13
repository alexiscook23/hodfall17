Table Assignments
================

``` r
# R script to randomize class and place them at tables
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
class<-read_csv("../../classlist.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   last_name = col_character(),
    ##   first_name = col_character(),
    ##   ghid = col_character()
    ## )

``` r
class["random"]<-runif(dim(class)[1])

class<-class%>%arrange(random)

class["index"]<-seq(1:dim(class)[1])

class<-class%>%mutate(table=cut(index,6,(1:6)))

class$rmse<-NA

print(select(class,first_name,last_name,table),n=100)
```

    ## # A tibble: 19 × 3
    ##    first_name  last_name  table
    ##         <chr>      <chr> <fctr>
    ## 1       Susan       Cobb      1
    ## 2       James  Michaelis      1
    ## 3       Ethan      Polan      1
    ## 4        Siqi       Chen      1
    ## 5      Brenda         Lu      2
    ## 6       Raven       Delk      2
    ## 7     William   Sullivan      2
    ## 8      Rachel      Anand      3
    ## 9      Connor       Kreb      3
    ## 10     Claire    Fogarty      3
    ## 11      Henry Livingston      4
    ## 12       Cole      Smith      4
    ## 13        Ben     Scheer      4
    ## 14       Jack     Cramer      5
    ## 15       C.J.       Pond      5
    ## 16      Arjun       Shah      5
    ## 17      Sunny        Cao      6
    ## 18      Katie      Means      6
    ## 19     Alexis       Cook      6

``` r
names(class)
```

    ## [1] "last_name"  "first_name" "ghid"       "random"     "index"     
    ## [6] "table"      "rmse"

Kaggle Style Results
--------------------

Bar Graph
---------

``` r
add_result<-function(df,group_number,rmse){
  df$rmse[df$table==group_number]<-rmse
  df
  }

new_rmse<-10000

class<-add_result(class,1,new_rmse)
class<-add_result(class,2,new_rmse)
class<-add_result(class,3,new_rmse)
class<-add_result(class,4,new_rmse)
class<-add_result(class,5,new_rmse)
class<-add_result(class,6,new_rmse)

class_summary<-class%>%group_by(table)%>%
  summarize(current_rmse=mean(rmse))

gg<-ggplot(class_summary,aes(x=table,y=current_rmse,fill=table))
gg<-gg+geom_bar(stat="identity",position=position_dodge())
gg
```

![](table_assignments_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-1.png)
