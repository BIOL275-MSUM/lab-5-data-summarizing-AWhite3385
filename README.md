Lab 5 Assignment
================
Alec White
2021-02-23

objectives: The purpose of this lab was to explore the summarize
function along with making data sets and graphs with multiple numerical
and categorical variables.

``` r
#load packages
library(tidyverse)    
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.0.4     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
#iris data set pakages
iris <- as_tibble(iris) # so it prints a little nicer
```

Rename each variable so that it is all lower-case and uses an underscore
\_ instead of a period . in the name (the recommended coding style in
the tidyverse style guide). Print the resulting table.

``` r
# Question 1

i2 <-rename(iris, sepal_length = Sepal.Length,
       sepal_width = Sepal.Width,
       petal_length = Petal.Length,
       petal_width = Petal.Width,
       species = Species)

i2
```

    ## # A tibble: 150 x 5
    ##    sepal_length sepal_width petal_length petal_width species
    ##           <dbl>       <dbl>        <dbl>       <dbl> <fct>  
    ##  1          5.1         3.5          1.4         0.2 setosa 
    ##  2          4.9         3            1.4         0.2 setosa 
    ##  3          4.7         3.2          1.3         0.2 setosa 
    ##  4          4.6         3.1          1.5         0.2 setosa 
    ##  5          5           3.6          1.4         0.2 setosa 
    ##  6          5.4         3.9          1.7         0.4 setosa 
    ##  7          4.6         3.4          1.4         0.3 setosa 
    ##  8          5           3.4          1.5         0.2 setosa 
    ##  9          4.4         2.9          1.4         0.2 setosa 
    ## 10          4.9         3.1          1.5         0.1 setosa 
    ## # ... with 140 more rows

Convert the four numerical variables from cm to mm by multiplying by 10.
Print the resulting table.

``` r
#question 2

ni2 <-mutate(i2, sepal_length = sepal_length * 10,
       sepal_width = sepal_width * 10,
       petal_length = petal_length * 10,
       petal_width = petal_width * 10)

ni2
```

    ## # A tibble: 150 x 5
    ##    sepal_length sepal_width petal_length petal_width species
    ##           <dbl>       <dbl>        <dbl>       <dbl> <fct>  
    ##  1           51          35           14           2 setosa 
    ##  2           49          30           14           2 setosa 
    ##  3           47          32           13           2 setosa 
    ##  4           46          31           15           2 setosa 
    ##  5           50          36           14           2 setosa 
    ##  6           54          39           17           4 setosa 
    ##  7           46          34           14           3 setosa 
    ##  8           50          34           15           2 setosa 
    ##  9           44          29           14           2 setosa 
    ## 10           49          31           15           1 setosa 
    ## # ... with 140 more rows

Calculate sepal area and petal area (area is equal to length multiplied
by width). Print a table with only the variables sepal area, petal area,
and species.

``` r
#question 3

ia1 <-mutate(ni2, sepal_area = sepal_length * sepal_width,
       petal_area = petal_length * petal_width)

select(ia1, petal_area, sepal_area)
```

    ## # A tibble: 150 x 2
    ##    petal_area sepal_area
    ##         <dbl>      <dbl>
    ##  1         28       1785
    ##  2         28       1470
    ##  3         26       1504
    ##  4         30       1426
    ##  5         28       1800
    ##  6         68       2106
    ##  7         42       1564
    ##  8         30       1700
    ##  9         28       1276
    ## 10         15       1519
    ## # ... with 140 more rows

Calculate the following statistics for the entire dataset from the sepal
length variable and print the resulting table:

sample size, maximum value, minimum value, range, median, first quartile
(q1), third quartile (q2), inter-quartile range (iqr)

``` r
#question 4

ques4 <-summarize(ni2, sample_size = n(), 
          max = max(sepal_length),
          min = min(sepal_length),
          range = max - min ,
          median = median(sepal_length),
          q1 = quantile(sepal_length, probs = 0.25),
          q3 = quantile(sepal_length, probs = 0.75),
          iqr = q3-q1)

ques4
```

    ## # A tibble: 1 x 8
    ##   sample_size   max   min range median    q1    q3   iqr
    ##         <int> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
    ## 1         150    79    43    36     58    51    64    13

Calculate the following statistics for each species from the petal width
variable and print the resulting table:

sample size, mean, standard deviation, variance, standard error of the
mean, approximate 95% confidence interval

``` r
#question 5

ni2_grouped <-group_by(ni2, species)

ques5 <-summarize(ni2_grouped, sample_size = n(),
          petal_mean = mean(petal_width),
          st_dev = sd(petal_width),
          width_variance = var(petal_width),
          sem = sd(petal_width) / sqrt(sample_size),
          upper_ci = mean(petal_width) + 1.96 * sem,
          lower_ci = mean(petal_width) - 1.96 * sem)
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
ques5                           
```

    ## # A tibble: 3 x 8
    ##   species   sample_size petal_mean st_dev width_variance   sem upper_ci lower_ci
    ##   <fct>           <int>      <dbl>  <dbl>          <dbl> <dbl>    <dbl>    <dbl>
    ## 1 setosa             50       2.46   1.05           1.11 0.149     2.75     2.17
    ## 2 versicol~          50      13.3    1.98           3.91 0.280    13.8     12.7 
    ## 3 virginica          50      20.3    2.75           7.54 0.388    21.0     19.5

Visualize the relationship between petal width and species using a strip
plot.

``` r
#question 6

ggplot(data = ni2) +
        geom_jitter(mapping = aes(x = species, y = petal_length))
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Starting with the previous graph, add the mean and 95% confidence
interval for each species

``` r
#question 7

width_summary <-
        summarise(
                ni2_grouped, 
                petal_mean = mean(petal_width),
                sem = sd(petal_width) / sqrt(n()),
                upper_limit =  petal_mean + 1.96 * sem,
                lower_limit =  petal_mean - 1.96 * sem
        )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
width_summary
```

    ## # A tibble: 3 x 5
    ##   species    petal_mean   sem upper_limit lower_limit
    ##   <fct>           <dbl> <dbl>       <dbl>       <dbl>
    ## 1 setosa           2.46 0.149        2.75        2.17
    ## 2 versicolor      13.3  0.280       13.8        12.7 
    ## 3 virginica       20.3  0.388       21.0        19.5

``` r
ggplot(data = ni2) +
        geom_jitter(mapping = aes(x = species, y = petal_width)) +
        geom_crossbar(
                data = width_summary,
                mapping = aes(
                        x = species,
                        y = petal_mean,
                        ymax = upper_limit,
                        ymin = lower_limit
                ),
                color = "red"
        )
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Visualize the relationship between petal length, petal width, and
species using a scatterplot. Map the two numerical variables to the x
and y axes and map species to the color and shape aesthetics.

``` r
#question 8

ggplot(data = ni2) +
        geom_point(mapping = aes(x = petal_length, y = petal_width, color = species), alpha = 0.5)
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
sessioninfo::session_info()
```

    ## - Session info ---------------------------------------------------------------
    ##  setting  value                       
    ##  version  R version 4.0.3 (2020-10-10)
    ##  os       Windows 10 x64              
    ##  system   x86_64, mingw32             
    ##  ui       RTerm                       
    ##  language (EN)                        
    ##  collate  English_United States.1252  
    ##  ctype    English_United States.1252  
    ##  tz       America/Chicago             
    ##  date     2021-02-23                  
    ## 
    ## - Packages -------------------------------------------------------------------
    ##  package     * version date       lib source        
    ##  assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.0.3)
    ##  backports     1.2.0   2020-11-02 [1] CRAN (R 4.0.3)
    ##  broom         0.7.3   2020-12-16 [1] CRAN (R 4.0.3)
    ##  cellranger    1.1.0   2016-07-27 [1] CRAN (R 4.0.3)
    ##  cli           2.2.0   2020-11-20 [1] CRAN (R 4.0.3)
    ##  colorspace    2.0-0   2020-11-11 [1] CRAN (R 4.0.3)
    ##  crayon        1.3.4   2017-09-16 [1] CRAN (R 4.0.3)
    ##  DBI           1.1.0   2019-12-15 [1] CRAN (R 4.0.3)
    ##  dbplyr        2.0.0   2020-11-03 [1] CRAN (R 4.0.3)
    ##  digest        0.6.27  2020-10-24 [1] CRAN (R 4.0.3)
    ##  dplyr       * 1.0.2   2020-08-18 [1] CRAN (R 4.0.3)
    ##  ellipsis      0.3.1   2020-05-15 [1] CRAN (R 4.0.3)
    ##  evaluate      0.14    2019-05-28 [1] CRAN (R 4.0.3)
    ##  fansi         0.4.1   2020-01-08 [1] CRAN (R 4.0.3)
    ##  farver        2.0.3   2020-01-16 [1] CRAN (R 4.0.3)
    ##  forcats     * 0.5.0   2020-03-01 [1] CRAN (R 4.0.3)
    ##  fs            1.5.0   2020-07-31 [1] CRAN (R 4.0.3)
    ##  generics      0.1.0   2020-10-31 [1] CRAN (R 4.0.3)
    ##  ggplot2     * 3.3.3   2020-12-30 [1] CRAN (R 4.0.3)
    ##  glue          1.4.2   2020-08-27 [1] CRAN (R 4.0.3)
    ##  gtable        0.3.0   2019-03-25 [1] CRAN (R 4.0.3)
    ##  haven         2.3.1   2020-06-01 [1] CRAN (R 4.0.3)
    ##  hms           1.0.0   2021-01-13 [1] CRAN (R 4.0.3)
    ##  htmltools     0.5.0   2020-06-16 [1] CRAN (R 4.0.3)
    ##  httr          1.4.2   2020-07-20 [1] CRAN (R 4.0.3)
    ##  jsonlite      1.7.2   2020-12-09 [1] CRAN (R 4.0.3)
    ##  knitr         1.30    2020-09-22 [1] CRAN (R 4.0.3)
    ##  labeling      0.4.2   2020-10-20 [1] CRAN (R 4.0.3)
    ##  lifecycle     0.2.0   2020-03-06 [1] CRAN (R 4.0.3)
    ##  lubridate     1.7.9.2 2020-11-13 [1] CRAN (R 4.0.3)
    ##  magrittr      2.0.1   2020-11-17 [1] CRAN (R 4.0.3)
    ##  modelr        0.1.8   2020-05-19 [1] CRAN (R 4.0.3)
    ##  munsell       0.5.0   2018-06-12 [1] CRAN (R 4.0.3)
    ##  pillar        1.4.7   2020-11-20 [1] CRAN (R 4.0.3)
    ##  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.0.3)
    ##  purrr       * 0.3.4   2020-04-17 [1] CRAN (R 4.0.3)
    ##  R6            2.5.0   2020-10-28 [1] CRAN (R 4.0.3)
    ##  Rcpp          1.0.5   2020-07-06 [1] CRAN (R 4.0.3)
    ##  readr       * 1.4.0   2020-10-05 [1] CRAN (R 4.0.3)
    ##  readxl        1.3.1   2019-03-13 [1] CRAN (R 4.0.3)
    ##  reprex        0.3.0   2019-05-16 [1] CRAN (R 4.0.3)
    ##  rlang         0.4.10  2020-12-30 [1] CRAN (R 4.0.3)
    ##  rmarkdown     2.6     2020-12-14 [1] CRAN (R 4.0.3)
    ##  rstudioapi    0.13    2020-11-12 [1] CRAN (R 4.0.3)
    ##  rvest         0.3.6   2020-07-25 [1] CRAN (R 4.0.3)
    ##  scales        1.1.1   2020-05-11 [1] CRAN (R 4.0.3)
    ##  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 4.0.3)
    ##  stringi       1.5.3   2020-09-09 [1] CRAN (R 4.0.3)
    ##  stringr     * 1.4.0   2019-02-10 [1] CRAN (R 4.0.3)
    ##  tibble      * 3.0.4   2020-10-12 [1] CRAN (R 4.0.3)
    ##  tidyr       * 1.1.2   2020-08-27 [1] CRAN (R 4.0.3)
    ##  tidyselect    1.1.0   2020-05-11 [1] CRAN (R 4.0.3)
    ##  tidyverse   * 1.3.0   2019-11-21 [1] CRAN (R 4.0.3)
    ##  utf8          1.1.4   2018-05-24 [1] CRAN (R 4.0.3)
    ##  vctrs         0.3.6   2020-12-17 [1] CRAN (R 4.0.3)
    ##  withr         2.3.0   2020-09-22 [1] CRAN (R 4.0.3)
    ##  xfun          0.20    2021-01-06 [1] CRAN (R 4.0.3)
    ##  xml2          1.3.2   2020-04-23 [1] CRAN (R 4.0.3)
    ##  yaml          2.2.1   2020-02-01 [1] CRAN (R 4.0.3)
    ## 
    ## [1] C:/Users/alecw/OneDrive/Documents/R/win-library/4.0
    ## [2] C:/Program Files/R/R-4.0.3/library
