Iterantion and List Columns
================
Purnima Sharma
11/8/2020

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,          
  out.width = "90%"     
)
theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis", 
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_color_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

\#\#Lists (you can put anything in a list)

``` r
l = list(
  vec_numeric = 5:8,                 #numeric vector
  mat = matrix(1:8, 2, 4),           #2 rows by 4 col matrix of digits 1 to 8
  vec_logical = c(TRUE, FALSE),      #logical vector
  summary     = summary(rnorm(100))) #sample from a normal distr of size 100.
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $vec_logical
    ## [1]  TRUE FALSE
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -2.58018 -0.74700  0.04807 -0.01170  0.70321  2.72135

``` r
l$vec_numeric         #accessing object in the list, OR could use:
```

    ## [1] 5 6 7 8

``` r
l[[1]]                #1st object of the list, OR 3rd way:
```

    ## [1] 5 6 7 8

``` r
l[["vec_numeric"]]
```

    ## [1] 5 6 7 8

``` r
mean(l[["vec_numeric"]])   #take do operations on vectors of list
```

    ## [1] 6.5

\#\#“for” loops

Creating a new list, list of samples from a normal distr.

``` r
list_norms =          # list name.
  list(
    a = rnorm(20, 3, 1),
    b = rnorm(30, 0, 5),
    c = rnorm(40, 10, .2),
    d = rnorm(20, -3, 1)
  )
```

``` r
list_norms[[1]]     #output is 1st object of the list (object a).
```

    ##  [1] 3.6610944 3.1319033 4.0872995 2.9387357 4.0224695 2.1677515 2.6857643
    ##  [8] 3.0612312 2.8667935 2.8896135 1.4242940 1.1207069 3.9083694 1.7852409
    ## [15] 0.6663897 2.8474894 2.2442546 1.1223551 2.5260501 2.9159092

``` r
list_norms          # output all objects of list
```

    ## $a
    ##  [1] 3.6610944 3.1319033 4.0872995 2.9387357 4.0224695 2.1677515 2.6857643
    ##  [8] 3.0612312 2.8667935 2.8896135 1.4242940 1.1207069 3.9083694 1.7852409
    ## [15] 0.6663897 2.8474894 2.2442546 1.1223551 2.5260501 2.9159092
    ## 
    ## $b
    ##  [1]   4.5505072   6.4997882   7.0057950  -1.0620332  -3.0037692  -3.2690503
    ##  [7]  -6.9205499   5.7777524   0.2423301  11.7706786 -11.4824970  -7.1135888
    ## [13]   0.9129043  -5.5302050  -7.8804966   8.5442873  -6.3558324   2.7048774
    ## [19]   3.4256626  -4.2186144  -1.9022250   0.2925386  -3.9775400   0.6439156
    ## [25]  -2.1976809  -9.2072738  -3.4531339 -14.6181861 -11.1230609  -0.3556556
    ## 
    ## $c
    ##  [1]  9.958823 10.265810  9.658536  9.852343  9.869613 10.066862  9.730645
    ##  [8] 10.114002 10.156743  9.733637  9.758531  9.847669  9.915376  9.920926
    ## [15]  9.862006 10.082136  9.774492  9.651833 10.009043 10.166924 10.138164
    ## [22] 10.415122 10.113726 10.249273 10.060151  9.766932 10.357697  9.814323
    ## [29] 10.182753  9.789539  9.726656 10.140883  9.667418  9.765870  9.668702
    ## [36] 10.276803 10.264272 10.247362  9.349843  9.915452
    ## 
    ## $d
    ##  [1] -1.223983 -3.109122 -3.966795 -2.434744 -1.979570 -3.109007 -4.651010
    ##  [8] -4.200212 -1.877265 -2.598877 -1.734364 -3.443617 -4.523489 -3.916555
    ## [15] -2.828332 -4.051873 -2.518728 -1.814581 -2.373815 -1.833310

# Using mean\_and\_sd function from “writing\_functions” module to each element of this list.

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("input be numeric")
  } else if (length(x) < 3) {
    stop("input must have 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}

# applying function to each object of list.

mean_and_sd(list_norms[[1]])  #getting mean and SD of 1st object, first way.
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.60 0.986

``` r
mean_and_sd(list_norms[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.71  6.30

``` r
mean_and_sd(list_norms[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.96 0.239

``` r
mean_and_sd(list_norms[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.91  1.04

``` r
# 2nd way: Same process as above (lines 58-61) using "for" loop (loop approach):

output = vector("list", length = 4) #create output list of vector, same length as input list. This is empty right now.

for (i in 1:4) {
  output[[i]] = mean_and_sd(list_norms[[i]])
}


# Or, 3rd way, using "maps" in package "purrr" - most clean (map approach):

output = map(list_norms, mean_and_sd)  

#first argument to map is the input list (or vector, or data frame) we want to iterate.
#second argument is the function we want to use to each object in list.

output = map(.x = list_norms, ~ mean_and_sd(.x)) 
# does the same thing as line73, with .x as place holder.
```

example of loop versus map approach

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  output[[i]] = median(list_norms[[i]])
}

output = map(list_norms, median)
# output = map(.x = list_norms, ~ median(.x))   alternative way using place-holder.

#Or, to find something else:
output = map(list_norms, IQR)
```

# map variants

``` r
output = map_dbl(list_norms, median)   #output will be vector of numerics, vs a list.
output = map_dbl(list_norms, median, .id = "input") #id is input,worked same as above though
```

Making each output of mean and sd a df (tibble)

``` r
output = map_df(list_norms, mean_and_sd)
```
