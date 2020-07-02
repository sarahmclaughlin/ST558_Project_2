Project 2 Render
================
Sarah McLaughlin
6/30/2020

``` r
library(rmarkdown)
library(tidyverse)

days <- c("tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")

output_file <- paste0(days, ".md")

params = lapply(days, FUN = function(x){list(day = x)})

reports <- tibble(output_file, params)

apply(reports, MARGIN = 1, 
      FUN = function(x){
        render(input = "README.Rmd", output_file = x[[1]], params = x[[2]])
      })
```
