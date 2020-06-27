ST 558 Project 2
================
Sarah McLaughlin
6/22/2020

# Introduction

# Data

Here, I will bring in the data that will be used in this project. With
the data, we are trying to predict the number of shares a particular
article will receive.

Some variables of note that will be used in our analysis and prediction:

1.  shares
      - (target variable)  
2.  weekday\_is\_ variables
      - (weekday published)  
3.  n\_tokens\_title
      - (number of words in title)  
4.  num\_keywords
      - (number of keywords in the metadata)  
5.  data\_channel\_is variables
      - (type of article)  
6.  kw\_max\_min
      - (worst keyword with max shares)  
7.  kw\_max\_max
      - (best keyword with max shares)  
8.  global\_rate\_positive\_words
      - (rate of positive words in content)  
9.  global\_rate\_negative\_words
      - (rate of negative words in content)

<!-- end list -->

``` r
data <- read_csv("OnlineNewsPopularity.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   url = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
#Look at column names  
attributes(data)$names
```

    ##  [1] "url"                           "timedelta"                    
    ##  [3] "n_tokens_title"                "n_tokens_content"             
    ##  [5] "n_unique_tokens"               "n_non_stop_words"             
    ##  [7] "n_non_stop_unique_tokens"      "num_hrefs"                    
    ##  [9] "num_self_hrefs"                "num_imgs"                     
    ## [11] "num_videos"                    "average_token_length"         
    ## [13] "num_keywords"                  "data_channel_is_lifestyle"    
    ## [15] "data_channel_is_entertainment" "data_channel_is_bus"          
    ## [17] "data_channel_is_socmed"        "data_channel_is_tech"         
    ## [19] "data_channel_is_world"         "kw_min_min"                   
    ## [21] "kw_max_min"                    "kw_avg_min"                   
    ## [23] "kw_min_max"                    "kw_max_max"                   
    ## [25] "kw_avg_max"                    "kw_min_avg"                   
    ## [27] "kw_max_avg"                    "kw_avg_avg"                   
    ## [29] "self_reference_min_shares"     "self_reference_max_shares"    
    ## [31] "self_reference_avg_sharess"    "weekday_is_monday"            
    ## [33] "weekday_is_tuesday"            "weekday_is_wednesday"         
    ## [35] "weekday_is_thursday"           "weekday_is_friday"            
    ## [37] "weekday_is_saturday"           "weekday_is_sunday"            
    ## [39] "is_weekend"                    "LDA_00"                       
    ## [41] "LDA_01"                        "LDA_02"                       
    ## [43] "LDA_03"                        "LDA_04"                       
    ## [45] "global_subjectivity"           "global_sentiment_polarity"    
    ## [47] "global_rate_positive_words"    "global_rate_negative_words"   
    ## [49] "rate_positive_words"           "rate_negative_words"          
    ## [51] "avg_positive_polarity"         "min_positive_polarity"        
    ## [53] "max_positive_polarity"         "avg_negative_polarity"        
    ## [55] "min_negative_polarity"         "max_negative_polarity"        
    ## [57] "title_subjectivity"            "title_sentiment_polarity"     
    ## [59] "abs_title_subjectivity"        "abs_title_sentiment_polarity" 
    ## [61] "shares"

``` r
#Select only certain variables  
data <- data %>% select(shares, contains("weekday_is_"), n_tokens_title, num_keywords, contains("data_channel_is"), kw_max_min, kw_max_max, global_rate_positive_words, global_rate_negative_words)

data
```

    ## # A tibble: 39,644 x 20
    ##    shares weekday_is_mond~ weekday_is_tues~ weekday_is_wedn~ weekday_is_thur~
    ##     <dbl>            <dbl>            <dbl>            <dbl>            <dbl>
    ##  1    593                1                0                0                0
    ##  2    711                1                0                0                0
    ##  3   1500                1                0                0                0
    ##  4   1200                1                0                0                0
    ##  5    505                1                0                0                0
    ##  6    855                1                0                0                0
    ##  7    556                1                0                0                0
    ##  8    891                1                0                0                0
    ##  9   3600                1                0                0                0
    ## 10    710                1                0                0                0
    ## # ... with 39,634 more rows, and 15 more variables: weekday_is_friday <dbl>,
    ## #   weekday_is_saturday <dbl>, weekday_is_sunday <dbl>, n_tokens_title <dbl>,
    ## #   num_keywords <dbl>, data_channel_is_lifestyle <dbl>,
    ## #   data_channel_is_entertainment <dbl>, data_channel_is_bus <dbl>,
    ## #   data_channel_is_socmed <dbl>, data_channel_is_tech <dbl>,
    ## #   data_channel_is_world <dbl>, kw_max_min <dbl>, kw_max_max <dbl>,
    ## #   global_rate_positive_words <dbl>, global_rate_negative_words <dbl>
