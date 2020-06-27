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

## Read in and filter data by day

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
#Select only certain variables for a specific day  
data <- data %>% filter(weekday_is_monday == 1) %>% 
  select(shares, n_tokens_title, num_keywords, contains("data_channel_is"), kw_max_min, kw_max_max, global_rate_positive_words, global_rate_negative_words) %>% collect()

data
```

    ## # A tibble: 6,661 x 13
    ##    shares n_tokens_title num_keywords data_channel_is~ data_channel_is~
    ##     <dbl>          <dbl>        <dbl>            <dbl>            <dbl>
    ##  1    593             12            5                0                1
    ##  2    711              9            4                0                0
    ##  3   1500              9            6                0                0
    ##  4   1200              9            7                0                1
    ##  5    505             13            7                0                0
    ##  6    855             10            9                0                0
    ##  7    556              8           10                1                0
    ##  8    891             12            9                0                0
    ##  9   3600             11            7                0                0
    ## 10    710             10            5                0                0
    ## # ... with 6,651 more rows, and 8 more variables: data_channel_is_bus <dbl>,
    ## #   data_channel_is_socmed <dbl>, data_channel_is_tech <dbl>,
    ## #   data_channel_is_world <dbl>, kw_max_min <dbl>, kw_max_max <dbl>,
    ## #   global_rate_positive_words <dbl>, global_rate_negative_words <dbl>

## Exploratory Data Analysis

Here, I will do a basic analysis of my variables to see basic trends,
correlations, and if the data needs to be standardized.

*Basic Summary Statistics*

``` r
sumStatsData <- data %>% select(-shares) %>% collect()  

mat <- apply(sumStatsData, 2, summary, digits = 2)

mat 
```

    ##         n_tokens_title num_keywords data_channel_is_lifestyle
    ## Min.                 2          1.0                     0.000
    ## 1st Qu.              9          6.0                     0.000
    ## Median              10          7.0                     0.000
    ## Mean                10          7.1                     0.048
    ## 3rd Qu.             12          9.0                     0.000
    ## Max.                18         10.0                     1.000
    ##         data_channel_is_entertainment data_channel_is_bus
    ## Min.                              0.0                0.00
    ## 1st Qu.                           0.0                0.00
    ## Median                            0.0                0.00
    ## Mean                              0.2                0.17
    ## 3rd Qu.                           0.0                0.00
    ## Max.                              1.0                1.00
    ##         data_channel_is_socmed data_channel_is_tech data_channel_is_world
    ## Min.                     0.000                 0.00                   0.0
    ## 1st Qu.                  0.000                 0.00                   0.0
    ## Median                   0.000                 0.00                   0.0
    ## Mean                     0.051                 0.19                   0.2
    ## 3rd Qu.                  0.000                 0.00                   0.0
    ## Max.                     1.000                 1.00                   1.0
    ##         kw_max_min kw_max_max global_rate_positive_words
    ## Min.             0          0                      0.000
    ## 1st Qu.        440     840000                      0.028
    ## Median         650     840000                      0.039
    ## Mean          1200     750000                      0.039
    ## 3rd Qu.       1000     840000                      0.050
    ## Max.        300000     840000                      0.140
    ##         global_rate_negative_words
    ## Min.                        0.0000
    ## 1st Qu.                     0.0097
    ## Median                      0.0150
    ## Mean                        0.0170
    ## 3rd Qu.                     0.0220
    ## Max.                        0.0920

*Correlation Plot*

``` r
correlation <- cor(data, method = "spearman")

corrplot(correlation, type = "upper", tl.pos = "lt")
corrplot(correlation, type = "lower", method = "number", add = TRUE, diag = FALSE, tl.pos= "n")
```

![](README_files/figure-gfm/corr%20plot-1.png)<!-- -->

## Make Train and Test Set

``` r
# 
set.seed(130)
# Set indices
train <- sample(1:nrow(data), size = 0.7)
test <- setdiff(1:nrow(data), train)

# Make Train and Test Sets  

dataTrain <- data[train, ]
dataTest <- data[test, ]
```

# Linear Regression Model

I will begin by running a regression model with all of the variables.

``` r
allVarFit <- lm(shares ~., data)

allVarFit
```

    ## 
    ## Call:
    ## lm(formula = shares ~ ., data = data)
    ## 
    ## Coefficients:
    ##                   (Intercept)                 n_tokens_title  
    ##                     4.122e+03                      4.831e+01  
    ##                  num_keywords      data_channel_is_lifestyle  
    ##                     1.395e+02                     -2.620e+03  
    ## data_channel_is_entertainment            data_channel_is_bus  
    ##                    -3.981e+03                     -2.890e+03  
    ##        data_channel_is_socmed           data_channel_is_tech  
    ##                    -2.685e+03                     -4.111e+03  
    ##         data_channel_is_world                     kw_max_min  
    ##                    -4.487e+03                      5.581e-02  
    ##                    kw_max_max     global_rate_positive_words  
    ##                     1.534e-03                      2.485e+02  
    ##    global_rate_negative_words  
    ##                     2.897e+03
