ST 558 Project 2
================
Sarah McLaughlin
6/22/2020

  - [Links to Other MD Files by Day](#links-to-other-md-files-by-day)
  - [Introduction](#introduction)
  - [Data](#data)
      - [Read in data](#read-in-data)
      - [Exploratory Data Analysis](#exploratory-data-analysis)
      - [Filter Data to Only Include these
        Variables](#filter-data-to-only-include-these-variables)
      - [Create Scatterplots of these Variables Against
        Shares](#create-scatterplots-of-these-variables-against-shares)
      - [Select only needed variables from data for specific
        day](#select-only-needed-variables-from-data-for-specific-day)
      - [Make Train and Test Set](#make-train-and-test-set)
  - [Linear Regression Model](#linear-regression-model)
      - [Comparison of Two Models](#comparison-of-two-models)
      - [Analysis](#analysis-4)
      - [Comparison of all 5 Models](#comparison-of-all-5-models)
  - [Ensemble Model](#ensemble-model)
      - [Fix Train and Test Data](#fix-train-and-test-data)
  - [Models Used](#models-used)

# Links to Other MD Files by Day

The analysis for Monday is available [here](monday.md). The analysis for
Tuesday is available [here](tuesday.md). The analysis for Wednesday is
available [here](wednesday.md). The analysis for Thursday is available
[here](thursday.md). The analysis for Friday is available
[here](friday.md). The analysis for Saturday is available
[here](saturday.md). The analysis for Sunday is available
[here](sunday.md).

# Introduction

The data that will be used in this project is from the *Online News
Popularity Data Set* from the *UCI Machine Learning Repository*. The
goal of this project is to create two models (one a linear model, the
other an ensemble model) that will be used to predict the number of
shares/the probability/if an article has more than 1400 shares. How I
picked which variables is detailed below.

The data is from Mashable (www.mashable.com) and contains the statistics
for articles that were written and published on their website. There are
statistics for 39,645 articles.

In this project, I will attempt to create a linear regression model for
the data, comparing the Adjusted R Squared values of the models. Due to
the very low Adjusted R Squared models, I will instead move to a
logistic model. These models produce very small RMSEs.

I will also fit a Random Forest Classification model to the data. I have
attempted a few different Random Forest Models but due to computing
speed, have only included two models.

# Data

Here, I will bring in the data that will be used in this project. With
the data, we are trying to predict the number of shares a particular
article will receive.

## Read in data

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

    ##  [1] "url"                          
    ##  [2] "timedelta"                    
    ##  [3] "n_tokens_title"               
    ##  [4] "n_tokens_content"             
    ##  [5] "n_unique_tokens"              
    ##  [6] "n_non_stop_words"             
    ##  [7] "n_non_stop_unique_tokens"     
    ##  [8] "num_hrefs"                    
    ##  [9] "num_self_hrefs"               
    ## [10] "num_imgs"                     
    ## [11] "num_videos"                   
    ## [12] "average_token_length"         
    ## [13] "num_keywords"                 
    ## [14] "data_channel_is_lifestyle"    
    ## [15] "data_channel_is_entertainment"
    ## [16] "data_channel_is_bus"          
    ## [17] "data_channel_is_socmed"       
    ## [18] "data_channel_is_tech"         
    ## [19] "data_channel_is_world"        
    ## [20] "kw_min_min"                   
    ## [21] "kw_max_min"                   
    ## [22] "kw_avg_min"                   
    ## [23] "kw_min_max"                   
    ## [24] "kw_max_max"                   
    ## [25] "kw_avg_max"                   
    ## [26] "kw_min_avg"                   
    ## [27] "kw_max_avg"                   
    ## [28] "kw_avg_avg"                   
    ## [29] "self_reference_min_shares"    
    ## [30] "self_reference_max_shares"    
    ## [31] "self_reference_avg_sharess"   
    ## [32] "weekday_is_monday"            
    ## [33] "weekday_is_tuesday"           
    ## [34] "weekday_is_wednesday"         
    ## [35] "weekday_is_thursday"          
    ## [36] "weekday_is_friday"            
    ## [37] "weekday_is_saturday"          
    ## [38] "weekday_is_sunday"            
    ## [39] "is_weekend"                   
    ## [40] "LDA_00"                       
    ## [41] "LDA_01"                       
    ## [42] "LDA_02"                       
    ## [43] "LDA_03"                       
    ## [44] "LDA_04"                       
    ## [45] "global_subjectivity"          
    ## [46] "global_sentiment_polarity"    
    ## [47] "global_rate_positive_words"   
    ## [48] "global_rate_negative_words"   
    ## [49] "rate_positive_words"          
    ## [50] "rate_negative_words"          
    ## [51] "avg_positive_polarity"        
    ## [52] "min_positive_polarity"        
    ## [53] "max_positive_polarity"        
    ## [54] "avg_negative_polarity"        
    ## [55] "min_negative_polarity"        
    ## [56] "max_negative_polarity"        
    ## [57] "title_subjectivity"           
    ## [58] "title_sentiment_polarity"     
    ## [59] "abs_title_subjectivity"       
    ## [60] "abs_title_sentiment_polarity" 
    ## [61] "shares"

## Exploratory Data Analysis

Here, I will do a basic analysis of my variables to see basic trends,
and correlations.

*Correlation of all Variables*

``` r
data <- data %>% select(-url)

correlation <- cor(data, method = "spearman")
```

Take only those with a correlation to shares of \> 0.06.

``` r
shareCor <- correlation[60, ] >= 0.06

corMax <- correlation[60, shareCor]

corMax
```

    ##                  num_hrefs 
    ##                 0.09001509 
    ##                   num_imgs 
    ##                 0.08311430 
    ##               num_keywords 
    ##                 0.07125251 
    ##     data_channel_is_socmed 
    ##                 0.11357154 
    ##       data_channel_is_tech 
    ##                 0.09451945 
    ##                 kw_max_min 
    ##                 0.09155533 
    ##                 kw_avg_min 
    ##                 0.09302653 
    ##                 kw_min_avg 
    ##                 0.10324214 
    ##                 kw_max_avg 
    ##                 0.22329145 
    ##                 kw_avg_avg 
    ##                 0.25562215 
    ##  self_reference_min_shares 
    ##                 0.18151675 
    ##  self_reference_max_shares 
    ##                 0.16872472 
    ## self_reference_avg_sharess 
    ##                 0.19217450 
    ##        weekday_is_saturday 
    ##                 0.10885957 
    ##          weekday_is_sunday 
    ##                 0.09840582 
    ##                 is_weekend 
    ##                 0.15171751 
    ##                     LDA_03 
    ##                 0.06768825 
    ##        global_subjectivity 
    ##                 0.11354818 
    ##  global_sentiment_polarity 
    ##                 0.07955141 
    ## global_rate_positive_words 
    ##                 0.07129599 
    ##                     shares 
    ##                 1.00000000

## Filter Data to Only Include these Variables

I will not include the weekday\_is\_ variables nor the indicator
variables.

``` r
data1 <- data %>% 
  select(shares, kw_max_avg, self_reference_avg_sharess, kw_min_avg, 
         kw_avg_avg, self_reference_max_shares, global_subjectivity, num_hrefs, num_imgs, num_keywords, kw_max_min, kw_avg_min, LDA_03, global_sentiment_polarity, global_rate_positive_words)%>% collect()
```

**Scale Data**

``` r
scaleData <- scale(data1)
```

## Create Scatterplots of these Variables Against Shares

``` r
dataGathered <- scaleData %>% as_data_frame() %>% 
  gather(key = "variable", value = "value", -shares)

ggplot(dataGathered, aes(x = value, y = shares)) + geom_point() + facet_wrap(~variable)
```

![](sunday_files/figure-gfm/scatter%20data-1.png)<!-- -->

I will take a closer look at a few of these variables.

``` r
dataGathered <- dataGathered %>% filter(variable %in% c("kw_avg_avg", "kw_avg_min", "kw_min_avg", "LDA_03", "num_imgs", "num_keywords"))

ggplot(dataGathered, aes(x = value, y = shares)) + geom_point() + facet_wrap(~variable)
```

![](sunday_files/figure-gfm/scatter%20data%202-1.png)<!-- -->

I will use these six variables in my regression. The data will be
separated by the weekday published and the response is number of shares.

1.  shares
      - (target variable)  
2.  kw\_avg\_avg
      - (average keyword in average number of shares)  
3.  kw\_avg\_min
      - (worst keyword in average number of shares)  
4.  kw\_min\_avg
      - (average keyword in minimum number of shares)  
5.  LDA\_03
      - (closeness to LDA topic 1)  
6.  num\_imgs
      - (number of images)  
7.  num\_keywords
      - (number of keywords in metadata)

## Select only needed variables from data for specific day

``` r
day1 <-paste0("weekday_is_", params$day)

day <- as.name(day1)

data <- data %>% 
  filter(eval(day) == 1) %>% 
  #select only needed variables.
  select(shares, kw_avg_avg, kw_avg_min, kw_min_avg, LDA_03, num_imgs, num_keywords) %>% 
  collect()
```

## Make Train and Test Set

``` r
# set seed
set.seed(130)
# Set indices
train <- sample(1:nrow(data), size =nrow(data)*0.7)
test <- setdiff(1:nrow(data), train)

# Make Train and Test Sets  

dataTrain <- data[train, ]
dataTest <- data[test, ]
```

**Run Quick Summaries on Train Data**

``` r
summary(dataTrain)
```

    ##      shares     
    ##  Min.   :   91  
    ##  1st Qu.: 1200  
    ##  Median : 1900  
    ##  Mean   : 3893  
    ##  3rd Qu.: 3600  
    ##  Max.   :83300  
    ##    kw_avg_avg     
    ##  Min.   :  743.5  
    ##  1st Qu.: 2500.0  
    ##  Median : 3044.0  
    ##  Mean   : 3287.8  
    ##  3rd Qu.: 3854.3  
    ##  Max.   :15336.1  
    ##    kw_avg_min    
    ##  Min.   :  -1.0  
    ##  1st Qu.: 157.4  
    ##  Median : 242.3  
    ##  Mean   : 305.6  
    ##  3rd Qu.: 374.0  
    ##  Max.   :4567.4  
    ##    kw_min_avg  
    ##  Min.   :   0  
    ##  1st Qu.:   0  
    ##  Median :1163  
    ##  Mean   :1210  
    ##  3rd Qu.:2156  
    ##  Max.   :3600  
    ##      LDA_03       
    ##  Min.   :0.01830  
    ##  1st Qu.:0.02597  
    ##  Median :0.05000  
    ##  Mean   :0.25949  
    ##  3rd Qu.:0.48936  
    ##  Max.   :0.91997  
    ##     num_imgs      
    ##  Min.   :  0.000  
    ##  1st Qu.:  1.000  
    ##  Median :  1.000  
    ##  Mean   :  5.883  
    ##  3rd Qu.:  8.000  
    ##  Max.   :128.000  
    ##   num_keywords   
    ##  Min.   : 1.000  
    ##  1st Qu.: 6.000  
    ##  Median : 8.000  
    ##  Mean   : 7.644  
    ##  3rd Qu.: 9.000  
    ##  Max.   :10.000

# Linear Regression Model

I will begin by running a regression model with all of the variables.

**allVarFit**

``` r
allVarFit <- lm(shares ~., data = dataTrain)

summary(allVarFit)
```

    ## 
    ## Call:
    ## lm(formula = shares ~ ., data = dataTrain)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q 
    ## -14693  -2423  -1490    -79 
    ##    Max 
    ##  80100 
    ## 
    ## Coefficients:
    ##               Estimate
    ## (Intercept)    -0.0123
    ## kw_avg_avg      0.8749
    ## kw_avg_min      0.1459
    ## kw_min_avg     -0.2729
    ## LDA_03       1029.7761
    ## num_imgs       12.6790
    ## num_keywords  125.5848
    ##              Std. Error
    ## (Intercept)    758.2441
    ## kw_avg_avg       0.1754
    ## kw_avg_min       0.5450
    ## kw_min_avg       0.1634
    ## LDA_03         574.3499
    ## num_imgs        15.5083
    ## num_keywords    85.1593
    ##              t value Pr(>|t|)
    ## (Intercept)    0.000   1.0000
    ## kw_avg_avg     4.990  6.6e-07
    ## kw_avg_min     0.268   0.7889
    ## kw_min_avg    -1.670   0.0951
    ## LDA_03         1.793   0.0731
    ## num_imgs       0.818   0.4137
    ## num_keywords   1.475   0.1405
    ##                 
    ## (Intercept)     
    ## kw_avg_avg   ***
    ## kw_avg_min      
    ## kw_min_avg   .  
    ## LDA_03       .  
    ## num_imgs        
    ## num_keywords    
    ## ---
    ## Signif. codes:  
    ##   0 '***' 0.001 '**' 0.01
    ##   '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6764 on 1908 degrees of freedom
    ## Multiple R-squared:  0.03071,    Adjusted R-squared:  0.02766 
    ## F-statistic: 10.08 on 6 and 1908 DF,  p-value: 5.484e-11

### Analysis

The adjusted R-Squared is very small.

I will create another linear model with one less variable (-LDA\_03).

**OneLM**

``` r
OneLM <- lm(shares ~ . -LDA_03,  
                data = dataTrain
)

summary(OneLM)
```

    ## 
    ## Call:
    ## lm(formula = shares ~ . - LDA_03, data = dataTrain)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q 
    ## -15886  -2391  -1528   -165 
    ##    Max 
    ##  79964 
    ## 
    ## Coefficients:
    ##               Estimate
    ## (Intercept)  -96.74566
    ## kw_avg_avg     1.00861
    ## kw_avg_min     0.06939
    ## kw_min_avg    -0.31086
    ## num_imgs      17.97968
    ## num_keywords 120.68425
    ##              Std. Error
    ## (Intercept)   756.76079
    ## kw_avg_avg      0.15880
    ## kw_avg_min      0.54365
    ## kw_min_avg      0.16216
    ## num_imgs       15.23273
    ## num_keywords   85.16474
    ##              t value Pr(>|t|)
    ## (Intercept)   -0.128   0.8983
    ## kw_avg_avg     6.351 2.66e-10
    ## kw_avg_min     0.128   0.8985
    ## kw_min_avg    -1.917   0.0554
    ## num_imgs       1.180   0.2380
    ## num_keywords   1.417   0.1566
    ##                 
    ## (Intercept)     
    ## kw_avg_avg   ***
    ## kw_avg_min      
    ## kw_min_avg   .  
    ## num_imgs        
    ## num_keywords    
    ## ---
    ## Signif. codes:  
    ##   0 '***' 0.001 '**' 0.01
    ##   '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6768 on 1909 degrees of freedom
    ## Multiple R-squared:  0.02908,    Adjusted R-squared:  0.02653 
    ## F-statistic: 11.43 on 5 and 1909 DF,  p-value: 6.796e-11

## Comparison of Two Models

I will compare the two models using the compareFitStats function.

``` r
compareFitStats(allVarFit, OneLM)
```

### Analysis

Neither model fits the data well. I am going to try just kw\_avg\_avg.

``` r
kwAVGFit <- lm(shares ~ kw_avg_avg, data = dataTrain)

summary(kwAVGFit)
```

    ## 
    ## Call:
    ## lm(formula = shares ~ kw_avg_avg, data = dataTrain)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q 
    ## -13920  -2397  -1618   -164 
    ##    Max 
    ##  80004 
    ## 
    ## Coefficients:
    ##             Estimate
    ## (Intercept) 949.3993
    ## kw_avg_avg    0.8953
    ##             Std. Error
    ## (Intercept)   455.2048
    ## kw_avg_avg      0.1302
    ##             t value Pr(>|t|)
    ## (Intercept)   2.086   0.0371
    ## kw_avg_avg    6.876 8.28e-12
    ##                
    ## (Intercept) *  
    ## kw_avg_avg  ***
    ## ---
    ## Signif. codes:  
    ##   0 '***' 0.001 '**' 0.01
    ##   '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6778 on 1913 degrees of freedom
    ## Multiple R-squared:  0.02412,    Adjusted R-squared:  0.02361 
    ## F-statistic: 47.29 on 1 and 1913 DF,  p-value: 8.277e-12

### Analysis

None of these models fit the data. I will instead use a logistic
model.  
\# Logistic Model  
First, I need to create a variable to reference whether the number of
shares is less than 1400 or greater than 1400. I am still going to use
the same variables as those in my linear regression attempt.

``` r
data1 <- data %>% mutate(logShares = ifelse(shares >= 1400, 1, 0)) 
data1 <- data1 %>% select(logShares, everything()) %>% select(-shares)

#Create New Test and Train Set with logShares Variable. Set seed gives same train and test set. 

# set seed
set.seed(130)
# Set indices
train <- sample(1:nrow(data1), size =nrow(data1)*0.7)
test <- setdiff(1:nrow(data1), train)

# Make Train and Test Sets  

data1Train <- data1[train, ]
data1Test <- data1[test, ]

data1
```

    ## # A tibble: 2,737 x 7
    ##    logShares kw_avg_avg
    ##        <dbl>      <dbl>
    ##  1         1      1295.
    ##  2         0      2232.
    ##  3         1      2000.
    ##  4         1      1549.
    ##  5         1      1470.
    ##  6         1      1707.
    ##  7         1      2411.
    ##  8         0      1448.
    ##  9         0      2710.
    ## 10         1      1460.
    ## # ... with 2,727 more rows,
    ## #   and 5 more variables:
    ## #   kw_avg_min <dbl>,
    ## #   kw_min_avg <dbl>,
    ## #   LDA_03 <dbl>,
    ## #   num_imgs <dbl>,
    ## #   num_keywords <dbl>

Here, I will fit a logistic regression model using the `glm()` function
with the `"binomial"` family. I will look at how the removal of certain
variables changes the AIC value for each model.

**GLM ALL Model**

``` r
glmALL <- glm(logShares ~., data = data1Train, family = "binomial")

glmALL
```

    ## 
    ## Call:  glm(formula = logShares ~ ., family = "binomial", data = data1Train)
    ## 
    ## Coefficients:
    ##  (Intercept)    kw_avg_avg  
    ##   -7.650e-01     2.618e-04  
    ##   kw_avg_min    kw_min_avg  
    ##    5.455e-04    -4.656e-06  
    ##       LDA_03      num_imgs  
    ##   -1.825e-01     5.058e-03  
    ## num_keywords  
    ##    8.287e-02  
    ## 
    ## Degrees of Freedom: 1914 Total (i.e. Null);  1908 Residual
    ## Null Deviance:       2346 
    ## Residual Deviance: 2292  AIC: 2306

``` r
summary(glmALL)
```

    ## 
    ## Call:
    ## glm(formula = logShares ~ ., family = "binomial", data = data1Train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median  
    ## -3.0006  -1.3639   0.7680  
    ##      3Q      Max  
    ##  0.8797   1.1712  
    ## 
    ## Coefficients:
    ##                Estimate
    ## (Intercept)  -7.650e-01
    ## kw_avg_avg    2.618e-04
    ## kw_avg_min    5.455e-04
    ## kw_min_avg   -4.656e-06
    ## LDA_03       -1.825e-01
    ## num_imgs      5.058e-03
    ## num_keywords  8.287e-02
    ##              Std. Error
    ## (Intercept)   2.556e-01
    ## kw_avg_avg    6.999e-05
    ## kw_avg_min    2.370e-04
    ## kw_min_avg    5.698e-05
    ## LDA_03        1.926e-01
    ## num_imgs      5.329e-03
    ## num_keywords  2.786e-02
    ##              z value Pr(>|z|)
    ## (Intercept)   -2.993 0.002759
    ## kw_avg_avg     3.741 0.000184
    ## kw_avg_min     2.302 0.021337
    ## kw_min_avg    -0.082 0.934875
    ## LDA_03        -0.947 0.343441
    ## num_imgs       0.949 0.342595
    ## num_keywords   2.974 0.002936
    ##                 
    ## (Intercept)  ** 
    ## kw_avg_avg   ***
    ## kw_avg_min   *  
    ## kw_min_avg      
    ## LDA_03          
    ## num_imgs        
    ## num_keywords ** 
    ## ---
    ## Signif. codes:  
    ##   0 '***' 0.001 '**' 0.01
    ##   '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2345.5  on 1914  degrees of freedom
    ## Residual deviance: 2292.1  on 1908  degrees of freedom
    ## AIC: 2306.1
    ## 
    ## Number of Fisher Scoring iterations: 4

I will remove `kw_avg_min` and `kw_min_avg` variable just to be able to
compare fits of the two logistic models.

**GLM2Fit Model**

``` r
glm2Fit <- glm(logShares ~ kw_avg_avg + LDA_03 + num_imgs + num_keywords,  
                data = data1Train, 
                family = "binomial"
)

summary(glm2Fit)
```

    ## 
    ## Call:
    ## glm(formula = logShares ~ kw_avg_avg + LDA_03 + num_imgs + num_keywords, 
    ##     family = "binomial", data = data1Train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median  
    ## -2.9333  -1.3731   0.7674  
    ##      3Q      Max  
    ##  0.8785   1.1619  
    ## 
    ## Coefficients:
    ##                Estimate
    ## (Intercept)  -7.295e-01
    ## kw_avg_avg    2.739e-04
    ## LDA_03       -1.986e-01
    ## num_imgs      4.747e-03
    ## num_keywords  9.405e-02
    ##              Std. Error
    ## (Intercept)   2.537e-01
    ## kw_avg_avg    5.556e-05
    ## LDA_03        1.878e-01
    ## num_imgs      5.295e-03
    ## num_keywords  2.635e-02
    ##              z value Pr(>|z|)
    ## (Intercept)   -2.875 0.004041
    ## kw_avg_avg     4.930 8.21e-07
    ## LDA_03        -1.057 0.290439
    ## num_imgs       0.896 0.369994
    ## num_keywords   3.570 0.000357
    ##                 
    ## (Intercept)  ** 
    ## kw_avg_avg   ***
    ## LDA_03          
    ## num_imgs        
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ##   0 '***' 0.001 '**' 0.01
    ##   '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2345.5  on 1914  degrees of freedom
    ## Residual deviance: 2298.3  on 1910  degrees of freedom
    ## AIC: 2308.3
    ## 
    ## Number of Fisher Scoring iterations: 4

### Analysis

The AIC is about the same for both models. I will remove LDA\_03 from
the first model.

**glm3Fit Model **

``` r
glm3Fit <- glm(logShares ~. -LDA_03, 
                data = data1Train, 
                family = "binomial"
)

summary(glm3Fit)
```

    ## 
    ## Call:
    ## glm(formula = logShares ~ . - LDA_03, family = "binomial", data = data1Train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median  
    ## -2.9034  -1.3612   0.7675  
    ##      3Q      Max  
    ##  0.8785   1.1776  
    ## 
    ## Coefficients:
    ##                Estimate
    ## (Intercept)  -7.362e-01
    ## kw_avg_avg    2.320e-04
    ## kw_avg_min    5.577e-04
    ## kw_min_avg    5.751e-06
    ## num_imgs      4.170e-03
    ## num_keywords  8.411e-02
    ##              Std. Error
    ## (Intercept)   2.531e-01
    ## kw_avg_avg    6.172e-05
    ## kw_avg_min    2.362e-04
    ## kw_min_avg    5.568e-05
    ## num_imgs      5.233e-03
    ## num_keywords  2.780e-02
    ##              z value Pr(>|z|)
    ## (Intercept)   -2.909  0.00363
    ## kw_avg_avg     3.759  0.00017
    ## kw_avg_min     2.361  0.01820
    ## kw_min_avg     0.103  0.91774
    ## num_imgs       0.797  0.42557
    ## num_keywords   3.026  0.00248
    ##                 
    ## (Intercept)  ** 
    ## kw_avg_avg   ***
    ## kw_avg_min   *  
    ## kw_min_avg      
    ## num_imgs        
    ## num_keywords ** 
    ## ---
    ## Signif. codes:  
    ##   0 '***' 0.001 '**' 0.01
    ##   '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2345.5  on 1914  degrees of freedom
    ## Residual deviance: 2293.0  on 1909  degrees of freedom
    ## AIC: 2305
    ## 
    ## Number of Fisher Scoring iterations: 4

\#\#Analysis  
AIC is slightly higher. I will now remove kw\_avg\_min this time.

**glm4Fit Model **

``` r
glm4Fit <- glm(logShares ~. -LDA_03 -kw_avg_min, 
                data = data1Train, 
                family = "binomial"
)

summary(glm4Fit)
```

    ## 
    ## Call:
    ## glm(formula = logShares ~ . - LDA_03 - kw_avg_min, family = "binomial", 
    ##     data = data1Train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median  
    ## -2.8916  -1.3730   0.7712  
    ##      3Q      Max  
    ##  0.8785   1.1535  
    ## 
    ## Coefficients:
    ##                Estimate
    ## (Intercept)  -6.916e-01
    ## kw_avg_avg    2.565e-04
    ## kw_min_avg   -1.149e-05
    ## num_imgs      3.814e-03
    ## num_keywords  9.226e-02
    ##              Std. Error
    ## (Intercept)   2.517e-01
    ## kw_avg_avg    6.090e-05
    ## kw_min_avg    5.538e-05
    ## num_imgs      5.203e-03
    ## num_keywords  2.755e-02
    ##              z value Pr(>|z|)
    ## (Intercept)   -2.747 0.006005
    ## kw_avg_avg     4.212 2.53e-05
    ## kw_min_avg    -0.208 0.835596
    ## num_imgs       0.733 0.463491
    ## num_keywords   3.349 0.000812
    ##                 
    ## (Intercept)  ** 
    ## kw_avg_avg   ***
    ## kw_min_avg      
    ## num_imgs        
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ##   0 '***' 0.001 '**' 0.01
    ##   '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2345.5  on 1914  degrees of freedom
    ## Residual deviance: 2299.3  on 1910  degrees of freedom
    ## AIC: 2309.3
    ## 
    ## Number of Fisher Scoring iterations: 4

\#\#Analysis  
Did not help. For my last model, I will just use num\_keywords.

**glm5Fit**

``` r
glm5Fit <- glm(logShares ~ num_keywords, 
                data = data1Train, 
                family = "binomial"
)
summary(glm5Fit)
```

    ## 
    ## Call:
    ## glm(formula = logShares ~ num_keywords, family = "binomial", 
    ##     data = data1Train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median  
    ## -1.6613  -1.4281   0.7961  
    ##      3Q      Max  
    ##  0.8689   1.1111  
    ## 
    ## Coefficients:
    ##              Estimate
    ## (Intercept)   0.05441
    ## num_keywords  0.10358
    ##              Std. Error
    ## (Intercept)     0.20119
    ## num_keywords    0.02598
    ##              z value Pr(>|z|)
    ## (Intercept)    0.270    0.787
    ## num_keywords   3.986 6.72e-05
    ##                 
    ## (Intercept)     
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ##   0 '***' 0.001 '**' 0.01
    ##   '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2345.5  on 1914  degrees of freedom
    ## Residual deviance: 2329.6  on 1913  degrees of freedom
    ## AIC: 2333.6
    ## 
    ## Number of Fisher Scoring iterations: 4

## Analysis

This produced the best model thus far.

## Comparison of all 5 Models

I will predict the test data and compare the RMSEs of those.

``` r
#Make predictions  
predALL <- predict(glmALL, newdata = data1Test, type = "link")
pred2 <- predict(glm2Fit, newdata = data1Test, type = "link")
pred3 <- predict(glm3Fit, newdata = data1Test, type = "link")
pred4 <- predict(glm4Fit, newdata = data1Test, type = "link")
pred5 <- predict(glm5Fit, newdata = data1Test, type = "link")

#Calculate RMSE  
AllMSE <- rmse(data1Test$logShares, predALL)
TwoMSE <- rmse(data1Test$logShares, pred2)
ThreeMSE <- rmse(data1Test$logShares, pred3)
FourMSE <- rmse(data1Test$logShares, pred4)
FiveMSE <- rmse(data1Test$logShares, pred5)


matMSE <- matrix(c(AllMSE, TwoMSE, ThreeMSE, FourMSE, FiveMSE), nrow = 1, ncol = 5, byrow = TRUE)

matMSE
```

    ##           [,1]      [,2]
    ## [1,] 0.8478362 0.5686435
    ##           [,3]      [,4]
    ## [1,] 0.8480307 0.5680028
    ##           [,5]
    ## [1,] 0.5281286

### Analysis

The glm5Fit produces the smallest MSE. I will use this as my model for
the data. The glm5Fit also produces the highest AIC value.

# Ensemble Model

From the past homework assigment, it seems that each of the ensemble
methods that we covered are equally efficient. I am going to use the
Random Forest model to fit my data. Overall, Random Forest is better
than bagging and boosting trees take longer to do. I will add a class
variable (less than 1400, more than 1400) that I will predict on the
test data.

## Fix Train and Test Data

``` r
dataTrain <- dataTrain %>% mutate(group = ifelse(shares <= 1400, "less than 1400", "more than 1400")) %>%
  select(group, everything()) %>% collect()

dataTrain$group <- as.factor(dataTrain$group)

dataTrain
```

    ## # A tibble: 1,915 x 8
    ##    group shares kw_avg_avg
    ##    <fct>  <dbl>      <dbl>
    ##  1 less~   1300      3958.
    ##  2 more~  11200      4077.
    ##  3 more~   3900      5371.
    ##  4 more~   4000      5055.
    ##  5 less~   1400      2520.
    ##  6 less~   1300      3082.
    ##  7 less~   1100      2989.
    ##  8 more~   3000      2816.
    ##  9 more~   4800      4676.
    ## 10 less~   1100      2263.
    ## # ... with 1,905 more rows,
    ## #   and 5 more variables:
    ## #   kw_avg_min <dbl>,
    ## #   kw_min_avg <dbl>,
    ## #   LDA_03 <dbl>,
    ## #   num_imgs <dbl>,
    ## #   num_keywords <dbl>

``` r
dataTest <- dataTest %>% mutate(group =ifelse(shares <= 1400, "less than 1400", "more than 1400")) %>%
  select(group, everything()) %>% collect()

dataTest$group <- as.factor(dataTest$group)
```

**Random Forest Model**

``` r
# train control parameters  
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

rfFit<- train(group~., data = dataTrain, method = "rf", trControl = trctrl, preProcess = c("center", "scale"))
```

**Predict Data with rfFit**

``` r
rfPred <- predict(rfFit, select(dataTest, -"group"))

head(rfPred)
```

    ## [1] less than 1400
    ## [2] less than 1400
    ## [3] more than 1400
    ## [4] more than 1400
    ## [5] more than 1400
    ## [6] more than 1400
    ## 2 Levels: less than 1400 ...

**Compare Predictions to Actual**

``` r
fullTbl <- table(data.frame(rfPred, dataTest$group))

fullTbl
```

    ##                 dataTest.group
    ## rfPred           less than 1400
    ##   less than 1400            317
    ##   more than 1400              0
    ##                 dataTest.group
    ## rfPred           more than 1400
    ##   less than 1400              0
    ##   more than 1400            505

**Find MisClassification Rate**

``` r
rfMis <- 1 - sum(diag(fullTbl)/sum(fullTbl))

rfMis
```

    ## [1] 0

### Analysis

This misclassification rate is suspiciously low.

**Another Random Forest**

``` r
rf2 <- train(group ~ num_keywords + kw_avg_avg + kw_min_avg + num_imgs, data = dataTrain, method = "rf", trControl = trctrl, preProcess = c("center", "scale"))  
```

**Predict Data with rf2**

``` r
rf2Pred <- predict(rf2, select(dataTest, -"group"))  
```

**Compare Predictions to Actual**

``` r
fullTbl <- table(data.frame(rf2Pred, dataTest$group))  

fullTbl
```

    ##                 dataTest.group
    ## rf2Pred          less than 1400
    ##   less than 1400            100
    ##   more than 1400            217
    ##                 dataTest.group
    ## rf2Pred          more than 1400
    ##   less than 1400             90
    ##   more than 1400            415

**Find MisClassification Rate**

``` r
rfMis <- 1 - sum(diag(fullTbl)/sum(fullTbl))

rfMis
```

    ## [1] 0.3734793

### Analysis

This does not help. I will keep my first Random Forest Model for
prediction.

# Models Used

Overall, I have chosen the following models for my data.

1.  glm5Fit: Logistic Regression Model  
2.  rfFit : Random Forest Model
