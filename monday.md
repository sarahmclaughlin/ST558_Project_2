<<<<<<< HEAD
ST 558 Project 2
================
Sarah McLaughlin
6/22/2020

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
```

    ## Warning: `as_data_frame()` is deprecated, use `as_tibble()` (but mind the new semantics).
    ## This warning is displayed once per session.

``` r
ggplot(dataGathered, aes(x = value, y = shares)) + geom_point() + facet_wrap(~variable)
```

![](monday_files/figure-gfm/scatter%20data-1.png)<!-- -->

I will take a closer look at a few of these variables.

``` r
dataGathered <- dataGathered %>% filter(variable %in% c("kw_avg_avg", "kw_avg_min", "kw_min_avg", "LDA_03", "num_imgs", "num_keywords"))

ggplot(dataGathered, aes(x = value, y = shares)) + geom_point() + facet_wrap(~variable)
```

![](monday_files/figure-gfm/scatter%20data%202-1.png)<!-- -->

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

    ##      shares           kw_avg_avg   
    ##  Min.   :     1.0   Min.   :    0  
    ##  1st Qu.:   922.2   1st Qu.: 2366  
    ##  Median :  1400.0   Median : 2855  
    ##  Mean   :  3582.0   Mean   : 3076  
    ##  3rd Qu.:  2700.0   3rd Qu.: 3554  
    ##  Max.   :690400.0   Max.   :33536  
    ##    kw_avg_min        kw_min_avg    
    ##  Min.   :   -1.0   Min.   :   0.0  
    ##  1st Qu.:  134.9   1st Qu.:   0.0  
    ##  Median :  230.6   Median : 989.2  
    ##  Mean   :  315.4   Mean   :1082.4  
    ##  3rd Qu.:  352.6   3rd Qu.:1996.0  
    ##  Max.   :29946.9   Max.   :3594.6  
    ##      LDA_03           num_imgs       num_keywords   
    ##  Min.   :0.01819   Min.   : 0.000   Min.   : 1.000  
    ##  1st Qu.:0.02857   1st Qu.: 1.000   1st Qu.: 6.000  
    ##  Median :0.04000   Median : 1.000   Median : 7.000  
    ##  Mean   :0.21986   Mean   : 4.407   Mean   : 7.157  
    ##  3rd Qu.:0.36546   3rd Qu.: 3.000   3rd Qu.: 9.000  
    ##  Max.   :0.91997   Max.   :93.000   Max.   :10.000

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
    ##    Min     1Q Median     3Q    Max 
    ## -27751  -2478  -1581   -394 685407 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  -1.045e+03  9.347e+02  -1.118    0.263
    ## kw_avg_avg    1.198e+00  2.279e-01   5.254 1.55e-07
    ## kw_avg_min   -4.201e-01  3.523e-01  -1.192    0.233
    ## kw_min_avg   -5.443e-02  2.199e-01  -0.247    0.805
    ## LDA_03        9.240e+02  7.813e+02   1.183    0.237
    ## num_imgs      1.197e+01  2.505e+01   0.478    0.633
    ## num_keywords  1.228e+02  1.125e+02   1.092    0.275
    ##                 
    ## (Intercept)     
    ## kw_avg_avg   ***
    ## kw_avg_min      
    ## kw_min_avg      
    ## LDA_03          
    ## num_imgs        
    ## num_keywords    
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 13500 on 4655 degrees of freedom
    ## Multiple R-squared:  0.01399,    Adjusted R-squared:  0.01272 
    ## F-statistic: 11.01 on 6 and 4655 DF,  p-value: 3.241e-12

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
    ##    Min     1Q Median     3Q    Max 
    ## -28062  -2493  -1625   -442 685382 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  -1.136e+03  9.316e+02  -1.219    0.223
    ## kw_avg_avg    1.318e+00  2.038e-01   6.470 1.08e-10
    ## kw_avg_min   -5.191e-01  3.423e-01  -1.517    0.129
    ## kw_min_avg   -8.865e-02  2.180e-01  -0.407    0.684
    ## num_imgs      1.643e+01  2.476e+01   0.663    0.507
    ## num_keywords  1.187e+02  1.125e+02   1.056    0.291
    ##                 
    ## (Intercept)     
    ## kw_avg_avg   ***
    ## kw_avg_min      
    ## kw_min_avg      
    ## num_imgs        
    ## num_keywords    
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 13500 on 4656 degrees of freedom
    ## Multiple R-squared:  0.01369,    Adjusted R-squared:  0.01263 
    ## F-statistic: 12.93 on 5 and 4656 DF,  p-value: 1.63e-12

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
    ##    Min     1Q Median     3Q    Max 
    ## -38895  -2525  -1712   -502 684454 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -40.0377   506.8000  -0.079    0.937    
    ## kw_avg_avg    1.1775     0.1517   7.762 1.02e-14 ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 13500 on 4660 degrees of freedom
    ## Multiple R-squared:  0.01276,    Adjusted R-squared:  0.01255 
    ## F-statistic: 60.24 on 1 and 4660 DF,  p-value: 1.023e-14

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

    ## # A tibble: 6,661 x 7
    ##    logShares kw_avg_avg kw_avg_min kw_min_avg LDA_03
    ##        <dbl>      <dbl>      <dbl>      <dbl>  <dbl>
    ##  1         0          0          0          0 0.0413
    ##  2         0          0          0          0 0.0501
    ##  3         1          0          0          0 0.0333
    ##  4         0          0          0          0 0.0289
    ##  5         0          0          0          0 0.0286
    ##  6         0          0          0          0 0.0222
    ##  7         0          0          0          0 0.0200
    ##  8         0          0          0          0 0.0222
    ##  9         1          0          0          0 0.0297
    ## 10         0          0          0          0 0.0400
    ## # ... with 6,651 more rows, and 2 more variables:
    ## #   num_imgs <dbl>, num_keywords <dbl>

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
    ##  (Intercept)    kw_avg_avg    kw_avg_min  
    ##   -1.518e+00     3.425e-04    -1.066e-04  
    ##   kw_min_avg        LDA_03      num_imgs  
    ##    2.861e-05    -3.027e-01     8.647e-03  
    ## num_keywords  
    ##    7.726e-02  
    ## 
    ## Degrees of Freedom: 4661 Total (i.e. Null);  4655 Residual
    ## Null Deviance:       6460 
    ## Residual Deviance: 6282  AIC: 6296

``` r
summary(glmALL)
```

    ## 
    ## Call:
    ## glm(formula = logShares ~ ., family = "binomial", data = data1Train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.8428  -1.1422   0.7478   1.1475   1.7476  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.518e+00  1.489e-01 -10.200  < 2e-16
    ## kw_avg_avg    3.425e-04  4.118e-05   8.317  < 2e-16
    ## kw_avg_min   -1.066e-04  7.802e-05  -1.367   0.1717
    ## kw_min_avg    2.861e-05  3.460e-05   0.827   0.4083
    ## LDA_03       -3.027e-01  1.223e-01  -2.475   0.0133
    ## num_imgs      8.647e-03  3.898e-03   2.218   0.0265
    ## num_keywords  7.726e-02  1.711e-02   4.517 6.28e-06
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   ***
    ## kw_avg_min      
    ## kw_min_avg      
    ## LDA_03       *  
    ## num_imgs     *  
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 6460.3  on 4661  degrees of freedom
    ## Residual deviance: 6281.9  on 4655  degrees of freedom
    ## AIC: 6295.9
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
    ##     Min       1Q   Median       3Q      Max  
    ## -4.6266  -1.1395   0.7441   1.1470   1.7465  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.484e+00  1.476e-01 -10.057  < 2e-16
    ## kw_avg_avg    3.485e-04  3.292e-05  10.586  < 2e-16
    ## LDA_03       -2.986e-01  1.191e-01  -2.507   0.0122
    ## num_imgs      8.970e-03  3.893e-03   2.304   0.0212
    ## num_keywords  6.956e-02  1.597e-02   4.356 1.32e-05
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   ***
    ## LDA_03       *  
    ## num_imgs     *  
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 6460.3  on 4661  degrees of freedom
    ## Residual deviance: 6284.8  on 4657  degrees of freedom
    ## AIC: 6294.8
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
    ##     Min       1Q   Median       3Q      Max  
    ## -3.7437  -1.1404   0.7421   1.1535   1.6500  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.468e+00  1.469e-01  -9.994  < 2e-16
    ## kw_avg_avg    2.929e-04  3.540e-05   8.275  < 2e-16
    ## kw_avg_min   -7.320e-05  7.666e-05  -0.955   0.3396
    ## kw_min_avg    4.464e-05  3.389e-05   1.317   0.1877
    ## num_imgs      7.294e-03  3.852e-03   1.894   0.0583
    ## num_keywords  7.907e-02  1.708e-02   4.630 3.66e-06
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   ***
    ## kw_avg_min      
    ## kw_min_avg      
    ## num_imgs     .  
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 6460.3  on 4661  degrees of freedom
    ## Residual deviance: 6288.1  on 4656  degrees of freedom
    ## AIC: 6300.1
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
    ##     Min       1Q   Median       3Q      Max  
    ## -4.2169  -1.1395   0.7466   1.1531   1.6485  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.459e+00  1.466e-01  -9.952  < 2e-16
    ## kw_avg_avg    2.836e-04  3.401e-05   8.340  < 2e-16
    ## kw_min_avg    4.900e-05  3.358e-05   1.459   0.1444
    ## num_imgs      7.444e-03  3.849e-03   1.934   0.0531
    ## num_keywords  7.803e-02  1.705e-02   4.577 4.72e-06
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   ***
    ## kw_min_avg      
    ## num_imgs     .  
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 6460.3  on 4661  degrees of freedom
    ## Residual deviance: 6289.0  on 4657  degrees of freedom
    ## AIC: 6299
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
    ##    Min      1Q  Median      3Q     Max  
    ## -1.293  -1.192   1.066   1.162   1.330  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -0.50811    0.11578  -4.388 1.14e-05
    ## num_keywords  0.07762    0.01566   4.956 7.18e-07
    ##                 
    ## (Intercept)  ***
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 6460.3  on 4661  degrees of freedom
    ## Residual deviance: 6435.6  on 4660  degrees of freedom
    ## AIC: 6439.6
    ## 
    ## Number of Fisher Scoring iterations: 3

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

    ##           [,1]      [,2]      [,3]      [,4]
    ## [1,] 0.7763334 0.8102003 0.7680279 0.7835722
    ##           [,5]
    ## [1,] 0.6876201

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

    ## # A tibble: 4,662 x 8
    ##    group shares kw_avg_avg kw_avg_min kw_min_avg
    ##    <fct>  <dbl>      <dbl>      <dbl>      <dbl>
    ##  1 more~   2900      4102.      323.       1818.
    ##  2 less~   1100      2659.      158.       1809.
    ##  3 less~   1300      3529.      494.       1100 
    ##  4 more~   6800      3953.     1170        2969.
    ##  5 less~    711      2229.      105.       1051.
    ##  6 less~   1100      1933.      341.          0 
    ##  7 less~    713      3487.      216.       1783.
    ##  8 less~    725      2879.       50.8      2623.
    ##  9 more~   7800      5375.      108.       3510.
    ## 10 less~   1200      2824.       69.5         0 
    ## # ... with 4,652 more rows, and 3 more variables:
    ## #   LDA_03 <dbl>, num_imgs <dbl>, num_keywords <dbl>

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

    ## [1] less than 1400 more than 1400 more than 1400
    ## [4] less than 1400 less than 1400 less than 1400
    ## Levels: less than 1400 more than 1400

**Compare Predictions to Actual**

``` r
fullTbl <- table(data.frame(rfPred, dataTest$group))

fullTbl
```

    ##                 dataTest.group
    ## rfPred           less than 1400 more than 1400
    ##   less than 1400           1067              0
    ##   more than 1400              0            932

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
    ## rf2Pred          less than 1400 more than 1400
    ##   less than 1400            715            465
    ##   more than 1400            352            467

**Find MisClassification Rate**

``` r
rfMis <- 1 - sum(diag(fullTbl)/sum(fullTbl))

rfMis
```

    ## [1] 0.4087044

### Analysis

This does not help. I will keep my first Random Forest Model for
prediction.

# Models Used

Overall, I have chosen the following models for my data.

1.  glm5Fit: Logistic Regression Model  
2.  rfFit : Random Forest Model
=======
ST 558 Project 2
================
Sarah McLaughlin
6/22/2020

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
```

    ## Warning: `as_data_frame()` is deprecated, use `as_tibble()` (but mind the new semantics).
    ## This warning is displayed once per session.

``` r
ggplot(dataGathered, aes(x = value, y = shares)) + geom_point() + facet_wrap(~variable)
```

![](monday_files/figure-gfm/scatter%20data-1.png)<!-- -->

I will take a closer look at a few of these variables.

``` r
dataGathered <- dataGathered %>% filter(variable %in% c("kw_avg_avg", "kw_avg_min", "kw_min_avg", "LDA_03", "num_imgs", "num_keywords"))

ggplot(dataGathered, aes(x = value, y = shares)) + geom_point() + facet_wrap(~variable)
```

![](monday_files/figure-gfm/scatter%20data%202-1.png)<!-- -->

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

    ##      shares           kw_avg_avg   
    ##  Min.   :     1.0   Min.   :    0  
    ##  1st Qu.:   922.2   1st Qu.: 2366  
    ##  Median :  1400.0   Median : 2855  
    ##  Mean   :  3582.0   Mean   : 3076  
    ##  3rd Qu.:  2700.0   3rd Qu.: 3554  
    ##  Max.   :690400.0   Max.   :33536  
    ##    kw_avg_min        kw_min_avg    
    ##  Min.   :   -1.0   Min.   :   0.0  
    ##  1st Qu.:  134.9   1st Qu.:   0.0  
    ##  Median :  230.6   Median : 989.2  
    ##  Mean   :  315.4   Mean   :1082.4  
    ##  3rd Qu.:  352.6   3rd Qu.:1996.0  
    ##  Max.   :29946.9   Max.   :3594.6  
    ##      LDA_03           num_imgs       num_keywords   
    ##  Min.   :0.01819   Min.   : 0.000   Min.   : 1.000  
    ##  1st Qu.:0.02857   1st Qu.: 1.000   1st Qu.: 6.000  
    ##  Median :0.04000   Median : 1.000   Median : 7.000  
    ##  Mean   :0.21986   Mean   : 4.407   Mean   : 7.157  
    ##  3rd Qu.:0.36546   3rd Qu.: 3.000   3rd Qu.: 9.000  
    ##  Max.   :0.91997   Max.   :93.000   Max.   :10.000

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
    ##    Min     1Q Median     3Q    Max 
    ## -27751  -2478  -1581   -394 685407 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  -1.045e+03  9.347e+02  -1.118    0.263
    ## kw_avg_avg    1.198e+00  2.279e-01   5.254 1.55e-07
    ## kw_avg_min   -4.201e-01  3.523e-01  -1.192    0.233
    ## kw_min_avg   -5.443e-02  2.199e-01  -0.247    0.805
    ## LDA_03        9.240e+02  7.813e+02   1.183    0.237
    ## num_imgs      1.197e+01  2.505e+01   0.478    0.633
    ## num_keywords  1.228e+02  1.125e+02   1.092    0.275
    ##                 
    ## (Intercept)     
    ## kw_avg_avg   ***
    ## kw_avg_min      
    ## kw_min_avg      
    ## LDA_03          
    ## num_imgs        
    ## num_keywords    
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 13500 on 4655 degrees of freedom
    ## Multiple R-squared:  0.01399,    Adjusted R-squared:  0.01272 
    ## F-statistic: 11.01 on 6 and 4655 DF,  p-value: 3.241e-12

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
    ##    Min     1Q Median     3Q    Max 
    ## -28062  -2493  -1625   -442 685382 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  -1.136e+03  9.316e+02  -1.219    0.223
    ## kw_avg_avg    1.318e+00  2.038e-01   6.470 1.08e-10
    ## kw_avg_min   -5.191e-01  3.423e-01  -1.517    0.129
    ## kw_min_avg   -8.865e-02  2.180e-01  -0.407    0.684
    ## num_imgs      1.643e+01  2.476e+01   0.663    0.507
    ## num_keywords  1.187e+02  1.125e+02   1.056    0.291
    ##                 
    ## (Intercept)     
    ## kw_avg_avg   ***
    ## kw_avg_min      
    ## kw_min_avg      
    ## num_imgs        
    ## num_keywords    
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 13500 on 4656 degrees of freedom
    ## Multiple R-squared:  0.01369,    Adjusted R-squared:  0.01263 
    ## F-statistic: 12.93 on 5 and 4656 DF,  p-value: 1.63e-12

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
    ##    Min     1Q Median     3Q    Max 
    ## -38895  -2525  -1712   -502 684454 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -40.0377   506.8000  -0.079    0.937    
    ## kw_avg_avg    1.1775     0.1517   7.762 1.02e-14 ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 13500 on 4660 degrees of freedom
    ## Multiple R-squared:  0.01276,    Adjusted R-squared:  0.01255 
    ## F-statistic: 60.24 on 1 and 4660 DF,  p-value: 1.023e-14

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

    ## # A tibble: 6,661 x 7
    ##    logShares kw_avg_avg kw_avg_min kw_min_avg LDA_03
    ##        <dbl>      <dbl>      <dbl>      <dbl>  <dbl>
    ##  1         0          0          0          0 0.0413
    ##  2         0          0          0          0 0.0501
    ##  3         1          0          0          0 0.0333
    ##  4         0          0          0          0 0.0289
    ##  5         0          0          0          0 0.0286
    ##  6         0          0          0          0 0.0222
    ##  7         0          0          0          0 0.0200
    ##  8         0          0          0          0 0.0222
    ##  9         1          0          0          0 0.0297
    ## 10         0          0          0          0 0.0400
    ## # ... with 6,651 more rows, and 2 more variables:
    ## #   num_imgs <dbl>, num_keywords <dbl>

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
    ##  (Intercept)    kw_avg_avg    kw_avg_min  
    ##   -1.518e+00     3.425e-04    -1.066e-04  
    ##   kw_min_avg        LDA_03      num_imgs  
    ##    2.861e-05    -3.027e-01     8.647e-03  
    ## num_keywords  
    ##    7.726e-02  
    ## 
    ## Degrees of Freedom: 4661 Total (i.e. Null);  4655 Residual
    ## Null Deviance:       6460 
    ## Residual Deviance: 6282  AIC: 6296

``` r
summary(glmALL)
```

    ## 
    ## Call:
    ## glm(formula = logShares ~ ., family = "binomial", data = data1Train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.8428  -1.1422   0.7478   1.1475   1.7476  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.518e+00  1.489e-01 -10.200  < 2e-16
    ## kw_avg_avg    3.425e-04  4.118e-05   8.317  < 2e-16
    ## kw_avg_min   -1.066e-04  7.802e-05  -1.367   0.1717
    ## kw_min_avg    2.861e-05  3.460e-05   0.827   0.4083
    ## LDA_03       -3.027e-01  1.223e-01  -2.475   0.0133
    ## num_imgs      8.647e-03  3.898e-03   2.218   0.0265
    ## num_keywords  7.726e-02  1.711e-02   4.517 6.28e-06
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   ***
    ## kw_avg_min      
    ## kw_min_avg      
    ## LDA_03       *  
    ## num_imgs     *  
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 6460.3  on 4661  degrees of freedom
    ## Residual deviance: 6281.9  on 4655  degrees of freedom
    ## AIC: 6295.9
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
    ##     Min       1Q   Median       3Q      Max  
    ## -4.6266  -1.1395   0.7441   1.1470   1.7465  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.484e+00  1.476e-01 -10.057  < 2e-16
    ## kw_avg_avg    3.485e-04  3.292e-05  10.586  < 2e-16
    ## LDA_03       -2.986e-01  1.191e-01  -2.507   0.0122
    ## num_imgs      8.970e-03  3.893e-03   2.304   0.0212
    ## num_keywords  6.956e-02  1.597e-02   4.356 1.32e-05
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   ***
    ## LDA_03       *  
    ## num_imgs     *  
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 6460.3  on 4661  degrees of freedom
    ## Residual deviance: 6284.8  on 4657  degrees of freedom
    ## AIC: 6294.8
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
    ##     Min       1Q   Median       3Q      Max  
    ## -3.7437  -1.1404   0.7421   1.1535   1.6500  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.468e+00  1.469e-01  -9.994  < 2e-16
    ## kw_avg_avg    2.929e-04  3.540e-05   8.275  < 2e-16
    ## kw_avg_min   -7.320e-05  7.666e-05  -0.955   0.3396
    ## kw_min_avg    4.464e-05  3.389e-05   1.317   0.1877
    ## num_imgs      7.294e-03  3.852e-03   1.894   0.0583
    ## num_keywords  7.907e-02  1.708e-02   4.630 3.66e-06
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   ***
    ## kw_avg_min      
    ## kw_min_avg      
    ## num_imgs     .  
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 6460.3  on 4661  degrees of freedom
    ## Residual deviance: 6288.1  on 4656  degrees of freedom
    ## AIC: 6300.1
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
    ##     Min       1Q   Median       3Q      Max  
    ## -4.2169  -1.1395   0.7466   1.1531   1.6485  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.459e+00  1.466e-01  -9.952  < 2e-16
    ## kw_avg_avg    2.836e-04  3.401e-05   8.340  < 2e-16
    ## kw_min_avg    4.900e-05  3.358e-05   1.459   0.1444
    ## num_imgs      7.444e-03  3.849e-03   1.934   0.0531
    ## num_keywords  7.803e-02  1.705e-02   4.577 4.72e-06
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   ***
    ## kw_min_avg      
    ## num_imgs     .  
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 6460.3  on 4661  degrees of freedom
    ## Residual deviance: 6289.0  on 4657  degrees of freedom
    ## AIC: 6299
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
    ##    Min      1Q  Median      3Q     Max  
    ## -1.293  -1.192   1.066   1.162   1.330  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -0.50811    0.11578  -4.388 1.14e-05
    ## num_keywords  0.07762    0.01566   4.956 7.18e-07
    ##                 
    ## (Intercept)  ***
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 6460.3  on 4661  degrees of freedom
    ## Residual deviance: 6435.6  on 4660  degrees of freedom
    ## AIC: 6439.6
    ## 
    ## Number of Fisher Scoring iterations: 3

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

    ##           [,1]      [,2]      [,3]      [,4]
    ## [1,] 0.7763334 0.8102003 0.7680279 0.7835722
    ##           [,5]
    ## [1,] 0.6876201

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

    ## # A tibble: 4,662 x 8
    ##    group shares kw_avg_avg kw_avg_min kw_min_avg
    ##    <fct>  <dbl>      <dbl>      <dbl>      <dbl>
    ##  1 more~   2900      4102.      323.       1818.
    ##  2 less~   1100      2659.      158.       1809.
    ##  3 less~   1300      3529.      494.       1100 
    ##  4 more~   6800      3953.     1170        2969.
    ##  5 less~    711      2229.      105.       1051.
    ##  6 less~   1100      1933.      341.          0 
    ##  7 less~    713      3487.      216.       1783.
    ##  8 less~    725      2879.       50.8      2623.
    ##  9 more~   7800      5375.      108.       3510.
    ## 10 less~   1200      2824.       69.5         0 
    ## # ... with 4,652 more rows, and 3 more variables:
    ## #   LDA_03 <dbl>, num_imgs <dbl>, num_keywords <dbl>

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

    ## [1] less than 1400 more than 1400 more than 1400
    ## [4] less than 1400 less than 1400 less than 1400
    ## Levels: less than 1400 more than 1400

**Compare Predictions to Actual**

``` r
fullTbl <- table(data.frame(rfPred, dataTest$group))

fullTbl
```

    ##                 dataTest.group
    ## rfPred           less than 1400 more than 1400
    ##   less than 1400           1067              0
    ##   more than 1400              0            932

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
    ## rf2Pred          less than 1400 more than 1400
    ##   less than 1400            715            465
    ##   more than 1400            352            467

**Find MisClassification Rate**

``` r
rfMis <- 1 - sum(diag(fullTbl)/sum(fullTbl))

rfMis
```

    ## [1] 0.4087044

### Analysis

This does not help. I will keep my first Random Forest Model for
prediction.

# Models Used

Overall, I have chosen the following models for my data.

1.  glm5Fit: Logistic Regression Model  
2.  rfFit : Random Forest Model
>>>>>>> 48998e8809e60020fba02fe95a400f49ba700320
