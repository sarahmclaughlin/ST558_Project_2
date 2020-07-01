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

ggplot(dataGathered, aes(x = value, y = shares)) + geom_point() + facet_wrap(~variable)
```

![](wednesday_files/figure-gfm/scatter%20data-1.png)<!-- -->

I will take a closer look at a few of these variables.

``` r
dataGathered <- dataGathered %>% filter(variable %in% c("kw_avg_avg", "kw_avg_min", "kw_min_avg", "LDA_03", "num_imgs", "num_keywords"))

ggplot(dataGathered, aes(x = value, y = shares)) + geom_point() + facet_wrap(~variable)
```

![](wednesday_files/figure-gfm/scatter%20data%202-1.png)<!-- -->

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

    ##      shares         kw_avg_avg     
    ##  Min.   :    36   Min.   :  424.3  
    ##  1st Qu.:   900   1st Qu.: 2364.6  
    ##  Median :  1300   Median : 2844.3  
    ##  Mean   :  3298   Mean   : 3109.8  
    ##  3rd Qu.:  2600   3rd Qu.: 3559.6  
    ##  Max.   :843300   Max.   :21000.7  
    ##    kw_avg_min        kw_min_avg       LDA_03       
    ##  Min.   :   -1.0   Min.   :  -1   Min.   :0.01820  
    ##  1st Qu.:  141.8   1st Qu.:   0   1st Qu.:0.02857  
    ##  Median :  234.7   Median :1014   Median :0.04000  
    ##  Mean   :  310.2   Mean   :1102   Mean   :0.21808  
    ##  3rd Qu.:  355.9   3rd Qu.:1996   3rd Qu.:0.36294  
    ##  Max.   :13078.5   Max.   :3613   Max.   :0.91998  
    ##     num_imgs        num_keywords   
    ##  Min.   :  0.000   Min.   : 1.000  
    ##  1st Qu.:  1.000   1st Qu.: 6.000  
    ##  Median :  1.000   Median : 7.000  
    ##  Mean   :  4.115   Mean   : 7.167  
    ##  3rd Qu.:  3.000   3rd Qu.: 9.000  
    ##  Max.   :111.000   Max.   :10.000

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
    ## -13098  -2182  -1275   -271 837759 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)   -26.1434   918.7203  -0.028 0.977299
    ## kw_avg_avg      0.6527     0.2123   3.074 0.002123
    ## kw_avg_min     -0.3035     0.4474  -0.678 0.497561
    ## kw_min_avg      0.1178     0.2125   0.554 0.579402
    ## LDA_03       2578.4382   779.3669   3.308 0.000945
    ## num_imgs       51.7032    26.8119   1.928 0.053864
    ## num_keywords   67.5334   108.8550   0.620 0.535023
    ##                 
    ## (Intercept)     
    ## kw_avg_avg   ** 
    ## kw_avg_min      
    ## kw_min_avg      
    ## LDA_03       ***
    ## num_imgs     .  
    ## num_keywords    
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 14060 on 5197 degrees of freedom
    ## Multiple R-squared:  0.01174,    Adjusted R-squared:  0.0106 
    ## F-statistic: 10.29 on 6 and 5197 DF,  p-value: 2.336e-11

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
    ## -17065  -2202  -1441   -384 838671 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  -359.77902  914.04190  -0.394   0.6939
    ## kw_avg_avg      0.96597    0.19024   5.078 3.96e-07
    ## kw_avg_min     -0.57170    0.44039  -1.298   0.1943
    ## kw_min_avg      0.07637    0.21234   0.360   0.7191
    ## num_imgs       63.72554   26.58994   2.397   0.0166
    ## num_keywords   67.69983  108.95907   0.621   0.5344
    ##                 
    ## (Intercept)     
    ## kw_avg_avg   ***
    ## kw_avg_min      
    ## kw_min_avg      
    ## num_imgs     *  
    ## num_keywords    
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 14070 on 5198 degrees of freedom
    ## Multiple R-squared:  0.009659,   Adjusted R-squared:  0.008707 
    ## F-statistic: 10.14 on 5 and 5198 DF,  p-value: 1.112e-09

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
    ## -17420  -2199  -1531   -473 839372 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 252.8586   509.0840   0.497    0.619    
    ## kw_avg_avg    0.9794     0.1512   6.477 1.02e-10 ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 14080 on 5202 degrees of freedom
    ## Multiple R-squared:  0.008001,   Adjusted R-squared:  0.00781 
    ## F-statistic: 41.96 on 1 and 5202 DF,  p-value: 1.019e-10

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

    ## # A tibble: 7,435 x 7
    ##    logShares kw_avg_avg kw_avg_min kw_min_avg LDA_03
    ##        <dbl>      <dbl>      <dbl>      <dbl>  <dbl>
    ##  1         0       707.       469.         0  0.491 
    ##  2         0      1516.       279.         0  0.0286
    ##  3         0       650.       444          0  0.0334
    ##  4         1      1048.       280          0  0.0250
    ##  5         1       849.       469.         0  0.0404
    ##  6         1       832.       572          0  0.910 
    ##  7         0      1777.       279.       727. 0.0250
    ##  8         0       858.       517          0  0.644 
    ##  9         1      1084.       675          0  0.0224
    ## 10         0      3493.       938.         0  0.866 
    ## # ... with 7,425 more rows, and 2 more variables:
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
    ##   -1.4438717     0.0003331    -0.0001129  
    ##   kw_min_avg        LDA_03      num_imgs  
    ##    0.0000165    -0.4034132     0.0155138  
    ## num_keywords  
    ##    0.0606217  
    ## 
    ## Degrees of Freedom: 5203 Total (i.e. Null);  5197 Residual
    ## Null Deviance:       7214 
    ## Residual Deviance: 7031  AIC: 7045

``` r
summary(glmALL)
```

    ## 
    ## Call:
    ## glm(formula = logShares ~ ., family = "binomial", data = data1Train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.9533  -1.1168  -0.9022   1.1766   1.6973  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.444e+00  1.409e-01 -10.248  < 2e-16
    ## kw_avg_avg    3.331e-04  3.974e-05   8.382  < 2e-16
    ## kw_avg_min   -1.129e-04  7.898e-05  -1.430 0.152716
    ## kw_min_avg    1.650e-05  3.252e-05   0.507 0.611939
    ## LDA_03       -4.034e-01  1.194e-01  -3.379 0.000728
    ## num_imgs      1.551e-02  4.026e-03   3.854 0.000116
    ## num_keywords  6.062e-02  1.589e-02   3.815 0.000136
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   ***
    ## kw_avg_min      
    ## kw_min_avg      
    ## LDA_03       ***
    ## num_imgs     ***
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 7213.6  on 5203  degrees of freedom
    ## Residual deviance: 7030.7  on 5197  degrees of freedom
    ## AIC: 7044.7
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
    ## -2.9423  -1.1167  -0.9084   1.1790   1.6668  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.417e+00  1.399e-01 -10.125  < 2e-16
    ## kw_avg_avg    3.313e-04  3.206e-05  10.334  < 2e-16
    ## LDA_03       -3.859e-01  1.166e-01  -3.309 0.000935
    ## num_imgs      1.583e-02  4.021e-03   3.936 8.29e-05
    ## num_keywords  5.471e-02  1.483e-02   3.690 0.000224
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   ***
    ## LDA_03       ***
    ## num_imgs     ***
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 7213.6  on 5203  degrees of freedom
    ## Residual deviance: 7033.2  on 5199  degrees of freedom
    ## AIC: 7043.2
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
    ## -2.6097  -1.1138  -0.9259   1.1844   1.5869  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.357e+00  1.376e-01  -9.862  < 2e-16
    ## kw_avg_avg    2.643e-04  3.304e-05   8.001 1.24e-15
    ## kw_avg_min   -6.187e-05  7.656e-05  -0.808 0.419016
    ## kw_min_avg    3.308e-05  3.192e-05   1.036 0.300083
    ## num_imgs      1.384e-02  3.981e-03   3.475 0.000511
    ## num_keywords  6.204e-02  1.586e-02   3.913 9.13e-05
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   ***
    ## kw_avg_min      
    ## kw_min_avg      
    ## num_imgs     ***
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 7213.6  on 5203  degrees of freedom
    ## Residual deviance: 7042.3  on 5198  degrees of freedom
    ## AIC: 7054.3
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
    ## -2.5683  -1.1134  -0.9318   1.1834   1.5822  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.351e+00  1.375e-01  -9.829  < 2e-16
    ## kw_avg_avg    2.569e-04  3.173e-05   8.095 5.72e-16
    ## kw_min_avg    3.721e-05  3.149e-05   1.182 0.237396
    ## num_imgs      1.402e-02  3.975e-03   3.528 0.000418
    ## num_keywords  6.114e-02  1.583e-02   3.863 0.000112
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   ***
    ## kw_min_avg      
    ## num_imgs     ***
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 7213.6  on 5203  degrees of freedom
    ## Residual deviance: 7042.9  on 5199  degrees of freedom
    ## AIC: 7052.9
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
    ## -1.240  -1.163  -1.064   1.192   1.348  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -0.45294    0.10834  -4.181 2.91e-05
    ## num_keywords  0.05997    0.01460   4.106 4.03e-05
    ##                 
    ## (Intercept)  ***
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 7213.6  on 5203  degrees of freedom
    ## Residual deviance: 7196.7  on 5202  degrees of freedom
    ## AIC: 7200.7
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
    ## [1,] 0.7687271 0.7737153 0.7589944 0.7605706
    ##          [,5]
    ## [1,] 0.707128

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

    ## # A tibble: 5,204 x 8
    ##    group shares kw_avg_avg kw_avg_min kw_min_avg
    ##    <fct>  <dbl>      <dbl>      <dbl>      <dbl>
    ##  1 less~    814      2501.       80.2      1545.
    ##  2 less~    688      2569.      179        1912.
    ##  3 less~   1300      3133.      343.       2541.
    ##  4 more~ 105400      5302.      700.       2917.
    ##  5 more~   5300      4745.      136.       3310.
    ##  6 less~   1100      2429.      484.       1759 
    ##  7 more~   1700      3769.      519           0 
    ##  8 more~   1500      2564.      230.       1626.
    ##  9 more~  11700      5303.      127.       3580.
    ## 10 more~   2100      4319.      139.          0 
    ## # ... with 5,194 more rows, and 3 more variables:
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

    ## [1] less than 1400 less than 1400 more than 1400
    ## [4] less than 1400 more than 1400 more than 1400
    ## Levels: less than 1400 more than 1400

**Compare Predictions to Actual**

``` r
fullTbl <- table(data.frame(rfPred, dataTest$group))

fullTbl
```

    ##                 dataTest.group
    ## rfPred           less than 1400 more than 1400
    ##   less than 1400           1259              0
    ##   more than 1400              0            972

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
    ##   less than 1400            834            516
    ##   more than 1400            425            456

**Find MisClassification Rate**

``` r
rfMis <- 1 - sum(diag(fullTbl)/sum(fullTbl))

rfMis
```

    ## [1] 0.421784

### Analysis

This does not help. I will keep my first Random Forest Model for
prediction.

# Models Used

Overall, I have chosen the following models for my data.

1.  glm5Fit: Logistic Regression Model  
2.  rfFit : Random Forest Model
