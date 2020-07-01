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

![](tuesday_files/figure-gfm/scatter%20data-1.png)<!-- -->

I will take a closer look at a few of these variables.

``` r
dataGathered <- dataGathered %>% filter(variable %in% c("kw_avg_avg", "kw_avg_min", "kw_min_avg", "LDA_03", "num_imgs", "num_keywords"))

ggplot(dataGathered, aes(x = value, y = shares)) + geom_point() + facet_wrap(~variable)
```

![](tuesday_files/figure-gfm/scatter%20data%202-1.png)<!-- -->

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
    ##  Min.   :    42   Min.   :  713.9  
    ##  1st Qu.:   904   1st Qu.: 2344.4  
    ##  Median :  1300   Median : 2836.5  
    ##  Mean   :  3298   Mean   : 3114.4  
    ##  3rd Qu.:  2500   3rd Qu.: 3555.1  
    ##  Max.   :441000   Max.   :27391.6  
    ##    kw_avg_min        kw_min_avg    
    ##  Min.   :   -1.0   Min.   :  -1.0  
    ##  1st Qu.:  140.4   1st Qu.:   0.0  
    ##  Median :  231.1   Median : 986.6  
    ##  Mean   :  302.8   Mean   :1103.1  
    ##  3rd Qu.:  351.8   3rd Qu.:2047.4  
    ##  Max.   :15851.2   Max.   :3609.7  
    ##      LDA_03           num_imgs      
    ##  Min.   :0.00000   Min.   :  0.000  
    ##  1st Qu.:0.02857   1st Qu.:  1.000  
    ##  Median :0.04000   Median :  1.000  
    ##  Mean   :0.22032   Mean   :  4.489  
    ##  3rd Qu.:0.35540   3rd Qu.:  4.000  
    ##  Max.   :0.91989   Max.   :100.000  
    ##   num_keywords   
    ##  Min.   : 1.000  
    ##  1st Qu.: 6.000  
    ##  Median : 7.000  
    ##  Mean   : 7.191  
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
    ##    Min     1Q Median     3Q    Max 
    ## -21509  -2190  -1204   -106 435476 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  -1942.4734   705.5258  -2.753 0.005922
    ## kw_avg_avg       1.2831     0.1657   7.744 1.15e-14
    ## kw_avg_min      -1.1419     0.3412  -3.347 0.000823
    ## kw_min_avg      -0.1884     0.1603  -1.175 0.239927
    ## LDA_03         911.1851   585.0051   1.558 0.119397
    ## num_imgs        53.8973    18.4659   2.919 0.003530
    ## num_keywords   188.5004    83.5702   2.256 0.024138
    ##                 
    ## (Intercept)  ** 
    ## kw_avg_avg   ***
    ## kw_avg_min   ***
    ## kw_min_avg      
    ## LDA_03          
    ## num_imgs     ** 
    ## num_keywords *  
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10670 on 5166 degrees of freedom
    ## Multiple R-squared:  0.02714,    Adjusted R-squared:  0.02601 
    ## F-statistic: 24.02 on 6 and 5166 DF,  p-value: < 2.2e-16

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
    ## -24481  -2212  -1242   -126 435672 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  -2071.9053   700.7115  -2.957 0.003122
    ## kw_avg_avg       1.4008     0.1475   9.498  < 2e-16
    ## kw_avg_min      -1.2393     0.3354  -3.695 0.000223
    ## kw_min_avg      -0.2083     0.1598  -1.303 0.192667
    ## num_imgs        57.3755    18.3329   3.130 0.001760
    ## num_keywords   188.4114    83.5817   2.254 0.024224
    ##                 
    ## (Intercept)  ** 
    ## kw_avg_avg   ***
    ## kw_avg_min   ***
    ## kw_min_avg      
    ## num_imgs     ** 
    ## num_keywords *  
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10670 on 5167 degrees of freedom
    ## Multiple R-squared:  0.02668,    Adjusted R-squared:  0.02574 
    ## F-statistic: 28.33 on 5 and 5167 DF,  p-value: < 2.2e-16

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
    ## -21238  -2173  -1377   -340 437048 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -516.9663   395.0075  -1.309    0.191
    ## kw_avg_avg     1.2250     0.1175  10.426   <2e-16
    ##                
    ## (Intercept)    
    ## kw_avg_avg  ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10700 on 5171 degrees of freedom
    ## Multiple R-squared:  0.02059,    Adjusted R-squared:  0.0204 
    ## F-statistic: 108.7 on 1 and 5171 DF,  p-value: < 2.2e-16

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

    ## # A tibble: 7,390 x 7
    ##    logShares kw_avg_avg kw_avg_min kw_min_avg LDA_03
    ##        <dbl>      <dbl>      <dbl>      <dbl>  <dbl>
    ##  1         0       804.       422.          0 0.0334
    ##  2         0       728.       408.          0 0.200 
    ##  3         1      1185.       300.          0 0.0200
    ##  4         0      1114.       461           0 0.0510
    ##  5         0       885.       581.          0 0.0201
    ##  6         0      1191.       408.          0 0.267 
    ##  7         1      1539.       306.        480 0.0250
    ##  8         1      1400.       531           0 0.733 
    ##  9         0      1803.      1007.          0 0.0200
    ## 10         1       714.       405           0 0.0401
    ## # ... with 7,380 more rows, and 2 more variables:
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
    ##   -1.612e+00     4.232e-04    -9.057e-06  
    ##   kw_min_avg        LDA_03      num_imgs  
    ##   -3.251e-05    -3.995e-01     1.143e-02  
    ## num_keywords  
    ##    5.297e-02  
    ## 
    ## Degrees of Freedom: 5172 Total (i.e. Null);  5166 Residual
    ## Null Deviance:       7171 
    ## Residual Deviance: 6944  AIC: 6958

``` r
summary(glmALL)
```

    ## 
    ## Call:
    ## glm(formula = logShares ~ ., family = "binomial", data = data1Train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.2828  -1.1096  -0.8115   1.1686   1.6447  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.612e+00  1.447e-01 -11.142  < 2e-16
    ## kw_avg_avg    4.232e-04  4.294e-05   9.855  < 2e-16
    ## kw_avg_min   -9.057e-06  9.649e-05  -0.094 0.925214
    ## kw_min_avg   -3.251e-05  3.337e-05  -0.974 0.330028
    ## LDA_03       -3.995e-01  1.205e-01  -3.315 0.000915
    ## num_imgs      1.143e-02  3.646e-03   3.135 0.001716
    ## num_keywords  5.297e-02  1.625e-02   3.260 0.001113
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   ***
    ## kw_avg_min      
    ## kw_min_avg      
    ## LDA_03       ***
    ## num_imgs     ** 
    ## num_keywords ** 
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 7171.3  on 5172  degrees of freedom
    ## Residual deviance: 6944.4  on 5166  degrees of freedom
    ## AIC: 6958.4
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
    ## -3.1933  -1.1103  -0.8094   1.1690   1.6418  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.620e+00  1.441e-01 -11.243  < 2e-16
    ## kw_avg_avg    3.988e-04  3.375e-05  11.816  < 2e-16
    ## LDA_03       -3.763e-01  1.169e-01  -3.220 0.001283
    ## num_imgs      1.134e-02  3.642e-03   3.114 0.001847
    ## num_keywords  5.853e-02  1.509e-02   3.879 0.000105
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   ***
    ## LDA_03       ** 
    ## num_imgs     ** 
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 7171.3  on 5172  degrees of freedom
    ## Residual deviance: 6945.4  on 5168  degrees of freedom
    ## AIC: 6955.4
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
    ## -3.0857  -1.1079  -0.8672   1.1820   1.6044  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.514e+00  1.407e-01 -10.759  < 2e-16
    ## kw_avg_avg    3.465e-04  3.519e-05   9.848  < 2e-16
    ## kw_avg_min    3.753e-05  9.423e-05   0.398 0.690448
    ## kw_min_avg   -1.026e-05  3.245e-05  -0.316 0.751944
    ## num_imgs      1.014e-02  3.610e-03   2.808 0.004983
    ## num_keywords  5.539e-02  1.620e-02   3.419 0.000629
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   ***
    ## kw_avg_min      
    ## kw_min_avg      
    ## num_imgs     ** 
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 7171.3  on 5172  degrees of freedom
    ## Residual deviance: 6955.5  on 5167  degrees of freedom
    ## AIC: 6967.5
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
    ## -3.0358  -1.1081  -0.8642   1.1812   1.6090  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.513e+00  1.406e-01 -10.761  < 2e-16
    ## kw_avg_avg    3.493e-04  3.447e-05  10.133  < 2e-16
    ## kw_min_avg   -1.199e-05  3.216e-05  -0.373 0.709184
    ## num_imgs      1.008e-02  3.607e-03   2.795 0.005182
    ## num_keywords  5.599e-02  1.613e-02   3.472 0.000517
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   ***
    ## kw_min_avg      
    ## num_imgs     ** 
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 7171.3  on 5172  degrees of freedom
    ## Residual deviance: 6955.7  on 5168  degrees of freedom
    ## AIC: 6965.7
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
    ## -1.254  -1.171  -1.011   1.184   1.325  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -0.46930    0.11002  -4.265  2.0e-05
    ## num_keywords  0.06478    0.01480   4.377  1.2e-05
    ##                 
    ## (Intercept)  ***
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 7171.3  on 5172  degrees of freedom
    ## Residual deviance: 7152.0  on 5171  degrees of freedom
    ## AIC: 7156
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
    ## [1,] 0.8077438 0.8003514 0.7874139 0.7877065
    ##           [,5]
    ## [1,] 0.7045153

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

    ## # A tibble: 5,173 x 8
    ##    group shares kw_avg_avg kw_avg_min kw_min_avg
    ##    <fct>  <dbl>      <dbl>      <dbl>      <dbl>
    ##  1 more~   3800      2737.      615.          0 
    ##  2 less~    633      2582.      222.       1005.
    ##  3 less~    178      2923.      223           0 
    ##  4 less~   1400      5502.      176.       3524.
    ##  5 more~   2200      3227.      391.       1533.
    ##  6 less~   1300      2696.      318.          0 
    ##  7 less~    927      3421.      265        2879.
    ##  8 less~    674      1919.       65.6         0 
    ##  9 less~   1000      2857.      117.       1295.
    ## 10 more~  11100      5258.      114.       3484.
    ## # ... with 5,163 more rows, and 3 more variables:
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

    ## [1] more than 1400 less than 1400 less than 1400
    ## [4] more than 1400 less than 1400 more than 1400
    ## Levels: less than 1400 more than 1400

**Compare Predictions to Actual**

``` r
fullTbl <- table(data.frame(rfPred, dataTest$group))

fullTbl
```

    ##                 dataTest.group
    ## rfPred           less than 1400 more than 1400
    ##   less than 1400           1210              0
    ##   more than 1400              0           1007

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
    ##   less than 1400            807            505
    ##   more than 1400            403            502

**Find MisClassification Rate**

``` r
rfMis <- 1 - sum(diag(fullTbl)/sum(fullTbl))

rfMis
```

    ## [1] 0.4095625

### Analysis

This does not help. I will keep my first Random Forest Model for
prediction.

# Models Used

Overall, I have chosen the following models for my data.

1.  glm5Fit: Logistic Regression Model  
2.  rfFit : Random Forest Model
