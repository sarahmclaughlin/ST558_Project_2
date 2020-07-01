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

![](thursday_files/figure-gfm/scatter%20data-1.png)<!-- -->

I will take a closer look at a few of these variables.

``` r
dataGathered <- dataGathered %>% filter(variable %in% c("kw_avg_avg", "kw_avg_min", "kw_min_avg", "LDA_03", "num_imgs", "num_keywords"))

ggplot(dataGathered, aes(x = value, y = shares)) + geom_point() + facet_wrap(~variable)
```

![](thursday_files/figure-gfm/scatter%20data%202-1.png)<!-- -->

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
    ##  Min.   :     5   Min.   :  675.2  
    ##  1st Qu.:   898   1st Qu.: 2367.0  
    ##  Median :  1400   Median : 2854.2  
    ##  Mean   :  3144   Mean   : 3112.3  
    ##  3rd Qu.:  2600   3rd Qu.: 3588.8  
    ##  Max.   :306100   Max.   :24260.5  
    ##    kw_avg_min        kw_min_avg       LDA_03       
    ##  Min.   :   -1.0   Min.   :   0   Min.   :0.01818  
    ##  1st Qu.:  143.3   1st Qu.:   0   1st Qu.:0.02857  
    ##  Median :  237.1   Median : 968   Median :0.04000  
    ##  Mean   :  316.0   Mean   :1092   Mean   :0.22392  
    ##  3rd Qu.:  355.8   3rd Qu.:2023   3rd Qu.:0.38064  
    ##  Max.   :21516.0   Max.   :3610   Max.   :0.91994  
    ##     num_imgs       num_keywords   
    ##  Min.   :  0.00   Min.   : 1.000  
    ##  1st Qu.:  1.00   1st Qu.: 6.000  
    ##  Median :  1.00   Median : 7.000  
    ##  Mean   :  4.44   Mean   : 7.166  
    ##  3rd Qu.:  4.00   3rd Qu.: 9.000  
    ##  Max.   :100.00   Max.   :10.000

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
    ## -35040  -1912  -1029    -99 303196 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  -599.6303   545.3630  -1.100 0.271599
    ## kw_avg_avg      1.4049     0.1449   9.695  < 2e-16
    ## kw_avg_min      1.7806     0.2346   7.591 3.77e-14
    ## kw_min_avg     -0.4918     0.1275  -3.859 0.000115
    ## LDA_03        117.7165   460.1950   0.256 0.798118
    ## num_imgs       45.3461    14.5678   3.113 0.001864
    ## num_keywords -123.1363    63.0911  -1.952 0.051026
    ##                 
    ## (Intercept)     
    ## kw_avg_avg   ***
    ## kw_avg_min   ***
    ## kw_min_avg   ***
    ## LDA_03          
    ## num_imgs     ** 
    ## num_keywords .  
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8105 on 5079 degrees of freedom
    ## Multiple R-squared:  0.06489,    Adjusted R-squared:  0.06379 
    ## F-statistic: 58.74 on 6 and 5079 DF,  p-value: < 2.2e-16

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
    ## -34940  -1915  -1035    -97 303181 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  -619.9989   539.4689  -1.149   0.2505
    ## kw_avg_avg      1.4231     0.1261  11.287  < 2e-16
    ## kw_avg_min      1.7683     0.2296   7.702 1.60e-14
    ## kw_min_avg     -0.4960     0.1264  -3.924 8.83e-05
    ## num_imgs       45.8504    14.4324   3.177   0.0015
    ## num_keywords -123.6831    63.0491  -1.962   0.0499
    ##                 
    ## (Intercept)     
    ## kw_avg_avg   ***
    ## kw_avg_min   ***
    ## kw_min_avg   ***
    ## num_imgs     ** 
    ## num_keywords *  
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8104 on 5080 degrees of freedom
    ## Multiple R-squared:  0.06488,    Adjusted R-squared:  0.06396 
    ## F-statistic: 70.49 on 5 and 5080 DF,  p-value: < 2.2e-16

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
    ## -18852  -2033  -1078     15 304076 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -1.633e+03  3.236e+02  -5.048 4.63e-07
    ## kw_avg_avg   1.535e+00  9.722e-02  15.788  < 2e-16
    ##                
    ## (Intercept) ***
    ## kw_avg_avg  ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8179 on 5084 degrees of freedom
    ## Multiple R-squared:  0.04674,    Adjusted R-squared:  0.04655 
    ## F-statistic: 249.3 on 1 and 5084 DF,  p-value: < 2.2e-16

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

    ## # A tibble: 7,267 x 7
    ##    logShares kw_avg_avg kw_avg_min kw_min_avg LDA_03
    ##        <dbl>      <dbl>      <dbl>      <dbl>  <dbl>
    ##  1         0      2144.       614.         0  0.0250
    ##  2         0      2295.       495.         0  0.599 
    ##  3         1      1354.       448.         0  0.0426
    ##  4         0      2061.       500.       593  0.0288
    ##  5         0      1545.       487.         0  0.0335
    ##  6         0      1321.       544.         0  0.0333
    ##  7         1      1023.       577          0  0.0222
    ##  8         0       924.       466.         0  0.238 
    ##  9         1      1756.       409.         0  0.420 
    ## 10         0      1747.       751.      1453. 0.231 
    ## # ... with 7,257 more rows, and 2 more variables:
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
    ##   -1.564e+00     3.622e-04    -8.739e-05  
    ##   kw_min_avg        LDA_03      num_imgs  
    ##    6.454e-05    -2.531e-01     1.093e-02  
    ## num_keywords  
    ##    5.831e-02  
    ## 
    ## Degrees of Freedom: 5085 Total (i.e. Null);  5079 Residual
    ## Null Deviance:       7051 
    ## Residual Deviance: 6825  AIC: 6839

``` r
summary(glmALL)
```

    ## 
    ## Call:
    ## glm(formula = logShares ~ ., family = "binomial", data = data1Train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.8083  -1.1142   0.5073   1.1647   1.6100  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.564e+00  1.435e-01 -10.902  < 2e-16
    ## kw_avg_avg    3.622e-04  4.115e-05   8.804  < 2e-16
    ## kw_avg_min   -8.739e-05  7.586e-05  -1.152 0.249307
    ## kw_min_avg    6.454e-05  3.296e-05   1.958 0.050193
    ## LDA_03       -2.531e-01  1.192e-01  -2.123 0.033758
    ## num_imgs      1.093e-02  3.798e-03   2.876 0.004023
    ## num_keywords  5.831e-02  1.603e-02   3.638 0.000275
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   ***
    ## kw_avg_min      
    ## kw_min_avg   .  
    ## LDA_03       *  
    ## num_imgs     ** 
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 7050.6  on 5085  degrees of freedom
    ## Residual deviance: 6824.9  on 5079  degrees of freedom
    ## AIC: 6838.9
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
    ## -2.8757  -1.1119   0.4691   1.1661   1.5946  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.5198703  0.1428015 -10.643  < 2e-16
    ## kw_avg_avg    0.0003954  0.0000338  11.700  < 2e-16
    ## LDA_03       -0.2730188  0.1169228  -2.335  0.01954
    ## num_imgs      0.0113521  0.0037909   2.995  0.00275
    ## num_keywords  0.0443769  0.0148605   2.986  0.00282
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   ***
    ## LDA_03       *  
    ## num_imgs     ** 
    ## num_keywords ** 
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 7050.6  on 5085  degrees of freedom
    ## Residual deviance: 6830.9  on 5081  degrees of freedom
    ## AIC: 6840.9
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
    ## -2.6877  -1.1128   0.5444   1.1719   1.5679  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.510e+00  1.407e-01 -10.728  < 2e-16
    ## kw_avg_avg    3.173e-04  3.478e-05   9.122  < 2e-16
    ## kw_avg_min   -6.229e-05  7.489e-05  -0.832 0.405541
    ## kw_min_avg    7.635e-05  3.239e-05   2.357 0.018427
    ## num_imgs      9.903e-03  3.761e-03   2.633 0.008462
    ## num_keywords  6.000e-02  1.600e-02   3.751 0.000176
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   ***
    ## kw_avg_min      
    ## kw_min_avg   *  
    ## num_imgs     ** 
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 7050.6  on 5085  degrees of freedom
    ## Residual deviance: 6829.5  on 5080  degrees of freedom
    ## AIC: 6841.5
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
    ## -2.6613  -1.1110   0.5413   1.1723   1.5624  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.504e+00  1.407e-01 -10.694  < 2e-16
    ## kw_avg_avg    3.099e-04  3.363e-05   9.216  < 2e-16
    ## kw_min_avg    8.031e-05  3.204e-05   2.507 0.012190
    ## num_imgs      1.010e-02  3.754e-03   2.691 0.007127
    ## num_keywords  5.908e-02  1.596e-02   3.702 0.000214
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   ***
    ## kw_min_avg   *  
    ## num_imgs     ** 
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 7050.6  on 5085  degrees of freedom
    ## Residual deviance: 6830.1  on 5081  degrees of freedom
    ## AIC: 6840.1
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
    ## -1.242  -1.177   1.114   1.178   1.310  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -0.35730    0.10819  -3.302 0.000958
    ## num_keywords  0.05085    0.01458   3.487 0.000488
    ##                 
    ## (Intercept)  ***
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 7050.6  on 5085  degrees of freedom
    ## Residual deviance: 7038.4  on 5084  degrees of freedom
    ## AIC: 7042.4
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
    ## [1,] 0.8276567 0.8364181 0.8153156 0.8133379
    ##           [,5]
    ## [1,] 0.7128586

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

    ## # A tibble: 5,086 x 8
    ##    group shares kw_avg_avg kw_avg_min kw_min_avg
    ##    <fct>  <dbl>      <dbl>      <dbl>      <dbl>
    ##  1 more~   3200      3228.      620.          0 
    ##  2 more~   7200      4414.      262.       2710.
    ##  3 more~   2000      2527.      484.          0 
    ##  4 less~   1300      3057.      334.       1900 
    ##  5 less~   1300      2882.      120.       2401.
    ##  6 less~    606      1883.      391.          0 
    ##  7 more~  17800      2242.       22.7         0 
    ##  8 less~   1000      4678.      223.       2011.
    ##  9 more~   3300      3126.      274.          0 
    ## 10 more~   1800      3076.      237           0 
    ## # ... with 5,076 more rows, and 3 more variables:
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
    ## [4] more than 1400 more than 1400 more than 1400
    ## Levels: less than 1400 more than 1400

**Compare Predictions to Actual**

``` r
fullTbl <- table(data.frame(rfPred, dataTest$group))

fullTbl
```

    ##                 dataTest.group
    ## rfPred           less than 1400 more than 1400
    ##   less than 1400           1131              0
    ##   more than 1400              0           1050

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
    ##   less than 1400            757            504
    ##   more than 1400            374            546

**Find MisClassification Rate**

``` r
rfMis <- 1 - sum(diag(fullTbl)/sum(fullTbl))

rfMis
```

    ## [1] 0.4025676

### Analysis

This does not help. I will keep my first Random Forest Model for
prediction.

# Models Used

Overall, I have chosen the following models for my data.

1.  glm5Fit: Logistic Regression Model  
2.  rfFit : Random Forest Model
