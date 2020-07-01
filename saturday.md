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

![](saturday_files/figure-gfm/scatter%20data-1.png)<!-- -->

I will take a closer look at a few of these variables.

``` r
dataGathered <- dataGathered %>% filter(variable %in% c("kw_avg_avg", "kw_avg_min", "kw_min_avg", "LDA_03", "num_imgs", "num_keywords"))

ggplot(dataGathered, aes(x = value, y = shares)) + geom_point() + facet_wrap(~variable)
```

![](saturday_files/figure-gfm/scatter%20data%202-1.png)<!-- -->

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

    ##      shares         kw_avg_avg      kw_avg_min    
    ##  Min.   :    43   Min.   : 1115   Min.   :  -1.0  
    ##  1st Qu.:  1300   1st Qu.: 2519   1st Qu.: 143.2  
    ##  Median :  2000   Median : 3041   Median : 244.6  
    ##  Mean   :  4079   Mean   : 3314   Mean   : 299.5  
    ##  3rd Qu.:  3600   3rd Qu.: 3847   3rd Qu.: 357.4  
    ##  Max.   :617900   Max.   :36717   Max.   :8549.3  
    ##    kw_min_avg       LDA_03          num_imgs      
    ##  Min.   :   0   Min.   :0.0182   Min.   :  0.000  
    ##  1st Qu.:   0   1st Qu.:0.0250   1st Qu.:  1.000  
    ##  Median :1291   Median :0.0400   Median :  1.000  
    ##  Mean   :1280   Mean   :0.2321   Mean   :  5.564  
    ##  3rd Qu.:2200   3rd Qu.:0.4068   3rd Qu.:  8.000  
    ##  Max.   :3594   Max.   :0.9200   Max.   :101.000  
    ##   num_keywords   
    ##  Min.   : 1.000  
    ##  1st Qu.: 6.000  
    ##  Median : 8.000  
    ##  Mean   : 7.555  
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
    ##  -7377  -2473  -1404   -107 611653 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)   613.13097 1869.53631   0.328  0.74298
    ## kw_avg_avg      0.03212    0.28589   0.112  0.91056
    ## kw_avg_min      0.29599    1.02100   0.290  0.77193
    ## kw_min_avg      0.27363    0.37324   0.733  0.46359
    ## LDA_03       4718.56848 1458.55137   3.235  0.00124
    ## num_imgs       -6.49427   42.33601  -0.153  0.87810
    ## num_keywords  246.43036  208.18731   1.184  0.23670
    ##                
    ## (Intercept)    
    ## kw_avg_avg     
    ## kw_avg_min     
    ## kw_min_avg     
    ## LDA_03       **
    ## num_imgs       
    ## num_keywords   
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16060 on 1710 degrees of freedom
    ## Multiple R-squared:  0.00868,    Adjusted R-squared:  0.005202 
    ## F-statistic: 2.495 on 6 and 1710 DF,  p-value: 0.02085

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
    ## -12510  -2608  -1868   -520 613596 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)   896.4799  1872.6422   0.479    0.632
    ## kw_avg_avg      0.3493     0.2693   1.297    0.195
    ## kw_avg_min     -0.1943     1.0125  -0.192    0.848
    ## kw_min_avg      0.2206     0.3739   0.590    0.555
    ## num_imgs       23.4956    41.4227   0.567    0.571
    ## num_keywords  221.0704   208.6144   1.060    0.289
    ## 
    ## Residual standard error: 16100 on 1711 degrees of freedom
    ## Multiple R-squared:  0.002613,   Adjusted R-squared:  -0.0003019 
    ## F-statistic: 0.8964 on 5 and 1711 DF,  p-value: 0.4825

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
    ## -14642  -2611  -1941   -476 613932 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) 2733.2201   899.7588   3.038  0.00242 **
    ## kw_avg_avg     0.4061     0.2449   1.658  0.09741 . 
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16090 on 1715 degrees of freedom
    ## Multiple R-squared:  0.001601,   Adjusted R-squared:  0.001019 
    ## F-statistic:  2.75 on 1 and 1715 DF,  p-value: 0.09741

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

    ## # A tibble: 2,453 x 7
    ##    logShares kw_avg_avg kw_avg_min kw_min_avg LDA_03
    ##        <dbl>      <dbl>      <dbl>      <dbl>  <dbl>
    ##  1         1      1480.       526.         0  0.209 
    ##  2         1      1604.       508.         0  0.0333
    ##  3         1      1759.       483.         0  0.537 
    ##  4         1      1731.       335.      1012. 0.0286
    ##  5         1      2342.       569.         0  0.815 
    ##  6         1      1685.       381          0  0.145 
    ##  7         1      2187.       838.         0  0.0255
    ##  8         1      2217.       486.      1222. 0.325 
    ##  9         1      1458.       940.         0  0.0223
    ## 10         1      1394.       467.         0  0.181 
    ## # ... with 2,443 more rows, and 2 more variables:
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
    ##   -1.1961954     0.0001542     0.0002877  
    ##   kw_min_avg        LDA_03      num_imgs  
    ##    0.0001819    -0.1561120     0.0006179  
    ## num_keywords  
    ##    0.2014380  
    ## 
    ## Degrees of Freedom: 1716 Total (i.e. Null);  1710 Residual
    ## Null Deviance:       1956 
    ## Residual Deviance: 1885  AIC: 1899

``` r
summary(glmALL)
```

    ## 
    ## Call:
    ## glm(formula = logShares ~ ., family = "binomial", data = data1Train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.2401  -1.2439   0.6773   0.7944   1.2290  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.196e+00  2.890e-01  -4.139 3.48e-05
    ## kw_avg_avg    1.542e-04  7.544e-05   2.044   0.0409
    ## kw_avg_min    2.877e-04  2.450e-04   1.174   0.2403
    ## kw_min_avg    1.819e-04  6.086e-05   2.989   0.0028
    ## LDA_03       -1.561e-01  2.363e-01  -0.661   0.5087
    ## num_imgs      6.179e-04  6.384e-03   0.097   0.9229
    ## num_keywords  2.014e-01  3.131e-02   6.434 1.24e-10
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   *  
    ## kw_avg_min      
    ## kw_min_avg   ** 
    ## LDA_03          
    ## num_imgs        
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1956.4  on 1716  degrees of freedom
    ## Residual deviance: 1885.2  on 1710  degrees of freedom
    ## AIC: 1899.2
    ## 
    ## Number of Fisher Scoring iterations: 5

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
    ## -4.2208  -1.2624   0.6762   0.7971   1.1509  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.074e+00  2.883e-01  -3.727 0.000194
    ## kw_avg_avg    2.888e-04  6.691e-05   4.317 1.58e-05
    ## LDA_03       -3.640e-01  2.278e-01  -1.598 0.110106
    ## num_imgs      1.547e-03  6.328e-03   0.245 0.806835
    ## num_keywords  1.738e-01  2.824e-02   6.154 7.57e-10
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   ***
    ## LDA_03          
    ## num_imgs        
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1956.4  on 1716  degrees of freedom
    ## Residual deviance: 1894.9  on 1712  degrees of freedom
    ## AIC: 1904.9
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
    ## -3.0315  -1.2444   0.6774   0.7944   1.2231  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.177e+00  2.859e-01  -4.116 3.85e-05
    ## kw_avg_avg    1.285e-04  6.251e-05   2.055  0.03983
    ## kw_avg_min    3.138e-04  2.424e-04   1.295  0.19548
    ## kw_min_avg    1.910e-04  5.893e-05   3.240  0.00119
    ## num_imgs     -1.264e-04  6.257e-03  -0.020  0.98389
    ## num_keywords  2.032e-01  3.118e-02   6.516 7.22e-11
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   *  
    ## kw_avg_min      
    ## kw_min_avg   ** 
    ## num_imgs        
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1956.4  on 1716  degrees of freedom
    ## Residual deviance: 1885.7  on 1711  degrees of freedom
    ## AIC: 1897.7
    ## 
    ## Number of Fisher Scoring iterations: 5

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
    ## -3.1383  -1.2500   0.6761   0.8020   1.1991  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -1.177e+00  2.860e-01  -4.116 3.85e-05
    ## kw_avg_avg    1.436e-04  6.167e-05   2.328  0.01992
    ## kw_min_avg    1.835e-04  5.872e-05   3.125  0.00178
    ## num_imgs     -5.900e-04  6.219e-03  -0.095  0.92442
    ## num_keywords  2.103e-01  3.071e-02   6.847 7.54e-12
    ##                 
    ## (Intercept)  ***
    ## kw_avg_avg   *  
    ## kw_min_avg   ** 
    ## num_imgs        
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1956.4  on 1716  degrees of freedom
    ## Residual deviance: 1887.7  on 1712  degrees of freedom
    ## AIC: 1897.7
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
    ##     Min       1Q   Median       3Q      Max  
    ## -1.8487  -1.3111   0.6828   0.7933   1.1919  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -0.20550    0.20973  -0.980    0.327
    ## num_keywords  0.17145    0.02792   6.142 8.16e-10
    ##                 
    ## (Intercept)     
    ## num_keywords ***
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1956.4  on 1716  degrees of freedom
    ## Residual deviance: 1918.3  on 1715  degrees of freedom
    ## AIC: 1922.3
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

    ##           [,1]      [,2]      [,3]      [,4]
    ## [1,] 0.7052987 0.6836843 0.7056426 0.7008286
    ##           [,5]
    ## [1,] 0.6160626

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

    ## # A tibble: 1,717 x 8
    ##    group shares kw_avg_avg kw_avg_min kw_min_avg
    ##    <fct>  <dbl>      <dbl>      <dbl>      <dbl>
    ##  1 less~   1200      3770.       220.      2617.
    ##  2 more~   4100      3672.       708.      2600 
    ##  3 more~   3300      2690.       320.       789 
    ##  4 less~   1100      2844.       135.      2198.
    ##  5 more~   6900      3541.       698.         0 
    ##  6 more~   2100      2646.        -1       2169.
    ##  7 more~   1700      2309.       438.         0 
    ##  8 more~   1800      3950.       183.         0 
    ##  9 more~   5600      1444.       325.         0 
    ## 10 more~   2600      3787.       172          0 
    ## # ... with 1,707 more rows, and 3 more variables:
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

    ## [1] more than 1400 more than 1400 more than 1400
    ## [4] less than 1400 more than 1400 more than 1400
    ## Levels: less than 1400 more than 1400

**Compare Predictions to Actual**

``` r
fullTbl <- table(data.frame(rfPred, dataTest$group))

fullTbl
```

    ##                 dataTest.group
    ## rfPred           less than 1400 more than 1400
    ##   less than 1400            221              0
    ##   more than 1400              0            515

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
    ##   less than 1400             46             75
    ##   more than 1400            175            440

**Find MisClassification Rate**

``` r
rfMis <- 1 - sum(diag(fullTbl)/sum(fullTbl))

rfMis
```

    ## [1] 0.3396739

### Analysis

This does not help. I will keep my first Random Forest Model for
prediction.

# Models Used

Overall, I have chosen the following models for my data.

1.  glm5Fit: Logistic Regression Model  
2.  rfFit : Random Forest Model
