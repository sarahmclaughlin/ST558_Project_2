ST 558 Project 2
================
Sarah McLaughlin
6/22/2020

# Introduction

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

## Exploratory Data Analysis

Here, I will do a basic analysis of my variables to see basic trends,
and correlations.

*Correlation of all Variables*

``` r
data <- data %>% select(-url)

correlation <- cor(data, method = "spearman")
```

Take only those with a correlation to shares of \> 0.10.

``` r
shareCor <- correlation[60, ] >= 0.1

corMax <- correlation[60, shareCor]

corMax
```

    ##     data_channel_is_socmed                 kw_min_avg 
    ##                  0.1135715                  0.1032421 
    ##                 kw_max_avg                 kw_avg_avg 
    ##                  0.2232914                  0.2556222 
    ##  self_reference_min_shares  self_reference_max_shares 
    ##                  0.1815168                  0.1687247 
    ## self_reference_avg_sharess        weekday_is_saturday 
    ##                  0.1921745                  0.1088596 
    ##                 is_weekend        global_subjectivity 
    ##                  0.1517175                  0.1135482 
    ##                     shares 
    ##                  1.0000000

Based on correlation values, these variables of note that will be used
in our analysis and prediction:

1.  shares
      - (target variable)  
2.  weekday\_is\_ variables
      - (weekday published)  
3.  data\_channel\_is\_socmed
      - (social media article)  
4.  kw\_max\_avg
      - (average keywords for the maximum shares)  
5.  self\_reference\_minimum\_sharess
      - (minimum shares of referenced articles)  
6.  is\_weekend
      - (published on a weekend)  
7.  kw\_min\_avg
      - (average keywords for minimum shares)  
8.  kw\_avg\_avg
      - (average keywords for average shares)  
9.  self\_reference\_max\_shares
      - (average shares of referenced articles )
10. global\_subjectivity
      - (text subjectivity)

## Select only needed variables from data for specific day

``` r
data <- data %>% 
  #filer by weekday
  filter(weekday_is_monday == 1) %>% 
  #select only needed variables. is_weekend not included
  select(shares, data_channel_is_socmed, kw_max_avg, self_reference_avg_sharess, kw_min_avg, 
         kw_avg_avg, self_reference_max_shares, global_subjectivity) %>% 
  collect()

data
```

    ## # A tibble: 6,661 x 8
    ##    shares data_channel_is~ kw_max_avg self_reference_~ kw_min_avg kw_avg_avg
    ##     <dbl>            <dbl>      <dbl>            <dbl>      <dbl>      <dbl>
    ##  1    593                0          0             496           0          0
    ##  2    711                0          0               0           0          0
    ##  3   1500                0          0             918           0          0
    ##  4   1200                0          0               0           0          0
    ##  5    505                0          0            3151.          0          0
    ##  6    855                0          0            8500           0          0
    ##  7    556                0          0            3151.          0          0
    ##  8    891                0          0            3151.          0          0
    ##  9   3600                0          0               0           0          0
    ## 10    710                0          0               0           0          0
    ## # ... with 6,651 more rows, and 2 more variables:
    ## #   self_reference_max_shares <dbl>, global_subjectivity <dbl>

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

## Compare Fit Stats Function to compare models

``` r
compareFitStats <- function(fit1, fit2){
  require(MuMIn)
  fitStats <- data.frame(fitStat = c("Adj R Square", "AIC", "AICc", "BIC"), 
              col1 = round(c(summary(fit1)$adj.r.squared, AIC(fit1), 
                             MuMIn::AICc(fit1), BIC(fit1)), 3), 
              col2 = round(c(summary(fit2)$adj.r.squared, AIC(fit2), 
                             MuMIn::AICc(fit2), BIC(fit2)), 3))
  
  #put names on returned df  
  calls <- as.list(match.call())
  calls[[1]] <- NULL
  names(fitStats[2:3])<- unlist(calls)
  fitStats
}
```

# Linear Regression Model

I will begin by running a regression model with all of the variables.

**allVarFit**

``` r
allVarFit <- lm(shares ~., data = dataTrain)

allVarFit
```

    ## 
    ## Call:
    ## lm(formula = shares ~ ., data = dataTrain)
    ## 
    ## Coefficients:
    ##                (Intercept)      data_channel_is_socmed  
    ##                 -3.513e+03                   1.821e+02  
    ##                 kw_max_avg  self_reference_avg_sharess  
    ##                 -2.727e-01                   4.343e-04  
    ##                 kw_min_avg                  kw_avg_avg  
    ##                 -6.544e-01                   2.560e+00  
    ##  self_reference_max_shares         global_subjectivity  
    ##                  1.062e-03                   3.215e+03

Then, I will create another linear model without the `kw_min_avg`
variable just to be able to compare fits.

**allButOne**

``` r
allButOne <- lm(shares ~ data_channel_is_socmed + 
                  kw_max_avg + 
                  self_reference_avg_sharess +
                  kw_avg_avg +
                  self_reference_max_shares +
                  global_subjectivity, 
                data = dataTrain
)

allButOne
```

    ## 
    ## Call:
    ## lm(formula = shares ~ data_channel_is_socmed + kw_max_avg + self_reference_avg_sharess + 
    ##     kw_avg_avg + self_reference_max_shares + global_subjectivity, 
    ##     data = dataTrain)
    ## 
    ## Coefficients:
    ##                (Intercept)      data_channel_is_socmed  
    ##                 -2.865e+03                   1.536e+02  
    ##                 kw_max_avg  self_reference_avg_sharess  
    ##                 -1.996e-01                   5.152e-04  
    ##                 kw_avg_avg   self_reference_max_shares  
    ##                  1.998e+00                   6.344e-04  
    ##        global_subjectivity  
    ##                  3.151e+03

## Comparison of Two Models

I will compare the two models using the compareFitStats function.

``` r
compareFitStats(allVarFit, allButOne)
```

    ##        fitStat       col1       col2
    ## 1 Adj R Square      0.018      0.016
    ## 2          AIC 101888.286 101894.695
    ## 3         AICc 101888.325 101894.726
    ## 4          BIC 101946.311 101946.273

### Analysis

Neither model fits the data well. I am going to try a logistic
regression model instead.

# Logistic Model

First, I need to create a logical variable to reference whether the
number of shares is less than 1400 or greater than 1400. I am still
going to use the same variables as those in my linear regression
attempt.

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

    ## # A tibble: 6,661 x 8
    ##    logShares data_channel_is~ kw_max_avg self_reference_~ kw_min_avg kw_avg_avg
    ##        <dbl>            <dbl>      <dbl>            <dbl>      <dbl>      <dbl>
    ##  1         0                0          0             496           0          0
    ##  2         0                0          0               0           0          0
    ##  3         1                0          0             918           0          0
    ##  4         0                0          0               0           0          0
    ##  5         0                0          0            3151.          0          0
    ##  6         0                0          0            8500           0          0
    ##  7         0                0          0            3151.          0          0
    ##  8         0                0          0            3151.          0          0
    ##  9         1                0          0               0           0          0
    ## 10         0                0          0               0           0          0
    ## # ... with 6,651 more rows, and 2 more variables:
    ## #   self_reference_max_shares <dbl>, global_subjectivity <dbl>

Here, I will fit a logistic regression model using the `glm()` function
with the `"binomial"` family.  
**GLM ALL Model**

``` r
glmALL <- glm(logShares ~., data = data1Train, family = "binomial")

glmALL
```

    ## 
    ## Call:  glm(formula = logShares ~ ., family = "binomial", data = data1Train)
    ## 
    ## Coefficients:
    ##                (Intercept)      data_channel_is_socmed  
    ##                 -1.628e+00                   1.105e+00  
    ##                 kw_max_avg  self_reference_avg_sharess  
    ##                 -6.630e-05                   7.798e-06  
    ##                 kw_min_avg                  kw_avg_avg  
    ##                 -1.057e-04                   5.595e-04  
    ##  self_reference_max_shares         global_subjectivity  
    ##                  6.940e-08                   7.835e-01  
    ## 
    ## Degrees of Freedom: 4661 Total (i.e. Null);  4654 Residual
    ## Null Deviance:       6460 
    ## Residual Deviance: 6186  AIC: 6202

I will remove `kw_avg_min` variable just to be able to compare fits of
the two logistic models.

**GLM All but One Model**

``` r
glmAllButOne <- glm(logShares ~ data_channel_is_socmed + 
                  kw_max_avg + 
                  self_reference_avg_sharess +
                  kw_avg_avg +
                  self_reference_max_shares +
                  global_subjectivity, 
                data = data1Train, 
                family = "binomial"
)

glmAllButOne
```

    ## 
    ## Call:  glm(formula = logShares ~ data_channel_is_socmed + kw_max_avg + 
    ##     self_reference_avg_sharess + kw_avg_avg + self_reference_max_shares + 
    ##     global_subjectivity, family = "binomial", data = data1Train)
    ## 
    ## Coefficients:
    ##                (Intercept)      data_channel_is_socmed  
    ##                 -1.514e+00                   1.104e+00  
    ##                 kw_max_avg  self_reference_avg_sharess  
    ##                 -5.326e-05                   8.270e-06  
    ##                 kw_avg_avg   self_reference_max_shares  
    ##                  4.637e-04                  -1.379e-07  
    ##        global_subjectivity  
    ##                  7.670e-01  
    ## 
    ## Degrees of Freedom: 4661 Total (i.e. Null);  4655 Residual
    ## Null Deviance:       6460 
    ## Residual Deviance: 6196  AIC: 6210

### Analysis

The AIC for the glmAllButOne model is much higher than the all variable
model. I will remove another variable, `global_subjectivity` (next
smallest correlation) and see if that helps.  
**glmAllButTwo**

``` r
glmAllButTwo <- glm(logShares ~ data_channel_is_socmed + 
                  kw_max_avg + 
                  self_reference_avg_sharess +
                  kw_avg_avg +
                  self_reference_max_shares, 
                data = data1Train, 
                family = "binomial"
)

glmAllButTwo
```

    ## 
    ## Call:  glm(formula = logShares ~ data_channel_is_socmed + kw_max_avg + 
    ##     self_reference_avg_sharess + kw_avg_avg + self_reference_max_shares, 
    ##     family = "binomial", data = data1Train)
    ## 
    ## Coefficients:
    ##                (Intercept)      data_channel_is_socmed  
    ##                 -1.186e+00                   1.113e+00  
    ##                 kw_max_avg  self_reference_avg_sharess  
    ##                 -5.373e-05                   9.100e-06  
    ##                 kw_avg_avg   self_reference_max_shares  
    ##                  4.668e-04                  -2.737e-07  
    ## 
    ## Degrees of Freedom: 4661 Total (i.e. Null);  4656 Residual
    ## Null Deviance:       6460 
    ## Residual Deviance: 6204  AIC: 6216

Remove `data_channel_is_socmed`.  
**glmAllButThree**

``` r
glmAllButThree <- glm(logShares ~ 
                  kw_max_avg + 
                  self_reference_avg_sharess +
                  kw_avg_avg +
                  self_reference_max_shares,
                data = data1Train, 
                family = "binomial"
)

glmAllButThree
```

    ## 
    ## Call:  glm(formula = logShares ~ kw_max_avg + self_reference_avg_sharess + 
    ##     kw_avg_avg + self_reference_max_shares, family = "binomial", 
    ##     data = data1Train)
    ## 
    ## Coefficients:
    ##                (Intercept)                  kw_max_avg  
    ##                 -1.140e+00                  -5.515e-05  
    ## self_reference_avg_sharess                  kw_avg_avg  
    ##                  9.951e-06                   4.699e-04  
    ##  self_reference_max_shares  
    ##                 -3.260e-07  
    ## 
    ## Degrees of Freedom: 4661 Total (i.e. Null);  4657 Residual
    ## Null Deviance:       6460 
    ## Residual Deviance: 6262  AIC: 6272

Remove `self_reference_max_shares`.  
**glmAllButFour**

``` r
glmAllButFour <- glm(logShares ~ 
                  kw_max_avg + 
                  self_reference_avg_sharess +
                  kw_avg_avg, 
                data = data1Train, 
                family = "binomial"
)
glmAllButFour
```

    ## 
    ## Call:  glm(formula = logShares ~ kw_max_avg + self_reference_avg_sharess + 
    ##     kw_avg_avg, family = "binomial", data = data1Train)
    ## 
    ## Coefficients:
    ##                (Intercept)                  kw_max_avg  
    ##                 -1.141e+00                  -5.527e-05  
    ## self_reference_avg_sharess                  kw_avg_avg  
    ##                  9.393e-06                   4.703e-04  
    ## 
    ## Degrees of Freedom: 4661 Total (i.e. Null);  4658 Residual
    ## Null Deviance:       6460 
    ## Residual Deviance: 6262  AIC: 6270

Did not help. Will keep `self_reference_max_shares`.

## Comparison of all Four Logistic Models

I will predict the test data and compare the RMSEs of those.

``` r
#Make predictions  
predALL <- predict(glmALL, newdata = data1Test, type = "link")
predALLbutOne <- predict(glmAllButOne, newdata = data1Test, type = "link")
predALLbutTwo <- predict(glmAllButTwo, newdata = data1Test, type = "link")
predALLbutThree <- predict(glmAllButThree, newdata = data1Test, type = "link")

#Calculate RMSE  
AllMSE <- rmse(data1Test$logShares, predALL)
OneMSE <- rmse(data1Test$logShares, predALLbutOne)
TwoMSE <- rmse(data1Test$logShares, predALLbutTwo)
ThreeMSE <- rmse(data1Test$logShares, predALLbutThree)

matMSE <- matrix(c(AllMSE, OneMSE, TwoMSE, ThreeMSE), nrow = 1, ncol = 4, byrow = TRUE)

matMSE
```

    ##           [,1]     [,2]    [,3]      [,4]
    ## [1,] 0.7923079 0.787188 0.78528 0.7626693

### Analysis

The glmAllButThree produces the smallest MSE. I will use this as my
model for the data.
