Exploratory analysis on LendingClub dataset
================
Akansha Chaudhari
09/12/2020

## R Markdown

``` r
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
load("LoanStats2017Q3.rda")

#Determining the mean and median of the borrowers' months since last public record (mths_since_last_record). Finding the number of observations for which this value is missing and creating a new variable where the value is imputed with the median.

mean(lendingData$mths_since_last_record)
```

    ## [1] NA

``` r
# Counting number of nulls = 103863
sum(is.na(lendingData$mths_since_last_record))
```

    ## [1] 103863

``` r
# Calculating mean without NULL values = 75.79589
mean(lendingData$mths_since_last_record, na.rm = TRUE)
```

    ## [1] 75.79589

``` r
# Calculating the median without null values = 79
median(lendingData$mths_since_last_record, na.rm = TRUE)
```

    ## [1] 79

``` r
# creating a new column for imputing median in place of NULL
medianCol <- c(lendingData$mths_since_last_record)

# Imputing median in the new column in place of NULLs
medianCol[is.na(medianCol)] <- median(medianCol, na.rm = TRUE)

# Counting NULLs after imputation = 0
sum(is.na(medianCol))
```

    ## [1] 0

``` r
# Calculating mean = 78.50808 & median = 79 for the new column
mean(medianCol)
```

    ## [1] 78.50808

``` r
median(medianCol)
```

    ## [1] 79

## Histograms and Boxplots

![](images/histogram-1.png)<!-- -->![](images/histogram-2.png)<!-- -->

``` r
# Creating a boxplot of interest rate (int_rate) for each loan status (loan_status) to find the status that has loans with the highest median interest rate  

boxplot(lendingData$int_rate ~ lendingData$loan_status, col = 6, labels = TRUE, las = 2, par(mar=c(8,5,2,2)))
```

![](images/unnamed-chunk-1-1.png)<!-- -->

``` r
# Verifying median values

lendingData %>%
              dplyr::select(loan_status, int_rate) %>%
              group_by(loan_status) %>%
              summarize(medianVal = median(int_rate, na.rm = TRUE)) %>%
              arrange(loan_status)
```

    ## # A tibble: 7 x 2
    ##   loan_status        medianVal
    ##   <fct>                  <dbl>
    ## 1 Charged Off             15.0
    ## 2 Current                 12.0
    ## 3 Default                 15.0
    ## 4 Fully Paid              12.6
    ## 5 In Grace Period         14.1
    ## 6 Late (16-30 days)       14.1
    ## 7 Late (31-120 days)      15.0

``` r
# Loan statuses: Charged Off, Default and Late (31-120 days) tend to have the highest median interest rates since these borrowers haven’t been able to pay their interests on time.
# The interest rates for Fully Paid loans are lower than those of Charged Off ones.
```

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
