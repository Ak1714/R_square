<b>Exploratory analysis and visualizing to get a feel of the distribution patterns </b>

## Importing packages

``` r
library(ggplot2)
library(dplyr)
```

## Exploratory analysis

Determining the mean and median of the borrowers' months since last public record (mths_since_last_record). Finding the number of observations for which this value is missing and creating a new variable where the value is imputed with the median.

``` r
load("LoanStats2017Q3.rda")
mean(lendingData$mths_since_last_record)
```


    ## [1] NA

Counting number of nulls:

``` r
sum(is.na(lendingData$mths_since_last_record))
```

    ## [1] 103863


Calculating mean without NULL values:
``` r
mean(lendingData$mths_since_last_record, na.rm = TRUE)
```

    ## [1] 75.79589

Calculating the median without null values:
``` r
median(lendingData$mths_since_last_record, na.rm = TRUE)
```

    ## [1] 79

Creating a new column for imputing median in place of NULL. Then imputing median in the new column in place of NULLs:
``` r
medianCol <- c(lendingData$mths_since_last_record)

medianCol[is.na(medianCol)] <- median(medianCol, na.rm = TRUE)

sum(is.na(medianCol))
```

    ## [1] 0


Calculating mean & median for the new column:
``` r
mean(medianCol)
```

    ## [1] 78.50808

``` r
median(medianCol)
```

    ## [1] 79

## Histogram

We plot a histogram for the settlement amount for loans using the function hist();

``` r
hist(lendingData$settlement_amount)
```

<img src="https://github.com/Ak1714/R_square/blob/master/histogram-1.png?raw=true" width="500">

The above histogram looks to be right skewed. Also, the column has a large range between $331.44 to $24102, it makes sense to do a log transformation to make it closer to a normal distribution. The below histogram appears to be close to a normal distribution.

``` r
logSettleamt <- log(lendingData$settlement_amount)
hist(logSettleamt, labels=TRUE)
```
<img src="https://github.com/Ak1714/R_square/blob/master/histogram-2.png?raw=true" width="500">

## Boxplot

Creating a boxplot of interest rate (int_rate) for each loan status (loan_status) to find the status that has loans with the highest median interest rate:

``` r
boxplot(lendingData$int_rate ~ lendingData$loan_status, col = 6, labels = TRUE, las = 2, par(mar=c(8,5,2,2)))
```

<img src="https://github.com/Ak1714/R_square/blob/master/unnamed-chunk-1-1.png?raw=true" width="500">

The median values for all loan statuses are summarized below using dplyr and group_by:
``` r
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


From the boxplot, loan statuses: Charged Off, Default and Late (31-120 days) tend to have the highest median interest rates since these borrowers haven’t been able to pay their interests on time. The interest rates for Fully Paid loans are lower than those of Charged Off ones.
