ClusterAnalysis\_PCA
================
Akansha Chaudhari
9/20/2020

## R Markdown

``` r
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
library(dummies) # dummy variables
```

    ## dummies-1.5.6 provided by Decision Patterns

``` r
library(cluster) # Clustering
library(rgl) # 3d plot

load("LoanStats2017Q3.rda")

set.seed(12345)

# Storing variables in vectors
annualInc <- lendingData$annual_inc
loan_amnt <- lendingData$loan_amnt
empLength <- lendingData$emp_length
homeOwnership <- lendingData$home_ownership
dti <- lendingData$dti
grade <- lendingData$grade


# Removing the value 'n/a' from emp_length column and replacing it with NA
empLength[empLength == 'n/a'] <- NA
empLength <- addNA(empLength)

# Storing all variables in a data frame
tempDF <- data.frame(annualInc, loan_amnt, 
                     dti, homeOwnership, empLength)

# Checking for NULL values in the data frame and then imputing them with the median
apply(is.na(tempDF),2,sum)
```

    ##     annualInc     loan_amnt           dti homeOwnership     empLength 
    ##             0             0           184             0             0

``` r
tempDF$dti[is.na(tempDF$dti)] <-
  median(tempDF$dti, na.rm=TRUE)

head(tempDF)
```

    ##   annualInc loan_amnt   dti homeOwnership empLength
    ## 1     42000     12000 27.74           OWN 10+ years
    ## 2     79077     16000 15.94          RENT   5 years
    ## 3    107000     33000 19.06      MORTGAGE  < 1 year
    ## 4    155000     32000 12.35      MORTGAGE 10+ years
    ## 5    120000     40000 31.11      MORTGAGE   9 years
    ## 6     32000      7000 12.27          RENT 10+ years

``` r
# Creating a dummy data frame with categorical columns
lendDF <- dummy.data.frame(tempDF, names=c("homeOwnership", "empLength"))
```

    ## Warning in model.matrix.default(~x - 1, model.frame(~x - 1), contrasts = FALSE):
    ## non-list contrasts argument ignored
    
    ## Warning in model.matrix.default(~x - 1, model.frame(~x - 1), contrasts = FALSE):
    ## non-list contrasts argument ignored

``` r
head(lendDF)
```

    ##   annualInc loan_amnt   dti homeOwnershipANY homeOwnershipMORTGAGE
    ## 1     42000     12000 27.74                0                     0
    ## 2     79077     16000 15.94                0                     0
    ## 3    107000     33000 19.06                0                     1
    ## 4    155000     32000 12.35                0                     1
    ## 5    120000     40000 31.11                0                     1
    ## 6     32000      7000 12.27                0                     0
    ##   homeOwnershipNONE homeOwnershipOWN homeOwnershipRENT empLength< 1 year
    ## 1                 0                1                 0                 0
    ## 2                 0                0                 1                 0
    ## 3                 0                0                 0                 1
    ## 4                 0                0                 0                 0
    ## 5                 0                0                 0                 0
    ## 6                 0                0                 1                 0
    ##   empLength1 year empLength10+ years empLength2 years empLength3 years
    ## 1               0                  1                0                0
    ## 2               0                  0                0                0
    ## 3               0                  0                0                0
    ## 4               0                  1                0                0
    ## 5               0                  0                0                0
    ## 6               0                  1                0                0
    ##   empLength4 years empLength5 years empLength6 years empLength7 years
    ## 1                0                0                0                0
    ## 2                0                1                0                0
    ## 3                0                0                0                0
    ## 4                0                0                0                0
    ## 5                0                0                0                0
    ## 6                0                0                0                0
    ##   empLength8 years empLength9 years empLengthNA
    ## 1                0                0           0
    ## 2                0                0           0
    ## 3                0                0           0
    ## 4                0                0           0
    ## 5                0                1           0
    ## 6                0                0           0

``` r
# Scaling the values in the data frame for clustering
lendDF <- scale(lendDF, center=TRUE, scale=TRUE)
head(lendDF)
```

    ##       annualInc  loan_amnt         dti homeOwnershipANY homeOwnershipMORTGAGE
    ## 1 -0.1149444060 -0.2702472  0.39399388     -0.008074829            -0.9699266
    ## 2  0.0002058191  0.1458219 -0.18034054     -0.008074829            -0.9699266
    ## 3  0.0869264204  1.9141156 -0.02848262     -0.008074829             1.0309975
    ## 4  0.2360002614  1.8100983 -0.35507448     -0.008074829             1.0309975
    ## 5  0.1273005856  2.6422365  0.55801989     -0.008074829             1.0309975
    ## 6 -0.1460014562 -0.7903336 -0.35896827     -0.008074829            -0.9699266
    ##   homeOwnershipNONE homeOwnershipOWN homeOwnershipRENT empLength< 1 year
    ## 1      -0.002854802        2.7746053        -0.8168759         -0.363390
    ## 2      -0.002854802       -0.3604087         1.2241661         -0.363390
    ## 3      -0.002854802       -0.3604087        -0.8168759          2.751842
    ## 4      -0.002854802       -0.3604087        -0.8168759         -0.363390
    ## 5      -0.002854802       -0.3604087        -0.8168759         -0.363390
    ## 6      -0.002854802       -0.3604087         1.2241661         -0.363390
    ##   empLength1 year empLength10+ years empLength2 years empLength3 years
    ## 1      -0.2636353          1.4603381       -0.3126643       -0.2885988
    ## 2      -0.2636353         -0.6847673       -0.3126643       -0.2885988
    ## 3      -0.2636353         -0.6847673       -0.3126643       -0.2885988
    ## 4      -0.2636353          1.4603381       -0.3126643       -0.2885988
    ## 5      -0.2636353         -0.6847673       -0.3126643       -0.2885988
    ## 6      -0.2636353          1.4603381       -0.3126643       -0.2885988
    ##   empLength4 years empLength5 years empLength6 years empLength7 years
    ## 1        -0.256473        -0.250409       -0.2100718       -0.1956559
    ## 2        -0.256473         3.993435       -0.2100718       -0.1956559
    ## 3        -0.256473        -0.250409       -0.2100718       -0.1956559
    ## 4        -0.256473        -0.250409       -0.2100718       -0.1956559
    ## 5        -0.256473        -0.250409       -0.2100718       -0.1956559
    ## 6        -0.256473        -0.250409       -0.2100718       -0.1956559
    ##   empLength8 years empLength9 years empLengthNA
    ## 1         -0.16981       -0.1896957  -0.2755667
    ## 2         -0.16981       -0.1896957  -0.2755667
    ## 3         -0.16981       -0.1896957  -0.2755667
    ## 4         -0.16981       -0.1896957  -0.2755667
    ## 5         -0.16981        5.2715566  -0.2755667
    ## 6         -0.16981       -0.1896957  -0.2755667

``` r
# Performing Kmeans clustering
lendKmeans <- kmeans(lendDF, centers=7)

# Doing PCA analysis
lendPCA <- prcomp(lendDF, retx=TRUE)
plot(lendPCA$x[,1:2], col=lendKmeans$cluster, pch=lendKmeans$cluster)
legend("topright", title = ,legend = 1:7, col = 1:7, pch =  1:7)
```

![](PCA.png)<!-- -->

``` r
# Rotation matrix for first 2 PCs
lendPCA$rotation[,1:2]
```

    ##                                PC1          PC2
    ## annualInc              0.061042681 -0.016995412
    ## loan_amnt              0.269090193  0.179204905
    ## dti                    0.063038509  0.195250523
    ## homeOwnershipANY      -0.003757898  0.005487485
    ## homeOwnershipMORTGAGE  0.634478287  0.247075768
    ## homeOwnershipNONE     -0.001164800 -0.001968247
    ## homeOwnershipOWN      -0.046104282 -0.423947441
    ## homeOwnershipRENT     -0.617114633  0.023901256
    ## empLength< 1 year     -0.002269023  0.458873341
    ## empLength1 year       -0.130646715  0.040961336
    ## empLength10+ years     0.287971605 -0.647021527
    ## empLength2 years      -0.128632358  0.075200216
    ## empLength3 years      -0.098845568  0.076617349
    ## empLength4 years      -0.069703845  0.074853769
    ## empLength5 years      -0.044876646  0.086490324
    ## empLength6 years      -0.017936943  0.103554449
    ## empLength7 years      -0.011065154  0.087224445
    ## empLength8 years      -0.003133917  0.074989097
    ## empLength9 years       0.009005405  0.092448604
    ## empLengthNA           -0.025108522 -0.010037250

``` r
summary(lendPCA)
```

    ## Importance of components:
    ##                            PC1    PC2     PC3     PC4     PC5     PC6     PC7
    ## Standard deviation     1.40804 1.1428 1.12931 1.06944 1.04506 1.04341 1.03802
    ## Proportion of Variance 0.09913 0.0653 0.06377 0.05719 0.05461 0.05444 0.05387
    ## Cumulative Proportion  0.09913 0.1644 0.22820 0.28538 0.33999 0.39442 0.44830
    ##                            PC8     PC9    PC10    PC11    PC12    PC13    PC14
    ## Standard deviation     1.03267 1.03082 1.02184 1.01897 1.01567 1.01301 1.00245
    ## Proportion of Variance 0.05332 0.05313 0.05221 0.05192 0.05158 0.05131 0.05025
    ## Cumulative Proportion  0.50162 0.55475 0.60696 0.65887 0.71045 0.76176 0.81200
    ##                           PC15    PC16    PC17    PC18      PC19      PC20
    ## Standard deviation     0.99940 0.99819 0.96982 0.90785 1.056e-14 9.797e-15
    ## Proportion of Variance 0.04994 0.04982 0.04703 0.04121 0.000e+00 0.000e+00
    ## Cumulative Proportion  0.86194 0.91176 0.95879 1.00000 1.000e+00 1.000e+00

``` r
# • After inspecting the first two PCs. The variation on the first component is due to several related variables concerning the borrowers’ loan amount, homes that are mortgaged and borrowers employed for more than 10 years. Borrowers with larger values on the first component will have larger values for these variables.
# • The variation on the second component is due to the borrowers employed for less than a year and for 6 years, homes that are mortgaged, and debt-to-income ratio. Borrowers with larger values on the second component will have larger values for these variables.
# • The black cluster has negative values for PC1, indicating fewer rented homes and lesser number of borrowers employed between 1 to 4 years inclusive. The scores on PC2 are negative as well, indicating borrowers with lower annual income.
# • The pink cluster has positive values for PC1 than the black cluster, indicating a greater number of borrowers whose homes are mortgaged, have higher loan amounts and have been employed for more than 10 years. The scores on PC2 are near zero, indicating that the values are moderate compared to the rest of the data.
# • The yellow & cyan clusters overlap with the black cluster and the red & green clusters overlap with pink cluster. This indicates that the overlapping clusters have similar characteristics.
# • The blue cluster has the largest positive values for PC2 corresponding to larger number of borrowers with employment term < 1 year, higher mortgaged homes, higher debt-to-income ratio and higher annual income. 
# • In PC2, there is a trade-off between more borrowers with mortgaged homes and lesser owned homes. Another trade-off between more borrowers with lower employment length (< 1 year) and lower number of borrowers employed for 10+ years.
# • In PC1, there is a trade-off between mortgaged homes and lower number of borrowers with rented homes. Another trade-off between more borrowers employed (>10 years) and less borrowers employed from 1 to 4 years.
# • The outlier seems to affect strongly the first principal component.
```

``` r
# Getting rid of the outlier
#lendPCA$x[,1]
which(lendPCA$x[,1] > 20)
```

    ## 72164 
    ## 72164

``` r
lendPCA$x[72164, 1]
```

    ## [1] 20.42088

``` r
lendDF <- lendDF[-72164,]

# Running Kmeans again and replotting
lendKmeans <- kmeans(lendDF, centers=7)

lendPCA <- prcomp(lendDF, retx=TRUE)
plot(lendPCA$x[,1:2], col=lendKmeans$cluster, pch=lendKmeans$cluster)
legend("topright", title = ,legend = 1:7, col = 1:7, pch =  1:7)
```

![](PCA_wOutlier.png)<!-- -->

``` r
plot3d(lendPCA$x[,1:3], col=lendKmeans$cluster, pch=lendKmeans$cluster)
lendPCA$rotation[,1:3]
```

    ##                                PC1           PC2          PC3
    ## annualInc              0.031617716 -0.0008315871  0.028532744
    ## loan_amnt              0.267672066  0.1832264396  0.115086885
    ## dti                    0.064235286  0.1896985757 -0.249456961
    ## homeOwnershipANY      -0.003734964  0.0049164790 -0.033920042
    ## homeOwnershipMORTGAGE  0.635696955  0.2469606171  0.069240871
    ## homeOwnershipNONE     -0.001156300 -0.0023847493 -0.024727648
    ## homeOwnershipOWN      -0.045806272 -0.4334673269 -0.589004430
    ## homeOwnershipRENT     -0.618552181  0.0302284478  0.313543643
    ## empLength< 1 year     -0.001797284  0.4576296418 -0.058412021
    ## empLength1 year       -0.130963768  0.0422715290  0.055130890
    ## empLength10+ years     0.287725982 -0.6416129898  0.363060034
    ## empLength2 years      -0.128941170  0.0763491979  0.039146531
    ## empLength3 years      -0.099034453  0.0770658747  0.008655596
    ## empLength4 years      -0.069874577  0.0751682623 -0.003951253
    ## empLength5 years      -0.044999307  0.0863378060 -0.037230429
    ## empLength6 years      -0.018064151  0.1038335394 -0.007034248
    ## empLength7 years      -0.011167098  0.0871645368 -0.027346571
    ## empLength8 years      -0.003164955  0.0746941318 -0.032580385
    ## empLength9 years       0.008960206  0.0919692402 -0.052628954
    ## empLengthNA           -0.023907131 -0.0211256891 -0.570980121

``` r
# Below is the PCA rotation matrix for first 3 PCs. PC3 is associated with borrowers’ loan amount, rented homes and employment length more than 10 years.
# The positive variation in the third PC has to do with borrowers with higher rented homes, loan amount and number of borrowers with over 10years of employment. 
# The negative variation in the third PC has to do with lower debt-to-income ratio and lesser number of borrowers with own homes.
```
