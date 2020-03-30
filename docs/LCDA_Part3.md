<b>Building a robust model using Logistic Regression & Classification trees</b>

## <b>Importing Packages</b>

``` r
library(dplyr)
library(rpart) # Classification tree
library(caret) # classification & regression
library(ROCR) # ROC curve

load("LoanStats2017Q3.rda")
```

Changing format of all date columns in the original data set and then creating a data frame ‘myLDF’ to store the dependent variable and the independent variables considered during loan approvals.
Dependent Variable: loan_status

```{r}
# Specifying date columns
dateColumns <- c("issue_d", "last_pymnt_d", "next_pymnt_d", "last_credit_pull_d", "sec_app_earliest_cr_line", 
       "hardship_end_date", "hardship_start_date", "payment_plan_start_date", "debt_settlement_flag_date", 
       "settlement_date", "earliest_cr_line")

# Formating the date columns
lendingData[,dateColumns] <- lapply(lendingData[,dateColumns], as.POSIXct)

myLDF <- lendingData %>%
  dplyr::select("loan_status", "issue_d", "last_pymnt_d", "last_credit_pull_d", "sec_app_earliest_cr_line", 
                "hardship_end_date", "hardship_start_date", "payment_plan_start_date", "debt_settlement_flag_date", 
                "settlement_date", "annual_inc", "emp_length", "mths_since_recent_inq","num_op_rev_tl", "pub_rec", 
                "fico_range_high", "fico_range_low", "num_sats", "open_acc", "pub_rec_bankruptcies", "pub_rec", "purpose") %>%
  dplyr:: filter(loan_status %in% c("Fully Paid", "Charged Off")) %>%
  droplevels()
```

Splitting the data frame “myLDF” into two sets: Training & test, where training set will have 20% of the rows and test set will have 80% of the rows for analysis:

``` r
trainRows <- createDataPartition(myLDF$loan_status, 
                                  p = 0.2, 
                                  list=FALSE)

myLDFtrain <- myLDF[trainRows,]
myLDFtest <- myLDF[-trainRows,]
```

Before moving on, we need to check for NULLs. All the columns with NULL values have to be ignored.

``` r
# Looking for NULLs in training dataset
apply(is.na(myLDFtrain),2,sum)

# Imputing NULLs with the median in the training dataset
myLDFtrain$mths_since_recent_inq[is.na(myLDFtrain$mths_since_recent_inq)] <-
  median(myLDFtrain$mths_since_recent_inq, na.rm=TRUE)
myLDFtrain$sec_app_earliest_cr_line[is.na(myLDFtrain$sec_app_earliest_cr_line)] <-
  median(myLDFtrain$sec_app_earliest_cr_line, na.rm=TRUE)
myLDFtrain$hardship_end_date[is.na(myLDFtrain$hardship_end_date)] <-
  median(myLDFtrain$hardship_end_date, na.rm=TRUE)
myLDFtrain$hardship_start_date[is.na(myLDFtrain$hardship_start_date)] <-
  median(myLDFtrain$hardship_start_date, na.rm=TRUE)
myLDFtrain$payment_plan_start_date[is.na(myLDFtrain$payment_plan_start_date)] <-
  median(myLDFtrain$payment_plan_start_date, na.rm=TRUE)
myLDFtrain$debt_settlement_flag_date[is.na(myLDFtrain$debt_settlement_flag_date)] <-
  median(myLDFtrain$debt_settlement_flag_date, na.rm=TRUE)
myLDFtrain$last_pymnt_d[is.na(myLDFtrain$last_pymnt_d)] <-
  median(myLDFtrain$last_pymnt_d, na.rm=TRUE)
myLDFtrain$settlement_date[is.na(myLDFtrain$settlement_date)] <-
  median(myLDFtrain$settlement_date, na.rm=TRUE)
```

	##               loan_status                   issue_d              last_pymnt_d 
	##                         0                     10671                     10671 
	##        last_credit_pull_d  sec_app_earliest_cr_line         hardship_end_date 
	##                     10671                     10671                     10671 
	##       hardship_start_date   payment_plan_start_date debt_settlement_flag_date 
	##                     10671                     10671                     10671 
	##           settlement_date                annual_inc                emp_length 
	##                     10671                         0                         0 
	##     mths_since_recent_inq             num_op_rev_tl                   pub_rec 
	##                      1006                         0                         0 
	##           fico_range_high            fico_range_low                  num_sats 
	##                         0                         0                         0 
	##                  open_acc      pub_rec_bankruptcies                   purpose 
	##                         0                         0                         0

``` r
# Imputing NULLs with the median of the training set in the test dataset
myLDFtest$mths_since_recent_inq[is.na(myLDFtest$mths_since_recent_inq)] <-
  median(myLDFtrain$mths_since_recent_inq, na.rm=TRUE)
myLDFtest$sec_app_earliest_cr_line[is.na(myLDFtest$sec_app_earliest_cr_line)] <-
  median(myLDFtrain$sec_app_earliest_cr_line, na.rm=TRUE)
myLDFtest$hardship_end_date[is.na(myLDFtest$hardship_end_date)] <-
  median(myLDFtrain$hardship_end_date, na.rm=TRUE)
myLDFtest$hardship_start_date[is.na(myLDFtest$hardship_start_date)] <-
  median(myLDFtrain$hardship_start_date, na.rm=TRUE)
myLDFtest$payment_plan_start_date[is.na(myLDFtest$payment_plan_start_date)] <-
  median(myLDFtrain$payment_plan_start_date, na.rm=TRUE)
myLDFtest$debt_settlement_flag_date[is.na(myLDFtest$debt_settlement_flag_date)] <-
  median(myLDFtrain$debt_settlement_flag_date, na.rm=TRUE)
myLDFtest$last_pymnt_d[is.na(myLDFtest$last_pymnt_d)] <-
  median(myLDFtrain$last_pymnt_d, na.rm=TRUE)
myLDFtest$settlement_date[is.na(myLDFtest$settlement_date)] <-
  median(myLDFtrain$settlement_date, na.rm=TRUE)
```

Since, we need to find the final loan status and for logistic regression we need two significant output values: “Fully Paid” & “Charged Off”. Then we can check for the total number of “Fully Charged” & “Fully Paid” and assigning weights accordingly;

``` r
sum(myLDFtrain$loan_status == "Fully Paid")
sum(myLDFtrain$loan_status == "Charged Off")
	## 8095
sum(myLDFtrain$loan_status == "Charged Off")
	## 2576

# We can now assign weights by making it thrice as expensive to misclassify
myLendWeights <- numeric(nrow(myLDFtrain))
myLendWeights[myLDFtrain$loan_stat == "Fully Paid"] <- 1
myLendWeights[myLDFtrain$loan_stat == "Charged Off"] <- 3
```

## <b>Model-1 Logistic Regression</b>

Running logistic regression using glm function on the training set and summarizing the output. Here we can consider the default significance level to be 0.05. The following variables don’t seem to be significant since their p values are less than 0.05.

 • sec_app_earliest_cr_line, 
 • hardship_end_date, emp_length, 
 • num_op_rev_tl, pub_rec, 
 • fico_range_high, 
 • fico_range_low, 
 • num_sats, 
 • open_acc,                  
 • pub_rec_bankruptcies           

``` r
myLendLR <- glm(loan_status ~ ., data=myLDFtrain, weights=myLendWeights,
                 family=binomial("logit"))

summary(myLendLR)
```

Now can predict the outcome (using predict function) using the test set where we obtain probabilities and store them in ‘myLendpredict’. For calculating the confusion matrix, we need to divide the list of predicted probabilities in ‘myLendpredict’ into 2 groups: “< 0.5 as Charged Off & >= 0.5 as Fully Paid”.

```r
# all probabilities as logistic regression
myLendPredict <- predict(myLendLR, 
                          newdata=myLDFtest, 
                          type="response")

# Classification matrix for logistic regression

myLendPredictLR <- character(length(myLendPredict))
myLendPredictLR[myLendPredict < 0.5] <- "Charged Off"
myLendPredictLR[myLendPredict >= 0.5] <- "Fully Paid"
length(myLendPredict)
myLendLRCM <- table(myLDFtest$loan_status, myLendPredictLR) # confusion matrix from test data

# Classification rate for logistic regresssion
1-sum(diag(myLendLRCM))/sum(myLendLRCM)

exp(coef(myLendLR))
```
	myLendPredictLR
                           Charged Off      Fully Paid
			Charged Off       6205              4095
			Fully Paid        12331            20045
			
	Misclassification rate: 0.3849

## <b>Model-2 Classification trees</b>

For classification tree, we can create a data frame ‘lendrpart’ and use function ‘rpart’ and plotting the tree.

``` r
set.seed(123)

lendrpart <- rpart(loan_status ~ ., data=myLDFtrain, weights = myLendWeights)
head(lendrpart)

lendrpart$variable.importance

#Plot the model
plot(lendrpart, uniform=TRUE,margin=0.01, asp = 0)
text(lendrpart, cex=.6)
```

<img src="https://github.com/Ak1714/R_square/blob/master/rpart1.jpg?raw=true" width="500">

``` r
# Classification matrix for rpart
myPredictrpart <- predict(lendrpart, 
                           newdata=myLDFtest, 
                           type="class")

myLendrpartCM <- table(myLDFtest$loan_status, myPredictrpart)
myLendrpartCM
1-sum(diag(myLendrpartCM))/sum(myLendrpartCM)
```

	myPredictrpart
							   Charged Off      Fully Paid
				Charged Off       6308                  3992
				Fully Paid        7856                  24520

	Misclassification rate: 0.2776


ROC curves for logistic regression & classification trees

The following variables seem to be more important in predicting the outcome using logistic regression since their p-values are less than 0.05;

 • issue_d, 

 • last_pymnt_d, 

 • last_credit_pull_d, 

 • hardship_start_date, 

 • payment_plan_start_date, 

 • debt_settlement_flag_date, 

 • settlement_date, annual_inc, 

 • mths_since_recent_inq,

 • purpose

	last_pymnt_d        last_credit_pull_d           fico_range_high            fico_range_low           settlement_date 
	763.0743221             219.6862028               188.5453735                 188.5453735              170.6381274 
	debt_settlement_flag_date                    
    146.6735896                 

``` r
# ROC Curve Model 1
myLRPredict <- predict(myLendLR, myLDFtest, type="response")
myLRPred <- prediction(myLRPredict, 
                        myLDFtest$loan_status, 
                        label.ordering=c("Charged Off", "Fully Paid"))
myLRPerf <- performance(myLRPred, "tpr", "fpr")

# ROC curve Model 2
myrpartPredict  <- predict(lendrpart, myLDFtest, type="prob")

myrpartPredict

# Only need positive for the plot hence '2'
myrpartPred <- prediction(myrpartPredict[,2], 
                           myLDFtest$loan_status,
                           label.ordering=c("Charged Off", "Fully Paid"))
myrpartPerf <- performance(myrpartPred, "tpr", "fpr")
```

Models with additional variables:

Let's try building a logistic regression model and a classification tree model for predicting the final status of a loan based on the following variables using the same training and testing observation:

 • loan_amnt, funded_amnt_inv, term, int_rate, installment, grade, emp_length, home_ownership, annual_inc, verification_status, loan_status, purpose, title, dti, total_pymnt, delinq_2yrs, open_acc, pub_rec, last_pymnt_d, last_pymnt_amnt, application_type, revol_bal, revol_util, recoveries.

The ‘home_ownership’ is a categorical variable with ‘NA’ values. Before proceeding, we need to filter out only the important values: ‘Mortgage’, ‘Own’ & ‘Rent’.

``` r
# filtering only 3 values from the column
lendingData <- lendingData %>%
  dplyr::filter(home_ownership %in% c("OWN", "RENT", "MORTGAGE")) %>%
  droplevels()

#levels(lendingData) <- c("MORTGAGE", "MORTGAGE", "MORTGAGE", "OWN", "RENT")

myL2DF <- lendingData %>%
  dplyr::select("loan_status", "annual_inc", "loan_amnt","term", "funded_amnt_inv", "int_rate", "installment", "grade",               "verification_status", "title", 
                "dti","total_pymnt", "delinq_2yrs", "last_pymnt_d", "last_pymnt_amnt", "application_type","revol_bal", "revol_util",
                "emp_length", "pub_rec", "home_ownership", "recoveries", "open_acc", "pub_rec", "purpose", "title", "verification_status") %>%
  dplyr:: filter(loan_status %in% c("Fully Paid", "Charged Off")) %>%
  droplevels()

table(lendingData$home_ownership)

apply(is.na(myL2DF),2,sum)
```

Dividing the data frame “myL2DF” into two sets: Training & test, where training set will have 20% of the rows and test set will have 80% of the rows for analysis. Before moving on, we need to check for NULLs. All the columns with NULL values have to be ignored.

``` r
trainRows2 <- createDataPartition(myL2DF$loan_status, 
                                  p = 0.2, 
                                  list=FALSE)

trainRows2
myL2DFtrain <- myL2DF[trainRows2,]
myL2DFtest <- myL2DF[-trainRows2,]

# Imputing median in place of NULLs
myL2DFtrain$last_pymnt_d[is.na(myL2DFtrain$last_pymnt_d)] <-
  median(myL2DFtrain$last_pymnt_d, na.rm=TRUE)
myL2DFtrain$revol_util[is.na(myL2DFtrain$revol_util)] <-
  median(myL2DFtrain$revol_util, na.rm=TRUE)
myL2DFtrain$dti[is.na(myL2DFtrain$dti)] <-
  median(myL2DFtrain$dti, na.rm=TRUE)

myL2DFtest$last_pymnt_d[is.na(myL2DFtest$last_pymnt_d)] <-
  median(myL2DFtrain$last_pymnt_d, na.rm=TRUE)
myL2DFtest$revol_util[is.na(myL2DFtest$revol_util)] <-
  median(myL2DFtrain$revol_util, na.rm=TRUE)
myL2DFtest$dti[is.na(myL2DFtest$dti)] <-
  median(myL2DFtrain$dti, na.rm=TRUE)
```

Since, we need to find the final loan status and for logistic regression we need two significant output values: “Fully Paid” & “Charged Off”. Then we can check for the total number of “Fully Charged” & “Fully Paid” and assigning weights accordingly;

``` r
sum(myL2DFtrain$loan_status == "Fully Paid")
	## 8094
sum(myL2DFtrain$loan_status == "Charged Off")
	## 2575

sum(myL2DFtrain$loan_status == "Fully Paid")
sum(myL2DFtrain$loan_status == "Charged Off")

myLendWeights2 <- numeric(nrow(myL2DFtrain))
myLendWeights2[myL2DFtrain$loan_status == "Fully Paid"] <- 1
myLendWeights2[myL2DFtrain$loan_status == "Charged Off"] <- 3
```

## <b>Model-3 Logistic Regression</b>

Running logistic regression using ‘glm’ function on the training set and summarizing the output. Then using the predict function on the test set for classifications based on the responses. Splitting these classes into two groups for the confusion matrix: “< 0.5 as Charged Off & >= 0.5 as Fully Paid”.

``` r

myLendLR2 <- glm(loan_status ~ ., data=myL2DFtrain, weights=myLendWeights2,
                 family=binomial("logit"))

summary(myLendLR2)
# all probabilities as logistic regression
myLendPredict2 <- predict(myLendLR2, 
                          newdata=myL2DFtest, 
                          type="response")

summary(myLendPredict2)

# confusion matrix from test data
myLendPredictLR2 <- character(length(myLendPredict2))
myLendPredictLR2[myLendPredict2 < 0.5] <- "Charged Off"
myLendPredictLR2[myLendPredict2 >= 0.5] <- "Fully Paid"

myLendLR2CM <- table(myL2DFtest$loan_status, myLendPredictLR2) 
myLendLR2CM
1-sum(diag(myLendLR2CM))/sum(myLendLR2CM)

summary(myLendLR2)
exp(coef(myLendLR2))

	myLendPredictLR2
								 Charged Off     Fully Paid
				Charged Off       10238                62
				Fully Paid        16                 32357
				
	Misclassification rate: 0.001827854

```

## <b>Model-4 Classification trees</b>

``` r
lendrpart2 <- rpart(loan_status ~ ., data=myL2DFtrain, weights = myLendWeights2)
head(lendrpart2)
#Plot the model
plot(lendrpart2, uniform=TRUE,margin=0.01, asp = 0)
text(lendrpart2, cex=.6)
```

<img src="https://github.com/Ak1714/R_square/blob/master/rpart2.jpg?raw=true" width="500">

``` r
# Confusion matrix
myPredictrpart2 <- predict(lendrpart2, 
                           newdata=myL2DFtest, 
                           type="class")
myPredictrpart2
myLendrpart2CM <- table(myL2DFtest$loan_status, myPredictrpart2)
myLendrpart2CM
1-sum(diag(myLendrpart2CM))/sum(myLendrpart2CM) # misclassification rate

	myPredictrpart2
							 Charged Off      Fully Paid
			Charged Off        	9874              426
			Fully Paid          1605             30768
	  
	Misclassification rate: 0.0475945


# ROC curve for logistic
myLRPredict2 <- predict(myLendLR2, myL2DFtest, type="response")
myLRPred2 <- prediction(myLRPredict2, 
                        myL2DFtest$loan_status, 
                        label.ordering=c("Charged Off", "Fully Paid"))
myLRPerf2 <- performance(myLRPred2, "tpr", "fpr")

# ROC curve for classification
myrpartPredict2  <- predict(lendrpart2, myL2DFtest, type="prob")

# Only need positive for the plot hence '2'
myrpartPred2 <- prediction(myrpartPredict2[,2], 
                           myL2DFtest$loan_status,
                           label.ordering=c("Charged Off", "Fully Paid"))
myrpartPerf2 <- performance(myrpartPred2, "tpr", "fpr")
```

## <b>ROC curves</b>

``` r
plot(myrpartPerf2, col=18)
plot(myLRPerf2, col=19, add=TRUE)
plot(myrpartPerf, col=20, add=TRUE)
plot(myLRPerf, col=21, add=TRUE)

legend(0.5, 0.6, c("Log. Reg.1", "Class. Tree1", "Log. Reg.2", "Class. Tree2"), col=c(21, 20, 19,18), lwd=3)
```

<img src="https://github.com/Ak1714/R_square/blob/master/roc.jpg?raw=true" width="500">

## <b>Area Under Curve</b>

``` r
performance(myLRPred, "auc")
performance(myrpartPred, "auc")
performance(myLRPred2, "auc")
performance(myrpartPred2, "auc")
```

Area under the ROC curve for model 1(Logistic regression): <font color="blue">0.6486718</font>

Area under the ROC curve for model 2(Classification): <font color="blue">0.7584767</font>

The curve for classification tree (model 2) is slightly higher than model 1, indicating a better performance across the different choices for false positive rate. At a false positive rate of 0.4 (so the 
true negative rate is 0.6), the true positive rate is about 0.8.

Area under the ROC curve for model 3(Logistic regression): <font color="blue">0.9987902</font>

Area under the ROC curve for model 4(Classification): <font color="blue">0.9890251</font>

The curve for logistic regression (model 3) is slightly higher than model 4 and much higher than models 1 & 2, indicating good performance across the different choices for false positive rate. 
At a false positive rate of 0 (so the true negative rate is 1), the true positive rate is 1.

It might seem that model 3 is the best model since it has the highest area under the curve and the lowest misclassification rate. 
However, using this loan performance data (that consists of variables considered during loan approval) will result in our model performing better with data than in actual practice because these variables are not known at the time of investment.
The differences in performance between the models in Q #2 and Q #3 can be attributed to <b>data leakage</b>. It happens when information from outside the training dataset is used to create the model. Data leakage can lead to the creation overly optimistic predictive models. Leakage can be detected if we get performance that seems a little too good to be true like in model 3. 

<b>The best model in my opinion is model 2 (Classification tree - dark blue line). It has AUC: 0.7584767.</b>


