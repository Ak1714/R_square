
## <b>Project layout</b>

    mkdocs.yml    # The configuration file.
    docs/
        index.md  # The documentation homepage with project summary
		LCDA_Part1.md # Analyzing and vizualizing the data
		LCDA_Part2.md # Clustering & PCA
		LCDA_Part3.md # Building models

## <b>Data source</b>

	https://www.kaggle.com/wordsforthewise/lending-club

## <b>Summary</b>

 <a href="https://www.lendingclub.com/">Lending Club</a> is the world’s leading online marketplace for connecting borrowers and investors. 
 In this project the goal is to build a robust model using logistic regression and classification tree for predicting the final status of a loan based on variables available at the time when the loan is granted.  


## <b>Vetting data</b>

 
 Reading the lendingdataclub 2017 Q3 csv file and scanning the file for all columns by skipping the first line since it is not required.
 
	colNames <- scan("LoanStats2017Q3.csv", what="character", skip=1, nlines=1, sep=",")
 
 Specifying the character and factor columns for categorical data, since rest of it is mostly numerical and date data.
 
	characterColumns <- c("id", "member_id", "emp_title", "issue_d", "url",
                      "desc", "zip_code", "addr_state", "earliest_cr_line", "last_pymnt_d",
                      "next_pymnt_d", "last_credit_pull_d", "sec_app_earliest_cr_line",
                      "hardship_type", "hardship_reason", "hardship_start_date",
                      "hardship_end_date", "payment_plan_start_date",
                      "debt_settlement_flag_date", "settlement_date")
	factorColumns <- c("term", "grade", "sub_grade", "emp_length",
                   "home_ownership", "verification_status", "loan_status",
                   "pymnt_plan", "purpose", "title", "initial_list_status",
                   "policy_code", "application_type", "verification_status_joint",
                   "hardship_flag", "hardship_status", "hardship_loan_status",
                   "disbursement_method", "debt_settlement_flag", "settlement_status")
  
  Excluding the top two rows and any other row that is not part of the main data by specifying the exact number of rows (122701)
  
	lendingRows <- 122701
	lendingData <- read.table("LoanStats2017Q3.csv", skip=1, sep=",", nrows=lendingRows,
                          colClasses = myColClasses, header=TRUE)

  After reading the columns, two columns: revol_util  and int_rate have % signs. We don’t want R to interpret it as character strings. Hence, we can convert to character strings and getting rid of the ‘%’ and then back to numeric.

	lendingData$revol_util <- as.character(lendingData$revol_util)
	lendingData$revol_util <- sub("%", "", lendingData$revol_util)
	lendingData$revol_util <- as.numeric(lendingData$revol_util)
	lendingData$int_rate <- as.character(lendingData$int_rate)
	lendingData$int_rate <- sub("%", "", lendingData$int_rate)
	lendingData$int_rate <- as.numeric(lendingData$int_rate)
	
  Formatting the columns for date data and specifying the first of the month since the data only has month and year.
  
	dateColumns <- c("issue_d", "last_pymnt_d", "next_pymnt_d", "last_credit_pull_d",
                 "sec_app_earliest_cr_line", "hardship_start_date", "hardship_end_date",
                 "payment_plan_start_date", "debt_settlement_flag_date", "settlement_date",
                 "earliest_cr_line")
				 
  Using function strptime(x, format), where x is a character vector of dates and format is a character string of the dates, using percent symbols with characters to specify what types of date and time information.%d: Day of the month as decimal number (01--31)%b: Abbreviated month name in the current locale on this platform%Y: Year with century	 e.g.: 2015

	lendingData[,dateColumns] <- apply(lendingData[,dateColumns], 2, function(x) {
	strptime(paste("1", x), "%d %b-%Y")})

	lendingData[,dateColumns] <- apply(lendingData[,dateColumns], 2, function(x) {
	strptime(paste("1", x), "%d %b-%Y")})	
	
  Saving the file as R object for faster loading. Then counting the number of each loan status.
  
	save(lendingData, file="LoanStats2017Q3.rda")

	load("LoanStats2017Q3.rda")

	(table(lendingData$loan_status))

	# Charged Off            Current            Default         Fully Paid    In Grace Period 
	#    12876                65816                161              40471                752 
	# Late (16-30 days) Late (31-120 days) 
	#      503                2122 
	

