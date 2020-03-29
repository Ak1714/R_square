# Decision Support Model

 This is my first project analyzing data using statistical methods/ tools. This is also my first gitpage ;)

## Tableau public dashboard

* https://public.tableau.com/profile/akansha2579#!/vizhome/Decision_support_viz/GraduateAdmissions


## Project layout

    mkdocs.yml    # The configuration file.
    docs/
        index.md  # The documentation homepage.

## Data source

https://www.kaggle.com/mohansacharya/graduate-admissions

## Summary

 This project analyzes the graduate admission process in American universities from international students’ perspectives. The goal was to build a decision support model that provides undergraduate international applicants with important information as well as the ability to make an informed decision during the application, with high confidence of being accepted. The analysis considers factors such as standardized test scores and GPA as well as university reputation.

## Data Analysis
 Step 1:
 
 We start by first studying the relationships between each parameter using a pairwise correlation matrix
 As per the correlation matrix below it is clear that there is high correlation between GRE score, TOEFL score and CGPA with Chance of admit. 
 It also shows that GRE score and TOEFL score have a high correlation (R=0.836), which can also mean that people who score more in GRE also score more in TOEFL. 
 Also, with R = 0.83 between CGPA and GRE score shows that students with good CGPA scores perform well in GRE. 
 

  <img src="https://github.com/Ak1714/SAS_models/blob/master/Decision_Support/image1.jpg?raw=true" width="700">

 Step 2:
 
 1) Regressing CGPA with Chance of admit
   Scatterplot looks homoscedastic.
  
   R-Square: 0.7626 | R-Square: 0.7626
  
  
  <img src="https://github.com/Ak1714/SAS_models/blob/master/Decision_Support/image2.jpg?raw=true" width="500">
  
  
  <img src="https://github.com/Ak1714/SAS_models/blob/master/Decision_Support/image3.jpg?raw=true" width="500">
  
   The Rsq value shows that the regression line is a good fit (i.e. 76% of the variability in the chance of admit is explained by our model with the help of CGPA alone). The RMSE is 0.0695 shows how spread out the residuals are.
 
 2)	Now, let’s analyze how GRE score alone helps predict w.r.t. Chance of admit but before that we need to check for homoskedasticity;
  
  <img src="https://github.com/Ak1714/SAS_models/blob/master/Decision_Support/image4.jpg?raw=true" width="500">
 
   Variance of residuals looks constant except for a few outliers. We can safely assume it is homoscedastic.
   R-Square: 0.6442 | R-Square: 0.6433
  
  <img src="https://github.com/Ak1714/SAS_models/blob/master/Decision_Support/image5.jpg?raw=true" width="500">
  
   Slope coefficient implies that for every 1-point increase in GRE score, there is approx. 0.01 increase in Chance of admit. (Very low)
  
 3)	Regressing TOEFL with Chance of admit:
   First verify whether it is homoscedastic; it does have constant variance
   R-Square: 0.6266 | R-Square: 0.6257
  
  <img src="https://github.com/Ak1714/SAS_models/blob/master/Decision_Support/image6.jpg?raw=true" width="500">
  
  
  <img src="https://github.com/Ak1714/SAS_models/blob/master/Decision_Support/image7.jpg?raw=true" width="500">
  
   Slope coefficient implies that with every 1-point increase in TOEFL score, there is approx. 0.02 increase in Chance of admit.
  
  <img src="https://github.com/Ak1714/SAS_models/blob/master/Decision_Support/image8.jpg?raw=true" width="500">
  
  4) Now, let’s add these 3 regressors to check whether our Rsquare value increases or decreases,
   The overall F-statistic is significant, indicating that the model accounts for a significant portion of the variability in the dependent variable. Also, individual t-statistic in ANOVA represent statistical significance of all the three regressors.

  <img src="https://github.com/Ak1714/SAS_models/blob/master/Decision_Support/image9.jpg?raw=true" width="500">
  
  <img src="https://github.com/Ak1714/SAS_models/blob/master/Decision_Support/image10.jpg?raw=true" width="500">
  
   R-Square: 0.7854 | R-Square: 0.7837
  
  5) Since we observed that GRE score/ TOEFL score are highly correlated with CGPA, let’s perform regression on each one of them to see if CGPA can predict GRE scores/TOEFL scores.
  
   R-Square: 0.6940 | R-Square: 0.6932
  
  <img src="https://github.com/Ak1714/SAS_models/blob/master/Decision_Support/image11.jpg?raw=true" width="500">
  
  <img src="https://github.com/Ak1714/SAS_models/blob/master/Decision_Support/image12.jpg?raw=true" width="500">
  
   Seems like our model helps predict GRE scores with CGPA alone. We don't have to worry about multicollinearity as our Rsquare has not very different from our previous model with 3 regressors (As a rule of thumb, if Rsquare is more than 88% that’s when multicollinearity would come into picture).
   
   Now, lets take a look at other models with similar Rsquare values,

  <img src="https://github.com/Ak1714/SAS_models/blob/master/Decision_Support/image113.jpg?raw=true" width="500">
  
   All of them have CGPA and GRE score in common. As stated earlier (through the correlation matrix) CGPA significantly influences chance of admit, its evident that the rest of the models increase the acceptance rate by a small proportion. 
  
   Let’s verify another model that has only CGPA in common,
   
   | Regressor 1 |	Regressor 2	 |	Regressor 3	|	R^sq   |  Adj. R^sq	| Root MSE |
   |-------------|---------------|--------------|----------|------------|----------|
   | LOR     	 |	CGPA    	 |	Research	|	0.7869 | 0.7853		| 0.066079 |
   

  
   After regressing LOR and Research with Chance of admit, we see that even though variation in LOR and Research explain only 50% of the variability in the model, we still see high Rsquare value.
					
  <img src="https://github.com/Ak1714/SAS_models/blob/master/Decision_Support/image14.jpg?raw=true" width="500">
  
  6) Final Model:
  
   After adding CGPA, Rsquare jumps by almost 44%. Which is the case with all other models. Since, GRE and TOEFL scores have a higher correlation to chance of admit than the rest of our parameters, we can select the below model,
  
   | Regressor 1 |	Regressor 2	 |	Regressor 3	|	R^sq   |  Adj. R^sq	| Root MSE |
   |-------------|---------------|--------------|----------|------------|----------|
   | GRE Score	 |	TOEFL Score	 |	CGPA		|	0.7854 | 0.7837		| 0.066321 |
  
   Residual plots:
  
  <img src="https://github.com/Ak1714/SAS_models/blob/master/Decision_Support/image15.jpg?raw=true" width="500">

   The residual plots don’t seem to have constant variance. However, our primary goal in this project is to total amount of the dependent variable rather than estimating specific effects of the independent variables, we chose to not correct heteroskedasticity.
  
   Chance of admit = -1.59 + 0.002 * GRE + 0.003 * TOEFL + 0.146 * CGPA
  
* Slope coefficient of GRE score: (can be interpreted as) a 1-point increase in GRE score corresponds to a 0.002 increase in Chance of admit
* Slope coefficient of TOEFL score: a 1-point increase in TOEFL score corresponds to a 0.003 increase in Chance of admit
* Slope coefficient of CGPA: a 1-point increase in CGPA corresponds to a 0.146 increase in Chance of admit
* Although GRE and TOEFL scores are statistically significant for our model, CGPA significantly influences chance of admit.

   As per the model, an applicant with;
*	GRE score: 312
*	TOEFL score: 107                  Chance of admit (actual)= 0.69 vs (predicted) = 0.66
*	Undergraduate CGPA: 8.27
   CGPA matters!
*	GRE score: 335
*	TOEFL score: 115 		          Chance of admit = 0.67                                    
*	Undergraduate CGPA: 7.8

## Insights:

*	Does undergraduate university ranking play any role in admission? 
    Yes, it does. However, it is not the most important factor. It can be helpful in determining the best of two very similar applicant profiles. Also, university rating shows decent correlation with chance of admit, however, after we perform regression, R^2 value is 0.51, i.e. only 50% of the variability can be explained by university rating.
*	What about SOP & LOR? 
    Although they are not directly linked to the acceptance rate, ensuring good written SOPs and LORs (getting them graded from your peers or professors) helps increase your chances of getting accepted.
*	All OLS (ordinary least squares) assumptions were verified in the above analysis
*	If the dataset also had university names the applicants were admitted to, we could have segregated the universities in to buckets based on their rankings. This would have helped us refine our model.
*	Treatment of outliers: Since our sample consisted of only 400 observations it didn’t make sense to eliminate the outliers.
*	Reason why 2 or 3 of the combinations that had either LOR, Research and SOP had greater R^2 and lower RMSE is because we didn’t scale or standardize the data. Clearly, since CGPA had a higher range it was more influenced by our analysis.
    CGPA = 10.00 - 6.80 = 32 points 
    GRE = 340 – 260 = 80 points
    TOEFL = 120 – 92 = 28 points



## SAS code

 Correlation matrix code:

	ods noproctitle; 
	ods graphics / imagemap=on; 
	 
	proc corr data=_TEMP0.GRAD pearson nosimple noprob plots=none; 
		var 'GRE Score'n 'TOEFL Score'n 'University Rating'n SOP LOR CGPA Research  
			'Chance of Admit'n; 
	run; 

[Click for results](https://github.com/Ak1714/SAS_models/tree/master/Decision_Support/Correlation_matrix-results.pdf)

 Linear regression using each of 3 regressors with Chance of admit:
 
	ods noproctitle; 
	ods graphics / imagemap=on; 
	 
	proc glmselect data=_TEMP0.GRAD outdesign(addinputvars)=Work.reg_design; 
		model 'Chance of Admit'n=CGPA / showpvalues selection=none; 
	run; 
	proc glmselect data=_TEMP0.GRAD outdesign(addinputvars)=Work.reg_design; 
		model 'Chance of Admit'n='GRE Score'n / showpvalues selection=none; 
	run; 
	proc glmselect data=_TEMP0.GRAD outdesign(addinputvars)=Work.reg_design; 
		model 'Chance of Admit'n='TOEFL Score'n / showpvalues selection=none; 
	run; 
	proc reg data=Work.reg_design alpha=0.05 plots(only)=(diagnostics residuals  
			observedbypredicted); 
		where 'GRE Score'n is not missing and 'TOEFL Score'n is not missing and CGPA  
			is not missing; 
		ods select DiagnosticsPanel ResidualPlot ObservedByPredicted; 
		model 'Chance of Admit'n=CGPA /; 
		model 'Chance of Admit'n='GRE Score'n /; 
		model 'Chance of Admit'n='TOEFL Score'n /; 
		run; 
	quit; 
	proc delete data=Work.reg_design; 
	run; 

[Click for results](https://github.com/Ak1714/SAS_models/tree/master/Decision_Support/LR-results.pdf)

 Multiple linear regression code:

	ods noproctitle; 
	ods graphics / imagemap=on; 
	 
	proc glmselect data=_TEMP0.GRAD outdesign(addinputvars)=Work.reg_design; 
		model 'Chance of Admit'n='GRE Score'n 'TOEFL Score'n CGPA / showpvalues  
			selection=none; 
	run; 
	proc reg data=Work.reg_design alpha=0.05 plots(only)=(diagnostics residuals  
			observedbypredicted); 
		where 'GRE Score'n is not missing and 'TOEFL Score'n is not missing and CGPA  
			is not missing; 
		ods select DiagnosticsPanel ResidualPlot ObservedByPredicted; 
		model 'Chance of Admit'n=&_GLSMOD /; 
		run; 
	quit; 
	proc delete data=Work.reg_design; 
	run; 

[Click for results](https://github.com/Ak1714/SAS_models/tree/master/Decision_Support/MLR-results.pdf)