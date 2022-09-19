# Workshop 3 - Questions

``` r
#load the data from lending club
lendingclub <- read_csv((here::here("lending_club_raw_data.csv")))
lendingclub_clean <- lendingclub[,c(1:19)]
head(lendingclub_clean,10)
```

    ## # A tibble: 10 x 19
    ##    int_rate loan_amnt `term (months)` installment   dti delinq_2yrs annual_inc
    ##       <dbl>     <dbl>           <dbl>       <dbl> <dbl>       <dbl>      <dbl>
    ##  1     0.11      5000              36       163.  27.6            0      24000
    ##  2     0.15      2500              60        59.8  1              0      30000
    ##  3     0.16      2400              36        84.3  8.72           0      12252
    ##  4     0.13     10000              36       339.  20              0      49200
    ##  5     0.13      3000              60        67.8 17.9            0      80000
    ##  6     0.08      5000              36       156.  11.2            0      36000
    ##  7     0.16      7000              60       170.  23.5            0      47004
    ##  8     0.19      3000              36       109.   5.35           0      48000
    ##  9     0.21      5600              60       152.   5.55           0      40000
    ## 10     0.13      5375              60       121.  18.1            0      15000
    ## # ... with 12 more variables: grade <chr>, emp_title <chr>, emp_length <chr>,
    ## #   home_ownership <chr>, verification_status <chr>, issue_d <chr>,
    ## #   zip_code <chr>, addr_state <chr>, loan_status <chr>, desc <chr>,
    ## #   purpose <chr>, title <chr>

## 1. Let’s go to the full dataset. Create a \[…\]

``` r
#create the column in which we will store the binary values of the loan status
loan_stat_binary <- lendingclub_clean$loan_status
lendingclub_clean <- cbind(lendingclub_clean,loan_stat_binary)

#create the binary values
lendingclub_clean <- lendingclub_clean %>%
      mutate(loan_stat_binary = ifelse(loan_stat_binary == "Charged Off",1,0))
colnames(lendingclub_clean)
```

    ##  [1] "int_rate"            "loan_amnt"           "term (months)"      
    ##  [4] "installment"         "dti"                 "delinq_2yrs"        
    ##  [7] "annual_inc"          "grade"               "emp_title"          
    ## [10] "emp_length"          "home_ownership"      "verification_status"
    ## [13] "issue_d"             "zip_code"            "addr_state"         
    ## [16] "loan_status"         "desc"                "purpose"            
    ## [19] "title"               "loan_stat_binary"

## 2. Investigate, how often do loans default? Create a correlation \[…\]

``` r
#select the quantitative variables and create a correlation plot
lendingclub_quant <- lendingclub_clean[c(1:2,4:5,7,20)]
ggpairs(lendingclub_quant)
```


[<img src="content/crop_hp_800px.png" style="max-width:20%;min-width:40px;float:right;" alt="Github repo" />](https://github.com/yihui/hugo-xmin)

!(crop_hp_800px.png)


\## 3. Create a smaller dataset (10,000 observations) to investigate
\[…\]

``` r
#create a sample from the data of 10,000 observations
lendingclub_sample <- sample(lendingclub_clean,10000)

#create a logistic regression with the two required variables
logistic_reg <- glm(loan_stat_binary ~
                    loan_amnt + dti,
                    family = binomial,
                    data = lendingclub_sample)
summary(logistic_reg)
```

    ## 
    ## Call:
    ## glm(formula = loan_stat_binary ~ loan_amnt + dti, family = binomial, 
    ##     data = lendingclub_sample)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.8145  -0.5785  -0.5323  -0.4868   2.1815  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.343e+00  8.265e-02 -28.353  < 2e-16 ***
    ## loan_amnt    2.570e-05  3.762e-06   6.831 8.41e-12 ***
    ## dti          1.872e-02  4.546e-03   4.118 3.82e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 7358.8  on 8932  degrees of freedom
    ## Residual deviance: 7293.5  on 8930  degrees of freedom
    ##   (1067 observations deleted due to missingness)
    ## AIC: 7299.5
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
#use our model to predict the data using our current data set and store it in a df
pred_log_reg <- predict(logistic_reg, newdata = lendingclub_sample, type = "response")
pred_log_reg_df <- data.frame(pred_log_reg)

#isolate the binary variables to create the upcoming ROC curves
sample_binary_loan_stat <- lendingclub_sample["loan_stat_binary"]

#create a ROC curve to assess the model's effectiveness
ROC_logreg <- cbind(sample_binary_loan_stat,pred_log_reg_df)
ROC_logreg_clean <- na.omit(ROC_logreg)
PRROC_obj <- roc.curve(scores.class0 = ROC_logreg_clean$pred_log_reg, weights.class0=ROC_logreg_clean$loan_stat_binary,
                       curve=TRUE)
plot(PRROC_obj)
```

![](Hadrien_Pistre_Assessment_2_R_Code_bis_files/figure-markdown_github/first%20logistic%20reg-1.png)

``` r
#create a confusion matrix with a 0.5 cut-off
pred_log_reg_cut_off_50 <- ifelse(pred_log_reg_df > 0.5, 1, 0)
pred_log_reg_cut_off_50_df <- data.frame(pred_log_reg_cut_off_50)

table(lendingclub_sample$loan_stat_binary, pred_log_reg_cut_off_50)
```

    ##    pred_log_reg_cut_off_50
    ##        0
    ##   0 7648
    ##   1 1285

``` r
#create a confusion matrix with a 0.25 cut-off
pred_log_reg_cut_off_25 <- ifelse(pred_log_reg_df > 0.25, 1, 0)
pred_log_reg_cut_off_25_df <- data.frame(pred_log_reg_cut_off_25)

table(lendingclub_sample$loan_stat_binary, pred_log_reg_cut_off_25)
```

    ##    pred_log_reg_cut_off_25
    ##        0    1
    ##   0 7624   24
    ##   1 1274   11

``` r
#create a confusion matrix with a 0.15 cut-off
pred_log_reg_cut_off_15 <- ifelse(pred_log_reg_df > 0.15, 1, 0)
pred_log_reg_cut_off_15_df <- data.frame(pred_log_reg_cut_off_15)

table(lendingclub_sample$loan_stat_binary, pred_log_reg_cut_off_15)
```

    ##    pred_log_reg_cut_off_15
    ##        0    1
    ##   0 5139 2509
    ##   1  734  551

## 5. Let’s try to improve the model’s accuracy by adding more variables, including \[…\]

``` r
#reduce the number of categories for several variables
lendingclub_sample$emp_length <- recode(lendingclub_sample$emp_length, '< 1 year' = '< 2 years', '1 year' = '< 2 years', '2 years' = '2-5 years', '3 years' = '2-5 years', '4 years' = '2-5 years', '5 years' = '2-5 years', '6 years' = '> 5 years','7 years' = '> 5 years','8 years' = '> 5 years','9 years' = '> 5 years','10+ years' = '> 5 years')

lendingclub_sample$delinq_2yrs <- recode(lendingclub_sample$delinq_2yrs, '0' = '< 2', '1' = '< 2', '2' = '2-5', '3' = '2-5', '4' = '2-5', '5' = '2-5', '6' = '> 5','7' = '> 5','8' = '> 5','9' = '> 5')

lendingclub_sample$verification_status <- recode(lendingclub_sample$verification_status, 'Source Verified' = 'Verified', 'Verified' = 'Verified', 'Not Verified' = 'Not Verified')

#select the relevant columns of our df
lendingclub_sample <- lendingclub_sample[,c(1:20)]

#create dummies for the categorical variables
lendingclub_sample_dummied <- dummy_cols(lendingclub_sample, select_columns = c('term (months)','grade','emp_length','home_ownership','verification_status','purpose'))

#delete the useless dummies
drops <- c("term (months)_60","term (months)_NA","grade_G","grade_NA","emp_length_> 5 years","emp_length_n/a","emp_length_NA","home_ownership_OTHER","home_ownership_NA","verification_status_Verified","verification_status_NA","purpose_other","purpose_NA")
lendingclub_sample_dummied_cleaned <- lendingclub_sample_dummied[ , !(names(lendingclub_sample_dummied) %in% drops)]
lendingclub_sample_dummied_cleaned_names <- clean_names(lendingclub_sample_dummied_cleaned)

#display the names of the selected variables
colnames(lendingclub_sample_dummied_cleaned_names)
```

    ##  [1] "int_rate"                         "loan_amnt"                       
    ##  [3] "term_months"                      "installment"                     
    ##  [5] "dti"                              "delinq_2yrs"                     
    ##  [7] "annual_inc"                       "grade"                           
    ##  [9] "emp_title"                        "emp_length"                      
    ## [11] "home_ownership"                   "verification_status"             
    ## [13] "issue_d"                          "zip_code"                        
    ## [15] "addr_state"                       "loan_status"                     
    ## [17] "desc"                             "purpose"                         
    ## [19] "title"                            "loan_stat_binary"                
    ## [21] "term_months_36"                   "grade_a"                         
    ## [23] "grade_b"                          "grade_c"                         
    ## [25] "grade_d"                          "grade_e"                         
    ## [27] "grade_f"                          "emp_length_2_years"              
    ## [29] "emp_length_2_5_years"             "home_ownership_mortgage"         
    ## [31] "home_ownership_none"              "home_ownership_own"              
    ## [33] "home_ownership_rent"              "verification_status_not_verified"
    ## [35] "purpose_car"                      "purpose_credit_card"             
    ## [37] "purpose_debt_consolidation"       "purpose_educational"             
    ## [39] "purpose_home_improvement"         "purpose_house"                   
    ## [41] "purpose_major_purchase"           "purpose_medical"                 
    ## [43] "purpose_moving"                   "purpose_renewable_energy"        
    ## [45] "purpose_small_business"           "purpose_vacation"                
    ## [47] "purpose_wedding"

``` r
#create a new logistic regression with the new variables
logistic_reg_2 <- glm(loan_stat_binary ~ 
                        loan_amnt + 
                        installment + 
                        dti +
                        annual_inc +
                        term_months_36 + 
                        grade_a + grade_b + grade_c + grade_d + grade_e + grade_f + 
                        emp_length_2_years + emp_length_2_5_years + 
                        home_ownership_mortgage + home_ownership_own + home_ownership_rent + 
                        verification_status_not_verified + 
                        purpose_car + purpose_credit_card + purpose_debt_consolidation + purpose_educational + purpose_home_improvement + purpose_house + purpose_major_purchase + purpose_medical + purpose_moving + purpose_renewable_energy + purpose_small_business + purpose_vacation + purpose_wedding,
                        family = binomial,
                        data = lendingclub_sample_dummied_cleaned_names)
summary(logistic_reg_2)
```

    ## 
    ## Call:
    ## glm(formula = loan_stat_binary ~ loan_amnt + installment + dti + 
    ##     annual_inc + term_months_36 + grade_a + grade_b + grade_c + 
    ##     grade_d + grade_e + grade_f + emp_length_2_years + emp_length_2_5_years + 
    ##     home_ownership_mortgage + home_ownership_own + home_ownership_rent + 
    ##     verification_status_not_verified + purpose_car + purpose_credit_card + 
    ##     purpose_debt_consolidation + purpose_educational + purpose_home_improvement + 
    ##     purpose_house + purpose_major_purchase + purpose_medical + 
    ##     purpose_moving + purpose_renewable_energy + purpose_small_business + 
    ##     purpose_vacation + purpose_wedding, family = binomial, data = lendingclub_sample_dummied_cleaned_names)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.2491  -0.5980  -0.4735  -0.3400   3.2956  
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                      -1.197e-01  6.227e-01  -0.192 0.847505    
    ## loan_amnt                        -3.077e-05  2.560e-05  -1.202 0.229261    
    ## installment                       1.407e-03  8.725e-04   1.613 0.106827    
    ## dti                               1.032e-02  4.878e-03   2.115 0.034402 *  
    ## annual_inc                       -5.142e-06  9.969e-07  -5.158 2.49e-07 ***
    ## term_months_36                   -5.436e-01  1.418e-01  -3.833 0.000126 ***
    ## grade_a                          -1.541e+00  2.731e-01  -5.641 1.69e-08 ***
    ## grade_b                          -9.688e-01  2.581e-01  -3.754 0.000174 ***
    ## grade_c                          -6.261e-01  2.559e-01  -2.446 0.014426 *  
    ## grade_d                          -4.193e-01  2.534e-01  -1.655 0.098008 .  
    ## grade_e                          -2.953e-01  2.577e-01  -1.146 0.251833    
    ## grade_f                          -1.548e-01  2.822e-01  -0.549 0.583195    
    ## emp_length_2_years               -1.354e-01  8.752e-02  -1.547 0.121885    
    ## emp_length_2_5_years             -2.438e-01  7.150e-02  -3.409 0.000652 ***
    ## home_ownership_mortgage          -2.256e-01  5.565e-01  -0.405 0.685184    
    ## home_ownership_own               -6.767e-02  5.641e-01  -0.120 0.904512    
    ## home_ownership_rent              -1.957e-01  5.556e-01  -0.352 0.724648    
    ## verification_status_not_verified  2.945e-02  6.931e-02   0.425 0.670934    
    ## purpose_car                      -3.917e-01  2.063e-01  -1.899 0.057586 .  
    ## purpose_credit_card              -3.950e-01  1.377e-01  -2.868 0.004130 ** 
    ## purpose_debt_consolidation       -1.137e-01  1.071e-01  -1.062 0.288257    
    ## purpose_educational              -3.121e-01  4.181e-01  -0.746 0.455477    
    ## purpose_home_improvement         -1.374e-01  1.539e-01  -0.892 0.372200    
    ## purpose_house                     1.386e-01  3.186e-01   0.435 0.663584    
    ## purpose_major_purchase           -4.386e-01  1.846e-01  -2.375 0.017529 *  
    ## purpose_medical                  -1.603e-02  2.503e-01  -0.064 0.948949    
    ## purpose_moving                   -1.007e-01  2.815e-01  -0.358 0.720673    
    ## purpose_renewable_energy          4.138e-01  5.763e-01   0.718 0.472806    
    ## purpose_small_business            6.187e-01  1.496e-01   4.135 3.55e-05 ***
    ## purpose_vacation                  1.721e-01  3.595e-01   0.479 0.632088    
    ## purpose_wedding                  -4.985e-01  2.621e-01  -1.902 0.057211 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 7358.8  on 8932  degrees of freedom
    ## Residual deviance: 6891.3  on 8902  degrees of freedom
    ##   (1067 observations deleted due to missingness)
    ## AIC: 6953.3
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
#predict with the log reg 2 the default probability with our current dataset and store in a df
pred_log_reg_2 <- predict(logistic_reg_2, newdata = lendingclub_sample_dummied_cleaned_names, type = "response")
pred_log_reg_2_df <- data.frame(pred_log_reg_2)

#isolate the binary variables to create the ROC curve
sample_binary_cleaned_loan_stat <- lendingclub_sample_dummied_cleaned_names["loan_stat_binary"]

#create a ROC curve to assess the model's effectiveness
ROC_logreg_2 <- cbind(sample_binary_cleaned_loan_stat,pred_log_reg_2_df)
ROC_logreg_2_clean <- na.omit(ROC_logreg_2)
PRROC_obj <- roc.curve(scores.class0 = ROC_logreg_2_clean$pred_log_reg, weights.class0=ROC_logreg_2_clean$loan_stat_binary,
                       curve=TRUE)
plot(PRROC_obj)
```

![](Hadrien_Pistre_Assessment_2_R_Code_bis_files/figure-markdown_github/second%20logistic%20reg-1.png)

``` r
#create interaction terms between the home ownership status and the credit rating
lendingclub_sample_dummied_cleaned_names$home_ownsh_x_mort_E <- lendingclub_sample_dummied_cleaned_names$home_ownership_mortgage * lendingclub_sample_dummied_cleaned_names$grade_e
lendingclub_sample_dummied_cleaned_names$home_ownsh_x_own_E <- lendingclub_sample_dummied_cleaned_names$home_ownership_own * lendingclub_sample_dummied_cleaned_names$grade_e
lendingclub_sample_dummied_cleaned_names$home_ownsh_x_rent_E <- lendingclub_sample_dummied_cleaned_names$home_ownership_rent * lendingclub_sample_dummied_cleaned_names$grade_e
lendingclub_sample_dummied_cleaned_names$home_ownsh_x_mort_F <- lendingclub_sample_dummied_cleaned_names$home_ownership_mortgage * lendingclub_sample_dummied_cleaned_names$grade_f
lendingclub_sample_dummied_cleaned_names$home_ownsh_x_own_F <- lendingclub_sample_dummied_cleaned_names$home_ownership_own * lendingclub_sample_dummied_cleaned_names$grade_f
lendingclub_sample_dummied_cleaned_names$home_ownsh_x_rent_F <- lendingclub_sample_dummied_cleaned_names$home_ownership_rent * lendingclub_sample_dummied_cleaned_names$grade_f

#create interaction terms between the loan term and the loan amount
lendingclub_sample_dummied_cleaned_names$loan_amnt_x_term_36 <- lendingclub_sample_dummied_cleaned_names$loan_amnt * lendingclub_sample_dummied_cleaned_names$term_months_36

#create a new logistic regression with our interaction terms
logistic_reg_3 <- glm(loan_stat_binary ~ 
                        loan_amnt + 
                        installment + 
                        dti +
                        annual_inc +
                        term_months_36 + 
                        grade_a + grade_b + grade_c + grade_d + grade_e + grade_f + 
                        emp_length_2_years + emp_length_2_5_years + 
                        home_ownership_mortgage + home_ownership_own + home_ownership_rent + 
                        verification_status_not_verified + 
                        purpose_car + purpose_credit_card + purpose_debt_consolidation + purpose_educational + purpose_home_improvement + purpose_house + purpose_major_purchase + purpose_medical + purpose_moving + purpose_renewable_energy + purpose_small_business + purpose_vacation + purpose_wedding +
                        home_ownsh_x_mort_E + home_ownsh_x_own_E + home_ownsh_x_rent_E +
                        home_ownsh_x_mort_F + home_ownsh_x_own_F + home_ownsh_x_rent_F +
                        loan_amnt_x_term_36,
                        family = binomial,
                        data = lendingclub_sample_dummied_cleaned_names)

summary(logistic_reg_3)
```

    ## 
    ## Call:
    ## glm(formula = loan_stat_binary ~ loan_amnt + installment + dti + 
    ##     annual_inc + term_months_36 + grade_a + grade_b + grade_c + 
    ##     grade_d + grade_e + grade_f + emp_length_2_years + emp_length_2_5_years + 
    ##     home_ownership_mortgage + home_ownership_own + home_ownership_rent + 
    ##     verification_status_not_verified + purpose_car + purpose_credit_card + 
    ##     purpose_debt_consolidation + purpose_educational + purpose_home_improvement + 
    ##     purpose_house + purpose_major_purchase + purpose_medical + 
    ##     purpose_moving + purpose_renewable_energy + purpose_small_business + 
    ##     purpose_vacation + purpose_wedding + home_ownsh_x_mort_E + 
    ##     home_ownsh_x_own_E + home_ownsh_x_rent_E + home_ownsh_x_mort_F + 
    ##     home_ownsh_x_own_F + home_ownsh_x_rent_F + loan_amnt_x_term_36, 
    ##     family = binomial, data = lendingclub_sample_dummied_cleaned_names)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3015  -0.5940  -0.4762  -0.3399   3.3312  
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                      -4.890e-01  6.415e-01  -0.762 0.445843    
    ## loan_amnt                        -1.838e-04  5.475e-05  -3.358 0.000786 ***
    ## installment                       7.862e-03  2.211e-03   3.556 0.000376 ***
    ## dti                               9.761e-03  4.895e-03   1.994 0.046158 *  
    ## annual_inc                       -5.246e-06  9.990e-07  -5.251 1.51e-07 ***
    ## term_months_36                   -5.404e-01  1.427e-01  -3.786 0.000153 ***
    ## grade_a                          -9.035e-01  3.403e-01  -2.655 0.007926 ** 
    ## grade_b                          -3.850e-01  3.184e-01  -1.209 0.226649    
    ## grade_c                          -1.311e-01  3.013e-01  -0.435 0.663361    
    ## grade_d                          -1.691e-02  2.848e-01  -0.059 0.952646    
    ## grade_e                          -1.042e+01  1.970e+02  -0.053 0.957822    
    ## grade_f                           1.471e-01  3.247e-01   0.453 0.650433    
    ## emp_length_2_years               -1.313e-01  8.756e-02  -1.500 0.133622    
    ## emp_length_2_5_years             -2.372e-01  7.167e-02  -3.309 0.000936 ***
    ## home_ownership_mortgage          -2.902e-01  5.581e-01  -0.520 0.603078    
    ## home_ownership_own               -1.770e-01  5.671e-01  -0.312 0.754996    
    ## home_ownership_rent              -2.874e-01  5.573e-01  -0.516 0.606050    
    ## verification_status_not_verified  2.885e-02  6.942e-02   0.416 0.677658    
    ## purpose_car                      -4.051e-01  2.059e-01  -1.967 0.049158 *  
    ## purpose_credit_card              -3.958e-01  1.377e-01  -2.873 0.004065 ** 
    ## purpose_debt_consolidation       -1.122e-01  1.071e-01  -1.047 0.294903    
    ## purpose_educational              -3.081e-01  4.172e-01  -0.739 0.460163    
    ## purpose_home_improvement         -1.374e-01  1.539e-01  -0.893 0.371943    
    ## purpose_house                     1.526e-01  3.214e-01   0.475 0.634904    
    ## purpose_major_purchase           -4.450e-01  1.845e-01  -2.412 0.015853 *  
    ## purpose_medical                  -2.022e-02  2.504e-01  -0.081 0.935620    
    ## purpose_moving                   -1.109e-01  2.809e-01  -0.395 0.692937    
    ## purpose_renewable_energy          3.841e-01  5.762e-01   0.667 0.504990    
    ## purpose_small_business            6.336e-01  1.500e-01   4.226 2.38e-05 ***
    ## purpose_vacation                  1.284e-01  3.590e-01   0.358 0.720583    
    ## purpose_wedding                  -4.925e-01  2.621e-01  -1.879 0.060217 .  
    ## home_ownsh_x_mort_E               1.030e+01  1.970e+02   0.052 0.958303    
    ## home_ownsh_x_own_E                1.078e+01  1.970e+02   0.055 0.956353    
    ## home_ownsh_x_rent_E               1.042e+01  1.970e+02   0.053 0.957823    
    ## home_ownsh_x_mort_F              -2.945e-01  3.259e-01  -0.903 0.366274    
    ## home_ownsh_x_own_F               -7.524e-01  7.038e-01  -1.069 0.285051    
    ## home_ownsh_x_rent_F                      NA         NA      NA       NA    
    ## loan_amnt_x_term_36              -6.873e-05  2.145e-05  -3.204 0.001356 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 7358.8  on 8932  degrees of freedom
    ## Residual deviance: 6877.7  on 8896  degrees of freedom
    ##   (1067 observations deleted due to missingness)
    ## AIC: 6951.7
    ## 
    ## Number of Fisher Scoring iterations: 10

``` r
#predict with the log reg 2 the default probability with our current dataset and store in a df
pred_log_reg_3 <- predict(logistic_reg_3, newdata = lendingclub_sample_dummied_cleaned_names, type = "response")
pred_log_reg_3_df <- data.frame(pred_log_reg_3)

#plot a ROC curve for log reg 3
ROC_logreg_3 <- cbind(sample_binary_cleaned_loan_stat,pred_log_reg_3_df)
ROC_logreg_3_clean <- na.omit(ROC_logreg_3)
PRROC_obj <- roc.curve(scores.class0 = ROC_logreg_3_clean$pred_log_reg, weights.class0=ROC_logreg_3_clean$loan_stat_binary,
                       curve=TRUE)
plot(PRROC_obj)
```

![](Hadrien_Pistre_Assessment_2_R_Code_bis_files/figure-markdown_github/interaction%20terms%20and%20third%20log%20reg-1.png)

``` r
#create a logistic regression removing the variables yielding only NAs
logistic_reg_4 <- glm(loan_stat_binary ~ 
                        loan_amnt + 
                        installment + 
                        dti +
                        annual_inc +
                        term_months_36 + 
                        grade_a + grade_b + grade_c + grade_d + grade_e + grade_f + 
                        emp_length_2_years + emp_length_2_5_years + 
                        home_ownership_mortgage + home_ownership_own + home_ownership_rent + 
                        verification_status_not_verified + 
                        purpose_car + purpose_credit_card + purpose_debt_consolidation + purpose_educational + purpose_home_improvement + purpose_house + purpose_major_purchase + purpose_medical + purpose_moving + purpose_renewable_energy + purpose_small_business + purpose_vacation + purpose_wedding +
                        home_ownsh_x_mort_E + home_ownsh_x_own_E + home_ownsh_x_rent_E +
                        home_ownsh_x_mort_F + home_ownsh_x_own_F +
                        loan_amnt_x_term_36,
                        family = binomial,
                        data = lendingclub_sample_dummied_cleaned_names)

summary(logistic_reg_4)
```

    ## 
    ## Call:
    ## glm(formula = loan_stat_binary ~ loan_amnt + installment + dti + 
    ##     annual_inc + term_months_36 + grade_a + grade_b + grade_c + 
    ##     grade_d + grade_e + grade_f + emp_length_2_years + emp_length_2_5_years + 
    ##     home_ownership_mortgage + home_ownership_own + home_ownership_rent + 
    ##     verification_status_not_verified + purpose_car + purpose_credit_card + 
    ##     purpose_debt_consolidation + purpose_educational + purpose_home_improvement + 
    ##     purpose_house + purpose_major_purchase + purpose_medical + 
    ##     purpose_moving + purpose_renewable_energy + purpose_small_business + 
    ##     purpose_vacation + purpose_wedding + home_ownsh_x_mort_E + 
    ##     home_ownsh_x_own_E + home_ownsh_x_rent_E + home_ownsh_x_mort_F + 
    ##     home_ownsh_x_own_F + loan_amnt_x_term_36, family = binomial, 
    ##     data = lendingclub_sample_dummied_cleaned_names)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3015  -0.5940  -0.4762  -0.3399   3.3312  
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                      -4.890e-01  6.415e-01  -0.762 0.445843    
    ## loan_amnt                        -1.838e-04  5.475e-05  -3.358 0.000786 ***
    ## installment                       7.862e-03  2.211e-03   3.556 0.000376 ***
    ## dti                               9.761e-03  4.895e-03   1.994 0.046158 *  
    ## annual_inc                       -5.246e-06  9.990e-07  -5.251 1.51e-07 ***
    ## term_months_36                   -5.404e-01  1.427e-01  -3.786 0.000153 ***
    ## grade_a                          -9.035e-01  3.403e-01  -2.655 0.007926 ** 
    ## grade_b                          -3.850e-01  3.184e-01  -1.209 0.226649    
    ## grade_c                          -1.311e-01  3.013e-01  -0.435 0.663361    
    ## grade_d                          -1.691e-02  2.848e-01  -0.059 0.952646    
    ## grade_e                          -1.042e+01  1.970e+02  -0.053 0.957822    
    ## grade_f                           1.471e-01  3.247e-01   0.453 0.650433    
    ## emp_length_2_years               -1.313e-01  8.756e-02  -1.500 0.133622    
    ## emp_length_2_5_years             -2.372e-01  7.167e-02  -3.309 0.000936 ***
    ## home_ownership_mortgage          -2.902e-01  5.581e-01  -0.520 0.603078    
    ## home_ownership_own               -1.770e-01  5.671e-01  -0.312 0.754996    
    ## home_ownership_rent              -2.874e-01  5.573e-01  -0.516 0.606050    
    ## verification_status_not_verified  2.885e-02  6.942e-02   0.416 0.677658    
    ## purpose_car                      -4.051e-01  2.059e-01  -1.967 0.049158 *  
    ## purpose_credit_card              -3.958e-01  1.377e-01  -2.873 0.004065 ** 
    ## purpose_debt_consolidation       -1.122e-01  1.071e-01  -1.047 0.294903    
    ## purpose_educational              -3.081e-01  4.172e-01  -0.739 0.460163    
    ## purpose_home_improvement         -1.374e-01  1.539e-01  -0.893 0.371943    
    ## purpose_house                     1.526e-01  3.214e-01   0.475 0.634904    
    ## purpose_major_purchase           -4.450e-01  1.845e-01  -2.412 0.015853 *  
    ## purpose_medical                  -2.022e-02  2.504e-01  -0.081 0.935620    
    ## purpose_moving                   -1.109e-01  2.809e-01  -0.395 0.692937    
    ## purpose_renewable_energy          3.841e-01  5.762e-01   0.667 0.504990    
    ## purpose_small_business            6.336e-01  1.500e-01   4.226 2.38e-05 ***
    ## purpose_vacation                  1.284e-01  3.590e-01   0.358 0.720583    
    ## purpose_wedding                  -4.925e-01  2.621e-01  -1.879 0.060217 .  
    ## home_ownsh_x_mort_E               1.030e+01  1.970e+02   0.052 0.958303    
    ## home_ownsh_x_own_E                1.078e+01  1.970e+02   0.055 0.956353    
    ## home_ownsh_x_rent_E               1.042e+01  1.970e+02   0.053 0.957823    
    ## home_ownsh_x_mort_F              -2.945e-01  3.259e-01  -0.903 0.366274    
    ## home_ownsh_x_own_F               -7.524e-01  7.038e-01  -1.069 0.285051    
    ## loan_amnt_x_term_36              -6.873e-05  2.145e-05  -3.204 0.001356 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 7358.8  on 8932  degrees of freedom
    ## Residual deviance: 6877.7  on 8896  degrees of freedom
    ##   (1067 observations deleted due to missingness)
    ## AIC: 6951.7
    ## 
    ## Number of Fisher Scoring iterations: 10

``` r
#predicting default probabilities on our current dataset and store in a df
pred_log_reg_4 <- predict(logistic_reg_4, newdata = lendingclub_sample_dummied_cleaned_names, type = "response")
pred_log_reg_4_df <- data.frame(pred_log_reg_4)

#create a ROC curve
ROC_logreg_4 <- cbind(sample_binary_cleaned_loan_stat,pred_log_reg_4_df)
ROC_logreg_4_clean <- na.omit(ROC_logreg_4)
PRROC_obj <- roc.curve(scores.class0 = ROC_logreg_4_clean$pred_log_reg, weights.class0=ROC_logreg_4_clean$loan_stat_binary,
                       curve=TRUE)
plot(PRROC_obj)
```

![](Hadrien_Pistre_Assessment_2_R_Code_bis_files/figure-markdown_github/fourth%20logistic%20reg-1.png)

## 6. The model we have estimated in the previous step used the same data to estimate \[…\]

``` r
#select 60% of the sample
smp_size <- floor(0.6 * nrow(lendingclub_sample_dummied_cleaned_names))

#set a seed to make the sample reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(lendingclub_sample_dummied_cleaned_names)), size = smp_size)

#assign the training and the testing datasets to new dfs
train <- lendingclub_sample_dummied_cleaned_names[train_ind, ]
test <- lendingclub_sample_dummied_cleaned_names[-train_ind, ]

#take the model we created before and train in on the training data
logistic_reg_trained <- glm(loan_stat_binary ~ 
                        loan_amnt + 
                        installment + 
                        dti +
                        annual_inc +
                        term_months_36 + 
                        grade_a + grade_b + grade_c + grade_d + grade_e + grade_f + 
                        emp_length_2_years + emp_length_2_5_years + 
                        home_ownership_mortgage + home_ownership_own + home_ownership_rent + 
                        verification_status_not_verified + 
                        purpose_car + purpose_credit_card + purpose_debt_consolidation + purpose_educational + purpose_home_improvement + purpose_house + purpose_major_purchase + purpose_medical + purpose_moving + purpose_renewable_energy + purpose_small_business + purpose_vacation + purpose_wedding +
                        home_ownsh_x_mort_E + home_ownsh_x_own_E + home_ownsh_x_rent_E +
                        home_ownsh_x_mort_F + home_ownsh_x_own_F +
                        loan_amnt_x_term_36,
                        family = binomial,
                        data = train)
summary(logistic_reg_trained)
```

    ## 
    ## Call:
    ## glm(formula = loan_stat_binary ~ loan_amnt + installment + dti + 
    ##     annual_inc + term_months_36 + grade_a + grade_b + grade_c + 
    ##     grade_d + grade_e + grade_f + emp_length_2_years + emp_length_2_5_years + 
    ##     home_ownership_mortgage + home_ownership_own + home_ownership_rent + 
    ##     verification_status_not_verified + purpose_car + purpose_credit_card + 
    ##     purpose_debt_consolidation + purpose_educational + purpose_home_improvement + 
    ##     purpose_house + purpose_major_purchase + purpose_medical + 
    ##     purpose_moving + purpose_renewable_energy + purpose_small_business + 
    ##     purpose_vacation + purpose_wedding + home_ownsh_x_mort_E + 
    ##     home_ownsh_x_own_E + home_ownsh_x_rent_E + home_ownsh_x_mort_F + 
    ##     home_ownsh_x_own_F + loan_amnt_x_term_36, family = binomial, 
    ##     data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.2710  -0.5850  -0.4644  -0.3351   3.3826  
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                      -1.086e+00  1.123e+00  -0.967 0.333519    
    ## loan_amnt                        -1.246e-04  6.996e-05  -1.781 0.074867 .  
    ## installment                       5.208e-03  2.837e-03   1.835 0.066462 .  
    ## dti                               1.226e-02  6.436e-03   1.905 0.056797 .  
    ## annual_inc                       -5.021e-06  1.316e-06  -3.815 0.000136 ***
    ## term_months_36                   -6.149e-01  1.842e-01  -3.339 0.000841 ***
    ## grade_a                          -1.231e+00  4.402e-01  -2.796 0.005177 ** 
    ## grade_b                          -7.070e-01  4.125e-01  -1.714 0.086528 .  
    ## grade_c                          -3.814e-01  3.889e-01  -0.981 0.326744    
    ## grade_d                          -2.374e-01  3.682e-01  -0.645 0.518998    
    ## grade_e                          -1.754e-01  3.789e-01  -0.463 0.643463    
    ## grade_f                          -2.009e-01  4.466e-01  -0.450 0.652814    
    ## emp_length_2_years               -7.529e-02  1.141e-01  -0.660 0.509310    
    ## emp_length_2_5_years             -2.095e-01  9.412e-02  -2.225 0.026052 *  
    ## home_ownership_mortgage           6.098e-01  1.048e+00   0.582 0.560588    
    ## home_ownership_own                5.404e-01  1.057e+00   0.511 0.609118    
    ## home_ownership_rent               5.507e-01  1.047e+00   0.526 0.599047    
    ## verification_status_not_verified -3.053e-02  9.137e-02  -0.334 0.738300    
    ## purpose_car                      -4.649e-01  2.809e-01  -1.655 0.097936 .  
    ## purpose_credit_card              -4.088e-01  1.775e-01  -2.303 0.021298 *  
    ## purpose_debt_consolidation       -1.433e-01  1.387e-01  -1.033 0.301485    
    ## purpose_educational               1.911e-01  5.130e-01   0.372 0.709533    
    ## purpose_home_improvement         -1.890e-01  2.002e-01  -0.944 0.345098    
    ## purpose_house                    -8.139e-02  4.170e-01  -0.195 0.845265    
    ## purpose_major_purchase           -3.247e-01  2.296e-01  -1.414 0.157379    
    ## purpose_medical                  -1.914e-02  3.109e-01  -0.062 0.950895    
    ## purpose_moving                   -3.504e-01  3.979e-01  -0.881 0.378417    
    ## purpose_renewable_energy          6.952e-01  7.010e-01   0.992 0.321337    
    ## purpose_small_business            5.653e-01  1.970e-01   2.869 0.004112 ** 
    ## purpose_vacation                  3.872e-01  4.394e-01   0.881 0.378268    
    ## purpose_wedding                  -3.021e-01  3.338e-01  -0.905 0.365525    
    ## home_ownsh_x_mort_E              -1.421e-01  2.705e-01  -0.525 0.599477    
    ## home_ownsh_x_own_E                5.981e-01  4.788e-01   1.249 0.211612    
    ## home_ownsh_x_rent_E                      NA         NA      NA       NA    
    ## home_ownsh_x_mort_F              -1.687e-01  4.539e-01  -0.372 0.710172    
    ## home_ownsh_x_own_F               -1.444e-01  9.092e-01  -0.159 0.873835    
    ## loan_amnt_x_term_36              -3.677e-05  2.788e-05  -1.319 0.187194    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 4314.6  on 5360  degrees of freedom
    ## Residual deviance: 4034.7  on 5325  degrees of freedom
    ##   (639 observations deleted due to missingness)
    ## AIC: 4106.7
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
#use our trained model to predict default probabilities on the testing dataset
pred_on_test <- predict(logistic_reg_trained, newdata = test, type = "response")
pred_on_test_df <- data.frame(pred_on_test)

#isolate the binary default probabilities of the testing data set to create the ROC curve
test_loan_stat <- test["loan_stat_binary"]

#create the ROC curve of our trained model predicting default probabilities on the testing dataset vs the actual defaults of the testing dataset
ROC_logreg_on_test <- cbind(test_loan_stat,pred_on_test_df)
ROC_logreg_on_test_clean <- na.omit(ROC_logreg_on_test)

PRROC_obj <- roc.curve(scores.class0 = ROC_logreg_on_test_clean$pred_on_test, weights.class0=ROC_logreg_on_test_clean$loan_stat_binary,
                       curve=TRUE)
plot(PRROC_obj)
```

![](Hadrien_Pistre_Assessment_2_R_Code_bis_files/figure-markdown_github/training%20and%20testing%20datasets-1.png)

## 7. Let’s exploit the model to select loans to invest.

### a. Before we focus on a more realistic revenue model \[…\]

``` r
#select actual defaults the from training set
training_loan_stat <- train["loan_stat_binary"]

#predict trained model on training dataset and put the prediction in a df
pred_on_trained <- predict(logistic_reg_trained, newdata = train, type = "response")
pred_on_trained_df <- data.frame(pred_on_trained)

#create a first function to speed up the comparison, x being the cut-off, y the loss and x the profit
pred_on_trained_function <- function(x,y,z){

  #transform the predicted default probabilities into binary default probabilities according to cut-off x
  pred_on_trained <- ifelse(pred_on_trained_df > x, 1, 0)

  #bind actual binary default probabilities of training set and predicted ones to later create our ROC curve
  pred_on_trained_vs_trained <- cbind(training_loan_stat,pred_on_trained)

  #create our p&l using a nested ifelse statement as follows:
  
  pred_on_trained_vs_trained$p_l <- ifelse(pred_on_trained_vs_trained$loan_stat_binary == 1 & pred_on_trained_vs_trained$pred_on_trained == 1,0,#if we predict the loan will default (1) and it defaults (1), then we did not invest and have a 0 p&l
                                           ifelse(pred_on_trained_vs_trained$loan_stat_binary == 0 & pred_on_trained_vs_trained$pred_on_trained == 1,0,#if we predict the loan will default (1) and it does not default (0), then we did not invest and have a 0 p&l (though we incur a cost of opportunity)
                                                  ifelse(pred_on_trained_vs_trained$loan_stat_binary == 1 & pred_on_trained_vs_trained$pred_on_trained == 0,y,#if we predict the loan will not default (0) and it defaults (1), then we invested and we lose y
                                                         ifelse(pred_on_trained_vs_trained$loan_stat_binary == 0 & pred_on_trained_vs_trained$pred_on_trained == 0,z,z))))#if we predict the loan will not default (0) and it does not default (0), then we invested and we make a gain of x

  #clean our df with the p&l from NAs      
  pred_on_trained_vs_trained_clean <- na.omit(pred_on_trained_vs_trained)

  #print and paste the total p&l
  pnl <- sum(pred_on_trained_vs_trained_clean$p_l)
  print(paste(pnl))
}#end of first function

#create a second function using the first function to plot the graphs
graph_trained_on_train_function <- function(x,y){

  #assign p&ls for cut-offs by 0.5 increments
  pnl_05 <- pred_on_trained_function(0.05,x,y)
  pnl_10 <- pred_on_trained_function(0.1,x,y)
  pnl_15 <- pred_on_trained_function(0.15,x,y)
  pnl_20 <- pred_on_trained_function(0.2,x,y)
  pnl_25 <- pred_on_trained_function(0.25,x,y)
  pnl_30 <- pred_on_trained_function(0.3,x,y)
  pnl_35 <- pred_on_trained_function(0.35,x,y)
  pnl_40 <- pred_on_trained_function(0.40,x,y)
  pnl_45 <- pred_on_trained_function(0.45,x,y)
  pnl_50 <- pred_on_trained_function(0.50,x,y)
  pnl_55 <- pred_on_trained_function(0.55,x,y)
  pnl_60 <- pred_on_trained_function(0.60,x,y)
  pnl_65 <- pred_on_trained_function(0.65,x,y)
  pnl_70 <- pred_on_trained_function(0.70,x,y)
  pnl_75 <- pred_on_trained_function(0.75,x,y)
  pnl_80 <- pred_on_trained_function(0.80,x,y)
  pnl_85 <- pred_on_trained_function(0.85,x,y)
  pnl_90 <- pred_on_trained_function(0.90,x,y)
  pnl_95 <- pred_on_trained_function(0.95,x,y)
  pnl_100 <- pred_on_trained_function(1,x,y)

  #put our inputs in a df
  pred_on_trained_sensitivity <- data.frame(cut_off = c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1),
                                            pnl = c(pnl_05,pnl_10,pnl_15,pnl_20,pnl_25,pnl_30,pnl_35,pnl_40,pnl_45,pnl_50,pnl_55,pnl_60,pnl_65,pnl_70,pnl_75,pnl_80,pnl_85,pnl_90,pnl_95,pnl_100))

  #set the p&l df as numeric to have continuous scales on our graph
  pred_on_trained_sensitivity$cut_off <- as.numeric(pred_on_trained_sensitivity$cut_off)
  pred_on_trained_sensitivity$pnl <- as.numeric(pred_on_trained_sensitivity$pnl)

  #print the tables
  print(pred_on_trained_sensitivity)
  
  #plot the tables
  ggplot(pred_on_trained_sensitivity, aes(x=cut_off,y=pnl,group = 1))+
    geom_line(alpha=1) +
    theme_bw() +
    scale_y_continuous()+
    labs (
      title = paste("Sensitivity of P&L to cut-off using the trained model on the training dataset\nwith loss =", x,"and profit =",y),
      x     = "Cut-off",
      y     = "P&L")
  }#end of function 2

#do a sensitivity analysis of profit by 10 increments
graph_trained_on_train_function(-100,20)
```

    ## [1] "6380"
    ## [1] "24360"
    ## [1] "33360"
    ## [1] "31820"
    ## [1] "27920"
    ## [1] "24760"
    ## [1] "21500"
    ## [1] "19720"
    ## [1] "19040"
    ## [1] "18420"
    ## [1] "18140"
    ## [1] "18060"
    ## [1] "18060"
    ## [1] "18060"
    ## [1] "18060"
    ## [1] "18060"
    ## [1] "18060"
    ## [1] "18060"
    ## [1] "18060"
    ## [1] "18060"
    ##    cut_off   pnl
    ## 1     0.05  6380
    ## 2     0.10 24360
    ## 3     0.15 33360
    ## 4     0.20 31820
    ## 5     0.25 27920
    ## 6     0.30 24760
    ## 7     0.35 21500
    ## 8     0.40 19720
    ## 9     0.45 19040
    ## 10    0.50 18420
    ## 11    0.55 18140
    ## 12    0.60 18060
    ## 13    0.65 18060
    ## 14    0.70 18060
    ## 15    0.75 18060
    ## 16    0.80 18060
    ## 17    0.85 18060
    ## 18    0.90 18060
    ## 19    0.95 18060
    ## 20    1.00 18060

![](Hadrien_Pistre_Assessment_2_R_Code_bis_files/figure-markdown_github/sensitivity%20trained%20on%20trained-1.png)

``` r
graph_trained_on_train_function(-100,30)
```

    ## [1] "10720"
    ## [1] "43090"
    ## [1] "65040"
    ## [1] "70280"
    ## [1] "70030"
    ## [1] "69440"
    ## [1] "67100"
    ## [1] "65580"
    ## [1] "65110"
    ## [1] "64580"
    ## [1] "64310"
    ## [1] "64240"
    ## [1] "64240"
    ## [1] "64240"
    ## [1] "64240"
    ## [1] "64240"
    ## [1] "64240"
    ## [1] "64240"
    ## [1] "64240"
    ## [1] "64240"
    ##    cut_off   pnl
    ## 1     0.05 10720
    ## 2     0.10 43090
    ## 3     0.15 65040
    ## 4     0.20 70280
    ## 5     0.25 70030
    ## 6     0.30 69440
    ## 7     0.35 67100
    ## 8     0.40 65580
    ## 9     0.45 65110
    ## 10    0.50 64580
    ## 11    0.55 64310
    ## 12    0.60 64240
    ## 13    0.65 64240
    ## 14    0.70 64240
    ## 15    0.75 64240
    ## 16    0.80 64240
    ## 17    0.85 64240
    ## 18    0.90 64240
    ## 19    0.95 64240
    ## 20    1.00 64240

![](Hadrien_Pistre_Assessment_2_R_Code_bis_files/figure-markdown_github/sensitivity%20trained%20on%20trained-2.png)

``` r
graph_trained_on_train_function(-100,40)
```

    ## [1] "15060"
    ## [1] "61820"
    ## [1] "96720"
    ## [1] "108740"
    ## [1] "112140"
    ## [1] "114120"
    ## [1] "112700"
    ## [1] "111440"
    ## [1] "111180"
    ## [1] "110740"
    ## [1] "110480"
    ## [1] "110420"
    ## [1] "110420"
    ## [1] "110420"
    ## [1] "110420"
    ## [1] "110420"
    ## [1] "110420"
    ## [1] "110420"
    ## [1] "110420"
    ## [1] "110420"
    ##    cut_off    pnl
    ## 1     0.05  15060
    ## 2     0.10  61820
    ## 3     0.15  96720
    ## 4     0.20 108740
    ## 5     0.25 112140
    ## 6     0.30 114120
    ## 7     0.35 112700
    ## 8     0.40 111440
    ## 9     0.45 111180
    ## 10    0.50 110740
    ## 11    0.55 110480
    ## 12    0.60 110420
    ## 13    0.65 110420
    ## 14    0.70 110420
    ## 15    0.75 110420
    ## 16    0.80 110420
    ## 17    0.85 110420
    ## 18    0.90 110420
    ## 19    0.95 110420
    ## 20    1.00 110420

![](Hadrien_Pistre_Assessment_2_R_Code_bis_files/figure-markdown_github/sensitivity%20trained%20on%20trained-3.png)

``` r
#print the ROC curve
pred_on_trained_vs_trained <- cbind(training_loan_stat,pred_on_trained)
pred_on_trained_vs_trained <- na.omit(pred_on_trained_vs_trained)
PRROC_obj <- roc.curve(scores.class0 = pred_on_trained_vs_trained$pred_on_trained, weights.class0=pred_on_trained_vs_trained$loan_stat_binary,
                       curve=TRUE)
plot(PRROC_obj)
```

![](Hadrien_Pistre_Assessment_2_R_Code_bis_files/figure-markdown_github/sensitivity%20trained%20on%20trained-4.png)

### b. Investigate whether the optimal investment cuttoff \[…\]

``` r
#select defaults from testing set
test_loan_stat <- test["loan_stat_binary"]

#predict trained model on testing set and put the prediction in a df
pred_on_test <- predict(logistic_reg_trained, newdata = test, type = "response")
pred_on_test_df <- data.frame(pred_on_test)

#rest of the chunk is as above but using the testing set to predict default probabilities using our model trained on the traing set
pred_on_test_function <- function(x,y,z){

  pred_on_test <- ifelse(pred_on_test_df > x, 1, 0)
  
  pred_on_test_vs_trained <- cbind(test_loan_stat,pred_on_test)

  pred_on_test_vs_trained$p_l <- ifelse(pred_on_test_vs_trained$loan_stat_binary == 1 & pred_on_test_vs_trained$pred_on_test == 1,0,
                                           ifelse(pred_on_test_vs_trained$loan_stat_binary == 0 & pred_on_test_vs_trained$pred_on_test == 1,0,
                                                  ifelse(pred_on_test_vs_trained$loan_stat_binary == 1 & pred_on_test_vs_trained$pred_on_test == 0,y,
                                                         ifelse(pred_on_test_vs_trained$loan_stat_binary == 0 & pred_on_test_vs_trained$pred_on_test == 0,z,z))))
                                                  
  pred_on_trained_vs_test_clean <- na.omit(pred_on_test_vs_trained)

  pnl <- sum(pred_on_trained_vs_test_clean$p_l)

  print(paste(pnl))
}

graph_trained_on_test_function <- function(x,y){

  pnl_05 <- pred_on_test_function(0.05,x,y)
  pnl_10 <- pred_on_test_function(0.1,x,y)
  pnl_15 <- pred_on_test_function(0.15,x,y)
  pnl_20 <- pred_on_test_function(0.2,x,y)
  pnl_25 <- pred_on_test_function(0.25,x,y)
  pnl_30 <- pred_on_test_function(0.3,x,y)
  pnl_35 <- pred_on_test_function(0.35,x,y)
  pnl_40 <- pred_on_test_function(0.40,x,y)
  pnl_45 <- pred_on_test_function(0.45,x,y)
  pnl_50 <- pred_on_test_function(0.50,x,y)
  pnl_55 <- pred_on_test_function(0.55,x,y)
  pnl_60 <- pred_on_test_function(0.60,x,y)
  pnl_65 <- pred_on_test_function(0.65,x,y)
  pnl_70 <- pred_on_test_function(0.70,x,y)
  pnl_75 <- pred_on_test_function(0.75,x,y)
  pnl_80 <- pred_on_test_function(0.80,x,y)
  pnl_85 <- pred_on_test_function(0.85,x,y)
  pnl_90 <- pred_on_test_function(0.90,x,y)
  pnl_95 <- pred_on_test_function(0.95,x,y)
  pnl_100 <- pred_on_test_function(1,x,y)

  pred_on_test_sensitivity <- data.frame(cut_off = c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1),
                                            pnl = c(pnl_05,pnl_10,pnl_15,pnl_20,pnl_25,pnl_30,pnl_35,pnl_40,pnl_45,pnl_50,pnl_55,pnl_60,pnl_65,pnl_70,pnl_75,pnl_80,pnl_85,pnl_90,pnl_95,pnl_100))

  pred_on_test_sensitivity$cut_off <- as.numeric(pred_on_test_sensitivity$cut_off)
  pred_on_test_sensitivity$pnl <- as.numeric(pred_on_test_sensitivity$pnl)

  print(pred_on_test_sensitivity)
  
  ggplot(pred_on_test_sensitivity, aes(x=cut_off,y=pnl,group = 1))+
    geom_line(alpha=1) +
    theme_bw() +
    scale_y_continuous()+
    labs (
      title = paste("Sensitivity of P&L to cut-off using the trained model on the testing dataset\nwith loss =", x,"and profit =",y),
      x     = "Cut-off",
      y     = "P&L")
}

graph_trained_on_test_function(-100,20)
```

    ## [1] "4040"
    ## [1] "13980"
    ## [1] "18720"
    ## [1] "17020"
    ## [1] "13000"
    ## [1] "10520"
    ## [1] "8460"
    ## [1] "7940"
    ## [1] "6960"
    ## [1] "6660"
    ## [1] "6400"
    ## [1] "6400"
    ## [1] "6400"
    ## [1] "6400"
    ## [1] "6400"
    ## [1] "6400"
    ## [1] "6400"
    ## [1] "6400"
    ## [1] "6400"
    ## [1] "6400"
    ##    cut_off   pnl
    ## 1     0.05  4040
    ## 2     0.10 13980
    ## 3     0.15 18720
    ## 4     0.20 17020
    ## 5     0.25 13000
    ## 6     0.30 10520
    ## 7     0.35  8460
    ## 8     0.40  7940
    ## 9     0.45  6960
    ## 10    0.50  6660
    ## 11    0.55  6400
    ## 12    0.60  6400
    ## 13    0.65  6400
    ## 14    0.70  6400
    ## 15    0.75  6400
    ## 16    0.80  6400
    ## 17    0.85  6400
    ## 18    0.90  6400
    ## 19    0.95  6400
    ## 20    1.00  6400

![](Hadrien_Pistre_Assessment_2_R_Code_bis_files/figure-markdown_github/sensitivity%20trained%20on%20test-1.png)

``` r
graph_trained_on_test_function(-100,30)
```

    ## [1] "6560"
    ## [1] "26070"
    ## [1] "39030"
    ## [1] "42230"
    ## [1] "40700"
    ## [1] "39730"
    ## [1] "38340"
    ## [1] "38060"
    ## [1] "37190"
    ## [1] "36940"
    ## [1] "36700"
    ## [1] "36700"
    ## [1] "36700"
    ## [1] "36700"
    ## [1] "36700"
    ## [1] "36700"
    ## [1] "36700"
    ## [1] "36700"
    ## [1] "36700"
    ## [1] "36700"
    ##    cut_off   pnl
    ## 1     0.05  6560
    ## 2     0.10 26070
    ## 3     0.15 39030
    ## 4     0.20 42230
    ## 5     0.25 40700
    ## 6     0.30 39730
    ## 7     0.35 38340
    ## 8     0.40 38060
    ## 9     0.45 37190
    ## 10    0.50 36940
    ## 11    0.55 36700
    ## 12    0.60 36700
    ## 13    0.65 36700
    ## 14    0.70 36700
    ## 15    0.75 36700
    ## 16    0.80 36700
    ## 17    0.85 36700
    ## 18    0.90 36700
    ## 19    0.95 36700
    ## 20    1.00 36700

![](Hadrien_Pistre_Assessment_2_R_Code_bis_files/figure-markdown_github/sensitivity%20trained%20on%20test-2.png)

``` r
graph_trained_on_test_function(-100,40)
```

    ## [1] "9080"
    ## [1] "38160"
    ## [1] "59340"
    ## [1] "67440"
    ## [1] "68400"
    ## [1] "68940"
    ## [1] "68220"
    ## [1] "68180"
    ## [1] "67420"
    ## [1] "67220"
    ## [1] "67000"
    ## [1] "67000"
    ## [1] "67000"
    ## [1] "67000"
    ## [1] "67000"
    ## [1] "67000"
    ## [1] "67000"
    ## [1] "67000"
    ## [1] "67000"
    ## [1] "67000"
    ##    cut_off   pnl
    ## 1     0.05  9080
    ## 2     0.10 38160
    ## 3     0.15 59340
    ## 4     0.20 67440
    ## 5     0.25 68400
    ## 6     0.30 68940
    ## 7     0.35 68220
    ## 8     0.40 68180
    ## 9     0.45 67420
    ## 10    0.50 67220
    ## 11    0.55 67000
    ## 12    0.60 67000
    ## 13    0.65 67000
    ## 14    0.70 67000
    ## 15    0.75 67000
    ## 16    0.80 67000
    ## 17    0.85 67000
    ## 18    0.90 67000
    ## 19    0.95 67000
    ## 20    1.00 67000

![](Hadrien_Pistre_Assessment_2_R_Code_bis_files/figure-markdown_github/sensitivity%20trained%20on%20test-3.png)

``` r
pred_on_test_vs_trained <- cbind(test_loan_stat,pred_on_test)
pred_on_test_vs_trained <- na.omit(pred_on_test_vs_trained)

PRROC_obj <- roc.curve(scores.class0 = pred_on_test_vs_trained$pred_on_test, weights.class0=pred_on_test_vs_trained$loan_stat_binary,
                       curve=TRUE)
plot(PRROC_obj)
```

![](Hadrien_Pistre_Assessment_2_R_Code_bis_files/figure-markdown_github/sensitivity%20trained%20on%20test-4.png)

### c. Let’s stay with the validation dataset (which has 4,000 loans). Let’s build a \[…\] & d. Investigate how sensitive is the performance of your \[…\]

``` r
#select all the terms used to compute the new return
test_loan_excerpt <- test[c("term_months","loan_amnt","installment","loan_stat_binary")]

#compute the return per loan if no default
test_loan_excerpt$return <- ((test_loan_excerpt$installment * test_loan_excerpt$term_months)/test_loan_excerpt$loan_amnt)-1

#as in the two chunks above
pred_on_test_new <- predict(logistic_reg_trained, newdata = test, type = "response")
pred_on_test_new_df <- data.frame(pred_on_test_new)

#create a first function to speed up the comparison, x being the cut-off and y the loss in terms of return (note: z is removed since the return is determined by the formula created above)
pred_on_test_new_function <- function(x,y){

  pred_on_test_new <- ifelse(pred_on_test_new_df > x, 1, 0)

  pred_on_test_vs_trained_new <- cbind(test_loan_excerpt,pred_on_test_new)
  pred_on_test_vs_trained_new$roi <- ifelse(pred_on_test_vs_trained_new$loan_stat_binary == 1 & pred_on_test_vs_trained_new$pred_on_test_new == 1,0,
                                           ifelse(pred_on_test_vs_trained_new$loan_stat_binary == 0 & pred_on_test_vs_trained_new$pred_on_test_new == 1,0,
                                                  ifelse(pred_on_test_vs_trained_new$loan_stat_binary == 1 & pred_on_test_vs_trained_new$pred_on_test_new == 0,y,
                                                         ifelse(pred_on_test_vs_trained_new$loan_stat_binary == 0 & pred_on_test_vs_trained_new$pred_on_test_new == 0,pred_on_test_vs_trained_new$return,pred_on_test_vs_trained_new$return))))#now using the formula for returns for loans we invested in that did not default
  
  #clean the data                                       
  pred_on_test_vs_trained_new_clean <- na.omit(pred_on_test_vs_trained_new)

  #we invest one dollar in each loan, so the total return is simply going to be the sum of the returns
  pnl <- sum(pred_on_test_vs_trained_new_clean$roi)

  print(paste(pnl))
}

#rest is as in the two chunk above
graph_trained_on_test_function <- function(x){

  pnl_05 <- pred_on_test_new_function(0.05,x)
  pnl_10 <- pred_on_test_new_function(0.1,x)
  pnl_15 <- pred_on_test_new_function(0.15,x)
  pnl_20 <- pred_on_test_new_function(0.2,x)
  pnl_25 <- pred_on_test_new_function(0.25,x)
  pnl_30 <- pred_on_test_new_function(0.3,x)
  pnl_35 <- pred_on_test_new_function(0.35,x)
  pnl_40 <- pred_on_test_new_function(0.40,x)
  pnl_45 <- pred_on_test_new_function(0.45,x)
  pnl_50 <- pred_on_test_new_function(0.50,x)
  pnl_55 <- pred_on_test_new_function(0.55,x)
  pnl_60 <- pred_on_test_new_function(0.60,x)
  pnl_65 <- pred_on_test_new_function(0.65,x)
  pnl_70 <- pred_on_test_new_function(0.70,x)
  pnl_75 <- pred_on_test_new_function(0.75,x)
  pnl_80 <- pred_on_test_new_function(0.80,x)
  pnl_85 <- pred_on_test_new_function(0.85,x)
  pnl_90 <- pred_on_test_new_function(0.90,x)
  pnl_95 <- pred_on_test_new_function(0.95,x)
  pnl_100 <- pred_on_test_new_function(1,x)

  pred_on_test_new_sensitivity <- data.frame(cut_off = c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1),
                                            pnl = c(pnl_05,pnl_10,pnl_15,pnl_20,pnl_25,pnl_30,pnl_35,pnl_40,pnl_45,pnl_50,pnl_55,pnl_60,pnl_65,pnl_70,pnl_75,pnl_80,pnl_85,pnl_90,pnl_95,pnl_100))

  pred_on_test_new_sensitivity$cut_off <- as.numeric(pred_on_test_new_sensitivity$cut_off)
  pred_on_test_new_sensitivity$pnl <- as.numeric(pred_on_test_new_sensitivity$pnl)
  
  print(pred_on_test_new_sensitivity)

  ggplot(pred_on_test_new_sensitivity, aes(x=cut_off,y=pnl,group = 1))+
    geom_line(alpha=1) +
    theme_bw() +
    labs (
      title = paste("Sensitivity of P&L to cut-off using the trained model on the testing dataset\nwith loss =", x),
      x     = "Cut-off",
      y     = "P&L")
}

#print the output graphs for different loss rate by 0.25 increments
graph_trained_on_test_function(-0)
```

    ## [1] "31.3536895003583"
    ## [1] "173.963502335206"
    ## [1] "356.444671178169"
    ## [1] "499.36013951453"
    ## [1] "595.989420026714"
    ## [1] "664.757597502339"
    ## [1] "698.454058605267"
    ## [1] "709.744204066473"
    ## [1] "715.50069210849"
    ## [1] "718.275892517866"
    ## [1] "719.515927987935"
    ## [1] "719.515927987935"
    ## [1] "719.515927987935"
    ## [1] "719.515927987935"
    ## [1] "719.515927987935"
    ## [1] "719.515927987935"
    ## [1] "719.515927987935"
    ## [1] "719.515927987935"
    ## [1] "719.515927987935"
    ## [1] "719.515927987935"
    ##    cut_off       pnl
    ## 1     0.05  31.35369
    ## 2     0.10 173.96350
    ## 3     0.15 356.44467
    ## 4     0.20 499.36014
    ## 5     0.25 595.98942
    ## 6     0.30 664.75760
    ## 7     0.35 698.45406
    ## 8     0.40 709.74420
    ## 9     0.45 715.50069
    ## 10    0.50 718.27589
    ## 11    0.55 719.51593
    ## 12    0.60 719.51593
    ## 13    0.65 719.51593
    ## 14    0.70 719.51593
    ## 15    0.75 719.51593
    ## 16    0.80 719.51593
    ## 17    0.85 719.51593
    ## 18    0.90 719.51593
    ## 19    0.95 719.51593
    ## 20    1.00 719.51593

![](Hadrien_Pistre_Assessment_2_R_Code_bis_files/figure-markdown_github/sensivity%20trained%20on%20test%20with%20realistic%20return-1.png)

``` r
graph_trained_on_test_function(-0.25)
```

    ## [1] "28.8536895003583"
    ## [1] "148.463502335206"
    ## [1] "301.694671178169"
    ## [1] "415.86013951453"
    ## [1] "489.989420026714"
    ## [1] "545.007597502339"
    ## [1] "570.204058605267"
    ## [1] "578.994204066473"
    ## [1] "581.75069210849"
    ## [1] "583.525892517866"
    ## [1] "584.015927987935"
    ## [1] "584.015927987935"
    ## [1] "584.015927987935"
    ## [1] "584.015927987935"
    ## [1] "584.015927987935"
    ## [1] "584.015927987935"
    ## [1] "584.015927987935"
    ## [1] "584.015927987935"
    ## [1] "584.015927987935"
    ## [1] "584.015927987935"
    ##    cut_off       pnl
    ## 1     0.05  28.85369
    ## 2     0.10 148.46350
    ## 3     0.15 301.69467
    ## 4     0.20 415.86014
    ## 5     0.25 489.98942
    ## 6     0.30 545.00760
    ## 7     0.35 570.20406
    ## 8     0.40 578.99420
    ## 9     0.45 581.75069
    ## 10    0.50 583.52589
    ## 11    0.55 584.01593
    ## 12    0.60 584.01593
    ## 13    0.65 584.01593
    ## 14    0.70 584.01593
    ## 15    0.75 584.01593
    ## 16    0.80 584.01593
    ## 17    0.85 584.01593
    ## 18    0.90 584.01593
    ## 19    0.95 584.01593
    ## 20    1.00 584.01593

![](Hadrien_Pistre_Assessment_2_R_Code_bis_files/figure-markdown_github/sensivity%20trained%20on%20test%20with%20realistic%20return-2.png)

``` r
graph_trained_on_test_function(-0.5)
```

    ## [1] "26.3536895003583"
    ## [1] "122.963502335206"
    ## [1] "246.944671178169"
    ## [1] "332.36013951453"
    ## [1] "383.989420026714"
    ## [1] "425.257597502339"
    ## [1] "441.954058605267"
    ## [1] "448.244204066473"
    ## [1] "448.00069210849"
    ## [1] "448.775892517866"
    ## [1] "448.515927987935"
    ## [1] "448.515927987935"
    ## [1] "448.515927987935"
    ## [1] "448.515927987935"
    ## [1] "448.515927987935"
    ## [1] "448.515927987935"
    ## [1] "448.515927987935"
    ## [1] "448.515927987935"
    ## [1] "448.515927987935"
    ## [1] "448.515927987935"
    ##    cut_off       pnl
    ## 1     0.05  26.35369
    ## 2     0.10 122.96350
    ## 3     0.15 246.94467
    ## 4     0.20 332.36014
    ## 5     0.25 383.98942
    ## 6     0.30 425.25760
    ## 7     0.35 441.95406
    ## 8     0.40 448.24420
    ## 9     0.45 448.00069
    ## 10    0.50 448.77589
    ## 11    0.55 448.51593
    ## 12    0.60 448.51593
    ## 13    0.65 448.51593
    ## 14    0.70 448.51593
    ## 15    0.75 448.51593
    ## 16    0.80 448.51593
    ## 17    0.85 448.51593
    ## 18    0.90 448.51593
    ## 19    0.95 448.51593
    ## 20    1.00 448.51593

![](Hadrien_Pistre_Assessment_2_R_Code_bis_files/figure-markdown_github/sensivity%20trained%20on%20test%20with%20realistic%20return-3.png)

``` r
graph_trained_on_test_function(-0.75)
```

    ## [1] "23.8536895003583"
    ## [1] "97.4635023352063"
    ## [1] "192.194671178169"
    ## [1] "248.86013951453"
    ## [1] "277.989420026714"
    ## [1] "305.507597502339"
    ## [1] "313.704058605267"
    ## [1] "317.494204066473"
    ## [1] "314.25069210849"
    ## [1] "314.025892517866"
    ## [1] "313.015927987935"
    ## [1] "313.015927987935"
    ## [1] "313.015927987935"
    ## [1] "313.015927987935"
    ## [1] "313.015927987935"
    ## [1] "313.015927987935"
    ## [1] "313.015927987935"
    ## [1] "313.015927987935"
    ## [1] "313.015927987935"
    ## [1] "313.015927987935"
    ##    cut_off       pnl
    ## 1     0.05  23.85369
    ## 2     0.10  97.46350
    ## 3     0.15 192.19467
    ## 4     0.20 248.86014
    ## 5     0.25 277.98942
    ## 6     0.30 305.50760
    ## 7     0.35 313.70406
    ## 8     0.40 317.49420
    ## 9     0.45 314.25069
    ## 10    0.50 314.02589
    ## 11    0.55 313.01593
    ## 12    0.60 313.01593
    ## 13    0.65 313.01593
    ## 14    0.70 313.01593
    ## 15    0.75 313.01593
    ## 16    0.80 313.01593
    ## 17    0.85 313.01593
    ## 18    0.90 313.01593
    ## 19    0.95 313.01593
    ## 20    1.00 313.01593

![](Hadrien_Pistre_Assessment_2_R_Code_bis_files/figure-markdown_github/sensivity%20trained%20on%20test%20with%20realistic%20return-4.png)

``` r
graph_trained_on_test_function(-1)
```

    ## [1] "21.3536895003583"
    ## [1] "71.9635023352063"
    ## [1] "137.444671178169"
    ## [1] "165.36013951453"
    ## [1] "171.989420026714"
    ## [1] "185.757597502339"
    ## [1] "185.454058605267"
    ## [1] "186.744204066473"
    ## [1] "180.50069210849"
    ## [1] "179.275892517866"
    ## [1] "177.515927987935"
    ## [1] "177.515927987935"
    ## [1] "177.515927987935"
    ## [1] "177.515927987935"
    ## [1] "177.515927987935"
    ## [1] "177.515927987935"
    ## [1] "177.515927987935"
    ## [1] "177.515927987935"
    ## [1] "177.515927987935"
    ## [1] "177.515927987935"
    ##    cut_off       pnl
    ## 1     0.05  21.35369
    ## 2     0.10  71.96350
    ## 3     0.15 137.44467
    ## 4     0.20 165.36014
    ## 5     0.25 171.98942
    ## 6     0.30 185.75760
    ## 7     0.35 185.45406
    ## 8     0.40 186.74420
    ## 9     0.45 180.50069
    ## 10    0.50 179.27589
    ## 11    0.55 177.51593
    ## 12    0.60 177.51593
    ## 13    0.65 177.51593
    ## 14    0.70 177.51593
    ## 15    0.75 177.51593
    ## 16    0.80 177.51593
    ## 17    0.85 177.51593
    ## 18    0.90 177.51593
    ## 19    0.95 177.51593
    ## 20    1.00 177.51593

![](Hadrien_Pistre_Assessment_2_R_Code_bis_files/figure-markdown_github/sensivity%20trained%20on%20test%20with%20realistic%20return-5.png)

# Assessment 2

## 1. Format the assessment 2 dataset

``` r
#read the assessment data
assessment_data <- read_csv((here::here("assessment_raw_data.csv")))

#recode the categorical variables as we did for the workshop dataset
assessment_data$emp_length <- recode(assessment_data$emp_length, '< 1 year' = '< 2 years', '1 year' = '< 2 years', '2 years' = '2-5 years', '3 years' = '2-5 years', '4 years' = '2-5 years', '5 years' = '2-5 years', '6 years' = '> 5 years','7 years' = '> 5 years','8 years' = '> 5 years','9 years' = '> 5 years','10+ years' = '> 5 years')
assessment_data$delinq_2yrs <- recode(assessment_data$delinq_2yrs, '0' = '< 2', '1' = '< 2', '2' = '2-5', '3' = '2-5', '4' = '2-5', '5' = '2-5', '6' = '> 5','7' = '> 5','8' = '> 5','9' = '> 5')
assessment_data$verification_status <- recode(assessment_data$verification_status, 'Source Verified' = 'Verified', 'Verified' = 'Verified', 'Not Verified' = 'Not Verified')

#create the same dummies as in the workshop dataset
assessment_data_dummied <- dummy_cols(assessment_data, select_columns = c('term (months)','grade','emp_length','home_ownership','verification_status'))

#delete the useless dummies
drops <- c("term (months)_60","term (months)_NA","grade_G","grade_NA","emp_length_> 5 years","emp_length_n/a","emp_length_NA","home_ownership_OTHER","home_ownership_NA","verification_status_Verified","verification_status_NA")
assessment_data_dummied_cleaned <- assessment_data_dummied[ , !(names(assessment_data_dummied) %in% drops)]
assessment_data_dummied_cleaned_names <- clean_names(assessment_data_dummied_cleaned)

#add our interaction terms of low grades * home ownership
assessment_data_dummied_cleaned_names$home_ownsh_x_mort_E <- assessment_data_dummied_cleaned_names$home_ownership_mortgage * assessment_data_dummied_cleaned_names$grade_e
assessment_data_dummied_cleaned_names$home_ownsh_x_own_E <- assessment_data_dummied_cleaned_names$home_ownership_own * assessment_data_dummied_cleaned_names$grade_e
assessment_data_dummied_cleaned_names$home_ownsh_x_rent_E <- assessment_data_dummied_cleaned_names$home_ownership_rent * assessment_data_dummied_cleaned_names$grade_e
assessment_data_dummied_cleaned_names$home_ownsh_x_mort_F <- assessment_data_dummied_cleaned_names$home_ownership_mortgage * assessment_data_dummied_cleaned_names$grade_f
assessment_data_dummied_cleaned_names$home_ownsh_x_own_F <- assessment_data_dummied_cleaned_names$home_ownership_own * assessment_data_dummied_cleaned_names$grade_f
assessment_data_dummied_cleaned_names$home_ownsh_x_rent_F <- assessment_data_dummied_cleaned_names$home_ownership_rent * assessment_data_dummied_cleaned_names$grade_f

#add our interaction term of loan term * loan amount
assessment_data_dummied_cleaned_names$loan_amnt_x_term_36 <- assessment_data_dummied_cleaned_names$loan_amnt * assessment_data_dummied_cleaned_names$term_months_36

colnames(assessment_data_dummied_cleaned_names)
```

    ##  [1] "loan_number"                      "int_rate"                        
    ##  [3] "loan_amnt"                        "term_months"                     
    ##  [5] "installment"                      "dti"                             
    ##  [7] "delinq_2yrs"                      "annual_inc"                      
    ##  [9] "grade"                            "emp_title"                       
    ## [11] "emp_length"                       "home_ownership"                  
    ## [13] "verification_status"              "issue_d"                         
    ## [15] "invest"                           "term_months_36"                  
    ## [17] "grade_a"                          "grade_b"                         
    ## [19] "grade_c"                          "grade_d"                         
    ## [21] "grade_e"                          "grade_f"                         
    ## [23] "emp_length_2_years"               "emp_length_2_5_years"            
    ## [25] "home_ownership_mortgage"          "home_ownership_own"              
    ## [27] "home_ownership_rent"              "verification_status_not_verified"
    ## [29] "home_ownsh_x_mort_E"              "home_ownsh_x_own_E"              
    ## [31] "home_ownsh_x_rent_E"              "home_ownsh_x_mort_F"             
    ## [33] "home_ownsh_x_own_F"               "home_ownsh_x_rent_F"             
    ## [35] "loan_amnt_x_term_36"

## 2. Fit our best model on the formatted assessment 2 data set

``` r
#train our latest model on the lending club sample used in the workshop (note: we did not use the purpose variables since informatiom about purposes was not available)
logistic_reg_assessment_data <- glm(loan_stat_binary ~ 
                        loan_amnt + 
                        installment + 
                        dti +
                        annual_inc +
                        term_months_36 + 
                        grade_a + grade_b + grade_c + grade_d + grade_e + grade_f + 
                        emp_length_2_years + emp_length_2_5_years + 
                        home_ownership_mortgage + home_ownership_own + home_ownership_rent + 
                        verification_status_not_verified + 
                        home_ownsh_x_mort_E + home_ownsh_x_own_E + home_ownsh_x_rent_E +
                        home_ownsh_x_mort_F + home_ownsh_x_own_F +
                        loan_amnt_x_term_36,
                        family = binomial,
                        data = lendingclub_sample_dummied_cleaned_names)
summary(logistic_reg_assessment_data)
```

    ## 
    ## Call:
    ## glm(formula = loan_stat_binary ~ loan_amnt + installment + dti + 
    ##     annual_inc + term_months_36 + grade_a + grade_b + grade_c + 
    ##     grade_d + grade_e + grade_f + emp_length_2_years + emp_length_2_5_years + 
    ##     home_ownership_mortgage + home_ownership_own + home_ownership_rent + 
    ##     verification_status_not_verified + home_ownsh_x_mort_E + 
    ##     home_ownsh_x_own_E + home_ownsh_x_rent_E + home_ownsh_x_mort_F + 
    ##     home_ownsh_x_own_F + loan_amnt_x_term_36, family = binomial, 
    ##     data = lendingclub_sample_dummied_cleaned_names)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.1884  -0.5988  -0.4870  -0.3520   3.3951  
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                      -4.001e-01  6.320e-01  -0.633 0.526673    
    ## loan_amnt                        -1.671e-04  5.423e-05  -3.081 0.002065 ** 
    ## installment                       7.294e-03  2.193e-03   3.326 0.000881 ***
    ## dti                               6.544e-03  4.761e-03   1.375 0.169243    
    ## annual_inc                       -5.560e-06  9.994e-07  -5.563 2.65e-08 ***
    ## term_months_36                   -5.162e-01  1.406e-01  -3.671 0.000242 ***
    ## grade_a                          -1.074e+00  3.381e-01  -3.177 0.001487 ** 
    ## grade_b                          -5.346e-01  3.164e-01  -1.690 0.091056 .  
    ## grade_c                          -2.585e-01  2.995e-01  -0.863 0.388000    
    ## grade_d                          -1.394e-01  2.832e-01  -0.492 0.622598    
    ## grade_e                          -1.053e+01  1.970e+02  -0.053 0.957366    
    ## grade_f                           1.370e-01  3.225e-01   0.425 0.670889    
    ## emp_length_2_years               -1.252e-01  8.698e-02  -1.439 0.150076    
    ## emp_length_2_5_years             -2.407e-01  7.131e-02  -3.376 0.000735 ***
    ## home_ownership_mortgage          -3.291e-01  5.543e-01  -0.594 0.552679    
    ## home_ownership_own               -2.388e-01  5.632e-01  -0.424 0.671530    
    ## home_ownership_rent              -3.443e-01  5.534e-01  -0.622 0.533851    
    ## verification_status_not_verified  1.824e-02  6.898e-02   0.264 0.791426    
    ## home_ownsh_x_mort_E               1.030e+01  1.970e+02   0.052 0.958297    
    ## home_ownsh_x_own_E                1.073e+01  1.970e+02   0.054 0.956550    
    ## home_ownsh_x_rent_E               1.042e+01  1.970e+02   0.053 0.957817    
    ## home_ownsh_x_mort_F              -3.453e-01  3.236e-01  -1.067 0.286001    
    ## home_ownsh_x_own_F               -7.849e-01  6.992e-01  -1.122 0.261651    
    ## loan_amnt_x_term_36              -6.391e-05  2.133e-05  -2.996 0.002735 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 7358.8  on 8932  degrees of freedom
    ## Residual deviance: 6938.4  on 8909  degrees of freedom
    ##   (1067 observations deleted due to missingness)
    ## AIC: 6986.4
    ## 
    ## Number of Fisher Scoring iterations: 10

``` r
#use this model to predict the default probabilities on the assessment data set and store in a df
pred_logistic_reg_assessment_data <- predict(logistic_reg_assessment_data, newdata = assessment_data_dummied_cleaned_names, type = "response")
pred_logistic_reg_assessment_data_df <- data.frame(pred_logistic_reg_assessment_data)

#add a column in which the return per loan if no default is computed
assessment_data_dummied_cleaned_names_excerpt <- assessment_data_dummied_cleaned_names[c("term_months","loan_amnt","installment")]
assessment_data_dummied_cleaned_names_excerpt$return_no_def <- ((assessment_data_dummied_cleaned_names$installment * assessment_data_dummied_cleaned_names$term_months)/assessment_data_dummied_cleaned_names$loan_amnt)-1

#create a df with the loan number, the predicted default probability and the return if no default
pred_logistic_reg_assessment_data_df_complete <- cbind(assessment_data_dummied_cleaned_names$loan_number,pred_logistic_reg_assessment_data_df,assessment_data_dummied_cleaned_names_excerpt$return_no_def)

#rename the columns with shorter names
names(pred_logistic_reg_assessment_data_df_complete)[1] <- "loan_nb"
names(pred_logistic_reg_assessment_data_df_complete)[2] <- "predicted_default_prob"
names(pred_logistic_reg_assessment_data_df_complete)[3] <- "return_no_default"

#plot the graph of return if no default vs default probability to assess the consistency of our model visually (we would expect higher default probability as the return increases)
ggplot(pred_logistic_reg_assessment_data_df_complete, aes(x=predicted_default_prob,y=return_no_default,group = 1))+
  geom_point(alpha=1) +
  theme_bw() +
  scale_y_continuous()+
  labs (
    title = paste("Probability of default VS return if no default"),
    x     = "Probability of default",
    y     = "Return if no default") #changing y-axis label to sentence case
```

![](Hadrien_Pistre_Assessment_2_R_Code_bis_files/figure-markdown_github/using%20best%20model%20on%20assessment%20data-1.png)

## 3. Use our best model to compute the P&L on the assessment 2 dataset

``` r
#create a column with binary predicted default values based on the cut7-off
pred_logistic_reg_assessment_data_df_complete$predicted_default_binary <- ifelse(pred_logistic_reg_assessment_data_df_complete$predicted_default_prob > 0.2, 1, 0)

#create a pnl column replacing the binary default values by their pnl
pred_logistic_reg_assessment_data_df_complete$pnl <- ifelse(pred_logistic_reg_assessment_data_df_complete$predicted_default_binary == 1,-0.5,pred_logistic_reg_assessment_data_df_complete$return_no_default)

#clean the obtained dataframe                                                  
pred_logistic_reg_assessment_data_df_complete <- na.omit(pred_logistic_reg_assessment_data_df_complete)

#create a new column with another selection criterion to select the loans: the inverse of the default probability * the return -> hence the loans with the highest score will be those with the lowest default probability and the highest return
pred_logistic_reg_assessment_data_df_complete$criterion <- (1/pred_logistic_reg_assessment_data_df_complete$predicted_default_prob) * (pred_logistic_reg_assessment_data_df_complete$return_no_default)
  
#arrange the table by pnl using the predicted returns by our model
pred_logistic_reg_assessment_data_df_complete_ordered_by_pnl <- arrange(pred_logistic_reg_assessment_data_df_complete,desc(pnl))
  
#print the first 200 loans by pnl which would be the ones we would invest in
top_200_pnl <- head(pred_logistic_reg_assessment_data_df_complete_ordered_by_pnl,200)
  by_pnl_total_pnl <- sum(top_200_pnl$pnl)
  by_pnl_total_pnl
```

    ## [1] 65.81254

``` r
print(top_200_pnl)
```

    ##      loan_nb predicted_default_prob return_no_default predicted_default_binary
    ## 1334    1334           9.811025e-02         0.6233333                        0
    ## 715      715           1.146008e-01         0.6117647                        0
    ## 208      208           1.844449e-01         0.5200000                        0
    ## 1647    1647           1.897485e-01         0.5094737                        0
    ## 65        65           1.952988e-02         0.5000000                        0
    ## 695      695           1.991824e-01         0.4979310                        0
    ## 146      146           1.894430e-01         0.4880000                        0
    ## 259      259           1.577174e-01         0.4863636                        0
    ## 1420    1420           1.976671e-01         0.4600000                        0
    ## 447      447           1.842895e-01         0.4538462                        0
    ## 480      480           1.622697e-01         0.4447368                        0
    ## 691      691           1.814927e-01         0.4357143                        0
    ## 465      465           1.661674e-01         0.4352000                        0
    ## 378      378           1.641246e-01         0.4350000                        0
    ## 1158    1158           1.894261e-01         0.4335878                        0
    ## 1738    1738           1.666374e-01         0.4333333                        0
    ## 769      769           1.939421e-01         0.4300000                        0
    ## 739      739           1.389896e-01         0.4200000                        0
    ## 20        20           1.968024e-01         0.4160000                        0
    ## 1091    1091           1.933554e-01         0.4040000                        0
    ## 981      981           1.913866e-01         0.4000000                        0
    ## 263      263           1.246546e-01         0.3944000                        0
    ## 1153    1153           1.767154e-01         0.3920000                        0
    ## 418      418           1.122390e-01         0.3896000                        0
    ## 266      266           9.471932e-02         0.3848000                        0
    ## 332      332           1.964515e-01         0.3800000                        0
    ## 410      410           1.979288e-01         0.3800000                        0
    ## 729      729           1.652905e-01         0.3800000                        0
    ## 1148    1148           2.870557e-02         0.3800000                        0
    ## 1567    1567           1.815084e-01         0.3800000                        0
    ## 1514    1514           1.765066e-01         0.3776000                        0
    ## 442      442           1.807551e-01         0.3752000                        0
    ## 467      467           1.696107e-01         0.3728000                        0
    ## 1220    1220           1.593721e-01         0.3728000                        0
    ## 889      889           1.992637e-01         0.3725000                        0
    ## 168      168           1.776697e-01         0.3656000                        0
    ## 1598    1598           1.815368e-01         0.3653061                        0
    ## 867      867           1.928473e-01         0.3645714                        0
    ## 1500    1500           1.617550e-01         0.3640000                        0
    ## 1678    1678           1.921022e-01         0.3569231                        0
    ## 10        10           1.731908e-01         0.3565217                        0
    ## 341      341           9.369636e-02         0.3560000                        0
    ## 123      123           1.618640e-01         0.3550000                        0
    ## 674      674           1.596614e-01         0.3550000                        0
    ## 1092    1092           1.651123e-01         0.3550000                        0
    ## 1281    1281           1.998183e-01         0.3550000                        0
    ## 7          7           1.633668e-01         0.3545455                        0
    ## 1113    1113           1.846110e-01         0.3542857                        0
    ## 416      416           1.703257e-01         0.3538462                        0
    ## 767      767           1.878971e-01         0.3521951                        0
    ## 354      354           1.400131e-01         0.3480000                        0
    ## 929      929           9.564535e-02         0.3474286                        0
    ## 1295    1295           1.501490e-01         0.3470000                        0
    ## 118      118           1.551894e-01         0.3450000                        0
    ## 1105    1105           1.568894e-01         0.3432836                        0
    ## 714      714           1.749099e-01         0.3360825                        0
    ## 1350    1350           1.360102e-01         0.3350000                        0
    ## 717      717           1.365162e-01         0.3336634                        0
    ## 179      179           1.431628e-01         0.3328571                        0
    ## 30        30           1.894012e-01         0.3320000                        0
    ## 1660    1660           1.808530e-01         0.3312500                        0
    ## 256      256           1.571819e-01         0.3300000                        0
    ## 1193    1193           1.819292e-01         0.3300000                        0
    ## 1024    1024           1.054128e-01         0.3296000                        0
    ## 1646    1646           1.387638e-01         0.3296000                        0
    ## 1419    1419           1.766946e-01         0.3292308                        0
    ## 1761    1761           1.771138e-01         0.3283721                        0
    ## 832      832           1.428059e-01         0.3275000                        0
    ## 840      840           1.752516e-01         0.3272727                        0
    ## 22        22           1.913718e-01         0.3270588                        0
    ## 117      117           1.636381e-01         0.3270588                        0
    ## 1562    1562           1.873405e-01         0.3268571                        0
    ## 364      364           1.272482e-01         0.3260274                        0
    ## 1093    1093           1.846787e-01         0.3260000                        0
    ## 1491    1491           1.857206e-01         0.3260000                        0
    ## 1551    1551           1.691694e-01         0.3260000                        0
    ## 1304    1304           1.480190e-01         0.3253731                        0
    ## 353      353           7.212383e-02         0.3251429                        0
    ## 1560    1560           1.017989e-01         0.3251429                        0
    ## 356      356           9.124192e-02         0.3248000                        0
    ## 924      924           1.365742e-01         0.3248000                        0
    ## 363      363           1.365392e-01         0.3241379                        0
    ## 1557    1557           1.869922e-01         0.3240000                        0
    ## 512      512           1.571551e-01         0.3221818                        0
    ## 1118    1118           1.837079e-01         0.3202864                        0
    ## 861      861           1.501183e-01         0.3202186                        0
    ## 35        35           1.525023e-01         0.3200000                        0
    ## 615      615           1.428943e-01         0.3200000                        0
    ## 709      709           1.441255e-01         0.3200000                        0
    ## 791      791           1.553962e-01         0.3200000                        0
    ## 948      948           1.484723e-01         0.3200000                        0
    ## 1035    1035           1.457109e-01         0.3200000                        0
    ## 1136    1136           1.318654e-01         0.3200000                        0
    ## 1312    1312           1.435644e-01         0.3200000                        0
    ## 1353    1353           1.459654e-01         0.3200000                        0
    ## 1580    1580           1.649564e-01         0.3200000                        0
    ## 1762    1762           9.191878e-02         0.3200000                        0
    ## 454      454           1.221889e-01         0.3187500                        0
    ## 1664    1664           1.542755e-01         0.3185185                        0
    ## 1349    1349           1.551041e-01         0.3183752                        0
    ## 971      971           1.299635e-01         0.3183673                        0
    ## 679      679           1.662239e-01         0.3176000                        0
    ## 265      265           1.705900e-01         0.3166667                        0
    ## 1523    1523           1.833084e-01         0.3152000                        0
    ## 213      213           1.328084e-01         0.3100000                        0
    ## 1379    1379           1.608314e-01         0.3100000                        0
    ## 571      571           1.607163e-01         0.3090909                        0
    ## 1155    1155           1.692826e-01         0.3090909                        0
    ## 610      610           1.550467e-01         0.3087500                        0
    ## 875      875           1.541386e-01         0.3087500                        0
    ## 1667    1667           1.602927e-01         0.3083333                        0
    ## 1219    1219           1.438534e-01         0.3080000                        0
    ## 1406    1406           1.668056e-01         0.3080000                        0
    ## 1617    1617           1.883524e-01         0.3069959                        0
    ## 1023    1023           1.465130e-01         0.3061538                        0
    ## 144      144           1.647616e-01         0.3051095                        0
    ## 719      719           1.249214e-01         0.3050000                        0
    ## 1214    1214           1.757353e-01         0.3050000                        0
    ## 1280    1280           1.983491e-01         0.3050000                        0
    ## 851      851           1.259161e-01         0.3045714                        0
    ## 1123    1123           1.020518e-01         0.3028571                        0
    ## 976      976           1.707658e-01         0.3026316                        0
    ## 386      386           1.714132e-01         0.3025455                        0
    ## 958      958           1.894234e-01         0.3020000                        0
    ## 970      970           1.989263e-01         0.3020000                        0
    ## 216      216           1.902233e-01         0.3000000                        0
    ## 486      486           1.251115e-01         0.3000000                        0
    ## 988      988           1.220587e-01         0.3000000                        0
    ## 1409    1409           1.404708e-01         0.2990000                        0
    ## 872      872           1.255254e-01         0.2978947                        0
    ## 419      419           1.214449e-01         0.2970350                        0
    ## 270      270           1.428547e-01         0.2966667                        0
    ## 209      209           1.670071e-01         0.2960000                        0
    ## 420      420           1.250261e-01         0.2960000                        0
    ## 611      611           1.445177e-01         0.2960000                        0
    ## 1020    1020           1.469369e-01         0.2960000                        0
    ## 1446    1446           1.303914e-01         0.2960000                        0
    ## 1522    1522           1.934182e-01         0.2960000                        0
    ## 1134    1134           1.862508e-01         0.2948229                        0
    ## 658      658           1.971727e-01         0.2940541                        0
    ## 196      196           1.664272e-01         0.2937500                        0
    ## 112      112           1.788493e-01         0.2936000                        0
    ## 1224    1224           6.781218e-11         0.2936000                        0
    ## 119      119           1.687344e-01         0.2935733                        0
    ## 433      433           1.781446e-01         0.2934286                        0
    ## 734      734           1.475698e-01         0.2933333                        0
    ## 917      917           7.409571e-02         0.2931034                        0
    ## 1223    1223           1.396149e-01         0.2930000                        0
    ## 946      946           1.263214e-01         0.2920000                        0
    ## 407      407           1.272552e-01         0.2912000                        0
    ## 1407    1407           1.327685e-01         0.2900000                        0
    ## 1421    1421           1.399349e-01         0.2900000                        0
    ## 1569    1569           1.965605e-01         0.2900000                        0
    ## 1423    1423           1.789118e-01         0.2895522                        0
    ## 1326    1326           1.377248e-01         0.2888000                        0
    ## 1200    1200           1.669528e-01         0.2884211                        0
    ## 629      629           1.406682e-01         0.2875294                        0
    ## 777      777           1.631921e-01         0.2866667                        0
    ## 1205    1205           1.777651e-01         0.2864000                        0
    ## 987      987           1.496812e-01         0.2850000                        0
    ## 790      790           1.360764e-01         0.2840000                        0
    ## 1073    1073           1.797722e-01         0.2840000                        0
    ## 1364    1364           1.430887e-01         0.2840000                        0
    ## 1383    1383           1.490984e-01         0.2840000                        0
    ## 255      255           1.631244e-01         0.2825000                        0
    ## 128      128           1.440341e-01         0.2816000                        0
    ## 606      606           1.881576e-01         0.2816000                        0
    ## 1708    1708           1.697917e-01         0.2816000                        0
    ## 163      163           1.862230e-01         0.2780000                        0
    ## 803      803           1.691556e-01         0.2780000                        0
    ## 1277    1277           1.923124e-01         0.2763636                        0
    ## 567      567           1.930009e-01         0.2760000                        0
    ## 1518    1518           1.344557e-01         0.2750000                        0
    ## 1571    1571           1.702413e-01         0.2750000                        0
    ## 1632    1632           1.592597e-01         0.2750000                        0
    ## 148      148           1.293238e-01         0.2744828                        0
    ## 1132    1132           1.751293e-01         0.2744000                        0
    ## 1465    1465           1.762112e-01         0.2744000                        0
    ## 726      726           1.739968e-01         0.2735000                        0
    ## 1229    1229           2.700716e-02         0.2727742                        0
    ## 34        34           1.720924e-01         0.2720000                        0
    ## 554      554           1.494199e-01         0.2720000                        0
    ## 1110    1110           1.916597e-01         0.2720000                        0
    ## 1112    1112           1.721811e-01         0.2720000                        0
    ## 1763    1763           1.594846e-01         0.2720000                        0
    ## 1076    1076           1.922237e-01         0.2710769                        0
    ## 678      678           1.070261e-01         0.2700000                        0
    ## 754      754           1.873819e-01         0.2700000                        0
    ## 1163    1163           1.678185e-01         0.2700000                        0
    ## 1733    1733           1.481143e-01         0.2700000                        0
    ## 692      692           1.719765e-01         0.2692308                        0
    ## 111      111           1.802151e-01         0.2690000                        0
    ## 700      700           9.393043e-02         0.2690000                        0
    ## 494      494           1.642799e-01         0.2672000                        0
    ## 569      569           1.243638e-01         0.2672000                        0
    ## 737      737           1.799053e-01         0.2672000                        0
    ## 1714    1714           1.250048e-01         0.2672000                        0
    ## 1434    1434           1.969252e-01         0.2667500                        0
    ## 738      738           1.731101e-01         0.2666667                        0
    ## 1731    1731           1.709167e-01         0.2660000                        0
    ##            pnl    criterion
    ## 1334 0.6233333 6.353396e+00
    ## 715  0.6117647 5.338224e+00
    ## 208  0.5200000 2.819271e+00
    ## 1647 0.5094737 2.684995e+00
    ## 65   0.5000000 2.560179e+01
    ## 695  0.4979310 2.499875e+00
    ## 146  0.4880000 2.575972e+00
    ## 259  0.4863636 3.083767e+00
    ## 1420 0.4600000 2.327146e+00
    ## 447  0.4538462 2.462681e+00
    ## 480  0.4447368 2.740727e+00
    ## 691  0.4357143 2.400726e+00
    ## 465  0.4352000 2.619045e+00
    ## 378  0.4350000 2.650425e+00
    ## 1158 0.4335878 2.288955e+00
    ## 1738 0.4333333 2.600457e+00
    ## 769  0.4300000 2.217157e+00
    ## 739  0.4200000 3.021809e+00
    ## 20   0.4160000 2.113796e+00
    ## 1091 0.4040000 2.089417e+00
    ## 981  0.4000000 2.090011e+00
    ## 263  0.3944000 3.163942e+00
    ## 1153 0.3920000 2.218256e+00
    ## 418  0.3896000 3.471166e+00
    ## 266  0.3848000 4.062529e+00
    ## 332  0.3800000 1.934319e+00
    ## 410  0.3800000 1.919883e+00
    ## 729  0.3800000 2.298983e+00
    ## 1148 0.3800000 1.323785e+01
    ## 1567 0.3800000 2.093566e+00
    ## 1514 0.3776000 2.139297e+00
    ## 442  0.3752000 2.075737e+00
    ## 467  0.3728000 2.197974e+00
    ## 1220 0.3728000 2.339180e+00
    ## 889  0.3725000 1.869382e+00
    ## 168  0.3656000 2.057751e+00
    ## 1598 0.3653061 2.012298e+00
    ## 867  0.3645714 1.890467e+00
    ## 1500 0.3640000 2.250317e+00
    ## 1678 0.3569231 1.857986e+00
    ## 10   0.3565217 2.058549e+00
    ## 341  0.3560000 3.799507e+00
    ## 123  0.3550000 2.193199e+00
    ## 674  0.3550000 2.223456e+00
    ## 1092 0.3550000 2.150051e+00
    ## 1281 0.3550000 1.776614e+00
    ## 7    0.3545455 2.170242e+00
    ## 1113 0.3542857 1.919093e+00
    ## 416  0.3538462 2.077468e+00
    ## 767  0.3521951 1.874404e+00
    ## 354  0.3480000 2.485481e+00
    ## 929  0.3474286 3.632467e+00
    ## 1295 0.3470000 2.311037e+00
    ## 118  0.3450000 2.223090e+00
    ## 1105 0.3432836 2.188061e+00
    ## 714  0.3360825 1.921461e+00
    ## 1350 0.3350000 2.463051e+00
    ## 717  0.3336634 2.444131e+00
    ## 179  0.3328571 2.325026e+00
    ## 30   0.3320000 1.752892e+00
    ## 1660 0.3312500 1.831598e+00
    ## 256  0.3300000 2.099479e+00
    ## 1193 0.3300000 1.813893e+00
    ## 1024 0.3296000 3.126754e+00
    ## 1646 0.3296000 2.375260e+00
    ## 1419 0.3292308 1.863276e+00
    ## 1761 0.3283721 1.854018e+00
    ## 832  0.3275000 2.293323e+00
    ## 840  0.3272727 1.867445e+00
    ## 22   0.3270588 1.709023e+00
    ## 117  0.3270588 1.998672e+00
    ## 1562 0.3268571 1.744722e+00
    ## 364  0.3260274 2.562138e+00
    ## 1093 0.3260000 1.765228e+00
    ## 1491 0.3260000 1.755325e+00
    ## 1551 0.3260000 1.927062e+00
    ## 1304 0.3253731 2.198185e+00
    ## 353  0.3251429 4.508120e+00
    ## 1560 0.3251429 3.193973e+00
    ## 356  0.3248000 3.559767e+00
    ## 924  0.3248000 2.378195e+00
    ## 363  0.3241379 2.373955e+00
    ## 1557 0.3240000 1.732693e+00
    ## 512  0.3221818 2.050089e+00
    ## 1118 0.3202864 1.743455e+00
    ## 861  0.3202186 2.133108e+00
    ## 35   0.3200000 2.098329e+00
    ## 615  0.3200000 2.239418e+00
    ## 709  0.3200000 2.220288e+00
    ## 791  0.3200000 2.059253e+00
    ## 948  0.3200000 2.155284e+00
    ## 1035 0.3200000 2.196129e+00
    ## 1136 0.3200000 2.426717e+00
    ## 1312 0.3200000 2.228964e+00
    ## 1353 0.3200000 2.192300e+00
    ## 1580 0.3200000 1.939907e+00
    ## 1762 0.3200000 3.481334e+00
    ## 454  0.3187500 2.608667e+00
    ## 1664 0.3185185 2.064608e+00
    ## 1349 0.3183752 2.052655e+00
    ## 971  0.3183673 2.449667e+00
    ## 679  0.3176000 1.910676e+00
    ## 265  0.3166667 1.856303e+00
    ## 1523 0.3152000 1.719507e+00
    ## 213  0.3100000 2.334190e+00
    ## 1379 0.3100000 1.927484e+00
    ## 571  0.3090909 1.923208e+00
    ## 1155 0.3090909 1.825888e+00
    ## 610  0.3087500 1.991335e+00
    ## 875  0.3087500 2.003067e+00
    ## 1667 0.3083333 1.923564e+00
    ## 1219 0.3080000 2.141069e+00
    ## 1406 0.3080000 1.846461e+00
    ## 1617 0.3069959 1.629902e+00
    ## 1023 0.3061538 2.089602e+00
    ## 144  0.3051095 1.851824e+00
    ## 719  0.3050000 2.441534e+00
    ## 1214 0.3050000 1.735565e+00
    ## 1280 0.3050000 1.537693e+00
    ## 851  0.3045714 2.418844e+00
    ## 1123 0.3028571 2.967679e+00
    ## 976  0.3026316 1.772203e+00
    ## 386  0.3025455 1.765007e+00
    ## 958  0.3020000 1.594312e+00
    ## 970  0.3020000 1.518150e+00
    ## 216  0.3000000 1.577094e+00
    ## 486  0.3000000 2.397862e+00
    ## 988  0.3000000 2.457834e+00
    ## 1409 0.2990000 2.128557e+00
    ## 872  0.2978947 2.373182e+00
    ## 419  0.2970350 2.445842e+00
    ## 270  0.2966667 2.076702e+00
    ## 209  0.2960000 1.772380e+00
    ## 420  0.2960000 2.367506e+00
    ## 611  0.2960000 2.048192e+00
    ## 1020 0.2960000 2.014470e+00
    ## 1446 0.2960000 2.270089e+00
    ## 1522 0.2960000 1.530362e+00
    ## 1134 0.2948229 1.582935e+00
    ## 658  0.2940541 1.491353e+00
    ## 196  0.2937500 1.765035e+00
    ## 112  0.2936000 1.641606e+00
    ## 1224 0.2936000 4.329606e+09
    ## 119  0.2935733 1.739854e+00
    ## 433  0.2934286 1.647137e+00
    ## 734  0.2933333 1.987760e+00
    ## 917  0.2931034 3.955741e+00
    ## 1223 0.2930000 2.098630e+00
    ## 946  0.2920000 2.311563e+00
    ## 407  0.2912000 2.288316e+00
    ## 1407 0.2900000 2.184252e+00
    ## 1421 0.2900000 2.072392e+00
    ## 1569 0.2900000 1.475373e+00
    ## 1423 0.2895522 1.618408e+00
    ## 1326 0.2888000 2.096935e+00
    ## 1200 0.2884211 1.727560e+00
    ## 629  0.2875294 2.044026e+00
    ## 777  0.2866667 1.756621e+00
    ## 1205 0.2864000 1.611115e+00
    ## 987  0.2850000 1.904047e+00
    ## 790  0.2840000 2.087062e+00
    ## 1073 0.2840000 1.579777e+00
    ## 1364 0.2840000 1.984783e+00
    ## 1383 0.2840000 1.904783e+00
    ## 255  0.2825000 1.731807e+00
    ## 128  0.2816000 1.955093e+00
    ## 606  0.2816000 1.496618e+00
    ## 1708 0.2816000 1.658503e+00
    ## 163  0.2780000 1.492834e+00
    ## 803  0.2780000 1.643458e+00
    ## 1277 0.2763636 1.437056e+00
    ## 567  0.2760000 1.430045e+00
    ## 1518 0.2750000 2.045283e+00
    ## 1571 0.2750000 1.615354e+00
    ## 1632 0.2750000 1.726739e+00
    ## 148  0.2744828 2.122445e+00
    ## 1132 0.2744000 1.566842e+00
    ## 1465 0.2744000 1.557222e+00
    ## 726  0.2735000 1.571868e+00
    ## 1229 0.2727742 1.010007e+01
    ## 34   0.2720000 1.580546e+00
    ## 554  0.2720000 1.820374e+00
    ## 1110 0.2720000 1.419182e+00
    ## 1112 0.2720000 1.579732e+00
    ## 1763 0.2720000 1.705494e+00
    ## 1076 0.2710769 1.410216e+00
    ## 678  0.2700000 2.522749e+00
    ## 754  0.2700000 1.440907e+00
    ## 1163 0.2700000 1.608881e+00
    ## 1733 0.2700000 1.822916e+00
    ## 692  0.2692308 1.565509e+00
    ## 111  0.2690000 1.492661e+00
    ## 700  0.2690000 2.863822e+00
    ## 494  0.2672000 1.626492e+00
    ## 569  0.2672000 2.148535e+00
    ## 737  0.2672000 1.485226e+00
    ## 1714 0.2672000 2.137518e+00
    ## 1434 0.2667500 1.354575e+00
    ## 738  0.2666667 1.540445e+00
    ## 1731 0.2660000 1.556314e+00

``` r
top_200_pnl_loan_nb <- top_200_pnl$loan_nb
top_200_pnl_loan_nb_df <- data.frame(top_200_pnl_loan_nb)
print(top_200_pnl_loan_nb_df)
```

    ##     top_200_pnl_loan_nb
    ## 1                  1334
    ## 2                   715
    ## 3                   208
    ## 4                  1647
    ## 5                    65
    ## 6                   695
    ## 7                   146
    ## 8                   259
    ## 9                  1420
    ## 10                  447
    ## 11                  480
    ## 12                  691
    ## 13                  465
    ## 14                  378
    ## 15                 1158
    ## 16                 1738
    ## 17                  769
    ## 18                  739
    ## 19                   20
    ## 20                 1091
    ## 21                  981
    ## 22                  263
    ## 23                 1153
    ## 24                  418
    ## 25                  266
    ## 26                  332
    ## 27                  410
    ## 28                  729
    ## 29                 1148
    ## 30                 1567
    ## 31                 1514
    ## 32                  442
    ## 33                  467
    ## 34                 1220
    ## 35                  889
    ## 36                  168
    ## 37                 1598
    ## 38                  867
    ## 39                 1500
    ## 40                 1678
    ## 41                   10
    ## 42                  341
    ## 43                  123
    ## 44                  674
    ## 45                 1092
    ## 46                 1281
    ## 47                    7
    ## 48                 1113
    ## 49                  416
    ## 50                  767
    ## 51                  354
    ## 52                  929
    ## 53                 1295
    ## 54                  118
    ## 55                 1105
    ## 56                  714
    ## 57                 1350
    ## 58                  717
    ## 59                  179
    ## 60                   30
    ## 61                 1660
    ## 62                  256
    ## 63                 1193
    ## 64                 1024
    ## 65                 1646
    ## 66                 1419
    ## 67                 1761
    ## 68                  832
    ## 69                  840
    ## 70                   22
    ## 71                  117
    ## 72                 1562
    ## 73                  364
    ## 74                 1093
    ## 75                 1491
    ## 76                 1551
    ## 77                 1304
    ## 78                  353
    ## 79                 1560
    ## 80                  356
    ## 81                  924
    ## 82                  363
    ## 83                 1557
    ## 84                  512
    ## 85                 1118
    ## 86                  861
    ## 87                   35
    ## 88                  615
    ## 89                  709
    ## 90                  791
    ## 91                  948
    ## 92                 1035
    ## 93                 1136
    ## 94                 1312
    ## 95                 1353
    ## 96                 1580
    ## 97                 1762
    ## 98                  454
    ## 99                 1664
    ## 100                1349
    ## 101                 971
    ## 102                 679
    ## 103                 265
    ## 104                1523
    ## 105                 213
    ## 106                1379
    ## 107                 571
    ## 108                1155
    ## 109                 610
    ## 110                 875
    ## 111                1667
    ## 112                1219
    ## 113                1406
    ## 114                1617
    ## 115                1023
    ## 116                 144
    ## 117                 719
    ## 118                1214
    ## 119                1280
    ## 120                 851
    ## 121                1123
    ## 122                 976
    ## 123                 386
    ## 124                 958
    ## 125                 970
    ## 126                 216
    ## 127                 486
    ## 128                 988
    ## 129                1409
    ## 130                 872
    ## 131                 419
    ## 132                 270
    ## 133                 209
    ## 134                 420
    ## 135                 611
    ## 136                1020
    ## 137                1446
    ## 138                1522
    ## 139                1134
    ## 140                 658
    ## 141                 196
    ## 142                 112
    ## 143                1224
    ## 144                 119
    ## 145                 433
    ## 146                 734
    ## 147                 917
    ## 148                1223
    ## 149                 946
    ## 150                 407
    ## 151                1407
    ## 152                1421
    ## 153                1569
    ## 154                1423
    ## 155                1326
    ## 156                1200
    ## 157                 629
    ## 158                 777
    ## 159                1205
    ## 160                 987
    ## 161                 790
    ## 162                1073
    ## 163                1364
    ## 164                1383
    ## 165                 255
    ## 166                 128
    ## 167                 606
    ## 168                1708
    ## 169                 163
    ## 170                 803
    ## 171                1277
    ## 172                 567
    ## 173                1518
    ## 174                1571
    ## 175                1632
    ## 176                 148
    ## 177                1132
    ## 178                1465
    ## 179                 726
    ## 180                1229
    ## 181                  34
    ## 182                 554
    ## 183                1110
    ## 184                1112
    ## 185                1763
    ## 186                1076
    ## 187                 678
    ## 188                 754
    ## 189                1163
    ## 190                1733
    ## 191                 692
    ## 192                 111
    ## 193                 700
    ## 194                 494
    ## 195                 569
    ## 196                 737
    ## 197                1714
    ## 198                1434
    ## 199                 738
    ## 200                1731

``` r
#arrange the table using our criterion that weight the predicted probability of default and the return if no default
pred_logistic_reg_assessment_data_df_complete_ordered_by_criterion <- arrange(pred_logistic_reg_assessment_data_df_complete,desc(criterion))
  
#print the first 200 loans which would be the ones we would invest in according to our criterion
top_200_criterion <- head(pred_logistic_reg_assessment_data_df_complete_ordered_by_criterion,200)
  by_criterion_total_pnl_no_default <- sum(top_200_criterion$return_no_default)
  by_criterion_total_pnl_no_default
```

    ## [1] 52.73529

``` r
top_200_criterion_loan_nb <- top_200_criterion$loan_nb
top_200_criterion_loan_nb_df <- data.frame(top_200_criterion_loan_nb)
print(top_200_criterion_loan_nb_df)
```

    ##     top_200_criterion_loan_nb
    ## 1                        1224
    ## 2                         315
    ## 3                          65
    ## 4                        1148
    ## 5                        1318
    ## 6                        1229
    ## 7                        1334
    ## 8                         715
    ## 9                        1319
    ## 10                       1302
    ## 11                        318
    ## 12                        353
    ## 13                        779
    ## 14                        266
    ## 15                        702
    ## 16                       1593
    ## 17                        917
    ## 18                        990
    ## 19                        341
    ## 20                       1103
    ## 21                       1398
    ## 22                        929
    ## 23                        356
    ## 24                         21
    ## 25                        974
    ## 26                       1762
    ## 27                        418
    ## 28                        520
    ## 29                       1288
    ## 30                        704
    ## 31                        469
    ## 32                       1018
    ## 33                       1560
    ## 34                        473
    ## 35                        263
    ## 36                       1024
    ## 37                        259
    ## 38                       1054
    ## 39                        932
    ## 40                        515
    ## 41                        722
    ## 42                       1307
    ## 43                        436
    ## 44                        739
    ## 45                        307
    ## 46                         64
    ## 47                       1123
    ## 48                        518
    ## 49                       1648
    ## 50                        440
    ## 51                       1072
    ## 52                       1261
    ## 53                        941
    ## 54                        620
    ## 55                        700
    ## 56                       1242
    ## 57                        821
    ## 58                       1262
    ## 59                       1078
    ## 60                       1189
    ## 61                        935
    ## 62                        208
    ## 63                       1293
    ## 64                       1393
    ## 65                        182
    ## 66                        480
    ## 67                        250
    ## 68                       1226
    ## 69                        504
    ## 70                        290
    ## 71                       1463
    ## 72                        552
    ## 73                        409
    ## 74                       1647
    ## 75                       1794
    ## 76                       1638
    ## 77                        544
    ## 78                       1100
    ## 79                        378
    ## 80                       1043
    ## 81                       1654
    ## 82                        320
    ## 83                       1488
    ## 84                        465
    ## 85                        454
    ## 86                        693
    ## 87                       1342
    ## 88                       1738
    ## 89                       1680
    ## 90                        640
    ## 91                        360
    ## 92                        146
    ## 93                       1030
    ## 94                        784
    ## 95                       1044
    ## 96                        364
    ## 97                       1443
    ## 98                        559
    ## 99                       1067
    ## 100                       637
    ## 101                      1739
    ## 102                        53
    ## 103                       511
    ## 104                      1340
    ## 105                        45
    ## 106                       678
    ## 107                       545
    ## 108                       226
    ## 109                       596
    ## 110                       695
    ## 111                      1121
    ## 112                       616
    ## 113                       703
    ## 114                       354
    ## 115                        56
    ## 116                       556
    ## 117                       243
    ## 118                        75
    ## 119                       190
    ## 120                      1350
    ## 121                       447
    ## 122                       988
    ## 123                       424
    ## 124                       971
    ## 125                       419
    ## 126                       592
    ## 127                       717
    ## 128                       719
    ## 129                      1374
    ## 130                       527
    ## 131                      1136
    ## 132                        79
    ## 133                       851
    ## 134                      1722
    ## 135                       426
    ## 136                       765
    ## 137                       107
    ## 138                       691
    ## 139                       359
    ## 140                       486
    ## 141                      1007
    ## 142                      1247
    ## 143                       246
    ## 144                       924
    ## 145                      1646
    ## 146                       363
    ## 147                       872
    ## 148                        50
    ## 149                      1612
    ## 150                       864
    ## 151                      1413
    ## 152                       420
    ## 153                       699
    ## 154                      1343
    ## 155                      1124
    ## 156                      1208
    ## 157                       667
    ## 158                      1445
    ## 159                         9
    ## 160                      1220
    ## 161                       213
    ## 162                      1461
    ## 163                       926
    ## 164                      1137
    ## 165                      1420
    ## 166                       179
    ## 167                      1357
    ## 168                       533
    ## 169                       546
    ## 170                      1097
    ## 171                      1255
    ## 172                       532
    ## 173                       340
    ## 174                       946
    ## 175                      1295
    ## 176                      1077
    ## 177                      1786
    ## 178                       501
    ## 179                      1641
    ## 180                       729
    ## 181                       832
    ## 182                       396
    ## 183                       947
    ## 184                      1158
    ## 185                       407
    ## 186                      1198
    ## 187                       972
    ## 188                      1359
    ## 189                       189
    ## 190                      1248
    ## 191                      1051
    ## 192                      1446
    ## 193                      1583
    ## 194                       730
    ## 195                      1596
    ## 196                       978
    ## 197                      1005
    ## 198                       247
    ## 199                      1500
    ## 200                       842

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
