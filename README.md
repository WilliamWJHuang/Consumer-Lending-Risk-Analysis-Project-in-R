# Consumer Lending Risk Analysis Project
**Project Description**
This project is to build a logistic regression model to see how the information on loan application impacts the default rates.
I identify six significant predictors based on my analysis.
<br>
Full pdf report: https://bit.ly/2VhfMLD
<br>
**Data Source** <br>
Data source: Home Credit Group, Kaggle
<br>

**Exploratory Data Analysis** <br>
![EDA](https://github.com/WilliamWJHuang/Consumer-Lending-Risk-Analysis-Project-in-R/blob/master/Exploratory%20Data%20Analysis.png)


**Missing Values Imputation** <br>
OWN_CAR_AGE and AMR_REQ_CREDOT_BUREAU_SUM are the two columns that contain missing values in the dataset. I used the mice package to conduct missing value imputation to generate complete datasets. For the imputation method, I chose cart, instead of the default method. I have tried to use the default method to impute missing values; however, it returned the following error "system is computationally singular". The cause of the problem here could probably be the large number of unbalanced factor variables, such as NAME_CONTRACT_TYPE,  NAME_TYPE_SUITE and NAME_EDUCATION_TYPE, in the dataset. When these variables are turned into dummy variables, there’s a high probability that one column is a linear combination of another. As the default imputation methods are parametric, which involve linear regression, this would result in a X matrix that cannot be inverted. Therefore, I considered to change the imputation method to Classification and Regression Trees (CART) that is not stochastic and non-parametric, which require no X matrix inversion.


**ROC curve** <br>
![ROC](https://github.com/WilliamWJHuang/Consumer-Lending-Risk-Analysis-Project-in-R/blob/master/ROC%20curve.png)

**Model Interpretation** <br>
After examining all the possible pairs of variables that could have interaction effects; furthermore, there’s no evidence suggests a logarithm or quadratic transformation on variables, therefore, the base model should be our ultimate model. The AUC is 0.6993.

Significant predictors: - CODE_GENDERM - NAME_CONTRACT_TYPERevolving loans - FLAG_OWN_CARY - DAYS_BIRTH - DAYS_EMPLOYED - OCCUPATION_TYPEWaiters/barmen staff - CNT_FAM_MEMBERS6 - ORGANIZATION_TYPERealtor - ORGANIZATION_TYPEXNA - AMR_REQ_CREDIT_BUREAU_SUM8

**Model Limitation** <br>
The limitation of this logistic regressino model includes the following: - I cannot do extrapolation for prediction that is our of the range that I use to form the logistic regression model. - The predictability of this model is relatively week as most p-values are quite large. - The predictability is subject to the choice of threshold.
