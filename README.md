# melbourne_housing
Regression problem; find the optimal model for predicting the price of accommodations in Melbourne, by using R.

After building the linear model with either the scaled or the original data set, trying different ways with the methods of Best and Forward/Backward Subset Selections and after applying either Ridge/Lasso Regression or the Principal Component Regression/ Partial Least Squares, it is concluded that the lowest test error (apart from the stochastic Gradient Boosted Trees) is the linear model (with the polynomial terms of a feature of the dataset) based on the rule Cp, which is extracted from the Best Subset Selection method.
