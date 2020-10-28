# Resampling-Methods Leave One Out Cross Validation Custom Implementation

I this program I will use R, to create a LOOCV Resampling method from scratch anc compare my results with the LOOCV implementaton from CARET Package.

Cross Validation is an important resampling method to give an insight about how the mode fits the data. It used to give an approximation of test error.

The drawbacks for LOOCV are that is it computational expensive depending on the number of observations in the dataset. For example given a dataset of 4,000 observations it will train on 3,999 and test on 1 observation. This applies with datasets with millions or billions of observartions. 

# Description of Leave One Out Cross Validation (LOOCV)

<img src="https://github.com/JaimeGoB/Resampling-Methods/blob/main/data/loocv.png" width="500" height="300" />


# Results

Using a custom LOOCV implementation, I am able to get the eaxact same result using the CARET package from R.

<img src="https://github.com/JaimeGoB/Resampling-Methods/blob/main/data/results.png" width="600" height="100" />
