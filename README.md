 # Pre Process
 The goal of the pre process package is to streamline exploratory data analysis. This package uses ggplot2 and vcd package for graphing. Feel free to provide suggestions on new functions to use or improvements on existing ones. 
 ## Installation
 The following is how ti install the package
 ```install.packages("Preprocess")```
 ## Functions 
 The following is a list explaining each functions usage and output,
 ### one_hot_code
 **Description:** Transforms categorical data into numerical values to be used in machine learning. Returns a dataset with hot encoded columns.<br>
 **Usage:** one_hot_code(data,variables) <br>
 **Value:** This function returns a data set with the specified variables hot encoded as well as the original. <br>
 **Arguments** <br>
 - Data (A data frame where the rows are observations and the columns are variables) <br>
 - Variable (The names of the variable to transform stored as a vector) <br>
### convert_tofactor
 **Description:** Converts categorical variables to factors with multiple levels. <br>
 **Usage:** convert_tofactor(data) <br>
 **Value:** This function returns a data set with the categorical variables in the dataset converted into factors, making each different value a level. <br>
 **Argument** <br>
  -Data (a data frame where the rows are observations and the columns are variables) <br>
### univar_breakdown
 **Description:** Does a breakdown of the internal structure of a variable. Produces a data summary and a graphical representation. <br>
 **Usage:** univar_breakdown(var) <br>
 **Argument** <br>
  -Var (the variable to be transformed stored as a vector or column from data frame) <br>
 **Value:** the function returns a list with different breakdowns based on the variable type. <br>
  -*Numeric* <br>
   *$summary (summary statistics of variable) <br>
   *$histogram (histogram of variable) <br>
  -*Categorical* <br>
   *$table (table of variable counts) <br>
   *$plot (bar chart of variable counts) <br>
### bivar_breakdown
 **Description:** Explores the relationship between two variables. Produces summaries and graphical representation. Uses the ggplot function to produce graphs. <br>
 **Usage:** bivar_breakdown(x,y,data) <br>
 **Argument** <br> 
   -x (the independent variable) <br>
   -y (the dependent variable) <br>
   -data (the data set) <br>
 **Value:** the function returns a list different breakdowns based on variable type. <br>
 *Numeric* <br>
   -$summary_of_x (summary statistics of x variable) <br>
   -$summary_of_y (summary statistics of y variable) <br>
   -$plot (scatterplot of variables) <br>
 *Categorical* <br>
   -$table (frequency table of variable counts) <br>
   -$plot (grouped bar chart) <br>
  *Numeric & Categorical* <br>
   -$table (it produces a summary of the numeric variables grouped by levels) <br>
   -$plot (grouped bar plot with categorical variables on the x axis) <br>

### clean_input_medi 
 **Description:** Replaces NA values in a data set with median value of each column. Returns dataset without NAs. <br>
 **Usage:** clean_input_medi(data) <br>
 **Value:** Returns data set with NA values replaced with the median of each column <br>
 **Argument** <br>
  -Data (data set with NA values to replace) <br>

### distribution_check
 **Description:** Uses a Kolov Smirnoff test to identify the underlying distribution of the data. Specifically if the data belongs to a normal, uniform, exponential, gamma and or beta distribution. It also identifies the kurtosis and skew of the data distribution. It returns these results in the form of a list. <br>
 **Usage:** distribution_check(data) <br>
 **Argument** <br>
  -Data (A data frame where the rows are observations and the columns are variables) <br>
 **Value:** Returns list of information on data set. <br>
  -$results (gives a summary of the results of the KS test) <br>
  -$kurt (gives the kurtosis) <br>
  -$skew (gives the skewdness) <br>

### multivar_breakdown <br>
 **Description:** Analyze the relationship between more than two variables. Uses ggplot function for graphing. <br>
 **Usage:** multivar_breakdown(data) <br>
  **Argument** <br>
   -Data (a data frame where the rows are observations and the columns are variables) <br>
 **Value:** Returns a list of breakdowns based on data set type <br>
  *Numeric* <br>
   -$pca (summary of pca matrix) <br>
   -$cov_matrix (covariance matrix) <br>
   -$scaled_matrix (scaled covariance matrix) <br>
   -$scree plot (scree plot) <br>
   -$biplot (biplot) <br>
   -$scattergrid (grid of scatter plots) <br>
 *Categorical* <br>
   -$freq (frequency table of all of the variables) <br>
   -$mos (mosaic graph on categorical data) <br>
 *Numeric & Categorical* <br>
   -$freq (table of the count of each categorical variable) <br>
   




