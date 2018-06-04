Do a person's brain size and body size predict his/her intelligence? Interested in answering this question, a group of researchers record the performance IQ scores ($Y$), the brain size ($X_1$) based on the count obtained from MRI scans, the height ($X_2$) in inches, and the weight ($X_3$) in pounds for a random sample of students. File iqsize.txt contains the data and is available on Blackboard.

Before you get started, make sure you have removed all objects stored in the current R session using, for instance, command rm(list = ls()). 

### 0) Load the data into R
```{r}
rm(list = ls())
setwd("C:\\Users\\Кан\\Documents")
iqdata = read.table("iqsize.txt", header = TRUE, sep = "")
attach(iqdata)
```

### 1) Prepare a boxplot of performance IQ score. Describe the distribution of this variable (skeweness, outliers, other). (2 points) 
```{r}
boxplot(PIQ,ylab="IQ score",main="Performance IQ data", outliers=TRUE)
```
the boxplot is skewed left, doesnt have any outliers
### 2) Obtain the correlation matrix of all four variables. (1 point)
```{r}
corvar=cor(iqdata, use='all.obs', method = "pearson")
```
### 3) Does the correlation matrix indicate the presence of multicollinearity? Justify. (2 points)
The correlation coefficient of the variables Weight and Height; and Height and Brain could indicate multicollinearity, as the
correlation coefficient is relatively high.

### 4) Obtain the scatter plot matrix of all four variables. (1 point) 
```{r}
pairs(corvar)
```

### In what follows, we will consider two proposed models to predict performance IQ score. Model 1 utilizes as independent variables Brain size and Height. Model 2 utilizes as independent variables Brain size and Weight. 

### 5) For each of the two proposed models, fit a first order regression model. Obtain a summary of the fitted models. (4 points) 
```{r}
model1=lm(PIQ~Brain+Height, data=iqdata)
summary(model1)
model2=lm(PIQ~Brain+Weight,data=iqdata)
summary(model2)
```

### 6) For each of the two proposed models, interpret $R^2$ in the context of the data being analysed. Is one model preferable in terms of this measure? (3 points)
Both $R^2$ and adjusted $R^2$ for both models are relatively small. However, if we are to decide which model is more preferable in terms of $R^2$, there is no answer, since the coefficient of determination alone does not indicate that a model is well specified. We would have to use adjusted $R^2$ coefficient.

### 7) Which of the two models is preferable in terms of the significance of the regression paramaters? Explain. (3 points)
Model 1 is preferable in terms of the significance of the regression parameters, since all its parameters are significant, while
Model 2 parameters(Weight) is not. Furthermore, adjusted $R^2$ coefficient of the Model 1 is higher than of the Model 2.
### We consider Model 1 in the following questions. 

### 8) For Model 1, obtain the residuals and plot them against performance IQ score and each of the independent variables entering into the model. Prepare also a normal quantile plot of the residuals. Use command layout(matrix(c(1,2,3,4),2,2)) before the first plot so that the four plots are shown on the same window. (5 points)
```{r}
resid.model1=resid(model1)
fitted.model1=fitted(model1)
layout(matrix(c(1,2,3,4),2,2)) # 4 graphs/page 
plot(fitted.model1, resid.model1, xlab="Performance IQ", ylab="Residuals", main="Residuals vs. fitted values")
plot(Brain, resid.model1, xlab="Brain", ylab="Residuals", main="Residuals vs. Brain")
plot(Height, resid.model1, xlab = "Height", ylab="Residuals", main="Residuals vs. Height")
qqnorm(resid.model1, xlab="Theoretical quantiles", ylab ="Sample quantiles", main="Normal Q-Q Plot")
qqline(resid.model1, col="red")
```

### 9) Are the Gauss-Markov assumptions satisfied? State the assumptions and explain how you arrive at your answer for each assumption. (3 points)
Note: you are asked to answer questions 1) to 8) even if you think some of these assumptions are violated.
Assumption 1: The random errors have mean 0
Although from the plot Residuals vs. fitted values we can see that there are residuals that are near 0, the errors are not normal(from the Normal Q-Q plot). Therefore there not enough information to say this assumption is valid.  
Assumption 2: The random errors are homoscedastic
Not violated. From the plots Residuals vs. Height and Residuals vs. Brain we can see that there is no pattern or megascope shape
on the plots. Therefore the variance of errors is constant.
Assumption 3: The random errors are uncorrelated
There is no pattern or structure in any residual plots therefore errors are uncorrelated 

### 10) We sometimes assume that the random errors in our model are normally distributed. Is this assumption violated? Justify. (1 points)
Yes, from the plot the errors are not normal(the Normal Q-Q plot), since the pattern does not excactly follows the line.



### 11) Find the "best" model possible to predict performance IQ score. Your model should at least be better than Model 1. Present your proposed model and clearly justify why you think it is the best. Which criterion did you use in your comparison and why such a choice? Be creative! (7 points)
Note: full marks will only be given to creative solutions that are fully explained and justified.

Since Model1 already utilzes the most optimal variables, and dropping both Height and Weight is controproductive, we can instead transform the variables. The most common one is log transformation. If we transform the dependent(PIQ) variable, we can observe change in the Normal Q-Q plot. The plot improves, therefore the normality of errors improves as well. By doing this we improved the approximate symmetry and the homoscedacity of residuals. By log transforming the independent variable Height we tried to improve the linear relationship between PIQ and Height.  If we build the model:
```{r}
model5=lm(log(PIQ)~Brain+log(Height), data=iqdata)
summary(model5)
```
