#=======================================

#STEP 1: Exploratory Data Analysis - EDA

#======================================
#As from lectures and the instructions
#Read table and initialize with data
data <- read.table("/Users/royho/Desktop/Demographic.Txt")

head(data)

region1_data <- subset(data, data[,17] == 1)

nrow(region1_data)


head(region1_data)

# response 10, predictors are 11, 13, 14

#Pairwise plot
#Here it also plots the matrix of correlation 
#This is in regards to columns 5 to 12
#This is for the visualization of the correlation between the variables
pairs(region1_data[,5:12]) 
cor(region1_data[,5:12]) 

#Organize new data based on region 1 and also make the plot have thicker lines with lwd
new.data <- cbind(region1_data[,10], region1_data[,11], region1_data[,13], region1_data[,14])

new.data <- as.data.frame(new.data)

colnames(new.data) <- c("Total_Crimes", "Percent_High_School_Grads", "Percent_Below_Poverty_Level", "Percent_Unemployment")
#Our predictors here 
pairs(new.data, lwd=3)

head(new.data)

#======================================


#STEP 2:Build our Models 
#Based on information given in the textbook and in R:

#======================================

#This is for the linear modeling
attach(new.data)

#Linear model for the total crimes 
model <- lm(Total_Crimes~Percent_High_School_Grads+Percent_Below_Poverty_Level+Percent_Unemployment)

summary(model)

anova(model)

plot(model)
Total_Crimes.n<-length(Total_Crimes)

# Fit multiple linear regression models with different combinations of predictors

# Model 1: Total_Crimes-1
model1 <- lm(Total_Crimes~1)
summary(model1)
anova(model1)
plot(model1)

# Model 2: Total_Crimes ~ Percent_High_School_Grads
model2 <- lm(Total_Crimes ~ Percent_High_School_Grads)
summary(model2)
anova(model2)
plot(model2)

# Model 3: Total_Crimes ~ Percent_Below_Poverty_Level
model3 <- lm(Total_Crimes ~ Percent_Below_Poverty_Level)
summary(model3)
anova(model3)
plot(model3)
# Model 4: Total_Crimes ~ Percent_Unemployment
model4 <- lm(Total_Crimes ~ Percent_Unemployment)
summary(model4)
anova(model4)
plot(model4)
# Model 5: Total_Crimes ~ Percent_High_School_Grads + Percent_Below_Poverty_Level
model5 <-lm(Total_Crimes ~ Percent_High_School_Grads + Percent_Below_Poverty_Level)
summary(model5)
anova(model5)
plot(model5)
# Model 6: Total_Crimes ~ Percent_High_School_Grads + Percent_Unemployment
model6 <- lm(Total_Crimes ~ Percent_High_School_Grads + Percent_Unemployment)
summary(model6)
anova(model6)
plot(model6)

# Model 7: Total_Crimes ~ Percent_Below_Poverty_Level + Percent_Unemployment
model7 <- lm(Total_Crimes ~ Percent_Below_Poverty_Level + Percent_Unemployment)
summary(model7)
anova(model7) 
plot(model7)

# Model 8: Total_Crimes ~ Percent_High_School_Grads + Percent_Below_Poverty_Level + Percent_Unemployment
model8 <- lm(Total_Crimes ~ Percent_High_School_Grads + Percent_Below_Poverty_Level + Percent_Unemployment)
summary(model8)
anova(model8) 
plot(model8)
#Here is the end of Part 2 of our Model building aspects of the project


#======================================

#STEP 3: Residuals for selected Model - Diagnostic Plots

#======================================

par(mfrow=c(2,2))
plot(model2)
plot(model2)

# Compute the RSS for the model
RSS <- sum(model2$residuals^2)  # Replace model2 with your desired model

# Calculate the likelihood function
likelihood <- exp(-RSS / 2)

# Number of parameters in the model (including intercept)
num_params <- length(coefficients(model2))

# Sample size
n <- length(model2$residuals) + num_params  # Adjust if necessary

# Calculate BIC and AIC
BIC <- -2 * log(likelihood) + num_params * log(n)
AIC <- -2 * log(likelihood) + 2 * num_params

step(model2)
step(model2, k=log(Total_Crimes.n) )

# Fit multiple linear regression models with different combinations of predictors
models <- list(
  model1 = lm(Total_Crimes ~ 1, data = new.data),
  model2 = lm(Total_Crimes ~ Percent_High_School_Grads, data = new.data),
  model3 = lm(Total_Crimes ~ Percent_Below_Poverty_Level, data = new.data),
  model4 = lm(Total_Crimes ~ Percent_Unemployment, data = new.data),
  model5 = lm(Total_Crimes ~ Percent_High_School_Grads + Percent_Below_Poverty_Level, data = new.data),
  model6 = lm(Total_Crimes ~ Percent_High_School_Grads + Percent_Unemployment, data = new.data),
  model7 = lm(Total_Crimes ~ Percent_Below_Poverty_Level + Percent_Unemployment, data = new.data),
  model8 = lm(Total_Crimes ~ Percent_High_School_Grads + Percent_Below_Poverty_Level + Percent_Unemployment, data = new.data)
)

# Initialize a matrix to store results
results <- matrix(NA, nrow = length(models), ncol = 9)
rownames(results) <- names(models)

# Calculate SSE, R^2, Cp, AIC, BIC, and PRESS for each model
for (i in seq_along(models)) {
  model <- models[[i]]
  RSS <- sum(model$residuals^2)
  R_squared <- summary(model)$r.squared
  num_params <- length(coefficients(model))
  n <- nrow(model$model)
  
  likelihood <- exp(-RSS / 2)
  AIC <- -2 * log(likelihood) + 2 * num_params
  BIC <- -2 * log(likelihood) + num_params * log(n)
  
  MSE <- RSS / n
  
  # Calculate Cp
  Cp <- (1 / n) * (RSS + 2 * num_params * MSE)
  
  # Calculate adjusted R^2
  adj_R_squared <- 1 - ((n - 1) / (n - num_params - 1)) * (1 - R_squared)
  
  # Calculate p-values for individual coefficients
  p_values <- summary(model)$coefficients[, 4]
  
  # Number of significant parameters (p-values < 0.01)
  num_significant <- sum(p_values < 0.01)
  

  F_statistic <- summary(model)$fstatistic
  F_value <- F_statistic[1]
  F_p_value <- pf(F_value, num_params, n - num_params - 1, lower.tail = FALSE)
  
  
  results[i, ] <- c(RSS, R_squared, adj_R_squared, Cp, AIC, BIC, num_significant, p_values)
}

# Output the table
colnames(results) <- c("Model", "SSE", "R2", "Adj_R2", "Cp", "AIC", "BIC", "Num_Significant", "Overall_Test_P_Value")
print(results)

#For each model, we would calculate R squared, adjusted R squared values, etc
#Additionally, we used the AIC and BIC for the sake of model comparisons which once again helped support our decision for the choosing of our model

#===============================

#STEP 4: Conclusions - Based on our analysis, coefficients, summaries, etc.

#===============================


#Stating the conclusions:
#Many components here:
#Why the model was selected based on results from summary and anova - take into account the coefficients, R-Squared as well as the adjusted R squared
#Also take into account the residuals from step 3  and for our selected model
#In the anova table we will also look at the results from that and the F-statistic and its p-value

#Why does the model fit? Significant predictors? Assumptions based on the model? And overall conclusion will be stated here:

#We chose 3 predictors for total crimes: Percent High School Grads, Percent below poverty level, and Percent unemployment

#Based on our summaries and model and anova table information, the 3 predictors that we chose for total crimes.
#We see that with our summary of the model, the intercept is not a significant value. The p=0.915. Because of this, this indicates that:
#When we set all of our predictors to 0, the total crimes do not really significantly differ from 0. 
#However something that stood out within our analysis and results is that percent below poverty level IS significant compared to the other predictors.
#Our percent below poverty level is significant with a p of less than 0.001. (p<0.001)
#Because of this significant value, out of the predictors percent below poverty level stands out among them. Not to say that our other
#predictors did not have an effect on total crime, but percent below poverty level stood out among them based on its significance for value. 
#Based on our rather low R-value, it also suggests that if we broaden our view on the whole question and project, there are other technical factors,
#which were not included in the model which could also be contributing to total crimes, which logically makes sense since we had to choose 3 predictors.

#Our R squared value is 0.2079
#20.79 percent variability in total crimes will be able to be explained by this model 

#For our summary of the model, our linear model summary:
#we have an intercept whose estimate is -14,461.1 (standard error is 145,885.9). The p value here is 0.915
#This indicates that in regards to our context, it is not too statistically significant.

#Now for percent of high school grads, the estimate for this predictor is 195.2 and the standard error is 1501.9 with a p value of 0.897
#This indicates that the percentage of high school graduates in regards to the context is also not too statistically significant here with total crimes in relation

#For our percent below poverty level this predictor has an estimate of 10918.5 and the standard error is 2661.6
#Its p value is significantly low as mentioned before, which is less than 0.001 - this is a very small number
#Because of this small p-value, out of all the predictors, percent below poverty level stands out with its significant based on the pvalue
#This would then suggest that it is a significant predictor of total crimes

#lastly with our percent of unemployment, this predictor has an estimate of -6819.9 with a standard error of 4269.7
#This has a p value of 0.113, and again since we are doing this with alpha as 0.05, the p value means that this predictor is also not as statistically significant as percent below poverty level.

#Our R-squared value is 0.2079 which explains the variability and estimates that 20.79% in the variability for total crimes would be explained using our model
#For our R-squared adjusted value

#For our anova table and model, it once again is pointing at our percent below poverty level as a significant predictor.
#The F-Statistic value is 14.39 and the p-value is also incredibly low, sitting at 0.0002553.

#You can also see all of this taking into account our plots and graphs. 






