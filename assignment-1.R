# Load libraries
library(robustHD)
library(dplyr)
library(lmtest)
library(vtable)
library(stargazer)
library(lmtest)
library(sandwich)

# Load the RData file
load("CAR_M&A.RData")

# Bullet 1 ------------------------------------------------------------------

# List the names of the objects in the environment
loaded_names <- ls()

# Assuming the first object is the data frame you want
df <- get(loaded_names[1])

# Now you can run summary statistics
summary(df)

# Create an empty list to store the boxplots
boxplot_list <- list()

# Set up the plotting area for multiple plots
par(mfrow = c(2, 2))  # 4 rows and 2 columns

# List of variables to plot
variables <- c("bidder.car", "deal.value", "bidder.size", "bidder.mtb", "bidder.runup", "bidder.fcf", "bidder.lev", "bidder.sigma", "deal.relsize")

# Loop through each variable to create a boxplot
for (var in variables) {
  boxplot(df[[var]], main = paste("Boxplot of", var), ylab = var)
  boxplot_list[[var]] <- recordPlot()  # Store the boxplot
}

# List of variables to Winsorize
variables <- c("bidder.car", "deal.value", "bidder.size", "bidder.mtb", "bidder.runup", "bidder.fcf", "bidder.lev", "bidder.sigma", "deal.relsize")

# Loop through each variable to Winsorize
for (var in variables) {
  df[[var]] <- winsorize(df[[var]], probs = c(0.025, 0.975))
}


# Descriptive table 1 ----------------------------------------------------------------
# Aggregate data by year
desc_table1 <- df %>%
  group_by(yyyy) %>%
  summarise(
    num_deals = n(),
    avg_deal_size = mean(deal.value, na.rm = TRUE),
    share_private = mean(private, na.rm = TRUE),
    share_allstock = mean(deal.allstock, na.rm = TRUE)
  )

# Display the table
print(desc_table1)


# Descriptive table 2 ----------------------------------------------------------------
# Variables to include in the descriptive table
vars_to_include <- c("bidder.car", "deal.allstock", "public", "bidder.size", "bidder.sigma")

# Create the summary table using sumtable()
desc_table2 <- sumtable(df[, vars_to_include])

# Display the table
print(desc_table2)



# Bullet 4 ----------------------------------------------------------------

# Regression models
model1 <- lm(bidder.car ~ deal.allstock, data = subset(df, public == 1))
model2 <- lm(bidder.car ~ deal.allstock, data = subset(df, private == 1))
model3 <- lm(bidder.car ~ deal.allstock + public + deal.allstock*public, data = df)
model2_1 <- lm(bidder.car ~ deal.allstock + hostile, data = subset(df, private == 1))

# Create a stargazer table for the regression models
stargazer(model3, model31, model32, title = "Regression Model Summaries", type = "text", 
          report = "vc*t")


# Display the results
summary(model1)
summary(model2)
summary(model3)

# Create a stargazer table for the regression models
stargazer(model1, model2, model3, title = "Regression Model Summaries", type = "text", 
          report = "vc*t")


# Bullet 5 ----------------------------------------------------------------

# Regression models with controls
model4 <- lm(bidder.car ~ deal.allstock + bidder.size + bidder.sigma, data = subset(df, public == 1))
model5 <- lm(bidder.car ~ deal.allstock + bidder.size + bidder.sigma, data = subset(df, private == 1))
model6 <- lm(bidder.car ~ deal.allstock + public + deal.allstock:public + bidder.size + bidder.sigma, data = df)
model7 <- lm(bidder.car ~ deal.allstock + public + deal.allstock:public + bidder.size + bidder.sigma + bidder.size:public + bidder.sigma:public, data = df)

# Display the results
summary(model4)
summary(model5)
summary(model6)
summary(model7)

# Bullet 5 ----------------------------------------------------------------
# Create a stargazer table for the regression models with controls
stargazer(model4, model5, model6, model7, title = "Regression Models with Controls", type = "text", 
          report = "vc*t")



# Bullet 4 ----------------------------------------------------------------

# Test for heteroskedasticity in the first set of models
bp1 <- bptest(model1)
bp2 <- bptest(model2)
bp3 <- bptest(model3)
bp2_1 <- bptest(model2_1)

# Display the results
print("Breusch-Pagan test for model1")
print(bp1)

print("Breusch-Pagan test for model2")
print(bp2)

print("Breusch-Pagan test for model3")
print(bp3)

print("Breusch-Pagan test for model2_1")
print(bp2_1)

# Bullet 5 ----------------------------------------------------------------





# Heteroscedatisity test ----------------------------------------------------------------

# Function to compare t-values
compare_t_values <- function(model) {
  # Standard OLS t-values
  standard_t_values <- summary(model)$coefficients[, "t value"]
  
  # Robust t-values
  robust_t_values <- coeftest(model, vcov = vcovHC(model, type = "HC3"))[, "t value"]
  
  # Combine and compare
  comparison <- data.frame(Standard = standard_t_values, Robust = robust_t_values)
  print(comparison)
}

# Run the function on each model
print("T-value comparison for model1")
compare_t_values(model1)

print("T-value comparison for model2")
compare_t_values(model2)

print("T-value comparison for model3")
compare_t_values(model3)

print("T-value comparison for model2_1")
compare_t_values(model2_1)

print("T-value comparison for model4")
compare_t_values(model4)

print("T-value comparison for model5")
compare_t_values(model5)

print("T-value comparison for model6")
compare_t_values(model6)

print("T-value comparison for model7")
compare_t_values(model7)

