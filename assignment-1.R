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

# Regression models with controls
model4 <- lm(bidder.car ~ deal.allstock + bidder.size + bidder.sigma, data = subset(df, public == 1))
model5 <- lm(bidder.car ~ deal.allstock + bidder.size + bidder.sigma, data = subset(df, private == 1))
model6 <- lm(bidder.car ~ deal.allstock + public + deal.allstock*public + bidder.size + bidder.sigma, data = df)
model7 <- lm(bidder.car ~ deal.allstock + public + deal.allstock*public + bidder.size + bidder.sigma + bidder.size:public + bidder.sigma:public, data = df)

# Create an empty list to store the robust standard errors
se <- list()

# Loop through each model to calculate robust standard errors
models <- list(model1, model2, model3, model2_1, model4, model5, model6, model7)
for (i in 1:length(models)) {
  se[[i]] <- coeftest(models[[i]], vcov = vcovHC(models[[i]], type = "HC3"))[, 2]
}
# Check the distribution of 'hostile' within the subset where 'private == 1'
table(subset(df, private == 1)$hostile)

table(subset(df, public == 1)$hostile)

# Check summary statistics
summary(subset(df, private == 1)$hostile)

# Stargazer table for model 1,2,3, adjusted for heterosceda..
stargazer(model1, model2, model3, title = "Regression Model Summaries", type = "text", 
          se = list(se[[1]], se[[2]], se[[3]]), report = "vc*t")

# Create a stargazer table for the regression models with controls
stargazer(model4, model5, model6, model7, title = "Regression Models with Controls", type = "text", 
          se = list(se[[1]], se[[2]], se[[3]]),
          report = "vc*t")


