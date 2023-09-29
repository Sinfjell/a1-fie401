# Load the RData file
load("CAR_M&A.RData")

# Load libraries
library(robustHD)
library(dplyr)
library(lmtest)
library(vtable)
library(stargazer)



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

# Winsorize the data (if needed)
df$bidder.size <- winsorize(df$bidder.size, probs = c(0.01, 0.99))

# List of variables to Winsorize
variables <- c("bidder.car", "deal.value", "bidder.size", "bidder.mtb", "bidder.runup", "bidder.fcf", "bidder.lev", "bidder.sigma", "deal.relsize")

# Loop through each variable to Winsorize
for (var in variables) {
  df[[var]] <- winsorize(df[[var]], probs = c(0.01, 0.99))
}




# Bullet 2 ----------------------------------------------------------------
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


# Bullet 3 ----------------------------------------------------------------

# Create the summary table using sumtable()
desc_table2 <- sumtable(df, c("bidder.car", "deal.allstock", "private", "public"))

# Display the table
print(desc_table2)



# Bullet 4 ----------------------------------------------------------------

# Regression models
model1 <- lm(bidder.car ~ deal.allstock, data = subset(df, public == 1))
model2 <- lm(bidder.car ~ deal.allstock, data = subset(df, private == 1))
model3 <- lm(bidder.car ~ deal.allstock + public + deal.allstock:public, data = df)

# Display the results
summary(model1)
summary(model2)a
summary(model3)


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






