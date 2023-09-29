# Load the RData file
load("CAR_M&A.RData")
library(robustHD)
library(dplyr)
library(lmtest)

# Bullet 1 ------------------------------------------------------------------

# List the names of the objects in the environment
loaded_names <- ls()

# Assuming the first object is the data frame you want
df <- get(loaded_names[1])

# Now you can run summary statistics
summary(df)

# Boxplot to check for outliers in multiple variables
boxplot(df[, c("deal.value", "bidder.size", "bidder.car", "bidder.mtb", "bidder.runup", "bidder.fcf", "bidder.lev", "deal.relsize")],
        main="Boxplot for Outliers",
        las=2,  # makes the axis labels perpendicular to the axis
        cex.axis=0.8)  # reduces the size of the axis labels

# Create separate boxplots for variables on different scales
par(mfrow=c(2,1))  # divide the plotting area into 2 rows and 1 column
boxplot(df[, c("deal.value", "bidder.size")], main="Boxplot for Large Scale Variables", las=2, cex.axis=0.8)
boxplot(df[, c("bidder.car", "bidder.mtb", "bidder.runup", "bidder.fcf", "bidder.lev", "deal.relsize")], main="Boxplot for Small Scale Variables", las=2, cex.axis=0.8)



# Winsorize the data (if needed)
df$bidder.size <- winsorize(df$bidder.size, probs = c(0.01, 0.99))


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
# Summary statistics for regression variables
desc_table2 <- df %>%
  summarise(
    across(
      c("bidder.car", "deal.allstock", "private", "public"),
      list(mean = mean, sd = sd, p10 = ~quantile(., 0.1), p25 = ~quantile(., 0.25), p50 = ~quantile(., 0.5), p75 = ~quantile(., 0.75), p90 = ~quantile(., 0.9)),
      .names = "{col}_{fn}"
    )
  )

# Display the table
print(desc_table2)


# Bullet 4 ----------------------------------------------------------------


# Regression models
model1 <- lm(bidder.car ~ deal.allstock, data = subset(df, public == 1))
model2 <- lm(bidder.car ~ deal.allstock, data = subset(df, private == 1))
model3 <- lm(bidder.car ~ deal.allstock + public + deal.allstock:public, data = df)

# Display the results
summary(model1)
summary(model2)
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






