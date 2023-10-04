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

# DATA TREATMENT ------------------------------------------------------------------

# List the names of the objects in the environment
loaded_names <- ls()

# Assuming the first object is the data frame you want
df <- get(loaded_names[1])

# Now you can run summary statistics
summary(df)

# Create an empty list to store the boxplots
boxplot_list <- list()

# Set up the plotting area for multiple plots
par(mfrow = c(3, 3))  # 4 rows and 2 columns

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

# Breusch-pagan test for homoscedasity
bptest(model1)
bptest(model2)
bptest(model3)
bptest(model4)
bptest(model5)
bptest(model6)
bptest(model7)
# low p-values, we need to adjust for heteroscedasity

# Adjusting for heteroscedasity
model1_hom <- coeftest(model1, vcov = vcocHC)
model2_hom <- coeftest(model2, vcov = vcocHC)
model3_hom <- coeftest(model3, vcov = vcocHC)
model4_hom <- coeftest(model4, vcov = vcocHC)
model5_hom <- coeftest(model5, vcov = vcocHC)
model6_hom <- coeftest(model6, vcov = vcocHC)
model7_hom <- coeftest(model7, vcov = vcovHC)

# printing out the models in correct format
stargazer(model1_hom, model2_hom, model3_hom, title = "Regression Models with Controls", 
          type = "text", report = "vc*t", keep.stat=c(), 
          covariate.labels = c("Stock payment", "Public target", "Stock payment x Public target"),
          dep.var.labels = "Acquirer’s cumulative abnormal return (CAR)")

stargazer(model4_hom, model5_hom, model6_hom, model7_hom, title = "Regression Models with Controls", 
          type = "text", report = "vc*t", 
          dep.var.labels = "Acquirer’s cumulative abnormal return (CAR)", 
          covariate.labels = c("Stock payment", "Public target", "Bidder market value", "Bidder volatility", 
                               "Stock payment x Public target", "Public target x Bidder market value", "Public target x Bidder volatility"))

# Check the distribution of 'hostile' within the subset where 'private == 1'
table(subset(df, private == 1)$hostile)

table(subset(df, public == 1)$hostile)

# Check summary statistics
summary(subset(df, private == 1)$hostile)

