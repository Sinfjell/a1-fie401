# Create an empty list to store the robust standard errors
se <- list()

# Loop through each model to calculate robust standard errors
models <- list(model1, model2, model3, model2_1, model4, model5, model6, model7)
for (i in 1:length(models)) {
  se[[i]] <- coeftest(models[[i]], vcov = vcovHC(models[[i]], type = "HC3"))[, 2]
}

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
compare_t_values(model4)
compare_t_values(model5)
compare_t_values(model6)
compare_t_values(model7)