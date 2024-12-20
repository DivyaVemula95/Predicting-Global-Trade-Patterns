h2o.ls()
# Remove all objects from the cluster to ensure it's clean
h2o.removeAll()
# Shutdown the H2O cluster
h2o.shutdown(prompt = FALSE)

# Installing and loading required packages using the outlined procedure
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('h2o')) install.packages('h2o'); library('h2o')
if (!require('kableExtra')) install.packages('kableExtra'); library('kableExtra')
if (!require('DALEXtra')) install.packages('DALEXtra'); library('DALEXtra')
if (!require('skimr')) install.packages('skimr'); library('skimr')
if (!require('recipes')) install.packages('recipes'); library('recipes')
if (!require('janitor')) install.packages('janitor'); library('janitor')
if (!require('caret')) install.packages('caret'); library('caret')
if (!require('stringr')) install.packages('stringr'); library('stringr')
if (!require('DALEX')) install.packages('DALEX'); library('DALEX')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('httr')) install.packages('httr'); library('httr')
if (!require('jsonlite')) install.packages('jsonlite'); library('jsonlite')
if (!require('tibble')) install.packages('tibble'); library('tibble')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('tidyr')) install.packages('tidyr'); library('tidyr')

# Step 1: Data Collection
api_url <- "https://api.census.gov/data/timeseries/intltrade/exports/hs?get=DISTRICT,DIST_NAME,E_COMMODITY,E_COMMODITY_LDESC,ALL_VAL_MO,ALL_VAL_YR,VES_VAL_MO,VES_VAL_YR,AIR_VAL_MO,AIR_VAL_YR,CC_YR,QTY_1_YR,QTY_2_YR,CTY_CODE,CTY_NAME,COMM_LVL,DF,LAST_UPDATE,YEAR,MONTH,VES_WGT_YR&YEAR=2013&MONTH=12&DISTRICT=13"
response <- GET(api_url, timeout(60))

if (status_code(response) == 200) {
  data <- fromJSON(content(response, "text"))
  headers <- data[1, ]
  records <- data[-1, ]
  
  # Convert to tibble and assign meaningful headers
  meaningful_headers <- c(
    "District Code", "District Name", "Export Commodity Code", 
    "Export Commodity Long Description", "Total Monthly Export Value", 
    "Total Year-to-Date Export Value", "Monthly Vessel Export Value", 
    "Year-to-Date Vessel Export Value", "Monthly Air Export Value", 
    "Year-to-Date Air Export Value", "Year-to-Date Card Count", 
    "Quantity 1 Year-to-Date", "Quantity 2 Year-to-Date", 
    "Country Code", "Country Name", "Commodity Level", 
    "Domestic/Foreign Indicator", "Last Update", "Year", 
    "Month", "Year-to-Date Vessel Weight"
  )
  data_table <- as_tibble(records)
  colnames(data_table) <- meaningful_headers
  print("Data successfully collected and loaded!")
} else {
  stop(paste("Error:", status_code(response)))
}

# Clean column names to ensure they are valid
data_table <- data_table %>% clean_names()

# Limit the data to 20,000 records
if (nrow(data_table) > 20000) {
  data_table <- data_table %>% sample_n(20000)  # Randomly sample 20,000 records
}

# Print the total number of records 
total_records <- nrow(data_table)  
print(paste("Total number of records:", total_records))

# Step 2: Data Wrangling
# Check for missing values
missing_values <- sapply(data_table, function(x) sum(is.na(x)))
print(missing_values)

# Drop rows with missing data
data_table <- data_table %>% drop_na()

# Convert categorical variables to factors
data_table <- data_table %>%
  mutate(across(where(is.character), as.factor))

# Normalize numeric variables
numeric_cols <- data_table %>%
  select(where(is.numeric)) %>%
  colnames()
data_table <- data_table %>%
  mutate(across(all_of(numeric_cols), ~ (.-min(.))/(max(.)-min(.))))

# Filter columns to meet 20 predictors requirement
selected_columns <- c(
  "district_code", "district_name", "export_commodity_code", 
  "export_commodity_long_description", "total_monthly_export_value", 
  "total_year_to_date_export_value", "monthly_vessel_export_value", 
  "year_to_date_vessel_export_value", "monthly_air_export_value", 
  "year_to_date_air_export_value", "year_to_date_card_count", 
  "quantity_1_year_to_date", "quantity_2_year_to_date", 
  "country_code", "country_name", "commodity_level", 
  "domestic_foreign_indicator", "last_update", "year", 
  "month", "year_to_date_vessel_weight"
)
data_table <- data_table %>% select(all_of(selected_columns))
data_table <- data_table %>% clean_names()

final_data <- data_table
write.csv(final_data, "TeamProject-Group1-PredictingGlobalTradePatterns.csv", row.names = FALSE)

# Initialize H2O
h2o.init(nthreads = -1) #-1 to use all cores
# Convert the cleaned data frame to H2O object
h2o_df <- as.h2o(data_table)

# Clean column names to ensure they are valid for H2O
colnames(h2o_df) <- gsub(" ", "_", colnames(h2o_df))  # Replace spaces with underscores
colnames(h2o_df) <- tolower(colnames(h2o_df))  # Convert to lowercase

# Check cleaned column names
print(colnames(h2o_df))

# **Ensure the target variable is numeric** before applying the log transformation
h2o_df$total_monthly_export_value <- as.numeric(h2o_df$total_monthly_export_value)

# Apply log transformation to reduce the impact of outliers
h2o_df$total_monthly_export_value <- log1p(h2o_df$total_monthly_export_value)

# Convert the target variable to numeric to ensure it is treated as regression
h2o_df$total_monthly_export_value <- as.numeric(h2o_df$total_monthly_export_value)

# Split the dataset into training and testing sets (80% train, 20% test)
splits <- h2o.splitFrame(data = h2o_df, ratios = 0.8, seed = 123)
train_h2o <- splits[[1]] # from training data
test_h2o <- splits[[2]] # from training data


# Define predictors and outcome variable
predictors <- c("district_code", "export_commodity_code", "export_commodity_long_description", 
                "total_year_to_date_export_value", "monthly_vessel_export_value", 
                "year_to_date_vessel_export_value", "monthly_air_export_value", 
                "year_to_date_air_export_value", "year_to_date_card_count", 
                "quantity_1_year_to_date", "quantity_2_year_to_date", 
                "country_code", "country_name", "commodity_level", 
                "domestic_foreign_indicator", "year_to_date_vessel_weight")
outcome <- "total_monthly_export_value"

# Train Deep Learning Model using the specified hyperparameters
dl_model <- h2o.deeplearning(
  model_id = "TeamProject-Group1-PredictingGlobalTradePatterns",
  x = predictors,
  y = outcome,
  training_frame = train_h2o,
  validation_frame = test_h2o,
  overwrite_with_best_model = F,    # Return the final model after 10 epochs, even if not the best
  hidden = c(128, 128, 128),       # More hidden layers for more complex interactions
  epochs = 50,                     # To keep it short enough
  score_validation_samples = 10000, # Downsample validation set for faster scoring
  score_duty_cycle = 0.025,        # Don't score more than 2.5% of the wall time
  adaptive_rate = F,               # Manually tuned learning rate
  rate = 0.01,                     # Learning rate
  rate_annealing = 2e-6,           # Learning rate annealing
  momentum_start = 0.2,            # Manually tuned momentum
  momentum_stable = 0.4,           # Momentum during training
  momentum_ramp = 1e7,             # Ramp up momentum
  l1 = 1e-5,                       # L1 regularization
  l2 = 1e-5,                       # L2 regularization
  max_w2 = 10                      # Helps stability for Rectifier activation
)

# Model summary of the trained model
summary(dl_model)

# Print model performance on the test set
performance <- h2o.performance(dl_model, newdata = test_h2o)
print(performance)

# Get R-squared value for the model
rsq <- performance@metrics$r2
print(paste("R-squared: ", rsq))

# Save the trained model to a file
# Saving the h2o model
saved_model_path <- h2o.saveModel(object = dl_model, 
                                  path = getwd(), 
                                  force = TRUE,
                                  filename = 'TeamProject-Group1-PredictingGlobalTradePatterns.h2o'
                                  )
print(paste("Model saved at: ", saved_model_path))

# Clean up H2O cluster
h2o.removeAll()
h2o.shutdown(prompt = FALSE)

