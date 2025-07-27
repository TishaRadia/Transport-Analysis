# Load required libraries
library(tidyverse)
library(lubridate)
library(cluster)
library(factoextra)
library(tidymodels)
library(ranger)

# Read data
data1 <- read_csv("./data/bandedvalidations2024-01-02-03.csv")
data2 <- read_csv("./data/bandedvalidations2024-04-05-06.csv")

transport_data <- bind_rows(data1, data2)

weather_data <- read_csv("./data/IDCJCM0034_023000.csv", skip = 10)  # Skip BOM metadata

# Clean transport data
transport_data <- transport_data %>%
  mutate(
    Date = parse_date_time(VALIDATION_DATE, orders = c("ymd", "dmy", "mdy")),
    Month = month(Date, label = TRUE),
    Year = year(Date),
    BAND_BOARDINGS = str_extract(BAND_BOARDINGS, "\\d+-\\d+"),
    Band_Low = as.numeric(str_extract(BAND_BOARDINGS, "^\\d+")),
    Band_High = as.numeric(str_extract(BAND_BOARDINGS, "\\d+$")),
    Avg_Boardings = (Band_Low + Band_High) / 2
  )

# Summarise monthly boardings
transport_monthly <- transport_data %>%
  group_by(Year, Month) %>%
  summarise(
    Total_Boardings = sum(Avg_Boardings, na.rm = TRUE),
    .groups = 'drop'
  )

# Extract weather monthly max temperature row and format
weather_row <- weather_data[1, 2:13] %>%
  pivot_longer(everything(), names_to = "Month", values_to = "Temperature") %>%
  mutate(Month = factor(Month, levels = month.name))

transport_monthly <- transport_monthly %>%
  mutate(Month = month.name[match(Month, month.abb)])
# Merge transport and weather data
combined_data <- transport_monthly %>%
  left_join(weather_row, by = "Month")

combined_data <- combined_data %>%
  mutate(Temperature = as.numeric(as.character(Temperature)))

combined_data <- combined_data %>%
  mutate(Total_Boardings = as.numeric(Total_Boardings))


# ----- Regression Analysis -----
combined_data_clean <- combined_data %>%
  filter(!is.na(Total_Boardings) & !is.na(Temperature))

# Scale cleaned data
clust_data <- combined_data_clean %>%
  select(Total_Boardings, Temperature) %>%
  scale()

reg_model <- lm(Total_Boardings ~ Temperature, data = combined_data)
summary(reg_model)

# Regression Plot
ggplot(combined_data, aes(x = Temperature, y = Total_Boardings)) +
  geom_point(size = 3, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Linear Regression: Boardings vs Temperature",
    x = "Mean Max Temperature (°C)",
    y = "Total Monthly Boardings"
  ) +
  theme_minimal()

# ----- K-means Clustering -----
# Scale data
clust_data <- combined_data %>%
  select(Total_Boardings, Temperature) %>%
  scale() %>%
  as.data.frame()

# Determine optimal clusters (elbow method)
max_k <- nrow(clust_data) - 1  # max possible clusters
fviz_nbclust(clust_data, kmeans, method = "wss", k.max = max_k) +
  labs(title = "Elbow Method for Optimal K")
# Apply k-means (choose 3 clusters for example)
kmeans_model <- kmeans(clust_data, centers = 3, nstart = 25)
combined_data$Cluster <- as.factor(kmeans_model$cluster)

# Cluster Visualization
ggplot(combined_data, aes(x = Temperature, y = Total_Boardings, color = Cluster)) +
  geom_point(size = 4) +
  labs(
    title = "K-means Clustering: Temperature vs Boardings",
    x = "Mean Max Temperature (°C)",
    y = "Total Monthly Boardings"
  ) +
  theme_minimal()

#Hierarchical Clustering -----
dist_matrix <- dist(clust_data)
hc <- hclust(dist_matrix, method = "ward.D2")

fviz_dend(hc, k = 3, rect = TRUE) +
  labs(title = "Hierarchical Clustering Dendrogram")

#-----------------------------------------
model_data <- combined_data_clean %>%
  select(Total_Boardings, Temperature)

# Split data
set.seed(123)
data_split <- initial_split(model_data, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# Create recipe
rf_recipe <- recipe(Total_Boardings ~ Temperature, data = train_data)

# Define random forest model
rf_model <- rand_forest(
  mode = "regression",
  trees = 500,
  mtry = 1,
  min_n = 5
) %>%
  set_engine("ranger")

# Build workflow
rf_workflow <- workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_model)

# Fit model
rf_fit <- rf_workflow %>% fit(data = train_data)

# Predict and evaluate
rf_predictions <- rf_fit %>%
  predict(new_data = test_data) %>%
  bind_cols(test_data)

# Performance metrics
rf_metrics <- rf_predictions %>%
  metrics(truth = Total_Boardings, estimate = .pred)
print(rf_metrics)

# Plot actual vs predicted
ggplot(rf_predictions, aes(x = Total_Boardings, y = .pred)) +
  geom_point(color = "blue", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Random Forest: Actual vs Predicted Boardings",
    x = "Actual Boardings",
    y = "Predicted Boardings"
  ) +
  theme_minimal()
#-----------------------------------------
# Create Transport Type label if not already present
transport_data <- transport_data %>%
  mutate(
    TRANSPORT_TYPE = case_when(
      NUM_MODE_TRANSPORT == 1 ~ "Bus",
      NUM_MODE_TRANSPORT == 5 ~ "Train",
      NUM_MODE_TRANSPORT == 4 ~ "Tram",
      TRUE ~ "Other"
    )
  )

# Group and summarise monthly boardings
monthly_boardings_by_type <- transport_data %>%
  group_by(Month, TRANSPORT_TYPE) %>%
  summarise(
    Total_Avg_Boardings = sum(Avg_Boardings, na.rm = TRUE),
    .groups = 'drop'
  )

# Grouped bar chart
ggplot(monthly_boardings_by_type, aes(x = Month, y = Total_Avg_Boardings, fill = TRANSPORT_TYPE)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Monthly Boardings by Transport Type",
    x = "Month",
    y = "Number of Boardings (Band Midpoint Avg)"
  ) +
  theme_minimal()

# Create a date label for time series plot
transport_monthly <- transport_monthly %>%
  mutate(Month_Year = paste(Year, Month, sep = "-"))

ggplot(transport_monthly, aes(x = Month_Year, y = Total_Boardings, group = 1)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkblue", size = 3) +
  labs(
    title = "Monthly Total Boardings Over Time",
    x = "Year-Month",
    y = "Total Average Boardings"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

summary_stats <- combined_data_clean %>%
  summarise(
    Min_Temp = min(Temperature, na.rm = TRUE),
    Max_Temp = max(Temperature, na.rm = TRUE),
    Avg_Temp = mean(Temperature, na.rm = TRUE),
    Min_Boardings = min(Total_Boardings, na.rm = TRUE),
    Max_Boardings = max(Total_Boardings, na.rm = TRUE),
    Avg_Boardings = mean(Total_Boardings, na.rm = TRUE)
  )

print(summary_stats)
