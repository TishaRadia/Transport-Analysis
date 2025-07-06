# Load required libraries
library(tidyverse)
library(lubridate)
library(cluster)
library(factoextra)

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

