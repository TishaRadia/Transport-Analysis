# Adelaide Public Transport & Weather Data Analysis

This project analyses **public transport ridership in Adelaide** and its relationship with **weather patterns**.  
It applies **data cleaning, regression, clustering, and machine learning models** to uncover insights.

---

##  Project Structure
- `data/` → Folder containing transport and weather CSV datasets.
- `analysis.R` → Main R script (data cleaning, regression, clustering, ML models).
- `README.md` → Project documentation.

---

## Key Analyses
1. **Data Cleaning**  
   - Combines multiple transport CSV files.  
   - Extracts monthly averages from banded boarding data.  
   - Merges with Bureau of Meteorology (BOM) weather data.

2. **Regression Analysis**  
   - Examines relationship between temperature and boardings.  
   - Linear regression with plots.

3. **Clustering**  
   - K-means and hierarchical clustering on transport/weather features.  
   - Visualizations of clusters and dendrogram.

4. **Random Forest Model**  
   - Predicts boardings based on temperature.  
   - Evaluates performance using RMSE, MAE, R².  
   - Actual vs predicted plots.

5. **Transport Type Analysis**  
   - Breaks down ridership by bus, train, tram.  
   - Monthly trends and grouped bar charts.

6. **Time Series Analysis**  
   - Visualizes monthly total boardings over time.

7. **Summary Statistics**  
   - Key descriptive stats for temperature and ridership.

---

## Requirements
The project uses **R** with the following packages:
```r
tidyverse
lubridate
cluster
factoextra
tidymodels
ranger
