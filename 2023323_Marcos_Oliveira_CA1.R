### CA1 - 2023323 Marcos Vinicius de Oliveira

### 1. Setting the path in RStudio:
# getwd()
# setwd("/home/marcos/Documents/CCT College/3 Data Exploration & Preparation/1 Semester/CA1 - 40%")


# -----------------------------------------------------------


### 2. Remove all variables and Load Data
# it removes all variables stores previously
rm(list = ls())

# Loading the data and checking the structure of the dataframe
covid19_vaccination_EU_EEA_df <- read.csv("covid19_vaccination_EU_EEA.csv")


# ------------------------------------------------------------


### 3. Installing the packages (if not yet)
install.packages("tidyverse")
install.packages("dplyr")
install.packages('ggthemes')
install.packages("plotly")
install.packages('scales')
install.packages("fastDummies")


# -----------------------------------------------------------


### 4. Loading the libraries
library(Hmisc)
library(readr)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)
library(forcats)
library(ggthemes)
library(plotly)
library(scales)
library(fastDummies)


# -----------------------------------------------------------


### 5. Describe and open the dataset 
# Describe this dataset (it may take some time)
describe(covid19_vaccination_EU_EEA_df)

# Quick look at the dataset in the next tab
View(covid19_vaccination_EU_EEA_df)


# -----------------------------------------------------------


### 6. Data Cleaning
##  6.1 Validating Data Types:
# Checking if the dataframe has correct data types (yeah it has)
str(covid19_vaccination_EU_EEA_df)


##  6.2 Missing Values:
# Showing missing values in each column
covid19_vaccination_EU_EEA_df %>% summarise_all(funs(sum(is.na(.))))

# Counting missing values in each column
missing_count <- covid19_vaccination_EU_EEA_df %>% 
  summarise(across(everything(), ~sum(is.na(.))))

# Filling missing values with mean for numeric columns only
covid19_vaccination_EU_EEA_df <- covid19_vaccination_EU_EEA_df %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Displaying all variables with no missing values after imputation
no_missing_post_imputation <- covid19_vaccination_EU_EEA_df %>% 
  summarise(across(everything(), ~sum(is.na(.))))

# No missing value using describe:
describe(covid19_vaccination_EU_EEA_df)


##  6.3 Normalize Data:
# Normalizing the FirstDose and SecondDose columns
covid19_vaccination_EU_EEA_df$FirstDosePerCapita <- covid19_vaccination_EU_EEA_df$FirstDose / covid19_vaccination_EU_EEA_df$Population
covid19_vaccination_EU_EEA_df$SecondDosePerCapita <- covid19_vaccination_EU_EEA_df$SecondDose / covid19_vaccination_EU_EEA_df$Population

# Display the first few rows of the new normalized columns Per Capita
head(covid19_vaccination_EU_EEA_df[c("ReportingCountry", "FirstDosePerCapita", "SecondDosePerCapita")])


# -----------------------------------------------------------


### 7. Identifying variables, discrete, continuous using plot and exploring missing values for any of the variables
# Visualizing a Categorical Variable (e.g., Vaccine)
ggplot(covid19_vaccination_EU_EEA_df, aes(x = Vaccine)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Vaccines", x = "Vaccine", y = "Count")

# Visualizing a Discrete Variable (e.g., FirstDose)
ggplot(covid19_vaccination_EU_EEA_df, aes(x = FirstDose)) +
  geom_histogram(bins = 30) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(title = "Distribution of First Doses Administered",
       x = "First Dose",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 12))

# Visualizing a Continuous Variable (e.g., FirstDosePerCapita)
ggplot(covid19_vaccination_EU_EEA_df, aes(x = FirstDosePerCapita)) +
  geom_histogram(bins = 30) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(title = "Distribution of First Dose Per Capita", x = "First Dose Per Capita", y = "Count")

# Visualizing Missing Values using gather and row_number (IT MIGHT TAKE SOME TIME TO LOAD)
covid19_vaccination_EU_EEA_df %>% 
  select(YearWeekISO, ReportingCountry, FirstDose, SecondDose, Vaccine) %>% 
  gather(key = "variable", value = "value") %>% 
  mutate(is_missing = is.na(value)) %>%
  mutate(row_id = row_number()) %>%
  ggplot(aes(x = variable, y = row_id)) +
  geom_tile(aes(fill = is_missing), color = "black") +
  scale_fill_viridis_d(name = "Missing", labels = c("Present", "Missing")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "Heatmap of Missing Values", x = "Variable", y = "Row")


# -----------------------------------------------------------


### 8. Statistical parameters mean, median, minimum, maximum, and standard deviation
# Numerical variables
numerical_columns <- c("Denominator", "NumberDosesReceived", "NumberDosesExported", 
                       "FirstDose", "FirstDoseRefused", "SecondDose", 
                       "DoseAdditional1", "DoseAdditional2", "DoseAdditional3", 
                       "DoseAdditional4", "DoseAdditional5", "UnknownDose", 
                       "Population")

# Using dplyr to summarise each numerical column
statistics <- covid19_vaccination_EU_EEA_df %>% 
  select(all_of(numerical_columns)) %>% 
  summarise_all(list(
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE)
  ))

# View the summarise each numerical column in the next tab
View(statistics)

# Convert the wide format statistics to a long format
statistics_long <- statistics %>%
  pivot_longer(cols = everything(), names_to = "statistic_variable", values_to = "value") %>%
  separate(statistic_variable, into = c("statistic", "variable"), sep = "_") %>%
  pivot_wider(names_from = statistic, values_from = value)

# View the transformed statistics in the next tab
View(statistics_long)


# -----------------------------------------------------------


### 9. Min-Max Normalization, Z-score Standardization and Robust scalar
# Min-Max Normalization
covid19_vaccination_EU_EEA_df_normalized <- covid19_vaccination_EU_EEA_df %>% 
  mutate(across(all_of(numerical_columns), ~(. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE)), .names = "minmax_{.col}"))

# Min-Max Normalization in the next tab
View(covid19_vaccination_EU_EEA_df_normalized)

# Z-score Standardization
covid19_vaccination_EU_EEA_df_standardized <- covid19_vaccination_EU_EEA_df %>% 
  mutate(across(all_of(numerical_columns), ~(. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE), .names = "z_{.col}"))

# Z-score Standardization in the next tab
View(covid19_vaccination_EU_EEA_df_standardized)

# Robust Scaling
covid19_vaccination_EU_EEA_df_robust <- covid19_vaccination_EU_EEA_df %>% 
  mutate(across(all_of(numerical_columns), ~(. - median(., na.rm = TRUE)) / IQR(., na.rm = TRUE), .names = "robust_{.col}"))

# Robust Scaling in the next tab
View(covid19_vaccination_EU_EEA_df_robust)


# -----------------------------------------------------------


### 10. Line, Scatter and Heatmaps used to show the correlation of the dataset.
## 10.1 Line Plot
# Extract year and week number from YearWeekISO
covid19_vaccination_EU_EEA_df$Week <- as.numeric(gsub(".*W", "", covid19_vaccination_EU_EEA_df$YearWeekISO))

# Aggregate total first doses by week for each country
first_doses_by_country_week <- covid19_vaccination_EU_EEA_df %>%
  group_by(ReportingCountry, Week) %>%
  summarize(TotalFirstDose = sum(FirstDose, na.rm = TRUE), .groups = 'drop') %>%
  arrange(ReportingCountry, Week)

# Using a color palette
color_palette <- viridis::scale_color_viridis(discrete = TRUE, option = "D")

# Line plot for total first doses by week for each country
plot <- ggplot(first_doses_by_country_week, aes(x = Week, y = TotalFirstDose, group = ReportingCountry, color = ReportingCountry)) +
  geom_line() +
  labs(title = "Total First Doses Administered Over Time by Country", x = "Week", y = "Total First Doses") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  color_palette

# Convert to an interactive plot using plotly
interactive_plot <- ggplotly(plot)

# Render the interactive plot
interactive_plot


## 10.2 Scatter Plot
# Scatter plot for first vs. second doses
ggplot(covid19_vaccination_EU_EEA_df, aes(x = FirstDose, y = SecondDose)) +
  geom_point(alpha = 0.5) +
  labs(title = "Relationship between First and Second Doses Administered", x = "First Dose", y = "Second Dose") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)


## 10.3 Heatmap
# Calculate the correlation matrix
correlation_matrix <- covid19_vaccination_EU_EEA_df %>%
  select(all_of(numerical_columns)) %>%
  cor(use = "complete.obs")

# Melt the correlation matrix to long format
correlation_matrix_melted <- melt(correlation_matrix, na.rm = TRUE)

ggplot(data = correlation_matrix_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1),
        axis.text.y = element_text(size = 10)) +
  labs(x = '', y = '', title = "Correlation Matrix Heatmap")


# -----------------------------------------------------------


### 11. Data Exploratory Analysis (EDA) - Graphics and Descriptive Insights
## 11.1 Distribution Plots
# Histogram for FirstDose
ggplot(covid19_vaccination_EU_EEA_df, aes(x = FirstDose)) +
  geom_histogram(bins = 30, fill = "cornflowerblue") +
  labs(title = "Histogram of First Doses Administered", x = "First Dose", y = "Frequency")

# Boxplot for FirstDose
ggplot(covid19_vaccination_EU_EEA_df, aes(y = FirstDose, x = 1)) +
  geom_boxplot(fill = "slateblue") +
  labs(title = "Boxplot of First Doses Administered", x = "", y = "First Dose")


## 11.2 Subgroups
# Bar plot for Vaccine types
ggplot(covid19_vaccination_EU_EEA_df, aes(x = Vaccine)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Vaccine Types", x = "Vaccine", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#########################################   Grouped bar plot for Vaccine types by TargetGroup
ggplot(covid19_vaccination_EU_EEA_df, aes(x = Vaccine, fill = TargetGroup)) +
  geom_bar(position = "dodge") +
  labs(title = "Vaccine Types by Target Group", x = "Vaccine", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## 11.3 Time Series Analysis
# Time series line plot for FirstDose by ReportingCountry
ggplot(covid19_vaccination_EU_EEA_df, aes(x = YearWeekISO, y = FirstDose, group = ReportingCountry, color = ReportingCountry)) +
  geom_line() +
  labs(title = "First Dose Administration Over Time by Country", x = "Time", y = "First Dose")


## 11.4 Correlation Analysis
# Calculate the correlation matrix and plot a heatmap
correlation_matrix <- covid19_vaccination_EU_EEA_df %>%
  select(all_of(numerical_columns)) %>%
  cor(use = "complete.obs")

# Melt the correlation matrix to long format for plotting
correlation_matrix_melted <- melt(correlation_matrix, na.rm = TRUE)

# Plot the heatmap
ggplot(correlation_matrix_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text.y = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap")


## 11.5 Dummy Encoding in one Categorical Variable
# Applying dummy encoding to the Vaccine column
covid19_vaccination_EU_EEA_df_dummies <- covid19_vaccination_EU_EEA_df %>%
  dummy_cols(select_columns = "Vaccine", remove_selected_columns = TRUE)

View(covid19_vaccination_EU_EEA_df_dummies)


# -----------------------------------------------------------


### 12. Performing (PCA)
## 12.1 Preparing the Data
# Selecting only numerical columns for PCA
numerical_data <- covid19_vaccination_EU_EEA_df %>% 
  select(Denominator, NumberDosesReceived, NumberDosesExported, FirstDose, FirstDoseRefused,
         SecondDose, DoseAdditional1, DoseAdditional2, DoseAdditional3, 
         DoseAdditional4, DoseAdditional5, UnknownDose, Population)

head(numerical_data)


## 12.2 Applying PCA
# Perform PCA with data standardization
pca_result <- prcomp(numerical_data, center = TRUE, scale. = TRUE)

# View summary of PCA results
summary(pca_result)
# The first principal component (PC1) captures the most variance (about 18.17%).
# The first two components (PC1 and PC2) together capture about 31.12% of the total variance.
# The first three components (PC1, PC2, PC3) capture about 41.74% of the total variance, and so on.


## 12.3 Exploring Component Loadings
# Viewing component loadings
pca_loadings <- pca_result$rotation
print(pca_loadings)


## 12.4 Visualizing PCA Results
# Create a data frame for the PCA scores
pca_scores <- as.data.frame(pca_result$x)

# Scatter plot of the first two principal components
ggplot(pca_scores, aes(x = PC1, y = PC2, color = as.factor(PC1 > 0))) +
  geom_point() +
  labs(title = "PCA - First Two Principal Components", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()



