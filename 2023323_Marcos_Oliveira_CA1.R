### CA1 - 2023323 Marcos Vinicius de Oliveira

### Setting the path in RStudio:
# getwd()
# setwd("/home/marcos/Documents/CCT College/3 Data Exploration & Preparation/1 Semester/CA1 - 40%")


# -----------------------------------------------------------


### Remove all variables and Load Data
# it removes all variables stores previously
rm(list = ls())

# Loading the data and checking the structure of the dataframe
covid19_vaccination_EU_EEA_df <- read.csv("covid19_vaccination_EU_EEA.csv")


# ------------------------------------------------------------


### Installing the packages
install.packages('Hmisc')
install.packages('caret')
install.packages('FactoMineR')


# -----------------------------------------------------------


### Loading the libraries
library(Hmisc)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(caret)
library(FactoMineR)

# Describe this dataset
describe(covid19_vaccination_EU_EEA_df)


# ------------------------------------------------------------------------------


### Characterization
## 1. Checking Data Types 
# Checking if the dataframe has correct data types
str(covid19_vaccination_EU_EEA_df)


## 2. Missing Values:
# Counting missing values in each column
missing_values <- covid19_vaccination_EU_EEA_df %>% 
  summarise(across(everything(), ~sum(is.na(.))))

# Checking the variables
head(missing_values)

# Filling missing values with mean for numeric columns only
covid19_vaccination_EU_EEA_df <- covid19_vaccination_EU_EEA_df %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Storing these counts in a new object
no_missing_post_imputation <- covid19_vaccination_EU_EEA_df %>% 
  summarise(across(everything(), ~sum(is.na(.))))

# Displaying no missing values after imputation
head(no_missing_post_imputation)


##  3. Normalize Data:
# Normalizing the FirstDose and SecondDose columns
covid19_vaccination_EU_EEA_df$FirstDosePerCapita <- covid19_vaccination_EU_EEA_df$FirstDose / covid19_vaccination_EU_EEA_df$Population
covid19_vaccination_EU_EEA_df$SecondDosePerCapita <- covid19_vaccination_EU_EEA_df$SecondDose / covid19_vaccination_EU_EEA_df$Population

# Display the first few rows of the new normalized columns Per Capita
head(covid19_vaccination_EU_EEA_df[c("ReportingCountry", "FirstDosePerCapita", "SecondDosePerCapita")])


# ------------------------------------------------------------------------------


### Identification
## 1. First-Dose Vaccination Rates Per Capita by Country
ggplot(covid19_vaccination_EU_EEA_df, aes(x = ReportingCountry, y = FirstDosePerCapita)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  scale_y_continuous(labels = scales::label_number(), limits = c(NA, NA)) +
  labs(title = "First Dose Vaccination Rates Per Capita by Country",
       y = "First Dose Per Capita",
       x = "Country") +
  theme_minimal()


## 2. Distribution of Vaccines
ggplot(covid19_vaccination_EU_EEA_df, aes(x = Vaccine, fill = Vaccine)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "bottom") + # Move legend to the bottom
  scale_fill_discrete(name = "Vaccine Type") + # Optional: Rename the legend title
  labs(title = "Distribution of Vaccines", y = "Count", x = "Vaccine") +
  theme_minimal()


## 3. First Dose Vaccination Rates Per Capita by Target Group
ggplot(covid19_vaccination_EU_EEA_df, aes(x = TargetGroup, y = FirstDosePerCapita)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  labs(title = "First Dose Vaccination Rates Per Capita by Target Group",
       y = "First Dose Per Capita",
       x = "Target Group") +
  theme_minimal()


# ------------------------------------------------------------------------------


### Encoding Schema and PCA
## One-Hot Encoding Schema
# Using 'ReportingCountry', 'Region', 'TargetGroup', and 'Vaccine' and converting categorical variables to factors
covid19_vaccination_EU_EEA_df$ReportingCountry <- as.factor(covid19_vaccination_EU_EEA_df$ReportingCountry)
covid19_vaccination_EU_EEA_df$Region <- as.factor(covid19_vaccination_EU_EEA_df$Region)
covid19_vaccination_EU_EEA_df$Vaccine <- as.factor(covid19_vaccination_EU_EEA_df$Vaccine)
covid19_vaccination_EU_EEA_df$TargetGroup <- as.factor(covid19_vaccination_EU_EEA_df$TargetGroup)

# Using model.matrix to create a one-hot encoding for the categorical variables
encoded_df <- model.matrix(~ ReportingCountry + Region + TargetGroup + Vaccine - 1, data = covid19_vaccination_EU_EEA_df)

# Bind the encoded data with the original numeric data
numeric_data <- covid19_vaccination_EU_EEA_df %>% select(FirstDose, Population, FirstDosePerCapita, SecondDosePerCapita)
prepared_df <- cbind(numeric_data, encoded_df)

# View the first few rows of the prepared dataframe
head(prepared_df)

# Check data types before PCA
str(prepared_df_for_pca)


### PCA
## Applying PCA
# prcomp() automatically centers and scales the data
pca_result <- prcomp(prepared_df, center = TRUE, scale. = TRUE)

# Summarizing PCA results
pca_summary <- summary(pca_result)

# Scree plot to visualize the importance of the components
plot(pca_result, type = "l", main = "Scree Plot")

# Biplot to visualize the principal components and the observations (it might take some time)
biplot(pca_result, main = "PCA Biplot")

# Interpreting the PCA results
# The proportion of variance explained by the principal components
pca_summary$importance

# The loadings of the first few principal components can be inspected to determine
# which variables contribute most to each component.
loadings <- pca_result$rotation