# House Rent Price Analysis in Major Indian Cities

[![Language: R](https://img.shields.io/badge/Language-R-276DC3.svg?style=for-the-badge&logo=r)](https://www.r-project.org/)
[![Libraries: Tidyverse](https://img.shields.io/badge/Libraries-Tidyverse-1E90FF.svg?style=for-the-badge)](https://www.tidyverse.org/)
[![Project Status](https://img.shields.io/badge/Status-Complete-brightgreen.svg?style=for-the-badge)](https://www.repostatus.org/#inactive)

---

## 1. Project Overview

This project presents a comprehensive data analysis of the house rental market across six major cities in India: **Mumbai, Bangalore, Chennai, Delhi, Hyderabad, and Kolkata**. Using a detailed dataset of rental properties, this analysis aims to uncover the key factors that influence rental prices.

The study explores the relationships between rental costs and various property attributes, including location, size, furnishing status, and floor level, providing valuable insights for tenants, landlords, and real estate stakeholders.

---

## 2. Key Research Objectives

Our analysis was structured around four primary objectives:

1.  **To analyze the relationship between a property's location (City, Area Type) and its rental price.**
2.  **To investigate the impact of property size (Area) on rental prices.**
3.  **To determine the effect of a property's furnishing status on its rental value.**
4.  **To understand the influence of a property's floor level on its rental price.**

---

## 3. Methodology

The project followed a structured data analysis workflow to ensure robust and reliable findings:

1.  **Data Import & Cleaning:** The initial dataset was imported, and a thorough cleaning process was conducted. This included handling inconsistencies (e.g., standardizing `Contact_Point` values) and removing significant outliers based on rental price and area to ensure data quality.
2.  **Data Pre-processing & Feature Engineering:** New, more useful features were created from existing data. A key step was categorizing the `Flooring` variable into broader groups (`Lower Floor`, `Medium Floor`, `Upper Floor`) to facilitate clearer analysis.
3.  **Exploratory Data Analysis (EDA):** Visualizations such as scatter plots and box plots were used to explore initial relationships between variables and identify patterns.
4.  **Statistical Analysis:** A combination of descriptive statistics (mean, median), visualizations (`ggplot2`), and statistical models (ANOVA, Linear Regression) were used to test hypotheses and quantify the impact of different factors on rental prices.

---

## 4. Key Findings

The analysis confirmed that rental prices are significantly influenced by a combination of factors.

* **Location is Paramount:**
    * **Mumbai** has the highest average rental prices by a significant margin compared to the other five cities.
    * **Kolkata** consistently shows the lowest average rental prices.

* **Size Matters:**
    * There is a strong positive correlation between the `Area` of a property and its `Rental_Price`. Larger properties consistently command higher rents across all cities.

* **Furnishing Status Impacts Price:**
    * **Furnished** properties have the highest average rent.
    * **Semi-Furnished** properties are the next most expensive and are the most common listing type, suggesting a market preference for a balance of convenience and flexibility.
    * **Unfurnished** properties are the most affordable.

* **Floor Level Influences Rent:**
    * Properties on **Upper Floors** generally have the highest rental prices, followed by Medium and Lower floors.
    * This trend was consistent across most cities, though some, like Kolkata and Delhi, had limited data for upper-floor properties, affecting the analysis for those specific locations.

![Average Rental Price by Floor Type](./assets/average_rental_by_floor)
*(Chart showing the significant difference in Average Rental Price by Floor Type)*

---

## 5. Technology Stack

* **Programming Language:** R
* **Key R Packages:**
    * `tidyverse` (including `ggplot2` for visualization and `dplyr` for data manipulation)
    * `stringr`
    * `plotly`

---

## 6. How to Replicate This Analysis

To run this project on your local machine, please follow these steps:

1.  **Install R and RStudio:**
    * Download and install R from the [CRAN website](https://cran.r-project.org/).
    * Download and install [RStudio Desktop](https://posit.co/download/rstudio-desktop/).

2.  **Prepare the Project:**
    * Clone or download this repository.
    * Place the `House_Rent_Dataset.csv` file in the project's working directory.

3.  **Run the R Script:**
    * Open the `.R` script file in RStudio.
    * Install the required packages by running the following commands in the R console:
        ```R
        install.packages("tidyverse")
        install.packages("stringr")
        install.packages("plotly")
        ```
    * Run the script sequentially to perform the data preparation, analysis, and generate the visualizations.

---

## 7. Project Team 

* **AU YONG KAI YI**
* **CERWIN LEE JIA YIING**
* **LEE WEN KANG**
* **PAVVILAN A/L KALAI VAANAN**
