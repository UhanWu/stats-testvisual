# Bro's Statistical Analyzer

## Introduction

Bro's Statistical Analyzer is a comprehensive R Shiny web application designed to empower users with robust tools for exploratory data analysis (EDA), data transformation, advanced statistical testing, and interactive data visualization. Developed to streamline common data science workflows, this application provides an intuitive interface for both rapid insights and rigorous hypothesis testing, making sophisticated statistical analysis accessible without requiring direct coding knowledge.

## Features

This application offers a multi-faceted approach to data analysis, encompassing:

* **Data Management & Exploratory Data Analysis (EDA):**
    * **CSV File Ingestion:** Facilitates direct upload of CSV formatted datasets.
    * **Interactive Data Tables:** Displays loaded and manipulated data in interactive `DT` tables, allowing for quick inspection.
    * **Dynamic Filtering:** Enables precise subsetting of data through interactive controls for both numeric ranges and categorical variable selections.
    * **Automated Data Summaries:** Generates key descriptive statistics (mean, standard deviation, min, max, missing counts, unique values) and frequency distributions for all variables, complete with inline sparkline visualizations.

* **Data Transformation:**
    * **Data Aggregation:** Supports summarization of numeric variables based on categorical grouping, including calculations for Mean, Sum, Median, and Count.
    * **Data Reshaping (Pivoting):** Provides functionalities to transform data between wide and long formats, using `tidyr` methods, to prepare datasets for specific analytical requirements.
    * **Flexible Reset Options:** Allows users to reset the analysis dataset to its original filtered state at any point.

* **Statistical Testing:**
    * **Comprehensive Test Suite:** Integrates **11 distinct statistical tests** to evaluate hypotheses and derive statistical inferences.
    * **Normality Tests:** Includes Shapiro-Wilk, Kolmogorov-Smirnov, and Anderson-Darling tests for assessing data distribution.
    * **Parametric Comparison Tests:** Features One-Sample t-test, Two-Sample t-test, and Paired t-test for comparing means.
    * **ANOVA:** Offers One-Way Analysis of Variance for comparing means across three or more groups.
    * **Non-Parametric Alternatives:** Provides the Wilcoxon Rank Sum Test for non-parametric group comparisons.
    * **Categorical Association Tests:** Incorporates Chi-Square Goodness of Fit, Chi-Square Test for Independence, and Fisher's Exact Test for analyzing relationships between categorical variables.
    * **Contextual Recommendations:** Offers automated suggestions for appropriate statistical tests based on selected variable types and data characteristics.

* **Data Visualization:**
    * **Diverse Plotting Options:** Generates **six distinct visualization types** powered by `ggplot2`:
        * Histograms: For visualizing numeric variable distributions.
        * Box Plots: For displaying data distribution and outliers, with support for grouped comparisons.
        * Scatter Plots: For illustrating relationships between two numeric variables.
        * Bar Plots: For presenting counts or summaries of categorical variables.
        * Line Plots: For visualizing trends, typically over time or ordered categories.
    * **Reproducible Code Output:** Outputs the exact `ggplot2` code used to generate each visualization, enabling users to reproduce and further customize plots outside the application environment.

## Technologies Used

This application is built entirely within the R ecosystem, leveraging the following key packages:

* `shiny`: The primary framework for building interactive web applications.
* `shinythemes`: Provides modern, attractive themes for the Shiny UI.
* `shinycssloaders`: Adds visual loading spinners for improved user experience during computations.
* `ggplot2`: Comprehensive package for creating static and animated graphics.
* `dplyr`: Offers a consistent and powerful set of verbs for data manipulation.
* `tidyr`: Essential for tidying data, including pivoting operations.
* `nortest`: Provides additional normality tests, such as Anderson-Darling.
* `knitr`: Facilitates dynamic report generation, used for table rendering.
* `kableExtra`: Enhances `knitr` tables with advanced styling and features.
* `lubridate`: Simplifies working with dates and times (if date columns are present).
* `sparkline`: Enables the creation of small, inline charts for data summaries.
* `htmltools`: Provides tools for generating HTML.
* `DT`: Creates interactive HTML tables from R data.

## How to Use

1.  **Upload Data:** Navigate to the 'Data & EDA' tab and upload your dataset in CSV format. The application will automatically display a summary.
2.  **Filter Data:** In the 'Data & EDA' sidebar, utilize the dynamic controls to subset your data. Click 'Apply Filters' to update the dataset used across all tabs. Use 'Reset All Filters' to revert to the original uploaded data.
3.  **Transform Data (Optional):** Proceed to the 'Data Transformation' tab to perform data summarization (e.g., group-by and aggregate) or reshape your data (pivot longer/wider). Any transformations applied here will become the active dataset for subsequent analysis. You can reset to the *filtered* data at any time.
4.  **Analyze & Visualize:** Access the 'Statistical Tests' and 'Visualizations' tabs to perform analyses and generate plots on the currently active dataset (after filtering and any transformations). Select variables and tests/plot types from the sidebars, and observe the results in the main panel.

## About the Developer

**Yuhan Wu**
Sophomore at Emory University, double majoring in Applied Mathematics & Statistics and Economics. Passionate about research, statistics, and risk analysis.

## Contact

* **LinkedIn:** [https://www.linkedin.com/in/wuyuhanm/](https://www.linkedin.com/in/wuyuhanm/)
* **Email:** yuhan.wu@emory.edu
