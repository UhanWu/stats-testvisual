# Load necessary libraries
library(shiny)
library(shinythemes)     # For themes
library(shinycssloaders) # For loading spinners
library(ggplot2)
library(dplyr)
library(tidyr)           # For pivoting
library(nortest)         # For Anderson-Darling test
library(knitr)           # For creating beautiful tables
library(kableExtra)      # For styling the kable tables
library(lubridate)       # For easier date handling
library(sparkline)       # For inline plots
library(htmltools)
library(DT)
library(tidyverse)




# Create the LinkedIn hyperlink
linkedin_link <- a(
  href = "https://www.linkedin.com/in/wuyuhanm/",
  target = "_blank", # Opens in a new tab
  "Linkedin"
)

# Create the email link
email_link <- a(
  href = "mailto:yuhan.wu@emory.edu",
  "yuhan.wu@emory.edu"
)

# Define the user interface (UI)
ui <- fluidPage(theme = shinytheme("flatly"),
                tags$head(
                  tags$style(HTML("
      /* TARGETED FIX: Override the fixed height of the spinner placeholder */
      .shiny-spinner-placeholder {
          height: auto !important;    /* Force height to be determined by content */
          min-height: auto !important; /* Force min-height to be determined by content */
          /* You could also set a small min-height if you want it to always be visible but not huge, e.g., min-height: 20px !important; */
      }

      /* Keep these general layout adjustments, as they might also help with overall spacing */
      .container-fluid > .tab-content {
          padding-top: 0px !important;
      }
      .tab-pane .row {
          margin-top: 0px !important;
      }
      .shiny-spinner-output-container {
          min-height: 20px !important;
          height: auto !important;
      }
      #transformed_table_wrapper {
          margin-top: 0px !important;
          margin-bottom: 0px !important;
          padding-top: 0px !important;
          padding-bottom: 0px !important;
          min-height: auto !important;
      }
      .dataTables_info, .dataTables_paginate, .dataTables_length, .dataTables_filter {
          margin-top: 0px !important;
          margin-bottom: 0px !important;
          padding-top: 0px !important;
          padding-bottom: 0px !important;
      }
      .col-sm-8, .col-md-8 { /* Using both just in case */
          padding-top: 0px !important;
          margin-top: 0px !important;
      }
    "))
                ),
                # Add sparkline JS dependency
                sparkline::sparklineOutput("dummy_sparkline_output", height = "0px"),
                navbarPage(
                  "Bro's Statistical Analyzer",
                  # Tab 0: About Page
                  tabPanel("About", icon = icon("info-circle"),
                           fluidRow(
                             column(12, style = "padding: 20px;",
                                    h2("Welcome to the Bro's Statistical Analyzer"),
                                    p("This application provides a comprehensive interface for exploratory data analysis (EDA), statistical testing, and data visualization."),
                                    hr(),
                                    h3("How to Use This App"),
                                    tags$ol(
                                      tags$li(tags$strong("Upload Data:"), "Navigate to the 'Data & EDA' tab and upload your dataset in CSV format. The app will automatically generate a summary."),
                                      tags$li(tags$strong("Filter Data:"), "Use the controls in the 'Data & EDA' sidebar to subset your data. Click the 'Apply Filters' button to update the entire application."),
                                      tags$li(tags$strong("Transform Data (Optional):"), "Go to the 'Data Transformation' tab to reshape or summarize your data. Any transformations applied here will become the new dataset for analysis. You can reset to the original filtered data at any time."),
                                      tags$li(tags$strong("Analyze & Visualize:"), "Use the 'Statistical Tests' and 'Visualizations' tabs to analyze the current dataset. All tests and plots will run on the data after filtering and transformations have been applied.")
                                    ),
                                    hr(),
                                    h3("A Brief on Hypothesis Testing"),
                                    p("Statistical tests are used in hypothesis testing to determine whether a certain belief about a population parameter is true. The process involves stating a ", tags$strong("Null Hypothesis (H₀)"), ", which represents the status quo or a statement of no effect, and an ", tags$strong("Alternative Hypothesis (Hₐ)"), ", which represents what we are trying to find evidence for. The test calculates a ", tags$strong("p-value"), ", which is the probability of observing our data, or something more extreme, if the null hypothesis were true. A small p-value (typically < 0.05) provides evidence against the null hypothesis, leading us to reject it in favor of the alternative."),
                                    hr(),
                                    h3("Guide to Statistical Tests in This App"),
                                    fluidRow(
                                      column(6,
                                             tags$h4("Normality Tests (e.g., Shapiro-Wilk)"),
                                             p("Used to determine if a dataset is well-modeled by a normal (bell-shaped) distribution. This is often a prerequisite for using parametric tests like t-tests and ANOVA."),
                                             tags$h4("One-Sample t-test"),
                                             p("Compares the mean of a single sample to a known or hypothesized population mean."),
                                             tags$h4("Two-Sample t-test"),
                                             p("Compares the means of two independent groups to see if they are significantly different."),
                                             tags$h4("Paired t-test"),
                                             p("Compares the means of two related groups, such as before-and-after measurements."),
                                             tags$h4("One-Way ANOVA"),
                                             p("Compares the means of three or more independent groups."),
                                             tags$h4("Chi-Square Goodness of Fit Test"),
                                             p("Determines if a single categorical variable's frequency distribution matches an expected distribution."),
                                             tags$h4("Chi-Square Test for Independence"),
                                             p("Determines if there is a significant association between two categorical variables."),
                                             tags$h4("Fisher's Exact Test"),
                                             p("An alternative to the Chi-Square test for independence, used when sample sizes are small."),
                                             tags$h4("Wilcoxon Rank Sum Test"),
                                             p("A non-parametric alternative to the two-sample t-test.")
                                      ),
                                      column(6,
                                             plotOutput("normality_plot_example", height = "250px"),
                                             plotOutput("ttest_plot_example", height = "250px"),
                                             plotOutput("anova_plot_example", height = "250px")
                                      )
                                    ),
                                    hr(),
                                    h3("About Bro"),
                                    p(tags$strong("Yuhan (Max) Wu")),
                                    p("Sophomore at Emory University, double majoring in Applied Mathematics & Statistics and Economics."),
                                    p("Passionate about research, statistics, and risk analysis."),
                                    p("Feel free to connect and chat with me at ", linkedin_link, " and ", email_link, "."),
                                    p("Thank you so much for using my Shiny app and I hope you have a good time! 😊")
                             )
                           )
                  ),
                  # Tab 1: Data Upload and View
                  tabPanel("Data & EDA", icon = icon("table"),
                           sidebarLayout(
                             sidebarPanel(width = 3,
                                          actionButton("view_filtered_data_eda", "View Filtered Table", icon = icon("search")),
                                          tags$hr(),
                                          fileInput("file1", h4("Choose CSV File"), accept = c(".csv")),
                                          tags$hr(),
                                          h4("Filter Data"),
                                          p("Use the controls below to filter the dataset."),
                                          uiOutput("filter_controls_ui"),
                                          actionButton("apply_filters", "Apply Filters", icon = icon("filter"), class = "btn-primary"),
                                          actionButton("reset_to_original", "Reset All Filters", icon = icon("undo"), class = "btn-danger"),
                                          tags$hr()
                             ),
                             mainPanel(width = 9,
                                       withSpinner(uiOutput("summary_output_ui"))
                             )
                           )
                  ),
                  
                  # Tab 2: Data Transformation
                  tabPanel("Data Transformation", icon = icon("cogs"),
                           # This entire tab's content will be rendered from the server
                           uiOutput("transform_tab_ui")
                  ),
                  
                  # Tab 3: Statistical Tests
                  tabPanel("Statistical Tests", icon = icon("flask"),
                           sidebarLayout(
                             sidebarPanel(
                               uiOutput("check_data_test"),
                               actionButton("view_data_test", "View Current Data", icon = icon("search")),
                               tags$hr(),
                               uiOutput("select_var_test"),
                               uiOutput("select_var_test2"),
                               conditionalPanel(
                                 condition = "input.tests.indexOf('t_one_sample') > -1",
                                 numericInput("mu_value", "Hypothesized Mean (μ):", 0)
                               ),
                               checkboxGroupInput("tests", "Choose tests to perform:",
                                                  choices = c(
                                                    "Shapiro-Wilk Normality Test" = "shapiro",
                                                    "Kolmogorov-Smirnov Normality Test" = "ks",
                                                    "Anderson-Darling Normality Test" = "ad",
                                                    "One-Sample t-test" = "t_one_sample",
                                                    "Two-Sample t-test" = "t_two_sample",
                                                    "Paired t-test" = "t_paired",
                                                    "One-Way ANOVA" = "anova",
                                                    "Wilcoxon Rank Sum Test" = "wilcoxon",
                                                    "Chi-Square Goodness of Fit" = "chisq_gof",
                                                    "Chi-Square Test for Independence" = "chisq_ind",
                                                    "Fisher's Exact Test" = "fisher"
                                                  )),
                               tags$hr(),
                               h5("Smart Recommendations"),
                               withSpinner(uiOutput("test_recommendations"))
                             ),
                             mainPanel(
                               h4("Statistical Test Results"),
                               withSpinner(uiOutput("test_results_ui"))
                             )
                           )
                  ),
                  
                  # Tab 4: Visualizations
                  tabPanel("Visualizations", icon = icon("chart-bar"),
                           sidebarLayout(
                             sidebarPanel(
                               uiOutput("check_data_viz"),
                               actionButton("view_data_viz", "View Current Data", icon = icon("search")),
                               tags$hr(),
                               h4("Plot Setup"),
                               uiOutput("select_var_viz"),
                               conditionalPanel(
                                 condition = "input.plot_type == 'scatter' || input.plot_type == 'box_grouped' || input.plot_type == 'line'",
                                 uiOutput("select_var_viz2")
                               ),
                               selectInput("plot_type", "Choose a plot type:",
                                           choices = c("Histogram" = "hist",
                                                       "Box Plot" = "box",
                                                       "Grouped Box Plot" = "box_grouped",
                                                       "Scatter Plot" = "scatter",
                                                       "Bar Plot" = "bar",
                                                       "Line Plot" = "line")),
                               checkboxInput("normalize_y", "Show as Probability/Density", value = FALSE),
                               tags$hr(),
                               h4("Customize Labels & Title"),
                               textInput("plot_title", "Title", placeholder = "Auto-generated from variables"),
                               textInput("plot_subtitle", "Subtitle", placeholder = "Optional subtitle"),
                               textInput("plot_xlab", "X-axis Label", placeholder = "Auto-generated from variable"),
                               textInput("plot_ylab", "Y-axis Label", placeholder = "Auto-generated from variable"),
                               tags$hr(),
                               h5("Recommendations"),
                               uiOutput("viz_recommendations")
                             ),
                             mainPanel(
                               h4("Visualization"),
                               withSpinner(plotOutput("plot", height = "600px")),
                               hr(),
                               h4("Reproducible Plot Code"),
                               withSpinner(verbatimTextOutput("plot_code"))
                             )
                           )
                  )
                )
)

# Define the server logic
server <- function(input, output, session) {
  
  # --- Reactive Values ---
  rv <- reactiveValues(
    original_data = NULL, # Holds the raw uploaded data
    filtered_data = NULL, # Holds data after filtering
    data_for_analysis = NULL # Holds the final data for use in tabs (can be filtered or transformed)
  )
  
  # --- About Page Plots ---
  output$normality_plot_example <- renderPlot({ ggplot(data.frame(x = rnorm(1000)), aes(x)) + geom_density(fill = "#3498db", alpha = 0.7) + labs(title = "Example: Normal Distribution", x = "", y = "") + theme_minimal() })
  output$ttest_plot_example <- renderPlot({ df <- data.frame(value = c(rnorm(50, 10, 2), rnorm(50, 12, 2)), group = rep(c("Group A", "Group B"), each = 50)); ggplot(df, aes(x = group, y = value, fill = group)) + geom_boxplot(alpha = 0.7) + labs(title = "Example: Comparing Two Groups", x = "", y = "Value") + theme_minimal() + theme(legend.position = "none") })
  output$anova_plot_example <- renderPlot({ df <- data.frame(value = c(rnorm(50, 10, 2), rnorm(50, 12, 2), rnorm(50, 10.5, 2)), group = rep(c("A", "B", "C"), each = 50)); ggplot(df, aes(x = group, y = value, fill = group)) + geom_boxplot(alpha = 0.7) + labs(title = "Example: Comparing 3+ Groups", x = "", y = "Value") + theme_minimal() + theme(legend.position = "none") })
  
  # --- Data Loading and Filtering ---
  observeEvent(input$file1, {
    tryCatch({
      df <- read.csv(input$file1$datapath, stringsAsFactors = TRUE)
      df <- df %>% mutate(across(where(is.character), as.factor))
      rv$original_data <- df
      rv$filtered_data <- df
      rv$data_for_analysis <- df
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error", duration = 10)
    })
  })
  
  output$filter_controls_ui <- renderUI({
    df <- rv$original_data
    req(df)
    lapply(names(df), function(col_name) {
      col_data <- df[[col_name]]
      if (is.numeric(col_data)) {
        sliderInput(paste0("filter_", col_name), label = col_name, min = min(col_data, na.rm = TRUE), max = max(col_data, na.rm = TRUE), value = c(min(col_data, na.rm = TRUE), max(col_data, na.rm = TRUE)))
      } else if (is.factor(col_data)) {
        selectInput(paste0("filter_", col_name), label = col_name, choices = levels(col_data), multiple = TRUE, selected = NULL)
      }
    })
  })
  
  observeEvent(input$apply_filters, {
    df <- rv$original_data
    req(df)
    for (col_name in names(df)) {
      filter_input_id <- paste0("filter_", col_name)
      if (!is.null(input[[filter_input_id]])) {
        if (is.numeric(df[[col_name]])) {
          df <- df %>% filter(.data[[col_name]] >= input[[filter_input_id]][1] & .data[[col_name]] <= input[[filter_input_id]][2])
        } else if (is.factor(df[[col_name]])) {
          if (length(input[[filter_input_id]]) > 0) {
            df <- df %>% filter(.data[[col_name]] %in% input[[filter_input_id]])
          }
        }
      }
    }
    rv$filtered_data <- df
    rv$data_for_analysis <- df
  })
  
  observeEvent(input$reset_to_original, {
    rv$filtered_data <- rv$original_data
    rv$data_for_analysis <- rv$original_data
    update_filter_controls()
  })
  
  update_filter_controls <- function() {
    df <- rv$original_data
    req(df)
    for (col_name in names(df)) {
      col_data <- df[[col_name]]
      if (is.numeric(col_data)) {
        updateSliderInput(session, paste0("filter_", col_name), value = c(min(col_data, na.rm = TRUE), max(col_data, na.rm = TRUE)))
      } else if (is.factor(col_data)) {
        updateSelectInput(session, paste0("filter_", col_name), selected = "")
      }
    }
  }
  
  # --- Data Transformation Tab ---
  output$transform_tab_ui <- renderUI({
    if(is.null(rv$original_data)) { # Ensure you've fixed this to rv$original_data as discussed previously
      return(h4(style="text-align: center; color: grey; margin-top: 20px;", "Please upload data in the 'Data & EDA' tab first."))
    }
    
    fluidRow(
      column(4,
             # Wrap BOTH sections in a single wellPanel
             wellPanel(
               h4("Summarize Data"),
               uiOutput("group_by_vars_ui"),
               uiOutput("summarize_var_ui"),
               selectInput("summary_func", "Summary Function:", choices = c("Mean" = "mean", "Sum" = "sum", "Median" = "median", "Count (N)" = "n")),
               actionButton("apply_summary", "Apply Summary", icon = icon("calculator")),
               
               tags$hr(), # Add a horizontal rule for visual separation within the same panel
               
               h4("Reshape Data (Pivot)"),
               p(strong("Wide to Long")),
               uiOutput("pivot_longer_cols_ui"),
               textInput("names_to_longer", "New Name Column:", "Variable"),
               textInput("values_to_longer", "New Value Column:", "Value"),
               actionButton("apply_pivot_longer", "Pivot to Long", icon = icon("arrow-down")),
               hr(),
               p(strong("Long to Wide")),
               uiOutput("pivot_wider_names_ui"),
               uiOutput("pivot_wider_values_ui"),
               actionButton("apply_pivot_wider", "Pivot to Wide", icon = icon("arrow-right"))
             ),
             actionButton("reset_to_filtered", "Reset to Filtered Data", icon = icon("undo"), class = "btn-danger", style="width:100%;")
      ),
      column(8,
             # This section remains the same for the table
             withSpinner(div(style = "overflow-x: auto;", dataTableOutput("transformed_table")))
      )
    )
  })
  
  output$group_by_vars_ui <- renderUI({ req(rv$data_for_analysis); selectInput("group_by_vars", "Group By:", choices = names(rv$data_for_analysis)[sapply(rv$data_for_analysis, is.factor)], multiple = TRUE) })
  output$summarize_var_ui <- renderUI({ req(rv$data_for_analysis); selectInput("summarize_var", "Summarize Variable:", choices = names(rv$data_for_analysis)[sapply(rv$data_for_analysis, is.numeric)]) })
  output$pivot_longer_cols_ui <- renderUI({ req(rv$data_for_analysis); selectInput("pivot_longer_cols", "Columns to Lengthen:", names(rv$data_for_analysis), multiple = TRUE) })
  output$pivot_wider_names_ui <- renderUI({ req(rv$data_for_analysis); selectInput("pivot_wider_names", "Column for New Names:", names(rv$data_for_analysis)) })
  output$pivot_wider_values_ui <- renderUI({ req(rv$data_for_analysis); selectInput("pivot_wider_values", "Column for New Values:", names(rv$data_for_analysis)) })
  
  observeEvent(input$apply_summary, {
    req(rv$data_for_analysis, input$group_by_vars, input$summarize_var, input$summary_func)
    df <- rv$data_for_analysis
    summary_expr <- if (input$summary_func == "n") "n()" else paste0(input$summary_func, "(`", input$summarize_var, "`, na.rm = TRUE)")
    df <- df %>% group_by(across(all_of(input$group_by_vars))) %>% summarise(summary_value = !!rlang::parse_expr(summary_expr), .groups = 'drop')
    rv$data_for_analysis <- df
  })
  
  observeEvent(input$apply_pivot_longer, {
    req(rv$data_for_analysis, input$pivot_longer_cols)
    rv$data_for_analysis <- rv$data_for_analysis %>% pivot_longer(cols = all_of(input$pivot_longer_cols), names_to = input$names_to_longer, values_to = input$values_to_longer)
  })
  
  observeEvent(input$apply_pivot_wider, {
    req(rv$data_for_analysis, input$pivot_wider_names, input$pivot_wider_values)
    rv$data_for_analysis <- rv$data_for_analysis %>% pivot_wider(names_from = all_of(input$pivot_wider_names), values_from = all_of(input$pivot_wider_values))
  })
  
  observeEvent(input$reset_to_filtered, {
    rv$data_for_analysis <- rv$filtered_data
  })
  
  output$transformed_table <- renderDataTable({
    df <- rv$data_for_analysis
    
    # Check if data exists AND has rows
    if (is.null(df) || nrow(df) == 0) {
      # Return a very minimal DT::datatable with just a message
      # 'dom = "t"' is crucial here: it tells DataTables to only render the table body ('t')
      # and none of the other controls (length menu, search, info, pagination).
      return(
        DT::datatable(
          data.frame(Message = "No transformed data to display. Apply a transformation."),
          options = list(
            dom = 't', # Only show the table body, no controls
            ordering = FALSE, # Disable column sorting
            paging = FALSE,   # Disable pagination
            info = FALSE,     # Disable "Showing X of Y entries"
            searching = FALSE,# Disable search box
            lengthChange = FALSE, # Disable "Show X entries" dropdown
            autoWidth = TRUE,
            # Ensure the message column fills the width cleanly
            columnDefs = list(list(width = '100%', targets = 0))
          ),
          rownames = FALSE, # Hide row names
          escape = FALSE,   # Allow HTML in the message if desired
          class = 'compact' # Use a compact class to reduce default padding/margin
        )
      )
    }
    
    # If data exists and has rows, render the full table with all controls
    # Ensure df is a data.frame for DT if it's a tibble (from dplyr operations)
    if ("tbl_df" %in% class(df)) {
      df <- as.data.frame(df)
    }
    
    DT::datatable(
      df,
      options = list(
        pageLength = 10, # Number of rows to display by default
        autoWidth = TRUE,
        scrollX = TRUE, # Allows horizontal scrolling if columns are too many
        scrollCollapse = TRUE, # Allows the table to collapse when fewer rows than pageLength
        # Use a standard 'dom' for when data is present (all controls)
        dom = 'lftipr'
      )
    )
  })
  
  # --- EDA Tab ---
  output$table <- renderDataTable({ req(rv$data_for_analysis); rv$data_for_analysis })
  output$summary_output_ui <- renderUI({
    df <- rv$data_for_analysis
    req(df)
    create_summary_box <- function(col_name) {
      col_data <- df[[col_name]]
      col_class <- class(col_data)[1]
      missing_count <- sum(is.na(col_data))
      missing_pct <- if(nrow(df) > 0) round(100 * missing_count / nrow(df), 1) else 0
      unique_count <- n_distinct(col_data)
      
      if (is.numeric(col_data)) {
        clean_data <- na.omit(col_data)
        if (length(clean_data) == 0) {
          left_content <- tags$p("No data after filtering.")
          right_content <- tags$p("")
        } else {
          mean_val <- round(mean(clean_data), 2); sd_val <- round(sd(clean_data), 2)
          hist_vals <- hist(clean_data, plot = FALSE, breaks = 20)$counts
          spk_html <- sparkline(hist_vals, type = "bar", barColor = "#3498db", barWidth = 20, height = "120px", tooltipFormat = '{{value}}')
          left_content <- tags$div(tags$p(tags$strong("Mean: "), mean_val), tags$p(tags$strong("Std Dev: "), sd_val), tags$p(tags$strong("Min: "), min(clean_data)), tags$p(tags$strong("Max: "), max(clean_data)))
          right_content <- tags$div(style = "text-align: center;", spk_html)
        }
      } else {
        freq_table <- sort(table(col_data), decreasing = TRUE)
        top_5 <- head(freq_table, 5)
        if(length(top_5) == 0){
          left_content <- tags$p("No data after filtering.")
          right_content <- tags$p("")
        } else {
          top_5_df <- data.frame(Level = names(top_5), Count = as.integer(top_5), Percent = paste0(round(100 * top_5 / length(col_data), 1), "%"))
          spk_html <- sparkline(as.integer(top_5), type = "bar", barColor = "#e74c3c", height = "120px", tooltipFormat = '{{offset:levels}}: {{value}}', tooltipValueLookups = list(levels = names(top_5)))
          left_content <- tags$div(HTML(kable(top_5_df, "html", row.names = FALSE) %>% kable_styling(bootstrap_options = "condensed", full_width = F)))
          right_content <- tags$div(style = "text-align: center;", spk_html)
        }
      }
      tags$div(style = "border: 1px solid #ddd; border-radius: 5px; padding: 15px; margin-bottom: 15px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
               tags$h5(tags$strong(col_name), tags$em(style="color:grey; margin-left:10px;", col_class)), hr(),
               fluidRow(column(5, tags$p(tags$strong("Distinct Values: "), unique_count), tags$p(tags$strong("Missing: "), paste0(missing_count, " (", missing_pct, "%)")), hr(), left_content), column(7, right_content))
      )
    }
    summary_boxes <- lapply(names(df), create_summary_box)
    tagList(h3("Exploratory Data Analysis"), summary_boxes)
  })
  
  # --- Dynamic UI Selectors ---
  output$check_data_test <- renderUI({ if(is.null(rv$data_for_analysis)) h5(style="color:red;", "Please upload data first.") })
  output$check_data_viz <- renderUI({ if(is.null(rv$data_for_analysis)) h5(style="color:red;", "Please upload data first.") })
  output$select_var_test <- renderUI({ req(rv$data_for_analysis); selectInput("var_test", "Select Variable 1 (Numeric/Response):", names(rv$data_for_analysis)) })
  output$select_var_test2 <- renderUI({ req(rv$data_for_analysis); selectInput("var_test2", "Select Variable 2 (Numeric/Group):", c("None", names(rv$data_for_analysis)), selected = "None") })
  output$select_var_viz <- renderUI({ req(rv$data_for_analysis); selectInput("var_viz", "Select Primary Variable (Y-axis):", names(rv$data_for_analysis)) })
  output$select_var_viz2 <- renderUI({ req(rv$data_for_analysis); selectInput("var_viz2", "Select Secondary Variable (X-axis):", names(rv$data_for_analysis)) })
  
  var_types <- reactive({
    req(rv$data_for_analysis, input$var_test)
    data <- rv$data_for_analysis
    type1 <- if(is.numeric(data[[input$var_test]])) "Numeric" else "Categorical"
    type2 <- "None"
    if(!is.null(input$var_test2) && input$var_test2 != "None"){
      type2 <- if(is.numeric(data[[input$var_test2]])) "Numeric" else "Categorical"
    }
    return(list(v1 = type1, v2 = type2))
  })
  
  # --- Recommendations ---
  output$test_recommendations <- renderUI({
    req(var_types())
    vt <- var_types(); data <- rv$data_for_analysis
    recommendations <- tags$ul()
    if (input$var_test2 == "None") {
      if (vt$v1 == "Numeric") {
        var1_clean <- na.omit(data[[input$var_test]]); normality_p <- if(length(var1_clean) > 2 && length(var1_clean) < 5000) shapiro.test(var1_clean)$p.value else 0
        if (normality_p > 0.05) { recommendations <- tagAppendChild(recommendations, tags$li("Data appears normally distributed. A ", tags$strong("One-Sample t-test"), " is appropriate.")) }
        else { recommendations <- tagAppendChild(recommendations, tags$li("Data may not be normally distributed. Consider non-parametric tests.")) }
        recommendations <- tagAppendChild(recommendations, tags$li("Use ", tags$strong("Normality Tests"), " to formally check the distribution."))
      } else { recommendations <- tagAppendChild(recommendations, tags$li("For a single categorical variable, use the ", tags$strong("Chi-Square Goodness of Fit"), " test.")) }
    } else {
      if (vt$v1 == "Numeric" && vt$v2 == "Categorical") {
        num_levels <- nlevels(as.factor(data[[input$var_test2]]))
        if (num_levels == 2) { recommendations <- tagAppendChild(recommendations, tags$li("To compare means of 2 groups, a ", tags$strong("Two-Sample t-test"), " is recommended.")) }
        else if (num_levels > 2) { recommendations <- tagAppendChild(recommendations, tags$li("To compare means of ", num_levels, " groups, a ", tags$strong("One-Way ANOVA"), " is appropriate.")) }
      }
      if (vt$v1 == "Numeric" && vt$v2 == "Numeric") {
        recommendations <- tagAppendChild(recommendations, tags$li("To compare two related measurements, consider a ", tags$strong("Paired t-test.")))
        recommendations <- tagAppendChild(recommendations, tags$li("For non-parametric comparison, use the ", tags$strong("Wilcoxon Rank Sum Test.")))
      }
      if (vt$v1 == "Categorical" && vt$v2 == "Categorical") {
        recommendations <- tagAppendChild(recommendations, tags$li("To test for association, use the ", tags$strong("Chi-Square Test for Independence.")))
        recommendations <- tagAppendChild(recommendations, tags$li("If cell counts are low (<5), ", tags$strong("Fisher's Exact Test"), " is more accurate."))
      }
    }
    if (length(recommendations$children) == 0) { return(tags$p("Select variables to see recommendations.")) }
    div(style="background-color: #f8f9fa; border-left: 5px solid #18bc9c; padding: 10px;", recommendations)
  })
  output$viz_recommendations <- renderUI({
    req(rv$data_for_analysis, input$var_viz)
    data <- rv$data_for_analysis; type1 <- if(is.numeric(data[[input$var_viz]])) "Numeric" else "Categorical"
    rec_text <- if(type1 == "Numeric") "A Histogram or Box Plot is great for viewing distribution." else "A Bar Plot is ideal for showing counts per category."
    tags$p(style="color:grey;", rec_text)
  })
  
  # --- Modal Data Table ---
  output$modal_data_table <- renderDataTable({
    req(rv$data_for_analysis)
    rv$data_for_analysis
  })
  
  dataModal <- function() {
    modalDialog(
      title = "Current Data",
      div(style = "overflow-x: auto;", withSpinner(dataTableOutput("modal_data_table"))),
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Close")
    )
  }
  
  observeEvent(input$view_data_test, { showModal(dataModal()) })
  observeEvent(input$view_data_viz, { showModal(dataModal()) })
  observeEvent(input$view_filtered_data_eda, { showModal(dataModal()) })
  
  # --- Statistical Test Logic & Interpretation ---
  output$test_results_ui <- renderUI({
    req(rv$data_for_analysis, input$var_test, input$tests)
    data <- rv$data_for_analysis
    
    results_list <- lapply(input$tests, function(test_id) {
      test_name <- names(which(sapply(isolate(input$tests), function(x) x == test_id)))
      
      tryCatch({
        var1 <- data[[input$var_test]]
        result_obj <- NULL; interpretation <- ""
        
        interpret_p_value <- function(p_val) {
          if (is.na(p_val)) return("")
          conclusion <- if (p_val < 0.05) "statistically significant" else "not statistically significant"
          tags$p(tags$strong("Interpretation: "), sprintf("The p-value is %.4f. At a 0.05 significance level, this result is %s.", p_val, conclusion))
        }
        
        if (test_id == "shapiro") {
          if(!is.numeric(var1)) stop("Requires a numeric variable.")
          if(length(na.omit(var1)) < 3 || length(na.omit(var1)) > 5000) stop("Sample size must be between 3 and 5000.")
          result_obj <- shapiro.test(var1)
          interpretation <- interpret_p_value(result_obj$p.value)
        } else if (test_id == "ks") {
          if(!is.numeric(var1)) stop("Requires a numeric variable.")
          result_obj <- ks.test(var1, "pnorm", mean(var1, na.rm=TRUE), sd(var1, na.rm=TRUE))
          interpretation <- interpret_p_value(result_obj$p.value)
        } else if (test_id == "ad") {
          if(!is.numeric(var1)) stop("Requires a numeric variable.")
          if(length(na.omit(var1)) < 8) stop("Anderson-Darling test requires at least 8 non-missing values.")
          result_obj <- ad.test(var1)
          interpretation <- interpret_p_value(result_obj$p.value)
        } else if (test_id == "t_one_sample") {
          if(!is.numeric(var1)) stop("Requires a numeric variable.")
          result_obj <- t.test(var1, mu = input$mu_value)
          interpretation <- interpret_p_value(result_obj$p.value)
        } else if (test_id == "chisq_gof") {
          if(is.numeric(var1)) stop("Requires a categorical variable.")
          result_obj <- chisq.test(table(var1))
          interpretation <- interpret_p_value(result_obj$p.value)
        } else { # Two-variable tests
          validate(need(input$var_test2 != "None", "This test requires a second variable."))
          var2 <- data[[input$var_test2]]
          if (test_id == "t_two_sample") {
            if(!is.numeric(var1) || !is.factor(var2) || nlevels(as.factor(var2)) != 2) stop("Requires a numeric variable and a categorical variable with exactly 2 levels.")
            result_obj <- t.test(var1 ~ as.factor(var2), data = data)
            interpretation <- interpret_p_value(result_obj$p.value)
          } else if (test_id == "t_paired") {
            if(!is.numeric(var1) || !is.numeric(var2)) stop("Requires two numeric variables.")
            if(length(na.omit(var1)) != length(na.omit(var2))) stop("Variables must have the same number of non-missing observations for a paired test.")
            result_obj <- t.test(var1, var2, paired = TRUE)
            interpretation <- interpret_p_value(result_obj$p.value)
          } else if (test_id == "anova") {
            if(!is.numeric(var1) || !is.factor(var2)) stop("Requires a numeric response and a categorical grouping variable.")
            result_obj <- summary(aov(var1 ~ as.factor(var2), data = data))
            p_val <- result_obj[[1]]$`Pr(>F)`[1]
            interpretation <- interpret_p_value(p_val)
          } else if (test_id == "wilcoxon") {
            if(!is.numeric(var1) || !is.numeric(var2)) stop("Requires two numeric variables.")
            result_obj <- wilcox.test(var1, var2)
            interpretation <- interpret_p_value(result_obj$p.value)
          } else if (test_id == "chisq_ind") {
            if(is.numeric(var1) || is.numeric(var2)) stop("Requires two categorical variables.")
            result_obj <- chisq.test(table(var1, var2))
            interpretation <- interpret_p_value(result_obj$p.value)
          } else if (test_id == "fisher") {
            if(is.numeric(var1) || is.numeric(var2)) stop("Requires two categorical variables.")
            result_obj <- fisher.test(table(var1, var2))
            interpretation <- interpret_p_value(result_obj$p.value)
          }
        }
        
        tags$div(class = "well", h5(tags$strong(test_name)), tags$pre(paste(capture.output(print(result_obj)), collapse="\n")), interpretation)
        
      }, error = function(e) {
        tags$div(class = "alert alert-danger", h5(tags$strong(test_name)), p(tags$strong("Error: "), e$message))
      })
    })
    do.call(tagList, results_list)
  })
  
  # --- Visualization Tab ---
  output$plot <- renderPlot({
    req(rv$data_for_analysis, input$var_viz, input$plot_type)
    data <- rv$data_for_analysis
    p <- switch(input$plot_type,
                "hist" = {
                  validate(need(is.numeric(data[[input$var_viz]]), "Histogram requires a numeric variable."))
                  y_aes <- if(input$normalize_y) aes(y = ..density..) else aes(y = ..count..); y_lab <- if(input$normalize_y) "Density" else "Count"
                  ggplot(data, aes_string(x = input$var_viz)) + geom_histogram(y_aes, alpha=0.7, fill="#3498db", color="white")
                },
                "box" = {
                  validate(need(is.numeric(data[[input$var_viz]]), "Box Plot requires a numeric variable."))
                  ggplot(data, aes_string(y = input$var_viz)) + geom_boxplot(fill = "#3498db", alpha = 0.7) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
                },
                "scatter" = {
                  req(input$var_viz2); validate(need(is.numeric(data[[input$var_viz]]) && is.numeric(data[[input$var_viz2]]), "Scatter Plot requires two numeric variables."))
                  ggplot(data, aes_string(x = input$var_viz2, y = input$var_viz)) + geom_point(alpha=0.6, color="#3498db") + geom_smooth(method="lm", color="#e74c3c", se=FALSE)
                },
                "bar" = {
                  validate(need(!is.numeric(data[[input$var_viz]]), "Bar Plot is best for categorical variables."))
                  y_aes <- if(input$normalize_y) aes(y = ..prop.., group = 1) else aes(y = ..count..); y_lab <- if(input$normalize_y) "Probability" else "Count"
                  ggplot(data, aes_string(x = input$var_viz)) + geom_bar(y_aes, fill="#3498db", alpha=0.7) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
                },
                "box_grouped" = {
                  req(input$var_viz2); validate(need(is.numeric(data[[input$var_viz]]) && !is.numeric(data[[input$var_viz2]]), "Grouped Box Plot requires one numeric (Y-axis) and one categorical (X-axis) variable."))
                  ggplot(data, aes_string(x = input$var_viz2, y = input$var_viz, fill = input$var_viz2)) + geom_boxplot(alpha = 0.7) + theme(legend.position = "none")
                },
                "line" = {
                  req(input$var_viz2); validate(need(is.numeric(data[[input$var_viz]]) && is.numeric(data[[input$var_viz2]]), "Line Plot requires two numeric variables."))
                  ggplot(data, aes_string(x = input$var_viz2, y = input$var_viz)) + geom_line(color="#3498db", size=1)
                }
    )
    
    # Auto-generate default labels
    default_y_title <- if(input$normalize_y) {
      switch(input$plot_type, "hist" = "Density", "bar" = "Probability", input$var_viz)
    } else {
      switch(input$plot_type, "hist" = "Count", "bar" = "Count", input$var_viz)
    }
    
    default_x_title <- switch(input$plot_type,
                              "hist" = input$var_viz,
                              "box" = "",
                              "scatter" = input$var_viz2,
                              "bar" = input$var_viz,
                              "box_grouped" = input$var_viz2,
                              "line" = input$var_viz2,
                              "")
    
    default_plot_title <- paste(
      tools::toTitleCase(gsub("_", " ", input$plot_type)),
      "of", input$var_viz,
      if(!is.null(default_x_title) && nzchar(default_x_title)) paste("by", default_x_title) else ""
    )
    
    # Use user input if provided, otherwise use defaults
    final_y_title <- if (nzchar(input$plot_ylab)) input$plot_ylab else default_y_title
    final_x_title <- if (nzchar(input$plot_xlab)) input$plot_xlab else default_x_title
    final_title <- if (nzchar(input$plot_title)) input$plot_title else default_plot_title
    final_subtitle <- input$plot_subtitle # Will be empty string "" if not provided
    
    # Apply to the plot
    print(p +
            labs(title = final_title, subtitle = final_subtitle, x = final_x_title, y = final_y_title) +
            theme_minimal(base_size = 14) +
            theme(
              plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(size = 16, hjust = 0.5) # Style for subtitle
            )
    )
  })
  
  output$plot_code <- renderText({
    req(rv$data_for_analysis, input$var_viz, input$plot_type)
    
    # 1. Start with the data and filtering code
    filter_code <- "my_filtered_data <- your_original_data %>%\n"
    filter_conditions <- c()
    # Use rv$original_data for comparison to see if a filter is active
    odata <- rv$original_data
    if(!is.null(odata)) {
      for (col_name in names(odata)) {
        filter_input_id <- paste0("filter_", col_name)
        if (!is.null(input[[filter_input_id]])) {
          if (is.numeric(odata[[col_name]])) {
            # Check if the slider is different from the max range
            if(!isTRUE(all.equal(input[[filter_input_id]], c(min(odata[[col_name]], na.rm=T), max(odata[[col_name]], na.rm=T))))) {
              filter_conditions <- c(filter_conditions, sprintf("  filter(`%s` >= %s & `%s` <= %s)", col_name, input[[filter_input_id]][1], col_name, input[[filter_input_id]][2]))
            }
          } else if (is.factor(odata[[col_name]])) {
            # Check if a selection was made (not all levels are selected)
            if (length(input[[filter_input_id]]) > 0 && length(input[[filter_input_id]]) < nlevels(odata[[col_name]])) {
              filter_conditions <- c(filter_conditions, sprintf("  filter(`%s` %%in%% c('%s'))", col_name, paste(input[[filter_input_id]], collapse="', '")))
            }
          }
        }
      }
    }
    
    if(length(filter_conditions) > 0) {
      filter_code <- paste0(filter_code, paste(filter_conditions, collapse = " %>%\n"))
    } else {
      filter_code <- "my_filtered_data <- your_original_data"
    }
    
    # 2. Generate the ggplot code
    plot_code <- ""
    if(input$plot_type %in% c("scatter", "box_grouped", "line")) {
      plot_code <- sprintf("ggplot(my_filtered_data, aes(x = `%s`, y = `%s`))", input$var_viz2, input$var_viz)
      if(input$plot_type == "box_grouped") plot_code <- sprintf("ggplot(my_filtered_data, aes(x = `%s`, y = `%s`, fill = `%s`))", input$var_viz2, input$var_viz, input$var_viz2)
    } else if(input$plot_type == "box") {
      plot_code <- sprintf("ggplot(my_filtered_data, aes(y = `%s`))", input$var_viz)
    } else {
      plot_code <- sprintf("ggplot(my_filtered_data, aes(x = `%s`))", input$var_viz)
    }
    
    geom_code <- switch(input$plot_type,
                        "hist" = "geom_histogram()",
                        "box" = "geom_boxplot()",
                        "scatter" = "geom_point()",
                        "bar" = "geom_bar()",
                        "box_grouped" = "geom_boxplot()",
                        "line" = "geom_line()"
    )
    
    # 3. Generate the labels code
    default_y_title <- if(input$normalize_y) {
      switch(input$plot_type, "hist" = "Density", "bar" = "Probability", input$var_viz)
    } else {
      switch(input$plot_type, "hist" = "Count", "bar" = "Count", input$var_viz)
    }
    default_x_title <- switch(input$plot_type, "hist" = input$var_viz, "box" = "", "scatter" = input$var_viz2, "bar" = input$var_viz, "box_grouped" = input$var_viz2, "line" = input$var_viz2, "")
    default_plot_title <- paste(tools::toTitleCase(gsub("_", " ", input$plot_type)), "of", input$var_viz, if(!is.null(default_x_title) && nzchar(default_x_title)) paste("by", default_x_title) else "")
    
    final_y_title <- if (nzchar(input$plot_ylab)) input$plot_ylab else default_y_title
    final_x_title <- if (nzchar(input$plot_xlab)) input$plot_xlab else default_x_title
    final_title <- if (nzchar(input$plot_title)) input$plot_title else default_plot_title
    final_subtitle <- input$plot_subtitle
    
    labs_args <- c(
      sprintf('title = "%s"', final_title),
      if (nzchar(final_subtitle)) sprintf('subtitle = "%s"', final_subtitle) else NULL,
      if (nzchar(final_x_title)) sprintf('x = "%s"', final_x_title) else NULL,
      sprintf('y = "%s"', final_y_title)
    )
    labs_code <- sprintf(" +\n  labs(%s)", paste(labs_args, collapse = ", "))
    
    # 4. Combine and return
    paste0("# Load libraries\nlibrary(dplyr)\nlibrary(ggplot2)\n\n# NOTE: Replace 'your_original_data' with your data frame's name.\n", filter_code, "\n\n# Create the plot\n", plot_code, " +\n  ", geom_code, labs_code, " +\n  theme_minimal()")
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)