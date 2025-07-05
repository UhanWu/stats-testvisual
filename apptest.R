## For App Test

# Load necessary libraries
library(shiny)
library(shinythemes)    # For themes
library(shinycssloaders)# For loading spinners
library(ggplot2)
library(dplyr)
library(nortest)        # For Anderson-Darling test
library(knitr)          # For creating beautiful tables
library(kableExtra)     # For styling the kable tables
library(lubridate)      # For easier date handling
library(sparkline)      # For inline plots

# Define the user interface (UI)
ui <- fluidPage(theme = shinytheme("flatly"),
                # Add sparkline JS dependency
                sparkline::sparklineOutput("dummy_sparkline_output", height = "0px"),
                navbarPage(
                  "Interactive Statistical Analyzer",
                  # Tab 0: About Page
                  tabPanel("About", icon = icon("info-circle"),
                           fluidRow(
                             column(12, style = "padding: 20px;",
                                    h2("Welcome to the Interactive Statistical Analyzer"),
                                    p("This application provides a comprehensive interface for exploratory data analysis (EDA), statistical testing, and data visualization."),
                                    hr(),
                                    h3("A Brief on Hypothesis Testing"),
                                    p("Statistical tests are used in hypothesis testing to determine whether a certain belief about a population parameter is true. The process involves stating a ", tags$strong("Null Hypothesis (H₀)"), ", which represents the status quo or a statement of no effect, and an ", tags$strong("Alternative Hypothesis (Hₐ)"), ", which represents what we are trying to find evidence for. The test calculates a ", tags$strong("p-value"), ", which is the probability of observing our data, or something more extreme, if the null hypothesis were true. A small p-value (typically < 0.05) provides evidence against the null hypothesis, leading us to reject it in favor of the alternative."),
                                    hr(),
                                    h3("Guide to Statistical Tests in This App"),
                                    # Two-column layout for test descriptions and plots
                                    fluidRow(
                                      column(6,
                                             tags$h4("Normality Tests (e.g., Shapiro-Wilk)"),
                                             p("Used to determine if a dataset is well-modeled by a normal (bell-shaped) distribution. This is often a prerequisite for using parametric tests like t-tests and ANOVA."),
                                             tags$h4("One-Sample t-test"),
                                             p("Compares the mean of a single sample to a known or hypothesized population mean. (e.g., 'Is the average blood pressure of our patient group different from the national average of 120 mmHg?')"),
                                             tags$h4("Two-Sample t-test"),
                                             p("Compares the means of two independent groups to see if they are significantly different from each other. (e.g., 'Does the new drug lower cholesterol more than the placebo?')"),
                                             tags$h4("Paired t-test"),
                                             p("Compares the means of two related groups, such as before-and-after measurements on the same subjects. (e.g., 'Did a diet plan result in significant weight loss for the same group of participants?')"),
                                             tags$h4("One-Way ANOVA"),
                                             p("Compares the means of three or more independent groups. (e.g., 'Do patients on Drug A, Drug B, and a placebo have different average recovery times?')"),
                                             tags$h4("Chi-Square Goodness of Fit Test"),
                                             p("Determines if a single categorical variable's frequency distribution matches a specific, expected distribution. (e.g., 'Do the proportions of patients choosing four different treatment options match the 25% proportion we expected for each?')"),
                                             tags$h4("Chi-Square Test for Independence"),
                                             p("Determines if there is a significant association between two categorical variables. (e.g., 'Is there an association between smoking status and the incidence of a certain disease?')"),
                                             tags$h4("Fisher's Exact Test"),
                                             p("An alternative to the Chi-Square test for independence, used when sample sizes or cell counts are small."),
                                             tags$h4("Wilcoxon Rank Sum Test"),
                                             p("A non-parametric alternative to the two-sample t-test. It is used to compare two independent groups when the data is not normally distributed.")
                                      ),
                                      column(6,
                                             plotOutput("normality_plot_example", height = "250px"),
                                             plotOutput("ttest_plot_example", height = "250px"),
                                             plotOutput("anova_plot_example", height = "250px")
                                      )
                                    ),
                                    hr(),
                                    h3("About Me"),
                                    p(tags$strong("Yuhan Wu")),
                                    p("Sophomore at Emory University, double majoring in Applied Mathematics & Statistics and Economics."),
                                    p("Passionate about research, statistics, and risk analysis.")
                             )
                           )
                  ),
                  # Tab 1: Data Upload and View
                  tabPanel("Data & EDA", icon = icon("table"),
                           sidebarLayout(
                             sidebarPanel(width = 3,
                                          fileInput("file1", "Choose CSV File", accept = c(".csv")),
                                          tags$hr(),
                                          h4("Filter Data"),
                                          p("Use the controls below to filter the dataset."),
                                          uiOutput("filter_controls_ui"),
                                          actionButton("apply_filters", "Apply Filters", icon = icon("filter"), class = "btn-primary"),
                                          tags$hr()
                             ),
                             mainPanel(width = 9,
                                       withSpinner(uiOutput("summary_output_ui")),
                                       h4("Data Table (Filtered)"),
                                       withSpinner(div(style = "overflow-x: auto;", dataTableOutput("table")))
                             )
                           )
                  ),
                  
                  # Tab 2: Statistical Tests
                  tabPanel("Statistical Tests", icon = icon("flask"),
                           sidebarLayout(
                             sidebarPanel(
                               uiOutput("check_data_test"),
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
                  
                  # Tab 3: Visualizations
                  tabPanel("Visualizations", icon = icon("chart-bar"),
                           sidebarLayout(
                             sidebarPanel(
                               uiOutput("check_data_viz"),
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
  
  # --- About Page Plots ---
  output$normality_plot_example <- renderPlot({
    ggplot(data.frame(x = rnorm(1000)), aes(x)) + geom_density(fill = "#3498db", alpha = 0.7) + labs(title = "Example: Normal Distribution", x = "", y = "") + theme_minimal()
  })
  output$ttest_plot_example <- renderPlot({
    df <- data.frame(value = c(rnorm(50, 10, 2), rnorm(50, 12, 2)), group = rep(c("Group A", "Group B"), each = 50))
    ggplot(df, aes(x = group, y = value, fill = group)) + geom_boxplot(alpha = 0.7) + labs(title = "Example: Comparing Two Groups", x = "", y = "Value") + theme_minimal() + theme(legend.position = "none")
  })
  output$anova_plot_example <- renderPlot({
    df <- data.frame(value = c(rnorm(50, 10, 2), rnorm(50, 12, 2), rnorm(50, 10.5, 2)), group = rep(c("A", "B", "C"), each = 50))
    ggplot(df, aes(x = group, y = value, fill = group)) + geom_boxplot(alpha = 0.7) + labs(title = "Example: Comparing 3+ Groups", x = "", y = "Value") + theme_minimal() + theme(legend.position = "none")
  })
  
  # --- Data Loading and Filtering ---
  original_dataset <- reactive({
    req(input$file1)
    tryCatch({
      df <- read.csv(input$file1$datapath, stringsAsFactors = TRUE)
      df <- df %>% mutate(across(where(is.character), as.factor))
      return(df)
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error", duration = 10)
      return(NULL)
    })
  })
  
  output$filter_controls_ui <- renderUI({
    df <- original_dataset()
    req(df)
    lapply(names(df), function(col_name) {
      col_data <- df[[col_name]]
      if (is.numeric(col_data)) {
        sliderInput(paste0("filter_", col_name), label = col_name, min = min(col_data, na.rm = TRUE), max = max(col_data, na.rm = TRUE), value = c(min(col_data, na.rm = TRUE), max(col_data, na.rm = TRUE)))
      } else if (is.factor(col_data)) {
        selectInput(paste0("filter_", col_name), label = col_name, choices = levels(col_data), multiple = TRUE)
      }
    })
  })
  
  filtered_dataset <- eventReactive(input$apply_filters, {
    df <- original_dataset()
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
    df
  }, ignoreNULL = FALSE)
  
  output$dummy_sparkline_output <- sparkline::renderSparkline({})
  output$table <- renderDataTable({ req(filtered_dataset()); filtered_dataset() })
  
  # --- EDA Tab ---
  output$summary_output_ui <- renderUI({
    df <- filtered_dataset()
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
    tagList(h3("Exploratory Data Analysis (on Filtered Data)"), summary_boxes)
  })
  
  # --- Dynamic UI Selectors ---
  output$check_data_test <- renderUI({ if(is.null(filtered_dataset())) h5(style="color:red;", "Please upload and filter data first.") })
  output$check_data_viz <- renderUI({ if(is.null(filtered_dataset())) h5(style="color:red;", "Please upload and filter data first.") })
  output$select_var_test <- renderUI({ req(filtered_dataset()); selectInput("var_test", "Select Variable 1 (Numeric/Response):", names(filtered_dataset())) })
  output$select_var_test2 <- renderUI({ req(filtered_dataset()); selectInput("var_test2", "Select Variable 2 (Numeric/Group):", c("None", names(filtered_dataset())), selected = "None") })
  output$select_var_viz <- renderUI({ req(filtered_dataset()); selectInput("var_viz", "Select Primary Variable (Y-axis):", names(filtered_dataset())) })
  output$select_var_viz2 <- renderUI({ req(filtered_dataset()); selectInput("var_viz2", "Select Secondary Variable (X-axis):", names(filtered_dataset())) })
  
  var_types <- reactive({
    req(filtered_dataset(), input$var_test)
    data <- filtered_dataset()
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
    vt <- var_types(); data <- filtered_dataset()
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
    req(filtered_dataset(), input$var_viz)
    data <- filtered_dataset(); type1 <- if(is.numeric(data[[input$var_viz]])) "Numeric" else "Categorical"
    rec_text <- if(type1 == "Numeric") "A Histogram or Box Plot is great for viewing distribution." else "A Bar Plot is ideal for showing counts per category."
    tags$p(style="color:grey;", rec_text)
  })
  
  # --- Statistical Test Logic & Interpretation ---
  output$test_results_ui <- renderUI({
    req(filtered_dataset(), input$var_test, input$tests)
    data <- filtered_dataset(); var1 <- data[[input$var_test]]
    
    # Helper for interpretation
    interpret_p_value <- function(p_val) {
      if (is.na(p_val)) return("")
      conclusion <- if (p_val < 0.05) "statistically significant" else "not statistically significant"
      tags$p(tags$strong("Interpretation: "), sprintf("The p-value is %.3f. At a 0.05 significance level, this result is %s.", p_val, conclusion))
    }
    
    results_list <- lapply(input$tests, function(test_id) {
      result_obj <- NULL; interpretation <- ""
      try({
        if (test_id == "shapiro") {
          if(!is.numeric(var1)) stop("Requires a numeric variable.")
          if(length(na.omit(var1)) < 3 || length(na.omit(var1)) > 5000) stop("Sample size must be between 3 and 5000.")
          result_obj <- shapiro.test(var1)
          interpretation <- interpret_p_value(result_obj$p.value)
        } else if (test_id %in% c("t_two_sample", "t_paired", "anova", "wilcoxon", "chisq_ind", "fisher")) {
          validate(need(input$var_test2 != "None", "This test requires a second variable."))
          var2 <- data[[input$var_test2]]
          if (test_id == "t_two_sample") {
            if(!is.numeric(var1) || !is.factor(var2) || nlevels(as.factor(var2)) != 2) stop("Requires a numeric variable and a categorical variable with exactly 2 levels.")
            result_obj <- t.test(var1 ~ as.factor(var2), data = data)
            interpretation <- interpret_p_value(result_obj$p.value)
          } else if (test_id == "anova") {
            if(!is.numeric(var1) || !is.factor(var2)) stop("Requires a numeric response and a categorical grouping variable.")
            result_obj <- summary(aov(var1 ~ as.factor(var2), data = data))
            p_val <- result_obj[[1]]$`Pr(>F)`[1]
            interpretation <- interpret_p_value(p_val)
          } else if (test_id == "chisq_ind") {
            if(is.numeric(var1) || is.numeric(var2)) stop("Requires two categorical variables.")
            result_obj <- chisq.test(table(var1, var2))
            interpretation <- interpret_p_value(result_obj$p.value)
          }
          # Add other two-variable tests here...
        } else if (test_id == "chisq_gof") {
          if(is.numeric(var1)) stop("Requires a categorical variable.")
          result_obj <- chisq.test(table(var1))
          interpretation <- interpret_p_value(result_obj$p.value)
        }
        # Add other one-variable tests here...
      })
      
      # Render output box
      if (!is.null(result_obj)) {
        tags$div(class = "well",
                 h5(tags$strong(names(which(unlist(sapply(isolate(input$tests), function(x) x == test_id)))))),
                 tags$pre(paste(capture.output(print(result_obj)), collapse="\n")),
                 interpretation
        )
      } else if (exists("error_message")) {
        tags$div(class = "alert alert-danger", h5(tags$strong(test_id)), p(error_message))
      }
    })
    do.call(tagList, results_list)
  })
  
  # --- Visualization Tab ---
  output$plot <- renderPlot({
    req(filtered_dataset(), input$var_viz, input$plot_type)
    data <- filtered_dataset()
    p <- switch(input$plot_type,
                "hist" = {
                  validate(need(is.numeric(data[[input$var_viz]]), "Histogram requires a numeric variable."))
                  y_aes <- if(input$normalize_y) aes(y = ..density..) else aes(y = ..count..); y_lab <- if(input$normalize_y) "Density" else "Count"
                  ggplot(data, aes_string(x = input$var_viz)) + geom_histogram(y_aes, alpha=0.7, fill="#3498db", color="white") + labs(y = y_lab)
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
                  ggplot(data, aes_string(x = input$var_viz)) + geom_bar(y_aes, fill="#3498db", alpha=0.7) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y = y_lab)
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
    y_title <- input$var_viz; x_title <- switch(input$plot_type, "hist" = input$var_viz, "box" = "", "scatter" = input$var_viz2, "bar" = input$var_viz, "box_grouped" = input$var_viz2, "line" = input$var_viz2, "")
    plot_title <- paste(tools::toTitleCase(input$plot_type), "of", y_title, if(!is.null(x_title) && nchar(x_title) > 0) paste("by", x_title) else "")
    print(p + labs(title = plot_title, x = x_title, y = y_title) + theme_minimal(base_size = 14) + theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)))
  })
  
  output$plot_code <- renderText({
    req(filtered_dataset(), input$var_viz, input$plot_type)
    
    # 1. Start with the data and filtering code
    filter_code <- "my_filtered_data <- your_original_data %>%\n"
    filter_conditions <- c()
    for (col_name in names(original_dataset())) {
      filter_input_id <- paste0("filter_", col_name)
      if (!is.null(input[[filter_input_id]])) {
        if (is.numeric(original_dataset()[[col_name]])) {
          if(!identical(input[[filter_input_id]], c(min(original_dataset()[[col_name]], na.rm=T), max(original_dataset()[[col_name]], na.rm=T)))) {
            filter_conditions <- c(filter_conditions, sprintf("  filter(%s >= %s & %s <= %s)", col_name, input[[filter_input_id]][1], col_name, input[[filter_input_id]][2]))
          }
        } else if (is.factor(original_dataset()[[col_name]])) {
          if (length(input[[filter_input_id]]) < nlevels(original_dataset()[[col_name]])) {
            filter_conditions <- c(filter_conditions, sprintf("  filter(%s %%in%% c('%s'))", col_name, paste(input[[filter_input_id]], collapse="', '")))
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
    plot_code <- sprintf("\n\nggplot(my_filtered_data, aes(x = %s", input$var_viz)
    if(input$plot_type %in% c("scatter", "box_grouped", "line")) {
      plot_code <- sprintf("\n\nggplot(my_filtered_data, aes(x = %s, y = %s", input$var_viz2, input$var_viz)
      if(input$plot_type == "box_grouped") plot_code <- paste0(plot_code, ", fill = ", input$var_viz2)
    } else if(input$plot_type == "box") {
      plot_code <- sprintf("\n\nggplot(my_filtered_data, aes(y = %s", input$var_viz)
    }
    plot_code <- paste0(plot_code, ")) +\n")
    
    geom_code <- switch(input$plot_type,
                        "hist" = "  geom_histogram()",
                        "box" = "  geom_boxplot()",
                        "scatter" = "  geom_point()",
                        "bar" = "  geom_bar()",
                        "box_grouped" = "  geom_boxplot()",
                        "line" = "  geom_line()"
    )
    
    # 3. Combine and return
    paste0("# Load libraries\nlibrary(dplyr)\nlibrary(ggplot2)\n\n# Filter your data (if any filters were applied)\n", filter_code, "\n", plot_code, geom_code)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
