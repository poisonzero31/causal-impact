# Fix SSL tempfile issue on shinyapps.io
if (Sys.getenv("R_CONFIG_ACTIVE") != "") {
  options(
    HTTPUserAgent        = NULL,
    download.file.method = "libcurl",
    download.file.extra  = "--ssl-no-revoke"
  )
}

Sys.setenv(TMPDIR = "/tmp", TMP = "/tmp", TEMP = "/tmp")

library(withr)
library(shiny)
library(xts)
library(ggplot2)
library(tidyr)
library(dplyr)
library(forecast)
library(corrplot)
library(CausalImpact)
library(scales)

ui <- fluidPage(
  titlePanel("Causal Impact Analysis"),
  
  tags$head(tags$style(HTML("
    @media print {
      .sidebar, .btn, button, .shiny-input-container { display: none !important; }
      .report-section { page-break-inside: avoid; }
      body { font-family: Arial, sans-serif; font-size: 11pt; }
      .report-container { width: 210mm; margin: 0 auto; padding: 20mm; }
      img { max-width: 100%; }
      h1 { font-size: 18pt; }
      h2 { font-size: 14pt; border-bottom: 1px solid #ccc; padding-bottom: 5px; }
      h4 { font-size: 12pt; }
    }
    .report-container {
      max-width: 210mm;
      margin: 0 auto;
      padding: 20px;
      font-family: Arial, sans-serif;
    }
    .report-section {
      margin-bottom: 30px;
    }
    .report-meta {
      color: #666;
      font-size: 0.9em;
      margin-bottom: 20px;
    }
    .guided-text {
      background: #f9f9f9;
      border-left: 3px solid #2196F3;
      padding: 10px 15px;
      margin: 10px 0;
      font-size: 0.95em;
    }
    .mgmt-summary {
      background: #f0f7ff;
      border: 1px solid #2196F3;
      border-radius: 4px;
      padding: 15px 20px;
      margin: 10px 0;
      font-size: 1em;
      line-height: 1.8;
    }
    .mgmt-summary .conclusion {
      margin-top: 12px;
      padding-top: 12px;
      border-top: 1px solid #2196F3;
      font-style: italic;
    }
    .print-btn {
      position: fixed;
      top: 10px;
      right: 10px;
      z-index: 9999;
    }
    .preview-note {
      color: #666;
      font-size: 0.85em;
      font-style: italic;
      margin-bottom: 8px;
    }
    table td:first-child {
      white-space: nowrap;
    }
  "))),
  
  sidebarLayout(
    sidebarPanel(
      # Step 1: File upload
      h4("1. Upload Data"),
      fileInput("file", "Upload CSV", accept = ".csv"),
      
      # Step 2: Column selection
      h4("2. Select Columns"),
      selectInput("date_col", "Date Column", choices = NULL),
      selectInput("dep_col", "Dependent Variable", choices = NULL),
      checkboxGroupInput("indep_cols", "Independent Variables", choices = NULL),
      actionButton("process_data", "Process Data", class = "btn-primary"),
      
      hr(),
      
      # Step 3: Date range
      h4("3. Select Date Range"),
      uiOutput("date_range_ui"),
      
      hr(),
      
      # Step 4: Outlier removal toggle
      h4("4. Outlier Removal"),
      radioButtons("remove_outliers", "Remove Outliers?",
                   choices  = c("Yes" = "yes", "No" = "no"),
                   selected = "yes",
                   inline   = TRUE),
      actionButton("apply_range", "Apply Date Range & Process", class = "btn-primary"),
      
      hr(),
      
      # Step 5: Correlation settings
      h4("5. Correlation Settings"),
      radioButtons("cor_method", "Correlation Method",
                   choices  = c("Pearson"  = "pearson",
                                "Spearman" = "spearman",
                                "Kendall"  = "kendall"),
                   selected = "pearson",
                   inline   = TRUE),
      sliderInput("cor_threshold", "Min Correlation with Dependent Variable",
                  min = 0, max = 1, value = 0.5, step = 0.05),
      
      hr(),
      
      # Step 6: Intervention date
      h4("6. Intervention Date"),
      uiOutput("intervention_ui"),
      actionButton("run_impact", "Run Causal Impact", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview",
                 h4("Raw Data Preview"),
                 tableOutput("raw_preview"),
                 h4("Processed XTS Preview"),
                 tableOutput("xts_preview")
        ),
        tabPanel("Boxplot",
                 plotOutput("boxplot_before", height = "500px")
        ),
        tabPanel("Correlation Matrix",
                 uiOutput("cor_matrix_ui")
        ),
        tabPanel("Normalized Line Chart",
                 plotOutput("line_chart", height = "500px")
        ),
        tabPanel("Causal Impact",
                 plotOutput("impact_plot", height = "600px"),
                 hr(),
                 h4("Summary"),
                 verbatimTextOutput("impact_summary"),
                 hr(),
                 h4("Report"),
                 div(style = "max-width: 400px; overflow-wrap: break-word; white-space: pre-wrap;",
                     verbatimTextOutput("impact_report")
                 )
        ),
        tabPanel("Report",
                 div(class = "print-btn",
                     actionButton("print_btn", "Print / Save as PDF",
                                  onclick = "window.print()",
                                  class = "btn-primary")
                 ),
                 div(class = "report-container",
                     
                     # Title
                     div(class = "report-section",
                         h1("Causal Impact Analysis Report"),
                         div(class = "report-meta",
                             textOutput("report_date"),
                             textOutput("report_dep_var")
                         )
                     ),
                     
                     hr(),
                     
                     # Management Summary
                     div(class = "report-section",
                         h2("Management Summary"),
                         uiOutput("report_mgmt_summary")
                     ),
                     
                     hr(),
                     
                     # Overview
                     div(class = "report-section",
                         h2("Overview"),
                         p("This report presents a Causal Impact analysis, which estimates the effect 
                           of an intervention on a time series outcome. The analysis uses a Bayesian 
                           structural time series model to construct a counterfactual — what would 
                           have happened if the intervention had not occurred — and compares it to 
                           the observed data after the intervention."),
                         p("The analysis follows these steps: data preparation and cleaning, 
                           outlier removal, correlation analysis to identify relevant control 
                           variables, and finally the causal impact estimation.")
                     ),
                     
                     hr(),
                     
                     # Step 1: Data
                     div(class = "report-section",
                         h2("1. Data Preparation"),
                         div(class = "guided-text",
                             uiOutput("report_data_summary")
                         ),
                         br(),
                         h4("Data Preview"),
                         p(class = "preview-note",
                           "Showing first 5 rows and first 5 columns (including date) for brevity."),
                         tableOutput("report_data_preview")
                     ),
                     
                     hr(),
                     
                     # Step 2: Outlier removal
                     div(class = "report-section",
                         h2("2. Distribution & Outlier Treatment"),
                         div(class = "guided-text",
                             uiOutput("report_outlier_text")
                         ),
                         br(),
                         plotOutput("report_boxplot", height = "350px")
                     ),
                     
                     hr(),
                     
                     # Step 3: Correlation
                     div(class = "report-section",
                         h2("3. Correlation Analysis"),
                         div(class = "guided-text",
                             uiOutput("report_cor_text")
                         ),
                         br(),
                         plotOutput("report_cor_matrix", height = "500px")
                     ),
                     
                     hr(),
                     
                     # Step 4: Line chart
                     div(class = "report-section",
                         h2("4. Normalized Trends"),
                         div(class = "guided-text",
                             uiOutput("report_line_text")
                         ),
                         br(),
                         plotOutput("report_line_chart", height = "350px")
                     ),
                     
                     hr(),
                     
                     # Step 5: Causal Impact
                     div(class = "report-section",
                         h2("5. Causal Impact Results"),
                         div(class = "guided-text",
                             uiOutput("report_impact_text")
                         ),
                         br(),
                         plotOutput("report_impact_plot", height = "500px"),
                         br(),
                         h4("Statistical Summary"),
                         uiOutput("report_impact_summary"),
                         br(),
                         h4("Interpretation"),
                         uiOutput("report_impact_report")
                     )
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values
  raw_data      <- reactiveVal(NULL)
  xts_data      <- reactiveVal(NULL)
  clean_data    <- reactiveVal(NULL)
  subset_data   <- reactiveVal(NULL)
  impact_result <- reactiveVal(NULL)
  
  # Helper: calculate correlation matrix using selected method
  calc_cor <- function(data) {
    cor(coredata(data), method = input$cor_method, use = "pairwise.complete.obs")
  }
  
  # Helper: get high correlation columns
  get_high_cor_cols <- function(clean) {
    cor_matrix <- calc_cor(clean)
    dep_name   <- names(clean)[1]
    high_cor   <- which(abs(cor_matrix[dep_name, ]) > input$cor_threshold)
    high_cor   <- high_cor[names(high_cor) != dep_name]
    high_cor
  }
  
  # Helper: extract and clean summary text
  extract_summary_text <- function(impact) {
    lines <- capture.output(summary(impact))
    lines <- lines[!grepl("For more details, type: summary\\(impact, \"report\"\\)", lines)]
    paste(lines, collapse = "\n")
  }
  
  # Helper: build data preview (5 rows, max 5 cols including date)
  build_data_preview <- function(clean) {
    date_col  <- data.frame(Date = format(index(clean), "%Y-%m-%d"))
    data_cols <- as.data.frame(coredata(clean))
    data_cols <- data_cols[, 1:min(4, ncol(data_cols)), drop = FALSE]
    d         <- cbind(date_col, data_cols)
    head(d, 5)
  }
  
  # Load CSV and populate column selectors
  observeEvent(input$file, {
    req(input$file)
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    raw_data(df)
    
    cols <- names(df)
    updateSelectInput(session, "date_col",  choices = cols, selected = cols[1])
    updateSelectInput(session, "dep_col",   choices = cols, selected = cols[2])
    updateCheckboxGroupInput(session, "indep_cols", choices = cols, selected = cols[3:min(10, length(cols))])
    
    output$raw_preview <- renderTable(head(df, 5))
  })
  
  # Process data into XTS
  observeEvent(input$process_data, {
    req(raw_data(), input$date_col, input$dep_col, input$indep_cols)
    
    df <- raw_data()
    
    # Select only relevant columns
    selected_cols <- unique(c(input$date_col, input$dep_col, input$indep_cols))
    df <- df[, selected_cols, drop = FALSE]
    
    # Replace null/blank with 0
    df[df == "null" | df == "" | is.na(df)] <- 0
    
    # Set dates and remove date column
    dates <- as.Date(df[[input$date_col]])
    df <- df[, !names(df) %in% input$date_col, drop = FALSE]
    
    # Convert all to integer
    df[] <- lapply(df, function(x) as.integer(as.character(x)))
    
    # Move dependent variable to column 1
    dep <- input$dep_col
    df  <- df[, c(dep, setdiff(names(df), dep)), drop = FALSE]
    
    # Create XTS
    ts_data <- xts(df, order.by = dates)
    xts_data(ts_data)
    
    output$xts_preview <- renderTable({
      d <- data.frame(Date = format(index(ts_data), "%Y-%m-%d"), coredata(ts_data))
      head(d, 5)
    })
    
    # Update date range UI
    output$date_range_ui <- renderUI({
      dateRangeInput("date_range", "Date Range",
                     start = start(ts_data),
                     end   = end(ts_data),
                     min   = start(ts_data),
                     max   = end(ts_data))
    })
    
    # Update intervention date UI
    output$intervention_ui <- renderUI({
      dateInput("intervention_date", "Intervention Date",
                value = end(ts_data) - 30,
                min   = start(ts_data) + 1,
                max   = end(ts_data) - 1)
    })
  })
  
  # Shared plot function for correlation matrix
  render_cor_matrix <- function(clean) {
    req(input$cor_method)
    cor_mat <- calc_cor(clean)
    n       <- ncol(clean)
    corrplot(cor_mat,
             method      = "color",
             type        = "lower",
             order       = "hclust",
             addCoef.col = "black",
             number.cex  = max(0.4, 1 - n * 0.02),
             tl.cex      = max(0.4, 1 - n * 0.02),
             tl.col      = "black",
             diag        = FALSE,
             mar         = c(0, 0, 2, 0))
  }
  
  # Shared plot function for line chart
  render_line_chart <- function(clean) {
    req(input$cor_threshold, input$cor_method)
    
    high_cor <- get_high_cor_cols(clean)
    
    if (length(high_cor) == 0) {
      plot.new()
      text(0.5, 0.5, "No columns above correlation threshold", cex = 1.5)
      return()
    }
    
    selected_ts  <- clean[, c(1, high_cor)]
    dep_col_name <- names(clean)[1]
    
    df_long <- pivot_longer(
      data.frame(date = index(selected_ts), coredata(selected_ts)),
      cols = -date, names_to = "variable", values_to = "value"
    )
    
    df_long_norm <- df_long %>%
      group_by(variable) %>%
      mutate(value = 100 * value / sum(value, na.rm = TRUE)) %>%
      ungroup()
    
    ggplot(df_long_norm, aes(x = date, y = value, color = variable,
                             linewidth = variable == dep_col_name)) +
      geom_line() +
      scale_linewidth_manual(values = c("TRUE" = 1, "FALSE" = 0.5), guide = "none") +
      scale_color_manual(values = setNames(
        ifelse(names(selected_ts) == dep_col_name, "black",
               hue_pal()(ncol(selected_ts))),
        names(selected_ts)
      )) +
      labs(title = paste("Normalized Line Chart (% of total) -",
                         input$cor_method, "correlation >", input$cor_threshold),
           x = "Date", y = "% of column total", color = "Variable") +
      theme_classic()
  }
  
  # Apply date range subset
  observeEvent(input$apply_range, {
    req(xts_data(), input$date_range)
    
    range_str <- paste(input$date_range[1], input$date_range[2], sep = "/")
    sub       <- xts_data()[range_str]
    subset_data(sub)
    
    # Boxplot before outlier removal (scaled)
    output$boxplot_before <- renderPlot({
      df_long <- pivot_longer(data.frame(scale(coredata(sub))),
                              cols = everything(),
                              names_to = "variable", values_to = "value")
      ggplot(df_long, aes(x = variable, y = value)) +
        geom_boxplot(fill = "steelblue", outlier.color = "red", outlier.size = 1) +
        labs(title = "Distribution per Column (before cleaning)",
             x = "Variable", y = "Scaled Value") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    # Clean outliers conditionally
    if (input$remove_outliers == "yes") {
      clean <- sub
      for (col in names(sub)) {
        clean[, col] <- tsclean(as.ts(sub[, col]))
      }
    } else {
      clean <- sub
    }
    clean_data(clean)
    
    # Dynamic height for correlation matrix
    output$cor_matrix_ui <- renderUI({
      n      <- ncol(clean)
      height <- max(800, n * 50)
      plotOutput("cor_matrix", height = paste0(height, "px"))
    })
    
    # Correlation matrix
    output$cor_matrix        <- renderPlot({ render_cor_matrix(clean) })
    
    # Line chart
    output$line_chart        <- renderPlot({ render_line_chart(clean) })
    
    # Report: boxplot
    output$report_boxplot    <- renderPlot({
      df_long <- pivot_longer(data.frame(scale(coredata(sub))),
                              cols = everything(),
                              names_to = "variable", values_to = "value")
      ggplot(df_long, aes(x = variable, y = value)) +
        geom_boxplot(fill = "steelblue", outlier.color = "red", outlier.size = 1) +
        labs(title = "Distribution per Column", x = "Variable", y = "Scaled Value") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    # Report: correlation matrix
    output$report_cor_matrix <- renderPlot({ render_cor_matrix(clean) })
    
    # Report: line chart
    output$report_line_chart <- renderPlot({ render_line_chart(clean) })
    
    # Report: data preview - 5 rows, max 5 cols, date no-wrap
    output$report_data_preview <- renderTable({
      build_data_preview(clean)
    }, striped = TRUE, bordered = TRUE, hover = TRUE)
    
    # Report: data summary text
    output$report_data_summary <- renderUI({
      n_cols <- ncol(clean)
      n_rows <- nrow(clean)
      dep    <- names(clean)[1]
      HTML(paste0(
        "<b>Dependent variable:</b> ", dep, "<br>",
        "<b>Number of independent variables:</b> ", n_cols - 1, "<br>",
        "<b>Date range:</b> ", input$date_range[1], " to ", input$date_range[2], "<br>",
        "<b>Number of observations:</b> ", n_rows, "<br>",
        "<b>Data cleaning:</b> 'null' and blank values replaced with 0. ",
        "All variables converted to integer."
      ))
    })
    
    # Report: outlier text
    output$report_outlier_text <- renderUI({
      if (input$remove_outliers == "yes") {
        HTML("Outliers were detected and removed using the <b>tsclean()</b> function from the 
             <i>forecast</i> package. This method identifies outliers using STL decomposition 
             and replaces them with interpolated values, preserving the time series structure. 
             The boxplot below shows the scaled distribution before outlier removal.")
      } else {
        HTML("Outlier removal was <b>not applied</b> in this analysis. The boxplot below shows 
             the scaled distribution of all variables as-is.")
      }
    })
    
    # Report: correlation text
    output$report_cor_text <- renderUI({
      high_cor <- get_high_cor_cols(clean)
      HTML(paste0(
        "Correlation between all variables was calculated using the <b>", input$cor_method,
        "</b> method. Variables with an absolute correlation above <b>", input$cor_threshold,
        "</b> with the dependent variable were selected as control variables for the causal ",
        "impact model. <b>", length(high_cor), " variable(s)</b> met this threshold: ",
        "<i>", paste(names(high_cor), collapse = ", "), "</i>."
      ))
    })
    
    # Report: line chart text
    output$report_line_text <- renderUI({
      high_cor <- get_high_cor_cols(clean)
      HTML(paste0(
        "The normalized line chart below shows the dependent variable and the ",
        length(high_cor), " selected control variable(s) as a percentage of their total, ",
        "making it easier to compare relative trends across variables with different scales. ",
        "The dependent variable is shown in <b>black</b>."
      ))
    })
  })
  
  # Run Causal Impact
  observeEvent(input$run_impact, {
    req(clean_data(), input$intervention_date, input$cor_threshold, input$cor_method)
    
    clean      <- clean_data()
    cor_matrix <- calc_cor(clean)
    dep_name   <- names(clean)[1]
    high_cor   <- which(abs(cor_matrix[dep_name, ]) > input$cor_threshold)
    impact_ts  <- clean[, high_cor]
    
    # Set periods
    intervention_date <- as.Date(input$intervention_date)
    pre_period  <- as.Date(c(start(impact_ts), intervention_date - 1))
    post_period <- as.Date(c(intervention_date, end(impact_ts)))
    
    # Run CausalImpact with SSL fix
    impact <- tryCatch({
      withr::with_envvar(
        c(TMPDIR = "/tmp", TMP = "/tmp", TEMP = "/tmp"),
        CausalImpact(impact_ts, pre_period, post_period)
      )
    }, error = function(e) {
      old_tmp <- Sys.getenv("TMPDIR")
      Sys.setenv(TMPDIR = "/tmp", TMP = "/tmp", TEMP = "/tmp")
      result <- CausalImpact(impact_ts, pre_period, post_period)
      Sys.setenv(TMPDIR = old_tmp)
      result
    })
    
    impact_result(impact)
    
    output$impact_plot    <- renderPlot(plot(impact))
    output$impact_summary <- renderPrint(summary(impact))
    output$impact_report  <- renderPrint(summary(impact, "report"))
    
    # Report: impact plot
    output$report_impact_plot <- renderPlot(plot(impact))
    
    # Report: statistical summary as flowing text
    output$report_impact_summary <- renderUI({
      summary_text <- extract_summary_text(impact)
      HTML(paste0(
        "<p style='white-space: pre-wrap; font-family: Arial; font-size: 11pt; line-height: 1.6;'>",
        summary_text,
        "</p>"
      ))
    })
    
    # Report: impact report as flowing text
    output$report_impact_report <- renderUI({
      report_text <- paste(capture.output(summary(impact, "report")), collapse = "\n")
      HTML(paste0(
        "<p style='white-space: pre-wrap; font-family: Arial; font-size: 11pt; line-height: 1.6;'>",
        report_text,
        "</p>"
      ))
    })
    
    # Report: impact text
    output$report_impact_text <- renderUI({
      HTML(paste0(
        "A Bayesian structural time series model was fitted to estimate the causal effect of 
        the intervention on <b>", dep_name, "</b>. ",
        "The pre-intervention period runs from <b>", pre_period[1],
        "</b> to <b>", pre_period[2], "</b>, ",
        "and the post-intervention period runs from <b>", post_period[1],
        "</b> to <b>", post_period[2], "</b>. ",
        "The model used <b>", length(high_cor) - 1, "</b> control variable(s) to construct ",
        "the counterfactual. ",
        "The three panels below show: (1) the observed vs predicted values, ",
        "(2) the pointwise difference at each time step, and ",
        "(3) the cumulative estimated effect of the intervention."
      ))
    })
    
    # Report: management summary
    output$report_mgmt_summary <- renderUI({
      s <- impact$summary
      
      avg_effect     <- round(s["Average", "AbsEffect"], 2)
      avg_effect_pct <- round(s["Average", "RelEffect"] * 100, 1)
      cum_effect     <- round(s["Cumulative", "AbsEffect"], 2)
      cum_effect_pct <- round(s["Cumulative", "RelEffect"] * 100, 1)
      p_value        <- round(s["Average", "p"], 3)
      significant    <- p_value < 0.05
      sig_text       <- ifelse(significant,
                               "statistically significant (p < 0.05)",
                               "not statistically significant (p ≥ 0.05)")
      
      conclusion <- if (significant && avg_effect > 0) {
        paste0("The evidence suggests that the intervention had a <b>positive and statistically 
               significant effect</b> on ", dep_name, ". Based on these results, the intervention 
               appears to have been effective and consideration should be given to maintaining 
               or scaling it.")
      } else if (significant && avg_effect < 0) {
        paste0("The evidence suggests that the intervention had a <b>negative and statistically 
               significant effect</b> on ", dep_name, ". Based on these results, the impact of 
               the intervention should be reviewed and corrective actions considered.")
      } else {
        paste0("The analysis did <b>not find a statistically significant effect</b> of the 
               intervention on ", dep_name, ". This suggests the intervention may not have had 
               a measurable impact, though other factors or a longer observation period may 
               need to be considered.")
      }
      
      HTML(paste0(
        "<div class='mgmt-summary'>",
        "<b>Dependent variable:</b> ", dep_name, "<br>",
        "<b>Intervention date:</b> ", intervention_date, "<br>",
        "<b>Post-intervention period:</b> ", post_period[1], " to ", post_period[2], "<br>",
        "<b>Average effect per period:</b> ", avg_effect,
        " (", ifelse(avg_effect > 0, "+", ""), avg_effect_pct, "%)<br>",
        "<b>Cumulative effect:</b> ", cum_effect,
        " (", ifelse(cum_effect > 0, "+", ""), cum_effect_pct, "%)<br>",
        "<b>Statistical significance:</b> ", sig_text, " (p = ", p_value, ")<br>",
        "<div class='conclusion'>", conclusion, "</div>",
        "</div>"
      ))
    })
  })
  
  # Report meta
  output$report_date <- renderText({
    paste("Report generated:", format(Sys.Date(), "%B %d, %Y"))
  })
  
  output$report_dep_var <- renderText({
    req(input$dep_col)
    paste("Dependent variable:", input$dep_col)
  })
}

shinyApp(ui, server)