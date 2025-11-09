# app.R
# Customer Segmentation — RFM Dashboard (single-file Shiny app)
# Put this file in your project folder and run with: shiny::runApp()

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(factoextra)
library(cluster)
library(tidyr)
library(reshape2)
library(scales)
library(viridis)
library(DT)

# Increase upload limit (200 MB)
options(shiny.maxRequestSize = 200 * 1024^2)

# Helper: safe read (handles Excel or CSV path if needed)
safe_read <- function(path) {
  ext <- tools::file_ext(path)
  if (tolower(ext) %in% c("xls","xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Please install readxl to read Excel files.")
    }
    df <- readxl::read_excel(path)
    df <- as.data.frame(df)
  } else {
    df <- readr::read_csv(path, show_col_types = FALSE)
  }
  df
}

# UI -------------------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  tags$head(
    tags$style(HTML("
      .app-title {font-size: 34px; font-weight:700; padding-top:6px;}
      .app-sub {color: #666; margin-bottom: 16px;}
      .sidebar-help {font-size: 12px; color: #666;}
      .box {background: #fff; padding: 12px; border-radius: 6px; box-shadow: 0 1px 2px rgba(0,0,0,0.05);}
    "))
  ),
  fluidRow(
    column(12,
           div(class = "app-title", "Customer Segmentation — RFM Dashboard"),
           div(class = "app-sub", "Interactive RFM clustering and segment exports")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "box",
      fileInput("file", "Upload cleaned_retail.csv",
                accept = c(".csv", ".xls", ".xlsx")),
      actionButton("use_sample", "Use sample (small) dataset", icon = icon("database")),
      hr(),
      numericInput("k", "k (clusters)", value = 5, min = 2, step = 1),
      textInput("snapshot_date", "Snapshot date (optional, YYYY-MM-DD)", value = ""),
      selectInput("filter_segment", "Filter segment (after clustering)", choices = c("All"), selected = "All"),
      br(),
      downloadButton("download_clusters", "Download labeled customers", icon = icon("download")),
      br(), br(),
      div(class = "sidebar-help",
          tags$b("Notes:"),
          tags$ul(
            tags$li("Data must contain: InvoiceDate (datetime), InvoiceNo, CustomerID, TotalPrice, Description, Country"),
            tags$li("Max upload: 30 MB (you can increase in app.R if needed)"),
            tags$li("If clustering seems slow, reduce dataset or use sample")
          )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 br(),
                 withSpinner(verbatimTextOutput("data_diag"), type = 6),
                 br(),
                 withSpinner(DTOutput("summary_tbl"), type = 6)
        ),
        tabPanel("RFM & Clustering",
                 br(),
                 fluidRow(
                   column(6, withSpinner(plotOutput("p_elbow", height = "340px"), type = 6)),
                   column(6, withSpinner(plotOutput("p_scatter", height = "340px"), type = 6))
                 ),
                 br(),
                 fluidRow(
                   column(6, withSpinner(plotOutput("p_pca", height = "420px"), type = 6)),
                   column(6, withSpinner(plotOutput("p_centroid", height = "420px"), type = 6))
                 )
        ),
        tabPanel("Customer Table",
                 br(),
                 withSpinner(DTOutput("table_customers"), type = 6)
        ),
        tabPanel("About",
                 br(),
                 p("This dashboard performs RFM (Recency, Frequency, Monetary) analysis and k-means clustering."),
                 p("Workflow: upload cleaned transactions -> compute RFM -> choose k -> inspect clusters -> download labeled customers."),
                 p("Author: Shubham"),
                 br(),
                 tags$details(
                   tags$summary("Technical notes (click to expand)"),
                   tags$ul(
                     tags$li("R packages used: shiny, readr, dplyr, lubridate, ggplot2, factoextra, cluster, tidyr, reshape2, scales, viridis, DT"),
                     tags$li("Snapshot date defaults to max(InvoiceDate) + 1 day if left blank."),
                     tags$li("If you want reproducible environment, commit renv.lock or list of packages.")
                   )
                 )
        )
      )
    )
  )
)

# Server ---------------------------------------------------------------------
server <- function(input, output, session) {
  
  # allow use of a built-in small sample to demo UI
  sample_data <- eventReactive(input$use_sample, {
    sample_path <- file.path("data", "cleaned_retail.csv")
    if (file.exists(sample_path)) {
      df <- readr::read_csv(sample_path, show_col_types = FALSE)
      df_sample <- df %>% slice_sample(n = min(1000, nrow(df)))
    } else {
      set.seed(42)
      n <- 200
      df_sample <- tibble(
        InvoiceNo = sample(50000:60000, n, replace = TRUE),
        StockCode = sample(LETTERS, n, replace = TRUE),
        Description = sample(c("Widget A","Widget B","Widget C"), n, replace = TRUE),
        Quantity = sample(1:10, n, replace = TRUE),
        InvoiceDate = as_datetime("2021-12-01") - days(sample(1:400, n, replace = TRUE)),
        UnitPrice = runif(n, 1, 100),
        CustomerID = sample(1000:2000, n, replace = TRUE),
        Country = sample(c("United Kingdom","Germany","France"), n, replace = TRUE)
      ) %>% mutate(TotalPrice = Quantity * UnitPrice)
    }
    df_sample
  })
  
  # Reactive: raw uploaded or sample dataset
  raw_data <- reactive({
    if (!is.null(input$file)) {
      tryCatch({
        df <- safe_read(input$file$datapath)
        return(df)
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error")
        return(NULL)
      })
    }
    if (!is.null(sample_data())) return(sample_data())
    return(NULL)
  })
  
  # Diagnostics: show column names and top rows
  output$data_diag <- renderPrint({
    df <- raw_data()
    req(df)
    if ("InvoiceDate" %in% names(df)) {
      if (!inherits(df$InvoiceDate, c("POSIXct","POSIXt","Date"))) {
        parsed <- tryCatch(lubridate::as_datetime(df$InvoiceDate), error = function(e) NA)
        if (all(is.na(parsed))) {
          parsed2 <- tryCatch(as_datetime(df$InvoiceDate, tz = "UTC"), error = function(e) NA)
          if (!all(is.na(parsed2))) df$InvoiceDate <- parsed2
        } else {
          df$InvoiceDate <- parsed
        }
      }
    }
    
    cat("Data diagnostics\n\n")
    cat("Columns (loaded):\n")
    print(names(df))
    cat("\nRows / Columns:\n")
    cat(nrow(df), " ", ncol(df), "\n\n")
    cat("First 5 rows:\n")
    print(utils::head(df, 5))
  })
  
  # reactive: cleaned + rfm base
  rfm_base <- reactive({
    df <- raw_data()
    req(df)
    
    required <- c("InvoiceDate", "InvoiceNo", "CustomerID", "TotalPrice", "Description", "Country")
    missing <- setdiff(required, names(df))
    if (length(missing) > 0) {
      showNotification(paste0("Missing required columns: ", paste(missing, collapse = ", ")), type = "error", duration = NULL)
      return(NULL)
    }
    
    if (!inherits(df$InvoiceDate, c("POSIXct","POSIXt","Date"))) {
      df$InvoiceDate <- tryCatch(lubridate::as_datetime(df$InvoiceDate), error = function(e) NA)
    }
    
    df <- df %>%
      filter(!is.na(CustomerID)) %>%
      mutate(Quantity = ifelse(is.na(Quantity), 0, Quantity),
             UnitPrice = ifelse(is.na(UnitPrice), 0, UnitPrice),
             TotalPrice = ifelse(is.na(TotalPrice), Quantity * UnitPrice, TotalPrice)) %>%
      filter(Quantity > -999999)
    
    df
  })
  
  
  # create RFM (reactive)
  rfm_calculated <- reactive({
    df <- rfm_base()
    req(df)
    
    snapshot <- if (nzchar(input$snapshot_date)) {
      parsed <- tryCatch(as_datetime(input$snapshot_date), error = function(e) NA)
      if (is.na(parsed)) {
        showNotification("snapshot_date not parseable; using max(InvoiceDate)+1", type = "warning")
        max(df$InvoiceDate, na.rm = TRUE) + days(1)
      } else parsed
    } else {
      max(df$InvoiceDate, na.rm = TRUE) + days(1)
    }
    
    rfm <- df %>%
      group_by(CustomerID) %>%
      summarise(
        Recency = as.numeric(difftime(snapshot, max(InvoiceDate, na.rm = TRUE), units = "days")),
        Frequency = n_distinct(InvoiceNo),
        Monetary = sum(TotalPrice, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Monetary_log = log1p(Monetary)) %>%
      arrange(CustomerID)
    
    rfm
  })
  
  # small summary table for overview
  output$summary_tbl <- renderDT({
    rfm <- rfm_calculated()
    req(rfm)
    out <- tibble(
      n_customers = nrow(rfm),
      avg_recency = round(mean(rfm$Recency, na.rm = TRUE), 2),
      avg_frequency = round(mean(rfm$Frequency, na.rm = TRUE), 2),
      avg_monetary = round(mean(rfm$Monetary, na.rm = TRUE), 2)
    )
    datatable(out, rownames = FALSE, options = list(dom = 't'))
  })
  
  # run clustering
  kmeans_result <- reactive({
    rfm <- rfm_calculated()
    req(rfm)
    k <- as.integer(input$k)
    validate(need(!is.null(k) && k >= 2, "Please provide k >= 2"))
    
    rfm_scaled <- scale(rfm %>% select(Recency, Frequency, Monetary_log))
    
    set.seed(42)
    km <- kmeans(rfm_scaled, centers = k, nstart = 50, iter.max = 200)
    
    rfm$Cluster <- factor(km$cluster)
    
    rfm_summary <- rfm %>%
      group_by(Cluster) %>%
      summarise(Avg_Recency = mean(Recency, na.rm = TRUE),
                Avg_Frequency = mean(Frequency, na.rm = TRUE),
                Avg_Monetary = mean(Monetary, na.rm = TRUE),
                Count = n(), .groups = "drop")
    
    ranked <- rfm_summary %>% arrange(desc(Avg_Monetary)) %>% mutate(Rank = row_number())
    
    labels_all <- c("VIP", "Loyal", "Occasional", "At-Risk", "Low-Value")
    labels_use <- labels_all[1:nrow(ranked)]
    label_map <- setNames(labels_use, ranked$Cluster)
    
    rfm$Segment <- unname(label_map[as.character(rfm$Cluster)])
    
    list(
      rfm = rfm,
      rfm_scaled = rfm_scaled,
      kmeans_model = km,
      rfm_summary = rfm_summary,
      ranked = ranked
    )
  })
  
  # populate filter choices after clustering
  observeEvent(kmeans_result(), {
    res <- kmeans_result()
    segs <- unique(res$rfm$Segment)
    updateSelectInput(session, "filter_segment", choices = c("All", segs), selected = "All")
  })
  
  # elbow plot
  output$p_elbow <- renderPlot({
    rfm <- rfm_calculated()
    req(rfm)
    rfm_scaled <- scale(rfm %>% select(Recency, Frequency, Monetary_log))
    maxK <- min(10, nrow(rfm))
    wss <- sapply(1:maxK, function(k) {
      kmeans(rfm_scaled, k, nstart = 5, iter.max = 100)$tot.withinss
    })
    df <- data.frame(k = 1:maxK, wss = wss)
    ggplot(df, aes(k, wss)) +
      geom_line() + geom_point() + labs(title = "WSS (Elbow)", x = "Number of clusters k", y = "Total Within Sum of Squares") +
      theme_minimal()
  })
  
  # scatter plot
  output$p_scatter <- renderPlot({
    kmres <- kmeans_result()
    req(kmres)
    df <- kmres$rfm
    if (input$filter_segment != "All") df <- df %>% filter(Segment == input$filter_segment)
    ggplot(df, aes(x = Frequency, y = Monetary, color = Segment)) +
      geom_jitter(alpha = 0.6, size = 1.8, width = 0.3, height = 0) +
      scale_y_continuous(labels = scales::comma) +
      labs(title = paste0("RFM Clusters (k = ", input$k, ")"), x = "Frequency (distinct invoices)", y = "Monetary (total spend)") +
      theme_minimal() + scale_color_brewer(palette = "Set2")
  })
  
  # PCA plot
  output$p_pca <- renderPlot({
    kmres <- kmeans_result()
    req(kmres)
    pca <- prcomp(kmres$rfm_scaled, center = TRUE, scale. = TRUE)
    pca_df <- data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2], Segment = kmres$rfm$Segment)
    if (input$filter_segment != "All") pca_df <- pca_df %>% filter(Segment == input$filter_segment)
    ggplot(pca_df, aes(x = PC1, y = PC2, color = Segment)) +
      geom_point(alpha = 0.6, size = 1.8) +
      labs(title = "Customer Segments (PCA Projection)") +
      theme_minimal() + scale_color_brewer(palette = "Set2")
  })
  
  # centroid heatmap
  output$p_centroid <- renderPlot({
    kmres <- kmeans_result()
    req(kmres)
    centers <- as.data.frame(kmres$kmeans_model$centers)
    centers$Cluster <- paste0("Cluster_", seq_len(nrow(centers)))
    cent_m <- tidyr::pivot_longer(centers, cols = -Cluster, names_to = "Feature", values_to = "Value")
    ggplot(cent_m, aes(x = Feature, y = Cluster, fill = Value)) +
      geom_tile() +
      scale_fill_viridis_c(option = "plasma") +
      labs(title = "Cluster centroid heatmap (scaled features)", x = "Feature", y = "Cluster") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  # customer table
  output$table_customers <- renderDT({
    kmres <- kmeans_result()
    req(kmres)
    df <- kmres$rfm %>% select(CustomerID, Recency, Frequency, Monetary, Monetary_log, Cluster, Segment)
    if (input$filter_segment != "All") df <- df %>% filter(Segment == input$filter_segment)
    datatable(df, options = list(pageLength = 10, lengthMenu = c(10,25,50)), rownames = FALSE)
  })
  
  # download labeled customers
  output$download_clusters <- downloadHandler(
    filename = function() {
      paste0("customer_segments_labeled_", Sys.Date(), ".csv")
    },
    content = function(file) {
      kmres <- kmeans_result()
      req(kmres)
      readr::write_csv(kmres$rfm, file)
    }
  )
  
  # save plots optionally to plots/ (non-blocking safety)
  observeEvent(kmeans_result(), {
    dir.create("plots", showWarnings = FALSE)
    # try to save last elbow plot (guarded)
    tryCatch({
      p <- recordPlot() # capture current plot stack (best-effort)
      # fallback: do not error if cannot save
      ggsave(filename = file.path("plots", paste0("elbow_k", input$k, ".png")), plot = last_plot(), width = 8, height = 5, dpi = 150)
    }, error = function(e) {
      # silently ignore save errors
      message("Unable to save plot: ", e$message)
    })
  }, once = FALSE)
  
  # friendly messages on errors / prompts
  observe({
    if (is.null(raw_data())) {
      showNotification("Upload a cleaned CSV to run the analysis, or click 'Use sample' to demo.", type = "message", duration = 6)
    }
  })
  
}

# Run the app
shinyApp(ui, server)
