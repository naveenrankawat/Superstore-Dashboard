# ============================================================
#  SUPERSTORE ENTERPRISE DASHBOARD
#  Framework : Shiny + bs4Dash + plotly + DT
#  Author    : Senior BI Architect
# ============================================================

# ── 0. AUTO-INSTALL MISSING PACKAGES ────────────────────────
pkgs <- c("shiny","bs4Dash","ggplot2","plotly","dplyr",
          "DT","readr","lubridate","scales","zoo","forecast",
          "shinycssloaders","fresh")
new  <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if(length(new)) install.packages(new, repos="https://cloud.r-project.org")

suppressWarnings({
  library(shiny); library(bs4Dash); library(ggplot2)
  library(plotly); library(dplyr); library(DT)
  library(readr); library(lubridate); library(scales)
  library(zoo); library(forecast); library(shinycssloaders)
  library(fresh)
})

# ── 1. LOAD & PREPARE DATA ──────────────────────────────────
load_data <- function() {
  f <- tryCatch(file.choose(), error = function(e) "superstore_cleaned_complete.csv")
  
  # Read raw bytes and strip non-UTF-8 characters before parsing
  raw_lines <- readLines(f, warn = FALSE, encoding = "latin1")
  clean_lines <- iconv(raw_lines, from = "latin1", to = "UTF-8", sub = "")
  tmp <- tempfile(fileext = ".csv")
  writeLines(clean_lines, tmp, useBytes = FALSE)
  
  df <- tryCatch(
    read_csv(tmp, show_col_types = FALSE, locale = locale(encoding = "UTF-8")),
    error = function(e) stop("Could not parse CSV after encoding fix: ", e$message)
  )
  
  # Normalize column names
  names(df) <- tolower(trimws(names(df)))
  names(df) <- gsub("\\s+","_", names(df))
  
  # Parse dates
  date_cols <- names(df)[sapply(df, function(x) inherits(x,"Date") |
                                  (is.character(x) && !all(is.na(suppressWarnings(
                                    dmy(x))))))]
  for(dc in date_cols) {
    parsed <- suppressWarnings(dmy(df[[dc]]))
    if(sum(!is.na(parsed)) > nrow(df)*0.5) df[[dc]] <- parsed
  }
  
  # Ensure key columns exist with fallbacks
  if(!"order_date" %in% names(df)) {
    d <- names(df)[sapply(df, inherits, "Date")]
    if(length(d)) df$order_date <- df[[d[1]]]
  }
  df
}

df_raw <- load_data()

# ── 2. COLUMN DETECTION ─────────────────────────────────────
num_cols  <- names(df_raw)[sapply(df_raw, is.numeric)]
cat_cols  <- names(df_raw)[sapply(df_raw, is.character)]
date_col  <- if("order_date" %in% names(df_raw)) "order_date" else
  names(df_raw)[sapply(df_raw, inherits, "Date")][1]

has_date     <- !is.na(date_col) && !is.null(date_col)
has_sales    <- "sales"       %in% num_cols
has_profit   <- "profit"      %in% num_cols
has_qty      <- "quantity"    %in% num_cols
has_category <- "category"    %in% cat_cols
has_subcat   <- "sub_category"%in% cat_cols
has_region   <- "region"      %in% cat_cols
has_segment  <- "segment"     %in% cat_cols
has_product  <- "product_name"%in% cat_cols
has_customer <- "customer_name"%in% cat_cols
has_state    <- "state"       %in% cat_cols

date_min <- if(has_date) min(df_raw[[date_col]], na.rm=TRUE) else Sys.Date()-365
date_max <- if(has_date) max(df_raw[[date_col]], na.rm=TRUE) else Sys.Date()

cat_choices   <- if(has_category) c("All", sort(unique(df_raw$category)))   else c("All")
subcat_choices<- if(has_subcat)   c("All", sort(unique(df_raw$sub_category)))else c("All")
region_choices<- if(has_region)   c("All", sort(unique(df_raw$region)))     else c("All")
seg_choices   <- if(has_segment)  c("All", sort(unique(df_raw$segment)))    else c("All")
product_choices<-if(has_product)  c("All", sort(unique(df_raw$product_name)))else c("All")

# ── 3. THEME ─────────────────────────────────────────────────
my_theme <- create_theme(
  bs4dash_vars(
    navbar_light_color       = "#ECEFF4",
    navbar_light_active_color= "#FFFFFF"
  ),
  bs4dash_color(
    gray_900 = "#1A1F36"
  )
)

# ── 4. UI ───────────────────────────────────────────────────
ui <- bs4DashPage(
  freshTheme = my_theme,
  dark       = NULL,
  help       = NULL,
  scrollToTop= TRUE,
  
  header = bs4DashNavbar(
    title = bs4DashBrand(
      title = "Superstore Intelligence Hub",
      color = "white",
      image = NULL
    ),
    skin  = "dark",
    status= "white",
    border= TRUE,
    tags$span(
      style = "color:#ECEFF4; font-size:12px; margin-left:10px;",
      icon("circle", style="color:#28a745; font-size:8px;"),
      " Live Dashboard  |  ",
      textOutput("last_updated", inline = TRUE)
    )
  ),
  
  sidebar = bs4DashSidebar(
    skin   = "dark",
    status = "primary",
    elevation= 4,
    bs4SidebarMenu(
      id = "sidebar",
      bs4SidebarMenuItem("Overview",    tabName="overview",    icon=icon("tachometer-alt")),
      bs4SidebarMenuItem("Sales Trend", tabName="trend",       icon=icon("chart-line")),
      bs4SidebarMenuItem("Categories",  tabName="categories",  icon=icon("tags")),
      bs4SidebarMenuItem("Geography",   tabName="geography",   icon=icon("globe-americas")),
      bs4SidebarMenuItem("Products",    tabName="products",    icon=icon("box")),
      bs4SidebarMenuItem("Customers",   tabName="customers",   icon=icon("users")),
      bs4SidebarMenuItem("Forecasting", tabName="forecast",    icon=icon("magic")),
      bs4SidebarMenuItem("Data Table",  tabName="datatable",   icon=icon("table")),
      bs4SidebarMenuItem("Insights",    tabName="insights",    icon=icon("lightbulb"))
    ),
    
    hr(style="border-color:#444;"),
    tags$div(style="padding:8px 16px; color:#aaa; font-size:11px; text-transform:uppercase; letter-spacing:1px;", "Filters"),
    
    # Date range
    if(has_date) dateRangeInput("date_range","Date Range",
                                start=date_min, end=date_max,
                                min=date_min,   max=date_max,
                                format="dd-mm-yyyy") else NULL,
    
    # Category
    if(has_category) selectInput("sel_category","Category",
                                 choices=cat_choices, selected="All") else NULL,
    # Sub-category
    if(has_subcat) selectInput("sel_subcat","Sub-Category",
                               choices=subcat_choices, selected="All") else NULL,
    # Region
    if(has_region) selectInput("sel_region","Region",
                               choices=region_choices, selected="All") else NULL,
    # Segment
    if(has_segment) selectInput("sel_segment","Segment",
                                choices=seg_choices, selected="All") else NULL,
    
    # Top-N
    sliderInput("top_n","Top N", min=5, max=30, value=10, step=5),
    
    br(),
    actionButton("reset_filters","Reset Filters",
                 icon=icon("sync"), width="85%",
                 class="btn-warning btn-sm",
                 style="margin-left:7%;")
  ),
  
  body = bs4DashBody(
    tags$head(
      tags$style(HTML("
        .kpi-box { border-radius:10px; box-shadow:0 4px 15px rgba(0,0,0,.12); }
        .nav-tabs-custom { border-bottom:2px solid #3c8dbc; }
        .info-box-icon { border-radius:10px 0 0 10px; }
        .small-box { border-radius:10px; }
        body { font-family:'Segoe UI',Arial,sans-serif; background:#F4F6F9; }
        .chart-title { font-size:14px; font-weight:600; color:#2c3e50; margin-bottom:6px; }
        h5.card-title { font-weight:700; color:#2c3e50; letter-spacing:.5px; }
      "))
    ),
    
    bs4TabItems(
      
      # ── OVERVIEW ─────────────────────────────────────────
      bs4TabItem(tabName="overview",
                 fluidRow(
                   bs4ValueBoxOutput("vb_sales",    width=3),
                   bs4ValueBoxOutput("vb_profit",   width=3),
                   bs4ValueBoxOutput("vb_quantity", width=3),
                   bs4ValueBoxOutput("vb_orders",   width=3)
                 ),
                 fluidRow(
                   bs4Card(title="Sales Over Time", width=8, status="primary",
                           solidHeader=TRUE, collapsible=TRUE,
                           withSpinner(plotlyOutput("chart_trend_overview", height="280px"), type=4, color="steelblue")),
                   bs4Card(title="Sales by Category", width=4, status="info",
                           solidHeader=TRUE, collapsible=TRUE,
                           withSpinner(plotlyOutput("chart_cat_overview",   height="280px"), type=4, color="steelblue"))
                 ),
                 fluidRow(
                   bs4Card(title="Profit vs Sales", width=6, status="success",
                           solidHeader=TRUE, collapsible=TRUE,
                           withSpinner(plotlyOutput("chart_scatter",        height="280px"), type=4, color="forestgreen")),
                   bs4Card(title="Sales by Region", width=6, status="warning",
                           solidHeader=TRUE, collapsible=TRUE,
                           withSpinner(plotlyOutput("chart_region_overview",height="280px"), type=4, color="orange"))
                 )
      ),
      
      # ── SALES TREND ──────────────────────────────────────
      bs4TabItem(tabName="trend",
                 bs4Card(title="Monthly Sales Trend with Moving Average",
                         width=12, status="primary", solidHeader=TRUE,
                         withSpinner(plotlyOutput("chart_trend_full", height="380px"), type=4)),
                 fluidRow(
                   bs4Card(title="Sales by Quarter", width=6, status="info", solidHeader=TRUE,
                           withSpinner(plotlyOutput("chart_quarterly",      height="280px"), type=4)),
                   bs4Card(title="Year-over-Year Comparison", width=6, status="success", solidHeader=TRUE,
                           withSpinner(plotlyOutput("chart_yoy",            height="280px"), type=4))
                 )
      ),
      
      # ── CATEGORIES ───────────────────────────────────────
      bs4TabItem(tabName="categories",
                 fluidRow(
                   bs4Card(title="Category Performance (Sales)", width=6, status="primary", solidHeader=TRUE,
                           withSpinner(plotlyOutput("chart_cat_sales",   height="320px"), type=4)),
                   bs4Card(title="Category Performance (Profit)", width=6, status="success", solidHeader=TRUE,
                           withSpinner(plotlyOutput("chart_cat_profit",  height="320px"), type=4))
                 ),
                 if(has_subcat) bs4Card(title="Sub-Category Breakdown", width=12, status="info",
                                        solidHeader=TRUE,
                                        withSpinner(plotlyOutput("chart_subcat", height="360px"), type=4))
      ),
      
      # ── GEOGRAPHY ────────────────────────────────────────
      bs4TabItem(tabName="geography",
                 fluidRow(
                   bs4Card(title="Sales by Region", width=6, status="warning", solidHeader=TRUE,
                           withSpinner(plotlyOutput("chart_region_bar",  height="320px"), type=4)),
                   bs4Card(title="Profit by Region", width=6, status="danger", solidHeader=TRUE,
                           withSpinner(plotlyOutput("chart_region_profit",height="320px"), type=4))
                 ),
                 if(has_state) bs4Card(title="Top 15 States by Sales", width=12,
                                       status="primary", solidHeader=TRUE,
                                       withSpinner(plotlyOutput("chart_state",  height="380px"), type=4))
      ),
      
      # ── PRODUCTS ─────────────────────────────────────────
      bs4TabItem(tabName="products",
                 bs4Card(title="Top N Products by Sales", width=12, status="primary", solidHeader=TRUE,
                         withSpinner(plotlyOutput("chart_top_products", height="400px"), type=4)),
                 bs4Card(title="Quantity Distribution", width=12, status="info", solidHeader=TRUE,
                         withSpinner(plotlyOutput("chart_qty_dist",    height="300px"), type=4))
      ),
      
      # ── CUSTOMERS ────────────────────────────────────────
      bs4TabItem(tabName="customers",
                 bs4Card(title="Top N Customers by Sales", width=12, status="primary", solidHeader=TRUE,
                         withSpinner(plotlyOutput("chart_top_customers", height="400px"), type=4)),
                 if(has_segment) bs4Card(title="Sales by Segment", width=12, status="success", solidHeader=TRUE,
                                         withSpinner(plotlyOutput("chart_segment",  height="300px"), type=4))
      ),
      
      # ── FORECASTING ──────────────────────────────────────
      bs4TabItem(tabName="forecast",
                 bs4Card(title="12-Month Sales Forecast (ETS Model)",
                         width=12, status="primary", solidHeader=TRUE,
                         withSpinner(plotlyOutput("chart_forecast", height="420px"), type=4)),
                 bs4Card(title="Forecast Notes", width=12, status="info",
                         p("The forecast is generated using an ETS (Error-Trend-Seasonality) exponential smoothing model on monthly aggregated sales data."),
                         p("Shaded band represents the 80% and 95% prediction intervals."))
      ),
      
      # ── DATA TABLE ───────────────────────────────────────
      bs4TabItem(tabName="datatable",
                 bs4Card(title="Filtered Dataset", width=12, status="primary", solidHeader=TRUE,
                         DTOutput("main_table"))
      ),
      
      # ── INSIGHTS ─────────────────────────────────────────
      bs4TabItem(tabName="insights",
                 fluidRow(
                   bs4Card(title=tagList(icon("star")," Smart Insights"), width=12,
                           status="warning", solidHeader=TRUE,
                           uiOutput("smart_insights"))
                 )
      )
    )
  ),
  
  footer = bs4DashFooter(
    left  = tagList(icon("copyright"), " 2024 Superstore Intelligence Hub"),
    right = "Built with R Shiny & bs4Dash"
  )
)

# ── 5. SERVER ────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Last updated
  output$last_updated <- renderText(format(Sys.time(), "%d %b %Y %H:%M"))
  
  # ── Reset filters
  observeEvent(input$reset_filters, {
    if(has_date)     updateDateRangeInput(session,"date_range",start=date_min,end=date_max)
    if(has_category) updateSelectInput(session,"sel_category", selected="All")
    if(has_subcat)   updateSelectInput(session,"sel_subcat",   selected="All")
    if(has_region)   updateSelectInput(session,"sel_region",   selected="All")
    if(has_segment)  updateSelectInput(session,"sel_segment",  selected="All")
    updateSliderInput(session,"top_n",value=10)
  })
  
  # ── Reactive filtered data
  df <- reactive({
    d <- df_raw
    if(has_date && !is.null(input$date_range)) {
      d <- d %>% filter(.data[[date_col]] >= input$date_range[1],
                        .data[[date_col]] <= input$date_range[2])
    }
    if(has_category && input$sel_category != "All")
      d <- d %>% filter(category == input$sel_category)
    if(has_subcat && input$sel_subcat != "All")
      d <- d %>% filter(sub_category == input$sel_subcat)
    if(has_region && input$sel_region != "All")
      d <- d %>% filter(region == input$sel_region)
    if(has_segment && input$sel_segment != "All")
      d <- d %>% filter(segment == input$sel_segment)
    d
  })
  
  # ── Monthly trend data
  trend_df <- reactive({
    req(has_date, has_sales)
    df() %>%
      mutate(month = floor_date(.data[[date_col]], "month")) %>%
      group_by(month) %>%
      summarise(sales = sum(sales, na.rm=TRUE),
                profit= sum(profit, na.rm=TRUE), .groups="drop") %>%
      arrange(month) %>%
      mutate(ma3 = rollmean(sales, k=3, fill=NA, align="right"))
  })
  
  # ── KPI Value Boxes ─────────────────────────────────────
  output$vb_sales <- renderbs4ValueBox({
    val <- if(has_sales) dollar(sum(df()$sales, na.rm=TRUE)) else "N/A"
    bs4ValueBox(value=val, subtitle="Total Sales",
                icon=icon("dollar-sign"), color="primary",
                footer=tagList(icon("chart-line"), " All time sales revenue"))
  })
  
  output$vb_profit <- renderbs4ValueBox({
    val <- if(has_profit) dollar(sum(df()$profit, na.rm=TRUE)) else "N/A"
    bs4ValueBox(value=val, subtitle="Total Profit",
                icon=icon("hand-holding-usd"), color="success",
                footer=tagList(icon("percentage"), " Net profit across all orders"))
  })
  
  output$vb_quantity <- renderbs4ValueBox({
    val <- if(has_qty) comma(sum(df()$quantity, na.rm=TRUE)) else "N/A"
    bs4ValueBox(value=val, subtitle="Units Sold",
                icon=icon("boxes"), color="warning",
                footer=tagList(icon("shopping-cart"), " Total quantity dispatched"))
  })
  
  output$vb_orders <- renderbs4ValueBox({
    val <- comma(nrow(df()))
    bs4ValueBox(value=val, subtitle="Total Records",
                icon=icon("receipt"), color="danger",
                footer=tagList(icon("database"), " Filtered transaction count"))
  })
  
  # ── OVERVIEW: Sales Trend ────────────────────────────────
  overview_trend <- reactive({
    req(has_date, has_sales)
    trend_df()
  })
  chart_trend_fn <- function(height=300) {
    req(nrow(overview_trend()) > 0)
    d <- overview_trend()
    plot_ly(d, x=~month) %>%
      add_lines(y=~sales, name="Sales",
                line=list(color="steelblue", width=2.5)) %>%
      add_lines(y=~ma3,   name="3M Moving Avg",
                line=list(color="tomato", dash="dash", width=2)) %>%
      layout(title=list(text=""),
             xaxis=list(title="", showgrid=FALSE),
             yaxis=list(title="Sales ($)", tickformat="$,.0f"),
             legend=list(orientation="h", y=-0.2),
             paper_bgcolor="rgba(0,0,0,0)",
             plot_bgcolor ="rgba(0,0,0,0)",
             font=list(family="Segoe UI"))
  }
  output$chart_trend_overview <- renderPlotly({ chart_trend_fn() })
  output$chart_trend_full     <- renderPlotly({
    req(nrow(overview_trend()) > 0)
    d <- overview_trend()
    plot_ly(d, x=~month) %>%
      add_lines(y=~sales, name="Monthly Sales",
                line=list(color="steelblue", width=2.5),
                fill="tozeroy", fillcolor="rgba(70,130,180,0.1)") %>%
      add_lines(y=~ma3,   name="3-Month MA",
                line=list(color="tomato", dash="dash", width=2)) %>%
      layout(xaxis=list(title="Month", showgrid=FALSE),
             yaxis=list(title="Sales ($)", tickformat="$,.0f"),
             hovermode="x unified",
             paper_bgcolor="rgba(0,0,0,0)",
             plot_bgcolor ="rgba(0,0,0,0)",
             font=list(family="Segoe UI"))
  })
  
  # ── OVERVIEW: Category pie ───────────────────────────────
  output$chart_cat_overview <- renderPlotly({
    req(has_category, has_sales)
    d <- df() %>% group_by(category) %>%
      summarise(sales=sum(sales,na.rm=TRUE), .groups="drop")
    plot_ly(d, labels=~category, values=~sales, type="pie",
            textposition="inside", textinfo="percent+label",
            marker=list(colors=c("steelblue","orange","forestgreen",
                                 "tomato","purple","gold"))) %>%
      layout(showlegend=FALSE,
             paper_bgcolor="rgba(0,0,0,0)",
             font=list(family="Segoe UI"))
  })
  
  # ── Scatter: Profit vs Sales ─────────────────────────────
  output$chart_scatter <- renderPlotly({
    req(has_sales, has_profit)
    d <- df()
    grp <- if(has_category) "category" else if(has_segment) "segment" else NULL
    p <- if(!is.null(grp)) {
      plot_ly(d, x=~sales, y=~profit, color=~.data[[grp]],
              type="scatter", mode="markers",
              marker=list(size=6, opacity=0.6))
    } else {
      plot_ly(d, x=~sales, y=~profit, type="scatter", mode="markers",
              marker=list(size=6, color="steelblue", opacity=0.6))
    }
    p %>% layout(xaxis=list(title="Sales ($)",  tickformat="$,.0f", showgrid=FALSE),
                 yaxis=list(title="Profit ($)", tickformat="$,.0f"),
                 paper_bgcolor="rgba(0,0,0,0)",
                 plot_bgcolor ="rgba(0,0,0,0)",
                 font=list(family="Segoe UI"),
                 legend=list(orientation="h", y=-0.2))
  })
  
  # ── Region bar overview ──────────────────────────────────
  output$chart_region_overview <- renderPlotly({
    req(has_region, has_sales)
    d <- df() %>% group_by(region) %>%
      summarise(sales=sum(sales,na.rm=TRUE),.groups="drop") %>%
      arrange(desc(sales))
    plot_ly(d, x=~reorder(region,-sales), y=~sales,
            type="bar", marker=list(color="orange")) %>%
      layout(xaxis=list(title="", showgrid=FALSE),
             yaxis=list(title="Sales ($)", tickformat="$,.0f"),
             paper_bgcolor="rgba(0,0,0,0)",
             plot_bgcolor ="rgba(0,0,0,0)",
             font=list(family="Segoe UI"))
  })
  
  # ── Quarterly ────────────────────────────────────────────
  output$chart_quarterly <- renderPlotly({
    req(has_date, has_sales)
    d <- df() %>%
      mutate(qtr = paste0(year(.data[[date_col]])," Q",quarter(.data[[date_col]]))) %>%
      group_by(qtr) %>%
      summarise(sales=sum(sales,na.rm=TRUE),.groups="drop") %>%
      arrange(qtr)
    plot_ly(d, x=~qtr, y=~sales, type="bar",
            marker=list(color="steelblue")) %>%
      layout(xaxis=list(title="Quarter", tickangle=-45, showgrid=FALSE),
             yaxis=list(title="Sales ($)", tickformat="$,.0f"),
             paper_bgcolor="rgba(0,0,0,0)",
             plot_bgcolor ="rgba(0,0,0,0)",
             font=list(family="Segoe UI"))
  })
  
  # ── YoY ──────────────────────────────────────────────────
  output$chart_yoy <- renderPlotly({
    req(has_date, has_sales)
    d <- df() %>%
      mutate(yr  = year(.data[[date_col]]),
             mon = month(.data[[date_col]], label=TRUE)) %>%
      group_by(yr, mon) %>%
      summarise(sales=sum(sales,na.rm=TRUE),.groups="drop")
    plot_ly(d, x=~mon, y=~sales, color=~factor(yr), type="scatter",
            mode="lines+markers", line=list(width=2)) %>%
      layout(xaxis=list(title="Month", showgrid=FALSE),
             yaxis=list(title="Sales ($)", tickformat="$,.0f"),
             legend=list(title=list(text="Year"), orientation="h", y=-0.25),
             paper_bgcolor="rgba(0,0,0,0)",
             plot_bgcolor ="rgba(0,0,0,0)",
             font=list(family="Segoe UI"))
  })
  
  # ── Category: Sales & Profit ─────────────────────────────
  cat_summary <- reactive({
    req(has_category)
    df() %>% group_by(category) %>%
      summarise(sales  = if(has_sales)  sum(sales, na.rm=TRUE)  else 0,
                profit = if(has_profit) sum(profit,na.rm=TRUE)  else 0,
                .groups="drop") %>% arrange(desc(sales))
  })
  output$chart_cat_sales <- renderPlotly({
    d <- cat_summary()
    plot_ly(d, y=~reorder(category,sales), x=~sales,
            type="bar", orientation="h",
            marker=list(color="steelblue")) %>%
      layout(xaxis=list(title="Sales ($)", tickformat="$,.0f", showgrid=FALSE),
             yaxis=list(title=""),
             paper_bgcolor="rgba(0,0,0,0)",
             plot_bgcolor ="rgba(0,0,0,0)",
             font=list(family="Segoe UI"))
  })
  output$chart_cat_profit <- renderPlotly({
    d <- cat_summary()
    clrs <- ifelse(d$profit>=0,"forestgreen","tomato")
    plot_ly(d, y=~reorder(category,profit), x=~profit,
            type="bar", orientation="h",
            marker=list(color=clrs)) %>%
      layout(xaxis=list(title="Profit ($)", tickformat="$,.0f", showgrid=FALSE),
             yaxis=list(title=""),
             paper_bgcolor="rgba(0,0,0,0)",
             plot_bgcolor ="rgba(0,0,0,0)",
             font=list(family="Segoe UI"))
  })
  
  # ── Sub-Category ─────────────────────────────────────────
  output$chart_subcat <- renderPlotly({
    req(has_subcat, has_sales)
    d <- df() %>% group_by(sub_category) %>%
      summarise(sales=sum(sales,na.rm=TRUE),
                profit=sum(profit,na.rm=TRUE),.groups="drop") %>%
      arrange(desc(sales))
    plot_ly(d, x=~reorder(sub_category,-sales), y=~sales, name="Sales",
            type="bar", marker=list(color="steelblue")) %>%
      add_bars(y=~profit, name="Profit",
               marker=list(color="orange")) %>%
      layout(barmode="group",
             xaxis=list(title="", tickangle=-35, showgrid=FALSE),
             yaxis=list(title="Amount ($)", tickformat="$,.0f"),
             legend=list(orientation="h", y=-0.25),
             paper_bgcolor="rgba(0,0,0,0)",
             plot_bgcolor ="rgba(0,0,0,0)",
             font=list(family="Segoe UI"))
  })
  
  # ── Region ───────────────────────────────────────────────
  output$chart_region_bar <- renderPlotly({
    req(has_region, has_sales)
    d <- df() %>% group_by(region) %>%
      summarise(sales=sum(sales,na.rm=TRUE),.groups="drop") %>% arrange(desc(sales))
    plot_ly(d, y=~reorder(region,sales), x=~sales,
            type="bar", orientation="h",
            marker=list(color=c("steelblue","orange","forestgreen","tomato","purple")[1:nrow(d)])) %>%
      layout(xaxis=list(title="Sales ($)", tickformat="$,.0f", showgrid=FALSE),
             yaxis=list(title=""),
             paper_bgcolor="rgba(0,0,0,0)",
             plot_bgcolor ="rgba(0,0,0,0)",
             font=list(family="Segoe UI"))
  })
  output$chart_region_profit <- renderPlotly({
    req(has_region, has_profit)
    d <- df() %>% group_by(region) %>%
      summarise(profit=sum(profit,na.rm=TRUE),.groups="drop") %>% arrange(desc(profit))
    clrs <- ifelse(d$profit>=0,"forestgreen","tomato")
    plot_ly(d, y=~reorder(region,profit), x=~profit,
            type="bar", orientation="h",
            marker=list(color=clrs)) %>%
      layout(xaxis=list(title="Profit ($)", tickformat="$,.0f", showgrid=FALSE),
             yaxis=list(title=""),
             paper_bgcolor="rgba(0,0,0,0)",
             plot_bgcolor ="rgba(0,0,0,0)",
             font=list(family="Segoe UI"))
  })
  
  # ── Top States ───────────────────────────────────────────
  output$chart_state <- renderPlotly({
    req(has_state, has_sales)
    d <- df() %>% group_by(state) %>%
      summarise(sales=sum(sales,na.rm=TRUE),.groups="drop") %>%
      arrange(desc(sales)) %>% head(15)
    plot_ly(d, y=~reorder(state,sales), x=~sales,
            type="bar", orientation="h",
            marker=list(color="steelblue")) %>%
      layout(xaxis=list(title="Sales ($)", tickformat="$,.0f", showgrid=FALSE),
             yaxis=list(title=""),
             paper_bgcolor="rgba(0,0,0,0)",
             plot_bgcolor ="rgba(0,0,0,0)",
             font=list(family="Segoe UI"))
  })
  
  # ── Top Products ─────────────────────────────────────────
  output$chart_top_products <- renderPlotly({
    req(has_product, has_sales)
    n <- input$top_n
    d <- df() %>% group_by(product_name) %>%
      summarise(sales=sum(sales,na.rm=TRUE),.groups="drop") %>%
      arrange(desc(sales)) %>% head(n) %>%
      mutate(label=substr(product_name,1,40))
    plot_ly(d, y=~reorder(label,sales), x=~sales,
            type="bar", orientation="h",
            marker=list(color="steelblue",
                        line=list(color="white",width=0.5)),
            text=~dollar(sales), textposition="outside") %>%
      layout(xaxis=list(title="Sales ($)", tickformat="$,.0f", showgrid=FALSE),
             yaxis=list(title=""),
             paper_bgcolor="rgba(0,0,0,0)",
             plot_bgcolor ="rgba(0,0,0,0)",
             font=list(family="Segoe UI", size=10),
             margin=list(l=280))
  })
  
  # ── Quantity Distribution ────────────────────────────────
  output$chart_qty_dist <- renderPlotly({
    req(has_qty)
    d <- df()
    if(has_category) {
      plot_ly(d, x=~quantity, color=~category, type="histogram",
              opacity=0.7, nbinsx=30) %>%
        layout(barmode="overlay",
               xaxis=list(title="Quantity", showgrid=FALSE),
               yaxis=list(title="Count"),
               paper_bgcolor="rgba(0,0,0,0)",
               plot_bgcolor ="rgba(0,0,0,0)",
               font=list(family="Segoe UI"))
    } else {
      plot_ly(d, x=~quantity, type="histogram",
              marker=list(color="steelblue"), nbinsx=30) %>%
        layout(xaxis=list(title="Quantity", showgrid=FALSE),
               yaxis=list(title="Count"),
               paper_bgcolor="rgba(0,0,0,0)",
               plot_bgcolor ="rgba(0,0,0,0)",
               font=list(family="Segoe UI"))
    }
  })
  
  # ── Top Customers ────────────────────────────────────────
  output$chart_top_customers <- renderPlotly({
    req(has_customer, has_sales)
    n <- input$top_n
    d <- df() %>% group_by(customer_name) %>%
      summarise(sales=sum(sales,na.rm=TRUE),
                orders=n(),.groups="drop") %>%
      arrange(desc(sales)) %>% head(n)
    plot_ly(d, y=~reorder(customer_name,sales), x=~sales,
            type="bar", orientation="h",
            marker=list(color="purple"),
            text=~dollar(sales), textposition="outside") %>%
      layout(xaxis=list(title="Sales ($)", tickformat="$,.0f", showgrid=FALSE),
             yaxis=list(title=""),
             paper_bgcolor="rgba(0,0,0,0)",
             plot_bgcolor ="rgba(0,0,0,0)",
             font=list(family="Segoe UI", size=11),
             margin=list(l=150))
  })
  
  # ── Segment ──────────────────────────────────────────────
  output$chart_segment <- renderPlotly({
    req(has_segment, has_sales)
    d <- df() %>% group_by(segment) %>%
      summarise(sales=sum(sales,na.rm=TRUE),
                profit=sum(profit,na.rm=TRUE),.groups="drop")
    plot_ly(d, x=~segment, y=~sales, name="Sales",
            type="bar", marker=list(color="steelblue")) %>%
      add_bars(y=~profit, name="Profit", marker=list(color="forestgreen")) %>%
      layout(barmode="group",
             xaxis=list(title="", showgrid=FALSE),
             yaxis=list(title="Amount ($)", tickformat="$,.0f"),
             legend=list(orientation="h", y=-0.2),
             paper_bgcolor="rgba(0,0,0,0)",
             plot_bgcolor ="rgba(0,0,0,0)",
             font=list(family="Segoe UI"))
  })
  
  # ── FORECAST ─────────────────────────────────────────────
  output$chart_forecast <- renderPlotly({
    req(has_date, has_sales, nrow(trend_df()) >= 12)
    td  <- trend_df()
    ts_obj <- tryCatch(
      ts(td$sales, frequency=12,
         start=c(year(min(td$month)), month(min(td$month)))),
      error=function(e) NULL
    )
    req(!is.null(ts_obj))
    fc  <- tryCatch(forecast(ets(ts_obj), h=12), error=function(e) NULL)
    req(!is.null(fc))
    
    hist_dates <- td$month
    fc_dates   <- seq(max(hist_dates) %m+% months(1),
                      by="month", length.out=12)
    
    plot_ly() %>%
      add_lines(x=hist_dates, y=td$sales, name="Actual",
                line=list(color="steelblue", width=2.5)) %>%
      add_lines(x=fc_dates, y=as.numeric(fc$mean), name="Forecast",
                line=list(color="tomato", dash="dash", width=2)) %>%
      add_ribbons(x=fc_dates,
                  ymin=as.numeric(fc$lower[,"80%"]),
                  ymax=as.numeric(fc$upper[,"80%"]),
                  name="80% CI", fillcolor="rgba(255,99,71,0.15)",
                  line=list(color="rgba(0,0,0,0)")) %>%
      add_ribbons(x=fc_dates,
                  ymin=as.numeric(fc$lower[,"95%"]),
                  ymax=as.numeric(fc$upper[,"95%"]),
                  name="95% CI", fillcolor="rgba(255,99,71,0.08)",
                  line=list(color="rgba(0,0,0,0)")) %>%
      layout(xaxis=list(title="Month", showgrid=FALSE),
             yaxis=list(title="Sales ($)", tickformat="$,.0f"),
             legend=list(orientation="h", y=-0.15),
             paper_bgcolor="rgba(0,0,0,0)",
             plot_bgcolor ="rgba(0,0,0,0)",
             font=list(family="Segoe UI"))
  })
  
  # ── DATA TABLE ───────────────────────────────────────────
  output$main_table <- renderDT({
    d <- df()
    datatable(
      d,
      filter   = "top",
      rownames = FALSE,
      extensions = "Buttons",
      options  = list(
        pageLength = 15,
        scrollX    = TRUE,
        dom        = "Bfrtip",
        buttons    = c("csv","excel","print"),
        columnDefs = list(list(className="dt-center", targets="_all")),
        initComplete = JS(
          "function(settings,json){",
          "$(this.api().table().header()).css({'background-color':'#2c3e50','color':'#fff'});",
          "}"
        )
      ),
      class = "table table-striped table-hover table-bordered"
    ) %>%
      formatStyle(columns=colnames(d), fontSize="12px") %>%
      (function(tbl) {
        if(has_sales)  tbl <- formatCurrency(tbl, "sales")
        if(has_profit) tbl <- formatCurrency(tbl, "profit")
        tbl
      })()
  })
  
  # ── SMART INSIGHTS ───────────────────────────────────────
  output$smart_insights <- renderUI({
    d <- df()
    
    insight_card <- function(icon_nm, color, title, text) {
      div(class=paste0("callout callout-",color),
          style="margin:10px 0; padding:10px 16px; border-left:4px solid;",
          h5(tagList(icon(icon_nm), " ", title),
             style=paste0("color:",color,";")),
          p(text))
    }
    
    insights <- list()
    
    # Best category
    if(has_category && has_sales) {
      top_cat <- d %>% group_by(category) %>%
        summarise(s=sum(sales,na.rm=TRUE),.groups="drop") %>%
        arrange(desc(s)) %>% slice(1)
      insights[[1]] <- insight_card(
        "trophy","steelblue","Top Category by Sales",
        paste0("'",top_cat$category,"' leads with ",dollar(top_cat$s)," in total sales."))
    }
    # Best region
    if(has_region && has_sales) {
      top_reg <- d %>% group_by(region) %>%
        summarise(s=sum(sales,na.rm=TRUE),.groups="drop") %>%
        arrange(desc(s)) %>% slice(1)
      insights[[2]] <- insight_card(
        "globe","forestgreen","Best Region by Sales",
        paste0(top_reg$region," is the highest-revenue region at ",dollar(top_reg$s),"."))
    }
    # Avg profit
    if(has_profit) {
      avg_p <- mean(d$profit, na.rm=TRUE)
      pct   <- if(has_sales) percent(sum(d$profit,na.rm=TRUE)/sum(d$sales,na.rm=TRUE),0.1) else "N/A"
      col   <- if(avg_p>0) "success" else "danger"
      insights[[3]] <- insight_card(
        "percent",col,"Average Profit per Transaction",
        paste0("Average profit is ",dollar(avg_p),
               " per order. Overall margin: ",pct,"."))
    }
    # Loss-making sub-categories
    if(has_subcat && has_profit) {
      losers <- d %>% group_by(sub_category) %>%
        summarise(p=sum(profit,na.rm=TRUE),.groups="drop") %>%
        filter(p<0) %>% arrange(p)
      if(nrow(losers)>0) {
        insights[[4]] <- insight_card(
          "exclamation-triangle","warning","Loss-Making Sub-Categories",
          paste0("The following sub-categories are loss-making: ",
                 paste(losers$sub_category, collapse=", "),
                 ". Review pricing or discount strategy."))
      }
    }
    # Sales trend direction
    if(has_date && has_sales && nrow(trend_df())>=3) {
      td  <- trend_df()
      pct_chg <- (last(td$sales)-first(td$sales))/first(td$sales)
      dir <- if(pct_chg>0) "upward" else "downward"
      col <- if(pct_chg>0) "primary" else "danger"
      insights[[5]] <- insight_card(
        "chart-line",col,"Overall Sales Trend",
        paste0("Sales show an overall ",dir," trend over the selected period (",
               percent(abs(pct_chg),0.1)," change from start to end)."))
    }
    # High discount impact
    if("discount" %in% num_cols && has_profit) {
      corr <- tryCatch(cor(d$discount, d$profit, use="complete.obs"), error=function(e) NA)
      if(!is.na(corr) && corr < -0.15) {
        insights[[6]] <- insight_card(
          "tags","danger","Discount-Profit Alert",
          paste0("Discounts show a negative correlation with profit (r = ",
                 round(corr,2),"). Reducing excessive discounts may improve margins."))
      }
    }
    
    if(length(insights)==0) {
      insights[[1]] <- p("Apply filters and explore the dashboard to generate insights.")
    }
    
    tagList(insights)
  })
  
}

# ── 6. LAUNCH ────────────────────────────────────────────────
shinyApp(ui, server)