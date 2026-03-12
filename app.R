library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

# Load data
sales_data <- read_csv("data/raw/amazon_sales_dataset.csv", show_col_types = FALSE) |>
  mutate(
    Year = year(order_date),
    Month_Num = month(order_date),
    Month = month(order_date, label = TRUE, abbr = TRUE),
    Quarter = quarter(order_date, with_year = FALSE, fiscal_start = 1),
    Quarter = paste0("Q", Quarter)
  )

# Filter choices from real data
years <- sort(unique(sales_data$Year))
months <- month.abb[sort(unique(sales_data$Month_Num))]
categories <- sort(unique(sales_data$product_category))
regions <- sort(unique(sales_data$customer_region))

ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  tags$head(
    tags$style(HTML("
      .value-box {
        border-radius: 12px;
        padding: 22px;
        color: white;
        font-weight: 600;
        margin-bottom: 18px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.12);
      }
      .value-box-blue {
        background: #0d6efd;
      }
      .value-box-cyan {
        background: #17c0eb;
      }
      .panel-card {
        background: white;
        border-radius: 12px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.10);
      }
      .chart-title {
        font-size: 18px;
        font-weight: 600;
        margin-bottom: 12px;
      }
      .placeholder-box {
        height: 320px;
        display: flex;
        align-items: center;
        justify-content: center;
        border: 2px dashed #d9d9d9;
        border-radius: 10px;
        color: #888;
        background: #fafafa;
      }
      .sidebar-title {
        font-size: 18px;
        font-weight: 600;
        margin-bottom: 18px;
      }
    "))
  ),
  
  titlePanel("Amazon Sales Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "sidebar-title", "Filters"),
      
      checkboxGroupInput(
        "years",
        "Years",
        choices = years,
        selected = years
      ),
      
      selectizeInput(
        "months",
        "Months",
        choices = months,
        selected = months,
        multiple = TRUE
      ),
      
      selectizeInput(
        "categories",
        "Categories (Max 3)",
        choices = categories,
        selected = head(categories, min(3, length(categories))),
        multiple = TRUE,
        options = list(maxItems = 3)
      ),
      
      checkboxGroupInput(
        "regions",
        "Regions",
        choices = regions,
        selected = regions
      ),
      
      radioButtons(
        "metric",
        "Primary Metric",
        choices = c("Revenue ($)" = "Revenue", "Total Orders" = "Orders"),
        selected = "Revenue"
      ),
      
      div(
        style = "margin-bottom: 18px;",
        checkboxInput("show_aggregate", "Show Aggregate", value = FALSE)
      ),
      
      div(
        style = "margin-bottom: 18px;",
        checkboxInput("show_seasonality", "Show Seasonality", value = TRUE)
      ),
      
      actionButton(
        "reset_filters",
        "Reset All Filters",
        class = "btn-warning"
      )
    ),
    
    mainPanel(
      fluidRow(
        column(
          6,
          div(
            class = "value-box value-box-blue",
            textOutput("total_revenue")
          )
        ),
        column(
          6,
          div(
            class = "value-box value-box-cyan",
            textOutput("total_orders")
          )
        )
      ),
      
      fluidRow(
        column(
          12,
          div(
            class = "panel-card",
            div(class = "chart-title", "Revenue Trends"),
            plotOutput("revenue_trend_plot", height = "320px")
        )
        )
      ),
      
      fluidRow(
        column(
          6,
          div(
            class = "panel-card",
            div(class = "chart-title", "Total Revenue by Quarter"),
            div(class = "placeholder-box", "Quarter chart will go here")
          )
        ),
        column(
          6,
          div(
            class = "panel-card",
            div(class = "chart-title", "Revenue by Payment Method"),
            div(class = "placeholder-box", "Payment method chart will go here")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  filtered_data <- reactive({
    req(input$years, input$months, input$categories, input$regions)

    sales_data |>
      filter(
        Year %in% input$years,
        as.character(Month) %in% input$months,
        product_category %in% input$categories,
        customer_region %in% input$regions
      )
  })

    output$revenue_trend_plot <- renderPlot({

        trend_data <- filtered_data() |>
            mutate(month_start = floor_date(order_date, unit = "month")) |>
            group_by(month_start, product_category) |>
            summarise(
            total_revenue = sum(total_revenue, na.rm = TRUE),
            .groups = "drop"
            )

        ggplot(trend_data, aes(x = month_start, y = total_revenue, color = product_category)) +
            geom_line(linewidth = 1.2) +
            geom_point(size = 2) +
            labs(
                x = "Month",
                y = "Revenue ($)",
                color = "Category"
            ) +
            theme_minimal(base_size = 13)+
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1)
            )
    })


    output$total_revenue <- renderText({

    revenue <- sum(filtered_data()$total_revenue, na.rm = TRUE)
        paste0(
            "Total Revenue: $",
            format(round(revenue, 0), big.mark = ",")
        )
    })

    output$total_orders <- renderText({

        orders <- sum(filtered_data()$quantity_sold, na.rm = TRUE)

        paste0(
            "Total Orders: ",
            format(round(orders, 0), big.mark = ",")
        )
    })

  observeEvent(input$reset_filters, {

    updateCheckboxGroupInput(
      session,
      "years",
      selected = years
    )

    updateSelectizeInput(
      session,
      "months",
      selected = months
    )

    updateSelectizeInput(
      session,
      "categories",
      selected = head(categories, min(3, length(categories)))
    )

    updateCheckboxGroupInput(
      session,
      "regions",
      selected = regions
    )

    updateRadioButtons(
      session,
      "metric",
      selected = "Revenue"
    )

    updateCheckboxInput(
      session,
      "show_aggregate",
      value = FALSE
    )

    updateCheckboxInput(
      session,
      "show_seasonality",
      value = TRUE
    )

  })

}

shinyApp(ui = ui, server = server)