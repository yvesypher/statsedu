library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)

color_palette <- c(
  "0" = "#f0f0f0",
  "1" = "#d9f0a3",
  "2" = "#addd8e",
  "3+" = "#41ab5d"
)

# Calendar 2026
base_calendar <- data.frame(date = seq(as.Date("2026-01-01"), as.Date("2026-12-31"), by = "day")) |>
  mutate(
    day_of_year = row_number(),
    month_label = month(date, label = TRUE, abbr = FALSE),
    day_in_month = mday(date)
  ) |>
  group_by(month_label) |>
  mutate(
    
    month_start_wday = wday(floor_date(date, "month"), week_start = 1),
    
    
    col_idx = wday(date, week_start = 1),
    
    
    row_idx = -ceiling((day_in_month + month_start_wday - 1) / 7)
  ) |>
  ungroup()

# ----------------- UI ----------------- 
ui <- fluidPage(
  tags$style(
    HTML("h2 { margin-top: 10px; } .shiny-plot-output { margin-top: 0px; }")
  ),
  
  titlePanel("Birthday Paradox: 2026 Calendar"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput(
        "num_people",
        "Number of people:",
        min = 1,
        max = 150,
        value = 23
      ),
      actionButton("reset_btn", "Reset Simulation", class = "btn-warning"),
      hr(),
      h4("Probabilities"),
      uiOutput("stats_ui"),
      hr()
    ),
    
    mainPanel(width = 9, plotOutput("calendarPlot", height = "550px"))
  )
)

# ----------------- server ----------------- 
server <- function(input, output, session) {
  rv <- reactiveValues(birthdays = c())
  
  observeEvent(input$num_people, {
    if (input$num_people > length(rv$birthdays)) {
      needed <- input$num_people - length(rv$birthdays)
      new_people <- sample(1:365, needed, replace = TRUE)
      rv$birthdays <- c(rv$birthdays, new_people)
    }
    else if (input$num_people < length(rv$birthdays)) {
      rv$birthdays <- rv$birthdays[1:input$num_people]
    }
  })
  
  observeEvent(input$reset_btn, {
    updateSliderInput(session, "num_people", value = 1)
    rv$birthdays <- sample(1:365, 1, replace = TRUE)
  })
  
  output$stats_ui <- renderUI({
    n <- input$num_people
    prob_no_match <- prod(seq(365, 365 - n + 1) / 365)
    prob_match <- 1 - prob_no_match
    
    tagList(p(paste("Room Count:", n)), p(
      paste(
        "Probability of at least one birthday match:",
        scales::percent(prob_match, accuracy = 0.01)
      )
    ))
  })
  
  output$calendarPlot <- renderPlot({
    req(rv$birthdays)
    
    counts_df <- data.frame(day_of_year = rv$birthdays) |>
      count(day_of_year, name = "people_count")
    
    plot_data <- base_calendar |>
      left_join(counts_df, by = "day_of_year") |>
      mutate(
        people_count = ifelse(is.na(people_count), 0, people_count),
        fill_category = case_when(
          people_count == 0 ~ "0",
          people_count == 1 ~ "1",
          people_count == 2 ~ "2",
          TRUE ~ "3+"
        ),
        fill_category = factor(fill_category, levels = c("0", "1", "2", "3+")),
        label_text = ifelse(people_count > 0, as.character(people_count), "")
      )
    
    ggplot(plot_data, aes(x = col_idx, y = row_idx)) +
      geom_tile(aes(fill = fill_category),
                color = "white",
                size = 0.5) +
      geom_text(aes(label = label_text),
                size = 2.5,
                color = "#555555") +
      facet_wrap( ~ month_label, ncol = 4) +
      scale_fill_manual(values = color_palette, name = "People sharing\nbirthday") +
      coord_fixed() +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(
          size = 11,
          face = "bold",
          color = "#888888"
        ),
        legend.position = "bottom",
        plot.margin = margin(
          t = 0,
          r = 0,
          b = 0,
          l = 0,
          unit = "pt"
        )
      )
  })
}

shinyApp(ui = ui, server = server)