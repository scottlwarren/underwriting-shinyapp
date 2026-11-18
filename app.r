library(tidyverse)
library(shiny)
library(DBI)
library(duckdb)

policy_bdx <- read.csv('./data/policy_bordereau.csv')
claims_bdx <- read.csv('./data/claims_bordereau.csv')
uw_spec    <- read.csv('./data/underwriting_spec.csv')

con <- dbConnect(duckdb::duckdb())
onStop(function() dbDisconnect(con, shutdown = TRUE))

duckdb_register(con, 'claims', claims_bdx)
duckdb_register(con, 'policies', policy_bdx)

age_bands <- c('0-4', '5-10', '11-15', '16-20', '20+')

dbExecute(con, "DROP TABLE IF EXISTS policies_metrics")
tbl(con, 'policies') %>% 
  mutate(
    age_band = case_when(
      horse_age >= 0  & horse_age < 5   ~ '0-4',
      horse_age >= 5  & horse_age < 11  ~ '5-10',
      horse_age >= 11 & horse_age < 16  ~ '11-15',
      horse_age >= 16 & horse_age <= 20 ~ '16-20',
      horse_age > 20                    ~ '20+',
      TRUE                              ~ 'Unk'
    )
  ) %>%
  compute(name = 'policies_metrics', temporary = FALSE)

dbExecute(con, "DROP TABLE IF EXISTS claims_tri")
tbl(con, 'claims') %>%
  left_join(
    tbl(con, 'policies_metrics') %>%
      select(policy_ref, uw_year, inception_date, age_band),
    by = 'policy_ref'
  ) %>%
  mutate(
    loss_year = year(as.Date(loss_date)),
    dev_year  = loss_year - uw_year
  ) %>%
  compute(name = 'claims_tri', temporary = FALSE)

duckdb_unregister(con, 'policies')
duckdb_unregister(con, 'claims')

################################################################################

ui <- fluidPage(
  titlePanel('Horse Insurance Underwriting Tool POC'),
  sidebarLayout(
    sidebarPanel(
      tags$strong("Horse Age Group Factors (Multiplier):"),
      tags$br(), tags$br(),
      fluidRow(
        lapply(age_bands, function(band) {
          input_id <- paste0("factor_", gsub("[^0-9]", "_", band))
          column(
            width = 6,
            numericInput(
              inputId = input_id,
              label   = paste("Age", band),
              value   = 1.00,
              min     = 0.10,
              max     = 5.00,
              step    = 0.05
            )
          )
        })
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Age Band Summary", tableOutput("age_summary_tbl")),
        tabPanel("Claims Triangle (UW Year)", tableOutput("triangle_tbl"))
      )
    )
  )
)

################################################################################

server <- function(input, output, session) {
  age_factor_table <- reactive({
    factors <- sapply(age_bands, function(band) {
      input_id <- paste0("factor_", gsub("[^0-9]", "_", band))
      val <- input[[input_id]]
      if (is.null(val) || is.na(val)) 1.0 else val
    })

    tibble(
      age_band   = age_bands,
      age_factor = as.numeric(factors)
    )
  })
  
  ## ageband factors tab
  output$age_summary_tbl <- renderTable({
    prem_summary <- tbl(con, 'policies_metrics') %>%
      left_join(age_factor_table(), by = 'age_band', copy = TRUE) %>%
      group_by(age_band) %>%
      summarise(
        age_factor       = max(age_factor, na.rm = TRUE),
        policy_count     = n(),
        base_premium     = sum(gross_premium, na.rm = TRUE),
        adjusted_premium = sum(gross_premium * age_factor, na.rm = TRUE),
        .groups          = 'drop'
      ) %>%
      collect()

    loss_summary <- tbl(con, 'claims_tri') %>%
      group_by(age_band) %>%
      summarise(
        claim_count    = n(), 
        total_incurred = sum(incurred, na.rm = TRUE),
        .groups        = 'drop'
      ) %>%
      collect()
    
    prem_summary %>%
      left_join(loss_summary, by = 'age_band') %>%
      mutate(
        age_band       = factor(age_band, levels = age_bands),
        total_incurred = coalesce(total_incurred, 0),
        base_lr        = sprintf("%.1f%%", (total_incurred / base_premium) * 100),
        adjusted_lr    = sprintf("%.1f%%", (total_incurred / adjusted_premium) * 100)
      ) %>%
      arrange(age_band)        
  })

  ## tri tab
  output$triangle_tbl <- renderTable({
    tbl(con, 'claims_tri') %>%
      filter(!is.na(uw_year), dev_year >= 0) %>%
      group_by(uw_year, dev_year) %>%
      summarise(
        incurred = sum(incurred, na.rm = TRUE),
        .groups  = 'drop'
      ) %>%
      collect() %>%
      arrange(uw_year, dev_year) %>%
      group_by(uw_year) %>%
      mutate(cumulative_incurred = cumsum(incurred)) %>%
      select(uw_year, dev_year, cumulative_incurred) %>%
      pivot_wider(
        names_from   = dev_year,
        values_from  = cumulative_incurred, 
        names_prefix = 'Dev_'
      )
  }, na = '')
}
  
shinyApp(ui, server)