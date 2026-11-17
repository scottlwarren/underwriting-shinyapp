library(shiny)
library(ggplot2)

policy_bdx <- read.csv('./data/policy_bordereau.csv')
claims_bdx <- read.csv('./data/claims_bordereau.csv')
uw_spec <- read.csv('./data/underwriting_spec.csv')

head(uw_spec)

 ### So starting out, we need to look at getting these claims and policies in a position where we can get a simple triangulation. Policy has underwriting year and inception date, claims has loss date. However because we don't have the UW year for the claim, we will need to tie that back to the policy number and likely will need a join on the policy name.

library(duckdb)
## instead of doing the transformations in the main df, I'll do them in duckdb.
con <- dbConnect(duckdb::duckdb())
duckdb_register(con, 'claims', claims_bdx)
duckdb_register(con, 'policies', policy_bdx)


age_bands <- c('0-4', '5-10', '11-15', '16-20', '20+')
tbl(con, 'policies') %>% 
  mutate(
    age_band = case_when(
      horse_age >= 0 && horse_age < 5 ~ '0-4',
      horse_age >= 5 && horse_age < 11 ~ '5-10',
      horse_age >= 11 && horse_age < 16 ~ '11-15',
      horse_age >= 16 && horse_age < 20 ~ '16-20',
      horse_age >= 20 ~ '20+',
      TRUE ~ 'Unk'
    )
  ) %>%
compute(name = 'policies_metrics', temporary = FALSE, overwrite = TRUE) ## essentially creates a materialised view.

duckdb_unregister(con, 'policies')

dbGetQuery(con, 'SELECT * FROM policies_metrics LIMIT 100')
dbGetQuery(con, 'SELECT * FROM claims LIMIT 10')



################################################################################################################################################

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

################################################################################################################################################

server <- function(input, output, session) {
  age_factor_table <- reactive({
    factors <- sapply(age_bands, function(band) {
      input_id <- paste0("factor_", gsub("[^0-9]", "_", band))
      val <- input[[input_id]]
      if(is.null(val) || is.na(val)) 1.0 else val
    })

    tibble(
      age_band = age_bands,
      age_factor = as.numeric(factors)
    )
  })
  
  

}