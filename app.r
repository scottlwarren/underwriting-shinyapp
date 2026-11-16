library(shiny)
library(ggplot2)

policy_bdx <- read.csv('./data/policy_bordereau.csv')
claims_bdx <- read.csv('./data/claims_bordereau.csv')
uw_spec <- read.csv('./data/underwriting_spec.csv')

head(uw_spec)

 ### So starting out, we need to look at getting these claims and policies in a position where we can get a simple triangulation. Policy has underwriting year and inception date, claims has loss date. However because we don't have the UW year for the claim, we will need to tie that back to the policy number and likely will need a join on the policy name.

library(duckdb)
con <- dbConnect(duckdb::duckdb())
duckdb::dbWriteTable(con, 'claims_bdx', claims_bdx)
duckdb::dbWriteTable(con, 'policy_bdx', policy_bdx)
dbGetQuery(con, paste0("SELECT * FROM claims_bdx LIMIT 10"))
