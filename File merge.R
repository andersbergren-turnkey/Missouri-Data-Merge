# Load libraries
library(tidyverse)
library(readxl)
library(stringr)
library(magrittr)
library(splitstackshape)

# Import '15 data
SCAN.15.acctlvl <- read_xlsx("15FBSCANSFORSEASONS.xlsx")
RESALE.15 <- read_csv("15FBVIVIDDATA.csv")
DONATE.15 <- read_xlsx("15MIZZOUTSFTurnkey.xlsx")
SEASON.15.acctlvl <- read_xlsx("2015MizzouFootballTurnkey.xlsx")
TENURE.15 <- read_csv("FBYOPTURNKEY.csv")

# Convert Season ticket holder data to seat level
list_seats <- function(first,qty){
  glue::collapse(as.character(seq(from = first, to = first + qty - 1)), sep = ",")
}

SEASON.15.seatlvl <- SEASON.15.acctlvl %>%
  mutate(rn = row_number()) %>%
  group_by(rn) %>%
  mutate(seat_number = list_seats(`1st`, Qty)) %>%
  ungroup() %>%
  cSplit(splitCols = "seat_number", sep = ",", direction = "long")

# Convert scan data to seat level (fix corrupts first, then test model.matrix implementation)
SCAN.15.seatlvl <- SCAN.15.acctlvl %>%
  rename(seat_number = seat_attended) %>%
  cSplit(splitCols = "seat_number", sep = ",", direction = "long") %>%
  mutate(event_date = factor(event_date)) %>%
  bind_cols(model.matrix( ~ event_date, data=.))
  

matrix.test <- model.matrix( ~ factor(event_date), data=SCAN.15.acctlvl)
