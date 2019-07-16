## code to prepare `hh_matrices` dataset goes here
library(readr)
hh_wide <- read_csv("data-raw/helmeted_honeyeater_census.csv")
hh_wide <- hh_wide %>% mutate(
  S_1991 = lead(`1992`) / `1991`,
  S_1992 = lead(`1993`) / `1992`,
  S_1993 = lead(`1994`) / `1993`
)
hh_fecund <- hh_wide %>% # start with census data
  gather(key = Year, value = N, 2:5) %>% # "stack"
  mutate(Year = as.numeric(Year)) %>% # force year to be a number
  group_by(Year) %>% # get one row per year
  summarize(
    numerator = first(N), # this is the number of new recruits in that year
    denominator = sum(N[-1])
  ) %>% # this is the number of adults in that year
  mutate(F = lead(numerator) / denominator) # per capita Fertility is next years babies divided by this years adults
hh_surv <- hh_wide %>%
  select(-(2:5)) %>% # remove census columns
  gather(key = S_Year, value = S, starts_with("S_")) %>% # stack it up
  separate(S_Year, into = c("toss", "Year")) # throw away the "S_" part of the name
A_91 <- matrix(0, nrow = 10, ncol = 10)
# fecundity goes in top row
# post-breeding matrix, but already includes survival
A_91[1, 2:10] <- hh_fecund$F[1] # first row
# getting the survival "diagonal" a bit more involved
A_91[cbind(2:10, 1:9)] <- hh_surv$S[1:9]
A_92 <- matrix(0, nrow = 10, ncol = 10)
# fecundity goes in top row
# post-breeding matrix, but already includes survival
A_92[1, 2:10] <- hh_fecund$F[2] # second row
# getting the survival "diagonal" a bit more involved
A_92[cbind(2:10, 1:9)] <- hh_surv$S[11:19]
A_93 <- matrix(0, nrow = 10, ncol = 10)
# fecundity goes in top row
# post-breeding matrix, but already includes survival
A_93[1, 2:10] <- hh_fecund$F[3] # third row
# getting the survival "diagonal" a bit more involved
A_93[cbind(2:10, 1:9)] <- hh_surv$S[21:29]
hh_A <- list(
  A_91 = A_91,
  A_92 = A_92,
  A_93 = A_93
)
hh_N <- list(
  N1991 = hh_wide$`1991`,
  N1992 = hh_wide$`1992`,
  N1993 = hh_wide$`1993`,
  N1994 = hh_wide$`1994`
)
usethis::use_data(hh_A, hh_N)
