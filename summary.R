library(dplyr)
library(vroom)
both_rates_counties_1990 <- vroom("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates-1990.csv?raw=true")
counties_discrepancies_race <- both_rates_counties_1990 %>%
  # calculate discrepancies
  select(year, fips, state, county_name,
         total_jail_pop_rate, total_prison_pop_rate, 
         aapi_jail_pop_rate, aapi_prison_pop_rate,
         black_jail_pop_rate, black_prison_pop_rate,
         latinx_jail_pop_rate, latinx_prison_pop_rate,
         native_jail_pop_rate, native_prison_pop_rate,
         white_jail_pop_rate, white_prison_pop_rate) %>%
  mutate(AsianAmericanPacificIslander = 
           (aapi_jail_pop_rate + aapi_prison_pop_rate)/
           (total_jail_pop_rate + total_prison_pop_rate),
         Black = 
           (black_jail_pop_rate + black_prison_pop_rate)/
           (total_jail_pop_rate + total_prison_pop_rate),
         Latinx = 
           (latinx_jail_pop_rate + latinx_prison_pop_rate)/
           (total_jail_pop_rate + total_prison_pop_rate),
         NativeAmerican = 
           (native_jail_pop_rate + native_prison_pop_rate)/
           (total_jail_pop_rate + total_prison_pop_rate),
         White = 
           (white_jail_pop_rate + white_prison_pop_rate)/
           (total_jail_pop_rate + total_prison_pop_rate),
         .keep = "unused")

mean_aapi_discrepancy <- counties_discrepancies_race %>%
  filter(!is.na(AsianAmericanPacificIslander)) %>%
  pull(AsianAmericanPacificIslander) %>%
  mean()

mean_black_discrepancy <- counties_discrepancies_race %>%
  filter(!is.na(Black)) %>%
  pull(Black) %>%
  mean()

mean_latinx_discrepancy <- counties_discrepancies_race %>%
  filter(!is.na(Latinx)) %>%
  pull(Latinx) %>%
  mean()

mean_native_discrepancy <- counties_discrepancies_race %>%
  filter(!is.na(NativeAmerican)) %>%
  pull(NativeAmerican) %>%
  mean()

mean_white_discrepancy <- counties_discrepancies_race %>%
  filter(!is.na(White)) %>%
  pull(White) %>%
  mean()
