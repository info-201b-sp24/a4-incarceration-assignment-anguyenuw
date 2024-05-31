library(ggplot2)
library(dplyr)
library(vroom)
both_rates_counties_1990 <- vroom("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates-1990.csv?raw=true")
chart2_counties_discrepancies_race <- both_rates_counties_1990 %>%
  # no prison population stats for years 2017 and 2018
  filter(year == 2013) %>%
  # calculate discrepancies
  select(year, fips, state, county_name, total_pop,
         total_jail_pop_rate, total_prison_pop_rate,
         black_jail_pop_rate, black_prison_pop_rate) %>%
  mutate(Black = 
           (black_jail_pop_rate + black_prison_pop_rate)/
           (total_jail_pop_rate + total_prison_pop_rate),
         .keep = "unused") %>%
  filter(!is.na(Black))


chart2_black_discrep_vs_pop <- ggplot(chart2_counties_discrepancies_race, 
                             aes(x = total_pop, y = Black)) +
  geom_point(size = .9) + 
  labs(title="Discrepancy of Black incarceration, versus county population",
       subtitle = "All counties in the year 2013",
       x = "County population, log plot", 
       y = "Discrepancy of Black incarceration rates") +
  scale_x_continuous(trans='log10', labels = scales::comma) + 
  coord_cartesian(ylim = c(0, 30))

#chart2_black_discrep_vs_pop