library(ggplot2)
library(dplyr)
library(vroom)
wa1990 <- vroom("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990-WA.csv")
king_discrepancy_by_race <- wa1990 %>%
  # no prison population stats for years 2017 and 2018
  filter(county_name == "King County", year < 2017) %>%
  # calculate discrepancies
  select(year,
         total_jail_pop_rate, total_prison_pop_rate, 
         aapi_jail_pop_rate, aapi_prison_pop_rate,
         black_jail_pop_rate, black_prison_pop_rate,
         latinx_jail_pop_rate, latinx_prison_pop_rate,
         native_jail_pop_rate, native_prison_pop_rate,
         white_jail_pop_rate, white_prison_pop_rate) %>%
  mutate("Asian American / Pacific Islander" = 
           (aapi_jail_pop_rate + aapi_prison_pop_rate)/
           (total_jail_pop_rate + total_prison_pop_rate),
         Black = 
           (black_jail_pop_rate + black_prison_pop_rate)/
           (total_jail_pop_rate + total_prison_pop_rate),
         Latinx = 
           (latinx_jail_pop_rate + latinx_prison_pop_rate)/
           (total_jail_pop_rate + total_prison_pop_rate),
         "Native American" = 
           (native_jail_pop_rate + native_prison_pop_rate)/
           (total_jail_pop_rate + total_prison_pop_rate),
         White = 
           (white_jail_pop_rate + white_prison_pop_rate)/
           (total_jail_pop_rate + total_prison_pop_rate),
         .keep = "unused") %>%
  # convert to long form to use in ggplot
  pivot_longer(c("Asian American / Pacific Islander", Black, Latinx, "Native American", White),
    cols_vary = "slowest",
    names_to = "Race",
    values_to = "Discrepancy"
  )

chart1_discrepancy <- ggplot(king_discrepancy_by_race, 
                             aes(x = year, 
                                 y = Discrepancy, 
                                 color = Race)) +
  geom_line(lwd = 1.1) + 
  labs(title="Ratio of incarceration rate per racial group versus general population",
       subtitle = "King County, WA, 1990 - 2016",
       x = "Year", 
       y = "Discrepancy from general population") + 
  guides(color = guide_legend(title = "Racial Group")) +
  coord_cartesian(ylim = c(0, 8))

#chart1_discrepancy