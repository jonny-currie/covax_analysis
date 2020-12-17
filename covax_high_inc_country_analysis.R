#Analysis of high-income country contributions to COVAX, the GAVI COVID-19 vaccine fund

#comparing pledges to spending on domestic economic programmes (eg furlough), PPE, testing etc

#initial manual data scrape

#17th December 2020
#@jonnycurrie specialty registrar in public health and primary care

#Packages

library(tidyverse)
library(knitr)
library(kableExtra)

#Data on COVAX pledges
#from https://www.gavi.org/sites/default/files/covid/covax/COVAX-AMC-Donors-Table.pdf
#last updated 17.11.20

covax_df <- tibble(
  country = c("Canada", "France", "Germany", "Italy", "Japan", "USA", "UK"), 
  pledges = c(194, 120, 120, 103, 130, 0, 731)
) %>%
  mutate(pledges = pledges * 1000000)

#Data on domestic spending
#from https://www.imf.org/en/Topics/imf-and-covid19/Policy-Responses-to-COVID-19
#UK data from https://www.gov.uk/government/publications/spending-review-2020-documents/spending-review-2020#responding-to-covid-19-1

domestic_df <- tibble(
  country = c("Canada", "France", "Germany", "Italy", "Japan", "USA", "UK"), 
  spend = c(sum(20+249+85), 
            sum(135+327), 
            sum(156+130+218.5), 
            sum(25+55+25+5.4), 
            sum(117100+117100), 
            sum(44+483+293+268+25+510+349+150+49.9+8.3+192), 
            sum(280)), 
  ex_rate = c(0.79, rep(1.22, 3), 0.0097, 1, 1.36))

domestic_df <- domestic_df %>%
  mutate(spend = spend*1000000000) %>%
  mutate(spend = spend*ex_rate)

merge_df <- left_join(covax_df, domestic_df, by = "country")

merge_df %>%
  select(-ex_rate) %>%
  pivot_longer(pledges:spend, names_to = "type", values_to = "spending") %>%
  ggplot(aes(x=country, y=spending, fill=type)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  coord_flip() %>%
  facet_wrap(~type)

merge_df <- merge_df %>%
  select(-ex_rate)

merge_df %>%
  arrange(desc(country)) %>%
  mutate(pledges = pledges/100000000, 
         spend = spend/100000000) %>%
  rename("Country" = 1,"COVAX pledges (100 million)" = 2, "Domestic COVID-19 spending (100 million" = 3) %>%
  kable(caption = "Values are in $/USD") %>%
  kable_styling(bootstrap_options = "striped")
