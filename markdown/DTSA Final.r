library(lubridate)
library(tidyverse)

# Load data, change from wide to long format and get basic summary
url_in <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
file_names <- c("time_series_covid19_confirmed_US.csv",
                "time_series_covid19_deaths_US.csv")
urls <- str_c(url_in, file_names)
US_cases <- read_csv(urls[1])
US_deaths <- read_csv(urls[2])
US_cases <- US_cases %>% pivot_longer(cols = -(UID:Combined_Key), 
                                      names_to = "date", values_to = "cases") %>% 
                                      select(Admin2:cases) %>% 
                                      mutate(date = mdy(date)) %>% 
                                      select(-c(Lat, Long_))
US_deaths <- US_deaths %>% pivot_longer(cols = -(UID:Population),
                                        names_to = "date", values_to = "deaths") %>% 
                                        select(Admin2:deaths) %>% mutate(date = mdy(date)) %>% 
                                        select(-c(Lat, Long_))
US <- US_cases %>% full_join(US_deaths)
summary(US)

# The data is segmented by county so we can modify the data to get totals by state and the whole USA
US_by_state <- US %>% 
               group_by(Province_State, Country_Region, date) %>%
               summarize(cases = sum(cases), deaths = sum(deaths), Population = sum(Population)) %>%
               mutate(deaths_per_mill = deaths * 1000000 / Population) %>%
               select(Province_State, Country_Region, date, cases, deaths, deaths_per_mill, Population) %>% ungroup()

US_totals <- US_by_state %>% 
             group_by(Country_Region, date) %>% 
             summarize(cases = sum(cases), deaths = sum(deaths), Population = sum(Population)) %>%
             mutate(deaths_per_mill = deaths * 1000000 / Population) %>% 
             select(Country_Region, date, cases, deaths, deaths_per_mill, Population) %>%
             ungroup()

US_totals %>% filter(cases > 0) %>%
    ggplot(aes(x = date, y = cases)) + 
    geom_line(aes(color = "cases")) + 
    geom_point(aes(color = "cases")) + 
    geom_line(aes(y = deaths, color = "deaths")) + 
    geom_point(aes(y = deaths, color = "deaths")) + 
    scale_y_log10() + 
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 90)) + 
    labs(title = "COVID19 in US", y = NULL)

US_by_state <- US %>% 
               group_by(Province_State, Country_Region, date) %>%
               summarize(cases = sum(cases), deaths = sum(deaths), Population = sum(Population)) %>%
               mutate(deaths_per_mill = deaths * 1000000 / Population) %>%
               select(Province_State, Country_Region, date, cases, deaths, deaths_per_mill, Population) %>% ungroup()

US_totals <- US_by_state %>% 
             group_by(Country_Region, date) %>% 
             summarize(cases = sum(cases), deaths = sum(deaths), Population = sum(Population)) %>%
             mutate(deaths_per_mill = deaths * 1000000 / Population) %>% 
             select(Country_Region, date, cases, deaths, deaths_per_mill, Population) %>%
             ungroup()

US_totals %>% filter(cases > 0) %>%
    ggplot(aes(x = date, y = cases)) + 
    geom_line(aes(color = "cases")) + 
    geom_point(aes(color = "cases")) + 
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 90)) + 
    labs(title = "COVID19 Cases in US", y = "Cases Count")

US_by_state <- US_by_state %>% 
    mutate(
        new_cases = cases - lag(cases), 
        new_deaths = deaths - lag(deaths), 
        cases_per_thou = cases * 1000 / Population, 
        deaths_per_thou = deaths * 1000 / Population
    ) %>% 
    filter(cases > 0, Population > 0)

mod <- lm(deaths_per_thou ~ cases_per_thou, data = US_by_state)
summary(mod)

US_w_pred <- US_by_state %>% mutate(pred = predict(mod))
US_w_pred %>% ggplot() + 
    geom_point(aes(x = cases_per_thou, y = deaths_per_thou), color = "blue") + 
    geom_point(aes(x = cases_per_thou, y = pred), color = "red")
