library(tidyverse)
library(tsiR)
theme_set(theme_bw())

# read in England & Wales measles data
url_loc <- paste0("https://raw.githubusercontent.com/WyattGMadden/",
                  "intro_to_ml_for_id_emory/main/data/england_and_wales_measles/measles.csv")
measles <- read_csv(url_loc)

head(measles)
str(measles)
summary(measles)

measles |>
    ggplot(aes(x = time, y = cases, color = city)) +
    geom_line()

measles |>
    mutate(city_group = cut(pop, breaks = quantile(pop), include.lowest = T)) |>
    ggplot(aes(x = time, y = cases)) +
    geom_line(alpha = 0.3) +
    facet_wrap(~city_group, scale = "free_y")


measles |>
    mutate(cases_normalized = cases / pop) |>
    ggplot(aes(x = time, y = cases_normalized, color = city)) +
    geom_line(alpha = 0.2)


london_measles <- measles |>
    filter(city == "London")

london_measles |>
    ggplot(aes(x = time, y = cases)) +
    geom_line()

# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0185528
# have students investigate arguments/other parameter options

LondonRes <- runtsir(data = london_measles, 
                     IP = 2,
                     xreg = 'cumcases',
                     regtype = 'gaussian',
                     alpha = NULL,
                     sbar = NULL,
                     family = 'gaussian',
                     link = 'identity',
                     method = 'negbin',
                     nsim = 100)

plotres(LondonRes)




