library(data.table)
library(tidyverse)
library(coronavirus)
library(countrycode)

load("C://Users//Ianní//Documents//Rdados//coronavirus.rda")

coronavirus <- coronavirus %>% setDT()

sapply(coronavirus, function(x) sum(is.na(x)))

coronavirus[,continent := countrycode(sourcevar = Country.Region,
                                      origin = "country.name",
                                      destination = "continent")]

# Ps : Diamond Princess is a ship, that's why the function can't find it's continent
coronavirus[Country.Region == "Brazil" & date == max(date)]

coronavirus[cases > 0,n_distinct(date),Country.Region][order(-V1)]

plot(coronavirus[Country.Region == "China"& type == "confirmed",cases])
plot(coronavirus[Country.Region == "China"& type == "death",cases])
hist(coronavirus[Country.Region == "China" & type == "death",cases]/coronavirus[Country.Region == "China" & type == "confirmed",cases])
summary(coronavirus[Country.Region == "China" & type == "death",cases]/coronavirus[Country.Region == "China" & type == "confirmed",cases])

plot(coronavirus[Country.Region == "China",])

#primeiros dias de infecção em cada país

start <- coronavirus[cases > 0,.(first_case =  min(date)),Country.Region][order(first_case)]

count_by_start <- start[,n_distinct(Country.Region),first_case]

count_by_start %>% 
  ggplot() +
  aes(x = first_case, y = V1) +
  geom_line()

# animated graph counting the growth of cases by continent given days


# how many days between the start and the day with more confirmed cases for : confirmed cases, deaths and deaths by confirmed cases respectively
# considering countries that have lived at least 30 days since the first confirmed case

countries_considered <- coronavirus[type == "confirmed" & cases > 0, max(date)-min(date),Country.Region][V1 >= 30,Country.Region]

confirmado <- coronavirus[type == "confirmed" & cases > 0 & Country.Region %in% countries_considered,
                          .(confirmed_cases = sum(cases)),
                          .(continent,Country.Region,date)]

Cinterval_til_spike <- confirmado[,.(start_day = min(date),
                                    spike_day = date[confirmed_cases == max(confirmed_cases)],
                                    flag_rm = min(confirmed_cases) == max(confirmed_cases),
                                    probably_still_growing = date[confirmed_cases == max(confirmed_cases)] == max(date)),
                                  .(continent,Country.Region)]

Cinterval_til_spike <- Cinterval_til_spike[flag_rm == F & probably_still_growing == F]

Cinterval_til_spike[,interval := as.numeric(spike_day - start_day)]

hist(Cinterval_til_spike$interval)
## ----------------     ----------------      ----------------      ----------------
mortes <- coronavirus[type == "death" & cases > 0 & Country.Region %in% countries_considered,
                      .(confirmed_cases = sum(cases)),
                      .(continent,Country.Region,date)]

Minterval_til_spike <- mortes[,.(start_day = min(date),
                                     spike_day = date[confirmed_cases == max(confirmed_cases)],
                                     flag_rm = min(confirmed_cases) == max(confirmed_cases),
                                     probably_still_growing = date[confirmed_cases == max(confirmed_cases)] == max(date)),
                                  .(continent,Country.Region)]

Minterval_til_spike <- Minterval_til_spike[flag_rm == F & probably_still_growing == F]

Minterval_til_spike[,interval := as.numeric(spike_day - start_day)]

hist(Minterval_til_spike$interval)
summary(Minterval_til_spike$interval)

## ----------------     ----------------      ----------------      ----------------

taxa <- coronavirus[cases > 0 & Country.Region %in% countries_considered,
                      .(confirmed_cases = sum(cases[type == "confirmed"]),
                        deaths = sum(cases[type == "death"])),
                      .(continent,Country.Region,date)]

taxa[,taxa := deaths/confirmed_cases]

Tinterval_til_spike <- taxa[,.(start_day = min(date),
                                 spike_day = date[taxa == max(taxa)],
                                 flag_rm = min(taxa) == max(taxa),
                                 probably_still_growing = date[taxa == max(taxa)] == max(date)),
                              .(continent,Country.Region)]

Tinterval_til_spike <- Tinterval_til_spike[flag_rm == F & probably_still_growing == F]

Tinterval_til_spike[,interval := as.numeric(spike_day - start_day)]

hist(Tinterval_til_spike$interval)
summary(Tinterval_til_spike$interval)

# predict China

infected <- coronavirus[Country.Region == "China" & type == "confirmed",sum(cases),date][order(date)]$V1
dias <- 30

modelo <- lm(log(infected[1:dias]) ~ c(1:dias))
summary(modelo)

exp(coef(modelo)[2])

# predicting infected for t days from the start
t <- 1:10
predito <- exp(coef(modelo)[1])*(exp(coef(modelo)[2])^t)

sum(predito)
sum(infected[1:10])
