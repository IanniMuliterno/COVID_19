# COVID_19
Ongoing Exploratory analysis on the available data about COVID-19

Insights so far :

 At Brazil Health Minister Mandetta said that the national health system may be at it's worst scenario by the second half of april and predictions suggest that about the same time we will also be at the "top of the curve" of confirmed cases. Having data about cases confirmed by day I did the following :
 
 ```r
 
 head(coronavirus)
 
#    Province.State Country.Region Lat Long       date cases      type continent
#1:                   Afghanistan  33   65 2020-01-22     0 confirmed      Asia
#2:                   Afghanistan  33   65 2020-01-23     0 confirmed      Asia
#3:                   Afghanistan  33   65 2020-01-24     0 confirmed      Asia
#4:                   Afghanistan  33   65 2020-01-25     0 confirmed      Asia
#5:                   Afghanistan  33   65 2020-01-26     0 confirmed      Asia
#6:                   Afghanistan  33   65 2020-01-27     0 confirmed      Asia

# considering countries that have lived at least 30 days since the first confirmed case

countries_considered <- coronavirus[type == "confirmed" & cases > 0, max(date)-min(date),Country.Region][V1 >= 30,Country.Region]

# how many days between the start and the day with more confirmed cases for : confirmed cases

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
```

With that I observed that data suggests that countries have the day with most new cases confirmed about 50 days after the first register, considering that Brazil registered the first case of infection by 26 of february, my simple analysis indicate the "top of the curve" will be at 16 of April, which corroborate with official predictions.
