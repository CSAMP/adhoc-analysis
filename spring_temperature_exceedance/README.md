# Spring Temperature Exceedance Analysis

Using [delta temperature data](https://github.com/CSAMP/delta-secchi-temperature-data) we examined the frequency of temperatures in March and April being >= 15°C and >= 18°C respectively for two strata (Yolo Bypass and Sacramento River). 

We first summarized the data to daily averages and then calculated the following:

* water_year: water year (Oct-Sept)
* water_year_type: water year type (Critical, Dry, Below Normal, Above Normal, Wet)
* month: month
* stat: summary statistic 
  * prop_above_threshold: number of days over threshold divided by number of days with observations
  * n: number of observations within the water_year/month combination
  * min_temp: minimum temperature within the water_year/month combination
  * max_temp: maximum temperature within the water_year/month combination
  * avg_temp: mean temperature within the water_year/month combination
  * first_day_exceeded: the day that the temperature first exceeded the temperature threshold
* value: value

The summarized data is saved in `cache_slough_temp_exceeding_tresholds_long.csv`.