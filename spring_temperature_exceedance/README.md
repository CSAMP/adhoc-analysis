# Spring Temperature Exceedance Analysis

Using [delta temperature data](https://github.com/CSAMP/delta-secchi-temperature-data) we examined the frequency of temperatures in March and April being >= 15°C and >= 17°C respectively for two strata (Yolo Bypass and Sacramento River). We used water year type data from [WaterYearType](https://github.com/FlowWest/waterYearType) and filtered to include only `location == "Sacramento Valley".`

We first summarized the data to daily averages and then calculated the following:

* water_year: water year (Oct-Sept)
* water_year_type: water year type (Critical, Dry, Below Normal, Above Normal, Wet)
* month: month
* stat: summary statistic 
  * prop_above_threshold: number of days over threshold divided by number of days with observations
  * n: number of observations within the water_year/month combination
  * min_temp: minimum temperature within the water_year/month combination
  * max_temp: maximum temperature within the water_year/month combination
  * mean_temp: mean temperature within the water_year/month combination
  * first_day_exceeded: the day that the temperature first exceeded the temperature threshold
* value: value

The summarized data is saved in two csv:
* `cache_slough_temp_exceeding_tresholds_long.csv` shows the results generated using the dataset `delta_water_quality_with_strata.csv`
* `cache_slough_exceedance_from_txt_temp_file_long.csv` shows the results generated using the dataset `temp.txt` 

Graphs showing temperature exceedence are stored in the figures directory. 
