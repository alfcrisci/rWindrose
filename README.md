rWindrose
========

An R package for creating rose plots from wind data that show both direction and speed. Initialilly based on code provided by Andy Clifton(http://stackoverflow.com/a/17266781/393354)  and improved Tom Hopper (https://github.com/tomhopper). Bugs related to ggplot2 were fixed following some suggestions in stackoverflow (https://stackoverflow.com/questions/17266780/wind-rose-with-ggplot-r)

Install with

```
library(devtools)
install_github("alfcrisci/windrose")
```

To create a wind rose plot before a windrose object must to be created than a plot could be provided:

```
data(wind_data)
wind_rose <- windrose(wind_data, spd = Wind_Speed_meter_per_second, dir = Wind_Direction_deg)
plot(wind_rose)
```

