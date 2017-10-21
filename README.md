# fhrs-osm-stats

[R](http://r-project.org) code for creating graphs to track the progress of using [Food Hygiene Rating Scheme data](http://ratings.food.gov.uk/open-data/en-GB) to improve [OpenStreetMap](http://openstreetmap.org). See the [wiki page](http://wiki.openstreetmap.org/wiki/UK_Food_Hygiene_Rating_System) for more details.

The code uses the [CSV files](http://gregrs.dev.openstreetmap.org/fhrs-stats/) created by the [FHRS/OSM comparison tool](http://gregrs.dev.openstreetmap.org/fhrs/). The `combine.sh` script reads any `stats-*.csv` files in the same directory as the script and its output can be redirected (e.g. `combine.sh > combined.csv`) to create a single CSV file to be read by R.
