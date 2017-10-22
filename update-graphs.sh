#!/bin/bash

# combine CSV files
cd ~/public_html/fhrs-stats || exit 1
~/fhrs-osm-stats/combine.sh > ~/fhrs-osm-stats/combined.csv || exit 1

# run R scripts
cd ~/fhrs-osm-stats || exit 1
Rscript -e "rmarkdown::render('summary-graphs.Rmd')" || exit 1
rm district-graphs/*.png
Rscript district-graphs.R || exit 1

# copy summary stats HTML file to public directory
cp ~/fhrs-osm-stats/summary-graphs.html ~/public_html/fhrs-stats/ || exit 1

# copy district graphs to public directory
if [ -d ~/public_html/fhrs-stats/district-graphs ]; then
	rm -r ~/public_html/fhrs-stats/district-graphs || exit 1
fi
mkdir ~/public_html/fhrs-stats/district-graphs || exit 1
cp ~/fhrs-osm-stats/district-graphs/*.png \
	~/public_html/fhrs-stats/district-graphs/ || exit 1
