#!/bin/bash

# get header row of first file with date header added to start
pattern="stats-*.csv"
files=( $pattern )
head -n 1 ${files[0]} | sed 's/^/date,/'

# get rest of files, adding date column and removing header rows
for filename in $pattern
do
	# get date part of filename
	datestring=`echo $filename | \
		sed 's/stats-\([0-9]*-[0-9]*-[0-9]*\).*.csv/\1/'`
	# add date as first column
	sed "s/^/$datestring,/" $filename | \
		# and delete header rows
		sed '/.*district_id,district_name.*/d'
done
