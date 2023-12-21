#!/bin/bash

for file in *.csv; do
	filename=$(basename "$file" .NS.csv)
	newfile="${filename}.csv"
	mv "$file" "$newfile"
done
