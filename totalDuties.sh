#!/bin/sh
#Creating total number of duties

cut -d "," -f2 faculty_data.csv > facultyName.txt
sed '1d' facultyName.txt

while read -r line           
do
  #echo $line
  total=$(grep -w "$line" examinationRoaster.ods | wc  -l)
  echo "$line ," $total >> numberOfDuties.csv 
done <"facultyName.txt" 
