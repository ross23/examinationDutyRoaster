#!/bin/sh
#Separate Faculty into csv

cat faculty_data.csv |grep -w "Lecturer" >> a.txt
sed '/Senior Lecturer/d' a.txt >> lecturer.csv
sed -i '1s/^/,Name,Designation,Joining Date\n/' lecturer.csv

cat faculty_data.csv |grep -w "Senior Lecturer" >> senior_lecturer.csv
sed -i '1s/^/,Name,Designation,Joining Date\n/' senior_lecturer.csv

cat faculty_data.csv |grep -w "Assistant Professor" >> assistant_professor.csv
sed -i '1s/^/,Name,Designation,Joining Date\n/' assistant_professor.csv

cat faculty_data.csv |grep -w "Associate Professor" >> associate_professor.csv
sed -i '1s/^/,Name,Designation,Joining Date\n/' associate_professor.csv

cat faculty_data.csv |grep -w "Professor" >> b.txt
sed '/Assistant Professor/d' b.txt >> c.txt
sed '/Associate Professor/d' c.txt >> professor.csv
sed -i '1s/^/,Name,Designation,Joining Date\n/' professor.csv

cat faculty_data.csv |grep -w "Adviser" >> adviser.csv 
sed -i '1s/^/,Name,Designation,Joining Date\n/' adviser.csv

R -f roasterScheduling.R

rm -rf lecturer.csv senior_lecturer.csv assistant_professor.csv associate_professor.csv associate_professor.csv professor.csv adviser.csv a.txt b.txt c.txt

#sh totalDuties.sh

rm -rf facultyName.txt

#data <- read.csv("faculty_data.csv", header=T) 
#date <- as.Date(data$Joining_Date, "%Y-%m-%d")   
# head(data)
#data$Joining_Date <- strftime(data$Joining_Date, "%m/%d/%Y")
#write.csv(data,"faculty.csv")

