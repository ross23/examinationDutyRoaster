#Faculty Roaster Generator a.k.a Exam Scheduling Generator of the Faculty

##Requirement
```
i) Operating System:

Open Source or Linux Based Operating System [Recommended]
```

```
ii) Sensitive CSV and Header/Title: 

faculty_data.csv

Note: Faculty List (Name, Designation, Joining Date)
```

```
iii) Sensitive CSV and Header/Title: 

room_capacity.csv

Note: Room List (Room, Capacity)
```
###**Fedora Project** 

> dnf install -y R 

or 

> yum install -y R 


R packages:

> install.packages("plyr")


## INPUT
Enter INPUT in Line no. from ** 41 to 55** in the file roasterScheduling.R

##Command
> sh generate-roaster.sh

```
[Note: run several times to get the expected results]
```

##Output
> examinationRoaster.odt

> totalDuties.ods

#Request
If you want to contribute on this project, please send me the patch, i'll try to upload this if seems gorgeous!

#Authors:
* Rashadul Islam 
  rashadul.cse@gmail.com