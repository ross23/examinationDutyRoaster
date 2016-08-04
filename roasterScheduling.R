library(plyr)

timeRange <- NULL
holiday <- NULL

#Collecting the faculty informations
lecturer <- read.csv("lecturer.csv", header=T) 
senior_lecturer <- read.csv("senior_lecturer.csv", header=T) 
assistant_professor <- read.csv("assistant_professor.csv", header=T) 
associate_professor <- read.csv("associate_professor.csv", header=T) 
professor <- read.csv("professor.csv", header=T) 
adviser <- read.csv("adviser.csv", header=T) 

#Collecting room and capacity informations
room_capacity <- read.csv("room_capacity.csv", header=T) 
    
#Sorting by date
sorted_lecturer1 <- arrange(lecturer, desc(lecturer$Joining.Date))
sorted_lecturer1$Sequence<-seq.int(nrow(sorted_lecturer1))

sorted_senior_lecturer1 <- arrange(senior_lecturer, desc(senior_lecturer$Joining.Date))
sorted_senior_lecturer1$Sequence<-seq.int(nrow(sorted_senior_lecturer1))

sorted_assistant_professor1 <-  arrange(assistant_professor, desc(assistant_professor$Joining.Date))
sorted_assistant_professor1$Sequence<-seq.int(nrow(sorted_assistant_professor1))

sorted_associate_professor1 <- arrange(associate_professor, desc(associate_professor$Joining.Date))
sorted_associate_professor1$Sequence<-seq.int(nrow(sorted_associate_professor1))

sorted_professor1 <- arrange(professor, desc(professor$Joining.Date))
sorted_professor1$Sequence<-seq.int(nrow(sorted_professor1))

sorted_adviser1 <- arrange(adviser, desc(adviser$Joining.Date))
sorted_adviser1$Sequence<-seq.int(nrow(sorted_adviser1))


day=0
shift=0
room=0

############################### Enter Total Examination Days and Number of Shifts in Each Day #################################
totalDay=2
totalShift=1

###############################Calculation for total duties#############################
totalDuties<-totalDay * totalShift
############################### Enter Shifts Time Range #################################
timeRange$time1 <- "10:00AM -01:00PM"
timeRange$time2 <- "02:00PM -05:00PM"

############################### Enter Start Date Here #################################
startDate="19/7/2016"

############################### Enter Holidays Date Here #################################
holiday$date1="20/7/2016"
holiday$date2="22/7/2016"
###################################################################################
endDay=(length(startDate)+length(holiday$date1)+ length(holiday$date2) +totalDay) -1

sorted_lecturer1$totalDuties <- 0
sorted_senior_lecturer1$totalDuties <- 0
sorted_assistant_professor1$totalDuties <- 0
sorted_associate_professor1$totalDuties <- 0
sorted_professor1$totalDuties <- 0
sorted_adviser1$totalDuties <- 0

	  
#loop to the day
for (day in 1:endDay){
  #print(paste("Day", i))

  if(as.Date(startDate,format = "%d/%m/%Y") != as.Date(holiday$date1,format = "%d/%m/%Y") &  as.Date(startDate,format = "%d/%m/%Y") != as.Date(holiday$date2,format = "%d/%m/%Y")) 
  {
      #loop to the shift
      for (shift in 1:totalShift){
	  #print(paste("Shift", shift))
	
	  sorted_lecturer <- sorted_lecturer1
	  sorted_senior_lecturer<-sorted_senior_lecturer1
	  sorted_assistant_professor<-sorted_assistant_professor1
	  sorted_associate_professor<-sorted_associate_professor1
	  sorted_professor<-sorted_professor1
	  sorted_adviser<-sorted_adviser1

	  
	  ##########################################
	  # Finding Chief Invigilator of the shift
	  ##########################################
	  invigilation_pool11 <- NULL
	  invigilation_pool12 <- NULL
			      
			    
	  #Selecting professor
	  randomSequence11 <- sample(1:nrow(sorted_professor), 1, replace=F)
	  invigilation_pool11<- sorted_professor[randomSequence11,]
	  sorted_professor <- sorted_professor[-(randomSequence11),]
	  #increase the total number of duties of lecturer
	  sorted_professor[randomSequence11,5] <- sorted_professor[randomSequence11,5] + 1

	  #Selecting adviser
	  randomSequence12 <- sample(1:nrow(sorted_adviser), 1, replace=F)
	  invigilation_pool12<- sorted_adviser[randomSequence12,]
	  sorted_adviser <- sorted_adviser[-(randomSequence12),]
	  #increase the total number of duties of lecturer
	  sorted_adviser[randomSequence12,5] <- sorted_adviser[randomSequence12,5] + 1

	  chiefInvigilator <- NULL
	  chiefInvigilator <- rbind(chiefInvigilator, invigilation_pool11)
	  chiefInvigilator <- rbind(chiefInvigilator, invigilation_pool12)
			    
	      
	#loop for rooms
	for (room in 1:nrow(room_capacity)){
	
	  
	 #if((sorted_lecturer$totalduties <= totalDuties) & (sorted_senior_lecturer$totalduties = totalDuties) & (sorted_assistant_professor$totalduties <= totalDuties) & (sorted_associate_professor$totalduties <= totalDuties) ){
	  
		capacity = transform(room_capacity, total = room_capacity$Capacity[room] %/% 20)
	      
		if(capacity$total >= 4){
	  
		    #capture.output(print("*********************************************************************************"),file="examinationRoaster.odt",append = TRUE)
		    #capture.output(print(paste("Day:", format(as.Date(startDate,format = "%d/%m/%Y"), "%a %b %d"), "## Shift:", ifelse(shift==1,timeRange$time1,timeRange$time2),"## Room:", room_capacity$Room[room],"## Room Capacity:", room_capacity$Capacity[room])),file="examinationRoaster.odt",append = TRUE)
		    #capture.output(print("*********************************************************************************"),file="examinationRoaster.odt",append = TRUE)
		  

		    info=print(paste("Day:", format(as.Date(startDate,format = "%d/%m/%Y"), "%a %b %d"), "## Shift:", ifelse(shift==1,timeRange$time1,timeRange$time2),"## Room:", room_capacity$Room[room],"## Room Capacity:", room_capacity$Capacity[room]))	    
		    
		    day_date <- NULL
		    day_date <- as.data.frame(info)

		    write.table(day_date$info, file="examinationRoaster.ods", append=T, row.names=F, col.names=F)

		      invigilation_pool1 <- NULL
		      invigilation_pool6 <- NULL
		      invigilation_pool2 <- NULL
		      invigilation_pool3 <- NULL
		      invigilation_pool4 <- NULL
		      invigilation_pool5 <- NULL
		      
		      #Selecting lecturer
		      randomSequence1 <- sample(1:nrow(sorted_lecturer), 1, replace=F)
		      
		      if((sorted_lecturer[randomSequence1,6]) < totalDuties){
			invigilation_pool1<- sorted_lecturer[randomSequence1,]
			sorted_lecturer <- sorted_lecturer[-(randomSequence1),]
			#increase the total number of duties of lecturer
			sorted_lecturer[randomSequence1,6] <- sorted_lecturer[randomSequence1,6] + 1
			sorted_lecturer1[randomSequence1,6] <- sorted_lecturer1[randomSequence1,6] + 1
		      }
		      #else{
			#sorted_lecturer1 <- sorted_lecturer1[-(randomSequence1),]
		      #}
		      #increase the total number of duties of lecturer
		      #sorted_lecturer[randomSequence1,5] <- sorted_lecturer[randomSequence1,5] + 1

      		      #Selecting lecturer
		      randomSequence2 <- sample(1:nrow(sorted_lecturer), 1, replace=F)
		      
		      if((sorted_lecturer[randomSequence2,6]) < totalDuties){
			invigilation_pool2<- sorted_lecturer[randomSequence2,]
			sorted_lecturer <- sorted_lecturer[-(randomSequence2),]
			#increase the total number of duties of lecturer
			sorted_lecturer[randomSequence2,6] <- sorted_lecturer[randomSequence2,6] + 1
			sorted_lecturer1[randomSequence2,6] <- sorted_lecturer1[randomSequence2,6] + 1
		      }
		      #else{
			#sorted_lecturer1 <- sorted_lecturer1[-(randomSequence2),]
		      #}

      		      #Selecting lecturer
		      randomSequence3 <- sample(1:nrow(sorted_lecturer), 1, replace=F)

		      if((sorted_lecturer[randomSequence3,6]) < totalDuties){
			invigilation_pool3<- sorted_lecturer[randomSequence3,]
			sorted_lecturer <- sorted_lecturer[-(randomSequence3),]
			#increase the total number of duties of lecturer
			sorted_lecturer[randomSequence3,6] <- sorted_lecturer[randomSequence3,6] + 1
			sorted_lecturer1[randomSequence3,6] <- sorted_lecturer1[randomSequence3,6] + 1
		      }
		      #else{
			#sorted_lecturer1 <- sorted_lecturer1[-(randomSequence3),]
		      #}

		      #Selecting senior lecturer
		      randomSequence4 <- sample(1:nrow(sorted_senior_lecturer), 1, replace=F)

		      if((sorted_senior_lecturer[randomSequence4,6]) < totalDuties){
			invigilation_pool4<- sorted_senior_lecturer[randomSequence4,]
			sorted_senior_lecturer <- sorted_senior_lecturer[-(randomSequence4),]
			#increase the total number of duties of lecturer
			sorted_senior_lecturer[randomSequence4,6] <- sorted_senior_lecturer[randomSequence4,6] + 1
			sorted_senior_lecturer1[randomSequence4,6] <- sorted_senior_lecturer1[randomSequence4,6] + 1
		      }
		      #else{
			#sorted_senior_lecturer1 <- sorted_senior_lecturer1[-(randomSequence4),]
		      #}
		      
		      #Selecting assistant professor
		      randomSequence5 <- sample(1:nrow(sorted_assistant_professor), 1, replace=F)
		      
		      if((sorted_assistant_professor[randomSequence5,6]) < totalDuties){
			invigilation_pool5<- sorted_assistant_professor[randomSequence5,]
			sorted_assistant_professor<- sorted_assistant_professor[-(randomSequence5),]
			#increase the total number of duties of lecturer
			sorted_assistant_professor[randomSequence5,6] <- sorted_assistant_professor[randomSequence5,6] + 1
			sorted_assistant_professor1[randomSequence5,6] <- sorted_assistant_professor1[randomSequence5,6] + 1
		      }
		      #else{
			#sorted_assistant_professor1 <- sorted_assistant_professor1[-(randomSequence5),]
		      #}
		      
		      #Selecting associate professor
		      randomSequence6 <- sample(1:nrow(sorted_associate_professor), 1, replace=F)
		      
		      if((sorted_associate_professor[randomSequence6,6]) < totalDuties){
			invigilation_pool6<- sorted_associate_professor[randomSequence6,]
			sorted_associate_professor <- sorted_associate_professor[-(randomSequence6),]
			#increase the total number of duties of lecturer
			sorted_associate_professor[randomSequence6,6] <- sorted_associate_professor[randomSequence6,6] + 1
			sorted_associate_professor1[randomSequence6,6] <- sorted_associate_professor1[randomSequence6,6] + 1
		      }
		      #else{
			#sorted_associate_professor1 <- sorted_associate_professor1[-(randomSequence6),]
		      #}
		      
		      invigilationDuty <- NULL
		      invigilationDuty <- rbind(invigilationDuty, invigilation_pool1)
		      invigilationDuty <- rbind(invigilationDuty, invigilation_pool2)
		      invigilationDuty <- rbind(invigilationDuty, invigilation_pool3)
		      invigilationDuty <- rbind(invigilationDuty, invigilation_pool4)
		      invigilationDuty <- rbind(invigilationDuty, invigilation_pool5)
		      invigilationDuty <- rbind(invigilationDuty, invigilation_pool6)


		      ##########################################
		      # Finding Reserve
		      ##########################################
		      invigilation_pool7 <- NULL
		      invigilation_pool8 <- NULL
		      invigilation_pool9 <- NULL
		      
		      #Selecting lecturer
		      randomSequence7 <- sample(1:nrow(sorted_lecturer), 1, replace=F)
		      invigilation_pool7<- sorted_lecturer[randomSequence7,]
		      #increase the total number of duties of lecturer
		      sorted_lecturer[randomSequence7,5] <- sorted_lecturer[randomSequence7,5] + 1

		      #Selecting senior lecturer
		      randomSequence8 <- sample(1:nrow(sorted_senior_lecturer), 1, replace=F)
		      invigilation_pool8<- sorted_senior_lecturer[randomSequence8,]
     		      #increase the total number of duties of lecturer
		      #sorted_senior_lecturer[randomSequence8,5] <- sorted_senior_lecturer[randomSequence8,5] + 1

		      #Selecting assistant professor
		      randomSequence9 <- sample(1:nrow(sorted_assistant_professor), 1, replace=F)
		      invigilation_pool9<- sorted_assistant_professor[randomSequence9,]
      		      #increase the total number of duties of lecturer
		      #sorted_assistant_professor[randomSequence9,5] <- sorted_assistant_professor[randomSequence9,5] + 1

		      reserveDuty <- NULL
		      reserveDuty <- rbind(reserveDuty, invigilation_pool7)
		      reserveDuty <- rbind(reserveDuty, invigilation_pool8)
		      reserveDuty <- rbind(reserveDuty, invigilation_pool9)
		      
		      ##########################################
		      #Printing all the data for the shift
		      ##########################################
		      
		      INVIGILATORS <- paste(invigilationDuty$Designation,invigilationDuty$Name,Sep="")
		      RESERVE <- paste(reserveDuty$Designation, reserveDuty$Name,Sep="")
		      CHIEF <- paste(chiefInvigilator$Designation,chiefInvigilator$Name,Sep="")
		      
		      a<- NULL
		      b<- NULL
		      c<- NULL
		      
		      a<- as.data.frame(INVIGILATORS)
		      b<- as.data.frame(RESERVE)
		      c<- as.data.frame(CHIEF)
		      
		      a$id <- rownames(a)
		      b$id <- rownames(b)
		      c$id <- rownames(c)
		      
		      aa<- merge(a,b,by=c("id"),by.x="id",by.y="id",all.x=T,all.y=T)
		      aa<- merge(aa,c,by=c("id"),by.x="id",by.y="id",all.x=T,all.y=T)
		      
		      #invigilationsFaculty <- as.data.frame(cbind(INVIGILATORS,RESERVE,CHIEF))
		      
		      #invigilationsFaculty[is.na(invigilationsFaculty)] <- ""
		      
		      
	      #capture.output(print(invigilationsFaculty,right=F),file="examinationRoaster.odt",append = TRUE)
		#capture.output(print("============================================================="),file="examinationRoaster.odt",append = TRUE)
	      #capture.output(print(paste("")),file="examinationRoaster.odt",append = TRUE)
	      #capture.output(print(paste("")),file="examinationRoaster.odt",append = TRUE)
	      blankLine <- NULL
	      write.table(aa, file="examinationRoaster.ods", append=T, row.names=F, col.names=T,  sep=",")
	      write.table(blankLine, file="examinationRoaster.ods", append=T, row.names=T, col.names=T,  sep=",")
	    }
	    
	    else if(capacity$total >= 3){
	  
		    #capture.output(print("*********************************************************************************"),file="examinationRoaster.odt",append = TRUE)
		    #capture.output(print(paste("Day:", format(as.Date(startDate,format = "%d/%m/%Y"), "%a %b %d"), "## Shift:", ifelse(shift==1,timeRange$time1,timeRange$time2),"## Room:", room_capacity$Room[room],"## Room Capacity:", room_capacity$Capacity[room])),file="examinationRoaster.odt",append = TRUE)
		    #capture.output(print("*********************************************************************************"),file="examinationRoaster.odt",append = TRUE)


		    info=print(paste("Day:", format(as.Date(startDate,format = "%d/%m/%Y"), "%a %b %d"), "## Shift:", ifelse(shift==1,timeRange$time1,timeRange$time2),"## Room:", room_capacity$Room[room],"## Room Capacity:", room_capacity$Capacity[room]))	    
		    
		    day_date <- NULL
		    day_date <- as.data.frame(info)

		    write.table(day_date$info, file="examinationRoaster.ods", append=T, row.names=F, col.names=F)

		      invigilation_pool1 <- NULL
		      invigilation_pool2 <- NULL
		      invigilation_pool3 <- NULL
		      invigilation_pool4 <- NULL
		      

      		      #Selecting lecturer
		      randomSequence1 <- sample(1:nrow(sorted_lecturer), 1, replace=F)
		      
		      if((sorted_lecturer[randomSequence1,6]) < totalDuties){
			invigilation_pool1<- sorted_lecturer[randomSequence1,]
			sorted_lecturer <- sorted_lecturer[-(randomSequence1),]
			#increase the total number of duties of lecturer
			sorted_lecturer[randomSequence1,6] <- sorted_lecturer[randomSequence1,6] + 1
			sorted_lecturer1[randomSequence1,6] <- sorted_lecturer1[randomSequence1,6] + 1
		      }
		      #else{
			#sorted_lecturer1 <- sorted_lecturer1[-(randomSequence1),]
		      #}

      		      #Selecting lecturer
		      randomSequence2 <- sample(1:nrow(sorted_lecturer), 1, replace=F)

		      if((sorted_lecturer[randomSequence2,6]) < totalDuties){
			invigilation_pool2<- sorted_lecturer[randomSequence2,]
			sorted_lecturer <- sorted_lecturer[-(randomSequence2),]
			#increase the total number of duties of lecturer
			sorted_lecturer[randomSequence2,6] <- sorted_lecturer[randomSequence2,6] + 1
			sorted_lecturer1[randomSequence2,6] <- sorted_lecturer1[randomSequence2,6] + 1
		      }
		      #else{
			#sorted_lecturer1 <- sorted_lecturer1[-(randomSequence2),]
		      #}

		      #Selecting senior lecturer
		      randomSequence3 <- sample(1:nrow(sorted_senior_lecturer), 1, replace=F)

		      if((sorted_senior_lecturer[randomSequence3,6]) < totalDuties){
			invigilation_pool3<- sorted_senior_lecturer[randomSequence3,]
			sorted_senior_lecturer <- sorted_senior_lecturer[-(randomSequence3),]
			#increase the total number of duties of lecturer
			sorted_senior_lecturer[randomSequence3,6] <- sorted_senior_lecturer[randomSequence3,6] + 1
			sorted_senior_lecturer1[randomSequence3,6] <- sorted_senior_lecturer1[randomSequence3,6] + 1

			}
		      #else{
			#sorted_senior_lecturer1 <- sorted_senior_lecturer1[-(randomSequence3),]
		      #}
		      
		      #Selecting assistant professor
		      randomSequence4 <- sample(1:nrow(sorted_assistant_professor), 1, replace=F)
		      
		      if((sorted_assistant_professor[randomSequence4,6]) < totalDuties){
			invigilation_pool4<- sorted_assistant_professor[randomSequence4,]
			sorted_assistant_professor<- sorted_assistant_professor[-(randomSequence4),]
			#increase the total number of duties of lecturer
			sorted_assistant_professor[randomSequence4,6] <- sorted_assistant_professor[randomSequence4,6] + 1
			sorted_assistant_professor1[randomSequence4,6] <- sorted_assistant_professor1[randomSequence4,6] + 1

		      }
		      #else{
			#sorted_assistant_professor1 <- sorted_assistant_professor1[-(randomSequence4),]
		      #}
		      
		      invigilationDuty <- NULL
		      invigilationDuty <- rbind(invigilationDuty, invigilation_pool1)
		      invigilationDuty <- rbind(invigilationDuty, invigilation_pool2)
		      invigilationDuty <- rbind(invigilationDuty, invigilation_pool3)
		      invigilationDuty <- rbind(invigilationDuty, invigilation_pool4)


		      ##########################################
		      # Finding Reserve
		      ##########################################
		      invigilation_pool7 <- NULL
		      invigilation_pool8 <- NULL
		      invigilation_pool9 <- NULL
		      
		      #Selecting lecturer
		      randomSequence7 <- sample(1:nrow(sorted_lecturer), 1, replace=F)
		      invigilation_pool7<- sorted_lecturer[randomSequence7,]
		      #increase the total number of duties of lecturer
		      #sorted_lecturer[randomSequence1,5] <- sorted_lecturer[randomSequence1,5] + 1
		      
		      #Selecting senior lecturer
		      randomSequence8 <- sample(1:nrow(sorted_senior_lecturer), 1, replace=F)
		      invigilation_pool8<- sorted_senior_lecturer[randomSequence8,]
		      #increase the total number of duties of lecturer
		      #sorted_lecturer[randomSequence1,5] <- sorted_lecturer[randomSequence1,5] + 1
		      
		      #Selecting assistant professor
		      randomSequence9 <- sample(1:nrow(sorted_assistant_professor), 1, replace=F)
		      invigilation_pool9<- sorted_assistant_professor[randomSequence9,]
		      #increase the total number of duties of lecturer
		      #sorted_lecturer[randomSequence1,5] <- sorted_lecturer[randomSequence1,5] + 1
		      
		      reserveDuty <- NULL
		      reserveDuty <- rbind(reserveDuty, invigilation_pool7)
		      reserveDuty <- rbind(reserveDuty, invigilation_pool8)
		      reserveDuty <- rbind(reserveDuty, invigilation_pool9)
		      
		      ##########################################
		      #Printing all the data for the shift
		      ##########################################

		      INVIGILATORS <- paste(invigilationDuty$Designation,invigilationDuty$Name,Sep="")
		      RESERVE <- paste(reserveDuty$Designation, reserveDuty$Name,Sep="")
		      CHIEF <- paste(chiefInvigilator$Designation,chiefInvigilator$Name,Sep="")
		      
		      a<- NULL
		      b<- NULL
		      c<- NULL
		      
		      a<- as.data.frame(INVIGILATORS)
		      b<- as.data.frame(RESERVE)
		      c<- as.data.frame(CHIEF)
		      
		      a$id <- rownames(a)
		      b$id <- rownames(b)
		      c$id <- rownames(c)
		      
		      aa<- merge(a,b,by=c("id"),by.x="id",by.y="id",all.x=T,all.y=T)
		      aa<- merge(aa,c,by=c("id"),by.x="id",by.y="id",all.x=T,all.y=T)
		      
		      #invigilationsFaculty <- as.data.frame(cbind(INVIGILATORS,RESERVE,CHIEF))
		      
		      #invigilationsFaculty[is.na(invigilationsFaculty)] <- ""
		      
		      
	      #capture.output(print(invigilationsFaculty,right=F),file="examinationRoaster.odt",append = TRUE)
		#capture.output(print("============================================================="),file="examinationRoaster.odt",append = TRUE)
	      #capture.output(print(paste("")),file="examinationRoaster.odt",append = TRUE)
	      #capture.output(print(paste("")),file="examinationRoaster.odt",append = TRUE)
	      blankLine <- NULL
	      write.table(aa, file="examinationRoaster.ods", append=T, row.names=F, col.names=T,  sep=",")
	      write.table(blankLine, file="examinationRoaster.ods", append=T, row.names=T, col.names=T,  sep=",")
	    }  

	    else if(capacity$total >= 2){
	  
		    #capture.output(print("*********************************************************************************"),file="examinationRoaster.odt",append = TRUE)
		    #capture.output(print(paste("Day:", format(as.Date(startDate,format = "%d/%m/%Y"), "%a %b %d"), "## Shift:", ifelse(shift==1,timeRange$time1,timeRange$time2),"## Room:", room_capacity$Room[room],"## Room Capacity:", room_capacity$Capacity[room])),file="examinationRoaster.odt",append = TRUE)
		    #capture.output(print("*********************************************************************************"),file="examinationRoaster.odt",append = TRUE)


		    info=print(paste("Day:", format(as.Date(startDate,format = "%d/%m/%Y"), "%a %b %d"), "## Shift:", ifelse(shift==1,timeRange$time1,timeRange$time2),"## Room:", room_capacity$Room[room],"## Room Capacity:", room_capacity$Capacity[room]))	    
		    
		    day_date <- NULL
		    day_date <- as.data.frame(info)

		    write.table(day_date$info, file="examinationRoaster.ods", append=T, row.names=F, col.names=F)
		    
		      invigilation_pool1 <- NULL
		      invigilation_pool2 <- NULL
		      
		      #Selecting lecturer
		      randomSequence1 <- sample(1:nrow(sorted_lecturer), 1, replace=F)

		      if((sorted_lecturer[randomSequence1,6]) < totalDuties){
			invigilation_pool1<- sorted_lecturer[randomSequence1,]
			sorted_lecturer <- sorted_lecturer[-(randomSequence1),]
			#increase the total number of duties of lecturer
			sorted_lecturer[randomSequence1,6] <- sorted_lecturer[randomSequence1,6] + 1
			sorted_lecturer1[randomSequence1,6] <- sorted_lecturer1[randomSequence1,6] + 1
		      }
		      #else{
			#sorted_lecturer1 <- sorted_lecturer1[-(randomSequence1),]
		      #}

		      #Selecting senior lecturer
		      randomSequence2 <- sample(1:nrow(sorted_senior_lecturer), 1, replace=F)

		      if((sorted_senior_lecturer[randomSequence2,6]) < totalDuties){
			invigilation_pool2<- sorted_senior_lecturer[randomSequence2,]
			sorted_senior_lecturer <- sorted_senior_lecturer[-(randomSequence2),]
			#increase the total number of duties of lecturer
			sorted_senior_lecturer[randomSequence2,6] <- sorted_senior_lecturer[randomSequence2,6] + 1
			sorted_senior_lecturer1[randomSequence2,6] <- sorted_senior_lecturer1[randomSequence2,6] + 1
			
		      }
		      #else{
			#sorted_senior_lecturer1 <- sorted_senior_lecturer1[-(randomSequence2),]
		      #}

		      invigilationDuty <- NULL
		      invigilationDuty <- rbind(invigilationDuty, invigilation_pool1)
		      invigilationDuty <- rbind(invigilationDuty, invigilation_pool2)


		      ##########################################
		      # Finding Reserve
		      ##########################################
		      invigilation_pool7 <- NULL
		      invigilation_pool8 <- NULL
		      
		      #Selecting lecturer
		      randomSequence7 <- sample(1:nrow(sorted_lecturer), 1, replace=F)
		      invigilation_pool7<- sorted_lecturer[randomSequence7,]
		      #increase the total number of duties of lecturer
		      #sorted_lecturer[randomSequence7,5] <- sorted_lecturer[randomSequence7,5] + 1
		      
		      #Selecting senior lecturer
		      randomSequence8 <- sample(1:nrow(sorted_senior_lecturer), 1, replace=F)
		      invigilation_pool8<- sorted_senior_lecturer[randomSequence8,]
		      #increase the total number of duties of lecturer
		      #sorted_lecturer[randomSequence8,5] <- sorted_lecturer[randomSequence8,5] + 1
		      
		      
		      reserveDuty <- NULL
		      reserveDuty <- rbind(reserveDuty, invigilation_pool7)
		      reserveDuty <- rbind(reserveDuty, invigilation_pool8)
		      
		      
		      ##########################################
		      #Printing all the data for the shift
		      ##########################################

		      INVIGILATORS <- paste(invigilationDuty$Designation,invigilationDuty$Name,Sep="")
		      RESERVE <- paste(reserveDuty$Designation, reserveDuty$Name,Sep="")
		      CHIEF <- paste(chiefInvigilator$Designation,chiefInvigilator$Name,Sep="")
		      
		      a<- NULL
		      b<- NULL
		      c<- NULL
		      
		      a<- as.data.frame(INVIGILATORS)
		      b<- as.data.frame(RESERVE)
		      c<- as.data.frame(CHIEF)
		      
		      a$id <- rownames(a)
		      b$id <- rownames(b)
		      c$id <- rownames(c)
		      
		      aa<- merge(a,b,by=c("id"),by.x="id",by.y="id",all.x=T,all.y=T)
		      aa<- merge(aa,c,by=c("id"),by.x="id",by.y="id",all.x=T,all.y=T)
		      
		      #invigilationsFaculty <- as.data.frame(cbind(INVIGILATORS,RESERVE,CHIEF))
		      
		      #invigilationsFaculty[is.na(invigilationsFaculty)] <- ""
		      
		      
	      #capture.output(print(invigilationsFaculty,right=F),file="examinationRoaster.odt",append = TRUE)
	      #capture.output(print("============================================================="),file="examinationRoaster.odt",append = TRUE)
	      #capture.output(print(paste("")),file="examinationRoaster.odt",append = TRUE)
	      #capture.output(print(paste("")),file="examinationRoaster.odt",append = TRUE)
	      blankLine <- NULL
	      write.table(aa, file="examinationRoaster.ods", append=T, row.names=F, col.names=T,  sep=",")
	      write.table(blankLine, file="examinationRoaster.ods", append=T, row.names=T, col.names=T,  sep=",")
	    }  
	    #Remove the room which has been allocated
	    #########################################
	    #room_capacity <- room_capacity[-(room),]
		    else if(capacity$total >= 1){
	  
		    #capture.output(print("*********************************************************************************"),file="examinationRoaster.odt",append = TRUE)
		    #capture.output(print(paste("Day:", format(as.Date(startDate,format = "%d/%m/%Y"), "%a %b %d"), "## Shift:", ifelse(shift==1,timeRange$time1,timeRange$time2),"## Room:", room_capacity$Room[room],"## Room Capacity:", room_capacity$Capacity[room])),file="examinationRoaster.odt",append = TRUE)
		    #capture.output(print("*********************************************************************************"),file="examinationRoaster.odt",append = TRUE)


		    info=print(paste("Day:", format(as.Date(startDate,format = "%d/%m/%Y"), "%a %b %d"), "## Shift:", ifelse(shift==1,timeRange$time1,timeRange$time2),"## Room:", room_capacity$Room[room],"## Room Capacity:", room_capacity$Capacity[room]))	    
		    
		    day_date <- NULL
		    day_date <- as.data.frame(info)

		    write.table(day_date$info, file="examinationRoaster.ods", append=T, row.names=F, col.names=F)

		      invigilation_pool1 <- NULL
		      invigilation_pool2 <- NULL
		      
		      #Selecting lecturer
		      randomSequence1 <- sample(1:nrow(sorted_lecturer), 1, replace=F)
		      
		      if((sorted_lecturer[randomSequence1,6]) < totalDuties){
			invigilation_pool1<- sorted_lecturer[randomSequence1,]
			sorted_lecturer <- sorted_lecturer[-(randomSequence1),]
			#increase the total number of duties of lecturer
			sorted_lecturer[randomSequence1,6] <- sorted_lecturer[randomSequence1,6] + 1
			sorted_lecturer1[randomSequence1,6] <- sorted_lecturer1[randomSequence1,6] + 1
		      }
		      #else{
			#sorted_lecturer1 <- sorted_lecturer1[-(randomSequence1),]
		      #}
		      #increase the total number of duties of lecturer
		      #sorted_lecturer[randomSequence1,5] <- sorted_lecturer[randomSequence1,5] + 1

      		      #Selecting lecturer
		      randomSequence2 <- sample(1:nrow(sorted_lecturer), 1, replace=F)
		      
		      if((sorted_lecturer[randomSequence2,6]) < totalDuties){
			invigilation_pool2<- sorted_lecturer[randomSequence2,]
			sorted_lecturer <- sorted_lecturer[-(randomSequence2),]
			#increase the total number of duties of lecturer
			sorted_lecturer[randomSequence2,6] <- sorted_lecturer[randomSequence2,6] + 1
			sorted_lecturer1[randomSequence2,6] <- sorted_lecturer1[randomSequence2,6] + 1

		      }
		      #else{
			#sorted_lecturer1 <- sorted_lecturer1[-(randomSequence2),]
		      #}
		      
		      invigilationDuty <- NULL
		      invigilationDuty <- rbind(invigilationDuty, invigilation_pool1)
		      invigilationDuty <- rbind(invigilationDuty, invigilation_pool2)
		      

		      ##########################################
		      # Finding Reserve
		      ##########################################
		      invigilation_pool7 <- NULL
		      invigilation_pool8 <- NULL
		      invigilation_pool9 <- NULL
		      
		      #Selecting lecturer
		      randomSequence7 <- sample(1:nrow(sorted_lecturer), 1, replace=F)
		      invigilation_pool7<- sorted_lecturer[randomSequence7,]
		      #increase the total number of duties of lecturer
		      #sorted_lecturer[randomSequence7,5] <- sorted_lecturer[randomSequence7,5] + 1
		      
		      #Selecting senior lecturer
		      randomSequence8 <- sample(1:nrow(sorted_senior_lecturer), 1, replace=F)
		      invigilation_pool8<- sorted_senior_lecturer[randomSequence8,]
		      #increase the total number of duties of lecturer
		      #sorted_senior_lecturer[randomSequence8,5] <- sorted_senior_lecturer[randomSequence8,5] + 1
		      
		      
		      reserveDuty <- NULL
		      reserveDuty <- rbind(reserveDuty, invigilation_pool7)
		      reserveDuty <- rbind(reserveDuty, invigilation_pool8)
		      
		      ##########################################
		      #Printing all the data for the shift
		      ##########################################

		      INVIGILATORS <- paste(invigilationDuty$Designation,invigilationDuty$Name,Sep="")
		      RESERVE <- paste(reserveDuty$Designation, reserveDuty$Name,Sep="")
		      CHIEF <- paste(chiefInvigilator$Designation,chiefInvigilator$Name,Sep="")
		      
		      a<- NULL
		      b<- NULL
		      c<- NULL
		      
		      a<- as.data.frame(INVIGILATORS)
		      b<- as.data.frame(RESERVE)
		      c<- as.data.frame(CHIEF)
		      
		      a$id <- rownames(a)
		      b$id <- rownames(b)
		      c$id <- rownames(c)
		      
		      aa<- merge(a,b,by=c("id"),by.x="id",by.y="id",all.x=T,all.y=T)
		      aa<- merge(aa,c,by=c("id"),by.x="id",by.y="id",all.x=T,all.y=T)
		      
		      #invigilationsFaculty <- as.data.frame(cbind(INVIGILATORS,RESERVE,CHIEF))
		      
		      #invigilationsFaculty[is.na(invigilationsFaculty)] <- ""
		      
		      
	      #capture.output(print(invigilationsFaculty,right=F),file="examinationRoaster.odt",append = TRUE)
		#capture.output(print("============================================================="),file="examinationRoaster.odt",append = TRUE)
	      #capture.output(print(paste("")),file="examinationRoaster.odt",append = TRUE)
	      #capture.output(print(paste("")),file="examinationRoaster.odt",append = TRUE)
	      blankLine <- NULL
	      write.table(aa, file="examinationRoaster.ods", append=T, row.names=F, col.names=T,  sep=",")
	      write.table(blankLine, file="examinationRoaster.ods", append=T, row.names=T, col.names=T,  sep=",")
	    }  

	    else {
		
		    #capture.output(print("*********************************************************************************"),file="examinationRoaster.odt",append = TRUE)
		    #capture.output(),file="examinationRoaster.odt",append = TRUE)
		    #capture.output(print("*********************************************************************************"),file="examinationRoaster.odt",append = TRUE)

		    info=print(paste("Day:", format(as.Date(startDate,format = "%d/%m/%Y"), "%a %b %d"), "## Shift:", ifelse(shift==1,timeRange$time1,timeRange$time2),"## Room:", room_capacity$Room[room],"## Room Capacity:", room_capacity$Capacity[room]))	    
		    
		    day_date <- NULL
		    day_date <- as.data.frame(info)

		    write.table(day_date$info, file="examinationRoaster.ods", append=T, row.names=F, col.names=F)
		    
		      invigilation_pool1 <- NULL
		      invigilation_pool2 <- NULL
		      
		      #Selecting lecturer
		      randomSequence1 <- sample(1:nrow(sorted_lecturer), 1, replace=F)
		      
		      if((sorted_lecturer[randomSequence1,6]) < totalDuties){
			invigilation_pool1<- sorted_lecturer[randomSequence1,]
			sorted_lecturer <- sorted_lecturer[-(randomSequence1),]
			#increase the total number of duties of lecturer
			sorted_lecturer[randomSequence1,6] <- sorted_lecturer[randomSequence1,6] + 1
			sorted_lecturer1[randomSequence1,6] <- sorted_lecturer1[randomSequence1,6] + 1

		      }
		      #else{
			#sorted_lecturer1 <- sorted_lecturer1[-(randomSequence1),]
		      #}
		      
		      #increase the total number of duties of lecturer
		      #sorted_lecturer[randomSequence1,5] <- sorted_lecturer[randomSequence1,5] + 1

      		      #Selecting lecturer
		      randomSequence2 <- sample(1:nrow(sorted_lecturer), 1, replace=F)
		      
		      if((sorted_lecturer[randomSequence2,6]) < totalDuties){
			invigilation_pool2<- sorted_lecturer[randomSequence2,]
			sorted_lecturer <- sorted_lecturer[-(randomSequence2),]
			#increase the total number of duties of lecturer
			sorted_lecturer[randomSequence2,6] <- sorted_lecturer[randomSequence2,6] + 1
			sorted_lecturer1[randomSequence2,6] <- sorted_lecturer1[randomSequence2,6] + 1
		      }
		      #else{
			#sorted_lecturer1 <- sorted_lecturer1[-(randomSequence2),]
		      #}

		      
		      invigilationDuty <- NULL
		      invigilationDuty <- rbind(invigilationDuty, invigilation_pool1)
		      invigilationDuty <- rbind(invigilationDuty, invigilation_pool2)
		      

		      ##########################################
		      # Finding Reserve
		      ##########################################
		      invigilation_pool8 <- NULL
		      invigilation_pool9 <- NULL
		      
		      #Selecting senior lecturer
		      randomSequence8 <- sample(1:nrow(sorted_senior_lecturer), 1, replace=F)
		      invigilation_pool8<- sorted_senior_lecturer[randomSequence8,]
		      #increase the total number of duties of lecturer
		      #sorted_senior_lecturer[randomSequence8,5] <- sorted_senior_lecturer[randomSequence8,5] + 1
		      
		      #Selecting assistant professor
		      randomSequence9 <- sample(1:nrow(sorted_assistant_professor), 1, replace=F)
		      invigilation_pool9<- sorted_assistant_professor[randomSequence9,]
		      #increase the total number of duties of lecturer
		      #sorted_assistant_professor[randomSequence9,5] <- sorted_assistant_professor[randomSequence9,5] + 1
		      
		      reserveDuty <- NULL
		      reserveDuty <- rbind(reserveDuty, invigilation_pool8)
		      reserveDuty <- rbind(reserveDuty, invigilation_pool9)
		      
		      
		      
		      ##########################################
		      #Printing all the data for the shift
		      ##########################################

		      INVIGILATORS <- paste(invigilationDuty$Designation,invigilationDuty$Name,Sep="")
		      RESERVE <- paste(reserveDuty$Designation, reserveDuty$Name,Sep="")
		      CHIEF <- paste(chiefInvigilator$Designation,chiefInvigilator$Name,Sep="")
		      
		      a<- NULL
		      b<- NULL
		      c<- NULL
		      
		      a<- as.data.frame(INVIGILATORS)
		      b<- as.data.frame(RESERVE)
		      c<- as.data.frame(CHIEF)
		      
		      a$id <- rownames(a)
		      b$id <- rownames(b)
		      c$id <- rownames(c)
		      
		      aa<- merge(a,b,by=c("id"),by.x="id",by.y="id",all.x=T,all.y=T)
		      aa<- merge(aa,c,by=c("id"),by.x="id",by.y="id",all.x=T,all.y=T)
		      
		      #invigilationsFaculty <- as.data.frame(cbind(INVIGILATORS,RESERVE,CHIEF))
		      
		      #invigilationsFaculty[is.na(invigilationsFaculty)] <- ""
		      
		      
	      #capture.output(print(invigilationsFaculty,right=F),file="examinationRoaster.odt",append = TRUE)
		#capture.output(print("============================================================="),file="examinationRoaster.odt",append = TRUE)
	      #capture.output(print(paste("")),file="examinationRoaster.odt",append = TRUE)
	      #capture.output(print(paste("")),file="examinationRoaster.odt",append = TRUE)
	      blankLine <- NULL
	      write.table(aa, file="examinationRoaster.ods", append=T, row.names=F, col.names=T,  sep=",")
	      write.table(blankLine, file="examinationRoaster.ods", append=T, row.names=T, col.names=T,  sep=",")
	    } 
	    
	    
	
	#}#End of checking total number of duties
	#else{
	#  room <- room - 1
	#}
	
    }#End of Room Search and Allocate    
  }#End of Shift Search and Allocate
  }#End of if which checking holiday
  startDate <- as.Date(startDate,format = "%d/%m/%Y") +1

}#End of Day Search and Allocate


write.table(sorted_lecturer1, file="totalDuties.ods", append=T, row.names=T, col.names=T,  sep=",")
write.table(sorted_senior_lecturer1, file="totalDuties.ods", append=T, row.names=T, col.names=T,  sep=",")
write.table(sorted_assistant_professor1, file="totalDuties.ods", append=T, row.names=T, col.names=T,  sep=",")
write.table(sorted_associate_professor1, file="totalDuties.ods", append=T, row.names=T, col.names=T,  sep=",")
write.table(sorted_professor1, file="totalDuties.ods", append=T, row.names=T, col.names=T,  sep=",")
write.table(sorted_adviser1, file="totalDuties.ods", append=T, row.names=T, col.names=T,  sep=",")

