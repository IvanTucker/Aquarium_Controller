#### Program Info ####
title: "Aquarium Data Collector"
author: "Ivan Tucker"
date: "September 30, 2016"

Copyright (C) 2016  Ivan Tucker

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Contact me electronically at OnTheEdgeConsulting@gmail.com

#### Load Packages ####

require("parallel")
require("snow")
require("snowfall")
require("tree")
require("XML")
require("RCurl")
require("plyr")
require("tidyr")

# Initialize Snowfall with explicit settings.
sfInit( parallel=TRUE, cpus=4) # adjust the number of CPUs to fit your machine

#### System information ####
system_name <- " enter system description here" # Leave the quotation marks. This will also be the name of the output file
IPAddress <- "000.000.0.00:00000" # leave the quotation marks - enter your IP address and port information
Record_Start_Date <- "161010"  # Make sure you enter in yymmdd format
Days_In_Record <- "4" # Enter the length of this record. My system will not transfer more than 6 days. 

#### Get probe data ####
xml.url.datalog <- paste("http://",IPAddress,"/cgi-bin/datalog.xml?sdate=",Record_Start_Date,"&days=",Days_In_Record, sep=""); #Make sure Open XML access is enabled in Apex
Script.datalog <- getURL(xml.url.datalog);
doc.datalog <- xmlParse(Script.datalog);
r <- xmlRoot(doc.datalog);
src <- xpathSApply((doc.datalog), "//record");
src_length<-length(src);
probes = xpathSApply(r[[4]], ".//name", xmlChildren)
probe.count<-length(probes)
# Create dataframe
foo<-xmlSApply(src[[1]], xmlValue)
xml_datalog<-data.frame(t(foo), stringsAsFactors = FALSE)
xml_datalog<-xml_datalog[,1, drop=FALSE]
for (j in 1:probe.count){
  if(j==1){  
    k<-j+1
    probename<-xmlSApply((src)[[1]][[k]][[1]], xmlValue)
    probevalue<-xmlSApply((src)[[1]][[k]][[3]], xmlValue)
    probedata<-data.frame(probevalue, row.names = probename, stringsAsFactors = FALSE)
    probedata<-data.frame(t(probedata))}
  else {
    k<-j+1
    tmp_name<-xmlSApply((src)[[1]][[k]][[1]], xmlValue)
    tmp_value<-xmlSApply((src)[[1]][[k]][[3]], xmlValue)
    tmp_data<-data.frame(tmp_value, row.names = tmp_name, stringsAsFactors = FALSE)
    tmp_data<-data.frame(t(tmp_data))
    probedata<-cbind(probedata, tmp_data)}}
xml_datalog<-cbind(xml_datalog, probedata)

# Fill in dataframe
for (i in 2:src_length){  
  foo<-xmlSApply(src[[i]], xmlValue)
  tmp<-data.frame(t(foo), stringsAsFactors = FALSE)
  tmp<-tmp[,1, drop=FALSE]
  for (j in 1:probe.count){
    if(j==1){  
      k<-j+1
      probename<-xmlSApply((src)[[i]][[k]][[1]], xmlValue)
      probevalue<-xmlSApply((src)[[i]][[k]][[3]], xmlValue)
      probedata<-data.frame(probevalue, row.names = probename, stringsAsFactors = FALSE)
      probedata<-data.frame(t(probedata))}
    else {
      k<-j+1
      probename<-xmlSApply((src)[[i]][[k]][[1]], xmlValue)
      probevalue<-xmlSApply((src)[[i]][[k]][[3]], xmlValue)
      tmp_data<-data.frame(probevalue, row.names = probename, stringsAsFactors = FALSE)
      tmp_data<-data.frame(t(tmp_data))
      probedata<-cbind(probedata, tmp_data)}}
  tmp<-cbind(tmp, probedata)
  xml_datalog<-rbind(xml_datalog, tmp)}

# check for and eliminate duplicate records
anyDuplicated(xml_datalog) # should be zero
xml_datalog<- unique(xml_datalog) # delete duplicates
anyDuplicated(xml_datalog) # definitely zero now

#### Get switch data ####
xml.url.outlet <- paste("http://",IPAddress,"/cgi-bin/outlog.xml?sdate=",Record_Start_Date,"&days=",Days_In_Record, sep=""); #Make sure Open XML access is enabled in Apex
Script.outlet <- getURL(xml.url.outlet);
doc.outlet <- xmlTreeParse(Script.outlet);
r.outlet <- xmlRoot(doc.outlet);
src.outlet <- xpathApply(xmlRoot(doc.outlet), "//record"); 

for(i in 1:length(src.outlet)){
  if (i==1){
    foo<-xmlApply(src.outlet[[i]], xmlValue)
    xml_outletlog<-data.frame(t(foo),
                              stringsAsFactors = TRUE)}
  else {
    foo<-xmlApply(src.outlet[[i]], xmlValue)
    tmp<-data.frame(t(foo), stringsAsFactors = TRUE)
    xml_outletlog<-rbind(xml_outletlog, tmp)}
}

# Reshape log data
xml_outletlog$date<-as.character(xml_outletlog$date)
xml_outletlog$name<-as.character(xml_outletlog$name)
xml_outletlog$value<-as.character(xml_outletlog$value)
xml_outletlog<-reshape(xml_outletlog, idvar = "date", timevar="name", direction = "wide")

# Order
xml_outletlog<- xml_outletlog[ do.call(order, xml_outletlog),]

# check for and delete duplicates
anyDuplicated(xml_outletlog) # should be zero 
xml_outletlog<- unique(xml_outletlog) # elimates duplicate records
anyDuplicated(xml_outletlog) # should be zero now

#### combine probe and switch data ####
# merge dataframes
new_dataset<- merge(xml_datalog, xml_outletlog, all = TRUE)

# fill in missing values
new_dataset_names<- colnames(new_dataset)
new_dataset<- fill_(new_dataset, fill_cols = dataset_names)

# if this is your first time through, run the line below. Otherwise leave it commented out.
# dataset<- new_dataset 

#### add to exist data ####
# merge dataframes 
dataset <- merge(dataset, new_dataset, all=TRUE)

# make sure data is in the right order
dataset<- dataset[ do.call(order, dataset),]

# check for and delete duplicates
anyDuplicated(dataset) # should be zero 
dataset<- unique(dataset) # elimates duplicate

#### write to file ####
file_name=paste(system_name),".csv", sep="" )
write.csv(dataset, file= file_name)