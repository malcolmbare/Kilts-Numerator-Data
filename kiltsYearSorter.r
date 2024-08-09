library(tidyverse)
library(data.table)
library(here)
source("/kilts/numerator_raw/20240423/Scripts/kiltsHelperFunctions.R")

# This limits how large input and output files can be
bufferSizeLimit <- 2000#MB 

# Directories
DATA_DIRNAME <- "/kilts/numerator_raw/20240423/"

#Processes
tableProcesses <- list("sortPeopleAttributes", "sortItem","sortPeople","sortBanner", "sortStatic", "sortFact")

#Patterns
peopleAttributesTablePatternIn <- "standard_nmr_feed_people_attributes_table_.*csv"
peopleTablePatternIn <- "standard_nmr_feed_people_table_.*csv"
staticTablePatternIn <- "standard_nmr_feed_static_table_.*csv"
bannerTablePatternIn <- "standard_nmr_feed_banner_table_.*csv"
itemTablePatternIn <- "standard_nmr_feed_item_table_.*csv"
factTablePatternIn <- "standard_nmr_feed_fact_table_.*csv"

#File Paths
peopleAttributesTableFPS<- list.files(DATA_DIRNAME, peopleAttributesTablePatternIn, full.names = TRUE)
peopleTableFPS<- list.files(DATA_DIRNAME, peopleTablePatternIn, full.names = TRUE)
staticTableFPS <- list.files(DATA_DIRNAME, staticTablePatternIn, full.names = TRUE)
bannerTableFPS <- list.files(DATA_DIRNAME, bannerTablePatternIn, full.names = TRUE)
itemTableFPS <- list.files(DATA_DIRNAME, itemTablePatternIn, full.names = TRUE)
factTableFPS <- list.files(DATA_DIRNAME, factTablePatternIn, full.names = TRUE)

for(factTable in factTableFPS)
{
processAllTable(factTable, "sortPeopleAttributes")}



