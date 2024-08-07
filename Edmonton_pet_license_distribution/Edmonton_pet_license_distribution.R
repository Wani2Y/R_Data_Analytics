library(sf)
library(dplyr)
library(plotly)
library(tmap)
library(tmaptools)
library(RColorBrewer)
library(ggformula)
library(ggplot2)
library(ggstance)

#read in the Edmonton shapefile
Ed_shp <- st_read("D:/R_projects/Edmonton_mapping/Shape_file_2019/geo_export_490d2b41-f973-439c-9011-ded07363af86.shp", stringsAsFactors = FALSE) %>% select("descriptiv", "geometry")
str(Ed_shp)
head(Ed_shp)

#read in the pet file
pet_info <- read.csv("D:/R_projects/Edmonton_mapping/Pet_Licenses_by_Neighbourhood.csv", header = TRUE) %>% as_tibble()
str(pet_info)
head(pet_info)

#filter out the observations without locations and the extrea location info
pet_info$lic_date <- paste(pet_info$YEAR, pet_info$MONTH, sep ="_")
pet_info$season <- ifelse(pet_info$MONTH_NUMBER == c(3, 4, 5), "Spring",  ifelse(pet_info$MONTH_NUMBER == c(6, 7, 8), "Summer", ifelse(pet_info$MONTH_NUMBER == c(9, 10, 11), "Fall", "Winter")))
pet_data <- pet_info %>% select("NEIGHBOURHOOD", "NEIGHBOURHOOD_ID", "lic_date", "YEAR", "season", "LONGITUDE", "LATITUDE", "PET_TYPE", "BREED", "SPAYED_OR_NEUTERED", "GENDER")
pet_data <- filter(pet_data, is.na(pet_info$LONGITUDE) == 0)
pet_data <- filter(pet_data, YEAR >= 2016)
Ed_pet_data <- left_join(Ed_shp, pet_data, by = c("descriptiv" = "NEIGHBOURHOOD"))

#cat vs dog dominated region in 2020
Ed_cat_dog_data <- filter(Ed_pet_data, PET_TYPE == "Cat" | PET_TYPE == "Dog")
#Ed_cat_dog_data <- filter(Ed_pet_data, YEAR == 2021)
##########################################################################################################################
#count the number of rows at each descriptive areas for cats and dogs
#Evansdale
Evansdale <- filter(Ed_cat_dog_data, descriptiv == "Evansdale")
cat_num_Evansdale <- sum(Evansdale$PET_TYPE == "Cat")
dog_num_Evansdale <- sum(Evansdale$PET_TYPE == "Dog")
ifelse(dog_num_Evansdale > cat_num_Evansdale, Evansdale$Cat_or_Dog <- "Dog", Evansdale$Cat_or_Dog <- "Cat")

#Northmount
Northmount <- filter(Ed_cat_dog_data, descriptiv == "Northmount")
cat_num_Northmount <- sum(Northmount$PET_TYPE == "Cat")
dog_num_Northmount <- sum(Northmount$PET_TYPE == "Dog")
ifelse(dog_num_Northmount > cat_num_Northmount, Northmount$Cat_or_Dog <- "Dog", Northmount$Cat_or_Dog <- "Cat")

#Prince_Rupert
Prince_Rupert <- filter(Ed_cat_dog_data, descriptiv == "Prince_Rupert")
cat_num_Prince_Rupert <- sum(Prince_Rupert$PET_TYPE == "Cat")
dog_num_Prince_Rupert <- sum(Prince_Rupert$PET_TYPE == "Dog")
ifelse(dog_num_Prince_Rupert > cat_num_Prince_Rupert, Prince_Rupert$Cat_or_Dog <- "Dog", Prince_Rupert$Cat_or_Dog <- "Cat")

#Spruce_Avenue
Spruce_Avenue <- filter(Ed_cat_dog_data, descriptiv == "Spruce_Avenue")
cat_num_Spruce_Avenue <- sum(Spruce_Avenue$PET_TYPE == "Cat")
dog_num_Spruce_Avenue <- sum(Spruce_Avenue$PET_TYPE == "Dog")
ifelse(dog_num_Spruce_Avenue > cat_num_Spruce_Avenue, Spruce_Avenue$Cat_or_Dog <- "Dog", Spruce_Avenue$Cat_or_Dog <- "Cat")

#Westwood
Westwood <- filter(Ed_cat_dog_data, descriptiv == "Westwood")
cat_num_Westwood <- sum(Westwood$PET_TYPE == "Cat")
dog_num_Westwood <- sum(Westwood$PET_TYPE == "Dog")
ifelse(dog_num_Westwood > cat_num_Westwood, Westwood$Cat_or_Dog <- "Dog", Westwood$Cat_or_Dog <- "Cat")

#Abbottsfield
Abbottsfield <- filter(Ed_cat_dog_data, descriptiv == "Abbottsfield")
cat_num_Abbottsfield <- sum(Abbottsfield$PET_TYPE == "Cat")
dog_num_Abbottsfield <- sum(Abbottsfield$PET_TYPE == "Dog")
ifelse(dog_num_Abbottsfield > cat_num_Abbottsfield, Abbottsfield$Cat_or_Dog <- "Dog", Abbottsfield$Cat_or_Dog <- "Cat")

#Albany
Albany <- filter(Ed_cat_dog_data, descriptiv == "Albany")
cat_num_Albany <- sum(Albany$PET_TYPE == "Cat")
dog_num_Albany <- sum(Albany$PET_TYPE == "Dog")
ifelse(dog_num_Albany > cat_num_Albany, Albany$Cat_or_Dog <- "Dog", Albany$Cat_or_Dog <- "Cat")

#Alberta Avenue
Alberta_Avenue <- filter(Ed_cat_dog_data, descriptiv == "Alberta Avenue")
cat_num_Alberta_Avenue <- sum(Alberta_Avenue$PET_TYPE == "Cat")
dog_num_Alberta_Avenue <- sum(Alberta_Avenue$PET_TYPE == "Dog")
ifelse(dog_num_Alberta_Avenue > cat_num_Alberta_Avenue, Alberta_Avenue$Cat_or_Dog <- "Dog", Alberta_Avenue$Cat_or_Dog <- "Cat")

#Aldergrove
Aldergrove <- filter(Ed_cat_dog_data, descriptiv == "Aldergrove")
cat_num_Aldergrove <- sum(Aldergrove$PET_TYPE == "Cat")
dog_num_Aldergrove <- sum(Aldergrove$PET_TYPE == "Dog")
ifelse(dog_num_Aldergrove > cat_num_Aldergrove, Aldergrove$Cat_or_Dog <- "Dog", Aldergrove$Cat_or_Dog <- "Cat")

#Alberta Park Industrial
Alberta_Park_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Alberta Park Industrial")
cat_num_Alberta_Park_Industrial <- sum(Alberta_Park_Industrial$PET_TYPE == "Cat")
dog_num_Alberta_Park_Industrial <- sum(Alberta_Park_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Alberta_Park_Industrial > cat_num_Alberta_Park_Industrial, Alberta_Park_Industrial$Cat_or_Dog <- "Dog", Alberta_Park_Industrial$Cat_or_Dog <- "Cat")

#Allard
Allard <- filter(Ed_cat_dog_data, descriptiv == "Allard")
cat_num_Allard <- sum(Allard$PET_TYPE == "Cat")
dog_num_Allard <- sum(Allard$PET_TYPE == "Dog")
ifelse(dog_num_Allard > cat_num_Allard, Allard$Cat_or_Dog <- "Dog", Allard$Cat_or_Dog <- "Cat")

#Allendale
Allendale <- filter(Ed_cat_dog_data, descriptiv == "Allendale")
cat_num_Allendale <- sum(Allendale$PET_TYPE == "Cat")
dog_num_Allendale <- sum(Allendale$PET_TYPE == "Dog")
ifelse(dog_num_Allendale > cat_num_Allendale, Allendale$Cat_or_Dog <- "Dog", Allendale$Cat_or_Dog <- "Cat")

#Ambleside
Ambleside <- filter(Ed_cat_dog_data, descriptiv == "Ambleside")
cat_num_Ambleside <- sum(Ambleside$PET_TYPE == "Cat")
dog_num_Ambleside <- sum(Ambleside$PET_TYPE == "Dog")
ifelse(dog_num_Ambleside > cat_num_Ambleside, Ambleside$Cat_or_Dog <- "Dog", Ambleside$Cat_or_Dog <- "Cat")

#Argyll
Argyll <- filter(Ed_cat_dog_data, descriptiv == "Argyll")
cat_num_Argyll <- sum(Argyll$PET_TYPE == "Cat")
dog_num_Argyll <- sum(Argyll$PET_TYPE == "Dog")
ifelse(dog_num_Argyll > cat_num_Argyll, Argyll$Cat_or_Dog <- "Dog", Argyll$Cat_or_Dog <- "Cat")

#Armstrong_Industrial
Armstrong_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Armstrong Industrial")
cat_num_Armstrong_Industrial <- sum(Armstrong_Industrial$PET_TYPE == "Cat")
dog_num_Armstrong_Industrial <- sum(Armstrong_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Armstrong_Industrial > cat_num_Armstrong_Industrial, Armstrong_Industrial$Cat_or_Dog <- "Dog", Armstrong_Industrial$Cat_or_Dog <- "Cat")

#Aspen_Gardens
Aspen_Gardens <- filter(Ed_cat_dog_data, descriptiv == "Aspen Gardens")
cat_num_Aspen_Gardens <- sum(Aspen_Gardens$PET_TYPE == "Cat")
dog_num_Aspen_Gardens <- sum(Aspen_Gardens$PET_TYPE == "Dog")
ifelse(dog_num_Aspen_Gardens > cat_num_Aspen_Gardens, Aspen_Gardens$Cat_or_Dog <- "Dog", Aspen_Gardens$Cat_or_Dog <- "Cat")

#Beacon_Heights
Beacon_Heights <- filter(Ed_cat_dog_data, descriptiv == "Beacon Heights")
cat_num_Beacon_Heights <- sum(Beacon_Heights$PET_TYPE == "Cat")
dog_num_Beacon_Heights <- sum(Beacon_Heights$PET_TYPE == "Dog")
ifelse(dog_num_Beacon_Heights > cat_num_Beacon_Heights, Beacon_Heights$Cat_or_Dog <- "Dog", Beacon_Heights$Cat_or_Dog <- "Cat")

#Aster
Aster <- filter(Ed_cat_dog_data, descriptiv == "Aster")
cat_num_Aster <- sum(Aster$PET_TYPE == "Cat")
dog_num_Aster <- sum(Aster$PET_TYPE == "Dog")
ifelse(dog_num_Aster > cat_num_Aster, Aster$Cat_or_Dog <- "Dog", Aster$Cat_or_Dog <- "Cat")

#Athlone
Athlone <- filter(Ed_cat_dog_data, descriptiv == "Athlone")
cat_num_Athlone <- sum(Athlone$PET_TYPE == "Cat")
dog_num_Athlone <- sum(Athlone$PET_TYPE == "Dog")
ifelse(dog_num_Athlone > cat_num_Athlone, Athlone$Cat_or_Dog <- "Dog", Athlone$Cat_or_Dog <- "Cat")

#Avonmore
Avonmore <- filter(Ed_cat_dog_data, descriptiv == "Avonmore")
cat_num_Avonmore <- sum(Avonmore$PET_TYPE == "Cat")
dog_num_Avonmore <- sum(Avonmore$PET_TYPE == "Dog")
ifelse(dog_num_Avonmore > cat_num_Avonmore, Avonmore$Cat_or_Dog <- "Dog", Avonmore$Cat_or_Dog <- "Cat")

#Balwin
Balwin <- filter(Ed_cat_dog_data, descriptiv == "Balwin")
cat_num_Balwin <- sum(Balwin$PET_TYPE == "Cat")
dog_num_Balwin <- sum(Balwin$PET_TYPE == "Dog")
ifelse(dog_num_Balwin > cat_num_Balwin, Balwin$Cat_or_Dog <- "Dog", Balwin$Cat_or_Dog <- "Cat")

#Bannerman
Bannerman <- filter(Ed_cat_dog_data, descriptiv == "Bannerman")
cat_num_Bannerman <- sum(Bannerman$PET_TYPE == "Cat")
dog_num_Bannerman <- sum(Bannerman$PET_TYPE == "Dog")
ifelse(dog_num_Bannerman > cat_num_Bannerman, Bannerman$Cat_or_Dog <- "Dog", Bannerman$Cat_or_Dog <- "Cat")

#Baranow
Baranow <- filter(Ed_cat_dog_data, descriptiv == "Baranow")
cat_num_Baranow <- sum(Baranow$PET_TYPE == "Cat")
dog_num_Baranow <- sum(Baranow$PET_TYPE == "Dog")
ifelse(dog_num_Baranow > cat_num_Baranow, Baranow$Cat_or_Dog <- "Dog", Baranow$Cat_or_Dog <- "Cat")

#Baturyn
Baturyn <- filter(Ed_cat_dog_data, descriptiv == "Baturyn")
cat_num_Baturyn <- sum(Baturyn$PET_TYPE == "Cat")
dog_num_Baturyn <- sum(Baturyn$PET_TYPE == "Dog")
ifelse(dog_num_Baturyn > cat_num_Baturyn, Baturyn$Cat_or_Dog <- "Dog", Baturyn$Cat_or_Dog <- "Cat")

#Bearspaw
Bearspaw <- filter(Ed_cat_dog_data, descriptiv == "Bearspaw")
cat_num_Bearspaw <- sum(Bearspaw$PET_TYPE == "Cat")
dog_num_Bearspaw <- sum(Bearspaw$PET_TYPE == "Dog")
ifelse(dog_num_Bearspaw > cat_num_Bearspaw, Bearspaw$Cat_or_Dog <- "Dog", Bearspaw$Cat_or_Dog <- "Cat")

#Beaumaris
Beaumaris <- filter(Ed_cat_dog_data, descriptiv == "Beaumaris")
cat_num_Beaumaris <- sum(Beaumaris$PET_TYPE == "Cat")
dog_num_Beaumaris <- sum(Beaumaris$PET_TYPE == "Dog")
ifelse(dog_num_Beaumaris > cat_num_Beaumaris, Beaumaris$Cat_or_Dog <- "Dog", Beaumaris$Cat_or_Dog <- "Cat")

#Bisset
Bisset <- filter(Ed_cat_dog_data, descriptiv == "Bisset")
cat_num_Bisset <- sum(Bisset$PET_TYPE == "Cat")
dog_num_Bisset <- sum(Bisset$PET_TYPE == "Dog")
ifelse(dog_num_Bisset > cat_num_Bisset, Bisset$Cat_or_Dog <- "Dog", Bisset$Cat_or_Dog <- "Cat")

#Belle_Rive
Belle_Rive <- filter(Ed_cat_dog_data, descriptiv == "Belle Rive")
cat_num_Belle_Rive <- sum(Belle_Rive$PET_TYPE == "Cat")
dog_num_Belle_Rive <- sum(Belle_Rive$PET_TYPE == "Dog")
ifelse(dog_num_Belle_Rive > cat_num_Belle_Rive, Belle_Rive$Cat_or_Dog <- "Dog", Belle_Rive$Cat_or_Dog <- "Cat")

#Belgravia
Belgravia <- filter(Ed_cat_dog_data, descriptiv == "Belgravia")
cat_num_Belgravia <- sum(Belgravia$PET_TYPE == "Cat")
dog_num_Belgravia <- sum(Belgravia$PET_TYPE == "Dog")
ifelse(dog_num_Belgravia > cat_num_Belgravia, Belgravia$Cat_or_Dog <- "Dog", Belgravia$Cat_or_Dog <- "Cat")

#Bellevue
Bellevue <- filter(Ed_cat_dog_data, descriptiv == "Bellevue")
cat_num_Bellevue <- sum(Bellevue$PET_TYPE == "Cat")
dog_num_Bellevue <- sum(Bellevue$PET_TYPE == "Dog")
ifelse(dog_num_Bellevue > cat_num_Bellevue, Bellevue$Cat_or_Dog <- "Dog", Bellevue$Cat_or_Dog <- "Cat")

#Belmead
Belmead <- filter(Ed_cat_dog_data, descriptiv == "Belmead")
cat_num_Belmead <- sum(Belmead$PET_TYPE == "Cat")
dog_num_Belmead <- sum(Belmead$PET_TYPE == "Dog")
ifelse(dog_num_Belmead > cat_num_Belmead, Belmead$Cat_or_Dog <- "Dog", Belmead$Cat_or_Dog <- "Cat")

#Belmont
Belmont <- filter(Ed_cat_dog_data, descriptiv == "Belmont")
cat_num_Belmont <- sum(Belmont$PET_TYPE == "Cat")
dog_num_Belmont <- sum(Belmont$PET_TYPE == "Dog")
ifelse(dog_num_Belmont > cat_num_Belmont, Belmont$Cat_or_Dog <- "Dog", Belmont$Cat_or_Dog <- "Cat")

#Belvedere
Belvedere <- filter(Ed_cat_dog_data, descriptiv == "Belvedere")
cat_num_Belvedere <- sum(Belvedere$PET_TYPE == "Cat")
dog_num_Belvedere <- sum(Belvedere$PET_TYPE == "Dog")
ifelse(dog_num_Belvedere > cat_num_Belvedere, Belvedere$Cat_or_Dog <- "Dog", Belvedere$Cat_or_Dog <- "Cat")

#Bergman
Bergman <- filter(Ed_cat_dog_data, descriptiv == "Bergman")
cat_num_Bergman <- sum(Bergman$PET_TYPE == "Cat")
dog_num_Bergman <- sum(Bergman$PET_TYPE == "Dog")
ifelse(dog_num_Bergman > cat_num_Bergman, Bergman$Cat_or_Dog <- "Dog", Bergman$Cat_or_Dog <- "Cat")

#Beverly_Heights
Beverly_Heights <- filter(Ed_cat_dog_data, descriptiv == "Beverly Heights")
cat_num_Beverly_Heights <- sum(Beverly_Heights$PET_TYPE == "Cat")
dog_num_Beverly_Heights <- sum(Beverly_Heights$PET_TYPE == "Dog")
ifelse(dog_num_Beverly_Heights > cat_num_Beverly_Heights, Beverly_Heights$Cat_or_Dog <- "Dog", Beverly_Heights$Cat_or_Dog <- "Cat")

#Bisset
Bisset <- filter(Ed_cat_dog_data, descriptiv == "Bisset")
cat_num_Bisset <- sum(Bisset$PET_TYPE == "Cat")
dog_num_Bisset <- sum(Bisset$PET_TYPE == "Dog")
ifelse(dog_num_Bisset > cat_num_Bisset, Bisset$Cat_or_Dog <- "Dog", Bisset$Cat_or_Dog <- "Cat")

#Blackburne
Blackburne <- filter(Ed_cat_dog_data, descriptiv == "Blackburne")
cat_num_Blackburne <- sum(Blackburne$PET_TYPE == "Cat")
dog_num_Blackburne <- sum(Blackburne$PET_TYPE == "Dog")
ifelse(dog_num_Blackburne > cat_num_Blackburne, Blackburne$Cat_or_Dog <- "Dog", Blackburne$Cat_or_Dog <- "Cat")

#Blackmud_Creek
Blackmud_Creek <- filter(Ed_cat_dog_data, descriptiv == "Blackmud Creek")
cat_num_Blackmud_Creek <- sum(Blackmud_Creek$PET_TYPE == "Cat")
dog_num_Blackmud_Creek <- sum(Blackmud_Creek$PET_TYPE == "Dog")
ifelse(dog_num_Blackmud_Creek > cat_num_Blackmud_Creek, Blackmud_Creek$Cat_or_Dog <- "Dog", Blackmud_Creek$Cat_or_Dog <- "Cat")

#Blatchford_Area
Blatchford_Area <- filter(Ed_cat_dog_data, descriptiv == "Blatchford Area")
cat_num_Blatchford_Area <- sum(Blatchford_Area$PET_TYPE == "Cat")
dog_num_Blatchford_Area <- sum(Blatchford_Area$PET_TYPE == "Dog")
ifelse(dog_num_Blatchford_Area > cat_num_Blatchford_Area, Blatchford_Area$Cat_or_Dog <- "Dog", Blatchford_Area$Cat_or_Dog <- "Cat")

#Blue_Quill
Blue_Quill <- filter(Ed_cat_dog_data, descriptiv == "Blue Quill")
cat_num_Blue_Quill <- sum(Blue_Quill$PET_TYPE == "Cat")
dog_num_Blue_Quill <- sum(Blue_Quill$PET_TYPE == "Dog")
ifelse(dog_num_Blue_Quill > cat_num_Blue_Quill, Blue_Quill$Cat_or_Dog <- "Dog", Blue_Quill$Cat_or_Dog <- "Cat")

#Blue_Quill_Estates
Blue_Quill_Estates <- filter(Ed_cat_dog_data, descriptiv == "Blue Quill Estates")
cat_num_Blue_Quill_Estates <- sum(Blue_Quill_Estates$PET_TYPE == "Cat")
dog_num_Blue_Quill_Estates <- sum(Blue_Quill_Estates$PET_TYPE == "Dog")
ifelse(dog_num_Blue_Quill_Estates > cat_num_Blue_Quill_Estates, Blue_Quill_Estates$Cat_or_Dog <- "Dog", Blue_Quill_Estates$Cat_or_Dog <- "Cat")

#Bonnie_Doon
Bonnie_Doon <- filter(Ed_cat_dog_data, descriptiv == "Bonnie Doon")
cat_num_Bonnie_Doon <- sum(Bonnie_Doon$PET_TYPE == "Cat")
dog_num_Bonnie_Doon <- sum(Bonnie_Doon$PET_TYPE == "Dog")
ifelse(dog_num_Bonnie_Doon > cat_num_Bonnie_Doon, Bonnie_Doon$Cat_or_Dog <- "Dog", Bonnie_Doon$Cat_or_Dog <- "Cat")

#Boyle_Street
Boyle_Street <- filter(Ed_cat_dog_data, descriptiv == "Boyle Street")
cat_num_Boyle_Street <- sum(Boyle_Street$PET_TYPE == "Cat")
dog_num_Boyle_Street <- sum(Boyle_Street$PET_TYPE == "Dog")
ifelse(dog_num_Boyle_Street > cat_num_Boyle_Street, Boyle_Street$Cat_or_Dog <- "Dog", Boyle_Street$Cat_or_Dog <- "Cat")

#Brander_Gardens
Brander_Gardens <- filter(Ed_cat_dog_data, descriptiv == "Brander Gardens")
cat_num_Brander_Gardens <- sum(Brander_Gardens$PET_TYPE == "Cat")
dog_num_Brander_Gardens <- sum(Brander_Gardens$PET_TYPE == "Dog")
ifelse(dog_num_Brander_Gardens > cat_num_Brander_Gardens, Brander_Gardens$Cat_or_Dog <- "Dog", Brander_Gardens$Cat_or_Dog <- "Cat")

#Breckenridge_Greens
Breckenridge_Greens <- filter(Ed_cat_dog_data, descriptiv == "Breckenridge Greens")
cat_num_Breckenridge_Greens <- sum(Breckenridge_Greens$PET_TYPE == "Cat")
dog_num_Breckenridge_Greens <- sum(Breckenridge_Greens$PET_TYPE == "Dog")
ifelse(dog_num_Breckenridge_Greens > cat_num_Breckenridge_Greens, Breckenridge_Greens$Cat_or_Dog <- "Dog", Breckenridge_Greens$Cat_or_Dog <- "Cat")

#Brintnell
Brintnell <- filter(Ed_cat_dog_data, descriptiv == "Brintnell")
cat_num_Brintnell <- sum(Brintnell$PET_TYPE == "Cat")
dog_num_Brintnell <- sum(Brintnell$PET_TYPE == "Dog")
ifelse(dog_num_Brintnell > cat_num_Brintnell, Brintnell$Cat_or_Dog <- "Dog", Brintnell$Cat_or_Dog <- "Cat")

#Britannia_Youngstown
Britannia_Youngstown <- filter(Ed_cat_dog_data, descriptiv == "Britannia Youngstown")
cat_num_Britannia_Youngstown <- sum(Britannia_Youngstown$PET_TYPE == "Cat")
dog_num_Britannia_Youngstown <- sum(Britannia_Youngstown$PET_TYPE == "Dog")
ifelse(dog_num_Britannia_Youngstown > cat_num_Britannia_Youngstown, Britannia_Youngstown$Cat_or_Dog <- "Dog", Britannia_Youngstown$Cat_or_Dog <- "Cat")

#Brookside
Brookside <- filter(Ed_cat_dog_data, descriptiv == "Brookside")
cat_num_Brookside <- sum(Brookside$PET_TYPE == "Cat")
dog_num_Brookside <- sum(Brookside$PET_TYPE == "Dog")
ifelse(dog_num_Brookside > cat_num_Brookside, Brookside$Cat_or_Dog <- "Dog", Brookside$Cat_or_Dog <- "Cat")

#Brown_Industrial
Brown_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Brown Industrial")
cat_num_Brown_Industrial <- sum(Brown_Industrial$PET_TYPE == "Cat")
dog_num_Brown_Industrial <- sum(Brown_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Brown_Industrial > cat_num_Brown_Industrial, Brown_Industrial$Cat_or_Dog <- "Dog", Brown_Industrial$Cat_or_Dog <- "Cat")

#Bulyea_Heights
Bulyea_Heights <- filter(Ed_cat_dog_data, descriptiv == "Bulyea Heights")
cat_num_Bulyea_Heights <- sum(Bulyea_Heights$PET_TYPE == "Cat")
dog_num_Bulyea_Heights <- sum(Bulyea_Heights$PET_TYPE == "Dog")
ifelse(dog_num_Bulyea_Heights > cat_num_Bulyea_Heights, Bulyea_Heights$Cat_or_Dog <- "Dog", Bulyea_Heights$Cat_or_Dog <- "Cat")

#Caernarvon
Caernarvon <- filter(Ed_cat_dog_data, descriptiv == "Caernarvon")
cat_num_Caernarvon <- sum(Caernarvon$PET_TYPE == "Cat")
dog_num_Caernarvon <- sum(Caernarvon$PET_TYPE == "Dog")
ifelse(dog_num_Caernarvon > cat_num_Caernarvon, Caernarvon$Cat_or_Dog <- "Dog", Caernarvon$Cat_or_Dog <- "Cat")

#Calder
Calder <- filter(Ed_cat_dog_data, descriptiv == "Calder")
cat_num_Calder <- sum(Calder$PET_TYPE == "Cat")
dog_num_Calder <- sum(Calder$PET_TYPE == "Dog")
ifelse(dog_num_Calder > cat_num_Calder, Calder$Cat_or_Dog <- "Dog", Calder$Cat_or_Dog <- "Cat")

#Calgary_Trail_North
Calgary_Trail_North <- filter(Ed_cat_dog_data, descriptiv == "Calgary Trail North")
cat_num_Calgary_Trail_North <- sum(Calgary_Trail_North$PET_TYPE == "Cat")
dog_num_Calgary_Trail_North <- sum(Calgary_Trail_North$PET_TYPE == "Dog")
ifelse(dog_num_Calgary_Trail_North > cat_num_Calgary_Trail_North, Calgary_Trail_North$Cat_or_Dog <- "Dog", Calgary_Trail_North$Cat_or_Dog <- "Cat")

#Callaghan
Callaghan <- filter(Ed_cat_dog_data, descriptiv == "Callaghan")
cat_num_Callaghan <- sum(Callaghan$PET_TYPE == "Cat")
dog_num_Callaghan <- sum(Callaghan$PET_TYPE == "Dog")
ifelse(dog_num_Callaghan > cat_num_Callaghan, Callaghan$Cat_or_Dog <- "Dog", Callaghan$Cat_or_Dog <- "Cat")

#Callingwood_North
Callingwood_North <- filter(Ed_cat_dog_data, descriptiv == "Callingwood North")
cat_num_Callingwood_North <- sum(Callingwood_North$PET_TYPE == "Cat")
dog_num_Callingwood_North <- sum(Callingwood_North$PET_TYPE == "Dog")
ifelse(dog_num_Callingwood_North > cat_num_Callingwood_North, Callingwood_North$Cat_or_Dog <- "Dog", Callingwood_North$Cat_or_Dog <- "Cat")

#Callingwood_South
Callingwood_South <- filter(Ed_cat_dog_data, descriptiv == "Callingwood South")
cat_num_Callingwood_South <- sum(Callingwood_South$PET_TYPE == "Cat")
dog_num_Callingwood_South <- sum(Callingwood_South$PET_TYPE == "Dog")
ifelse(dog_num_Callingwood_South > cat_num_Callingwood_South, Callingwood_South$Cat_or_Dog <- "Dog", Callingwood_South$Cat_or_Dog <- "Cat")

#Cameron_Heights
Cameron_Heights <- filter(Ed_cat_dog_data, descriptiv == "Cameron Heights")
cat_num_Cameron_Heights <- sum(Cameron_Heights$PET_TYPE == "Cat")
dog_num_Cameron_Heights <- sum(Cameron_Heights$PET_TYPE == "Dog")
ifelse(dog_num_Cameron_Heights > cat_num_Cameron_Heights, Cameron_Heights$Cat_or_Dog <- "Dog", Cameron_Heights$Cat_or_Dog <- "Cat")

#Canon_Ridge
Canon_Ridge <- filter(Ed_cat_dog_data, descriptiv == "Canon Ridge")
cat_num_Canon_Ridge <- sum(Canon_Ridge$PET_TYPE == "Cat")
dog_num_Canon_Ridge <- sum(Canon_Ridge$PET_TYPE == "Dog")
ifelse(dog_num_Canon_Ridge > cat_num_Canon_Ridge, Canon_Ridge$Cat_or_Dog <- "Dog", Canon_Ridge$Cat_or_Dog <- "Cat")

#Canora
Canora <- filter(Ed_cat_dog_data, descriptiv == "Canora")
cat_num_Canora <- sum(Canora$PET_TYPE == "Cat")
dog_num_Canora <- sum(Canora$PET_TYPE == "Dog")
ifelse(dog_num_Canora > cat_num_Canora, Canora$Cat_or_Dog <- "Dog", Canora$Cat_or_Dog <- "Cat")

#Canossa
Canossa <- filter(Ed_cat_dog_data, descriptiv == "Canossa")
cat_num_Canossa <- sum(Canossa$PET_TYPE == "Cat")
dog_num_Canossa <- sum(Canossa$PET_TYPE == "Dog")
ifelse(dog_num_Canossa > cat_num_Canossa, Canossa$Cat_or_Dog <- "Dog", Canossa$Cat_or_Dog <- "Cat")

#Capilano
Capilano <- filter(Ed_cat_dog_data, descriptiv == "Capilano")
cat_num_Capilano <- sum(Capilano$PET_TYPE == "Cat")
dog_num_Capilano <- sum(Capilano$PET_TYPE == "Dog")
ifelse(dog_num_Capilano > cat_num_Capilano, Capilano$Cat_or_Dog <- "Dog", Capilano$Cat_or_Dog <- "Cat")

#Carlton
Carlton <- filter(Ed_cat_dog_data, descriptiv == "Carlton")
cat_num_Carlton <- sum(Carlton$PET_TYPE == "Cat")
dog_num_Carlton <- sum(Carlton$PET_TYPE == "Dog")
ifelse(dog_num_Carlton > cat_num_Carlton, Carlton$Cat_or_Dog <- "Dog", Carlton$Cat_or_Dog <- "Cat")

#Carleton_Square_Industrial
Carleton_Square_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Carleton Square Industrial")
cat_num_Carleton_Square_Industrial <- sum(Carleton_Square_Industrial$PET_TYPE == "Cat")
dog_num_Carleton_Square_Industrial <- sum(Carleton_Square_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Carleton_Square_Industrial > cat_num_Carleton_Square_Industrial, Carleton_Square_Industrial$Cat_or_Dog <- "Dog", Carleton_Square_Industrial$Cat_or_Dog <- "Cat")

#Carlisle
Carlisle <- filter(Ed_cat_dog_data, descriptiv == "Carlisle")
cat_num_Carlisle <- sum(Carlisle$PET_TYPE == "Cat")
dog_num_Carlisle <- sum(Carlisle$PET_TYPE == "Dog")
ifelse(dog_num_Carlisle > cat_num_Carlisle, Carlisle$Cat_or_Dog <- "Dog", Carlisle$Cat_or_Dog <- "Cat")

#Carter_Crest
Carter_Crest <- filter(Ed_cat_dog_data, descriptiv == "Carter Crest")
cat_num_Carter_Crest <- sum(Carter_Crest$PET_TYPE == "Cat")
dog_num_Carter_Crest <- sum(Carter_Crest$PET_TYPE == "Dog")
ifelse(dog_num_Carter_Crest > cat_num_Carter_Crest, Carter_Crest$Cat_or_Dog <- "Dog", Carter_Crest$Cat_or_Dog <- "Cat")

#Cashman
Cashman <- filter(Ed_cat_dog_data, descriptiv == "Cashman")
cat_num_Cashman <- sum(Cashman$PET_TYPE == "Cat")
dog_num_Cashman <- sum(Cashman$PET_TYPE == "Dog")
ifelse(dog_num_Cashman > cat_num_Cashman, Cashman$Cat_or_Dog <- "Dog", Cashman$Cat_or_Dog <- "Cat")

#Casselman
Casselman <- filter(Ed_cat_dog_data, descriptiv == "Casselman")
cat_num_Casselman <- sum(Casselman$PET_TYPE == "Cat")
dog_num_Casselman <- sum(Casselman$PET_TYPE == "Dog")
ifelse(dog_num_Casselman > cat_num_Casselman, Casselman$Cat_or_Dog <- "Dog", Casselman$Cat_or_Dog <- "Cat")

#Cavanagh
Cavanagh <- filter(Ed_cat_dog_data, descriptiv == "Cavanagh")
cat_num_Cavanagh <- sum(Cavanagh$PET_TYPE == "Cat")
dog_num_Cavanagh <- sum(Cavanagh$PET_TYPE == "Dog")
ifelse(dog_num_Cavanagh > cat_num_Cavanagh, Cavanagh$Cat_or_Dog <- "Dog", Cavanagh$Cat_or_Dog <- "Cat")

#Central_McDougall
Central_McDougall <- filter(Ed_cat_dog_data, descriptiv == "Central McDougall")
cat_num_Central_McDougall <- sum(Central_McDougall$PET_TYPE == "Cat")
dog_num_Central_McDougall <- sum(Central_McDougall$PET_TYPE == "Dog")
ifelse(dog_num_Central_McDougall > cat_num_Central_McDougall, Central_McDougall$Cat_or_Dog <- "Dog", Central_McDougall$Cat_or_Dog <- "Cat")

#Chambery
Chambery <- filter(Ed_cat_dog_data, descriptiv == "Chambery")
cat_num_Chambery <- sum(Chambery$PET_TYPE == "Cat")
dog_num_Chambery <- sum(Chambery$PET_TYPE == "Dog")
ifelse(dog_num_Chambery > cat_num_Chambery, Chambery$Cat_or_Dog <- "Dog", Chambery$Cat_or_Dog <- "Cat")

#Chappelle_Area
Chappelle_Area <- filter(Ed_cat_dog_data, descriptiv == "Chappelle Area")
cat_num_Chappelle_Area <- sum(Chappelle_Area$PET_TYPE == "Cat")
dog_num_Chappelle_Area <- sum(Chappelle_Area$PET_TYPE == "Dog")
ifelse(dog_num_Chappelle_Area > cat_num_Chappelle_Area, Chappelle_Area$Cat_or_Dog <- "Dog", Chappelle_Area$Cat_or_Dog <- "Cat")

#Charlesworth
Charlesworth <- filter(Ed_cat_dog_data, descriptiv == "Charlesworth")
cat_num_Charlesworth <- sum(Charlesworth$PET_TYPE == "Cat")
dog_num_Charlesworth <- sum(Charlesworth$PET_TYPE == "Dog")
ifelse(dog_num_Charlesworth > cat_num_Charlesworth, Charlesworth$Cat_or_Dog <- "Dog", Charlesworth$Cat_or_Dog <- "Cat")

#Clareview_Town_Centre
Clareview_Town_Centre <- filter(Ed_cat_dog_data, descriptiv == "Clareview Town Centre")
cat_num_Clareview_Town_Centre <- sum(Clareview_Town_Centre$PET_TYPE == "Cat")
dog_num_Clareview_Town_Centre <- sum(Clareview_Town_Centre$PET_TYPE == "Dog")
ifelse(dog_num_Clareview_Town_Centre > cat_num_Clareview_Town_Centre, Clareview_Town_Centre$Cat_or_Dog <- "Dog", Clareview_Town_Centre$Cat_or_Dog <- "Cat")

#Clover_Bar_Area
Clover_Bar_Area <- filter(Ed_cat_dog_data, descriptiv == "Clover Bar Area")
cat_num_Clover_Bar_Area <- sum(Clover_Bar_Area$PET_TYPE == "Cat")
dog_num_Clover_Bar_Area <- sum(Clover_Bar_Area$PET_TYPE == "Dog")
ifelse(dog_num_Clover_Bar_Area > cat_num_Clover_Bar_Area, Clover_Bar_Area$Cat_or_Dog <- "Dog", Clover_Bar_Area$Cat_or_Dog <- "Cat")

#Cloverdale
Cloverdale <- filter(Ed_cat_dog_data, descriptiv == "Cloverdale")
cat_num_Cloverdale <- sum(Cloverdale$PET_TYPE == "Cat")
dog_num_Cloverdale <- sum(Cloverdale$PET_TYPE == "Dog")
ifelse(dog_num_Cloverdale > cat_num_Cloverdale, Cloverdale$Cat_or_Dog <- "Dog", Cloverdale$Cat_or_Dog <- "Cat")

#Coronet_Addition_Industrial
Coronet_Addition_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Coronet Addition Industrial")
cat_num_Coronet_Addition_Industrial <- sum(Coronet_Addition_Industrial$PET_TYPE == "Cat")
dog_num_Coronet_Addition_Industrial <- sum(Coronet_Addition_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Coronet_Addition_Industrial > cat_num_Coronet_Addition_Industrial, Coronet_Addition_Industrial$Cat_or_Dog <- "Dog", Coronet_Addition_Industrial$Cat_or_Dog <- "Cat")

#Coronet_Industrial
Coronet_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Coronet Industrial")
cat_num_Coronet_Industrial <- sum(Coronet_Industrial$PET_TYPE == "Cat")
dog_num_Coronet_Industrial <- sum(Coronet_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Coronet_Industrial > cat_num_Coronet_Industrial, Coronet_Industrial$Cat_or_Dog <- "Dog", Coronet_Industrial$Cat_or_Dog <- "Cat")

#CPR_Irvine
CPR_Irvine <- filter(Ed_cat_dog_data, descriptiv == "CPR_Irvine")
cat_num_CPR_Irvine <- sum(CPR_Irvine$PET_TYPE == "Cat")
dog_num_CPR_Irvine <- sum(CPR_Irvine$PET_TYPE == "Dog")
ifelse(dog_num_CPR_Irvine > cat_num_CPR_Irvine, CPR_Irvine$Cat_or_Dog <- "Dog", CPR_Irvine$Cat_or_Dog <- "Cat")

#Crawford_Plains
Crawford_Plains <- filter(Ed_cat_dog_data, descriptiv == "Crawford Plains")
cat_num_Crawford_Plains <- sum(Crawford_Plains$PET_TYPE == "Cat")
dog_num_Crawford_Plains <- sum(Crawford_Plains$PET_TYPE == "Dog")
ifelse(dog_num_Crawford_Plains > cat_num_Crawford_Plains, Crawford_Plains$Cat_or_Dog <- "Dog", Crawford_Plains$Cat_or_Dog <- "Cat")

#Crestwood
Crestwood <- filter(Ed_cat_dog_data, descriptiv == "Crestwood")
cat_num_Crestwood <- sum(Crestwood$PET_TYPE == "Cat")
dog_num_Crestwood <- sum(Crestwood$PET_TYPE == "Dog")
ifelse(dog_num_Crestwood > cat_num_Crestwood, Crestwood$Cat_or_Dog <- "Dog", Crestwood$Cat_or_Dog <- "Cat")

#Cromdale
Cromdale <- filter(Ed_cat_dog_data, descriptiv == "Cromdale")
cat_num_Cromdale <- sum(Cromdale$PET_TYPE == "Cat")
dog_num_Cromdale <- sum(Cromdale$PET_TYPE == "Dog")
ifelse(dog_num_Cromdale > cat_num_Cromdale, Cromdale$Cat_or_Dog <- "Dog", Cromdale$Cat_or_Dog <- "Cat")

#Crystallina_Nera_West
Crystallina_Nera_West <- filter(Ed_cat_dog_data, descriptiv == "Crystallina Nera West")
cat_num_Crystallina_Nera_West <- sum(Crystallina_Nera_West$PET_TYPE == "Cat")
dog_num_Crystallina_Nera_West <- sum(Crystallina_Nera_West$PET_TYPE == "Dog")
ifelse(dog_num_Crystallina_Nera_West > cat_num_Crystallina_Nera_West, Crystallina_Nera_West$Cat_or_Dog <- "Dog", Crystallina_Nera_West$Cat_or_Dog <- "Cat")

#Cumberland
Cumberland <- filter(Ed_cat_dog_data, descriptiv == "Cumberland")
cat_num_Cumberland <- sum(Cumberland$PET_TYPE == "Cat")
dog_num_Cumberland <- sum(Cumberland$PET_TYPE == "Dog")
ifelse(dog_num_Cumberland > cat_num_Cumberland, Cumberland$Cat_or_Dog <- "Dog", Cumberland$Cat_or_Dog <- "Cat")

#Cy_Becker
Cy_Becker <- filter(Ed_cat_dog_data, descriptiv == "Cy Becker")
cat_num_Cy_Becker <- sum(Cy_Becker$PET_TYPE == "Cat")
dog_num_Cy_Becker <- sum(Cy_Becker$PET_TYPE == "Dog")
ifelse(dog_num_Cy_Becker > cat_num_Cy_Becker, Cy_Becker$Cat_or_Dog <- "Dog", Cy_Becker$Cat_or_Dog <- "Cat")

#Daly_Grove
Daly_Grove <- filter(Ed_cat_dog_data, descriptiv == "Daly Grove")
cat_num_Daly_Grove <- sum(Daly_Grove$PET_TYPE == "Cat")
dog_num_Daly_Grove <- sum(Daly_Grove$PET_TYPE == "Dog")
ifelse(dog_num_Daly_Grove > cat_num_Daly_Grove, Daly_Grove$Cat_or_Dog <- "Dog", Daly_Grove$Cat_or_Dog <- "Cat")

#Davies_Industrial_East
Davies_Industrial_East <- filter(Ed_cat_dog_data, descriptiv == "Davies Industrial East")
cat_num_Davies_Industrial_East <- sum(Davies_Industrial_East$PET_TYPE == "Cat")
dog_num_Davies_Industrial_East <- sum(Davies_Industrial_East$PET_TYPE == "Dog")
ifelse(dog_num_Davies_Industrial_East > cat_num_Davies_Industrial_East, Davies_Industrial_East$Cat_or_Dog <- "Dog", Davies_Industrial_East$Cat_or_Dog <- "Cat")

#Dechene
Dechene <- filter(Ed_cat_dog_data, descriptiv == "Dechene")
cat_num_Dechene <- sum(Dechene$PET_TYPE == "Cat")
dog_num_Dechene <- sum(Dechene$PET_TYPE == "Dog")
ifelse(dog_num_Dechene > cat_num_Dechene, Dechene$Cat_or_Dog <- "Dog", Dechene$Cat_or_Dog <- "Cat")

#Decoteau
Decoteau <- filter(Ed_cat_dog_data, descriptiv == "Decoteau")
cat_num_Decoteau <- sum(Decoteau$PET_TYPE == "Cat")
dog_num_Decoteau <- sum(Decoteau$PET_TYPE == "Dog")
ifelse(dog_num_Decoteau > cat_num_Decoteau, Decoteau$Cat_or_Dog <- "Dog", Decoteau$Cat_or_Dog <- "Cat")

#Decoteau_North
Decoteau_North <- filter(Ed_cat_dog_data, descriptiv == "Decoteau North")
cat_num_Decoteau_North <- sum(Decoteau_North$PET_TYPE == "Cat")
dog_num_Decoteau_North <- sum(Decoteau_North$PET_TYPE == "Dog")
ifelse(dog_num_Decoteau_North > cat_num_Decoteau_North, Decoteau_North$Cat_or_Dog <- "Dog", Decoteau_North$Cat_or_Dog <- "Cat")

#Delton
Delton <- filter(Ed_cat_dog_data, descriptiv == "Delton")
cat_num_Delton <- sum(Delton$PET_TYPE == "Cat")
dog_num_Delton <- sum(Delton$PET_TYPE == "Dog")
ifelse(dog_num_Delton > cat_num_Delton, Delton$Cat_or_Dog <- "Dog", Delton$Cat_or_Dog <- "Cat")

#Delwood
Delwood <- filter(Ed_cat_dog_data, descriptiv == "Delwood")
cat_num_Delwood <- sum(Delwood$PET_TYPE == "Cat")
dog_num_Delwood <- sum(Delwood$PET_TYPE == "Dog")
ifelse(dog_num_Delwood > cat_num_Delwood, Delwood$Cat_or_Dog <- "Dog", Delwood$Cat_or_Dog <- "Cat")

#Desrochers_Area
Desrochers_Area <- filter(Ed_cat_dog_data, descriptiv == "Desrochers Area")
cat_num_Desrochers_Area <- sum(Desrochers_Area$PET_TYPE == "Cat")
dog_num_Desrochers_Area <- sum(Desrochers_Area$PET_TYPE == "Dog")
ifelse(dog_num_Desrochers_Area > cat_num_Desrochers_Area, Desrochers_Area$Cat_or_Dog <- "Dog", Desrochers_Area$Cat_or_Dog <- "Cat")

#Donsdale
Donsdale <- filter(Ed_cat_dog_data, descriptiv == "Donsdale")
cat_num_Donsdale <- sum(Donsdale$PET_TYPE == "Cat")
dog_num_Donsdale <- sum(Donsdale$PET_TYPE == "Dog")
ifelse(dog_num_Donsdale > cat_num_Donsdale, Donsdale$Cat_or_Dog <- "Dog", Donsdale$Cat_or_Dog <- "Cat")

#Dovercourt
Dovercourt <- filter(Ed_cat_dog_data, descriptiv == "Dovercourt")
cat_num_Dovercourt <- sum(Dovercourt$PET_TYPE == "Cat")
dog_num_Dovercourt <- sum(Dovercourt$PET_TYPE == "Dog")
ifelse(dog_num_Dovercourt > cat_num_Dovercourt, Dovercourt$Cat_or_Dog <- "Dog", Dovercourt$Cat_or_Dog <- "Cat")

#Downtown
Downtown <- filter(Ed_cat_dog_data, descriptiv == "Downtown")
cat_num_Downtown <- sum(Downtown$PET_TYPE == "Cat")
dog_num_Downtown <- sum(Downtown$PET_TYPE == "Dog")
ifelse(dog_num_Downtown > cat_num_Downtown, Downtown$Cat_or_Dog <- "Dog", Downtown$Cat_or_Dog <- "Cat")

#Duggan
Duggan <- filter(Ed_cat_dog_data, descriptiv == "Duggan")
cat_num_Duggan <- sum(Duggan$PET_TYPE == "Cat")
dog_num_Duggan <- sum(Duggan$PET_TYPE == "Dog")
ifelse(dog_num_Duggan > cat_num_Duggan, Duggan$Cat_or_Dog <- "Dog", Duggan$Cat_or_Dog <- "Cat")

#Dunluce
Dunluce <- filter(Ed_cat_dog_data, descriptiv == "Dunluce")
cat_num_Dunluce <- sum(Dunluce$PET_TYPE == "Cat")
dog_num_Dunluce <- sum(Dunluce$PET_TYPE == "Dog")
ifelse(dog_num_Dunluce > cat_num_Dunluce, Dunluce$Cat_or_Dog <- "Dog", Dunluce$Cat_or_Dog <- "Cat")

#Eastwood
Eastwood <- filter(Ed_cat_dog_data, descriptiv == "Eastwood")
cat_num_Eastwood <- sum(Eastwood$PET_TYPE == "Cat")
dog_num_Eastwood <- sum(Eastwood$PET_TYPE == "Dog")
ifelse(dog_num_Eastwood > cat_num_Eastwood, Eastwood$Cat_or_Dog <- "Dog", Eastwood$Cat_or_Dog <- "Cat")

#Eaux_Claires
Eaux_Claires <- filter(Ed_cat_dog_data, descriptiv == "Eaux Claires")
cat_num_Eaux_Claires <- sum(Eaux_Claires$PET_TYPE == "Cat")
dog_num_Eaux_Claires <- sum(Eaux_Claires$PET_TYPE == "Dog")
ifelse(dog_num_Eaux_Claires > cat_num_Eaux_Claires, Eaux_Claires$Cat_or_Dog <- "Dog", Eaux_Claires$Cat_or_Dog <- "Cat")

#Ebbers
Ebbers <- filter(Ed_cat_dog_data, descriptiv == "Ebbers")
cat_num_Ebbers <- sum(Ebbers$PET_TYPE == "Cat")
dog_num_Ebbers <- sum(Ebbers$PET_TYPE == "Dog")
ifelse(dog_num_Ebbers > cat_num_Ebbers, Ebbers$Cat_or_Dog <- "Dog", Ebbers$Cat_or_Dog <- "Cat")

#Edgemont
Edgemont <- filter(Ed_cat_dog_data, descriptiv == "Edgemont")
cat_num_Edgemont <- sum(Edgemont$PET_TYPE == "Cat")
dog_num_Edgemont <- sum(Edgemont$PET_TYPE == "Dog")
ifelse(dog_num_Edgemont > cat_num_Edgemont, Edgemont$Cat_or_Dog <- "Dog", Edgemont$Cat_or_Dog <- "Cat")

#Edmiston_Industrial
Edmiston_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Edmiston Industrial")
cat_num_Edmiston_Industrial <- sum(Edmiston_Industrial$PET_TYPE == "Cat")
dog_num_Edmiston_Industrial <- sum(Edmiston_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Edmiston_Industrial > cat_num_Edmiston_Industrial, Edmiston_Industrial$Cat_or_Dog <- "Dog", Edmiston_Industrial$Cat_or_Dog <- "Cat")

#Edmonton_Energy_And_Technology_Park
Edmonton_Energy_And_Technology_Park <- filter(Ed_cat_dog_data, descriptiv == "Edmonton Energy And Technology Park")
cat_num_Edmonton_Energy_And_Technology_Park <- sum(Edmonton_Energy_And_Technology_Park$PET_TYPE == "Cat")
dog_num_Edmonton_Energy_And_Technology_Park <- sum(Edmonton_Energy_And_Technology_Park$PET_TYPE == "Dog")
ifelse(dog_num_Edmonton_Energy_And_Technology_Park > cat_num_Edmonton_Energy_And_Technology_Park, Edmonton_Energy_And_Technology_Park$Cat_or_Dog <- "Dog", Edmonton_Energy_And_Technology_Park$Cat_or_Dog <- "Cat")

#Edmonton_South_Central
Edmonton_South_Central <- filter(Ed_cat_dog_data, descriptiv == "Edmonton South Central")
cat_num_Edmonton_South_Central <- sum(Edmonton_South_Central$PET_TYPE == "Cat")
dog_num_Edmonton_South_Central <- sum(Edmonton_South_Central$PET_TYPE == "Dog")
ifelse(dog_num_Edmonton_South_Central > cat_num_Edmonton_South_Central, Edmonton_South_Central$Cat_or_Dog <- "Dog", Edmonton_South_Central$Cat_or_Dog <- "Cat")

#Edmonton_South_Central_East
Edmonton_South_Central_East <- filter(Ed_cat_dog_data, descriptiv == "Edmonton South Central East")
cat_num_Edmonton_South_Central_East <- sum(Edmonton_South_Central_East$PET_TYPE == "Cat")
dog_num_Edmonton_South_Central_East <- sum(Edmonton_South_Central_East$PET_TYPE == "Dog")
ifelse(dog_num_Edmonton_South_Central_East > cat_num_Edmonton_South_Central_East, Edmonton_South_Central_East$Cat_or_Dog <- "Dog", Edmonton_South_Central_East$Cat_or_Dog <- "Cat")

#Edmonton_South_East
Edmonton_South_East <- filter(Ed_cat_dog_data, descriptiv == "Edmonton South East")
cat_num_Edmonton_South_East <- sum(Edmonton_South_East$PET_TYPE == "Cat")
dog_num_Edmonton_South_East <- sum(Edmonton_South_East$PET_TYPE == "Dog")
ifelse(dog_num_Edmonton_South_East > cat_num_Edmonton_South_East, Edmonton_South_East$Cat_or_Dog <- "Dog", Edmonton_South_East$Cat_or_Dog <- "Cat")

#Edmonton_South_West
Edmonton_South_West <- filter(Ed_cat_dog_data, descriptiv == "Edmonton South West")
cat_num_Edmonton_South_West <- sum(Edmonton_South_West$PET_TYPE == "Cat")
dog_num_Edmonton_South_West <- sum(Edmonton_South_West$PET_TYPE == "Dog")
ifelse(dog_num_Edmonton_South_West > cat_num_Edmonton_South_West, Edmonton_South_West$Cat_or_Dog <- "Dog", Edmonton_South_West$Cat_or_Dog <- "Cat")

#Ekota
Ekota <- filter(Ed_cat_dog_data, descriptiv == "Ekota")
cat_num_Ekota <- sum(Ekota$PET_TYPE == "Cat")
dog_num_Ekota <- sum(Ekota$PET_TYPE == "Dog")
ifelse(dog_num_Ekota > cat_num_Ekota, Ekota$Cat_or_Dog <- "Dog", Ekota$Cat_or_Dog <- "Cat")

#Ellerslie
Ellerslie <- filter(Ed_cat_dog_data, descriptiv == "Ellerslie")
cat_num_Ellerslie <- sum(Ellerslie$PET_TYPE == "Cat")
dog_num_Ellerslie <- sum(Ellerslie$PET_TYPE == "Dog")
ifelse(dog_num_Ellerslie > cat_num_Ellerslie, Ellerslie$Cat_or_Dog <- "Dog", Ellerslie$Cat_or_Dog <- "Cat")

#Elmwood
Elmwood <- filter(Ed_cat_dog_data, descriptiv == "Elmwood")
cat_num_Elmwood <- sum(Elmwood$PET_TYPE == "Cat")
dog_num_Elmwood <- sum(Elmwood$PET_TYPE == "Dog")
ifelse(dog_num_Elmwood > cat_num_Elmwood, Elmwood$Cat_or_Dog <- "Dog", Elmwood$Cat_or_Dog <- "Cat")

#Elmwood_Park
Elmwood_Park <- filter(Ed_cat_dog_data, descriptiv == "Elmwood Park")
cat_num_Elmwood_Park <- sum(Elmwood_Park$PET_TYPE == "Cat")
dog_num_Elmwood_Park <- sum(Elmwood_Park$PET_TYPE == "Dog")
ifelse(dog_num_Elmwood_Park > cat_num_Elmwood_Park, Elmwood_Park$Cat_or_Dog <- "Dog", Elmwood_Park$Cat_or_Dog <- "Cat")

#Elsinore
Elsinore <- filter(Ed_cat_dog_data, descriptiv == "Elsinore")
cat_num_Elsinore <- sum(Elsinore$PET_TYPE == "Cat")
dog_num_Elsinore <- sum(Elsinore$PET_TYPE == "Dog")
ifelse(dog_num_Elsinore > cat_num_Elsinore, Elsinore$Cat_or_Dog <- "Dog", Elsinore$Cat_or_Dog <- "Cat")

#Empire_Park
Empire_Park <- filter(Ed_cat_dog_data, descriptiv == "Empire Park")
cat_num_Empire_Park <- sum(Empire_Park$PET_TYPE == "Cat")
dog_num_Empire_Park <- sum(Empire_Park$PET_TYPE == "Dog")
ifelse(dog_num_Empire_Park > cat_num_Empire_Park, Empire_Park$Cat_or_Dog <- "Dog", Empire_Park$Cat_or_Dog <- "Cat")

#Ermineskin
Ermineskin <- filter(Ed_cat_dog_data, descriptiv == "Ermineskin")
cat_num_Ermineskin <- sum(Ermineskin$PET_TYPE == "Cat")
dog_num_Ermineskin <- sum(Ermineskin$PET_TYPE == "Dog")
ifelse(dog_num_Ermineskin > cat_num_Ermineskin, Ermineskin$Cat_or_Dog <- "Dog", Ermineskin$Cat_or_Dog <- "Cat")

#Evansdale
Evansdale <- filter(Ed_cat_dog_data, descriptiv == "Evansdale")
cat_num_Evansdale <- sum(Evansdale$PET_TYPE == "Cat")
dog_num_Evansdale <- sum(Evansdale$PET_TYPE == "Dog")
ifelse(dog_num_Evansdale > cat_num_Evansdale, Evansdale$Cat_or_Dog <- "Dog", Evansdale$Cat_or_Dog <- "Cat")

#Evergreen
Evergreen <- filter(Ed_cat_dog_data, descriptiv == "Evergreen")
cat_num_Evergreen <- sum(Evergreen$PET_TYPE == "Cat")
dog_num_Evergreen <- sum(Evergreen$PET_TYPE == "Dog")
ifelse(dog_num_Evergreen > cat_num_Evergreen, Evergreen$Cat_or_Dog <- "Dog", Evergreen$Cat_or_Dog <- "Cat")

#Falconer_Heights
Falconer_Heights <- filter(Ed_cat_dog_data, descriptiv == "Falconer Heights")
cat_num_Falconer_Heights <- sum(Falconer_Heights$PET_TYPE == "Cat")
dog_num_Falconer_Heights <- sum(Falconer_Heights$PET_TYPE == "Dog")
ifelse(dog_num_Falconer_Heights > cat_num_Falconer_Heights, Falconer_Heights$Cat_or_Dog <- "Dog", Falconer_Heights$Cat_or_Dog <- "Cat")

#Forest_Heights
Forest_Heights <- filter(Ed_cat_dog_data, descriptiv == "Forest Heights")
cat_num_Forest_Heights <- sum(Forest_Heights$PET_TYPE == "Cat")
dog_num_Forest_Heights <- sum(Forest_Heights$PET_TYPE == "Dog")
ifelse(dog_num_Forest_Heights > cat_num_Forest_Heights, Forest_Heights$Cat_or_Dog <- "Dog", Forest_Heights$Cat_or_Dog <- "Cat")

#Fraser
Fraser <- filter(Ed_cat_dog_data, descriptiv == "Fraser")
cat_num_Fraser <- sum(Fraser$PET_TYPE == "Cat")
dog_num_Fraser <- sum(Fraser$PET_TYPE == "Dog")
ifelse(dog_num_Fraser > cat_num_Fraser, Fraser$Cat_or_Dog <- "Dog", Fraser$Cat_or_Dog <- "Cat")

#Fulton_Place
Fulton_Place <- filter(Ed_cat_dog_data, descriptiv == "Fulton Place")
cat_num_Fulton_Place <- sum(Fulton_Place$PET_TYPE == "Cat")
dog_num_Fulton_Place <- sum(Fulton_Place$PET_TYPE == "Dog")
ifelse(dog_num_Fulton_Place > cat_num_Fulton_Place, Fulton_Place$Cat_or_Dog <- "Dog", Fulton_Place$Cat_or_Dog <- "Cat")

#Gariepy
Gariepy <- filter(Ed_cat_dog_data, descriptiv == "Gariepy")
cat_num_Gariepy <- sum(Gariepy$PET_TYPE == "Cat")
dog_num_Gariepy <- sum(Gariepy$PET_TYPE == "Dog")
ifelse(dog_num_Gariepy > cat_num_Gariepy, Gariepy$Cat_or_Dog <- "Dog", Gariepy$Cat_or_Dog <- "Cat")

#Garneau
Garneau <- filter(Ed_cat_dog_data, descriptiv == "Garneau")
cat_num_Garneau <- sum(Garneau$PET_TYPE == "Cat")
dog_num_Garneau <- sum(Garneau$PET_TYPE == "Dog")
ifelse(dog_num_Garneau > cat_num_Garneau, Garneau$Cat_or_Dog <- "Dog", Garneau$Cat_or_Dog <- "Cat")

#Garside_Industrial
Garside_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Garside Industrial")
cat_num_Garside_Industrial <- sum(Garside_Industrial$PET_TYPE == "Cat")
dog_num_Garside_Industrial <- sum(Garside_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Garside_Industrial > cat_num_Garside_Industrial, Garside_Industrial$Cat_or_Dog <- "Dog", Garside_Industrial$Cat_or_Dog <- "Cat")

#Glastonbury
Glastonbury <- filter(Ed_cat_dog_data, descriptiv == "Glastonbury")
cat_num_Glastonbury <- sum(Glastonbury$PET_TYPE == "Cat")
dog_num_Glastonbury <- sum(Glastonbury$PET_TYPE == "Dog")
ifelse(dog_num_Glastonbury > cat_num_Glastonbury, Glastonbury$Cat_or_Dog <- "Dog", Glastonbury$Cat_or_Dog <- "Cat")

#Glengarry
Glengarry <- filter(Ed_cat_dog_data, descriptiv == "Glengarry")
cat_num_Glengarry <- sum(Glengarry$PET_TYPE == "Cat")
dog_num_Glengarry <- sum(Glengarry$PET_TYPE == "Dog")
ifelse(dog_num_Glengarry > cat_num_Glengarry, Glengarry$Cat_or_Dog <- "Dog", Glengarry$Cat_or_Dog <- "Cat")

#Glenora
Glenora <- filter(Ed_cat_dog_data, descriptiv == "Glenora")
cat_num_Glenora <- sum(Glenora$PET_TYPE == "Cat")
dog_num_Glenora <- sum(Glenora$PET_TYPE == "Dog")
ifelse(dog_num_Glenora > cat_num_Glenora, Glenora$Cat_or_Dog <- "Dog", Glenora$Cat_or_Dog <- "Cat")

#Glenridding_Heights
Glenridding_Heights <- filter(Ed_cat_dog_data, descriptiv == "Glenridding Heights")
cat_num_Glenridding_Heights <- sum(Glenridding_Heights$PET_TYPE == "Cat")
dog_num_Glenridding_Heights <- sum(Glenridding_Heights$PET_TYPE == "Dog")
ifelse(dog_num_Glenridding_Heights > cat_num_Glenridding_Heights, Glenridding_Heights$Cat_or_Dog <- "Dog", Glenridding_Heights$Cat_or_Dog <- "Cat")

#Glenridding_Ravine
Glenridding_Ravine <- filter(Ed_cat_dog_data, descriptiv == "Glenridding Ravine")
cat_num_Glenridding_Ravine <- sum(Glenridding_Ravine$PET_TYPE == "Cat")
dog_num_Glenridding_Ravine <- sum(Glenridding_Ravine$PET_TYPE == "Dog")
ifelse(dog_num_Glenridding_Ravine > cat_num_Glenridding_Ravine, Glenridding_Ravine$Cat_or_Dog <- "Dog", Glenridding_Ravine$Cat_or_Dog <- "Cat")

#Glenwood
Glenwood <- filter(Ed_cat_dog_data, descriptiv == "Glenwood")
cat_num_Glenwood <- sum(Glenwood$PET_TYPE == "Cat")
dog_num_Glenwood <- sum(Glenwood$PET_TYPE == "Dog")
ifelse(dog_num_Glenwood > cat_num_Glenwood, Glenwood$Cat_or_Dog <- "Dog", Glenwood$Cat_or_Dog <- "Cat")

#Gold_Bar
Gold_Bar <- filter(Ed_cat_dog_data, descriptiv == "Gold Bar")
cat_num_Gold_Bar <- sum(Gold_Bar$PET_TYPE == "Cat")
dog_num_Gold_Bar <- sum(Gold_Bar$PET_TYPE == "Dog")
ifelse(dog_num_Gold_Bar > cat_num_Gold_Bar, Gold_Bar$Cat_or_Dog <- "Dog", Gold_Bar$Cat_or_Dog <- "Cat")

#Grandview_Heights
Grandview_Heights <- filter(Ed_cat_dog_data, descriptiv == "Grandview Heights")
cat_num_Grandview_Heights <- sum(Grandview_Heights$PET_TYPE == "Cat")
dog_num_Grandview_Heights <- sum(Grandview_Heights$PET_TYPE == "Dog")
ifelse(dog_num_Grandview_Heights > cat_num_Grandview_Heights, Grandview_Heights$Cat_or_Dog <- "Dog", Grandview_Heights$Cat_or_Dog <- "Cat")

#Granville
Granville <- filter(Ed_cat_dog_data, descriptiv == "Granville")
cat_num_Granville <- sum(Granville$PET_TYPE == "Cat")
dog_num_Granville <- sum(Granville$PET_TYPE == "Dog")
ifelse(dog_num_Granville > cat_num_Granville, Granville$Cat_or_Dog <- "Dog", Granville$Cat_or_Dog <- "Cat")

#Graydon_Hill
Graydon_Hill <- filter(Ed_cat_dog_data, descriptiv == "Graydon Hill")
cat_num_Graydon_Hill <- sum(Graydon_Hill$PET_TYPE == "Cat")
dog_num_Graydon_Hill <- sum(Graydon_Hill$PET_TYPE == "Dog")
ifelse(dog_num_Graydon_Hill > cat_num_Graydon_Hill, Graydon_Hill$Cat_or_Dog <- "Dog", Graydon_Hill$Cat_or_Dog <- "Cat")

#Greenfield
Greenfield <- filter(Ed_cat_dog_data, descriptiv == "Greenfield")
cat_num_Greenfield <- sum(Greenfield$PET_TYPE == "Cat")
dog_num_Greenfield <- sum(Greenfield$PET_TYPE == "Dog")
ifelse(dog_num_Greenfield > cat_num_Greenfield, Greenfield$Cat_or_Dog <- "Dog", Greenfield$Cat_or_Dog <- "Cat")

#Greenview
Greenview <- filter(Ed_cat_dog_data, descriptiv == "Greenview")
cat_num_Greenview <- sum(Greenview$PET_TYPE == "Cat")
dog_num_Greenview <- sum(Greenview$PET_TYPE == "Dog")
ifelse(dog_num_Greenview > cat_num_Greenview, Greenview$Cat_or_Dog <- "Dog", Greenview$Cat_or_Dog <- "Cat")

#Griesbach
Griesbach <- filter(Ed_cat_dog_data, descriptiv == "Griesbach")
cat_num_Griesbach <- sum(Griesbach$PET_TYPE == "Cat")
dog_num_Griesbach <- sum(Griesbach$PET_TYPE == "Dog")
ifelse(dog_num_Griesbach > cat_num_Griesbach, Griesbach$Cat_or_Dog <- "Dog", Griesbach$Cat_or_Dog <- "Cat")

#Grovenor
Grovenor <- filter(Ed_cat_dog_data, descriptiv == "Grovenor")
cat_num_Grovenor <- sum(Grovenor$PET_TYPE == "Cat")
dog_num_Grovenor <- sum(Grovenor$PET_TYPE == "Dog")
ifelse(dog_num_Grovenor > cat_num_Grovenor, Grovenor$Cat_or_Dog <- "Dog", Grovenor$Cat_or_Dog <- "Cat")

#Haddow
Haddow <- filter(Ed_cat_dog_data, descriptiv == "Haddow")
cat_num_Haddow <- sum(Haddow$PET_TYPE == "Cat")
dog_num_Haddow <- sum(Haddow$PET_TYPE == "Dog")
ifelse(dog_num_Haddow > cat_num_Haddow, Haddow$Cat_or_Dog <- "Dog", Haddow$Cat_or_Dog <- "Cat")

#Hagmann_Estate_Industrial
Hagmann_Estate_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Hagmann Estate Industrial")
cat_num_Hagmann_Estate_Industrial <- sum(Hagmann_Estate_Industrial$PET_TYPE == "Cat")
dog_num_Hagmann_Estate_Industrial <- sum(Hagmann_Estate_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Hagmann_Estate_Industrial > cat_num_Hagmann_Estate_Industrial, Hagmann_Estate_Industrial$Cat_or_Dog <- "Dog", Hagmann_Estate_Industrial$Cat_or_Dog <- "Cat")

#Hairsine
Hairsine <- filter(Ed_cat_dog_data, descriptiv == "Hairsine")
cat_num_Hairsine <- sum(Hairsine$PET_TYPE == "Cat")
dog_num_Hairsine <- sum(Hairsine$PET_TYPE == "Dog")
ifelse(dog_num_Hairsine > cat_num_Hairsine, Hairsine$Cat_or_Dog <- "Dog", Hairsine$Cat_or_Dog <- "Cat")

#Hawks_Ridge
Hawks_Ridge <- filter(Ed_cat_dog_data, descriptiv == "Hawks Ridge")
cat_num_Hawks_Ridge <- sum(Hawks_Ridge$PET_TYPE == "Cat")
dog_num_Hawks_Ridge <- sum(Hawks_Ridge$PET_TYPE == "Dog")
ifelse(dog_num_Hawks_Ridge > cat_num_Hawks_Ridge, Hawks_Ridge$Cat_or_Dog <- "Dog", Hawks_Ridge$Cat_or_Dog <- "Cat")

#Hays_Ridge_Area
Hays_Ridge_Area <- filter(Ed_cat_dog_data, descriptiv == "Hays Ridge Area")
cat_num_Hays_Ridge_Area <- sum(Hays_Ridge_Area$PET_TYPE == "Cat")
dog_num_Hays_Ridge_Area <- sum(Hays_Ridge_Area$PET_TYPE == "Dog")
ifelse(dog_num_Hays_Ridge_Area > cat_num_Hays_Ridge_Area, Hays_Ridge_Area$Cat_or_Dog <- "Dog", Hays_Ridge_Area$Cat_or_Dog <- "Cat")

#Hazeldean
Hazeldean <- filter(Ed_cat_dog_data, descriptiv == "Hazeldean")
cat_num_Hazeldean <- sum(Hazeldean$PET_TYPE == "Cat")
dog_num_Hazeldean <- sum(Hazeldean$PET_TYPE == "Dog")
ifelse(dog_num_Hazeldean > cat_num_Hazeldean, Hazeldean$Cat_or_Dog <- "Dog", Hazeldean$Cat_or_Dog <- "Cat")

#Henderson_Estates
Henderson_Estates <- filter(Ed_cat_dog_data, descriptiv == "Henderson Estates")
cat_num_Henderson_Estates <- sum(Henderson_Estates$PET_TYPE == "Cat")
dog_num_Henderson_Estates <- sum(Henderson_Estates$PET_TYPE == "Dog")
ifelse(dog_num_Henderson_Estates > cat_num_Henderson_Estates, Henderson_Estates$Cat_or_Dog <- "Dog", Henderson_Estates$Cat_or_Dog <- "Cat")

#Heritage_Valley_Town_Centre_Area
Heritage_Valley_Town_Centre_Area <- filter(Ed_cat_dog_data, descriptiv == "Heritage Valley Town Centre Area")
cat_num_Heritage_Valley_Town_Centre_Area <- sum(Heritage_Valley_Town_Centre_Area$PET_TYPE == "Cat")
dog_num_Heritage_Valley_Town_Centre_Area <- sum(Heritage_Valley_Town_Centre_Area$PET_TYPE == "Dog")
ifelse(dog_num_Heritage_Valley_Town_Centre_Area > cat_num_Heritage_Valley_Town_Centre_Area, Heritage_Valley_Town_Centre_Area$Cat_or_Dog <- "Dog", Heritage_Valley_Town_Centre_Area$Cat_or_Dog <- "Cat")

#High_Park
High_Park <- filter(Ed_cat_dog_data, descriptiv == "High Park")
cat_num_High_Park <- sum(High_Park$PET_TYPE == "Cat")
dog_num_High_Park <- sum(High_Park$PET_TYPE == "Dog")
ifelse(dog_num_High_Park > cat_num_High_Park, High_Park$Cat_or_Dog <- "Dog", High_Park$Cat_or_Dog <- "Cat")

#High_Park_Industrial
High_Park_Industrial <- filter(Ed_cat_dog_data, descriptiv == "High_Park Industrial")
cat_num_High_Park_Industrial <- sum(High_Park_Industrial$PET_TYPE == "Cat")
dog_num_High_Park_Industrial <- sum(High_Park_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_High_Park_Industrial > cat_num_High_Park_Industrial, High_Park_Industrial$Cat_or_Dog <- "Dog", High_Park_Industrial$Cat_or_Dog <- "Cat")

#Highlands
Highlands <- filter(Ed_cat_dog_data, descriptiv == "Highlands")
cat_num_Highlands <- sum(Highlands$PET_TYPE == "Cat")
dog_num_Highlands <- sum(Highlands$PET_TYPE == "Dog")
ifelse(dog_num_Highlands > cat_num_Highlands, Highlands$Cat_or_Dog <- "Dog", Highlands$Cat_or_Dog <- "Cat")

#Hillview
Hillview <- filter(Ed_cat_dog_data, descriptiv == "Hillview")
cat_num_Hillview <- sum(Hillview$PET_TYPE == "Cat")
dog_num_Hillview <- sum(Hillview$PET_TYPE == "Dog")
ifelse(dog_num_Hillview > cat_num_Hillview, Hillview$Cat_or_Dog <- "Dog", Hillview$Cat_or_Dog <- "Cat")

#Hodgson
Hodgson <- filter(Ed_cat_dog_data, descriptiv == "Hodgson")
cat_num_Hodgson <- sum(Hodgson$PET_TYPE == "Cat")
dog_num_Hodgson <- sum(Hodgson$PET_TYPE == "Dog")
ifelse(dog_num_Hodgson > cat_num_Hodgson, Hodgson$Cat_or_Dog <- "Dog", Hodgson$Cat_or_Dog <- "Cat")

#Hollick
Hollick <- filter(Ed_cat_dog_data, descriptiv == "Hollick")
cat_num_Hollick <- sum(Hollick$PET_TYPE == "Cat")
dog_num_Hollick <- sum(Hollick$PET_TYPE == "Dog")
ifelse(dog_num_Hollick > cat_num_Hollick, Hollick$Cat_or_Dog <- "Dog", Hollick$Cat_or_Dog <- "Cat")

#Hollick-Kenyon
Hollick_Kenyon <- filter(Ed_cat_dog_data, descriptiv == "Hollick-Kenyon")
cat_num_Hollick_Kenyon <- sum(Hollick_Kenyon$PET_TYPE == "Cat")
dog_num_Hollick_Kenyon <- sum(Hollick_Kenyon$PET_TYPE == "Dog")
ifelse(dog_num_Hollick_Kenyon > cat_num_Hollick_Kenyon, Hollick_Kenyon$Cat_or_Dog <- "Dog", Hollick_Kenyon$Cat_or_Dog <- "Cat")

#Holyrood
Holyrood <- filter(Ed_cat_dog_data, descriptiv == "Holyrood")
cat_num_Holyrood <- sum(Holyrood$PET_TYPE == "Cat")
dog_num_Holyrood <- sum(Holyrood$PET_TYPE == "Dog")
ifelse(dog_num_Holyrood > cat_num_Holyrood, Holyrood$Cat_or_Dog <- "Dog", Holyrood$Cat_or_Dog <- "Cat")

#Homesteader
Homesteader <- filter(Ed_cat_dog_data, descriptiv == "Homesteader")
cat_num_Homesteader <- sum(Homesteader$PET_TYPE == "Cat")
dog_num_Homesteader <- sum(Homesteader$PET_TYPE == "Dog")
ifelse(dog_num_Homesteader > cat_num_Homesteader, Homesteader$Cat_or_Dog <- "Dog", Homesteader$Cat_or_Dog <- "Cat")

#Hudson
Hudson <- filter(Ed_cat_dog_data, descriptiv == "Hudson")
cat_num_Hudson <- sum(Hudson$PET_TYPE == "Cat")
dog_num_Hudson <- sum(Hudson$PET_TYPE == "Dog")
ifelse(dog_num_Hudson > cat_num_Hudson, Hudson$Cat_or_Dog <- "Dog", Hudson$Cat_or_Dog <- "Cat")

#Huff_Bremner_Estate_Industrial
Huff_Bremner_Estate_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Huff Bremner Estate Industrial")
cat_num_Huff_Bremner_Estate_Industrial <- sum(Huff_Bremner_Estate_Industrial$PET_TYPE == "Cat")
dog_num_Huff_Bremner_Estate_Industrial <- sum(Huff_Bremner_Estate_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Huff_Bremner_Estate_Industrial > cat_num_Huff_Bremner_Estate_Industrial, Huff_Bremner_Estate_Industrial$Cat_or_Dog <- "Dog", Huff_Bremner_Estate_Industrial$Cat_or_Dog <- "Cat")

#Idylwylde
Idylwylde <- filter(Ed_cat_dog_data, descriptiv == "Idylwylde")
cat_num_Idylwylde <- sum(Idylwylde$PET_TYPE == "Cat")
dog_num_Idylwylde <- sum(Idylwylde$PET_TYPE == "Dog")
ifelse(dog_num_Idylwylde > cat_num_Idylwylde, Idylwylde$Cat_or_Dog <- "Dog", Idylwylde$Cat_or_Dog <- "Cat")

#Industrial_Heights
Industrial_Heights <- filter(Ed_cat_dog_data, descriptiv == "Industrial Heights")
cat_num_Industrial_Heights <- sum(Industrial_Heights$PET_TYPE == "Cat")
dog_num_Industrial_Heights <- sum(Industrial_Heights$PET_TYPE == "Dog")
ifelse(dog_num_Industrial_Heights > cat_num_Industrial_Heights, Industrial_Heights$Cat_or_Dog <- "Dog", Industrial_Heights$Cat_or_Dog <- "Cat")

#Inglewood
Inglewood <- filter(Ed_cat_dog_data, descriptiv == "Inglewood")
cat_num_Inglewood <- sum(Inglewood$PET_TYPE == "Cat")
dog_num_Inglewood <- sum(Inglewood$PET_TYPE == "Dog")
ifelse(dog_num_Inglewood > cat_num_Inglewood, Inglewood$Cat_or_Dog <- "Dog", Inglewood$Cat_or_Dog <- "Cat")

#Jackson_Heights
Jackson_Heights <- filter(Ed_cat_dog_data, descriptiv == "Jackson Heights")
cat_num_Jackson_Heights <- sum(Jackson_Heights$PET_TYPE == "Cat")
dog_num_Jackson_Heights <- sum(Jackson_Heights$PET_TYPE == "Dog")
ifelse(dog_num_Jackson_Heights > cat_num_Jackson_Heights, Jackson_Heights$Cat_or_Dog <- "Dog", Jackson_Heights$Cat_or_Dog <- "Cat")

#Jamieson_Place
Jamieson_Place <- filter(Ed_cat_dog_data, descriptiv == "Jamieson Place")
cat_num_Jamieson_Place <- sum(Jamieson_Place$PET_TYPE == "Cat")
dog_num_Jamieson_Place <- sum(Jamieson_Place$PET_TYPE == "Dog")
ifelse(dog_num_Jamieson_Place > cat_num_Jamieson_Place, Jamieson_Place$Cat_or_Dog <- "Dog", Jamieson_Place$Cat_or_Dog <- "Cat")

#Jasper_Park
Jasper_Park <- filter(Ed_cat_dog_data, descriptiv == "Jasper Park")
cat_num_Jasper_Park <- sum(Jasper_Park$PET_TYPE == "Cat")
dog_num_Jasper_Park <- sum(Jasper_Park$PET_TYPE == "Dog")
ifelse(dog_num_Jasper_Park > cat_num_Jasper_Park, Jasper_Park$Cat_or_Dog <- "Dog", Jasper_Park$Cat_or_Dog <- "Cat")

#Kameyosek
Kameyosek <- filter(Ed_cat_dog_data, descriptiv == "Kameyosek")
cat_num_Kameyosek <- sum(Kameyosek$PET_TYPE == "Cat")
dog_num_Kameyosek <- sum(Kameyosek$PET_TYPE == "Dog")
ifelse(dog_num_Kameyosek > cat_num_Kameyosek, Kameyosek$Cat_or_Dog <- "Dog", Kameyosek$Cat_or_Dog <- "Cat")

#Keheewin
Keheewin <- filter(Ed_cat_dog_data, descriptiv == "Keheewin")
cat_num_Keheewin <- sum(Keheewin$PET_TYPE == "Cat")
dog_num_Keheewin <- sum(Keheewin$PET_TYPE == "Dog")
ifelse(dog_num_Keheewin > cat_num_Keheewin, Keheewin$Cat_or_Dog <- "Dog", Keheewin$Cat_or_Dog <- "Cat")

#Kenilworth
Kenilworth <- filter(Ed_cat_dog_data, descriptiv == "Kenilworth")
cat_num_Kenilworth <- sum(Kenilworth$PET_TYPE == "Cat")
dog_num_Kenilworth <- sum(Kenilworth$PET_TYPE == "Dog")
ifelse(dog_num_Kenilworth > cat_num_Kenilworth, Kenilworth$Cat_or_Dog <- "Dog", Kenilworth$Cat_or_Dog <- "Cat")

#Kennedale_Industrial
Kennedale_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Kennedale Industrial")
cat_num_Kennedale_Industrial <- sum(Kennedale_Industrial$PET_TYPE == "Cat")
dog_num_Kennedale_Industrial <- sum(Kennedale_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Kennedale_Industrial > cat_num_Kennedale_Industrial, Kennedale_Industrial$Cat_or_Dog <- "Dog", Kennedale_Industrial$Cat_or_Dog <- "Cat")

#Kensington
Kensington <- filter(Ed_cat_dog_data, descriptiv == "Kensington")
cat_num_Kensington <- sum(Kensington$PET_TYPE == "Cat")
dog_num_Kensington <- sum(Kensington$PET_TYPE == "Dog")
ifelse(dog_num_Kensington > cat_num_Kensington, Kensington$Cat_or_Dog <- "Dog", Kensington$Cat_or_Dog <- "Cat")

#Kernohan
Kernohan <- filter(Ed_cat_dog_data, descriptiv == "Kernohan")
cat_num_Kernohan <- sum(Kernohan$PET_TYPE == "Cat")
dog_num_Kernohan <- sum(Kernohan$PET_TYPE == "Dog")
ifelse(dog_num_Kernohan > cat_num_Kernohan, Kernohan$Cat_or_Dog <- "Dog", Kernohan$Cat_or_Dog <- "Cat")

#Keswick_Area
Keswick_Area <- filter(Ed_cat_dog_data, descriptiv == "Keswick Area")
cat_num_Keswick_Area <- sum(Keswick_Area$PET_TYPE == "Cat")
dog_num_Keswick_Area <- sum(Keswick_Area$PET_TYPE == "Dog")
ifelse(dog_num_Keswick_Area > cat_num_Keswick_Area, Keswick_Area$Cat_or_Dog <- "Dog", Keswick_Area$Cat_or_Dog <- "Cat")

#Kildare
Kildare <- filter(Ed_cat_dog_data, descriptiv == "Kildare")
cat_num_Kildare <- sum(Kildare$PET_TYPE == "Cat")
dog_num_Kildare <- sum(Kildare$PET_TYPE == "Dog")
ifelse(dog_num_Kildare > cat_num_Kildare, Kildare$Cat_or_Dog <- "Dog", Kildare$Cat_or_Dog <- "Cat")

#Kilkenny
Kilkenny <- filter(Ed_cat_dog_data, descriptiv == "Kilkenny")
cat_num_Kilkenny <- sum(Kilkenny$PET_TYPE == "Cat")
dog_num_Kilkenny <- sum(Kilkenny$PET_TYPE == "Dog")
ifelse(dog_num_Kilkenny > cat_num_Kilkenny, Kilkenny$Cat_or_Dog <- "Dog", Kilkenny$Cat_or_Dog <- "Cat")

#Killarney
Killarney <- filter(Ed_cat_dog_data, descriptiv == "Killarney")
cat_num_Killarney <- sum(Killarney$PET_TYPE == "Cat")
dog_num_Killarney <- sum(Killarney$PET_TYPE == "Dog")
ifelse(dog_num_Killarney > cat_num_Killarney, Killarney$Cat_or_Dog <- "Dog", Killarney$Cat_or_Dog <- "Cat")

#King_Edward_Park
King_Edward_Park <- filter(Ed_cat_dog_data, descriptiv == "King Edward Park")
cat_num_King_Edward_Park <- sum(King_Edward_Park$PET_TYPE == "Cat")
dog_num_King_Edward_Park <- sum(King_Edward_Park$PET_TYPE == "Dog")
ifelse(dog_num_King_Edward_Park > cat_num_King_Edward_Park, King_Edward_Park$Cat_or_Dog <- "Dog", King_Edward_Park$Cat_or_Dog <- "Cat")

#Kinglet_Gardens
Kinglet_Gardens <- filter(Ed_cat_dog_data, descriptiv == "Kinglet Gardens")
cat_num_Kinglet_Gardens <- sum(Kinglet_Gardens$PET_TYPE == "Cat")
dog_num_Kinglet_Gardens <- sum(Kinglet_Gardens$PET_TYPE == "Dog")
ifelse(dog_num_Kinglet_Gardens > cat_num_Kinglet_Gardens, Kinglet_Gardens$Cat_or_Dog <- "Dog", Kinglet_Gardens$Cat_or_Dog <- "Cat")

#Kiniski_Gardens
Kiniski_Gardens <- filter(Ed_cat_dog_data, descriptiv == "Kiniski Gardens")
cat_num_Kiniski_Gardens <- sum(Kiniski_Gardens$PET_TYPE == "Cat")
dog_num_Kiniski_Gardens <- sum(Kiniski_Gardens$PET_TYPE == "Dog")
ifelse(dog_num_Kiniski_Gardens > cat_num_Kiniski_Gardens, Kiniski_Gardens$Cat_or_Dog <- "Dog", Kiniski_Gardens$Cat_or_Dog <- "Cat")

#Kinokamau_Plains_Area
Kinokamau_Plains_Area <- filter(Ed_cat_dog_data, descriptiv == "Kinokamau Plains Area")
cat_num_Kinokamau_Plains_Area <- sum(Kinokamau_Plains_Area$PET_TYPE == "Cat")
dog_num_Kinokamau_Plains_Area <- sum(Kinokamau_Plains_Area$PET_TYPE == "Dog")
ifelse(dog_num_Kinokamau_Plains_Area > cat_num_Kinokamau_Plains_Area, Kinokamau_Plains_Area$Cat_or_Dog <- "Dog", Kinokamau_Plains_Area$Cat_or_Dog <- "Cat")

#Kirkness
Kirkness <- filter(Ed_cat_dog_data, descriptiv == "Kirkness")
cat_num_Kirkness <- sum(Kirkness$PET_TYPE == "Cat")
dog_num_Kirkness <- sum(Kirkness$PET_TYPE == "Dog")
ifelse(dog_num_Kirkness > cat_num_Kirkness, Kirkness$Cat_or_Dog <- "Dog", Kirkness$Cat_or_Dog <- "Cat")

#Klarvatten
Klarvatten <- filter(Ed_cat_dog_data, descriptiv == "Klarvatten")
cat_num_Klarvatten <- sum(Klarvatten$PET_TYPE == "Cat")
dog_num_Klarvatten <- sum(Klarvatten$PET_TYPE == "Dog")
ifelse(dog_num_Klarvatten > cat_num_Klarvatten, Klarvatten$Cat_or_Dog <- "Dog", Klarvatten$Cat_or_Dog <- "Cat")

#La_Perle
La_Perle <- filter(Ed_cat_dog_data, descriptiv == "La Perle")
cat_num_La_Perle <- sum(La_Perle$PET_TYPE == "Cat")
dog_num_La_Perle <- sum(La_Perle$PET_TYPE == "Dog")
ifelse(dog_num_La_Perle > cat_num_La_Perle, La_Perle$Cat_or_Dog <- "Dog", La_Perle$Cat_or_Dog <- "Cat")

#Lago_Lindo
Lago_Lindo <- filter(Ed_cat_dog_data, descriptiv == "Lago Lindo")
cat_num_Lago_Lindo <- sum(Lago_Lindo$PET_TYPE == "Cat")
dog_num_Lago_Lindo <- sum(Lago_Lindo$PET_TYPE == "Dog")
ifelse(dog_num_Lago_Lindo > cat_num_Lago_Lindo, Lago_Lindo$Cat_or_Dog <- "Dog", Lago_Lindo$Cat_or_Dog <- "Cat")

#Lansdowne
Lansdowne <- filter(Ed_cat_dog_data, descriptiv == "Lansdowne")
cat_num_Lansdowne <- sum(Lansdowne$PET_TYPE == "Cat")
dog_num_Lansdowne <- sum(Lansdowne$PET_TYPE == "Dog")
ifelse(dog_num_Lansdowne > cat_num_Lansdowne, Lansdowne$Cat_or_Dog <- "Dog", Lansdowne$Cat_or_Dog <- "Cat")

#Larkspur
Larkspur <- filter(Ed_cat_dog_data, descriptiv == "Larkspur")
cat_num_Larkspur <- sum(Larkspur$PET_TYPE == "Cat")
dog_num_Larkspur <- sum(Larkspur$PET_TYPE == "Dog")
ifelse(dog_num_Larkspur > cat_num_Larkspur, Larkspur$Cat_or_Dog <- "Dog", Larkspur$Cat_or_Dog <- "Cat")

#Lauderdale
Lauderdale <- filter(Ed_cat_dog_data, descriptiv == "Lauderdale")
cat_num_Lauderdale <- sum(Lauderdale$PET_TYPE == "Cat")
dog_num_Lauderdale <- sum(Lauderdale$PET_TYPE == "Dog")
ifelse(dog_num_Lauderdale > cat_num_Lauderdale, Lauderdale$Cat_or_Dog <- "Dog", Lauderdale$Cat_or_Dog <- "Cat")

#Laurel
Laurel <- filter(Ed_cat_dog_data, descriptiv == "Laurel")
cat_num_Laurel <- sum(Laurel$PET_TYPE == "Cat")
dog_num_Laurel <- sum(Laurel$PET_TYPE == "Dog")
ifelse(dog_num_Laurel > cat_num_Laurel, Laurel$Cat_or_Dog <- "Dog", Laurel$Cat_or_Dog <- "Cat")

#Laurier_Heights
Laurier_Heights <- filter(Ed_cat_dog_data, descriptiv == "Laurier Heights")
cat_num_Laurier_Heights <- sum(Laurier_Heights$PET_TYPE == "Cat")
dog_num_Laurier_Heights <- sum(Laurier_Heights$PET_TYPE == "Dog")
ifelse(dog_num_Laurier_Heights > cat_num_Laurier_Heights, Laurier_Heights$Cat_or_Dog <- "Dog", Laurier_Heights$Cat_or_Dog <- "Cat")

#Laurier_Heights
Laurier_Heights <- filter(Ed_cat_dog_data, descriptiv == "Laurier Heights")
cat_num_Laurier_Heights <- sum(Laurier_Heights$PET_TYPE == "Cat")
dog_num_Laurier_Heights <- sum(Laurier_Heights$PET_TYPE == "Dog")
ifelse(dog_num_Laurier_Heights > cat_num_Laurier_Heights, Laurier_Heights$Cat_or_Dog <- "Dog", Laurier_Heights$Cat_or_Dog <- "Cat")

#Lee_Ridge
Lee_Ridge <- filter(Ed_cat_dog_data, descriptiv == "Lee Ridge")
cat_num_Lee_Ridge <- sum(Lee_Ridge$PET_TYPE == "Cat")
dog_num_Lee_Ridge <- sum(Lee_Ridge$PET_TYPE == "Dog")
ifelse(dog_num_Lee_Ridge > cat_num_Lee_Ridge, Lee_Ridge$Cat_or_Dog <- "Dog", Lee_Ridge$Cat_or_Dog <- "Cat")


#Leger
Leger <- filter(Ed_cat_dog_data, descriptiv == "Leger")
cat_num_Leger <- sum(Leger$PET_TYPE == "Cat")
dog_num_Leger <- sum(Leger$PET_TYPE == "Dog")
ifelse(dog_num_Leger > cat_num_Leger, Leger$Cat_or_Dog <- "Dog", Leger$Cat_or_Dog <- "Cat")

#Lendrum_Place
Lendrum_Place <- filter(Ed_cat_dog_data, descriptiv == "Lendrum Place")
cat_num_Lendrum_Place <- sum(Lendrum_Place$PET_TYPE == "Cat")
dog_num_Lendrum_Place <- sum(Lendrum_Place$PET_TYPE == "Dog")
ifelse(dog_num_Lendrum_Place > cat_num_Lendrum_Place, Lendrum_Place$Cat_or_Dog <- "Dog", Lendrum_Place$Cat_or_Dog <- "Cat")

#Lewis_Farms_Industrial
Lewis_Farms_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Lewis Farms Industrial")
cat_num_Lewis_Farms_Industrial <- sum(Lewis_Farms_Industrial$PET_TYPE == "Cat")
dog_num_Lewis_Farms_Industrial <- sum(Lewis_Farms_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Lewis_Farms_Industrial > cat_num_Lewis_Farms_Industrial, Lewis_Farms_Industrial$Cat_or_Dog <- "Dog", Lewis_Farms_Industrial$Cat_or_Dog <- "Cat")

#Lorelei
Lorelei <- filter(Ed_cat_dog_data, descriptiv == "Lorelei")
cat_num_Lorelei <- sum(Lorelei$PET_TYPE == "Cat")
dog_num_Lorelei <- sum(Lorelei$PET_TYPE == "Dog")
ifelse(dog_num_Lorelei > cat_num_Lorelei, Lorelei$Cat_or_Dog <- "Dog", Lorelei$Cat_or_Dog <- "Cat")

#Lymburn
Lymburn <- filter(Ed_cat_dog_data, descriptiv == "Lymburn")
cat_num_Lymburn <- sum(Lymburn$PET_TYPE == "Cat")
dog_num_Lymburn <- sum(Lymburn$PET_TYPE == "Dog")
ifelse(dog_num_Lymburn > cat_num_Lymburn, Lymburn$Cat_or_Dog <- "Dog", Lymburn$Cat_or_Dog <- "Cat")

#Lynnwood
Lynnwood <- filter(Ed_cat_dog_data, descriptiv == "Lynnwood")
cat_num_Lynnwood <- sum(Lynnwood$PET_TYPE == "Cat")
dog_num_Lynnwood <- sum(Lynnwood$PET_TYPE == "Dog")
ifelse(dog_num_Lynnwood > cat_num_Lynnwood, Lynnwood$Cat_or_Dog <- "Dog", Lynnwood$Cat_or_Dog <- "Cat")

#MacEwan
MacEwan <- filter(Ed_cat_dog_data, descriptiv == "MacEwan")
cat_num_MacEwan <- sum(MacEwan$PET_TYPE == "Cat")
dog_num_MacEwan <- sum(MacEwan$PET_TYPE == "Dog")
ifelse(dog_num_MacEwan > cat_num_MacEwan, MacEwan$Cat_or_Dog <- "Dog", MacEwan$Cat_or_Dog <- "Cat")

#Mactaggart
Mactaggart <- filter(Ed_cat_dog_data, descriptiv == "Mactaggart")
cat_num_Mactaggart <- sum(Mactaggart$PET_TYPE == "Cat")
dog_num_Mactaggart <- sum(Mactaggart$PET_TYPE == "Dog")
ifelse(dog_num_Mactaggart > cat_num_Mactaggart, Mactaggart$Cat_or_Dog <- "Dog", Mactaggart$Cat_or_Dog <- "Cat")

#Magrath_Heights
Magrath_Heights <- filter(Ed_cat_dog_data, descriptiv == "Magrath Heights")
cat_num_Magrath_Heights <- sum(Magrath_Heights$PET_TYPE == "Cat")
dog_num_Magrath_Heights <- sum(Magrath_Heights$PET_TYPE == "Dog")
ifelse(dog_num_Magrath_Heights > cat_num_Magrath_Heights, Magrath_Heights$Cat_or_Dog <- "Dog", Magrath_Heights$Cat_or_Dog <- "Cat")

#Malmo_Plains
Malmo_Plains <- filter(Ed_cat_dog_data, descriptiv == "Malmo Plains")
cat_num_Malmo_Plains <- sum(Malmo_Plains$PET_TYPE == "Cat")
dog_num_Malmo_Plains <- sum(Malmo_Plains$PET_TYPE == "Dog")
ifelse(dog_num_Malmo_Plains > cat_num_Malmo_Plains, Malmo_Plains$Cat_or_Dog <- "Dog", Malmo_Plains$Cat_or_Dog <- "Cat")

#Maple
Maple <- filter(Ed_cat_dog_data, descriptiv == "Maple")
cat_num_Maple <- sum(Maple$PET_TYPE == "Cat")
dog_num_Maple <- sum(Maple$PET_TYPE == "Dog")
ifelse(dog_num_Maple > cat_num_Maple, Maple$Cat_or_Dog <- "Dog", Maple$Cat_or_Dog <- "Cat")

#Maple_Ridge
Maple_Ridge <- filter(Ed_cat_dog_data, descriptiv == "Maple Ridge")
cat_num_Maple_Ridge <- sum(Maple_Ridge$PET_TYPE == "Cat")
dog_num_Maple_Ridge <- sum(Maple_Ridge$PET_TYPE == "Dog")
ifelse(dog_num_Maple_Ridge > cat_num_Maple_Ridge, Maple_Ridge$Cat_or_Dog <- "Dog", Maple_Ridge$Cat_or_Dog <- "Cat")

#Maple_Ridge_Industrial
Maple_Ridge_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Maple Ridge Industrial")
cat_num_Maple_Ridge_Industrial <- sum(Maple_Ridge_Industrial$PET_TYPE == "Cat")
dog_num_Maple_Ridge_Industrial <- sum(Maple_Ridge_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Maple_Ridge_Industrial > cat_num_Maple_Ridge_Industrial, Maple_Ridge_Industrial$Cat_or_Dog <- "Dog", Maple_Ridge_Industrial$Cat_or_Dog <- "Cat")

#Marquis
Marquis <- filter(Ed_cat_dog_data, descriptiv == "Marquis")
cat_num_Marquis <- sum(Marquis$PET_TYPE == "Cat")
dog_num_Marquis <- sum(Marquis$PET_TYPE == "Dog")
ifelse(dog_num_Marquis > cat_num_Marquis, Marquis$Cat_or_Dog <- "Dog", Marquis$Cat_or_Dog <- "Cat")

#Matt_Berry
Matt_Berry <- filter(Ed_cat_dog_data, descriptiv == "Matt_Berry")
cat_num_Matt_Berry <- sum(Matt_Berry$PET_TYPE == "Cat")
dog_num_Matt_Berry <- sum(Matt_Berry$PET_TYPE == "Dog")
ifelse(dog_num_Matt_Berry > cat_num_Matt_Berry, Matt_Berry$Cat_or_Dog <- "Dog", Matt_Berry$Cat_or_Dog <- "Cat")

#Mayfield
Mayfield <- filter(Ed_cat_dog_data, descriptiv == "Mayfield")
cat_num_Mayfield <- sum(Mayfield$PET_TYPE == "Cat")
dog_num_Mayfield <- sum(Mayfield$PET_TYPE == "Dog")
ifelse(dog_num_Mayfield > cat_num_Mayfield, Mayfield$Cat_or_Dog <- "Dog", Mayfield$Cat_or_Dog <- "Cat")

#Mayliewan
Mayliewan <- filter(Ed_cat_dog_data, descriptiv == "Mayliewan")
cat_num_Mayliewan <- sum(Mayliewan$PET_TYPE == "Cat")
dog_num_Mayliewan <- sum(Mayliewan$PET_TYPE == "Dog")
ifelse(dog_num_Mayliewan > cat_num_Mayliewan, Mayliewan$Cat_or_Dog <- "Dog", Mayliewan$Cat_or_Dog <- "Cat")

#McCauley
McCauley <- filter(Ed_cat_dog_data, descriptiv == "McCauley")
cat_num_McCauley <- sum(McCauley$PET_TYPE == "Cat")
dog_num_McCauley <- sum(McCauley$PET_TYPE == "Dog")
ifelse(dog_num_McCauley > cat_num_McCauley, McCauley$Cat_or_Dog <- "Dog", McCauley$Cat_or_Dog <- "Cat")

#McConachie_Area
McConachie_Area <- filter(Ed_cat_dog_data, descriptiv == "McConachie Area")
cat_num_McConachie_Area <- sum(McConachie_Area$PET_TYPE == "Cat")
dog_num_McConachie_Area <- sum(McConachie_Area$PET_TYPE == "Dog")
ifelse(dog_num_McConachie_Area > cat_num_McConachie_Area, McConachie_Area$Cat_or_Dog <- "Dog", McConachie_Area$Cat_or_Dog <- "Cat")

#McIntyre_Industrial
McIntyre_Industrial <- filter(Ed_cat_dog_data, descriptiv == "McIntyre Industrial")
cat_num_McIntyre_Industrial <- sum(McIntyre_Industrial$PET_TYPE == "Cat")
dog_num_McIntyre_Industrial <- sum(McIntyre_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_McIntyre_Industrial > cat_num_McIntyre_Industrial, McIntyre_Industrial$Cat_or_Dog <- "Dog", McIntyre_Industrial$Cat_or_Dog <- "Cat")

#McKernan
McKernan <- filter(Ed_cat_dog_data, descriptiv == "McKernan")
cat_num_McKernan <- sum(McKernan$PET_TYPE == "Cat")
dog_num_McKernan <- sum(McKernan$PET_TYPE == "Dog")
ifelse(dog_num_McKernan > cat_num_McKernan, McKernan$Cat_or_Dog <- "Dog", McKernan$Cat_or_Dog <- "Cat")

#McLeod
McLeod <- filter(Ed_cat_dog_data, descriptiv == "McLeod")
cat_num_McLeod <- sum(McLeod$PET_TYPE == "Cat")
dog_num_McLeod <- sum(McLeod$PET_TYPE == "Dog")
ifelse(dog_num_McLeod > cat_num_McLeod, McLeod$Cat_or_Dog <- "Dog", McLeod$Cat_or_Dog <- "Cat")

#McQueen
McQueen <- filter(Ed_cat_dog_data, descriptiv == "McQueen")
cat_num_McQueen <- sum(McQueen$PET_TYPE == "Cat")
dog_num_McQueen <- sum(McQueen$PET_TYPE == "Dog")
ifelse(dog_num_McQueen > cat_num_McQueen, McQueen$Cat_or_Dog <- "Dog", McQueen$Cat_or_Dog <- "Cat")

#Meadowlark_Park
Meadowlark_Park <- filter(Ed_cat_dog_data, descriptiv == "Meadowlark_Park")
cat_num_Meadowlark_Park <- sum(Meadowlark_Park$PET_TYPE == "Cat")
dog_num_Meadowlark_Park <- sum(Meadowlark_Park$PET_TYPE == "Dog")
ifelse(dog_num_Meadowlark_Park > cat_num_Meadowlark_Park, Meadowlark_Park$Cat_or_Dog <- "Dog", Meadowlark_Park$Cat_or_Dog <- "Cat")

#Meltwater
Meltwater <- filter(Ed_cat_dog_data, descriptiv == "Meltwater")
cat_num_Meltwater <- sum(Meltwater$PET_TYPE == "Cat")
dog_num_Meltwater <- sum(Meltwater$PET_TYPE == "Dog")
ifelse(dog_num_Meltwater > cat_num_Meltwater, Meltwater$Cat_or_Dog <- "Dog", Meltwater$Cat_or_Dog <- "Cat")

#Menisa
Menisa <- filter(Ed_cat_dog_data, descriptiv == "Menisa")
cat_num_Menisa <- sum(Menisa$PET_TYPE == "Cat")
dog_num_Menisa <- sum(Menisa$PET_TYPE == "Dog")
ifelse(dog_num_Menisa > cat_num_Menisa, Menisa$Cat_or_Dog <- "Dog", Menisa$Cat_or_Dog <- "Cat")

#Meyokumin
Meyokumin <- filter(Ed_cat_dog_data, descriptiv == "Meyokumin")
cat_num_Meyokumin <- sum(Meyokumin$PET_TYPE == "Cat")
dog_num_Meyokumin <- sum(Meyokumin$PET_TYPE == "Dog")
ifelse(dog_num_Meyokumin > cat_num_Meyokumin, Meyokumin$Cat_or_Dog <- "Dog", Meyokumin$Cat_or_Dog <- "Cat")

#Meyonohk
Meyonohk <- filter(Ed_cat_dog_data, descriptiv == "Meyonohk")
cat_num_Meyonohk <- sum(Meyonohk$PET_TYPE == "Cat")
dog_num_Meyonohk <- sum(Meyonohk$PET_TYPE == "Dog")
ifelse(dog_num_Meyonohk > cat_num_Meyonohk, Meyonohk$Cat_or_Dog <- "Dog", Meyonohk$Cat_or_Dog <- "Cat")

#Michaels_Park
Michaels_Park <- filter(Ed_cat_dog_data, descriptiv == "Michaels Park")
cat_num_Michaels_Park <- sum(Michaels_Park$PET_TYPE == "Cat")
dog_num_Michaels_Park <- sum(Michaels_Park$PET_TYPE == "Dog")
ifelse(dog_num_Michaels_Park > cat_num_Michaels_Park, Michaels_Park$Cat_or_Dog <- "Dog", Michaels_Park$Cat_or_Dog <- "Cat")

#Mill_Creek_Ravine_North
Mill_Creek_Ravine_North <- filter(Ed_cat_dog_data, descriptiv == "Mill Creek Ravine North")
cat_num_Mill_Creek_Ravine_North <- sum(Mill_Creek_Ravine_North$PET_TYPE == "Cat")
dog_num_Mill_Creek_Ravine_North <- sum(Mill_Creek_Ravine_North$PET_TYPE == "Dog")
ifelse(dog_num_Mill_Creek_Ravine_North > cat_num_Mill_Creek_Ravine_North, Mill_Creek_Ravine_North$Cat_or_Dog <- "Dog", Mill_Creek_Ravine_North$Cat_or_Dog <- "Cat")

#Mill_Woods_Town_Centre
Mill_Woods_Town_Centre <- filter(Ed_cat_dog_data, descriptiv == "Mill Woods Town Centre")
cat_num_Mill_Woods_Town_Centre <- sum(Mill_Woods_Town_Centre$PET_TYPE == "Cat")
dog_num_Mill_Woods_Town_Centre <- sum(Mill_Woods_Town_Centre$PET_TYPE == "Dog")
ifelse(dog_num_Mill_Woods_Town_Centre > cat_num_Mill_Woods_Town_Centre, Mill_Woods_Town_Centre$Cat_or_Dog <- "Dog", Mill_Woods_Town_Centre$Cat_or_Dog <- "Cat")

#Miller
Miller <- filter(Ed_cat_dog_data, descriptiv == "Miller")
cat_num_Miller <- sum(Miller$PET_TYPE == "Cat")
dog_num_Miller <- sum(Miller$PET_TYPE == "Dog")
ifelse(dog_num_Miller > cat_num_Miller, Miller$Cat_or_Dog <- "Dog", Miller$Cat_or_Dog <- "Cat")

#Minchau
Minchau <- filter(Ed_cat_dog_data, descriptiv == "Minchau")
cat_num_Minchau <- sum(Minchau$PET_TYPE == "Cat")
dog_num_Minchau <- sum(Minchau$PET_TYPE == "Dog")
ifelse(dog_num_Minchau > cat_num_Minchau, Minchau$Cat_or_Dog <- "Dog", Minchau$Cat_or_Dog <- "Cat")

#Mistatim_Industrial
Mistatim_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Mistatim Industrial")
cat_num_Mistatim_Industrial <- sum(Mistatim_Industrial$PET_TYPE == "Cat")
dog_num_Mistatim_Industrial <- sum(Mistatim_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Mistatim_Industrial > cat_num_Mistatim_Industrial, Mistatim_Industrial$Cat_or_Dog <- "Dog", Mistatim_Industrial$Cat_or_Dog <- "Cat")

#Montrose
Montrose <- filter(Ed_cat_dog_data, descriptiv == "Montrose")
cat_num_Montrose <- sum(Montrose$PET_TYPE == "Cat")
dog_num_Montrose <- sum(Montrose$PET_TYPE == "Dog")
ifelse(dog_num_Montrose > cat_num_Montrose, Montrose$Cat_or_Dog <- "Dog", Montrose$Cat_or_Dog <- "Cat")

#Morris_Industrial
Morris_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Morris Industrial")
cat_num_Morris_Industrial <- sum(Morris_Industrial$PET_TYPE == "Cat")
dog_num_Morris_Industrial <- sum(Morris_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Morris_Industrial > cat_num_Morris_Industrial, Morris_Industrial$Cat_or_Dog <- "Dog", Morris_Industrial$Cat_or_Dog <- "Cat")

#Newton
Newton <- filter(Ed_cat_dog_data, descriptiv == "Newton")
cat_num_Newton <- sum(Newton$PET_TYPE == "Cat")
dog_num_Newton <- sum(Newton$PET_TYPE == "Dog")
ifelse(dog_num_Newton > cat_num_Newton, Newton$Cat_or_Dog <- "Dog", Newton$Cat_or_Dog <- "Cat")

#North_Glenora
North_Glenora <- filter(Ed_cat_dog_data, descriptiv == "North Glenora")
cat_num_North_Glenora <- sum(North_Glenora$PET_TYPE == "Cat")
dog_num_North_Glenora <- sum(North_Glenora$PET_TYPE == "Dog")
ifelse(dog_num_North_Glenora > cat_num_North_Glenora, North_Glenora$Cat_or_Dog <- "Dog", North_Glenora$Cat_or_Dog <- "Cat")

#Ogilvie_Ridge
Ogilvie_Ridge <- filter(Ed_cat_dog_data, descriptiv == "Ogilvie Ridge")
cat_num_Ogilvie_Ridge <- sum(Ogilvie_Ridge$PET_TYPE == "Cat")
dog_num_Ogilvie_Ridge <- sum(Ogilvie_Ridge$PET_TYPE == "Dog")
ifelse(dog_num_Ogilvie_Ridge > cat_num_Ogilvie_Ridge, Ogilvie_Ridge$Cat_or_Dog <- "Dog", Ogilvie_Ridge$Cat_or_Dog <- "Cat")

#Oleskiw
Oleskiw <- filter(Ed_cat_dog_data, descriptiv == "Oleskiw")
cat_num_Oleskiw <- sum(Oleskiw$PET_TYPE == "Cat")
dog_num_Oleskiw <- sum(Oleskiw$PET_TYPE == "Dog")
ifelse(dog_num_Oleskiw > cat_num_Oleskiw, Oleskiw$Cat_or_Dog <- "Dog", Oleskiw$Cat_or_Dog <- "Cat")

#Oliver
Oliver <- filter(Ed_cat_dog_data, descriptiv == "Oliver")
cat_num_Oliver <- sum(Oliver$PET_TYPE == "Cat")
dog_num_Oliver <- sum(Oliver$PET_TYPE == "Dog")
ifelse(dog_num_Oliver > cat_num_Oliver, Oliver$Cat_or_Dog <- "Dog", Oliver$Cat_or_Dog <- "Cat")

#Ormsby_Place
Ormsby_Place <- filter(Ed_cat_dog_data, descriptiv == "Ormsby Place")
cat_num_Ormsby_Place <- sum(Ormsby_Place$PET_TYPE == "Cat")
dog_num_Ormsby_Place <- sum(Ormsby_Place$PET_TYPE == "Dog")
ifelse(dog_num_Ormsby_Place > cat_num_Ormsby_Place, Ormsby_Place$Cat_or_Dog <- "Dog", Ormsby_Place$Cat_or_Dog <- "Cat")

#Ottewell
Ottewell <- filter(Ed_cat_dog_data, descriptiv == "Ottewell")
cat_num_Ottewell <- sum(Ottewell$PET_TYPE == "Cat")
dog_num_Ottewell <- sum(Ottewell$PET_TYPE == "Dog")
ifelse(dog_num_Ottewell > cat_num_Ottewell, Ottewell$Cat_or_Dog <- "Dog", Ottewell$Cat_or_Dog <- "Cat")

#Overlanders
Overlanders <- filter(Ed_cat_dog_data, descriptiv == "Overlanders")
cat_num_Overlanders <- sum(Overlanders$PET_TYPE == "Cat")
dog_num_Overlanders <- sum(Overlanders$PET_TYPE == "Dog")
ifelse(dog_num_Overlanders > cat_num_Overlanders, Overlanders$Cat_or_Dog <- "Dog", Overlanders$Cat_or_Dog <- "Cat")

#Oxford
Oxford <- filter(Ed_cat_dog_data, descriptiv == "Oxford")
cat_num_Oxford <- sum(Oxford$PET_TYPE == "Cat")
dog_num_Oxford <- sum(Oxford$PET_TYPE == "Dog")
ifelse(dog_num_Oxford > cat_num_Oxford, Oxford$Cat_or_Dog <- "Dog", Oxford$Cat_or_Dog <- "Cat")

#Ozerna
Ozerna <- filter(Ed_cat_dog_data, descriptiv == "Ozerna")
cat_num_Ozerna <- sum(Ozerna$PET_TYPE == "Cat")
dog_num_Ozerna <- sum(Ozerna$PET_TYPE == "Dog")
ifelse(dog_num_Ozerna > cat_num_Ozerna, Ozerna$Cat_or_Dog <- "Dog", Ozerna$Cat_or_Dog <- "Cat")

#Paisley
Paisley <- filter(Ed_cat_dog_data, descriptiv == "Paisley")
cat_num_Paisley <- sum(Paisley$PET_TYPE == "Cat")
dog_num_Paisley <- sum(Paisley$PET_TYPE == "Dog")
ifelse(dog_num_Paisley > cat_num_Paisley, Paisley$Cat_or_Dog <- "Dog", Paisley$Cat_or_Dog <- "Cat")

#Papaschase_Industrial
Papaschase_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Papaschase Industrial")
cat_num_Papaschase_Industrial <- sum(Papaschase_Industrial$PET_TYPE == "Cat")
dog_num_Papaschase_Industrial <- sum(Papaschase_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Papaschase_Industrial > cat_num_Papaschase_Industrial, Papaschase_Industrial$Cat_or_Dog <- "Dog", Papaschase_Industrial$Cat_or_Dog <- "Cat")

#Parkallen
Parkallen <- filter(Ed_cat_dog_data, descriptiv == "Parkallen")
cat_num_Parkallen <- sum(Parkallen$PET_TYPE == "Cat")
dog_num_Parkallen <- sum(Parkallen$PET_TYPE == "Dog")
ifelse(dog_num_Parkallen > cat_num_Parkallen, Parkallen$Cat_or_Dog <- "Dog", Parkallen$Cat_or_Dog <- "Cat")

#Parkdale
Parkdale <- filter(Ed_cat_dog_data, descriptiv == "Parkdale")
cat_num_Parkdale <- sum(Parkdale$PET_TYPE == "Cat")
dog_num_Parkdale <- sum(Parkdale$PET_TYPE == "Dog")
ifelse(dog_num_Parkdale > cat_num_Parkdale, Parkdale$Cat_or_Dog <- "Dog", Parkdale$Cat_or_Dog <- "Cat")

#Parkview
Parkview <- filter(Ed_cat_dog_data, descriptiv == "Parkview")
cat_num_Parkview <- sum(Parkview$PET_TYPE == "Cat")
dog_num_Parkview <- sum(Parkview$PET_TYPE == "Dog")
ifelse(dog_num_Parkview > cat_num_Parkview, Parkview$Cat_or_Dog <- "Dog", Parkview$Cat_or_Dog <- "Cat")

#Parsons_Industrial
Parsons_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Parsons Industrial")
cat_num_Parsons_Industrial <- sum(Parsons_Industrial$PET_TYPE == "Cat")
dog_num_Parsons_Industrial <- sum(Parsons_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Parsons_Industrial > cat_num_Parsons_Industrial, Parsons_Industrial$Cat_or_Dog <- "Dog", Parsons_Industrial$Cat_or_Dog <- "Cat")

#Patricia_Heights
Patricia_Heights <- filter(Ed_cat_dog_data, descriptiv == "Patricia Heights")
cat_num_Patricia_Heights <- sum(Patricia_Heights$PET_TYPE == "Cat")
dog_num_Patricia_Heights <- sum(Patricia_Heights$PET_TYPE == "Dog")
ifelse(dog_num_Patricia_Heights > cat_num_Patricia_Heights, Patricia_Heights$Cat_or_Dog <- "Dog", Patricia_Heights$Cat_or_Dog <- "Cat")

#Pembina
Pembina <- filter(Ed_cat_dog_data, descriptiv == "Pembina")
cat_num_Pembina <- sum(Pembina$PET_TYPE == "Cat")
dog_num_Pembina <- sum(Pembina$PET_TYPE == "Dog")
ifelse(dog_num_Pembina > cat_num_Pembina, Pembina$Cat_or_Dog <- "Dog", Pembina$Cat_or_Dog <- "Cat")

#Pintail_Landing
Pintail_Landing <- filter(Ed_cat_dog_data, descriptiv == "Pintail Landing")
cat_num_Pintail_Landing <- sum(Pintail_Landing$PET_TYPE == "Cat")
dog_num_Pintail_Landing <- sum(Pintail_Landing$PET_TYPE == "Dog")
ifelse(dog_num_Pintail_Landing > cat_num_Pintail_Landing, Pintail_Landing$Cat_or_Dog <- "Dog", Pintail_Landing$Cat_or_Dog <- "Cat")

#Pleasantview
Pleasantview <- filter(Ed_cat_dog_data, descriptiv == "Pleasantview")
cat_num_Pleasantview <- sum(Pleasantview$PET_TYPE == "Cat")
dog_num_Pleasantview <- sum(Pleasantview$PET_TYPE == "Dog")
ifelse(dog_num_Pleasantview > cat_num_Pleasantview, Pleasantview$Cat_or_Dog <- "Dog", Pleasantview$Cat_or_Dog <- "Cat")

#Pollard_Meadows
Pollard_Meadows <- filter(Ed_cat_dog_data, descriptiv == "Pollard Meadows")
cat_num_Pollard_Meadows <- sum(Pollard_Meadows$PET_TYPE == "Cat")
dog_num_Pollard_Meadows <- sum(Pollard_Meadows$PET_TYPE == "Dog")
ifelse(dog_num_Pollard_Meadows > cat_num_Pollard_Meadows, Pollard_Meadows$Cat_or_Dog <- "Dog", Pollard_Meadows$Cat_or_Dog <- "Cat")

#Potter_Greens
Potter_Greens <- filter(Ed_cat_dog_data, descriptiv == "Potter Greens")
cat_num_Potter_Greens <- sum(Potter_Greens$PET_TYPE == "Cat")
dog_num_Potter_Greens <- sum(Potter_Greens$PET_TYPE == "Dog")
ifelse(dog_num_Potter_Greens > cat_num_Potter_Greens, Potter_Greens$Cat_or_Dog <- "Dog", Potter_Greens$Cat_or_Dog <- "Cat")

#Prince_Charles
Prince_Charles <- filter(Ed_cat_dog_data, descriptiv == "Prince Charles")
cat_num_Prince_Charles <- sum(Prince_Charles$PET_TYPE == "Cat")
dog_num_Prince_Charles <- sum(Prince_Charles$PET_TYPE == "Dog")
ifelse(dog_num_Prince_Charles > cat_num_Prince_Charles, Prince_Charles$Cat_or_Dog <- "Dog", Prince_Charles$Cat_or_Dog <- "Cat")

#Queen_Alexandra
Queen_Alexandra <- filter(Ed_cat_dog_data, descriptiv == "Queen Alexandra")
cat_num_Queen_Alexandra <- sum(Queen_Alexandra$PET_TYPE == "Cat")
dog_num_Queen_Alexandra <- sum(Queen_Alexandra$PET_TYPE == "Dog")
ifelse(dog_num_Queen_Alexandra > cat_num_Queen_Alexandra, Queen_Alexandra$Cat_or_Dog <- "Dog", Queen_Alexandra$Cat_or_Dog <- "Cat")

#Queen_Mary_Park
Queen_Mary_Park <- filter(Ed_cat_dog_data, descriptiv == "Queen Mary Park")
cat_num_Queen_Mary_Park <- sum(Queen_Mary_Park$PET_TYPE == "Cat")
dog_num_Queen_Mary_Park <- sum(Queen_Mary_Park$PET_TYPE == "Dog")
ifelse(dog_num_Queen_Mary_Park > cat_num_Queen_Mary_Park, Queen_Mary_Park$Cat_or_Dog <- "Dog", Queen_Mary_Park$Cat_or_Dog <- "Cat")

#Quesnell_Heights
Quesnell_Heights <- filter(Ed_cat_dog_data, descriptiv == "Quesnell Heights")
cat_num_Quesnell_Heights <- sum(Quesnell_Heights$PET_TYPE == "Cat")
dog_num_Quesnell_Heights <- sum(Quesnell_Heights$PET_TYPE == "Dog")
ifelse(dog_num_Quesnell_Heights > cat_num_Quesnell_Heights, Quesnell_Heights$Cat_or_Dog <- "Dog", Quesnell_Heights$Cat_or_Dog <- "Cat")

#Rampart_Industrial
Rampart_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Rampart Industrial")
cat_num_Rampart_Industrial <- sum(Rampart_Industrial$PET_TYPE == "Cat")
dog_num_Rampart_Industrial <- sum(Rampart_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Rampart_Industrial > cat_num_Rampart_Industrial, Rampart_Industrial$Cat_or_Dog <- "Dog", Rampart_Industrial$Cat_or_Dog <- "Cat")

#Ramsay_Heights
Ramsay_Heights <- filter(Ed_cat_dog_data, descriptiv == "Ramsay Heights")
cat_num_Ramsay_Heights <- sum(Ramsay_Heights$PET_TYPE == "Cat")
dog_num_Ramsay_Heights <- sum(Ramsay_Heights$PET_TYPE == "Dog")
ifelse(dog_num_Ramsay_Heights > cat_num_Ramsay_Heights, Ramsay_Heights$Cat_or_Dog <- "Dog", Ramsay_Heights$Cat_or_Dog <- "Cat")

#Rapperswill
Rapperswill <- filter(Ed_cat_dog_data, descriptiv == "Rapperswill")
cat_num_Rapperswill <- sum(Rapperswill$PET_TYPE == "Cat")
dog_num_Rapperswill <- sum(Rapperswill$PET_TYPE == "Dog")
ifelse(dog_num_Rapperswill > cat_num_Rapperswill, Rapperswill$Cat_or_Dog <- "Dog", Rapperswill$Cat_or_Dog <- "Cat")

#Rhatigan_Ridge
Rhatigan_Ridge <- filter(Ed_cat_dog_data, descriptiv == "Rhatigan Ridge")
cat_num_Rhatigan_Ridge <- sum(Rhatigan_Ridge$PET_TYPE == "Cat")
dog_num_Rhatigan_Ridge <- sum(Rhatigan_Ridge$PET_TYPE == "Dog")
ifelse(dog_num_Rhatigan_Ridge > cat_num_Rhatigan_Ridge, Rhatigan_Ridge$Cat_or_Dog <- "Dog", Rhatigan_Ridge$Cat_or_Dog <- "Cat")

#Richfield
Richfield <- filter(Ed_cat_dog_data, descriptiv == "Richfield")
cat_num_Richfield <- sum(Richfield$PET_TYPE == "Cat")
dog_num_Richfield <- sum(Richfield$PET_TYPE == "Dog")
ifelse(dog_num_Richfield > cat_num_Richfield, Richfield$Cat_or_Dog <- "Dog", Richfield$Cat_or_Dog <- "Cat")

#Richford
Richford <- filter(Ed_cat_dog_data, descriptiv == "Richford")
cat_num_Richford <- sum(Richford$PET_TYPE == "Cat")
dog_num_Richford <- sum(Richford$PET_TYPE == "Dog")
ifelse(dog_num_Richford > cat_num_Richford, Richford$Cat_or_Dog <- "Dog", Richford$Cat_or_Dog <- "Cat")

#Rideau_Park
Rideau_Park <- filter(Ed_cat_dog_data, descriptiv == "Rideau Park")
cat_num_Rideau_Park <- sum(Rideau_Park$PET_TYPE == "Cat")
dog_num_Rideau_Park <- sum(Rideau_Park$PET_TYPE == "Dog")
ifelse(dog_num_Rideau_Park > cat_num_Rideau_Park, Rideau_Park$Cat_or_Dog <- "Dog", Rideau_Park$Cat_or_Dog <- "Cat")

#Rio_Terrace
Rio_Terrace <- filter(Ed_cat_dog_data, descriptiv == "Rio Terrace")
cat_num_Rio_Terrace <- sum(Rio_Terrace$PET_TYPE == "Cat")
dog_num_Rio_Terrace <- sum(Rio_Terrace$PET_TYPE == "Dog")
ifelse(dog_num_Rio_Terrace > cat_num_Rio_Terrace, Rio_Terrace$Cat_or_Dog <- "Dog", Rio_Terrace$Cat_or_Dog <- "Cat")

#Ritchie
Ritchie <- filter(Ed_cat_dog_data, descriptiv == "Ritchie")
cat_num_Ritchie <- sum(Ritchie$PET_TYPE == "Cat")
dog_num_Ritchie <- sum(Ritchie$PET_TYPE == "Dog")
ifelse(dog_num_Ritchie > cat_num_Ritchie, Ritchie$Cat_or_Dog <- "Dog", Ritchie$Cat_or_Dog <- "Cat")

#River_Valley_Fort_Edmonton
River_Valley_Fort_Edmonton <- filter(Ed_cat_dog_data, descriptiv == "River Valley Fort Edmonton")
cat_num_River_Valley_Fort_Edmonton <- sum(River_Valley_Fort_Edmonton$PET_TYPE == "Cat")
dog_num_River_Valley_Fort_Edmonton <- sum(River_Valley_Fort_Edmonton$PET_TYPE == "Dog")
ifelse(dog_num_River_Valley_Fort_Edmonton > cat_num_River_Valley_Fort_Edmonton, River_Valley_Fort_Edmonton$Cat_or_Dog <- "Dog", River_Valley_Fort_Edmonton$Cat_or_Dog <- "Cat")

#River_Valley_Highlands
River_Valley_Highlands <- filter(Ed_cat_dog_data, descriptiv == "River Valley Highlands")
cat_num_River_Valley_Highlands <- sum(River_Valley_Highlands$PET_TYPE == "Cat")
dog_num_River_Valley_Highlands <- sum(River_Valley_Highlands$PET_TYPE == "Dog")
ifelse(dog_num_River_Valley_Highlands > cat_num_River_Valley_Highlands, River_Valley_Highlands$Cat_or_Dog <- "Dog", River_Valley_Highlands$Cat_or_Dog <- "Cat")

#River_Valley_Kinnaird
River_Valley_Kinnaird <- filter(Ed_cat_dog_data, descriptiv == "River Valley Kinnaird")
cat_num_River_Valley_Kinnaird <- sum(River_Valley_Kinnaird$PET_TYPE == "Cat")
dog_num_River_Valley_Kinnaird <- sum(River_Valley_Kinnaird$PET_TYPE == "Dog")
ifelse(dog_num_River_Valley_Kinnaird > cat_num_River_Valley_Kinnaird, River_Valley_Kinnaird$Cat_or_Dog <- "Dog", River_Valley_Kinnaird$Cat_or_Dog <- "Cat")

#River_Valley_Laurier
River_Valley_Laurier <- filter(Ed_cat_dog_data, descriptiv == "River Valley Laurier")
cat_num_River_Valley_Laurier <- sum(River_Valley_Laurier$PET_TYPE == "Cat")
dog_num_River_Valley_Laurier <- sum(River_Valley_Laurier$PET_TYPE == "Dog")
ifelse(dog_num_River_Valley_Laurier > cat_num_River_Valley_Laurier, River_Valley_Laurier$Cat_or_Dog <- "Dog", River_Valley_Laurier$Cat_or_Dog <- "Cat")

#Riverdale
Riverdale <- filter(Ed_cat_dog_data, descriptiv == "Riverdale")
cat_num_Riverdale <- sum(Riverdale$PET_TYPE == "Cat")
dog_num_Riverdale <- sum(Riverdale$PET_TYPE == "Dog")
ifelse(dog_num_Riverdale > cat_num_Riverdale, Riverdale$Cat_or_Dog <- "Dog", Riverdale$Cat_or_Dog <- "Cat")

#River_s_Edge
River_s_Edge <- filter(Ed_cat_dog_data, descriptiv == "River's Edge")
cat_num_River_s_Edge <- sum(River_s_Edge$PET_TYPE == "Cat")
dog_num_River_s_Edge <- sum(River_s_Edge$PET_TYPE == "Dog")
ifelse(dog_num_River_s_Edge > cat_num_River_s_Edge, River_s_Edge$Cat_or_Dog <- "Dog", River_s_Edge$Cat_or_Dog <- "Cat")

# Riverview_Area
Riverview_Area <- filter(Ed_cat_dog_data, descriptiv == "Riverview Area")
cat_num_Riverview_Area <- sum(Riverview_Area$PET_TYPE == "Cat")
dog_num_Riverview_Area <- sum(Riverview_Area$PET_TYPE == "Dog")
ifelse(dog_num_Riverview_Area > cat_num_Riverview_Area, Riverview_Area$Cat_or_Dog <- "Dog", Riverview_Area$Cat_or_Dog <- "Cat")

#Rosedale_Industrial
Rosedale_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Rosedale Industrial")
cat_num_Rosedale_Industrial <- sum(Rosedale_Industrial$PET_TYPE == "Cat")
dog_num_Rosedale_Industrial <- sum(Rosedale_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Rosedale_Industrial > cat_num_Rosedale_Industrial, Rosedale_Industrial$Cat_or_Dog <- "Dog", Rosedale_Industrial$Cat_or_Dog <- "Cat")

#Rosenthal
Rosenthal <- filter(Ed_cat_dog_data, descriptiv == "Rosenthal")
cat_num_Rosenthal <- sum(Rosenthal$PET_TYPE == "Cat")
dog_num_Rosenthal <- sum(Rosenthal$PET_TYPE == "Dog")
ifelse(dog_num_Rosenthal > cat_num_Rosenthal, Rosenthal$Cat_or_Dog <- "Dog", Rosenthal$Cat_or_Dog <- "Cat")

#Rossdale
Rossdale <- filter(Ed_cat_dog_data, descriptiv == "Rossdale")
cat_num_Rossdale <- sum(Rossdale$PET_TYPE == "Cat")
dog_num_Rossdale <- sum(Rossdale$PET_TYPE == "Dog")
ifelse(dog_num_Rossdale > cat_num_Rossdale, Rossdale$Cat_or_Dog <- "Dog", Rossdale$Cat_or_Dog <- "Cat")

#Rosslyn
Rosslyn <- filter(Ed_cat_dog_data, descriptiv == "Rosslyn")
cat_num_Rosslyn <- sum(Rosslyn$PET_TYPE == "Cat")
dog_num_Rosslyn <- sum(Rosslyn$PET_TYPE == "Dog")
ifelse(dog_num_Rosslyn > cat_num_Rosslyn, Rosslyn$Cat_or_Dog <- "Dog", Rosslyn$Cat_or_Dog <- "Cat")

#Royal_Gardens
Royal_Gardens <- filter(Ed_cat_dog_data, descriptiv == "Royal Gardens")
cat_num_Royal_Gardens <- sum(Royal_Gardens$PET_TYPE == "Cat")
dog_num_Royal_Gardens <- sum(Royal_Gardens$PET_TYPE == "Dog")
ifelse(dog_num_Royal_Gardens > cat_num_Royal_Gardens, Royal_Gardens$Cat_or_Dog <- "Dog", Royal_Gardens$Cat_or_Dog <- "Cat")

#Rundle_Heights
Rundle_Heights <- filter(Ed_cat_dog_data, descriptiv == "Rundle Heights")
cat_num_Rundle_Heights <- sum(Rundle_Heights$PET_TYPE == "Cat")
dog_num_Rundle_Heights <- sum(Rundle_Heights$PET_TYPE == "Dog")
ifelse(dog_num_Rundle_Heights > cat_num_Rundle_Heights, Rundle_Heights$Cat_or_Dog <- "Dog", Rundle_Heights$Cat_or_Dog <- "Cat")

#Rural_North_East_Horse_Hill
Rural_North_East_Horse_Hill <- filter(Ed_cat_dog_data, descriptiv == "Rural North East Horse Hill")
cat_num_Rural_North_East_Horse_Hill <- sum(Rural_North_East_Horse_Hill$PET_TYPE == "Cat")
dog_num_Rural_North_East_Horse_Hill <- sum(Rural_North_East_Horse_Hill$PET_TYPE == "Dog")
ifelse(dog_num_Rural_North_East_Horse_Hill > cat_num_Rural_North_East_Horse_Hill, Rural_North_East_Horse_Hill$Cat_or_Dog <- "Dog", Rural_North_East_Horse_Hill$Cat_or_Dog <- "Cat")

#Rural_North_East_South_Sturgeon
Rural_North_East_South_Sturgeon <- filter(Ed_cat_dog_data, descriptiv == "Rural North East South Sturgeon")
cat_num_Rural_North_East_South_Sturgeon <- sum(Rural_North_East_South_Sturgeon$PET_TYPE == "Cat")
dog_num_Rural_North_East_South_Sturgeon <- sum(Rural_North_East_South_Sturgeon$PET_TYPE == "Dog")
ifelse(dog_num_Rural_North_East_South_Sturgeon > cat_num_Rural_North_East_South_Sturgeon, Rural_North_East_South_Sturgeon$Cat_or_Dog <- "Dog", Rural_North_East_South_Sturgeon$Cat_or_Dog <- "Cat")

#Rutherford
Rutherford <- filter(Ed_cat_dog_data, descriptiv == "Rutherford")
cat_num_Rutherford <- sum(Rutherford$PET_TYPE == "Cat")
dog_num_Rutherford <- sum(Rutherford$PET_TYPE == "Dog")
ifelse(dog_num_Rutherford > cat_num_Rutherford, Rutherford$Cat_or_Dog <- "Dog", Rutherford$Cat_or_Dog <- "Cat")

#Sakaw
Sakaw <- filter(Ed_cat_dog_data, descriptiv == "Sakaw")
cat_num_Sakaw <- sum(Sakaw$PET_TYPE == "Cat")
dog_num_Sakaw <- sum(Sakaw$PET_TYPE == "Dog")
ifelse(dog_num_Sakaw > cat_num_Sakaw, Sakaw$Cat_or_Dog <- "Dog", Sakaw$Cat_or_Dog <- "Cat")

#Satoo
Satoo <- filter(Ed_cat_dog_data, descriptiv == "Satoo")
cat_num_Satoo <- sum(Satoo$PET_TYPE == "Cat")
dog_num_Satoo <- sum(Satoo$PET_TYPE == "Dog")
ifelse(dog_num_Satoo > cat_num_Satoo, Satoo$Cat_or_Dog <- "Dog", Satoo$Cat_or_Dog <- "Cat")

#Schonsee
Schonsee <- filter(Ed_cat_dog_data, descriptiv == "Schonsee")
cat_num_Schonsee <- sum(Schonsee$PET_TYPE == "Cat")
dog_num_Schonsee <- sum(Schonsee$PET_TYPE == "Dog")
ifelse(dog_num_Schonsee > cat_num_Schonsee, Schonsee$Cat_or_Dog <- "Dog", Schonsee$Cat_or_Dog <- "Cat")

#Secord
Secord <- filter(Ed_cat_dog_data, descriptiv == "Secord")
cat_num_Secord <- sum(Secord$PET_TYPE == "Cat")
dog_num_Secord <- sum(Secord$PET_TYPE == "Dog")
ifelse(dog_num_Secord > cat_num_Secord, Secord$Cat_or_Dog <- "Dog", Secord$Cat_or_Dog <- "Cat")

#Sherbrooke
Sherbrooke <- filter(Ed_cat_dog_data, descriptiv == "Sherbrooke")
cat_num_Sherbrooke <- sum(Sherbrooke$PET_TYPE == "Cat")
dog_num_Sherbrooke <- sum(Sherbrooke$PET_TYPE == "Dog")
ifelse(dog_num_Sherbrooke > cat_num_Sherbrooke, Sherbrooke$Cat_or_Dog <- "Dog", Sherbrooke$Cat_or_Dog <- "Cat")

#Sherwood
Sherwood <- filter(Ed_cat_dog_data, descriptiv == "Sherwood")
cat_num_Sherwood <- sum(Sherwood$PET_TYPE == "Cat")
dog_num_Sherwood <- sum(Sherwood$PET_TYPE == "Dog")
ifelse(dog_num_Sherwood > cat_num_Sherwood, Sherwood$Cat_or_Dog <- "Dog", Sherwood$Cat_or_Dog <- "Cat")

#Sifton_Park
Sifton_Park <- filter(Ed_cat_dog_data, descriptiv == "Sifton Park")
cat_num_Sifton_Park <- sum(Sifton_Park$PET_TYPE == "Cat")
dog_num_Sifton_Park <- sum(Sifton_Park$PET_TYPE == "Dog")
ifelse(dog_num_Sifton_Park > cat_num_Sifton_Park, Sifton_Park$Cat_or_Dog <- "Dog", Sifton_Park$Cat_or_Dog <- "Cat")

#Silver_Berry
Silver_Berry <- filter(Ed_cat_dog_data, descriptiv == "Silver Berry")
cat_num_Silver_Berry <- sum(Silver_Berry$PET_TYPE == "Cat")
dog_num_Silver_Berry <- sum(Silver_Berry$PET_TYPE == "Dog")
ifelse(dog_num_Silver_Berry > cat_num_Silver_Berry, Silver_Berry$Cat_or_Dog <- "Dog", Silver_Berry$Cat_or_Dog <- "Cat")

#Skyrattler
Skyrattler <- filter(Ed_cat_dog_data, descriptiv == "Skyrattler")
cat_num_Skyrattler <- sum(Skyrattler$PET_TYPE == "Cat")
dog_num_Skyrattler <- sum(Skyrattler$PET_TYPE == "Dog")
ifelse(dog_num_Skyrattler > cat_num_Skyrattler, Skyrattler$Cat_or_Dog <- "Dog", Skyrattler$Cat_or_Dog <- "Cat")

#South_Terwillegar
South_Terwillegar <- filter(Ed_cat_dog_data, descriptiv == "South Terwillegar")
cat_num_South_Terwillegar <- sum(South_Terwillegar$PET_TYPE == "Cat")
dog_num_South_Terwillegar <- sum(South_Terwillegar$PET_TYPE == "Dog")
ifelse(dog_num_South_Terwillegar > cat_num_South_Terwillegar, South_Terwillegar$Cat_or_Dog <- "Dog", South_Terwillegar$Cat_or_Dog <- "Cat")

#Southeast_Industrial
Southeast_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Southeast Industrial")
cat_num_Southeast_Industrial <- sum(Southeast_Industrial$PET_TYPE == "Cat")
dog_num_Southeast_Industrial <- sum(Southeast_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Southeast_Industrial > cat_num_Southeast_Industrial, Southeast_Industrial$Cat_or_Dog <- "Dog", Southeast_Industrial$Cat_or_Dog <- "Cat")

#Starling
Starling <- filter(Ed_cat_dog_data, descriptiv == "Starling")
cat_num_Starling <- sum(Starling$PET_TYPE == "Cat")
dog_num_Starling <- sum(Starling$PET_TYPE == "Dog")
ifelse(dog_num_Starling > cat_num_Starling, Starling$Cat_or_Dog <- "Dog", Starling$Cat_or_Dog <- "Cat")

#Steinhauer
Steinhauer <- filter(Ed_cat_dog_data, descriptiv == "Steinhauer")
cat_num_Steinhauer <- sum(Steinhauer$PET_TYPE == "Cat")
dog_num_Steinhauer <- sum(Steinhauer$PET_TYPE == "Dog")
ifelse(dog_num_Steinhauer > cat_num_Steinhauer, Steinhauer$Cat_or_Dog <- "Dog", Steinhauer$Cat_or_Dog <- "Cat")

#Stewart_Greens
Stewart_Greens <- filter(Ed_cat_dog_data, descriptiv == "Stewart Greens")
cat_num_Stewart_Greens <- sum(Stewart_Greens$PET_TYPE == "Cat")
dog_num_Stewart_Greens <- sum(Stewart_Greens$PET_TYPE == "Dog")
ifelse(dog_num_Stewart_Greens > cat_num_Stewart_Greens, Stewart_Greens$Cat_or_Dog <- "Dog", Stewart_Greens$Cat_or_Dog <- "Cat")

#Stillwater
Stillwater <- filter(Ed_cat_dog_data, descriptiv == "Stillwater")
cat_num_Stillwater <- sum(Stillwater$PET_TYPE == "Cat")
dog_num_Stillwater <- sum(Stillwater$PET_TYPE == "Dog")
ifelse(dog_num_Stillwater > cat_num_Stillwater, Stillwater$Cat_or_Dog <- "Dog", Stillwater$Cat_or_Dog <- "Cat")

#Stone_Industrial
Stone_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Stone Industrial")
cat_num_Stone_Industrial <- sum(Stone_Industrial$PET_TYPE == "Cat")
dog_num_Stone_Industrial <- sum(Stone_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Stone_Industrial > cat_num_Stone_Industrial, Stone_Industrial$Cat_or_Dog <- "Dog", Stone_Industrial$Cat_or_Dog <- "Cat")

#Strathcona
Strathcona <- filter(Ed_cat_dog_data, descriptiv == "Strathcona")
cat_num_Strathcona <- sum(Strathcona$PET_TYPE == "Cat")
dog_num_Strathcona <- sum(Strathcona$PET_TYPE == "Dog")
ifelse(dog_num_Strathcona > cat_num_Strathcona, Strathcona$Cat_or_Dog <- "Dog", Strathcona$Cat_or_Dog <- "Cat")

#Strathcona_Industrial_Park
Strathcona_Industrial_Park <- filter(Ed_cat_dog_data, descriptiv == "Strathcona Industrial Park")
cat_num_Strathcona_Industrial_Park <- sum(Strathcona_Industrial_Park$PET_TYPE == "Cat")
dog_num_Strathcona_Industrial_Park <- sum(Strathcona_Industrial_Park$PET_TYPE == "Dog")
ifelse(dog_num_Strathcona_Industrial_Park > cat_num_Strathcona_Industrial_Park, Strathcona_Industrial_Park$Cat_or_Dog <- "Dog", Strathcona_Industrial_Park$Cat_or_Dog <- "Cat")

#Strathcona_Junction
Strathcona_Junction <- filter(Ed_cat_dog_data, descriptiv == "Strathcona Junction")
cat_num_Strathcona_Junction <- sum(Strathcona_Junction$PET_TYPE == "Cat")
dog_num_Strathcona_Junction <- sum(Strathcona_Junction$PET_TYPE == "Dog")
ifelse(dog_num_Strathcona_Junction > cat_num_Strathcona_Junction, Strathcona_Junction$Cat_or_Dog <- "Dog", Strathcona_Junction$Cat_or_Dog <- "Cat")

#Strathearn
Strathearn <- filter(Ed_cat_dog_data, descriptiv == "Strathearn")
cat_num_Strathearn <- sum(Strathearn$PET_TYPE == "Cat")
dog_num_Strathearn <- sum(Strathearn$PET_TYPE == "Dog")
ifelse(dog_num_Strathearn > cat_num_Strathearn, Strathearn$Cat_or_Dog <- "Dog", Strathearn$Cat_or_Dog <- "Cat")

#Suder_Greens
Suder_Greens <- filter(Ed_cat_dog_data, descriptiv == "Suder Greens")
cat_num_Suder_Greens <- sum(Suder_Greens$PET_TYPE == "Cat")
dog_num_Suder_Greens <- sum(Suder_Greens$PET_TYPE == "Dog")
ifelse(dog_num_Suder_Greens > cat_num_Suder_Greens, Suder_Greens$Cat_or_Dog <- "Dog", Suder_Greens$Cat_or_Dog <- "Cat")

#Summerlea
Summerlea <- filter(Ed_cat_dog_data, descriptiv == "Summerlea")
cat_num_Summerlea <- sum(Summerlea$PET_TYPE == "Cat")
dog_num_Summerlea <- sum(Summerlea$PET_TYPE == "Dog")
ifelse(dog_num_Summerlea > cat_num_Summerlea, Summerlea$Cat_or_Dog <- "Dog", Summerlea$Cat_or_Dog <- "Cat")

#Summerside
Summerside <- filter(Ed_cat_dog_data, descriptiv == "Summerside")
cat_num_Summerside <- sum(Summerside$PET_TYPE == "Cat")
dog_num_Summerside <- sum(Summerside$PET_TYPE == "Dog")
ifelse(dog_num_Summerside > cat_num_Summerside, Summerside$Cat_or_Dog <- "Dog", Summerside$Cat_or_Dog <- "Cat")

#Sweet_Grass
Sweet_Grass <- filter(Ed_cat_dog_data, descriptiv == "Sweet_Grass")
cat_num_Sweet_Grass <- sum(Sweet_Grass$PET_TYPE == "Cat")
dog_num_Sweet_Grass <- sum(Sweet_Grass$PET_TYPE == "Dog")
ifelse(dog_num_Sweet_Grass > cat_num_Sweet_Grass, Sweet_Grass$Cat_or_Dog <- "Dog", Sweet_Grass$Cat_or_Dog <- "Cat")

#Tamarack
Tamarack <- filter(Ed_cat_dog_data, descriptiv == "Tamarack")
cat_num_Tamarack <- sum(Tamarack$PET_TYPE == "Cat")
dog_num_Tamarack <- sum(Tamarack$PET_TYPE == "Dog")
ifelse(dog_num_Tamarack > cat_num_Tamarack, Tamarack$Cat_or_Dog <- "Dog", Tamarack$Cat_or_Dog <- "Cat")

#Tawa
Tawa <- filter(Ed_cat_dog_data, descriptiv == "Tawa")
cat_num_Tawa <- sum(Tawa$PET_TYPE == "Cat")
dog_num_Tawa <- sum(Tawa$PET_TYPE == "Dog")
ifelse(dog_num_Tawa > cat_num_Tawa, Tawa$Cat_or_Dog <- "Dog", Tawa$Cat_or_Dog <- "Cat")

#Terra_Losa
Terra_Losa <- filter(Ed_cat_dog_data, descriptiv == "Terra Losa")
cat_num_Terra_Losa <- sum(Terra_Losa$PET_TYPE == "Cat")
dog_num_Terra_Losa <- sum(Terra_Losa$PET_TYPE == "Dog")
ifelse(dog_num_Terra_Losa > cat_num_Terra_Losa, Terra_Losa$Cat_or_Dog <- "Dog", Terra_Losa$Cat_or_Dog <- "Cat")

#Terrace_Heights
Terrace_Heights <- filter(Ed_cat_dog_data, descriptiv == "Terrace Heights")
cat_num_Terrace_Heights <- sum(Terrace_Heights$PET_TYPE == "Cat")
dog_num_Terrace_Heights <- sum(Terrace_Heights$PET_TYPE == "Dog")
ifelse(dog_num_Terrace_Heights > cat_num_Terrace_Heights, Terrace_Heights$Cat_or_Dog <- "Dog", Terrace_Heights$Cat_or_Dog <- "Cat")

#Terwillegar_Towne
Terwillegar_Towne <- filter(Ed_cat_dog_data, descriptiv == "Terwillegar Towne")
cat_num_Terwillegar_Towne <- sum(Terwillegar_Towne$PET_TYPE == "Cat")
dog_num_Terwillegar_Towne <- sum(Terwillegar_Towne$PET_TYPE == "Dog")
ifelse(dog_num_Terwillegar_Towne > cat_num_Terwillegar_Towne, Terwillegar_Towne$Cat_or_Dog <- "Dog", Terwillegar_Towne$Cat_or_Dog <- "Cat")

#The_Hamptons
The_Hamptons <- filter(Ed_cat_dog_data, descriptiv == "The Hamptons")
cat_num_The_Hamptons <- sum(The_Hamptons$PET_TYPE == "Cat")
dog_num_The_Hamptons <- sum(The_Hamptons$PET_TYPE == "Dog")
ifelse(dog_num_The_Hamptons > cat_num_The_Hamptons, The_Hamptons$Cat_or_Dog <- "Dog", The_Hamptons$Cat_or_Dog <- "Cat")

#The_Orchards_At_Ellerslie
The_Orchards_At_Ellerslie <- filter(Ed_cat_dog_data, descriptiv == "The Orchards At Ellerslie")
cat_num_The_Orchards_At_Ellerslie <- sum(The_Orchards_At_Ellerslie$PET_TYPE == "Cat")
dog_num_The_Orchards_At_Ellerslie <- sum(The_Orchards_At_Ellerslie$PET_TYPE == "Dog")
ifelse(dog_num_The_Orchards_At_Ellerslie > cat_num_The_Orchards_At_Ellerslie, The_Orchards_At_Ellerslie$Cat_or_Dog <- "Dog", The_Orchards_At_Ellerslie$Cat_or_Dog <- "Cat")

#The_Uplands
The_Uplands <- filter(Ed_cat_dog_data, descriptiv == "The Uplands")
cat_num_The_Uplands <- sum(The_Uplands$PET_TYPE == "Cat")
dog_num_The_Uplands <- sum(The_Uplands$PET_TYPE == "Dog")
ifelse(dog_num_The_Uplands > cat_num_The_Uplands, The_Uplands$Cat_or_Dog <- "Dog", The_Uplands$Cat_or_Dog <- "Cat")

#Thorncliff
Thorncliff <- filter(Ed_cat_dog_data, descriptiv == "Thorncliff")
cat_num_Thorncliff <- sum(Thorncliff$PET_TYPE == "Cat")
dog_num_Thorncliff <- sum(Thorncliff$PET_TYPE == "Dog")
ifelse(dog_num_Thorncliff > cat_num_Thorncliff, Thorncliff$Cat_or_Dog <- "Dog", Thorncliff$Cat_or_Dog <- "Cat")

#Tipaskan
Tipaskan <- filter(Ed_cat_dog_data, descriptiv == "Tipaskan")
cat_num_Tipaskan <- sum(Tipaskan$PET_TYPE == "Cat")
dog_num_Tipaskan <- sum(Tipaskan$PET_TYPE == "Dog")
ifelse(dog_num_Tipaskan > cat_num_Tipaskan, Tipaskan$Cat_or_Dog <- "Dog", Tipaskan$Cat_or_Dog <- "Cat")

#Trumpeter_Area
Trumpeter_Area <- filter(Ed_cat_dog_data, descriptiv == "Trumpeter Area")
cat_num_Trumpeter_Area <- sum(Trumpeter_Area$PET_TYPE == "Cat")
dog_num_Trumpeter_Area <- sum(Trumpeter_Area$PET_TYPE == "Dog")
ifelse(dog_num_Trumpeter_Area > cat_num_Trumpeter_Area, Trumpeter_Area$Cat_or_Dog <- "Dog", Trumpeter_Area$Cat_or_Dog <- "Cat")

#Tweddle_Place
Tweddle_Place <- filter(Ed_cat_dog_data, descriptiv == "Tweddle Place")
cat_num_Tweddle_Place <- sum(Tweddle_Place$PET_TYPE == "Cat")
dog_num_Tweddle_Place <- sum(Tweddle_Place$PET_TYPE == "Dog")
ifelse(dog_num_Tweddle_Place > cat_num_Tweddle_Place, Tweddle_Place$Cat_or_Dog <- "Dog", Tweddle_Place$Cat_or_Dog <- "Cat")

#Twin_Brooks
Twin_Brooks <- filter(Ed_cat_dog_data, descriptiv == "Twin Brooks")
cat_num_Twin_Brooks <- sum(Twin_Brooks$PET_TYPE == "Cat")
dog_num_Twin_Brooks <- sum(Twin_Brooks$PET_TYPE == "Dog")
ifelse(dog_num_Twin_Brooks > cat_num_Twin_Brooks, Twin_Brooks$Cat_or_Dog <- "Dog", Twin_Brooks$Cat_or_Dog <- "Cat")

#University_of_Alberta_Farm
University_of_Alberta_Farm <- filter(Ed_cat_dog_data, descriptiv == "University of Alberta Farm")
cat_num_University_of_Alberta_Farm <- sum(University_of_Alberta_Farm$PET_TYPE == "Cat")
dog_num_University_of_Alberta_Farm <- sum(University_of_Alberta_Farm$PET_TYPE == "Dog")
ifelse(dog_num_University_of_Alberta_Farm > cat_num_University_of_Alberta_Farm, University_of_Alberta_Farm$Cat_or_Dog <- "Dog", University_of_Alberta_Farm$Cat_or_Dog <- "Cat")

#Virginia_Park
Virginia_Park <- filter(Ed_cat_dog_data, descriptiv == "Virginia_Park")
cat_num_Virginia_Park <- sum(Virginia_Park$PET_TYPE == "Cat")
dog_num_Virginia_Park <- sum(Virginia_Park$PET_TYPE == "Dog")
ifelse(dog_num_Virginia_Park > cat_num_Virginia_Park, Virginia_Park$Cat_or_Dog <- "Dog", Virginia_Park$Cat_or_Dog <- "Cat")

#Walker
Walker <- filter(Ed_cat_dog_data, descriptiv == "Walker")
cat_num_Walker <- sum(Walker$PET_TYPE == "Cat")
dog_num_Walker <- sum(Walker$PET_TYPE == "Dog")
ifelse(dog_num_Walker > cat_num_Walker, Walker$Cat_or_Dog <- "Dog", Walker$Cat_or_Dog <- "Cat")

#Webber_Greens
Webber_Greens <- filter(Ed_cat_dog_data, descriptiv == "Webber Greens")
cat_num_Webber_Greens <- sum(Webber_Greens$PET_TYPE == "Cat")
dog_num_Webber_Greens <- sum(Webber_Greens$PET_TYPE == "Dog")
ifelse(dog_num_Webber_Greens > cat_num_Webber_Greens, Webber_Greens$Cat_or_Dog <- "Dog", Webber_Greens$Cat_or_Dog <- "Cat")

#Wedgewood_Heights
Wedgewood_Heights <- filter(Ed_cat_dog_data, descriptiv == "Wedgewood Heights")
cat_num_Wedgewood_Heights <- sum(Wedgewood_Heights$PET_TYPE == "Cat")
dog_num_Wedgewood_Heights <- sum(Wedgewood_Heights$PET_TYPE == "Dog")
ifelse(dog_num_Wedgewood_Heights > cat_num_Wedgewood_Heights, Wedgewood_Heights$Cat_or_Dog <- "Dog", Wedgewood_Heights$Cat_or_Dog <- "Cat")

#Weinlos
Weinlos <- filter(Ed_cat_dog_data, descriptiv == "Weinlos")
cat_num_Weinlos <- sum(Weinlos$PET_TYPE == "Cat")
dog_num_Weinlos <- sum(Weinlos$PET_TYPE == "Dog")
ifelse(dog_num_Weinlos > cat_num_Weinlos, Weinlos$Cat_or_Dog <- "Dog", Weinlos$Cat_or_Dog <- "Cat")

#Weir_Industrial
Weir_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Weir Industrial")
cat_num_Weir_Industrial <- sum(Weir_Industrial$PET_TYPE == "Cat")
dog_num_Weir_Industrial <- sum(Weir_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Weir_Industrial > cat_num_Weir_Industrial, Weir_Industrial$Cat_or_Dog <- "Dog", Weir_Industrial$Cat_or_Dog <- "Cat")

#Wellington
Wellington <- filter(Ed_cat_dog_data, descriptiv == "Wellington")
cat_num_Wellington <- sum(Wellington$PET_TYPE == "Cat")
dog_num_Wellington <- sum(Wellington$PET_TYPE == "Dog")
ifelse(dog_num_Wellington > cat_num_Wellington, Wellington$Cat_or_Dog <- "Dog", Wellington$Cat_or_Dog <- "Cat")

#West_Jasper_Place
West_Jasper_Place <- filter(Ed_cat_dog_data, descriptiv == "West Jasper Place")
cat_num_West_Jasper_Place <- sum(West_Jasper_Place$PET_TYPE == "Cat")
dog_num_West_Jasper_Place <- sum(West_Jasper_Place$PET_TYPE == "Dog")
ifelse(dog_num_West_Jasper_Place > cat_num_West_Jasper_Place, West_Jasper_Place$Cat_or_Dog <- "Dog", West_Jasper_Place$Cat_or_Dog <- "Cat")

#West_Meadowlark_Park
West_Meadowlark_Park <- filter(Ed_cat_dog_data, descriptiv == "West Meadowlark Park")
cat_num_West_Meadowlark_Park <- sum(West_Meadowlark_Park$PET_TYPE == "Cat")
dog_num_West_Meadowlark_Park <- sum(West_Meadowlark_Park$PET_TYPE == "Dog")
ifelse(dog_num_West_Meadowlark_Park > cat_num_West_Meadowlark_Park, West_Meadowlark_Park$Cat_or_Dog <- "Dog", West_Meadowlark_Park$Cat_or_Dog <- "Cat")

#West_Sheffield_Industrial
West_Sheffield_Industrial <- filter(Ed_cat_dog_data, descriptiv == "West Sheffield Industrial")
cat_num_West_Sheffield_Industrial <- sum(West_Sheffield_Industrial$PET_TYPE == "Cat")
dog_num_West_Sheffield_Industrial <- sum(West_Sheffield_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_West_Sheffield_Industrial > cat_num_West_Sheffield_Industrial, West_Sheffield_Industrial$Cat_or_Dog <- "Dog", West_Sheffield_Industrial$Cat_or_Dog <- "Cat")

#Westbrook_Estates
Westbrook_Estates <- filter(Ed_cat_dog_data, descriptiv == "Westbrook Estates")
cat_num_Westbrook_Estates <- sum(Westbrook_Estates$PET_TYPE == "Cat")
dog_num_Westbrook_Estates <- sum(Westbrook_Estates$PET_TYPE == "Dog")
ifelse(dog_num_Westbrook_Estates > cat_num_Westbrook_Estates, Westbrook_Estates$Cat_or_Dog <- "Dog", Westbrook_Estates$Cat_or_Dog <- "Cat")

#Westmount
Westmount <- filter(Ed_cat_dog_data, descriptiv == "Westmount")
cat_num_Westmount <- sum(Westmount$PET_TYPE == "Cat")
dog_num_Westmount <- sum(Westmount$PET_TYPE == "Dog")
ifelse(dog_num_Westmount > cat_num_Westmount, Westmount$Cat_or_Dog <- "Dog", Westmount$Cat_or_Dog <- "Cat")

#Westridge
Westridge <- filter(Ed_cat_dog_data, descriptiv == "Westridge")
cat_num_Westridge <- sum(Westridge$PET_TYPE == "Cat")
dog_num_Westridge <- sum(Westridge$PET_TYPE == "Dog")
ifelse(dog_num_Westridge > cat_num_Westridge, Westridge$Cat_or_Dog <- "Dog", Westridge$Cat_or_Dog <- "Cat")

#Westview_Village
Westview_Village <- filter(Ed_cat_dog_data, descriptiv == "Westview Village")
cat_num_Westview_Village <- sum(Westview_Village$PET_TYPE == "Cat")
dog_num_Westview_Village <- sum(Westview_Village$PET_TYPE == "Dog")
ifelse(dog_num_Westview_Village > cat_num_Westview_Village, Westview_Village$Cat_or_Dog <- "Dog", Westview_Village$Cat_or_Dog <- "Cat")

#Wild_Rose
Wild_Rose <- filter(Ed_cat_dog_data, descriptiv == "Wild Rose")
cat_num_Wild_Rose <- sum(Wild_Rose$PET_TYPE == "Cat")
dog_num_Wild_Rose <- sum(Wild_Rose$PET_TYPE == "Dog")
ifelse(dog_num_Wild_Rose > cat_num_Wild_Rose, Wild_Rose$Cat_or_Dog <- "Dog", Wild_Rose$Cat_or_Dog <- "Cat")

#Wilson_Industrial
Wilson_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Wilson Industrial")
cat_num_Wilson_Industrial <- sum(Wilson_Industrial$PET_TYPE == "Cat")
dog_num_Wilson_Industrial <- sum(Wilson_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Wilson_Industrial > cat_num_Wilson_Industrial, Wilson_Industrial$Cat_or_Dog <- "Dog", Wilson_Industrial$Cat_or_Dog <- "Cat")

#Windermere
Windermere <- filter(Ed_cat_dog_data, descriptiv == "Windermere")
cat_num_Windermere <- sum(Windermere$PET_TYPE == "Cat")
dog_num_Windermere <- sum(Windermere$PET_TYPE == "Dog")
ifelse(dog_num_Windermere > cat_num_Windermere, Windermere$Cat_or_Dog <- "Dog", Windermere$Cat_or_Dog <- "Cat")

#Windermere_Area
Windermere_Area <- filter(Ed_cat_dog_data, descriptiv == "Windermere Area")
cat_num_Windermere_Area <- sum(Windermere_Area$PET_TYPE == "Cat")
dog_num_Windermere_Area <- sum(Windermere_Area$PET_TYPE == "Dog")
ifelse(dog_num_Windermere_Area > cat_num_Windermere_Area, Windermere_Area$Cat_or_Dog <- "Dog", Windermere_Area$Cat_or_Dog <- "Cat")

#Windsor_Park
Windsor_Park <- filter(Ed_cat_dog_data, descriptiv == "Windsor Park")
cat_num_Windsor_Park <- sum(Windsor_Park$PET_TYPE == "Cat")
dog_num_Windsor_Park <- sum(Windsor_Park$PET_TYPE == "Dog")
ifelse(dog_num_Windsor_Park > cat_num_Windsor_Park, Windsor_Park$Cat_or_Dog <- "Dog", Windsor_Park$Cat_or_Dog <- "Cat")

#Winterburn_Industrial_Area_East
Winterburn_Industrial_Area_East <- filter(Ed_cat_dog_data, descriptiv == "Winterburn Industrial Area East")
cat_num_Winterburn_Industrial_Area_East <- sum(Winterburn_Industrial_Area_East$PET_TYPE == "Cat")
dog_num_Winterburn_Industrial_Area_East <- sum(Winterburn_Industrial_Area_East$PET_TYPE == "Dog")
ifelse(dog_num_Winterburn_Industrial_Area_East > cat_num_Winterburn_Industrial_Area_East, Winterburn_Industrial_Area_East$Cat_or_Dog <- "Dog", Winterburn_Industrial_Area_East$Cat_or_Dog <- "Cat")

#Winterburn_Industrial_Area_West
Winterburn_Industrial_Area_West <- filter(Ed_cat_dog_data, descriptiv == "Winterburn Industrial Area West")
cat_num_Winterburn_Industrial_Area_West <- sum(Winterburn_Industrial_Area_West$PET_TYPE == "Cat")
dog_num_Winterburn_Industrial_Area_West <- sum(Winterburn_Industrial_Area_West$PET_TYPE == "Dog")
ifelse(dog_num_Winterburn_Industrial_Area_West > cat_num_Winterburn_Industrial_Area_West, Winterburn_Industrial_Area_West$Cat_or_Dog <- "Dog", Winterburn_Industrial_Area_West$Cat_or_Dog <- "Cat")

#Woodcroft
Woodcroft <- filter(Ed_cat_dog_data, descriptiv == "Woodcroft")
cat_num_Woodcroft <- sum(Woodcroft$PET_TYPE == "Cat")
dog_num_Woodcroft <- sum(Woodcroft$PET_TYPE == "Dog")
ifelse(dog_num_Woodcroft > cat_num_Woodcroft, Woodcroft$Cat_or_Dog <- "Dog", Woodcroft$Cat_or_Dog <- "Cat")

#Yellowhead_Corridor_East
Yellowhead_Corridor_East <- filter(Ed_cat_dog_data, descriptiv == "Yellowhead Corridor East")
cat_num_Yellowhead_Corridor_East <- sum(Yellowhead_Corridor_East$PET_TYPE == "Cat")
dog_num_Yellowhead_Corridor_East <- sum(Yellowhead_Corridor_East$PET_TYPE == "Dog")
ifelse(dog_num_Yellowhead_Corridor_East > cat_num_Yellowhead_Corridor_East, Yellowhead_Corridor_East$Cat_or_Dog <- "Dog", Yellowhead_Corridor_East$Cat_or_Dog <- "Cat")

#York
York <- filter(Ed_cat_dog_data, descriptiv == "York")
cat_num_York <- sum(York$PET_TYPE == "Cat")
dog_num_York <- sum(York$PET_TYPE == "Dog")
ifelse(dog_num_York > cat_num_York, York$Cat_or_Dog <- "Dog", York$Cat_or_Dog <- "Cat")

#Youngstown_Industrial
Youngstown_Industrial <- filter(Ed_cat_dog_data, descriptiv == "Youngstown Industrial")
cat_num_Youngstown_Industrial <- sum(Youngstown_Industrial$PET_TYPE == "Cat")
dog_num_Youngstown_Industrial <- sum(Youngstown_Industrial$PET_TYPE == "Dog")
ifelse(dog_num_Youngstown_Industrial > cat_num_Youngstown_Industrial, Youngstown_Industrial$Cat_or_Dog <- "Dog", Youngstown_Industrial$Cat_or_Dog <- "Cat")

#####################################################################################################################################
#attach the sections back to orginal map size
Ed_cat_dog_map_data <- bind_rows(Abbottsfield, Albany, Alberta_Avenue, Alberta_Park_Industrial, Aldergrove, Allard, Allendale, Ambleside,
                                 Argyll, Armstrong_Industrial, Aspen_Gardens, Aster, Athlone, Avonmore, Balwin, Bannerman, Baranow, Baturyn,
                                 Beacon_Heights, Bearspaw, Beaumaris, Belgravia, Belle_Rive, Bellevue, Belmead, Belmont, Belvedere, Bergman,
                                 Beverly_Heights, Bisset, Blackburne, Blackmud_Creek, Blatchford_Area, Blue_Quill, Blue_Quill_Estates, 
                                 Bonnie_Doon, Boyle_Street, Brander_Gardens, Breckenridge_Greens, Brintnell, Britannia_Youngstown, Brookside,
                                 Brown_Industrial, Bulyea_Heights, Caernarvon, Calder, Calgary_Trail_North, Callaghan, Callingwood_North,
                                 Callingwood_South, Cameron_Heights, Canon_Ridge, Canora, Canossa, Capilano, Carleton_Square_Industrial,
                                 Carlisle, Carlton, Carter_Crest, Cashman, Casselman, Cavanagh, Central_McDougall, Chambery, Chappelle_Area,
                                 Charlesworth, Clareview_Town_Centre, Clover_Bar_Area, Cloverdale, Coronet_Addition_Industrial, 
                                 Coronet_Industrial, CPR_Irvine, Crawford_Plains, Crestwood, Cromdale, Crystallina_Nera_West, Cumberland, 
                                 Cy_Becker, Daly_Grove, Davies_Industrial_East, Dechene, Decoteau, Decoteau_North, Delton, Delwood, 
                                 Desrochers_Area, Donsdale, Dovercourt, Downtown, Duggan, Dunluce, Eastwood, Eaux_Claires, Ebbers, Edgemont,
                                 Edmiston_Industrial, Edmonton_Energy_And_Technology_Park, Edmonton_South_Central, Edmonton_South_Central_East,
                                 Edmonton_South_East, Edmonton_South_West, Ekota, Ellerslie, Elmwood, Elmwood_Park, Elsinore, Empire_Park, 
                                 Ermineskin, Evansdale, Evergreen, Falconer_Heights, Forest_Heights, Fraser, Fulton_Place, Gariepy, Garneau,
                                 Garside_Industrial, Glastonbury, Glengarry, Glenora, Glenridding_Heights, Glenridding_Ravine, Glenwood,
                                 Gold_Bar, Grandview_Heights, Granville, Graydon_Hill, Greenfield, Greenview, Griesbach, Grovenor, Haddow,
                                 Hagmann_Estate_Industrial, Hairsine, Hawks_Ridge, Hays_Ridge_Area, Hazeldean, Henderson_Estates, 
                                 Heritage_Valley_Town_Centre_Area, High_Park, High_Park_Industrial, Highlands, Hillview, Hodgson, 
                                 Hollick_Kenyon, Holyrood, Homesteader, Hudson, Huff_Bremner_Estate_Industrial, Idylwylde, Industrial_Heights,
                                 Inglewood, Jackson_Heights, Jamieson_Place, Jasper_Park, Kameyosek, Keheewin, Kenilworth, Kennedale_Industrial,
                                 Kensington, Kernohan, Keswick_Area, Kildare, Kilkenny, Killarney, King_Edward_Park, Kinglet_Gardens, 
                                 Kiniski_Gardens, Kinokamau_Plains_Area, Kirkness, Klarvatten, La_Perle, Lago_Lindo, Lansdowne, Larkspur, 
                                 Lauderdale, Laurel, Laurier_Heights, Lee_Ridge, Leger, Lendrum_Place, Lewis_Farms_Industrial, Lorelei, Lymburn,
                                 Lynnwood, MacEwan, Mactaggart, Magrath_Heights, Malmo_Plains, Maple, Maple_Ridge, Maple_Ridge_Industrial,
                                 Marquis, Matt_Berry, Mayfield, Mayliewan, McCauley, McConachie_Area, McIntyre_Industrial, McKernan, McLeod,
                                 McQueen, Meadowlark_Park, Meltwater, Menisa, Meyokumin, Meyonohk, Michaels_Park, Mill_Creek_Ravine_North,
                                 Mill_Woods_Town_Centre, Miller, Minchau, Mistatim_Industrial, Montrose, Morris_Industrial, Newton, 
                                 North_Glenora, Northmount, Ogilvie_Ridge, Oleskiw, Oliver, Ormsby_Place, Ottewell, Overlanders, Oxford, Ozerna,
                                 Paisley, Papaschase_Industrial, Parkallen, Parkdale, Parkview, Parsons_Industrial, Patricia_Heights, Pembina,
                                 Pintail_Landing, Pleasantview, Pollard_Meadows, Potter_Greens, Prince_Charles, Prince_Rupert, Queen_Alexandra,
                                 Queen_Mary_Park, Quesnell_Heights, Rampart_Industrial, Ramsay_Heights, Rapperswill, Rhatigan_Ridge, Richfield,
                                 Richford, Rideau_Park, Rio_Terrace, Ritchie, River_Valley_Fort_Edmonton, River_Valley_Highlands, 
                                 River_Valley_Kinnaird, River_Valley_Laurier, Riverdale, River_s_Edge, Riverview_Area, Rosedale_Industrial,
                                 Rosenthal, Rossdale, Rosslyn, Royal_Gardens, Rundle_Heights, Rural_North_East_Horse_Hill, Rural_North_East_South_Sturgeon,
                                 Rutherford, Sakaw, Satoo, Schonsee, Secord, Sherbrooke, Sherwood, Sifton_Park, Silver_Berry, Skyrattler, South_Terwillegar, 
                                 Southeast_Industrial, Spruce_Avenue, Starling, Steinhauer, Stewart_Greens, Stillwater, Stone_Industrial, Strathcona,
                                 Strathcona_Industrial_Park, Strathcona_Junction, Strathearn, Suder_Greens, Summerlea, Summerside, Sweet_Grass, Tamarack,
                                 Tawa, Terra_Losa, Terrace_Heights, Terwillegar_Towne, The_Hamptons, The_Orchards_At_Ellerslie, The_Uplands, Thorncliff,
                                 Tipaskan, Trumpeter_Area, Tweddle_Place, Twin_Brooks, University_of_Alberta_Farm, Virginia_Park, Walker, Webber_Greens,
                                 Wedgewood_Heights, Weinlos, Weir_Industrial, Wellington, West_Jasper_Place, West_Meadowlark_Park, West_Sheffield_Industrial,
                                 Westbrook_Estates, Westmount, Westridge, Westview_Village, Westwood, Wild_Rose, Wilson_Industrial, Windermere, 
                                 Windermere_Area, Windsor_Park, Winterburn_Industrial_Area_East, Winterburn_Industrial_Area_West, Woodcroft, 
                                 Yellowhead_Corridor_East, York, Youngstown_Industrial)

#####################################################################################################################################
#mapping the fun things!

catdog_map_2020 <- tm_shape(shp = Ed_shp) + 
  tm_polygons(col = "gray") + 
  tm_shape(shp = Ed_cat_dog_map_data) +
  tm_polygons(col = "Cat_or_Dog", id = "descriptiv", palette = "Pastel1") +
  tm_text("descriptiv", size = 0.5 )

tmap_save(catdog_map_2020, "catdog.TIFF")

catdog_map_2020

catdog_map_2021 <- tm_shape(shp = Ed_shp) + 
  tm_polygons(col = "gray") + 
  tm_shape(shp = Ed_cat_dog_map_data) +
  tm_polygons(col = "Cat_or_Dog", id = "descriptiv", palette = "Pastel1")
tmap_save(catdog_map_2021, "catdog_no_text.TIFF")
catdog_map_2021
