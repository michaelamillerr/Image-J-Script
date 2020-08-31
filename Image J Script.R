## Load Packages
install.packages("dplyr")
library(dplyr)
install.packages("readxl")
library(readxl)
install.packages("writexl")
library(writexl)

## Load Your Files
Raw_Image_J <- read.csv("Raw Image J.csv")
Microscope_Sheet <- read.csv("Microscope Sheet.csv")

## Select Appropriate Columns
Selected_Sizes <- dplyr::select(Raw_Image_J, Putative.Microplastic.Sample.ID, 
                                Area1, Feret1, MinFeret1, Length1, Length2, Length3, Length4)
Selected_Microscope <- dplyr::select(Microscope_Sheet, Putative.Microplastic.Sample.ID, Shape)
Combined_Data <- dplyr::right_join(Selected_Microscope,Selected_Sizes)
head(Combined_Data)

## Do Calculations and Merge Data
Fragments <- Combined_Data %>% dplyr::filter(Shape=="Fragment") %>% dplyr::mutate(Area=Area1+0) %>%
  dplyr::mutate(Length=Feret1+0) %>% dplyr::mutate(Width=MinFeret1+0) %>% dplyr::mutate(SD=NA)
Fibres <- Combined_Data %>% dplyr::filter(Shape=="Fibre") %>% dplyr::mutate(Area=NA) %>%
  dplyr::mutate(Length=Length1+0) %>% rowwise() %>% dplyr::mutate(Width=(Length2+Length3+Length4)/3, SD= sd(c(Length2, Length3, Length4)))
Beads <- Combined_Data %>% dplyr::filter(Shape=="Bead") %>% dplyr::mutate(Area=Area1+0) %>%
  dplyr::mutate(Length=Feret1+0) %>% dplyr::mutate(Width=MinFeret1+0) %>% dplyr::mutate(SD=NA)
Fragments_Fibres <- full_join(Fragments,Fibres) %>% dplyr::select(Putative.Microplastic.Sample.ID, Shape, Area, Length, Width, SD)
Fragments_Fibres_Beads <- full_join(Fragments_Fibres,Beads) %>% dplyr::select(Putative.Microplastic.Sample.ID, Shape, Area, Length, Width, SD)
head(Fragments_Fibres_Beads)

## Combine with Original Data
Merged_Data <- dplyr::full_join(Fragments_Fibres_Beads, Microscope_Sheet, 
                                by=c("Putative.Microplastic.Sample.ID")) %>%
  dplyr::select(Putative.Microplastic.Sample.ID, Photograph.ID, Shape.x, Colour, 
                Area.x, Length.x, Width.x, SD.x, FTIR, FTIR.Mode)
Merged_Data$Shape <- Merged_Data$Shape.x
Merged_Data$Area <- Merged_Data$Area.x
Merged_Data$Length <- Merged_Data$Length.x
Merged_Data$Width <- Merged_Data$Width.x
Merged_Data$SD <- Merged_Data$SD.x
Final_Datasheet <- Merged_Data %>% dplyr::select(Putative.Microplastic.Sample.ID, Photograph.ID, 
                                                 Shape, Colour, Area, Length, Width, SD, FTIR, FTIR.Mode)
head(Final_Datasheet)

## Save Datasheet
write.csv(Final_Datasheet, "Project_Specific_Data.csv")