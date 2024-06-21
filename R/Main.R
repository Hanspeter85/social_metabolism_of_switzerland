
library(tidyverse)
library(data.table)
library(openxlsx)

# Avoid scientific notation when viewing R objects
options(scipen=999)

# Path to folder where result files are located
path <- "C:/Users/hwieland/Data/Social_Metabolism_of_Switzerland/"

# Set time frame
TIME = 1995:2020
t = length(TIME)

# Lists to store yearly results
list_FromTo = list_CHE = list_EuE = vector(mode = "list", length = t)


# i = 1
# Write data sets into lists
for(i in 1:t)
{
  print(TIME[i])
  
  # Load data sets
  list_FromTo[[i]] = fread( str_c(path, TIME[i], "_AllStressors_FromTo.csv") )
  list_CHE[[i]] = read.xlsx( str_c(path, TIME[i], "_AllStressors_FromSourceRegions_to_Sectors_in_CHE.xlsx") )
  tmp = read.xlsx( str_c(path, "EuE_Sector_CHE_", TIME[i], ".xlsx") )
  
  
  # Transform EuE data to long format
  list_EuE[[i]] = tmp %>% 
    pivot_longer(cols = colnames(tmp)[4:31], names_to = "indicator") %>% 
    mutate("year" = TIME[i])
}

# Aggregate each list into data frames
FromTo = bind_rows(list_FromTo, .id = "column_label")
CHE = bind_rows(list_CHE, .id = "column_label")
EuE = bind_rows(list_EuE, .id = "column_label")

# Calculate monetary drain on national level
EuE_nat = EuE %>% 
  filter(indicator %in% colnames(tmp)[4:15]) %>% 
  group_by(indicator, year) %>% 
  summarise(value = sum(value), .groups = 'drop') %>% 
  pivot_wider(id_cols = year, names_from = indicator) %>% 
  mutate(Price.RME.South = VA.South/RME.South,
         Price.RME.North = VA.North/RME.North,
         Price.Energy.South = VA.South/Energy.South,
         Price.Energy.North = VA.North/Energy.North,
         Price.GHG.South = VA.South/GHG.South,
         Price.GHG.North = VA.North/GHG.North,
         Price.Land.South = VA.South/Land.South,
         Price.Land.North = VA.North/Land.North,
         Price.Employment.South = VA.South/Employment.South,
         Price.Employment.North = VA.North/Employment.North,
         Drain.RME = ( RME.South * Price.RME.North ) - VA.South,
         Drain.Energy = ( Energy.South * Price.Energy.North ) - VA.South,
         Drain.GHG = ( GHG.South * Price.GHG.North ) - VA.South,
         Drain.Land = ( Land.South * Price.Land.North ) - VA.South,
         Drain.Employment = ( Employment.South * Price.Employment.North ) - VA.South,
         Drain.Average = (Drain.RME + Drain.Energy + Drain.GHG + Drain.Land + Drain.Employment) / 5)

# Write file to folder
write.xlsx( EuE_nat, "./output/EuE_national_CHE.xlsx" )

# Aggregate national-level data to dimensions as selected in group_by() 
tmp = FromTo %>% 
  group_by(year, To_RegionName, To_IncomeGroup, stressor, From_IncomeGroup) %>% 
  summarise(value = sum(value), .groups = 'drop')

# Write file to folder
write.xlsx(tmp, "./output/Aggregated_national_level_data.xlsx" )


# Aggregate switzerland data to dimensions as selected in group_by() 
tmp = CHE %>% 
  group_by(year, stressor, From_IncomeGroup, From_WorldRegion, 
           From_DevelopmentGroup, To_SectorName) %>% 
  summarise(value = sum(value), .groups = 'drop')

# Write file to folder
write.xlsx(tmp, "./output/Switzerland_data.xlsx" )
