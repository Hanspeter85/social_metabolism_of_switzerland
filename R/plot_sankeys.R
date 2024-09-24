library(tidyverse)
library(data.table)
library(openxlsx)
library(networkD3)
library(tidyverse)

# Avoid scientific notation when viewing R objects
options(scipen=999)

# Path to folder where result files are located
path <- "C:/Users/hwieland/Data/Social_Metabolism_of_Switzerland/"

# Set year and country of analysis
year <- 2020
region <- "Switzerland"

# Load raw data
raw <- fread( str_c(path,year,"_AllStressors_FromTo.csv"))

# Get numnber of stressors
stressors <- data.frame("name" = raw$stressor |> unique(),
                        "unit" = c("Mt", "Mt", "Mt", "Mt", "PJ", "Mt", "Mt",
                                   "1000ha", "m.ppl", "b.USD"),
                        "factor" = c(10^6, 10^6, 10^6, 10^6, 1000, 1000, 1000, 1, 10^6, 10^6)
                        )

# Set order of regions
reg_sort <- data.frame("name" = c("Switzerland",
                                  "High income",
                                  "Upper middle income",
                                  "China",
                                  "Lower middle income",
                                  "India",
                                  "Low income"),
                       "abbrev" = c("SUI",
                                    "HI",
                                    "UMI",
                                    "CHN",
                                    "LMI",
                                    "IND",
                                    "LI")
                       )

# Aggregate to desired level
dat <- raw %>% 
  filter(From_RegionName == region | To_RegionName == region,
         year == year) %>% 
  mutate("FROM" = case_when(From_RegionName == region ~ region,
                            From_RegionName != region ~ From_IncomeGroup),
         "TO" = case_when(To_RegionName == region ~ region,
                          To_RegionName != region ~ To_IncomeGroup)) %>% 
  group_by(FROM, TO, stressor) %>% 
  summarise(value = sum(value), .groups = 'drop')

write.xlsx(dat, "./output/Aggregated embodied flows Switzerland 2020 for Sankey all stressors.xlsx")

# Loop over stressor and plot sankey
s <- 10
for(s in 1:nrow(stressors))
{
  
  dat_sel <- dat %>% 
    filter(stressor == stressors$name[s]) %>% 
    select(-stressor) %>% 
    mutate(value = value / stressors$factor[s]) %>% 
    left_join(reg_sort, by = c("FROM" = "name")) %>% 
    left_join(reg_sort, by = c("TO" = "name")) %>%
    rename(source = abbrev.x,
           target =  abbrev.y) %>% 
    group_by(source) %>% 
    mutate(source_value = round(sum(value),digits = 2) ) %>% 
    ungroup() %>% 
    group_by(target) %>% 
    mutate(target_value = round( sum(value), digit = 2) ) %>% 
    ungroup() %>% 
    mutate(source  = str_c(source," (",source_value," ",stressors$unit[s],")"),
           target  = str_c(target," (",target_value," ",stressors$unit[s],")"))
  
  reg_sort_sel <- reg_sort %>% 
    left_join(dat_sel %>%
                select(FROM, source) %>% 
                distinct(),
              by = c("name" = "FROM")) %>%
    left_join(dat_sel %>%
                select(TO, target) %>% 
                distinct(),
              by = c("name" = "TO"))
    
  dat_sel <- dat_sel %>% 
    select(source, target, value)
  
  # From these flows we need to create a node data frame: it lists every entities involved in the flow
  nodes <- data.frame(name=c(reg_sort_sel$source, reg_sort_sel$target)) |> unique()
  
  # Create an index column for source and target nodes
  dat_sel$source_id <- match(dat_sel$source, nodes$name) - 1
  dat_sel$target_id <- match(dat_sel$target, nodes$name) - 1
  
  # Set colors
  # my_color <- 'd3.scaleOrdinal() .domain(["Switzerland", "High income","Upper middle income", "China", "Lower middle income", "India", "Low income", "group_H"]) .range(["blue", "blue" , "blue", "red", "red", "yellow", "purple", "purple"])'
  # Define a black-to-grey color scale
  color_scale <- 'd3.scaleOrdinal(["#000000", "#555555", "#AAAAAA", "#CCCCCC", "#E0E0E0"])'
  
  # Generate the Sankey diagram
 sankeyNetwork(Links = dat_sel, 
                Nodes = nodes,
                Source = "source_id",
                Target = "target_id", 
                Value = "value",
                NodeID = "name",
                units = "t",
                fontSize = 16,
                nodeWidth = 30, 
                sinksRight = FALSE,
                colourScale = color_scale)
  
  print(stressors$name[s])
  
}

