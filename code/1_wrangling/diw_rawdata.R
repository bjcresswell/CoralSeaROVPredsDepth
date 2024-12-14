## Wrangle in fish data and extract predator observations


# Housekeeping + prelims
rm(list=ls())
#dev.off()
source("code/0_extras/packages.R")

# Load main fish data - 15274 rows 18487
# fish_raw <- data.io::read$csv("https://figshare.com/ndownloader/files/42424440?private_link=0a7edbfa91467638b998") |> 
#   mutate_if(is.character, as.factor) |>                    # Fix ch vars
#   tibble()

#save(fish_raw, file = "data/2_Rdata/fish_raw.Rdata")    
load("data/2_Rdata/fish_raw.Rdata")    

#glimpse(fish_raw)
#head(fish_raw)

# Load metadata
source("code/1_wrangling/diw_metadata.R")
# OR
#load("data/Rdata/lob_metadata.Rdata")    

# Combine fish and metadata
lob_fish <- 
  fish_raw  |> 
  right_join(lob_metadata) |> 
  mutate(Number = replace_na(Number, 0)) |>   # Need to replace NAs in transects where there were no observations
  droplevels()


# Save/load
#save(lob_fish, file = "data/2_Rdata/lob_fish.Rdata")    

# Checks
lob_fish  |> 
  select(Reef_1, Site, T_ID) |> 
  unique()
# 346 transects

# Any transects with no fish?
lob_fish |> filter(is.na(OpCode))
# 6 

# Depths
lob_fish  %$% 
  summary(Depth)

# Detailed checks - these should all be 0
# If not, then likely it is the inclusion of lagoon or inner transects
check1 <- anti_join(lob_fish, lob_metadata)
check2 <- anti_join(lob_metadata, lob_fish)
check3 <- check1  |> 
  select(Reef_1, Site, T_ID) |> 
  unique()


# Extract predator fishes, based on established criteria - 795 total actual obs
lob_preds <- lob_fish |> 
  filter(str_detect(Functional.Group, "pisc") |
           str_detect(Functional.Group, "carn") |
           str_detect(Trophic.Group, "pisc") |
           Family =="Carangidae" | # Trevallies 
           Family =="Scombridae" | # Mackerels/tunas
           Family == "Lethrinidae" | # Emperors
           Family == "Lutjanidae" | # Snappers
           Family == "Haemulidae" | # Grunts/sweetlips
           Family == "Sphyraenidae" | # Barracudas
           Family == "Serranidae" | # Groupers (also contains pseudanthias - will get rid of these below)
           Family == "Epinephelidae" | # Some of the groupers listed under this fam
           Family == "Carcharhinidae" | # Sharks
           Binomial == "Cheilinus undulatus") %>%   
  filter(Family != "Echeneidae" & # Get rid of remoras
           #Family != "Apogonidae" & # Could get rid of cardinal fishes - there are only 4 obs in the whole df but one is a small piscivore (Cheilodipterus)
           Genus != "Pseudanthias" &  # Get rid of fairy basslets/anthias
           Genus != "Serranocirrhitus" &  # .. as above
           Genus != "Pyronotanthias" &  # .. as above
           Genus != "Liopropoma" &  # .. as above
           Functional.Group != "cleaner" & # And remoras
           Trophic.Level >= 3.3)  |> 
  full_join(lob_metadata) %>%                 # The full join with the rmetadata needs to happen before...
  mutate(Number = replace_na(Number, 0)) |> 
  mutate(Family = replace(Family, Family == "Epinephelidae", "Serranidae"))  |> 
  droplevels()


# Checks

# Should now just have Lihou, Osprey, Bougainville
lob_preds %$% summary(Reef_1)
## Yes, and reasonably balanced

# Check that all have a Zone applied
lob_preds %$% summary(Zone)

# Number of individual pred observations - use both preds and predsum to check consistency
#lob_predsum  |>  summarise(Total_preds = sum(No_Preds)) # Can only run this once predsum loaded
lob_preds  |>  summarise(No_Preds =sum(Number)) 

## No of families
lob_preds |>  group_by(Family) |>  summarise() # 18-1 = 17 fams total

# Number of predator species (78- NA = 77)
(lob_predtaxa <- lob_preds |>  group_by(Binomial) |>  summarise()) # 77

# Check number of transects: 346
lob_preds |>  select(T_ID)  |>   distinct()
#lob_preds  |>  group_by(T_ID)  |>  summarise()

# Check number of transects per depth zone - can use lob_preds or lob_predsum
#lob_predsum %>% group_by(Depth)  %$% summary(Depth) 
lob_preds |>  distinct(Depth, T_ID)  %$% summary (Depth)

# Check how many are zeros - 98
lob_preds |>  filter(is.na(OpCode))

# Should have no NAs in the Number var
lob_preds %$% summary(Number)


# Some other checks
## By body size - note we do have some NAs as not all videos were stereo
lob_preds %$% summary(Length_mm) # Some crazy small measurements in here - pass on to GFG
small_preds <- lob_preds |>  filter(Length_mm <200) # 
#lg_preds <- lob_preds %>% filter(Length_mm >300) # 


# Save/load
#save(lob_preds, file = "data/2_Rdata/lob_preds.Rdata")    
#load("data/2_Rdata/lob_preds.Rdata")    


