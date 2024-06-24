



# 1. First we'll store a list of the environmental properties we'll be interested in at the end.
env_vars <- 
  lob_preds |> 
  select(Region, Reef_1, Site, Site_Depth, T_ID,
         Depth_bin_meso, Depth, Depth_bin, Depth_m)  |> 
  distinct()

# 2. Summarise species accounts by depth bin and site:
transect_speccount <- 
  lob_preds |> 
  group_by(T_ID, Binomial) |> # Just the grouping variable (single) that we are interested in here
  dplyr::summarise(Count=sum(Number)) |> 
  ungroup()

# 3. Check how many taxa we are capturing:
transect_speccount |> 
  select(Binomial) |> 
  distinct()
# 78

# 4. Convert into "wide" diversity tbl
transect_abun_matrix <- 
  transect_speccount |> 
  pivot_wider(names_from = Binomial, values_from=Count, values_fill = 0) |> 
  dplyr::select(!`NA`) |>  # Get rid of column where the sp name ('Binomial') had been NA
  ungroup() |> 
  mutate(Total = rowSums(across(where(is.numeric)))) |> 
  filter(Total > 1) |>                  # We are removing the 0 or 1 observation transects but might need to put them back in later for calculating means
  select(!Total)


# 4a. Check which sites had only one or less species
#zero_observations <- 
transect_speccount |> 
  pivot_wider(names_from = Binomial, values_from=Count, values_fill = 0) |> 
  dplyr::select(!`NA`) |>  # Get rid of column where the sp name ('Binomial') had been NA
  ungroup() |> 
  mutate_if(is.numeric, ~1 * (. != 0)) |> 
  mutate(Total = rowSums(across(where(is.numeric)))) |> 
  filter(Total < 2) |>                             
  select(T_ID, Total)

# 5. Make matrix for iNEXT input
transect_inext_matrix <- 
  transect_abun_matrix |> 
  column_to_rownames("T_ID") |> 
  t() #|> 
#  as.data.frame()  |> 
#  rownames_to_column() |> 
#  select(!rowname) |> 
#  as.matrix() |> 
#  t()


# 6. Run in iNEXT
# Abundance based
transect_rarefied <- 
  transect_inext_matrix |> 
  iNEXT(q = c(0,1),  datatype = "abundance", endpoint = 1000, nboot = 100)


transect_rarefied$AsyEst # asymptotic diversity estimates.
