# Predator diversity diw script #


#####

# By site and depth bin
# List of species observations at each site*depth combination



# Summarise counts of all taxa by site * depth
site_depth_specsum <- 
  lob_preds %>% 
  group_by(Site_Depth, Binomial) %>% # Group including family for use in taxa_nos analysis later
  dplyr::summarise(Count=sum(Number)) %>% 
  ungroup()

# Diversity matrix
site_depth_matrix_tbl <- 
  site_depth_specsum %>% 
  pivot_wider(names_from = Binomial, values_from=Count, values_fill = 0) %>% 
  dplyr::select(!`NA`) %>%  # Get rid of column where the sp name ('Binomial') had been NA
  ungroup()

# Diversity matrix with row names (rather than names in their own column)  
site_depth_matrix_df <- site_depth_matrix_tbl %>% 
  column_to_rownames(var = "Site_Depth") 

# Simple raw species richness
site_depth_spec_rich <- 
  site_depth_specsum %>% 
  mutate(Count = ifelse(Count >1, 1, Count)) %>% 
  group_by(Site_Depth) %>% 
  summarise(n_sp = sum(Count)) %>% 
  arrange(n_sp)


















##### JUNK #####

  # Make a diversity matrix for all transects
lob_predspec <- rmetadata %>% 
  full_join(predmatrix, by = "T_ID")

  


map



# Calculate diversity metrics
pred_diversity <-  rmetadata %>% 
  full_join(
    predmatrix %>% 
      mutate(SpecNo = specnumber(predmatrix[-1]), 
             Shannon = diversity(predmatrix[-1]))
      select(T_ID, SpecNo, Shannon))


# Shannon vs Simpson
ggplot(pred_diversity, aes(y=Simpson,  x=Shannon))+
  geom_point()

# Whereas
ggplot(pred_diversity, aes(y=Shannon,  x=SpecNo))+
  geom_point()


# Histogram - ZI dataset?
ggplot(pred_diversity, aes(x=Shannon)) + 
  geom_histogram(binwidth=0.1, color="black", fill="grey")
