# Analysis of assemblage composition in gllvm

# Packages
source("code/packages.R")
library(mvabund)
library(gllvm)

# Load data and helper functions
#source("code/helper_functions.R")
source("code/packages.R")
source("code/extras.R")
load("data/Rdata/lob_preds.Rdata")


## Store list of predictors.
env_boral_TID <- 
  lob_preds |> 
  select(#Region, Reef_1, Site, Site_Depth,
    # Reef_1_Area, Reef_100, Reef_size,
    Depth,
    #dCT, dGBR
    T_ID) |> 
  distinct() |> 
  as.data.frame()


## Turn into mvabund object
predabund <- mvabund(transect_abun_matrix) 



# We know we are interested in diffs between region
# We don't know how many latent variables will best explain our data so we will try a few:

criteria <- NULL
for(i in 1:5){
  fiti <- gllvm(predabund, env_boral_TID, family = "negative.binomial", num.lv = i, sd.errors = FALSE,
                formula = ~ Depth, seed = 1234)
  criteria[i] <- summary(fiti)$AICc
  names(criteria)[i] = i
}

criteria

#save(criteria, file = "data/Rdata/mod_data/criteria.Rdata") # 
#load(file = "data/Rdata/mod_data/criteria.Rdata") # 


# So one latent variable best model -> won't work in boral though

# Run as its own object
lvmod_depthTID <- gllvm(predabund, env_boral_TID, family = "negative.binomial", num.lv = 2, sd.errors = TRUE,
                 formula = ~ Depth, seed = 1234)

#save(lvmod_depthTID, file = "data/Rdata/mod_data/lvmod_depthTID.Rdata") # 
#load(file = "data/Rdata/mod_data/lvmod_depthTID.Rdata") # 


# Check residuals
plot(lvmod_depthTID, which = 1)
plot(lvmod_depthTID, which = 2)
plot(lvmod_depthTID, which = 3)
plot(lvmod_depthTID, which = 4)
plot(lvmod_depthTID, which = 5)

# Look reasonable - can't see any real pattern

# Check summary - not that useful
summary(lvmod_depthTID)


# How to interrogate model from here?
# Can't (figure out how to) do pairwise comparisons among depth zones
# But this appraoch is useful for finding any correlations between species occurrence

cr <- getResidualCor(lvmod_depthTID)
library(corrplot)
library(gclus)

# In built correlation matrix - looks horrible
depthTID_corplot <- corrplot(cr[order.single(cr), order.single(cr)], diag = FALSE, type = "lower", 
         method = "square", tl.cex = 0.5, tl.srt = 45, tl.col = "red")

# But we can extract the cor scores for plotting in ggplot
sp_cors <- tibble(depthTID_corplot$corrPos) |> 
  mutate_if(is.character, as.factor) |> 
  mutate(xName = fct_reorder(xName, x),          # Need to reorder the sp names according to the numeric position
         yName = fct_reorder(yName, y))          # For both axes
 
# If you need as a matrix
sp_cor_matrix <- 
  sp_cors %>% 
  pivot_wider(names_from = yName, values_from = corr, id_cols = xName)


####

# Filter out high and low cors
hicor_pos <- sp_cors %>% 
  #slice_max(corr, n = 20) |> 
  filter(corr > 0.8) |> 
  rename(hicor = corr)

hicor_neg <- sp_cors %>% 
  #slice_min(corr, n = 20) |> 
  filter(corr < -0.8) |> 
  rename(hicor = corr)


sp_hicors <- bind_rows(hicor_neg, hicor_pos)

sp_cors1 <- 
  sp_cors |> 
  left_join(sp_hicors) |> 
  mutate(xName1 = factor(if_else(is.na(hicor), "", xName), ordered = TRUE),
         yName1 = factor(if_else(is.na(hicor), "", yName), ordered = TRUE)) |> 
  mutate(xName1 = fct_reorder(xName1, x),          # Need to reorder the sp names according to the numeric position
         yName1 = fct_reorder(yName1, y)) 

  
  
####


ggplot(data = sp_cors) +
  geom_tile(aes(x = xName, y = yName, fill = corr)) +
  #geom_tile(aes(x = xName1, y = yName1), fill = 'transparent') +
  scale_fill_gradient2("Correlation", midpoint = 0, mid ="white", limits = c(-1, +1)) +
  #annotate()
  annotate("text", x = -1, y = 67, label = "L. gibbus", fontface = "italic", hjust = 2) +
  coord_cartesian(clip ="off") +
  scale_y_discrete(breaks = sp_cors1$yName1, labels = sp_cors1$yName1) +
  scale_x_discrete(expand = c(0, 0)) +
  theme_bw() +
  theme(plot.title = element_blank(), 
        axis.title = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        #axis.text.y = element_blank(),
        plot.margin = unit(c(1, 0, 0, 1), "cm"),           # Top, right, bottom, left
        legend.title = element_text(colour="black", size = 9)) 



  













# We can check this correlation in the absence of the regional predictor

# Rerun model without regional predictor and with 2 lvs
lvmod_lvonly <- gllvm(predabund, family = "negative.binomial", num.lv = 2, seed = 1234)


# Correlation matrix
cr0 <- getResidualCor(lvmod_lvonly)
lv_sp_corplot <- corrplot(cr0[order.single(cr0), order.single(cr0)], diag = FALSE, type = "lower", 
         method = "square", tl.cex = 0.5, tl.srt = 45, tl.col = "red")




### JUNK FOR NOW ####

#Think the ordination might be best in boral. We'll see.

# Model based ordination
lvord_1 <- gllvm(predabund, family="negative.binomial")
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot(lvord_1, var.colors = 1)

# You can check information criteria
AIC(lvmod_depthTID, lvord_1)




ordiplot(lvord_1, biplot = TRUE, ind.spp = 8, 
         main = "Biplot")

dev.off()


# ---------- FULL TUTORIAL --------- # 

library(mvabund)
library(gllvm)
data(antTraits)
y <- as.matrix(antTraits$abund)
X <- scale(as.matrix(antTraits$env))
TR <- antTraits$traits
  

abund <- y
env <- X


blah <- reshape(data.frame(cbind(abund, env)), 
              #direction = 'wide',
              direction = "long",
              varying = colnames(abund), 
              v.names = "abund", 
              timevar = "doodah")

yX <- reshape(data.frame(cbind(y, X)), 
              direction = "long", 
              varying = colnames(y), 
              v.names = "y", 
              timevar = "sp")



TR2 <- data.frame(sp = 1:41, TR)

TR3 <- TR2 |> 
  rownames_to_column(var = 'Species')


datalong <- merge(yX, TR2, by = "sp")
datalong2 <- merge(yX, TR3, by = "sp")


datalong2 <- yX |> 
  left_join(TR2, by = "sp")


# Check
datalong |> head()

# Model with environmental variables Bare.ground and Shrub.cover as predictors
test_lvm <- gllvm(formula = y ~ (Bare.ground + Shrub.cover), data = datalong2,  
      family = "negative.binomial")


testsum <- summary(test_lvm)





