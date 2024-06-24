# Aesthetics, parameters etc

#websafe_bluepal <- c("#ccccff", "#6666ff", "#000099")
#websafe_pinkpal <- c("#660066" ,"#ff66ff", "#ffc4ff")
#websafe_greenpal <- c("#003333", "#336633", "#66cc99")


#options(mc.cores = 3,         brms.backend = "cmdstanr")

# Set some global Stan options
#CHAINS <- 3
#ITER <- 2000
#WARMUP <- 1000
#BAYES_SEED <- 1234



#depth_palette = c("#000099", "#0d0887" )\



theme_bjc <- function (base_size = 11, base_family = "Arial") {
  theme_minimal() %+replace% 
    theme(
      plot.background = element_rect(fill = "white", colour = "transparent"),
      legend.title = element_text(color = "black", size = 10, family = "Arial"),
      legend.text = element_text(color = "black", size = 9, family = "Arial"),
      axis.title = element_text(color = "black", size = 10, family = "Arial"),
      axis.text = element_text(color = "black", size = 9, family = "Arial")
    )
}
