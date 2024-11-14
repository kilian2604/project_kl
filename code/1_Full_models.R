install.packages("metafor")
install.packages("dplyr")
install.packages("corrplot")
install.packages("MuMIn")

library(metafor)
library(dplyr)
library(corrplot)
library(MuMIn)

###################################################
# Activity analysis ###############################

# Import data
rai_noSame <- read.csv("data/rai_with_covariates.csv", header=T)
# Sample size
length(unique(rai_noSame$Project.ID)) 
length(unique(rai_noSame$Species)) 
length(unique(paste(rai_noSame$Species, rai_noSame$Project.ID))) 

# Data preparation
# Set factors and specify reference level 
rai_noSame$Treatment_Activity_NoSame <- as.factor(rai_noSame$Treatment_Activity_NoSame)
rai_noSame$Habitat_closure <- as.factor(rai_noSame$Habitat_closure)
rai_noSame$family <- as.factor(rai_noSame$family)
rai_noSame$trophic_level <- as.factor(rai_noSame$trophic_level)
rai_noSame$activity_cycle <- as.factor(rai_noSame$activity_cycle)
rai_noSame$foraging_stratum <- as.factor(rai_noSame$foraging_stratum)
rai_noSame$trophic_groups <- as.factor(rai_noSame$trophic_groups)

# Reference level - large herbivore
# das Referenzlevel wird auf large herbivore gesetzt
rai_noSame$trophic_groups <- relevel(rai_noSame$trophic_groups, "large_herbivore")

# 95% CI summary data
rai_noSame %>% 
  summarise(mean.yi = mean(yi_mod_lowhigh, na.rm = TRUE),
            sd.yi = sd(yi_mod_lowhigh, na.rm = TRUE),
            n.yi = n()) %>%
  mutate(se.yi = sd.yi / sqrt(n.yi),
         lower.ci.yi = mean.yi - qt(1 - (0.05 / 2), n.yi - 1) * se.yi,
         upper.ci.yi = mean.yi + qt(1 - (0.05 / 2), n.yi - 1) * se.yi)

#####################
# Models

#####################################
# Global model

# Full model
full_mod <- rma.mv(yi = yi_mod_lowhigh, V = vi_mod_lowhigh, 
                   # Additives
                   mods = ~ Comparison + #Method
                     trophic_groups + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                     stringency_med + HMI_med + Habitat_closure, # Human factors 
                   random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                   data = rai_noSame , method = "ML")

# Null model 
null_mod <- rma.mv(yi = yi_mod_lowhigh, V = vi_mod_lowhigh, 
                   # Additives
                   mods = ~ 1, # Human factors 
                   random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                   data = rai_noSame , method = "ML")

# Save the objects to plot later #

dir.create("model_objects")
saveRDS(full_mod, "model_objects/RAI_full_model_object.rds")
saveRDS(null_mod, "model_objects/RAI_null_model_object.rds")

#####
# Psuedo-R2 according to https://gist.github.com/wviechtb/6fbfca40483cb9744384ab4572639169 
max(0, 100 * (sum(null_mod$sigma2) - sum(full_mod$sigma2)) / sum(null_mod$sigma2))

#####
# i2
i2.mv <- function(model) {
  require(tidyverse)
  require(metafor)
  library(tidyverse)
  library(metafor)
  vars <- 1/model$vi %>% na.omit()
  W <- diag(vars)
  X <- model.matrix(model)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  sample.var <- (model$k-model$p)/sum(diag(P), na.rm = T)
  total.var <- (sum(model$sigma2) + (model$k-model$p)/sum(diag(P)))
  out <- tibble(
    component = NA_character_,
    sigma2 = NA_real_,
    i2 = NA_real_,
    i2.pc = NA_character_
  )
  for(i in 1:length(model$sigma2)) {
    out <- out %>%
      add_row(component = paste0("sigma^2.", i),
              sigma2 = model$sigma2[i],
              i2 = model$sigma2[i]/total.var,
              i2.pc = paste0(signif((model$sigma2[i]/total.var)*100, 3), "%"))
  }
  out <- out %>% na.omit()
  out <- out %>%
    add_row(
      component = "total",
      sigma2 = sum(model$sigma2),
      i2 = sum(out$i2),
      i2.pc = paste0(signif(sum(out$i2)*100, 3), "%")
    )
  return(out)
}

i2.mv(null_mod)


################################
# Interaction models ###########

top_mod_rai_int1 <- rma.mv(yi = yi_mod_lowhigh, V = vi_mod_lowhigh, 
                           # Additives
                           mods = ~ Comparison + #Method
                             trophic_groups + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                             stringency_med + # Traits
                             HMI_med * Habitat_closure, # Human factors   
                           
                           random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                           data = rai_noSame , method = "ML")


top_mod_rai_int2 <- rma.mv(yi = yi_mod_lowhigh, V = vi_mod_lowhigh, 
                           # Additives
                           mods = ~ Comparison + #Method
                             activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                             stringency_med + Habitat_closure + 
                             trophic_groups * HMI_med,    
                           
                           random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                           data = rai_noSame , method = "ML")


top_mod_rai_int3 <- rma.mv(yi = yi_mod_lowhigh, V = vi_mod_lowhigh, 
                           # Additives
                           mods = ~  Comparison + #Method
                             activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                             stringency_med + HMI_med +
                             trophic_groups * Habitat_closure, 
                           
                           random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                           data = rai_noSame , method = "ML")



top_mod_rai_int4 <- rma.mv(yi = yi_mod_lowhigh, V = vi_mod_lowhigh,
                           # Additives
                           mods = ~  Comparison + #Method
                             trophic_groups + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                             stringency_med + HMI_med + Habitat_closure + I(HMI_med^2),  # Human factors
                           
                           random = list(~ 1 | Project.ID, ~ 1 | family/Species),
                           data = rai_noSame , method = "ML")


# Select between the top models - note not all terms always show
model.sel(full_mod, top_mod_rai_int1,top_mod_rai_int2, top_mod_rai_int3, top_mod_rai_int4)


saveRDS(top_mod_rai_int1, "model_objects/RAI_int_model_1_object.rds")
saveRDS(top_mod_rai_int2, "model_objects/RAI_int_model_2_object.rds")
saveRDS(top_mod_rai_int3, "model_objects/RAI_int_model_3_object.rds")
saveRDS(top_mod_rai_int4, "model_objects/RAI_int_model_4_object.rds")


########################################################
# Nocturnality #########################################
# Import data
noct <- read.csv("data/noct_with_covariates.csv", header=T)

# Set factor levels
noct$Treatment_Activity_NoSame <- as.factor(noct$Treatment_Activity_NoSame)
noct$Habitat_closure <- as.factor(noct$Habitat_closure)
noct$family <- as.factor(noct$family)
noct$trophic_level <- as.factor(noct$trophic_level)
noct$trophic_groups <- as.factor(noct$trophic_groups)
noct$activity_cycle <- as.factor(noct$activity_cycle)
noct$foraging_stratum <- as.factor(noct$foraging_stratum)
noct$activity_cycle <- as.factor(noct$activity_cycle)

# Relevel - compare to noctural and large herbivore
noct$activity_cycle <- relevel(noct$activity_cycle, "nocturnal_only")
noct$trophic_groups <- relevel(noct$trophic_groups, "large_herbivore")

# Sumamrise the effect sizes
noct %>% 
  summarise(mean.yi = mean(RR_lowhigh, na.rm = TRUE),
            sd.yi = sd(RR_lowhigh, na.rm = TRUE),
            n.yi = n()) %>%
  mutate(se.yi = sd.yi / sqrt(n.yi),
         lower.ci.yi = mean.yi - qt(1 - (0.05 / 2), n.yi - 1) * se.yi,
         upper.ci.yi = mean.yi + qt(1 - (0.05 / 2), n.yi - 1) * se.yi)

# Full model
full_mod_noc <- rma.mv(yi = RR_lowhigh , V = Variance_lowhigh,
                       # Additives
                       mods = ~ Comparison + #Method
                         trophic_groups + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                         stringency_med + HMI_med + Habitat_closure # Human factors   
                       , 
                       random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                       data = noct , method = "ML")

null_mod_noc <- rma.mv(yi = RR_lowhigh , V = Variance_lowhigh,
                       # Additives
                       mods = ~ 1,
                       random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                       data = noct , method = "ML")


# Psuedo-R2 according to https://gist.github.com/wviechtb/6fbfca40483cb9744384ab4572639169 
max(0, 100 * (sum(null_mod_noc$sigma2) - sum(full_mod_noc$sigma2)) / sum(null_mod_noc$sigma2))
#I2
i2.mv(null_mod_noc)


# write the nocturnality global model object
saveRDS(full_mod_noc, "model_objects/NOC_full_model_object.rds")
saveRDS(null_mod_noc, "model_objects/NOC_null_model_object.rds")


##########################################
# Interaction models #####################

full_mod_noc_int <- rma.mv(yi = RR_lowhigh , V = Variance_lowhigh,
                           # Additives
                           mods = ~ Comparison + #Method
                             trophic_groups + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                             stringency_med + HMI_med * Habitat_closure # Human factors   
                           , 
                           random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                           data = noct , method = "ML")



full_mod_noc_int2 <- rma.mv(yi = RR_lowhigh , V = Variance_lowhigh,
                            # Additives
                            mods = ~ Comparison + #Method
                              activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                              stringency_med +trophic_groups * HMI_med + Habitat_closure # Human factors   
                            , 
                            random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                            data = noct , method = "ML")

full_mod_noc_int3 <- rma.mv(yi = RR_lowhigh , V = Variance_lowhigh,
                            # Additives
                            mods = ~ Comparison + #Method
                              trophic_groups* Habitat_closure + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                              stringency_med + HMI_med  # Human factors   
                            , 
                             random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                            data = noct , method = "ML")




full_mod_noc_int4 <- rma.mv(yi = RR_lowhigh , V = Variance_lowhigh,
                            # Additives
                            mods = ~ Comparison + #Method
                              trophic_groups + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                              stringency_med + HMI_med + Habitat_closure  + I(HMI_med^2), # Human factors   
                            random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                            data = noct , method = "ML")



noct_sel<- model.sel(full_mod_noc, full_mod_noc_int ,full_mod_noc_int2, full_mod_noc_int3, full_mod_noc_int4)
clipr::write_clip(noct_sel)

saveRDS(full_mod_noc_int,  "model_objects/NOC_int_model_1_object.rds")
saveRDS(full_mod_noc_int2, "model_objects/NOC_int_model_2_object.rds")
saveRDS(full_mod_noc_int3, "model_objects/NOC_int_model_3_object.rds")
saveRDS(full_mod_noc_int4, "model_objects/NOC_int_model_4_object.rds")