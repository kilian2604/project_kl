############################################################
# Model subsets for Burton et al NEE #######################
############################################################

# Empirical human use change models

library(metafor)
library(dplyr)
library(corrplot)
library(MuMIn)

# Subset to human data
rai_hum <- rai_noSame[is.na(rai_noSame$hum_mod_noSame)==F,]

nrow(rai_hum)
length(unique(rai_hum$Project.ID))

global_mod_empirical <- rma.mv(yi = yi_mod_lowhigh, V = vi_mod_lowhigh, 
                               mods = ~ Comparison + #Method
                                 trophic_groups + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                                 stringency_med + HMI_med + Habitat_closure + hum_mod_noSame,  # Human factors   
                               random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                               data = rai_hum , method = "ML")

# Compare to a global model without the empirical human use term

global_mod <- rma.mv(yi = yi_mod_lowhigh, V = vi_mod_lowhigh, 
                     mods = ~ Comparison + #Method
                       trophic_groups + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                       stringency_med + HMI_med + Habitat_closure,   # Human factors   
                     random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                     data = rai_hum , method = "ML")


model.sel(global_mod_empirical, global_mod)

##############################################
# NOCTURNALALITY
# Remove the projects without human data
noct_hum <- noct[is.na(noct$hum_mod_noSame)==F,]
# Full data set summary
length(unique(noct_hum$Project.ID))
length(noct_hum$Project.ID)


global_noc_empirical <- rma.mv(yi = RR_lowhigh , V = Variance_lowhigh,
                               # Additives
                               mods = ~ Comparison + #Method
                                 trophic_groups + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                                 stringency_med + HMI_med + Habitat_closure + hum_mod_noSame # Human factors   
                               , #Interactions
                               #interactions
                               # Not yet done
                               random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                               data = noct_hum , method = "ML")


global_noc <- rma.mv(yi = RR_lowhigh , V = Variance_lowhigh,
                     # Additives
                     mods = ~ Comparison + #Method
                       trophic_groups + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                       stringency_med + HMI_med + Habitat_closure # Human factors   
                     , #Interactions
                     #interactions
                     # Not yet done
                     random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                     data = noct_hum , method = "ML")

model.sel(global_noc_empirical, global_noc)


##########################################################
#  Brain size

# Activity
rai_noSame_brain <- rai_noSame[is.na(rai_noSame$brain_mass_relative)==F,]
rai_noSame_brain$z_rel_brain <- as.numeric(stdize(rai_noSame_brain$brain_mass_relative))
nrow(rai_noSame_brain)/1065
length(unique(rai_noSame_brain$Species))


global_mod_brain <- rma.mv(yi = yi_mod_lowhigh, V = vi_mod_lowhigh, 
                           mods = ~ Comparison + #Method
                             trophic_groups + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                             stringency_med + HMI_med + Habitat_closure + z_rel_brain,  # Human factors   
                           random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                           data = rai_noSame_brain , method = "ML")

# Compare to a global model without the brain size term

global_mod <- rma.mv(yi = yi_mod_lowhigh, V = vi_mod_lowhigh, 
                     mods = ~ Comparison + #Method
                       trophic_groups + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                       stringency_med + HMI_med + Habitat_closure,   # Human factors   
                     random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                     data = rai_noSame_brain , method = "ML")

model.sel(global_mod_brain, global_mod)



# Nocturnality
noct$z_rel_brain <- as.numeric(stdize(noct$brain_mass_relative))
noct_filtered_brain <- noct[is.na(noct$z_rel_brain)==F,]
#How many projects and project-species does this leave?
length(unique(noct_filtered_brain$Project.ID))
# Number of species pairs
nrow(noct_filtered_brain)

global_noc_brain <- rma.mv(yi = RR_lowhigh , V = Variance_lowhigh,
                           # Additives
                           mods = ~ Comparison + #Method
                             trophic_groups + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                             stringency_med + HMI_med + Habitat_closure + z_rel_brain # Human factors   
                           , #Interactions
                           #interactions
                           # Not yet done
                           random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                           data = noct_filtered_brain , method = "ML")


global_noc <- rma.mv(yi = RR_lowhigh , V = Variance_lowhigh,
                     # Additives
                     mods = ~ Comparison + #Method
                       trophic_groups + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                       stringency_med + HMI_med + Habitat_closure # Human factors   
                     , #Interactions
                     #interactions
                     # Not yet done
                     random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                     data = noct_filtered_brain , method = "ML")



model.sel(global_noc_brain, global_noc)


################################
# Hunting

rai_noSame_hunt <- rai_noSame[is.na(rai_noSame$Hunted)==F,]
rai_noSame_hunt <- rai_noSame_hunt[rai_noSame_hunt$Hunted!="Unknown",]
# Add a binary hunted score
rai_noSame_hunt$hunt_bin <- "Yes"
rai_noSame_hunt$hunt_bin[rai_noSame_hunt$Hunted=="No"] <- "No"
table(rai_noSame_hunt$hunt_bin)
nrow(rai_noSame_hunt)/1065

global_mod_hunt <- rma.mv(yi = yi_mod_lowhigh, V = vi_mod_lowhigh, 
                          mods = ~ Comparison + #Method
                            trophic_groups + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                            stringency_med + HMI_med + Habitat_closure + hunt_bin,  # Human factors   
                          random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                          data = rai_noSame_hunt , method = "ML")

# Compare to a global model without the empirical human use term

global_mod <- rma.mv(yi = yi_mod_lowhigh, V = vi_mod_lowhigh, 
                     mods = ~ Comparison + #Method
                       trophic_groups + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                       stringency_med + HMI_med + Habitat_closure,   # Human factors   
                     random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                     data = rai_noSame_hunt , method = "ML")


####################################
# Interaction models
global_mod_hunt2 <- rma.mv(yi = yi_mod_lowhigh, V = vi_mod_lowhigh, 
                           mods = ~ Comparison + #Method
                             trophic_groups + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                             stringency_med + HMI_med + Habitat_closure * hunt_bin,  # Human factors   
                           random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                           data = rai_noSame_hunt , method = "ML")

global_mod_hunt3 <- rma.mv(yi = yi_mod_lowhigh, V = vi_mod_lowhigh, 
                           mods = ~ Comparison + #Method
                             trophic_groups + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                             stringency_med + Habitat_closure + HMI_med * hunt_bin,  # Human factors   
                           random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                           data = rai_noSame_hunt , method = "ML")

global_mod_hunt4 <- rma.mv(yi = yi_mod_lowhigh, V = vi_mod_lowhigh, 
                           mods = ~ Comparison + #Method
                             HMI_med +  activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                             stringency_med + Habitat_closure + trophic_groups * hunt_bin,  # Human factors   
                           random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                           data = rai_noSame_hunt , method = "ML")

model.sel(global_mod_hunt, global_mod,global_mod_hunt2, global_mod_hunt3,
          global_mod_hunt4)

############################
# Nocturnality
noct_filtered_hunt <- noct[is.na(noct$Hunted)==F,]
noct_filtered_hunt <- noct_filtered_hunt[noct_filtered_hunt$Hunted!="Unknown",]

noct_filtered_hunt$hunt_bin <- "Yes"
noct_filtered_hunt$hunt_bin[noct_filtered_hunt$Hunted=="No"] <- "No"

noct_filtered_hunt$trophic_groups <- as.factor(noct_filtered_hunt$trophic_groups)
noct_filtered_hunt$trophic_groups <- relevel(noct_filtered_hunt$trophic_groups, "large_herbivore")

#How many projects and project-species does this leave?
length(unique(noct_filtered_hunt$Project.ID))
# Number of species pairs
nrow(noct_filtered_hunt)

global_noc_empirical <- rma.mv(yi = RR_lowhigh , V = Variance_lowhigh,
                               # Additives
                               mods = ~ Comparison + #Method
                                 trophic_groups + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                                 stringency_med + HMI_med + Habitat_closure + hunt_bin # Human factors   
                               , #Interactions
                               #interactions
                               # Not yet done
                               random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                               data = noct_filtered_hunt , method = "ML")


global_noc <- rma.mv(yi = RR_lowhigh , V = Variance_lowhigh,
                     # Additives
                     mods = ~ Comparison + #Method
                       trophic_groups + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                       stringency_med + HMI_med + Habitat_closure # Human factors   
                     , #Interactions
                     #interactions
                     # Not yet done
                     random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                     data = noct_filtered_hunt , method = "ML")




global_noc_empirical2 <- rma.mv(yi = RR_lowhigh , V = Variance_lowhigh,
                                # Additives
                                mods = ~ Comparison + #Method
                                  trophic_groups + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                                  stringency_med + HMI_med + + Habitat_closure * hunt_bin # Human factors   
                                , 
                                random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                                data = noct_filtered_hunt , method = "ML")

global_noc_empirical3 <- rma.mv(yi = RR_lowhigh , V = Variance_lowhigh,
                                # Additives
                                mods = ~ Comparison + #Method
                                  trophic_groups + activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                                  stringency_med + Habitat_closure + HMI_med * hunt_bin  
                                , 
                                random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                                data = noct_filtered_hunt , method = "ML")

global_noc_empirical4 <- rma.mv(yi = RR_lowhigh , V = Variance_lowhigh,
                                # Additives
                                mods = ~ Comparison + #Method
                                  HMI_med +  activity_cycle + det_diet_breadth_n + habitat_breadth_n + # Traits
                                  stringency_med + Habitat_closure + trophic_groups * hunt_bin  
                                , 
                                random = list(~ 1 | Project.ID, ~ 1 | family/Species), 
                                data = noct_filtered_hunt , method = "ML")

model.sel(global_noc_empirical, global_noc , global_noc_empirical2, global_noc_empirical3, global_noc_empirical4)


# Save the top model
saveRDS(global_noc_empirical3, "model_objects/global_noc_empirical3.rds")


