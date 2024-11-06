###################################---------------------
#  Plots for the manuscript - Run 1_Full_models and 2_Subset_models first

library(lubridate)
library("rnaturalearth")
library("rnaturalearthdata")
library(sf)
library(dplyr)
library(plotrix)
library(MetBrewer)
library(RColorBrewer)
library(corrplot)
library(metafor)

# Create the folder
dir.create("figures")

#Import all camera locations
locs <- read.csv("data/camera_stations_covariates.csv", header=T, check.names = F)
mean_locs <- locs %>% group_by(Project.ID) %>%  summarize(Latitude_mean=mean(Latitude), Longitude_mean=mean(Longitude))

###########################################################
###########################################################
# Figure 1 Map of the world with locations and two example sites

# Set the layout
pdf("figures/Figure_1.pdf", height=5, width=6)
    layout(matrix(c(1,1,1,2,2,1,1,1,3,3), nrow=5))
    par(mar=c(1,1,1,1))
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    # Load the deployment data and take an average XY for each project
    
    plot(st_geometry(world), col="grey", border=F, asp=1, ylim=c(-20,35))
    #points(mean_locs$Longitude_mean,mean_locs$Latitude_mean, pch=19, col="red")
    points(mean_locs$Longitude_mean[mean_locs$Project.ID %in% rai$Project.ID],mean_locs$Latitude_mean[mean_locs$Project.ID %in% rai$Project.ID], 
           pch=21, col=met.brewer("Hiroshige",10)[1],bg=met.brewer("Hiroshige",10)[3])
  
    par(mar=c(5,4,1,1))
    Detections <- read.csv("data/example1_summary.csv", header=T)
    Detections$Date <- ymd(Detections$Date)
    # Make a similar plot in Base R  
    
    plot(Detections$Rate~Detections$Date, type="n", xaxt="n", las=1, ylab=expression(paste("Human detections ", camera^-1, week^-1)),
         xlab="", cex.lab=0.9, ylim=c(0, 0.9))    
    polygon(as.Date(c("2020-04-17","2020-04-17",	"2020-06-12",	"2020-06-12")), c(-10,10,10,-10), col=rgb(1,0,0,0.1), border=F)
    polygon(as.Date(c("2019-04-17","2019-04-17",	"2019-06-12",	"2019-06-12")), c(-10,10,10,-10), col=rgb(0,0,1,0.1), border=F)
    
    mtext("Increase in human activity (across years)",3, line=0.5, cex=0.7)
    
    for(i in 1:nrow(Detections)){
      lines(rep(Detections$Date[i],2), c(0, Detections$Rate[i]), lwd=1)
    }  
    
    
    dates <- seq(
      from = floor_date(as.Date(paste0(substr(min(Detections$Date),1,8),"01")), unit = "month"),
      to =   floor_date(as.Date(paste0(substr(max(Detections$Date),1,8),"01")), unit = "month"),
      length.out=7
      #by = "month",
    )
    #axis(1, dates, labels=substr(dates,1,7), las=2, cex.axis=0.7)  
    axis(1, dates, labels=rep(" ",length(dates)), las=2, cex.axis=0.7)  
    text(dates, par("usr")[3]-0.075, 
         srt = 60, adj = 1, xpd = TRUE,
         labels = substr(dates,1,7), cex = 0.9)
    
   
    text(as.Date("2019-05-08"),0.84, "Control", col=rgb(0,0,1,0.5))
    text(as.Date("2020-05-14"),0.84, "Treatment", col=rgb(1,0,0,0.5))
    
    box()
    
    
    
   
    #Panel 2
    Detections <- read.csv("data/example2_summary.csv", header=T)
    Detections <- Detections
    Detections$Date <- ymd(Detections$Date)
    Detections<- Detections[Detections$Date> ymd("2019-11-01"),]
    Detections<- Detections[Detections$Date< ymd("2020-06-07"),]
    plot(Detections$Rate~Detections$Date, type="n", xaxt="n", las=1, ylab=expression(paste("Human detections ", camera^-1, week^-1)),
         xlab="", cex.lab=0.9, ylim=c(0, 0.9))    
    polygon(as.Date(c("2020-01-07","2020-01-07",	"2020-03-16",	"2020-03-16")), c(-10,10,10,-10), col=rgb(1,0,0,0.1), border=F)
    polygon(as.Date(c("2020-03-17","2020-03-17",	"2020-05-26",	"2020-05-26")), c(-10,10,10,-10), col=rgb(0,0,1,0.1), border=F)
    
    mtext("Decrease in human activity (within year)",3, line=0.5, cex=0.7)
    
    for(i in 1:nrow(Detections)){
      lines(rep(Detections$Date[i],2), c(0, Detections$Rate[i]), lwd=1)
    }  
    
    
     dates <- seq(
      from = floor_date(as.Date(paste0(substr(min(Detections$Date),1,8),"01")), unit = "month"),
      to =   floor_date(as.Date(paste0(substr(max(Detections$Date),1,8),"01")), unit = "month"),
      by = "month"
    )
    dates <- dates[2:length(dates)]
    
    #axis(1, dates, labels=substr(dates,1,7), las=2, cex.axis=0.7)  
    axis(1, dates, labels=rep(" ",length(dates)), las=2, cex.axis=0.7)  
    text(dates, par("usr")[3]-0.075, 
         srt = 60, adj = 1, xpd = TRUE,
         labels = substr(dates,1,7), cex = 0.9)
    
    text(as.Date("2020-04-20"),0.84, "Control", col=rgb(0,0,1,0.5))
    text(as.Date("2020-02-10"),0.84, "Treatment", col=rgb(1,0,0,0.5))
    
    box()

dev.off()



############################################################################################
############################################################################################
# Supplimentary Figure 1
# Correlation between human yi and standarised stringency

# Import project covariates
project_cov <- read.csv("data/project_covariates.csv") %>% 
  filter(Project.ID %in% rai_noSame$Project.ID) %>% # remove any projects that we aren't using
  mutate_if(is.numeric, scale) # scale all numeric covariates
head(project_cov)

par(mfrow=c(1,1))
plot(project_cov$Human_yi ~ project_cov$stringency_med, las=1, asp=1,
     xlab="Standardised median stringency index", ylab="Camera derived change in human activity")
project_cov$stringency_med <- as.vector(project_cov$stringency_med)
tmp <- lm(Human_yi ~ stringency_med, data=project_cov)
summary(tmp)

newx <- data.frame(stringency_med=seq(min(project_cov$stringency_med, na.rm=T),max(project_cov$stringency_med, na.rm=T),by = 0.05), Human_yi=1)
newx$stringency_med <- as.vector(newx$stringency_med)

conf_interval <- predict(tmp, newdata=newx, interval="confidence",
                         level = 0.95)
new_dat <- cbind(newx, conf_interval)


pdf("figures/Figure_Supp_1.pdf", height=8, width=15)
    par(mfrow=c(1,2))
    plot(project_cov$Human_yi ~ project_cov$stringency_med, las=1, asp=1,
         xlab="Standardised median stringency index", ylab="Camera derived change in human activity", pch=19)
    
    lines(new_dat$stringency_med, new_dat$fit, lwd=2)
    polygon(c(new_dat$lwr, rev(new_dat$upr))~ c(new_dat$stringency_med, rev(new_dat$stringency_med)), border=F, col=rgb(0,0,0,0.1))
    
    
    tmp <- project_cov[is.na(project_cov$Human_yi)==F, c("Human_yi", "HMI_med", "RoadDensity_m_km2_med", "GPW_med")] 
    colnames(tmp) <- c("Human_yi", "Human\nmodification", "Road\ndensity", "Population\ndensity")
    M <- cor(tmp)
    corrplot.mixed(M, order = 'AOE', main="")
    corrplot(M, order='AOE', add=T, type="lower", method="number",
             col="black", diag=F, tl.pos="n", cl.pos="n")


dev.off()





############################################################
############################################################
# Supplimentrary figure S2 #########
# Correlation between input variables


# Two part figure - Human variables
#par(mar=c(4,2,2,1))
pdf("figures/Figure_Supp_2.pdf", height=5, width=10)
    par(mfrow=c(1,2))
    length(project_cov$Project.ID)
    tmp  <- project_cov[, c("stringency_med", "HMI_med", "RoadDensity_m_km2_med", "GPW_med")] 
    colnames(tmp) <- c("Lockdown\nstrigency", "Human\nmodification", "Road\ndensity", "Population\ndensity")
    M <- cor(tmp)
    corrplot.mixed(M, order = 'AOE', main="")
    corrplot(M, order='AOE', add=T, type="lower", method="number",
             col="black", diag=F, tl.pos="n", cl.pos="n")
    # Model inputs
    head(rai_noSame)
    tmp  <- rai_noSame[, c("det_diet_breadth_n","habitat_breadth_n","stringency_med", "HMI_med" )] 
    colnames(tmp) <- c("Diet\nbreadth", "Habitat\nbreadth", "Lockdown\nstringency", "Human\nmodification")
    M <- cor(tmp)
    corrplot.mixed(M, order = 'AOE', main="")
    corrplot(M, order='AOE', add=T, type="lower", method="number",
             col="black", diag=F, tl.pos="n", cl.pos="n")

dev.off()


#############################################################
#############################################################
# RAI model output ##########################################

# Responce ratios annd CI's for all species

tmp <- rai_noSame[order(rai_noSame$yi_mod_lowhigh),]
summary(rai_noSame$yi_mod_lowhigh)

length(unique(rai_noSame$Species))
length(unique(rai_noSame$Project.ID))
nrow(rai_noSame)

#########################################
# Coeffcient table
# If required
#full_mod_rai <- readRDS("model_objects/RAI_full_model_object.rds")

tmp_coeffs <- summary(full_mod_rai)
broom::tidy(tmp_coeffs)
#clipr::write_clip(broom::tidy(tmp_coeffs))
tmp_coeffs$beta
tmp$uci <- tmp$yi_mod_lowhigh+tmp$vi_mod_lowhigh
tmp$lci <- tmp$yi_mod_lowhigh-tmp$vi_mod_lowhigh

met.brewer("Hiroshige", 10)[1]
key <- data.frame(trophic_groups= unique(tmp$trophic_groups), 
                  cols=NA)
#cols=as.character(met.brewer(MetBrewer::colorblind_palettes[6], 6)))

# Make carnivores reds
key$cols[key$trophic_groups=="large_carnivore"] <- "#9f201f"
key$cols[key$trophic_groups=="small_carnivore"] <- met.brewer("Hiroshige", 10)[1]

key$cols[key$trophic_groups=="large_herbivore"] <- met.brewer("Hiroshige", 10)[10]
key$cols[key$trophic_groups=="small_herbivore"] <- met.brewer("Hiroshige", 10)[8]

key$cols[key$trophic_groups=="large_omnivore"] <- "#9a7512"
key$cols[key$trophic_groups=="small_omnivore"] <- met.brewer("Hiroshige", 10)[4]


key$names <- c("Large omnivore", "Small herbivore", "Large carnivore",
               "Small carnivore", "Small omnivore", "Large herbivore")
#Order by group
key<- key[order(substr(key$trophic_groups,7,7), key$trophic_groups),]

tmp <- left_join(tmp, key)


#########

pdf("figures/Figure_2.pdf", height=8, width=11)

    #################
    #RAW DATA PANEL
    
    layout(matrix(c(1,1,1,2,2,2,2,2,2,2,
                    3,3,3,3,3,4,4,4,4,4), 2, 10, byrow = TRUE))
    
    par(mar=c(4,6,3,1))
    # Empty box for infographic
    plot(c(-1,0,1), c(-1,0,1), xaxt="n", yaxt="n", type="n",
         ylab="", xlab="")
    axis(2, c(-1,0,1), c("Negative", "None", "Positive"), las=1, cex.axis=1.2)
    abline(h=0, lty=2)
    
    # Up arrow
    polygon(c(-0.3, -0.3, -0.6,  0, 0.6, 0.3, 0.3)-0.3,
            c( 0.1,  0.5, 0.5, 0.9, 0.5, 0.5, 0.1 ),
            border=NA, col=rgb(0,0,0,0.1))
    
    # Down arrow
    polygon(c(-0.3, -0.3, -0.6,  0, 0.6, 0.3, 0.3)-0.3,
            c( 0.1,  0.5, 0.5, 0.9, 0.5, 0.5, 0.1 )*-1,
            border=NA, col=rgb(0,0,0,0.1))
    
    text(0,1,"Increasing human activity leads to:", cex=1.2)
    text(0.7,0.5,"Increased\nanimal\nactivity", cex=1.4)
    text(0.7,-0.5,"Decreased\nanimal\nactivity", cex=1.4)
    
    mtext("Effect direction", 2, line=4, cex=1.2)
    mtext(expression(bold("Predictions")), 3, line=1, cex=1.2 )
    
    # Add a counter
    tmp$counter <- 1:nrow(tmp)
    par(cex.lab=1.4)
    par(mar=c(2,4,1,1))
    plot(1:nrow(tmp), tmp$yi_mod_lowhigh, #uiw=tmp$vi_mod_lowhigh, 
         pch=19, cex=1,
         xlab="Project-Species",ylab="", xaxt="n",  #
         ylim=c(min(tmp$lci, na.rm=T), max(tmp$uci, na.rm=T)),
         xlim=c(0, nrow(tmp)+80), las=1,
         col=tmp$cols, cex.axis=1.4, type="n")
    mtext("Raw treatment effect sizes", 2, cex=1.2, line=2.5)
    
    # GLOBAL MEAN 95% quantiles
    polygon(c(nrow(tmp)+20, nrow(tmp)+1000, nrow(tmp)+1000, nrow(tmp)+20),
            c(-100,-100,100,100), col=rgb(0,0,0,0.05), border=NA)
    
    
    lines( c(nrow(tmp)+70,nrow(tmp)+70), as.numeric(quantile(tmp$yi_mod_lowhigh, prob=c(.025,0.975))), lwd=2)
    points( nrow(tmp)+70,mean(tmp$yi_mod_lowhigh), pch=19, col="black", cex=2)
    text(nrow(tmp)+70, 5,"Mean", cex=1.2 )
    
    for(i in 1:nrow(tmp))
    {
      lines( c(i,i),c(tmp$uci[i],tmp$lci[i]), col=tmp$cols[i], lwd=0.55)
    }
    
    
    
    # Species specific highlights
    # Mule deer
    tmp_sp <- tmp[tmp$Species=="Odocoileus hemionus",]
    for(i in 1:nrow(tmp_sp))
    {
      lines(c(tmp_sp$counter[i],710), c(tmp_sp$yi_mod_lowhigh[i], 4), lwd=1, lty=3, col=rgb(0,0,0))
    }
    text(710,4, "Mule deer", cex=1.4,pos=3)
    
    
    # Wolverine
    tmp_sp <- tmp[tmp$Species=="Gulo gulo",]
    for(i in 1:nrow(tmp_sp))
    {
      lines(c(tmp_sp$counter[i],200), c(tmp_sp$yi_mod_lowhigh[i], -4), lwd=1, lty=3, col=rgb(0,0,0))
    }
    text(200,-4, "Wolverine", cex=1.4,pos=1)
    
    abline(h=0, lty=2)
    
    points(1:nrow(tmp),tmp$yi_mod_lowhigh, #uiw=tmp$vi_mod_lowhigh, 
           pch=15, cex=0.06, col=rgb(0,0,0))
    
    legend("topleft", legend=c(key$names), 
           cex=1.2, col=c(key$cols) ,lty=1,
           lwd=3)
    
    
    
    ####################
    # Global model coeffs
    par(mar=c(10,6,0.5,1))
    
    tmp2 <- full_mod_rai
    results <- data.frame(
      Treatment = factor(row.names(tmp2$beta)),
      Estimate = tmp2$beta,
      CI_l = tmp2$ci.lb,
      CI_u = tmp2$ci.ub
    )
    
    # ggplot(results, aes(x=Estimate, y=Treatment)) +
    #   geom_point(shape=15, size=3) +
    #   geom_errorbarh(aes(xmin=CI_l, xmax=CI_u), height=0) +
    #   geom_vline(xintercept=0, linetype="dashed") +
    #   theme_bw()
    row.names(tmp2$beta)
    # Add a grouping variable
    results$group <- NA
    results$group[results$Treatment %in% c("trophic_groupslarge_carnivore","trophic_groupslarge_omnivore",
                                           "trophic_groupssmall_carnivore", "trophic_groupssmall_herbivore",
                                           "trophic_groupssmall_omnivore")] <- "Trophic group"
    
    results$group[results$Treatment %in% c("ComparisonYear")] <- "Comparison"
    results$group[results$Treatment %in% c("intrcpt")] <- "Intercept"
    results$group[results$Treatment %in% c("activity_cyclemix", "activity_cyclenocturnal_only")] <- "Activity"
    results$group[results$Treatment %in% c("det_diet_breadth_n")] <- "Diet"
    results$group[results$Treatment %in% c("habitat_breadth_n")] <- "Habitat"
    results$group[results$Treatment %in% c("stringency_med")] <- "Stringency"
    results$group[results$Treatment %in% c("Habitat_closureOpen")] <- "Habitat closure"
    results$group[results$Treatment %in% c("HMI_med")] <- "Human modification"
    
    # Label for the individual vairables
    results$label <- ""
    results$label[results$Treatment %in% c("ComparisonYear")] <- "Year comparison"
    results$label[results$Treatment %in% c("intrcpt")] <- "Intercept"
    results$label[results$Treatment %in% c("activity_cyclemix")] <- "Cathemeral"
    results$label[results$Treatment %in% c("activity_cyclenocturnal_only")] <- "Nocturnal"
    results$label[results$Treatment %in% c("det_diet_breadth_n")] <- "Diet breadth"
    results$label[results$Treatment %in% c("habitat_breadth_n")] <- "Habitat breadth"
    results$label[results$Treatment %in% c("stringency_med")] <- "Stringency"
    results$label[results$Treatment %in% c("Habitat_closureOpen")] <- "Open habitat"
    results$label[results$Treatment %in% c("HMI_med")] <- "Human\nmodification index"
    
    results$label[results$Treatment %in% c("trophic_groupslarge_carnivore")] <- "Large carnivore"
    results$label[results$Treatment %in% c("trophic_groupslarge_omnivore")] <- "Large omnivore"
    results$label[results$Treatment %in% c("trophic_groupssmall_carnivore")] <- "Small carnivore"
    results$label[results$Treatment %in% c("trophic_groupssmall_herbivore")] <- "Small herbivore"
    results$label[results$Treatment %in% c("trophic_groupssmall_omnivore")] <- "Small omnivore"
    
    # Add pvals
    results$pval <- tmp2$pval
    results$group
    tmp2 <- results %>% group_by(group) %>% dplyr::summarise(p=min(pval))
    tmp2 <- tmp2[order(tmp2$p),]
    colnames(tmp2)[2] <- "best_p" 
    tmp2$best_p[tmp2$group=="Intercept"] <- 0
    # Order reuslts by p-values
    results <- left_join(results, tmp2)
    results <- results[order(results$best_p),]
    
    # Colour the output
    results$col <- NA
    results$col[results$pval<0.05] <- "black"
    results$col[results$pval>0.05 & results$pval<0.1] <- "grey50"
    results$col[results$pval>0.1] <- "grey80"
    
    # Create custom spacing (so you can see groups)
    locs <- c(1,
              3,4,5,6,7,
              9,
              11,
              13,14,
              16,
              18,
              20,
              22)
    plot(locs, results$Estimate, ylim=c(min(results$CI_l), max(results$CI_u)), pch=19, cex=2.5,
         xaxt="n", las=1, ylab="", xlab="", col=results$col, cex.axis=1.2)
    # Add tick marks
    axis(1, at=locs, labels=rep("", times=length(locs)), las=2)
    # Add angled labels
    text(locs, par("usr")[3]-0.045, 
         srt = 60, adj = 1, xpd = TRUE,
         labels = results$label, cex = 1.4)
    mtext("Regression coefficients", 2, cex=1.2, line=3.5)
    for(i in 1:length(locs))
    {
      lines(c(locs[i],locs[i]), c(results$CI_l[i],results$CI_u[i]), lwd=2, col=results$col[i])
    }
    abline(h=0, lty=2, col=rgb(0,0,0,0.5))
    
    # Legend
    legend("topright", legend=c("Strong (<0.05)", "Marginal (0.05-0.10)", "Weak (>0.10)"), 
           pch=19, cex=1.4,pt.cex=2.5, col=c("black", "grey50", "grey80")
    )
    
    
    ######################
    # Interaction elements
    # PLOTTING THE TOP INTERACTION MODEL
    int_mod <- readRDS( "model_objects/RAI_int_model_2_object.rds")
    
    # TROPHIC LEVEL 
    # To make a custom prediction simply change these values
    # I will first make the all zero (to get the carnivore open habitat prediction)
    test <- predict(int_mod, addx = T)
    test2 <- test$X
    
    pred_dat <- as.data.frame(test2[1:120,2:ncol(test2)])
    
    pred_dat[1:nrow(pred_dat),] <- 0
    
    # Make open 1
    pred_dat[,"Habitat_closureOpen"] <- 1
    pred_dat[,"ComparisonYear"] <- 1
    
    # add a 1 in the second colum for large herbivores
    
    pred_dat[21:40,"trophic_groupslarge_carnivore"] <- 1
    pred_dat[21:40,"trophic_groupslarge_carnivore:HMI_med"]  <- seq(min(rai_noSame$HMI_med),max(rai_noSame$HMI_med), length.out=20 )
    
    pred_dat[41:60,"trophic_groupslarge_omnivore"] <- 1
    pred_dat[41:60,"trophic_groupslarge_omnivore:HMI_med"]   <- seq(min(rai_noSame$HMI_med),max(rai_noSame$HMI_med), length.out=20 )
    
    pred_dat[61:80,"trophic_groupssmall_carnivore"] <- 1
    pred_dat[61:80,"trophic_groupssmall_carnivore:HMI_med"]  <- seq(min(rai_noSame$HMI_med),max(rai_noSame$HMI_med), length.out=20 )
    
    pred_dat[81:100,"trophic_groupssmall_herbivore"] <- 1
    pred_dat[81:100,"trophic_groupssmall_herbivore:HMI_med"] <- seq(min(rai_noSame$HMI_med),max(rai_noSame$HMI_med), length.out=20 )
    
    pred_dat[101:120,"trophic_groupssmall_omnivore"] <- 1
    pred_dat[101:120,"trophic_groupssmall_omnivore:HMI_med"] <- seq(min(rai_noSame$HMI_med),max(rai_noSame$HMI_med), length.out=20 )
    
    
    pred_dat$HMI_med <- rep(seq(min(rai_noSame$HMI_med),max(rai_noSame$HMI_med), length.out=20 ), times=6)
    
    # Turn back to matrix
    pred_dat <- as.matrix(pred_dat)
    pred_res <- as.data.frame(predict(int_mod, newmods  = pred_dat, transf=exp))
    pred_res$trophic_group <- rep(c("Large herbivore", "Large carnivore", "Large omnivore", "Small carnivore", "Small herbivore", "Small omnivore"), each=20)
    pred_res$hmi_med <- rep(seq(min(rai_noSame$HMI_med),max(rai_noSame$HMI_med), length.out=20 ), times=6)
    
    
    par(mar=c(4,6,0.5,1))
    plot(pred_res$hmi_med, pred_res$pred, type="n", xlab="",
         ylab="", ylim=c(0.55,1.55), las=1, cex.axis=1.4,
         yaxt="n")
    axis(2, at=seq(0.6, 1.4, by=0.2), labels=seq(-40, 40, by=20), las=1)
    mtext("Predicted activity change (%)", 2, cex=1.2, line=3.2)
    mtext("Human modification index", 1, cex=1.2, line=2.6)
    
    i <- 1
    for(i in 1:length(unique(pred_res$trophic_group)))
    {
      group <- unique(pred_res$trophic_group)[i]
      tmp_col <- key$cols[key$names==unique(pred_res$trophic_group)[i]]
      lines(pred_res$hmi_med[pred_res$trophic_group==group], pred_res$pred[pred_res$trophic_group == group],
            col=tmp_col, lty=1, lwd=3)
    }
    legend("topleft",key$names, lty=1,col=key$cols, lwd=3,
           cex=1.2 )      
    abline(h=1,lty=2)
    max(pred_res$pred[pred_res$trophic_group=="Large omnivore"]);min(pred_res$pred[pred_res$trophic_group=="Large omnivore"]) 
    tmp <- pred_res %>% group_by(trophic_group) %>% summarize(min=min(pred), max=max(pred))
    tmp$delta <- ((tmp$max-tmp$min)/tmp$min)*100 
    mean(tmp$delta[tmp$trophic_group!="Large omnivore"])

dev.off()



##############################################
##### SUPPLIMENTARY FIGURE 4
# ALL THE ACTIVITY PREDICTION PLOTS
# Make predcitions for open habitat (1) , Season (0), Acivity_diurnal(0), Large herbivore (0)

pdf(height=6, width=11, "figures/Figure_supp_3.pdf")
    # HMI
    par(mfrow=c(2,4))
    
    # Make a dummy dataframe
    test <- predict(full_mod_rai, addx = T,  transf=exp)
    test2 <- test$X
    pred_dat <- as.data.frame(test2[1:20,2:ncol(test2)])
    
    # Empty the dataframe
    pred_dat[1:nrow(pred_dat),] <- 0
    pred_dat[,"HMI_med"] <- seq(min(rai_noSame$HMI_med),max(rai_noSame$HMI_med), length.out=20 )
    pred_dat[,"Habitat_closureOpen"] <- 1
    pred_dat[,"trophic_groupssmall_herbivore"] <- 1
    
    pred_dat <- as.matrix(pred_dat)
    pred_res <- as.data.frame(predict(full_mod_rai, newmods  = pred_dat, transf=exp))
    pred_res$X <- seq(min(rai_noSame$HMI_med),max(rai_noSame$HMI_med), length.out=20 )
    par(mar=c(8,4,1,1))
    plot(pred_res$X, pred_res$pred, 
         xlab="HMI", las=1, ylab="Predicted activity change (%)", type="l", lwd=2,
         ylim=c(0.6, 1.8), yaxt="n")
    axis(2, at=c(0.6,0.8,1,1.2,1.4,1.6), labels=c("-40","-20", "0", "+20", "+40" ,"+60"), las=1)
    polygon(c(pred_res$X, rev(pred_res$X)), c(pred_res$ci.lb, rev(pred_res$ci.ub)), col=rgb(0,0,0,0.1), border=F)
    
    # Percentage change
    min(pred_res$pred); max(pred_res$pred)
    # 33.3% increase (low to high)
    
    # TROPHIC LEVEL 
    # To make a custom prediction simply change these values
    # I will first make the all zero (to get the carnivore open habitat prediction)
    pred_dat <- as.data.frame(test2[1:6,2:ncol(test2)])
    pred_dat[1:nrow(pred_dat),] <- 0
    
    # Make open 1
    pred_dat[,"Habitat_closureOpen"] <- 1
    # add a 1 in the second colum for large herbivores
    pred_dat[2,"trophic_groupslarge_carnivore"] <- 1
    pred_dat[3,"trophic_groupslarge_omnivore"] <- 1
    pred_dat[4,"trophic_groupssmall_carnivore"] <- 1
    pred_dat[5,"trophic_groupssmall_herbivore"] <- 1
    pred_dat[6,"trophic_groupssmall_omnivore"] <- 1
    
    # Turn back to matrix
    pred_dat <- as.matrix(pred_dat)
    pred_res <- as.data.frame(predict(full_mod_rai, newmods  = pred_dat, transf=exp))
    pred_res$trophic_group <- c("Large herbivore", "Large carnivore", "Large omnivore", "Small carnivore", "Small herbivore", "Small omnivore")
    pred_res <- pred_res[c(1,5,3,6,2,4),] 
    
    par(mar=c(8,4,1,1))
    plotCI(c(0.9, 1.1, 1.9,2.1,2.9,3.1), pred_res$pred, ui=pred_res$ci.ub, li=pred_res$ci.lb, pch=19,
           xlim=c(0.5, 3.5), sfrac=0.000001,
           xaxt="n", xlab="", las=1, ylab="Predicted activity change (%)",
           ylim=c(0.6, 1.8), yaxt="n")
    axis(2, at=c(0.6,0.8,1,1.2,1.4,1.6), labels=c("-40","-20", "0", "+20", "+40" ,"+60"), las=1)
    axis(1, at=c(0.9, 1.1, 1.9,2.1,2.9,3.1), labels=pred_res$trophic_group, las=2,
         cex.axis=1)
    abline(h=1, lty=2)
    
    # % change
    # Large herb
    1-pred_res$pred[1]
    1-pred_res$pred[6]
    
    
    
    # HABITAT OPENNESS
    pred_dat <- as.data.frame(test2[1:2,2:ncol(test2)])
    pred_dat[1:nrow(pred_dat),] <- 0
    
    # Make open 1
    pred_dat[2,"Habitat_closureOpen"] <- 1
    pred_dat[,"trophic_groupssmall_herbivore"] <- 1
    
    # add a 1 in the second colum for large herbivores
    # Turn back to matrix
    pred_dat <- as.matrix(pred_dat)
    pred_res <- as.data.frame(predict(full_mod_rai, newmods  = pred_dat, transf=exp))
    pred_res$closure <- c("Closed", "Open")
    
    par(mar=c(8,4,1,1))
    plotCI(1:nrow(pred_res), pred_res$pred, ui=pred_res$ci.ub, li=pred_res$ci.lb, pch=19,
           xlim=c(0.5, nrow(pred_res)+0.5), sfrac=0.000001,
           xaxt="n", xlab="", las=1, ylab="Predicted activity change (%)",
           ylim=c(0.6, 1.8), yaxt="n")
    axis(2, at=c(0.6,0.8,1,1.2,1.4,1.6), labels=c("-40","-20", "0", "+20", "+40" ,"+60"), las=1)
    axis(1, at=1:2, labels=pred_res$closure, las=2,
         cex.axis=1)
    abline(h=1, lty=2)
    
    # % change
    # Closed
    pred_res$pred[1]
    # Open
    pred_res$pred[2]
    
    
    # ACTIVITY CYCLE
    pred_dat <- as.data.frame(test2[1:3,2:ncol(test2)])
    pred_dat[1:nrow(pred_dat),] <- 0
    
    # Make open 1
    pred_dat[2,"activity_cyclemix"] <- 1
    pred_dat[3,"activity_cyclenocturnal_only"] <- 1
    pred_dat[,"trophic_groupssmall_herbivore"] <- 1
    pred_dat[,"Habitat_closureOpen"] <- 1
    
    # add a 1 in the second colum for large herbivores
    # Turn back to matrix
    pred_dat <- as.matrix(pred_dat)
    pred_res <- as.data.frame(predict(full_mod_rai, newmods  = pred_dat, transf=exp))
    pred_res$activity <- c("Diurnal", "Mixed", "Nocturnal")
    
    par(mar=c(8,4,1,1))
    plotCI(1:nrow(pred_res), pred_res$pred, ui=pred_res$ci.ub, li=pred_res$ci.lb, pch=19,
           xlim=c(0.5, nrow(pred_res)+0.5), sfrac=0.000001,
           xaxt="n", xlab="", las=1, ylab="Predicted activity change (%)",
           ylim=c(0.6, 1.8), yaxt="n")
    axis(2, at=c(0.6,0.8,1,1.2,1.4,1.6), labels=c("-40","-20", "0", "+20", "+40" ,"+60"), las=1)
    axis(1, at=1:3, labels=pred_res$activity, las=2,
         cex.axis=1)
    abline(h=1, lty=2)
    
    # COMPARISON
    pred_dat <- as.data.frame(test2[1:2,2:ncol(test2)])
    pred_dat[1:nrow(pred_dat),] <- 0
    
    # Make open 1
    pred_dat[2,"ComparisonYear"] <- 1
    pred_dat[,"trophic_groupssmall_herbivore"] <- 1
    pred_dat[,"Habitat_closureOpen"] <- 1
    
    # add a 1 in the second colum for large herbivores
    # Turn back to matrix
    pred_dat <- as.matrix(pred_dat)
    pred_res <- as.data.frame(predict(full_mod_rai, newmods  = pred_dat, transf=exp))
    pred_res$label <- c("Season", "Year")
    
    par(mar=c(8,4,1,1))
    plotCI(1:nrow(pred_res), pred_res$pred, ui=pred_res$ci.ub, li=pred_res$ci.lb, pch=19,
           xlim=c(0.5, nrow(pred_res)+0.5), sfrac=0.000001,
           xaxt="n", xlab="", las=1, ylab="Predicted activity change (%)",
           ylim=c(0.6, 1.8), yaxt="n")
    axis(2, at=c(0.6,0.8,1,1.2,1.4,1.6), labels=c("-40","-20", "0", "+20", "+40" ,"+60"), las=1)
    axis(1, at=1:nrow(pred_res), labels=pred_res$label, las=2,
         cex.axis=1)
    abline(h=1, lty=2)
    
    
    # habitat breadth
    pred_dat <- as.data.frame(test2[1:20,2:ncol(test2)])
    
    # Make everything zeros
    pred_dat[1:nrow(pred_dat),] <- 0
    # FILL HMI Column with span of HMI
    pred_dat[,"habitat_breadth_n"] <- seq(min(rai_noSame$habitat_breadth_n),max(rai_noSame$habitat_breadth_n), length.out=20 )
    pred_dat[,"Habitat_closureOpen"] <- 1
    pred_dat[,"trophic_groupssmall_herbivore"] <- 1
    
    pred_dat <- as.matrix(pred_dat)
    pred_res <- as.data.frame(predict(full_mod_rai, newmods  = pred_dat, transf=exp))
    pred_res$X <- seq(min(rai_noSame$habitat_breadth_n),max(rai_noSame$habitat_breadth_n), length.out=20 )
    par(mar=c(8,4,1,1))
    plot(pred_res$X, pred_res$pred, 
         xlab="Habitat breadth", las=1, ylab="Predicted activity change (%)", type="l", lwd=2,
         ylim=c(0.6, 1.8),yaxt="n")
    
    axis(2, at=c(0.6,0.8,1,1.2,1.4,1.6), labels=c(-40,-20, 0, +20, +40, +60), las=1)
    polygon(c(pred_res$X, rev(pred_res$X)), c(pred_res$ci.lb, rev(pred_res$ci.ub)), col=rgb(0,0,0,0.1), border=F)
    
    
    # det_diet_breadth_n
    pred_dat <- as.data.frame(test2[1:20,2:ncol(test2)])
    
    # Make everything zeros
    pred_dat[1:nrow(pred_dat),] <- 0
    # FILL HMI Column with span of HMI
    pred_dat[,"det_diet_breadth_n"] <- seq(min(rai_noSame$det_diet_breadth_n),max(rai_noSame$det_diet_breadth_n), length.out=20 )
    pred_dat[,"Habitat_closureOpen"] <- 1
    pred_dat[,"trophic_groupssmall_herbivore"] <- 1
    
    pred_dat <- as.matrix(pred_dat)
    pred_res <- as.data.frame(predict(full_mod_rai, newmods  = pred_dat, transf=exp))
    pred_res$X <- seq(min(rai_noSame$det_diet_breadth_n),max(rai_noSame$det_diet_breadth_n), length.out=20 )
    par(mar=c(8,4,1,1))
    plot(pred_res$X, pred_res$pred, 
         xlab="Diet breadth", las=1, ylab="Predicted activity change (%)", type="l", lwd=2,
         ylim=c(0.6, 1.8), yaxt="n")
    axis(2, at=c(0.6,0.8,1,1.2,1.4,1.6), labels=c("-40","-20", "0", "+20", "+40" ,"+60"), las=1)
    
    
    polygon(c(pred_res$X, rev(pred_res$X)), c(pred_res$ci.lb, rev(pred_res$ci.ub)), col=rgb(0,0,0,0.1), border=F)
    
    
    # Stringency
    pred_dat <- as.data.frame(test2[1:20,2:ncol(test2)])
    
    # Make everything zeros
    pred_dat[1:nrow(pred_dat),] <- 0
    # FILL HMI Column with span of HMI
    pred_dat[,"stringency_med"] <- seq(min(rai_noSame$stringency_med),max(rai_noSame$stringency_med), length.out=20 )
    pred_dat[,"Habitat_closureOpen"] <- 1
    pred_dat[,"trophic_groupssmall_herbivore"] <- 1
    
    pred_dat <- as.matrix(pred_dat)
    pred_res <- as.data.frame(predict(full_mod_rai, newmods  = pred_dat, transf=exp))
    pred_res$X <- seq(min(rai_noSame$stringency_med),max(rai_noSame$stringency_med), length.out=20 )
    par(mar=c(8,4,1,1))
    plot(pred_res$X, pred_res$pred, 
         xlab="Stringency", las=1, ylab="Predicted activity change (%)", type="l", lwd=2,
         ylim=c(0.6, 1.8), yaxt="n")
    axis(2, at=c(0.6,0.8,1,1.2,1.4,1.6), labels=c("-40","-20", "0", "+20", "+40" ,"+60"), las=1)
    
    polygon(c(pred_res$X, rev(pred_res$X)), c(pred_res$ci.lb, rev(pred_res$ci.ub)), col=rgb(0,0,0,0.1), border=F)
    # # Hidden
dev.off()


#####################################################################################
#####################################################################################
#####################################################################################
# NOCTURNALITY ######################################################################

length(unique(paste(noct$Project.ID,noct$Species)))
length(unique(noct$Project.ID))
length(unique(noct$Species))

# If required
#full_mod_noc <- readRDS("results/rds_files/NOC_full_model_object.rds")
#null_mod_noc <- readRDS("results/rds_files/NOC_null_model_object.rds")

tmp_coeffs <- summary(full_mod_noc)
broom::tidy(tmp_coeffs)
clipr::write_clip(broom::tidy(tmp_coeffs))

tmp <- full_mod_noc
results <- data.frame(
  Treatment = factor(row.names(tmp$beta)),
  Estimate = tmp$beta,
  CI_l = tmp$ci.lb,
  CI_u = tmp$ci.ub
)

tmp <- noct[order(noct$RR_lowhigh),]
tmp$uci <- tmp$RR_lowhigh+tmp$Variance_lowhigh
tmp$lci <- tmp$RR_lowhigh-tmp$Variance_lowhigh
tmp <- left_join(tmp, key)

# Figure 3
pdf("figures/Figure_3.pdf", height=8, width=11)

    #################
    #RAW DATA PANEL
    layout(matrix(c(1,1,1,2,2,2,2,2,2,2,
                    3,3,3,3,4,4,4,5,5,5), 2, 10, byrow = TRUE))
    
    par(mar=c(4,6,3,1))
    # Empty box for infographic
    plot(c(-1,0,1), c(-1,0,1), xaxt="n", yaxt="n", type="n",
         ylab="", xlab="")
    axis(2, c(-1,0,1), c("Negative", "None", "Positive"), las=1, cex.axis=1.2)
    abline(h=0, lty=2)
    
    # Up arrow
    polygon(c(-0.3, -0.3, -0.6,  0, 0.6, 0.3, 0.3)-0.3,
            c( 0.1,  0.5, 0.5, 0.9, 0.5, 0.5, 0.1 ),
            border=NA, col=rgb(0,0,0,0.1))
    
    # Down arrow
    polygon(c(-0.3, -0.3, -0.6,  0, 0.6, 0.3, 0.3)-0.3,
            c( 0.1,  0.5, 0.5, 0.9, 0.5, 0.5, 0.1 )*-1,
            border=NA, col=rgb(0,0,0,0.1))
    
    text(0,1,"Increasing human activity leads to:", cex=1.2)
    text(0.7,0.5,"Increased\nanimal\nnocturnality", cex=1.4)
    text(0.7,-0.5,"Decreased\nanimal\nnocturnality", cex=1.4)
    
    mtext("Effect direction", 2, line=4, cex=1.2)
    mtext(expression(bold("Predictions")), 3, line=1, cex=1.2 )
    
    # Add a counter
    tmp$counter <- 1:nrow(tmp)
    
    
    par(mar=c(2,4,1,1))
    plot(1:nrow(tmp), tmp$RR_lowhigh, #uiw=tmp$vi_mod_lowhigh, 
         pch=19, cex=0.5,
         xlab="Project-Species",ylab="", xaxt="n",
         #ylim=c(min(tmp$lci, na.rm=T), max(tmp$uci, na.rm=T)),
         ylim=c(-2.4,2.9),
         xlim=c(0, nrow(tmp)+60), las=1,
         col=tmp$cols, type="n",
         cex.axis=1.4)
    mtext("Raw treatment effect sizes", 2, cex=1.2, line=2.5)
    
    #axis(2, cex.axis=3)
    # GLOBAL MEAN 95% quantiles
    polygon(c(nrow(tmp)+20, nrow(tmp)+1000, nrow(tmp)+1000, nrow(tmp)+20),
            c(-100,-100,100,100), col=rgb(0,0,0,0.05), border=NA)
    
    # GLOBAL MEAN 95% quantiles
    lines( c(nrow(tmp)+50,nrow(tmp)+50), as.numeric(quantile(tmp$RR_lowhigh, prob=c(.025,0.975))), lwd=2)
    points( nrow(tmp)+50,mean(tmp$RR_lowhigh), pch=19, col="black", cex=2)
    text(nrow(tmp)+50, 2,"Mean", cex=1.2 )
    
    for(i in 1:nrow(tmp))
    {
      lines( c(i,i),c(tmp$uci[i],tmp$lci[i]), col=tmp$cols[i])
    }
    
    # # Highlight some individual species
    # # Negative species
    # sp_loc <- 5
    # lines(c(sp_loc,75), c(tmp[sp_loc,]$RR_lowhigh, -4), lwd=1, lty=2)
    # text(75,-4, bquote(italic(.(tmp[sp_loc,]$Species))), cex=1,pos=4)
    # 
    # sp_loc <- 50
    # lines(c(sp_loc,125), c(tmp[sp_loc,]$RR_lowhigh, -2.5), lwd=1, lty=2)
    # text(125,-2.5, bquote(italic(.(tmp[sp_loc,]$Species))), cex=1,pos=4)
    # 
    # # middle species
    # 
    # sp_loc <- 245
    # lines(c(sp_loc,300), c(tmp[sp_loc,]$RR_lowhigh, -1.8), lwd=1, lty=2)
    # text(300,-1.8, bquote(italic(.(tmp[sp_loc,]$Species))), cex=1,pos=4)
    # 
    # sp_loc <- 255
    # lines(c(sp_loc,200), c(tmp[sp_loc,]$RR_lowhigh, 1.8), lwd=1, lty=2)
    # text(200,1.8, bquote(italic(.(tmp[sp_loc,]$Species))), cex=1,pos=2)
    # 
    # # high species
    # sp_loc <- 495
    # lines(c(sp_loc,420), c(tmp[sp_loc,]$RR_lowhigh, 4), lwd=1, lty=2)
    # text(420,4, bquote(italic(.(tmp[sp_loc,]$Species))), cex=1,pos=2)
    # 
    # sp_loc <- 445
    # lines(c(sp_loc,335), c(tmp[sp_loc,]$RR_lowhigh, 3), lwd=1, lty=2)
    # text(335,3, bquote(italic(.(tmp[sp_loc,]$Species))), cex=1,pos=2)
    # 
    
    # Option 2 Species specific
    # Gray fox
    tmp_sp <- tmp[tmp$Species=="Urocyon cinereoargenteus",]
    i <- 1
    for(i in 1:nrow(tmp_sp))
    {
      lines(c(tmp_sp$counter[i],300), c(tmp_sp$RR_lowhigh[i], 2.5), lwd=1, lty=3, col=rgb(0,0,0))
    }
    text(300,2.5, "Gray fox", cex=1.4,pos=3)
    
    
    # Bobcat
    tmp_sp <- tmp[tmp$Species=="Lynx rufus",]
    i <- 1
    for(i in 1:nrow(tmp_sp))
    {
      lines(c(tmp_sp$counter[i],100), c(tmp_sp$RR_lowhigh[i], -2), lwd=1, lty=3, col=rgb(0,0,0))
    }
    text(100,-2, "Bobcat", cex=1.4,pos=1)
    abline(h=0, lty=2)
    
    points(1:nrow(tmp),tmp$RR_lowhigh, #uiw=tmp$vi_mod_lowhigh, 
           pch=15, cex=0.12, col=rgb(0,0,0))
    
    legend("topleft", legend=c(key$names), 
           cex=1.2, col= key$cols ,lty=1,
           lwd=3, bg="white")
    
    ####################
    # Global model coeffs
    par(mar=c(8,6,1,1))
    tmp2 <- full_mod_noc
    results <- data.frame(
      Treatment = factor(row.names(tmp2$beta)),
      Estimate = tmp2$beta,
      CI_l = tmp2$ci.lb,
      CI_u = tmp2$ci.ub
    )
    
    row.names(tmp2$beta)
    
    # Add a grouping variable
    results$group <- NA
    results$group[results$Treatment %in% c("trophic_groupslarge_carnivore","trophic_groupslarge_omnivore",
                                           "trophic_groupssmall_carnivore", "trophic_groupssmall_herbivore",
                                           "trophic_groupssmall_omnivore")] <- "Trophic group"
    
    results$group[results$Treatment %in% c("ComparisonYear")] <- "Comparison"
    results$group[results$Treatment %in% c("intrcpt")] <- "Intercept"
    results$group[results$Treatment %in% c("activity_cyclemix", "activity_cyclenocturnal_only","activity_cyclediurnal_only")] <- "Activity"
    
    results$group[results$Treatment %in% c("det_diet_breadth_n")] <- "Diet"
    results$group[results$Treatment %in% c("habitat_breadth_n")] <- "Habitat"
    results$group[results$Treatment %in% c("stringency_med")] <- "Stringency"
    results$group[results$Treatment %in% c("Habitat_closureOpen")] <- "Habitat closure"
    results$group[results$Treatment %in% c("HMI_med")] <- "Human modification"
    
    # Label for the individual vairables
    results$label <- ""
    results$label[results$Treatment %in% c("ComparisonYear")] <- "Year comparison"
    results$label[results$Treatment %in% c("intrcpt")] <- "Intercept"
    results$label[results$Treatment %in% c("activity_cyclemix")] <- "Cathemeral"
    results$label[results$Treatment %in% c("activity_cyclenocturnal_only")] <- "Nocturnal"
    results$label[results$Treatment %in% c("activity_cyclediurnal_only")] <- "Diurnal"
    
    results$label[results$Treatment %in% c("det_diet_breadth_n")] <- "Diet breadth"
    results$label[results$Treatment %in% c("habitat_breadth_n")] <- "Habitat breadth"
    results$label[results$Treatment %in% c("stringency_med")] <- "Stringency"
    results$label[results$Treatment %in% c("Habitat_closureOpen")] <- "Open habitat"
    results$label[results$Treatment %in% c("HMI_med")] <- "Human\nmodification index"
    
    results$label[results$Treatment %in% c("trophic_groupslarge_carnivore")] <- "Large carnivore"
    results$label[results$Treatment %in% c("trophic_groupslarge_omnivore")] <- "Large omnivore"
    results$label[results$Treatment %in% c("trophic_groupssmall_carnivore")] <- "Small carnivore"
    results$label[results$Treatment %in% c("trophic_groupssmall_herbivore")] <- "Small herbivore"
    results$label[results$Treatment %in% c("trophic_groupssmall_omnivore")] <- "Small omnivore"
    
    # Add pvals
    results$pval <- tmp2$pval
    results$group
    tmp2 <- results %>% group_by(group) %>% dplyr::summarise(p=min(pval))
    tmp2 <- tmp2[order(tmp2$p),]
    colnames(tmp2)[2] <- "best_p" 
    tmp2$best_p[tmp2$group=="Intercept"] <- 0
    # Order reuslts by p-values
    results <- left_join(results, tmp2)
    results <- results[order(results$best_p),]
    
    results$col <- NA
    results$col[results$pval<0.05] <- "black"
    results$col[results$pval>0.05 & results$pval<0.1] <- "grey50"
    results$col[results$pval>0.1] <- "grey80"
    
    
    # Create custom spacing (so you can see groups)
    # Create custom spacing (so you can see groups)
    locs <- c(1,
              3,
              5,6,7,8,9,
              11,13, 
              15,16,
              18,
              20,
              22
    )
    
    
    
    plot(locs, results$Estimate, ylim=c(min(results$CI_l), max(results$CI_u)), pch=19, cex=2.5,
         xaxt="n", las=1, ylab="", xlab="", col=results$col,
         cex.axis=1.2)
    
    # Add tick marks
    axis(1, at=locs, labels=rep("", times=length(locs)), las=2)
    # Add angled labels
    text(locs, par("usr")[3]-0.025, 
         srt = 60, adj = 1, xpd = TRUE,
         labels = results$label, cex = 1.1)
    
    
    mtext("Regression coefficients", 2, cex=1.2, line=3.5)
    
    for(i in 1:length(locs))
    {
      lines(c(locs[i],locs[i]), c(results$CI_l[i],results$CI_u[i]), lwd=2, col=results$col[i])
    }
    abline(h=0, lty=2, col=rgb(0,0,0,0.5))
    
    legend("topleft", legend=c("Strong (<0.05)", "Marginal (0.05-0.10)", "Weak (>0.10)"), 
           pch=19, cex=1.4,pt.cex=2.5, col=c("black", "grey50", "grey80"))
    
    
    ##################
    par(mar=c(8,4.5,1,1))
    int_mod <- full_mod_noc_int2
    #int_mod <- readRDS("model_objects/NOC_int_model_2_object.rds")
    
    # TROPHIC LEVEL 
    # Dummy dataframe
    test <- predict(int_mod, addx = T)
    test2 <- test$X
    pred_dat <- as.data.frame(test2[1:120,2:ncol(test2)])
    # Make them 0
    pred_dat[1:nrow(pred_dat),] <- 0
    
    # Make open 1
    pred_dat[,"Habitat_closureOpen"] <- 1
    # add a 1 in the second column for large herbivores
    pred_dat[,"ComparisonYear"] <- 1
    
    pred_dat[21:40,"trophic_groupslarge_carnivore"] <- 1
    pred_dat[21:40,"trophic_groupslarge_carnivore:HMI_med"]  <- seq(min(noct$HMI_med),max(noct$HMI_med), length.out=20 )
    
    pred_dat[41:60,"trophic_groupslarge_omnivore"] <- 1
    pred_dat[41:60,"trophic_groupslarge_omnivore:HMI_med"]   <- seq(min(noct$HMI_med),max(noct$HMI_med), length.out=20 )
    
    pred_dat[61:80,"trophic_groupssmall_carnivore"] <- 1
    pred_dat[61:80,"trophic_groupssmall_carnivore:HMI_med"]  <- seq(min(noct$HMI_med),max(noct$HMI_med), length.out=20 )
    
    pred_dat[81:100,"trophic_groupssmall_herbivore"] <- 1
    pred_dat[81:100,"trophic_groupssmall_herbivore:HMI_med"] <- seq(min(noct$HMI_med),max(noct$HMI_med), length.out=20 )
    
    pred_dat[101:120,"trophic_groupssmall_omnivore"] <- 1
    pred_dat[101:120,"trophic_groupssmall_omnivore:HMI_med"] <- seq(min(noct$HMI_med),max(noct$HMI_med), length.out=20 )
    
    
    pred_dat$HMI_med <- rep(seq(min(noct$HMI_med),max(noct$HMI_med), length.out=20 ), times=6)
    
    # Turn back to matrix
    pred_dat <- as.matrix(pred_dat)
    pred_res <- as.data.frame(predict(int_mod, newmods  = pred_dat, transf=exp))
    pred_res$trophic_group <- rep(c("Large herbivore", "Large carnivore", "Large omnivore", "Small carnivore", "Small herbivore", "Small omnivore"), each=20)
    pred_res$hmi_med <- rep(seq(min(noct$HMI_med),max(noct$HMI_med), length.out=20 ), times=6)
    
    tmp <- pred_res %>% group_by(trophic_group) %>% summarize(min=min(pred), max=max(pred))
    tmp$delta <- ((tmp$max-tmp$min)/tmp$min)*100 
    mean(tmp$delta[tmp$trophic_group!="Large carnivore"])
    
    
    plot(pred_res$hmi_med, pred_res$pred, type="n", xlab="Human modification index",
         ylab="Predicted nocturnality change (%)", las=1, cex.axis=1.2, cex.lab=1.4, yaxt="n")
    axis(2, at=seq(0.95,1.25,by=0.05), labels=seq(-5,25, by=5), las=1)
    i <- 1
    for(i in 1:length(unique(pred_res$trophic_group)))
    {
      group <- unique(pred_res$trophic_group)[i]
      tmp_col <- key$cols[key$names==unique(pred_res$trophic_group)[i]]
      lines(pred_res$hmi_med[pred_res$trophic_group==group], pred_res$pred[pred_res$trophic_group == group],
            col=tmp_col, lty=1, lwd=3)
    }
    legend("topleft",key$names, lty=1,col=key$cols, lwd=3,
           cex=1.1 )      
    abline(h=1,lty=2)
    
    
    # Top Hunting model
    # If required
    #global_noc_empirical3 <- readRDS("model_objects/global_noc_empirical3.rds")
    
    # TROPHIC LEVEL 
    # Dummy dataframe
    test <- predict(global_noc_empirical3, addx = T)
    test2 <- test$X
    pred_dat <- as.data.frame(test2[1:40,2:ncol(test2)])
    # Make 0
    pred_dat[1:nrow(pred_dat),] <- 0
    colnames(pred_dat)
    
    # Make open 1
    pred_dat[,"Habitat_closureOpen"] <- 1
    pred_dat[,"ComparisonYear"] <- 1
    
    # add a 1 in the second colum for large herbivores
    
    pred_dat[1:20,"HMI_med"]  <- seq(min(noct$HMI_med),max(noct$HMI_med), length.out=20)
    pred_dat[21:40,"HMI_med"]  <- seq(min(noct$HMI_med),max(noct$HMI_med), length.out=20)
    pred_dat[21:40,"hunt_binYes"]  <- 1
    
    pred_dat[21:40,"HMI_med:hunt_binYes"]  <- seq(min(noct$HMI_med),max(noct$HMI_med), length.out=20)
    
    # Turn back to matrix
    pred_dat <- as.matrix(pred_dat)
    pred_res <- as.data.frame(predict(global_noc_empirical3, newmods  = pred_dat, transf=exp))
    pred_res$hunt_binYes <- rep(c(0,1), each=20)
    pred_res$hmi_med <- rep(seq(min(noct$HMI_med),max(noct$HMI_med), length.out=20 ), times=2)
    
    plot(pred_res$hmi_med, pred_res$pred, type="n", xlab="Human modification index",
         ylab="Predicted nocturnality change (%)", las=1, cex.axis=1.2, cex.lab=1.4, yaxt="n")
    axis(2, at=seq(0.95,1.25,by=0.05), labels=seq(-5,25, by=5), las=1)
    
    #mtext("Predicted nocturnality change", 2, cex=1.2, line=3.7)
    
    #polygon(c(pred_res$hmi_med[pred_res$hunt_binYes==1], rev(pred_res$hmi_med[pred_res$hunt_binYes==1])),
    #    c(pred_res$ci.lb[pred_res$hunt_binYes==1], rev(pred_res$ci.ub[pred_res$hunt_binYes==1])), col=paste0(as.character(met.brewer("Isfahan2", 3))[1],50),
    #    border=NA)
    
    
    lines(pred_res$hmi_med[pred_res$hunt_binYes==1], pred_res$pred[pred_res$hunt_binYes==1],
          col=as.character(met.brewer("Isfahan2", 3))[1], lty=1, lwd=3)
    
    
    #polygon(c(pred_res$hmi_med[pred_res$hunt_binYes==0], rev(pred_res$hmi_med[pred_res$hunt_binYes==0])),
    #        c(pred_res$ci.lb[pred_res$hunt_binYes==0], rev(pred_res$ci.ub[pred_res$hunt_binYes==0])), 
    #        col=paste0(as.character(met.brewer("Isfahan2", 3))[3],50),
    #        border=NA)
    
    
    lines(pred_res$hmi_med[pred_res$hunt_binYes==0], pred_res$pred[pred_res$hunt_binYes==0],
          col=as.character(met.brewer("Isfahan2", 3))[3], lty=1, lwd=3)
    
    
    
    
    legend("topleft",c("Hunted", "Non-hunted"), lty=1,
           col=c(as.character(met.brewer("Isfahan2", 3))[1], as.character(met.brewer("Isfahan2", 3))[3]), lwd=3,
           cex=1.2 )      
    abline(h=1,lty=2)
    
    tmp <- pred_res %>% group_by(hunt_binYes) %>% summarize(min=min(pred), max=max(pred))
    tmp$delta <- ((tmp$max-tmp$min)/tmp$min)*100 

dev.off()




#################################
#################################
# ALL EFFECT PLOTS #############


test <- predict(full_mod_noc, addx = T)
test2 <- test$X

pdf(height=6, width=11, "Figures/Figure_supp_4.pdf")
  # HMI
  par(mfrow=c(2,4))
  pred_dat <- as.data.frame(test2[1:20,2:ncol(test2)])
  
  # Make everything zeros
  pred_dat[1:nrow(pred_dat),] <- 0
  # FILL HMI Column with span of HMI
  pred_dat[,"HMI_med"] <- seq(min(noct$HMI_med),max(noct$HMI_med), length.out=20 )
  pred_dat[,"Habitat_closureOpen"] <- 1
  pred_dat[,"trophic_groupssmall_herbivore"] <- 1
  
  pred_dat <- as.matrix(pred_dat)
  pred_res <- as.data.frame(predict(full_mod_noc, newmods  = pred_dat, transf=exp))
  pred_res$X <- seq(min(noct$HMI_med),max(noct$HMI_med), length.out=20 )
  par(mar=c(8,4,1,1))
  plot(pred_res$X, pred_res$pred, 
       xlab="HMI", las=1, ylab="Predicted nocturnality change (%)", type="l", lwd=2,
       ylim=c(0.7, 1.4), yaxt="n")
  axis(2, at=seq(0.6,1.6,by=0.2), labels=seq(-40, 60, by=20), las=1)
  polygon(c(pred_res$X, rev(pred_res$X)), c(pred_res$ci.lb, rev(pred_res$ci.ub)), col=rgb(0,0,0,0.1), border=F)
  
  # Fold change 
  min(pred_res$pred); max(pred_res$pred)
  # 19.3% increase
  
  # TROPHIC LEVEL 
  # To make a custom prediction simply change these values
  # I will first make the all zero (to get the carnivore open habitat prediction)
  pred_dat <- as.data.frame(test2[1:6,2:ncol(test2)])
  pred_dat[1:nrow(pred_dat),] <- 0
  
  # Make open 1
  pred_dat[,"Habitat_closureOpen"] <- 1
  # add a 1 in the second colum for large herbivores
  pred_dat[2,"trophic_groupslarge_carnivore"] <- 1
  pred_dat[3,"trophic_groupslarge_omnivore"] <- 1
  pred_dat[4,"trophic_groupssmall_carnivore"] <- 1
  pred_dat[5,"trophic_groupssmall_herbivore"] <- 1
  pred_dat[6,"trophic_groupssmall_omnivore"] <- 1
  
  # Turn back to matrix
  pred_dat <- as.matrix(pred_dat)
  pred_res <- as.data.frame(predict(full_mod_noc, newmods  = pred_dat, transf=exp))
  pred_res$trophic_group <- c("Large herbivore", "Large carnivore", "Large omnivore", "Small carnivore", "Small herbivore", "Small omnivore")
  pred_res <- pred_res[c(1,5,3,6,2,4),] 
  
  par(mar=c(8,4,1,1))
  plotCI(c(0.8, 1.2, 1.8,2.2,2.8,3.2), pred_res$pred, ui=pred_res$ci.ub, li=pred_res$ci.lb, pch=19,
         xlim=c(0.5, 3.5), sfrac=0.000001,
         xaxt="n", xlab="", las=1, ylab="Predicted nocturnality change (%)",
         ylim=c(0.7, 1.4), yaxt="n")
  axis(2, at=seq(0.6,1.6,by=0.2), labels=seq(-40, 60, by=20), las=1)
  axis(1, at=c(0.8, 1.1, 1.8,2.2,2.8,3.2), labels=pred_res$trophic_group, las=2,
       cex.axis=1)
  abline(h=1, lty=2)
  pred_res$pred[pred_res$trophic_group=="Large carnivore"]-mean(pred_res$pred[pred_res$trophic_group!="Large carnivore"])
  
  # HABITAT OPENNESS
  pred_dat <- as.data.frame(test2[1:2,2:ncol(test2)])
  pred_dat[1:nrow(pred_dat),] <- 0
  
  # Make open 1
  pred_dat[2,"Habitat_closureOpen"] <- 1
  pred_dat[,"trophic_groupssmall_herbivore"] <- 1
  
  # add a 1 in the second colum for large herbivores
  # Turn back to matrix
  pred_dat <- as.matrix(pred_dat)
  pred_res <- as.data.frame(predict(full_mod_noc, newmods  = pred_dat, transf=exp))
  pred_res$closure <- c("Closed", "Open")
  
  par(mar=c(8,4,1,1))
  plotCI(1:nrow(pred_res), pred_res$pred, ui=pred_res$ci.ub, li=pred_res$ci.lb, pch=19,
         xlim=c(0.5, nrow(pred_res)+0.5), sfrac=0.000001,
         xaxt="n", xlab="", las=1, ylab="Predicted nocturnality change (%)",
         ylim=c(0.7, 1.4), yaxt="n")
  axis(2, at=seq(0.6,1.6,by=0.2), labels=seq(-40, 60, by=20), las=1)
  axis(1, at=1:2, labels=pred_res$closure, las=2,
       cex.axis=1)
  abline(h=1, lty=2)
  
  
  
  # ACTIVITY CYCLE
  pred_dat <- as.data.frame(test2[1:3,2:ncol(test2)])
  pred_dat[1:nrow(pred_dat),] <- 0
  
  # Make open 1
  pred_dat[2,"activity_cyclemix"] <- 1
  pred_dat[3,"activity_cyclediurnal_only"] <- 1
  pred_dat[,"trophic_groupssmall_herbivore"] <- 1
  pred_dat[,"Habitat_closureOpen"] <- 1
  
  # add a 1 in the second colum for large herbivores
  # Turn back to matrix
  pred_dat <- as.matrix(pred_dat)
  pred_res <- as.data.frame(predict(full_mod_noc, newmods  = pred_dat, transf=exp))
  pred_res$activity <- c("Nocturnal", "Mixed", "Diurnal")
  
  par(mar=c(8,4,1,1))
  plotCI(1:nrow(pred_res), pred_res$pred, ui=pred_res$ci.ub, li=pred_res$ci.lb, pch=19,
         xlim=c(0.5, nrow(pred_res)+0.5), sfrac=0.000001,
         xaxt="n", xlab="", las=1, ylab="Predicted nocturnality change (%)",
         ylim=c(0.7, 1.4), yaxt="n")
  axis(2, at=seq(0.6,1.6,by=0.2), labels=seq(-40, 60, by=20), las=1)
  axis(1, at=1:3, labels=pred_res$activity, las=2,
       cex.axis=1)
  abline(h=1, lty=2)
  
  # COMPARISON
  pred_dat <- as.data.frame(test2[1:2,2:ncol(test2)])
  pred_dat[1:nrow(pred_dat),] <- 0
  
  # Make open 1
  pred_dat[2,"ComparisonYear"] <- 1
  pred_dat[,"trophic_groupssmall_herbivore"] <- 1
  pred_dat[,"Habitat_closureOpen"] <- 1
  
  # add a 1 in the second colum for large herbivores
  # Turn back to matrix
  pred_dat <- as.matrix(pred_dat)
  pred_res <- as.data.frame(predict(full_mod_noc, newmods  = pred_dat, transf=exp))
  pred_res$label <- c("Season", "Year")
  
  par(mar=c(8,4,1,1))
  plotCI(1:nrow(pred_res), pred_res$pred, ui=pred_res$ci.ub, li=pred_res$ci.lb, pch=19,
         xlim=c(0.5, nrow(pred_res)+0.5), sfrac=0.000001,
         xaxt="n", xlab="", las=1, ylab="Predicted nocturnality change (%)",
         ylim=c(0.7, 1.4), yaxt="n")
  axis(2, at=seq(0.6,1.6,by=0.2), labels=seq(-40, 60, by=20), las=1)
  axis(1, at=1:nrow(pred_res), labels=pred_res$label, las=2,
       cex.axis=1)
  abline(h=1, lty=2)
  
  pred_res$pred[1]-pred_res$pred[2]
  
  # habitat breadth
  pred_dat <- as.data.frame(test2[1:20,2:ncol(test2)])
  
  # Make everything zeros
  pred_dat[1:nrow(pred_dat),] <- 0
  # FILL HMI Column with span of HMI
  pred_dat[,"habitat_breadth_n"] <- seq(min(noct$habitat_breadth_n),max(noct$habitat_breadth_n), length.out=20 )
  pred_dat[,"Habitat_closureOpen"] <- 1
  pred_dat[,"trophic_groupssmall_herbivore"] <- 1
  
  pred_dat <- as.matrix(pred_dat)
  pred_res <- as.data.frame(predict(full_mod_noc, newmods  = pred_dat, transf=exp))
  pred_res$X <- seq(min(noct$habitat_breadth_n),max(noct$habitat_breadth_n), length.out=20 )
  par(mar=c(8,4,1,1))
  plot(pred_res$X, pred_res$pred, 
       xlab="Habitat breadth", las=1, ylab="Predicted nocturnality change (%)",
       ylim=c(0.7, 1.4), yaxt="n", type="l", lwd=2)
  axis(2, at=seq(0.6,1.6,by=0.2), labels=seq(-40, 60, by=20), las=1)
  polygon(c(pred_res$X, rev(pred_res$X)), c(pred_res$ci.lb, rev(pred_res$ci.ub)), col=rgb(0,0,0,0.1), border=F)
  
  
  # det_diet_breadth_n
  pred_dat <- as.data.frame(test2[1:20,2:ncol(test2)])
  
  # Make everything zeros
  pred_dat[1:nrow(pred_dat),] <- 0
  # FILL HMI Column with span of HMI
  pred_dat[,"det_diet_breadth_n"] <- seq(min(noct$det_diet_breadth_n),max(noct$det_diet_breadth_n), length.out=20 )
  pred_dat[,"Habitat_closureOpen"] <- 1
  pred_dat[,"trophic_groupssmall_herbivore"] <- 1
  
  pred_dat <- as.matrix(pred_dat)
  pred_res <- as.data.frame(predict(full_mod_noc, newmods  = pred_dat, transf=exp))
  pred_res$X <- seq(min(noct$det_diet_breadth_n),max(noct$det_diet_breadth_n), length.out=20 )
  par(mar=c(8,4,1,1))
  plot(pred_res$X, pred_res$pred, 
       xlab="Diet breadth", las=1, ylab="Predicted nocturnality change (%)",
       ylim=c(0.7, 1.4), yaxt="n", type="l", lwd=2)
  axis(2, at=seq(0.6,1.6,by=0.2), labels=seq(-40, 60, by=20), las=1)
  polygon(c(pred_res$X, rev(pred_res$X)), c(pred_res$ci.lb, rev(pred_res$ci.ub)), col=rgb(0,0,0,0.1), border=F)
  
  
  # Stringency
  pred_dat <- as.data.frame(test2[1:20,2:ncol(test2)])
  
  # Make everything zeros
  pred_dat[1:nrow(pred_dat),] <- 0
  # FILL HMI Column with span of HMI
  pred_dat[,"stringency_med"] <- seq(min(noct$stringency_med),max(noct$stringency_med), length.out=20 )
  pred_dat[,"Habitat_closureOpen"] <- 1
  pred_dat[,"trophic_groupssmall_herbivore"] <- 1
  
  pred_dat <- as.matrix(pred_dat)
  pred_res <- as.data.frame(predict(full_mod_noc, newmods  = pred_dat, transf=exp))
  pred_res$X <- seq(min(noct$stringency_med),max(noct$stringency_med), length.out=20 )
  par(mar=c(8,4,1,1))
  plot(pred_res$X, pred_res$pred, 
       xlab="Stringency", las=1, ylab="Predicted nocturnality change (%)",
       ylim=c(0.7, 1.4), yaxt="n", type="l", lwd=2)
  axis(2, at=seq(0.6,1.6,by=0.2), labels=seq(-40, 60, by=20), las=1)
  polygon(c(pred_res$X, rev(pred_res$X)), c(pred_res$ci.lb, rev(pred_res$ci.ub)), col=rgb(0,0,0,0.1), border=F)

dev.off()

