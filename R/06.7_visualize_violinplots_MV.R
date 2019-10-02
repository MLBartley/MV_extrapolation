  ###############################################################################
  ## This script aims to provide summary statistics and visuals for lakes
  ## identified as extrapolations under the 95% cutoff value
  ## (least conservative, most extrapolations)
  ##
  ## Created: March 29, 2019
  ## Updated 1: scaled data to make figures align on one plot
  ###############################################################################
  
  library(magrittr)
  library(ggplot2)
  library(reshape2)
  library(xtable)
  
  
  #load in extrapolation data
  load(file = "./rdata-data/extrap_full") #created in 04.1
  load(file = "./rdata-data/list-into-extrap.Rdata") #created in 04.1
  list2env(savelist,globalenv())
  load(file = "./rdata-data/extrap_values_20180927") #created in 06.1
  
  load("./rdata-data/AllDataList_180627v2.Rdata") #created in cluster.datamanage.R
  load("./rdata-data/MissingDataList_180815.Rdata")
  
  X.scaled <- as.data.frame(scale(X))
  
  ##bring Lake IDs forward
  gg_dat2 <-  cbind(gg_dat[[1]], #extrapolation info
                    lakeID, #lake id
                    X.scaled, #need to put all on same plot
                    Y) #logY 
  
  
  # gg_dat_unscaled <- cbind(gg_dat[[1]], #extrapolation info
  #                          lakeID, #lake id
  #                          X,
  #                          Y)
  
  #isolate which lakes are extrapolation - 95% cutoff
  
  table(factor(gg_dat2$nf_trace))
  
  extrapolated.all <- which(factor(gg_dat2$nf_trace) == 0)
  extrapolated.pred <- extrapolated.all[which(extrapolated.all > max(Sampled))]
  
  whichID.extrap.pred <- gg_dat2$lakeID[extrapolated.pred]
  
  gg_dat2$nft_extrap <- "predicted"
  gg_dat2$nft_extrap[extrapolated.pred] <- "extrapolated"
  
  table(gg_dat2$nft_extrap)
  
xtable(gg_dat_unscaled[extrapolated.pred, colnames(X)[-1]])
  
  # 99% cutoff
  
  table(factor(gg_dat2$nn_trace))
  
  extrapolated.all <- which(factor(gg_dat2$nn_trace) == 0)
  extrapolated.pred <- extrapolated.all[which(extrapolated.all > max(Sampled))]
  
  
  gg_dat2$nnt_extrap <- "predicted"
  gg_dat2$nnt_extrap[extrapolated.pred] <- "extrapolated"
  
  table(gg_dat2$nnt_extrap)
  
 
  
  ## reformat data for all in one plot
  
  gg_dat_melt <- melt(gg_dat2, 
                      id.vars = c("lakeID", "nft_extrap", "nnt_extrap"), 
                                  measure.vars = names(X.scaled)[-c(1, 17, 18)])
                      
  
  
  ## summary statistcs
  
  summary(gg_dat2[extrapolated.pred,])
  
  
  
  
  ## hist/density plots for X variables
  
  Xnames <- colnames(X)[-1] #-1 removes mean vector of 1s
  
  # Xggplot <- list()
  
  # pdf("./figures/extrap_violins.pdf")
  # for(i in 1:length(Xnames)) {
  #   var <- Xnames[i]
  #   
  # q <- ggplot(gg_dat2, aes(x = "All", y = gg_dat2[, var])) +
  #     geom_violin() +
  #     geom_point(aes(color="All"), position = "jitter") +
  #     geom_violin(data=gg_dat2, aes(x = nft_extrap, y = gg_dat2[, var])) +
  #     geom_point(data=gg_dat2, aes(x = nft_extrap, y = gg_dat2[, var], color = nft_extrap), 
  #                position = "jitter") +
  #     scale_color_manual(values = c("black","#F8766D","#619CFF")) +
  #     theme_minimal(base_size = 16) +
  #     ylab(label = var) + 
  #     theme(axis.title.x = element_blank(), legend.title = element_blank())
  # print(q)
  # 
  # }
  # dev.off()
  
  
  ## all covariates scaled on one figure
  
  # all <- ggplot(gg_dat_melt, aes(x = variable, y = value)) +
  #   geom_violin(scale = "width") +
  #   geom_point(data = subset(gg_dat_melt, 
  #                            nft_extrap == "extrapolated" ), 
  #              aes(color= "#F8766D"), 
  #              position = "jitter", 
  #              alpha = 0.35) +
  #   theme_minimal(base_size = 16) +
  #   theme(axis.text.x = element_text(angle = 45,  hjust = 1)) +
  #   # ylab(label = var) + 
  #   theme(axis.title.x = element_blank(), legend.title = element_blank())
  # 
  # 
  # all  + ylim(c(-5, 25))
  
  
  ## parallel coordinates
  library(GGally)
  
  ggparcoord(subset(gg_dat2, nft_extrap == "extrapolated"), 
             # scale = "uniminmax", 
             columns=c(29:44), 
             alphaLines = 0.4, 
             showPoints = T, 
             mapping=aes(color=as.factor(lakeID))) +
  
   ylim(c(-3, 10)) +
  
    
    geom_violin(data = gg_dat_melt, 
                aes_string(group = "variable"), 
                scale = "width", 
                fill = NA) +
    # geom_point(data = gg_dat_melt, aes(y = "value", group = "variable", color= "#F8766D"), 
    #            position = "jitter", 
    #            alpha = 0.35) +
    theme_minimal(base_size = 40) +
    theme(axis.text.x = element_text(angle = 45,  hjust = 1)) +
    # ylab(label = var) + 
    theme(axis.title.x = element_blank(), 
          legend.title = element_blank()) +
    ggtitle("All Extrapolated Lakes - 95% Cutoff")
  
  # ggsave("figures/violin95_MVPV.pdf",
  #        width = 860, height = 573,
  #        units = "mm")
  
  #99%
  ggparcoord(subset(gg_dat2, nnt_extrap == "extrapolated"), 
             # scale = "uniminmax", 
             columns=c(29:43), 
             alphaLines = 0.8, 
             showPoints = T, 
             mapping=aes(color=as.factor(lakeID)), 
             order = "skewness") +
     geom_point(aes(size = 3)) +
    
    ylim(c(-3, 4)) +
    geom_violin(data = gg_dat_melt, 
                aes_string(group = "variable"), 
                scale = "width", 
                fill = NA) +
    # geom_point(data = gg_dat_melt, aes(y = "value", group = "variable", color= "#F8766D"), 
    #            position = "jitter", 
    #            alpha = 0.35) +
    theme_minimal(base_size = 40) +
    theme(axis.text.x = element_text(angle = 45,  hjust = 1)) +
    ylab(label = "Scaled Value") +
    theme(axis.title.x = element_blank(), 
          legend.title = element_blank()) +
    ggtitle("All Extrapolated Lakes - 99% Cutoff") +
     theme(legend.position="none")
  
    
  ggsave("figures/figureS1.pdf",
         width = 860, height = 573,
         units = "mm")
  
## set alpha = 0 
  # ggsave("figures/violin99_MVPV_nolines.pdf",
  #        width = 860, height = 573,
  #        units = "mm")

  
