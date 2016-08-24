#### FUNCTION TO MAKE PLOTS ####

MakePlot2 <- function(dd, control.turf, p.var, p.stage, p.unit, title.unit){
  g <- dd %>%
    filter(pheno.var == p.var) %>%
    filter(pheno.stage == p.stage) %>%
    filter(pheno.unit == p.unit) %>% 
    mutate(newlabel = plyr::mapvalues(newTT, c("TT2", "TT3", "TT4"), c("Warm", "Wet", "Warm & wet"))) %>%  
    mutate(newlabel = factor(newlabel, levels = c("Warm", "Wet", "Warm & wet"))) %>% 
    ggplot(aes(x = control, y = value)) + 
    geom_errorbar(aes(ymin=value-sdev, ymax=value+sdev), width=0.1, color = "gray") +
    geom_errorbarh(aes(xmin=control-control_sd, xmax=control+control_sd), height=0.1, color = "gray") +
    geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed") +
    scale_shape_manual(name = "Temperature:", labels = c("alpine", "subalpine"), values = c(17,16)) +
    theme(legend.position= "top") + 
    ylab(paste("Transplant in ", title.unit)) + xlab(paste("Control in ", title.unit)) +
    geom_text(aes(x = control, y = value, label=species),hjust=0, vjust=0) +
    facet_wrap(~ newlabel) +
    background_grid(major = "xy", minor = "none") +

    if(control.turf == "destination"){
      g + geom_point(aes(x = control, y = value, color = factor(destP_level), shape = factor(destT_level)), size = 4) +
        scale_colour_manual(name = "Precipitation:", labels = c("dry", "intermediate", "wet"), values = c("lightblue","blue", "darkblue"))
      
  }
  else if(control.turf == "origin"){
      g + geom_point(aes(color = factor(Precipitation_level), shape = factor(Temperature_level)), size = 4) +
      scale_colour_manual(name = "Precipitation:", labels = c("very dry", "dry", "intermediate", "wet"), values = c("white", "lightblue","blue", "darkblue"))
     
  }
}

library("cowplot")
MakePlot2(pheno.treat.dest, "destination", "first", "f", "doy", "DOY")
PhenologyPlot1 <- MakePlot2(pheno.treat.orig, "origin", "peak", "f", "doy", "DOY")
# + theme_grey(base_size = 24)+theme(legend.position= "top")


### Plot for functionalGroup
MakePlotFuncGroup(pheno.treat.orig, "origin", "duration", "f", "doy", "DOY")

MakePlotFuncGroup <- function(dd, control.turf, p.var, p.stage, p.unit, title.unit){
  if(control.turf == "destination"){
    dd %>%
      filter(pheno.var == p.var) %>%
      filter(pheno.stage == p.stage) %>%
      filter(pheno.unit == p.unit) %>% 
      ggplot() + 
      geom_point(aes(x = control, y = value, color = factor(functionalGroup)), size = 4) +
      #geom_errorbar(aes(x = control, ymin=control-TT2_sd, ymax=control+TT2_sd), width=0.1, color = "gray") +
      geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed") +
      theme(legend.position= "top") +
      ylab("Transplant") + xlab("Control") +
      geom_text(aes(x = control, y = value, label=species),hjust=0, vjust=0) +
      facet_wrap(~ newTT)
  }
  else if(control.turf == "origin"){
    dd %>%
      filter(pheno.var == p.var) %>%
      filter(pheno.stage == p.stage) %>%
      filter(pheno.unit == p.unit) %>% 
      ggplot() + 
      geom_point(aes(x = control, y = value, color = factor(functionalGroup)), size = 4) +
      #geom_errorbar(aes(x = control, ymin=control-TT2_sd, ymax=control+TT2_sd), width=0.1, color = "gray") +
      geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed") +
      theme(legend.position= "top") +
      ylab(paste("Transplant in ", title.unit)) + xlab(paste("Control in ", title.unit)) +
      geom_text(aes(x = control, y = value, label=species),hjust=0, vjust=0) +
      facet_wrap(~ newTT)
  }
}