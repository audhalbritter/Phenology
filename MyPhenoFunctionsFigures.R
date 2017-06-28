#### COLORS #####
precblue <- c("lightskyblue","royalblue2", "royalblue4")

########################
#####   FIGURES   ######
########################

### FUNCTION TO MAKE A FIGURE FOR TREATMENTS VS. PHENO.STAGE AND PHENO.VAR PER PHENO.UNIT
# dd = data (plasticity, adaptation, warm, warmadapt,...)
# dat = plastic or adapt
# p.unit = DOY, Days, DaysSinceSM, TempSinceSM, DaysSinceSMDest, TempSincesSMDest
PrecLevelPlot <- function(dd, dat, p.unit){
  if(dat == "plastic"){
    if(p.unit == "Days"){
      dd2 <- dd %>% filter(pheno.unit == p.unit, pheno.var != "duration", pheno.stage != "SMBudDest")
    }
    else if(p.unit != "Days"){
      dd2 <- dd %>% filter(pheno.unit == p.unit)
    }
    LevelPlot <- dd2 %>% 
      #mutate(newTT = factor(newTT, levels = rev(c("Control", "Warm", "Wet", "Warm & wet")))) %>%
      group_by(Precipitation_level, newTT, pheno.var, pheno.stage) %>% 
      summarize(mean = mean(value), sd = sd(value)) %>% 
      ggplot(aes(x = newTT, y = mean, color = factor(Precipitation_level), group = factor(Precipitation_level), ymin = (mean - sd), ymax = (mean + sd))) +
      geom_errorbar(color = "grey", width = 0.1, position = position_dodge(width = 0.2)) +
      geom_point(size = 3, position = position_dodge(width = 0.2)) +
      scale_colour_manual(name = "Precipitation:", labels = c("dry", "intermediate", "wet"), values = precblue) +
      ylab("Mean value") + xlab("") +
      ggtitle(p.unit) +
      facet_grid(pheno.stage ~ pheno.var, scales = "free")
  } 
  if(dat == "adapt"){
    if(p.unit == "Days"){
      dd2 <- dd %>% filter(pheno.unit == p.unit, pheno.var != "duration", pheno.stage != "SMBud")
    }
    else if(p.unit != "Days"){
      dd2 <- dd %>% filter(pheno.unit == p.unit)
    }
    LevelPlot <- dd %>% 
      #mutate(newTT = factor(newTT, levels = rev(c("Control", "Warm", "Wet", "Warm & wet")))) %>%
      group_by(destP_level, newTT, pheno.var, pheno.stage) %>% 
      summarize(mean = mean(value), sd = sd(value)) %>% 
      ggplot(aes(x = newTT, y = mean, color = factor(destP_level), group = factor(destP_level), ymin = (mean - sd), ymax = (mean + sd))) +
      geom_errorbar(color = "grey", width = 0.1, position = position_dodge(width = 0.2)) +
      geom_point(size = 3, position = position_dodge(width = 0.2)) +
      scale_colour_manual(name = "Precipitation:", labels = c("dry", "intermediate", "wet"), values = precblue) +
      ylab("Mean value") + xlab("") +
      ggtitle(p.unit) +
      facet_grid(pheno.stage ~ pheno.var, scales = "free")
  }  
  return(LevelPlot)
}

#### PLOT FOR DURATION
# dd = data (plasticity, adaptation, warm, warmadapt,...)
# dat = plastic or adapt
DurationPlot <- function(dd, dat){
  dd <- dd %>% 
    filter(pheno.unit == "DOY", pheno.var == "duration")
  if(dat == "plastic"){
    LevelPlot <- dd %>% 
      #mutate(newTT = factor(newTT, levels = rev(c("Control", "Warm", "Wet", "Warm & wet")))) %>%
      group_by(Precipitation_level, newTT, pheno.stage) %>% 
      summarize(mean = mean(value), sd = sd(value)) %>% 
      ggplot(aes(x = newTT, y = mean, color = factor(Precipitation_level), group = factor(Precipitation_level), ymin = (mean - sd), ymax = (mean + sd))) +
      geom_errorbar(color = "grey", width = 0.1, position = position_dodge(width = 0.2)) +
      geom_point(size = 3, position = position_dodge(width = 0.2)) +
      scale_colour_manual(name = "Precipitation:", labels = c("dry", "intermediate", "wet"), values = precblue) +
      ylab("Mean value") + xlab("") +
      facet_wrap(~pheno.stage)
  } 
  if(dat == "adapt"){
    LevelPlot <- dd %>% 
      #mutate(newTT = factor(newTT, levels = rev(c("Control", "Warm", "Wet", "Warm & wet")))) %>%
      group_by(destP_level, newTT, pheno.stage) %>% 
      summarize(mean = mean(value), sd = sd(value)) %>% 
      ggplot(aes(x = newTT, y = mean, color = factor(destP_level), group = factor(destP_level), ymin = (mean - sd), ymax = (mean + sd))) +
      geom_errorbar(color = "grey", width = 0.1, position = position_dodge(width = 0.2)) +
      geom_point(size = 3, position = position_dodge(width = 0.2)) +
      scale_colour_manual(name = "Precipitation:", labels = c("dry", "intermediate", "wet"), values = precblue) +
      ylab("Mean value") + xlab("") +
      facet_wrap(~pheno.stage)
  } 
  return(LevelPlot)
}


### FUNCTION TO MAKE A FIGURE FOR PREC AND TEMP LEVELS VS. PHENO.STAGE AND PHENO.VAR PER PHENO.UNIT
# dd = data (plasticity, adaptation, warm, warmadapt,...)
# dat = plastic or adapt
# p.unit = DOY, Days since SM, Temp since SM, Days since SM dest or Temp since SM dest
PrecTempLevelPlot <- function(dd, dat, p.unit){
  dd <- subset(dd, pheno.var != "duration")
  if(dat == "plastic"){
    if(p.unit == "DOY"){
      dd <- subset(dd, pheno.unit == "DOY")
    }
    else if(p.unit == "Days since SM"){
      dd <- subset(dd, pheno.unit == "Days since SM")
    }
    else if(p.unit == "Temp since SM"){
      dd <- subset(dd, pheno.unit == "Temp since SM")
    }
    LevelPlot <- dd %>% 
      #mutate(newTT = factor(newTT, levels = rev(c("Control", "Warm", "Wet", "Warm & wet")))) %>%
      group_by(Precipitation_level, Temperature_level, newTT, pheno.var, pheno.stage) %>% 
      summarize(mean = mean(value), sd = sd(value)) %>% 
      ggplot(aes(x = newTT, y = mean, color = factor(Precipitation_level), shape = factor(Temperature_level), group = factor(Precipitation_level), ymin = (mean - sd), ymax = (mean + sd))) +
      geom_errorbar(color = "grey", width = 0.1, position = position_dodge(width = 0.2)) +
      geom_point(size = 3, position = position_dodge(width = 0.2)) +
      scale_shape_manual(name = "Temperature:", labels = c("alpine", "subalpine"), values = c(17,16)) +
      scale_colour_manual(name = "Precipitation:", labels = c("dry", "intermediate"), values = precblue[1:2]) +
      ylab("Mean value") + xlab("") +
      ggtitle(p.unit) +
      facet_grid(pheno.stage ~ pheno.var, scales = "fixed")
  } 
  if(dat == "adapt"){
    if(p.unit == "DOY"){
      dd <- subset(dd, pheno.unit == "DOY")
    }
    else if(p.unit == "Days since SM dest"){
      dd <- subset(dd, pheno.unit == "Days since SM dest")
    }
    else if(p.unit == "Temp since SM dest"){
      dd <- subset(dd, pheno.unit == "Temp since SM dest")
    }
    LevelPlot <- dd %>% 
      #mutate(newTT = factor(newTT, levels = rev(c("Control", "Warm", "Wet", "Warm & wet")))) %>%
      group_by(destP_level, destT_level, newTT, pheno.var, pheno.stage) %>% 
      summarize(mean = mean(value), sd = sd(value)) %>% 
      ggplot(aes(x = newTT, y = mean, color = factor(destP_level), shape = factor(destT_level), group = factor(destP_level), ymin = (mean - sd), ymax = (mean + sd))) +
      geom_errorbar(color = "grey", width = 0.1, position = position_dodge(width = 0.2)) +
      geom_point(size = 3, position = position_dodge(width = 0.2)) +
      scale_shape_manual(name = "Temperature:", labels = c("alpine", "subalpine"), values = c(17,16)) +
      scale_colour_manual(name = "Precipitation:", labels = c("dry", "intermediate"), values = precblue[1:2]) +
      ylab("Mean value") + xlab("") +
      ggtitle(p.unit) +
      facet_grid(pheno.stage ~ pheno.var, scales = "fixed")
  } 
  return(LevelPlot)
}


#### PLOT FOR DURATION PREC AND TEMP LEVEL
# dd = data (plasticity, adaptation, warm, warmadapt,...)
# dat = plastic or adapt
DurationPrecTempPlot <- function(dd, dat){
  dd <- dd %>% 
    filter(pheno.unit == "DOY", pheno.var == "duration")
  if(dat == "plastic"){
    LevelPlot <- dd %>% 
      #mutate(newTT = factor(newTT, levels = rev(c("Control", "Warm", "Wet", "Warm & wet")))) %>%
      group_by(Precipitation_level, Temperature_level, newTT, pheno.stage) %>% 
      summarize(mean = mean(value), sd = sd(value)) %>% 
      ggplot(aes(x = newTT, y = mean, color = factor(Precipitation_level), shape = factor(Temperature_level), group = factor(Precipitation_level), ymin = (mean - sd), ymax = (mean + sd))) +
      geom_errorbar(color = "grey", width = 0.1, position = position_dodge(width = 0.2)) +
      geom_point(size = 3, position = position_dodge(width = 0.2)) +
      scale_shape_manual(name = "Temperature:", labels = c("alpine", "subalpine"), values = c(17,16)) +
      scale_colour_manual(name = "Precipitation:", labels = c("dry", "intermediate"), values = precblue[1:2]) +
      ylab("Mean value") + xlab("") +
      facet_wrap(~pheno.stage)
  } 
  if(dat == "adapt"){
    LevelPlot <- dd %>% 
      #mutate(newTT = factor(newTT, levels = rev(c("Control", "Warm", "Wet", "Warm & wet")))) %>%
      group_by(destP_level, newTT, pheno.stage) %>% 
      summarize(mean = mean(value), sd = sd(value)) %>% 
      ggplot(aes(x = newTT, y = mean, color = factor(destP_level), shape = factor(destT_level), group = factor(destP_level), ymin = (mean - sd), ymax = (mean + sd))) +
      geom_errorbar(color = "grey", width = 0.1, position = position_dodge(width = 0.2)) +
      geom_point(size = 3, position = position_dodge(width = 0.2)) +
      scale_shape_manual(name = "Temperature:", labels = c("alpine", "subalpine"), values = c(17,16)) +
      scale_colour_manual(name = "Precipitation:", labels = c("dry", "intermediate"), values = precblue[1:2]) +
      ylab("Mean value") + xlab("") +
      facet_wrap(~pheno.stage)
  } 
  return(LevelPlot)
}


  