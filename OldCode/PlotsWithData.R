# Transform data set
CalcMean <- function(x){
  means <- x %>% 
    group_by(Temp_value, Prec_value, TransWarm, TransWet) %>% 
    summarize(mean = mean(value), sd = sd(value))
  return(means)
}

PhenoLongTransformed <- pheno.long %>% 
  filter(pheno.unit %in% c("doy", "o.snowmelt", "oCumTemp")) %>% # for origin
  filter(pheno.stage != "r") %>%
  filter(pheno.var != "duration") %>% 
  mutate(pheno.var = factor(pheno.var, levels = c("first", "peak", "end"))) %>% 
  mutate(newTT = plyr::mapvalues(newTT, c("control", "TT2", "TT3", "TT4"), c("Control", "Warm", "Wet", "Warm & wet"))) %>%
  mutate(newTT = factor(newTT, levels = c("Control", "Warm", "Wet", "Warm & wet"))) %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("b", "f", "s"), c("Bud", "Flower", "Seed"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed"))) %>% 
  mutate(pheno.unit = plyr::mapvalues(pheno.unit, c("doy", "o.snowmelt", "oCumTemp"), c("DOY", "Days since SM", "Temp since SM"))) %>%
  mutate(pheno.unit = factor(pheno.unit, levels = c("DOY", "Days since SM", "Temp since SM"))) %>%
  group_by(pheno.unit, pheno.var, pheno.stage) %>% 
  mutate_each(funs(as.factor), species, flowering.time, functionalGroup, occurrence.2) %>% 
  mutate(TransWarm = ifelse(newTT %in% c("Warm", "Warm & wet"), 1, 0)) %>% 
  mutate(TransWet = ifelse(newTT %in% c("Wet", "Warm & wet"), 1, 0)) %>%
  mutate(NoTrans = ifelse(newTT == "control", 1, 0)) %>% 
  do(CalcMean(.))


newplotData <- PhenoLongTransformed %>% 
  mutate(Treatment = paste(TransWarm, TransWet, sep = "")) %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("00", "01", "10", "11"), c("Control", "Wet", "Warm", "Warm & wet"))) %>%
  mutate(Treatment = factor(Treatment, levels = rev(c("Control", "Warm", "Wet", "Warm & wet")))) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = Treatment, shape = pheno.var, color = pheno.var), size = 3) +
  ylab(paste("")) + xlab(paste("Mean value")) +
  scale_shape_manual(name = "", values = c(16,17,15)) +
  scale_colour_manual(name = "", values = c(col.red, col.dblue, col.orange)) +
  #geom_errorbarh(aes(xmin=mean-sd, xmax=mean+sd), height=0.1, color = "gray") +
  facet_grid(~pheno.stage ~ pheno.unit, scales = "free")

newplotData + theme_grey(base_size = 20) + theme(legend.title=element_blank()) 


LevelPlot <- PhenoLongTransformed %>% 
  filter(pheno.unit == "DOY", pheno.stage == "Flower") %>%
  filter(Prec_value != 0.0000000) %>% 
  mutate(Treatment = paste(TransWarm, TransWet, sep = "")) %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("00", "01", "10", "11"), c("Control", "Wet", "Warm", "Warm & wet"))) %>%
  mutate(Treatment = factor(Treatment, levels = rev(c("Control", "Warm", "Wet", "Warm & wet")))) %>% 
  mutate(Temp_value = plyr::mapvalues(Temp_value, c(0, 0.6), c("alpine", "intermediate"))) %>%
  mutate(Temp_value = factor(Temp_value, levels = c("alpine", "intermediate"))) %>%
  mutate(Prec_value = plyr::mapvalues(Prec_value, c(0.2857143, 0.6666667, 1.0000000), c("dry", "intermediate", "wet"))) %>%
  mutate(Prec_value = factor(Prec_value, levels = c("dry", "intermediate", "wet"))) %>%
  ggplot() +
  geom_point(aes(x = mean, y = Treatment, color = Temp_value), size = 3) +
  ylab(paste("")) + xlab(paste("Mean value")) +
  ggtitle("Flowering in DOY") +
  #scale_shape_manual(name = "", values = c(16,17,15)) +
  #scale_colour_manual(name = "Precipitation:", labels = c("dry", "intermediate", "wet"), values = c("lightblue","blue", "darkblue")) +
  #geom_errorbarh(aes(xmin=mean-sd, xmax=mean+sd), height=0.1, color = "gray") +
  facet_grid(~pheno.var) +
  coord_fixed(ratio = 18)
LevelPlot + theme_grey(base_size = 20) + theme(legend.title=element_blank())