#### ANALYSE DATA ####

### ORIGIN (doy, o.snowmelt, oCumTemp)
pheno.long %>% 
  filter(pheno.unit == "doy", pheno.stage == "f") %>% 
  filter(pheno.var != "duration") %>% 
  group_by(pheno.var) %>% 
  mutate_each(funs(as.factor), species, Temp_value, Prec_value, destTemp_value, destPrec_value, newTT) %>% 
  do(GlmerModelAndPredict(.)) %>%

       
# Function to perform Model and predict new values
GlmerModelAndPredict <- function(dd){
  modelfit <- glmer(value ~ Temp_value * Prec_value + newTT + (1|species), data = dd, family = "poisson")
  newdat <- expand.grid(
    Temp_value=c(0, 0.6)
    , Prec_value=c(0, 0.2857143, 0.6666667, 1)
    , newTT=c("control", "TT2", "TT3", "TT4")
    , value = 0
  )
  mm <- model.matrix(terms(modelfit), newdat)
  newdat$value <- predict(modelfit,newdat,re.form=NA, type="response")  # re.form=NA without random part; re.form=NULL with random part
  
  means <- newdat %>% 
    group_by(newTT) %>% 
    summarize(mean = mean(value), sd = sd(value))
  return(means)
}
  




# Model and Predict Data by Hand
pheno <- pheno.long %>% filter(pheno.stage =="f", pheno.var == "duration", pheno.unit == "doy")
fit01 <- glmer((value) ~ Temp_value * Prec_value + newTT + (1|species), pheno, family = "poisson")
disp(fit01, pheno)
pheno$newFactor <- rnorm(nrow(pheno))
fit01 <- glmer((value) ~ Temp_value + Prec_value + newTT + (1|species) + (1|newFactor), pheno, family = "poisson")
summary(fit01)
newdat <- expand.grid(
  Temp_value=c(0, 0.6)
  , Prec_value=c(0, 0.2857143, 0.6666667, 1)
  , newTT=c("control", "TT2", "TT3", "TT4")
  , value = 0
)
mm <- model.matrix(terms(fit01), newdat)
newdat$value <- predict(fit01,newdat,re.form=NA, type="response")

newdat %>% 
  group_by(newTT) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  ggplot() +
  geom_point(aes(x= mean, y = newTT)) + 
  geom_errorbarh(aes(x = mean, xmin=-sd, xmax=sd)) +
  facet_wrap(~pheno.unit)




