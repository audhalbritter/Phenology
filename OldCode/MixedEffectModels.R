### LMER WITH MODEL SELECTION ####

### DESTINATION #####
# get data
pheno.long <- GetData(pheno15[!is.na(pheno15$doy),], "peak", "nr.s", "destination", "doy")
MakeFigure(pheno.long, "destination", "newTT", "DFl_DAY", "Duration flowering in days")

head(pheno.long)
par(mfrow=c(1,1))
hist((pheno.long$value))
ModelSelection(pheno.long, "norm")

ddd <- pheno.long.f %>% filter(var == "nr.f", stage == "first", unit == "doy")

MakeFigure(pheno.long, "destination", "newTT", "PSs_Dest", "Peak Seed set in days of year")
ddd$value

pheno.long <- GetData(pheno15[!is.na(pheno15$doy),], "peak", "nr.s", "origin", "doy")
#### NORMAL ####
hist((pheno.long$value))
fit01 <- lmer(value ~ newTT + (1|destSiteID) + (1|variable), pheno.long)
fit02 <- lmer(value ~ 1 + (1|destSiteID) + (1|variable), pheno.long)
modsel(list(fit01, fit02), 1000)
fix.check(fit01)
fit01
ranef(fit01)

fit01 <- lmer(value ~ newTT*functionalGroup + (1|destSiteID) + (1|variable), pheno.long)
fit02 <- lmer(value ~ newTT+functionalGroup + (1|destSiteID) + (1|variable), pheno.long)
fit03 <- lmer(value ~ newTT + (1|destSiteID) + (1|variable), pheno.long)
fit04 <- lmer(value ~ functionalGroup + (1|destSiteID) + (1|variable), pheno.long)
fit05 <- lmer(value ~ 1 + (1|destSiteID) + (1|variable), pheno.long)
modsel(list(fit01, fit02, fit03, fit04, fit05), 1000)
fix.check(fit01)
fit01
fix <- fixef(fit01)
data.frame(herbs = c(fix[1],fix[2],fix[3],fix[4]), 
           graminoids = c(fix[1]+fix[5],fix[2]+fix[6],fix[7]+fix[3],fix[4]+fix[8]), 
           row.names = c("TTC", "TT2", "TT3", "TT4"))




#### TEST ONLY FOR CONTROL PLOTS ####
#### FUNCTIONAL GROUP
fit01 <- lmer(value ~ functionalGroup + (1|destSiteID) + (1|variable), ddd[ddd$newTT=="control",])
fit02 <- lmer(value ~ 1 + (1|destSiteID) + (1|variable), ddd[ddd$newTT=="control",])
modsel(list(fit01, fit02), 1000)
fit01

#### FLOWIERING TIME
fit01 <- lmer(value ~ flowering.time + (1|destSiteID) + (1|variable), ddd[ddd$newTT=="control",])
fit02 <- lmer(value ~ 1 + (1|destSiteID) + (1|variable), ddd[ddd$newTT=="control",])
modsel(list(fit01, fit02), 1000)
fit01

fit01 <- lmer(value ~ occurrence.2 + (1|destSiteID) + (1|variable), ddd[ddd$newTT=="control",])
fit02 <- lmer(value ~ 1 + (1|destSiteID) + (1|variable), ddd[ddd$newTT=="control",])
modsel(list(fit01, fit02), 1000)
fit01


#### POISSON ####
hist((ddd$value))
new.factor <- as.factor(rnorm(nrow(ddd)))
fit01 <- glmer(value ~ newTT + (1|destSiteID) + (1|variable) , pheno.long, family=poisson)
fit02 <- glmer(value ~ 1 + (1|destSiteID) + (1|variable) , pheno.long, family=poisson)
phi.mod <- disp(fit01, ddd)
modsel(list(fit01, fit02), 1000, phi = 1)
fit01


fit01 <- glmer(value ~ newTT + flowering.time + (1|destSiteID) + (1|variable) + (1|new.factor), ddd, family=poisson)
fit02 <- glmer(value ~ newTT + (1|destSiteID) + (1|variable) + (1|new.factor), ddd, family=poisson)
modsel(list(fit01, fit02), 1000, phi = phi.mod)
fit01




fit01 <- glmer(value ~ newTT + (1|destSiteID) + (1|variable), pheno.long, family=poisson)
fit02 <- glmer(value ~ 1 + (1|destSiteID) + (1|variable), pheno.long, family=poisson)
disp(fit01, pheno.long)

fit01 <- lmer(value ~ newTT + (1|destSiteID) + (1|variable), pheno.long)
fit02 <- lmer(value ~ 1 + (1|destSiteID) + (1|variable), pheno.long)
modsel(list(fit01, fit02), 100)
fix.check(fit01)
fit01

fixef(fit01)
summary(fit01)

which(resid(fit01) >7)
pheno.long[592,]

turfs.15 %>%
  filter(destSiteID %in% c("Lavisdalen", "Gudmedalen")) %>%
  group_by(destSiteID) %>%
  ggplot() +
  geom_boxplot(aes(y = d.dosm, x = destSiteID))
  



#### PERFORM MODEL SELECTION WITH MODSEL ####
ModelSelection <- function(dd, dist){
  # for poisson distribution
  if(dist == "pois"){
    fit01 <- glmer(value ~ newTT + (1|destSiteID) + (1|variable), pheno.long, family=poisson)
    fit02 <- glmer(value ~ 1 + (1|destSiteID) + (1|variable), pheno.long, family=poisson)
  }
  
  # for normal distribution (duration)
  else if(dist == "norm"){
    fit01 <- lmer(value ~ newTT + (1|destSiteID) + (1|variable), pheno.long)
    fit02 <- lmer(value ~ 1 + (1|destSiteID) + (1|variable), pheno.long)
  }
  # model selection
  modsel(list(fit01, fit02), 1000)
}


fit02 <- glmer(value ~ destTemp_value+destPrec_value + (1|destSiteID) + (1|variable), dd, family=poisson)
tab




### need to add this to modsel!
tab$wH <- tab$wi
if(tab$Model==2){
  tab$wH <- tab$wi[tab$Model==2] + tab$wi[tab$Model==1]
}
tab$ER <- tab$wH/(1-tab$wH)






### TEST FOR THE EFFECT OF SITE
# poisson
fit01 <- glmer(value ~ destTemp_value*destPrec_value + (1|destSiteID) + (1|variable), pheno.long, family=poisson)
fit02 <- glmer(value ~ destTemp_value+destPrec_value + (1|destSiteID) + (1|variable), pheno.long, family=poisson)
fit03 <- glmer(value ~ destTemp_value + (1|destSiteID) + (1|variable), pheno.long, family=poisson)
fit04 <- glmer(value ~ destPrec_value + (1|destSiteID) + (1|variable), pheno.long, family=poisson)
fit05 <- glmer(value ~ 1 + (1|destSiteID) + (1|variable), pheno.long, family=poisson)
modsel(list(fit01, fit02, fit03, fit04, fit05), 1000)

# normal for duration
fit01 <- lmer(value ~ destTemp_value*destPrec_value + (1|destSiteID) + (1|variable), pheno.long)
fit02 <- lmer(value ~ destTemp_value+destPrec_value + (1|destSiteID) + (1|variable), pheno.long)
fit03 <- lmer(value ~ destTemp_value + (1|destSiteID) + (1|variable), pheno.long)
fit04 <- lmer(value ~ destPrec_value + (1|destSiteID) + (1|variable), pheno.long)
fit05 <- lmer(value ~ 1 + (1|destSiteID) + (1|variable), pheno.long)
modsel(list(fit01, fit02, fit03, fit04, fit05), 1000)

# test models
disp(fit01, pheno.long)
fit01
fix.check(fit01)


# Test for difference in functional Groups
fit1 <- glmer(value ~ destTemp_value*destPrec_value + functionalGroup + (1|destSiteID) + (1|variable), pheno.long, family=poisson)
fit2 <- glmer(value ~ destTemp_value*destPrec_value + (1|destSiteID) + (1|variable), pheno.long, family=poisson)
modsel(list(fit1, fit2), 1000)
fit1

# Flowering time
fit1 <- glmer(value ~ destTemp_value*destPrec_value + flowering.time + (1|destSiteID) + (1|variable), pheno.long, family=poisson)
fit2 <- glmer(value ~ destTemp_value*destPrec_value + (1|destSiteID) + (1|variable), pheno.long, family=poisson)
modsel(list(fit1, fit2), 1000)
fit1






### ORIGIN ####
pheno.long <- pheno.long[pheno.long[,"value"] >= 0,] # remove all values < 0 (does not work for poisson model)
fit1 <- glmer(value ~ Temp_value*Prec_value + (1|siteID) + (1|variable), pheno.long, family=poisson)
fit2 <- glmer(value ~ Temp_value+Prec_value + (1|siteID) + (1|variable), pheno.long, family=poisson)
fit3 <- glmer(value ~ Temp_value + (1|siteID) + (1|variable), pheno.long, family=poisson)
fit4 <- glmer(value ~ Prec_value + (1|siteID) + (1|variable), pheno.long, family=poisson)
fit5 <- glmer(value ~ 1 + (1|siteID) + (1|variable), pheno.long, family=poisson)

modsel(list(fit1, fit2, fit3, fit4, fit5), 1000)
fix.check(fit1)
fit1

# Test for difference in functional Groups
fit1 <- glmer(value ~ Temp_value*Prec_value + functionalGroup + (1|siteID) + (1|variable), pheno.long, family=poisson)
fit1 <- glmer(value ~ Temp_value*Prec_value + (1|siteID) + (1|variable), pheno.long, family=poisson)
modsel(list(fit1, fit2), 1000)
fit1

# Flowering time
fit1 <- glmer(value ~ Temp_value*Prec_value + flowering.time + (1|siteID) + (1|variable), pheno.long, family=poisson)
fit2 <- glmer(value ~ Temp_value*Prec_value + (1|siteID) + (1|variable), pheno.long, family=poisson)
modsel(list(fit1, fit2), 1000)
fit1
