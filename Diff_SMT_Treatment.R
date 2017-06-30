#### TEST DIFFERENCE IN SMT IN TREATMENTS ####

# Create MetaData
SMData <- MetaData %>%
  filter(!is.na(SMDiff), Treatment != "Control")


fit <- lmer(SMDiff ~ Treatment + (1|siteID/blockID), SMData)
summary(fit)
plot(fit)
hist(SMData$SMDiff)

ggplot(SMData, aes(y = SMDiff, x = siteID, color = Treatment, shape = Shape)) + 
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_jitter() + 
  facet_wrap(~ Treatment)
