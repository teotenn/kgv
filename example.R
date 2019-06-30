library(kgv)
data(compost)

## Here tukey differences are set as T
aov.K <- with(compost, Tt.aov(K, Treatment))
aov2.K <- with(compost, Tt.aov(K, Treatment, Plant))

## Plots with defaults
K.plot <- Tt.barplot(aov.K$stats, "Treatment") # No dif specified
K2.plot <- Tt.barplot(aov2.K$stats, "Treatment", T2="Plant", dif="Dif")
