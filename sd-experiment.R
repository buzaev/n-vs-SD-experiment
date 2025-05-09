setwd ("/Users/garrybear/Documents/t3/n-vs-SD-experiment")
set.seed(123) # reproducibility

# Calculate sample size for experiment
alpha=0.05
power=0.95
effect=0.05
zAlpha = qnorm(1 - alpha / 2)
zBeta = qnorm(power)

# Set the number of iterations
iterations = round(((zAlpha + zBeta)^2 / effect^2 + 2),0)

# Experiment A: variants of population with the same mean and SD (constant)
# Initialize the results dataframe
resultsSD = data.frame(
  n = integer(), 
  populationMean = numeric(), 
  populationSD = numeric(), 
  sampleN = integer(), 
  sampleMean = numeric(), 
  sampleSD = numeric(), 
  sampleSE = numeric()
)
populationMean= runif(1, 0, 10000) # Random mean between 0 and 10000
populationSD= runif(1, 0, 10000)   # Random SD between 0 and 10000

#populationMean =2876 # reproducibility
#populationSD = 7883 # reproducibility

for (i in 1:iterations) {
  # 2.1 Generate population with random mean and SD
  populationB = rnorm(1e6, mean = populationMean, sd = populationSD) # 1 million cases in population
  
  # 2.2 Create a random subset
  sampleN = sample(10:1000, 1) # Random number of cases N (between 10 and 1000)
  subset = sample(populationB, sampleN)#, replace = FALSE)
  
  # 2.3 Calculate sample statistics
  sampleMean = mean(subset)
  sampleSD = sd(subset)
  sampleSE = sampleSD / sqrt(sampleN)
  
  # 2.4 Fill the row in resultsSD
  resultsSD = rbind(
    resultsSD, 
    data.frame(
      n = i,
      populationMean = populationMean,
      populationSD = populationSD,
      sampleN = sampleN,
      sampleMean = sampleMean,
      sampleSD = sampleSD,
      sampleSE = sampleSE
    )
  )
}
resultsA=resultsSD


# Experiment B mean and SD is constant
# Initialize the results dataframe
resultsSD = data.frame( n = integer(), 
                        populationMean = numeric(), 
                        populationSD = numeric(), 
                        sampleN = integer(), 
                        sampleMean = numeric(), 
                        sampleSD = numeric(), 
                        sampleSE = numeric()
)
for (i in 1:iterations) {
  # 2.1 Generate population with random mean and SD
  populationMean = runif(1, 0, 10000) # Random mean 
  populationSD = runif(1, 0, 10000)   # Random SD between 0 and 10000
  population = rnorm(1e6, mean = populationMean, sd = populationSD) # 1 million cases in population
  
  # 2.2 Create a random subset
  sampleN = sample(10:1000, 1) # Random number of cases N (between 10 and 1000)
  subset = sample(population, sampleN, replace = FALSE)
  
  # 2.3 Calculate sample statistics
  sampleMean = mean(subset)
  sampleSD = sd(subset)
  sampleSE = sampleSD / sqrt(sampleN)
  
  # 2.4 Fill the row in resultsSD
  resultsSD = rbind(
    resultsSD, 
    data.frame(
      n = i,
      populationMean = populationMean,
      populationSD = populationSD,
      sampleN = sampleN,
      sampleMean = sampleMean,
      sampleSD = sampleSD,
      sampleSE = sampleSE
    )
  )
}
resultsB=resultsSD
rm(resultsSD)




library(ggplot2) 
library(ggthemes)
library(ggpubr)



### Analisys


ds=resultsB # Choose experiment A or B

summary(ds)


gg=ggplot(ds)+ 
  aes(x=sampleN,y=sampleSD)+
  geom_point(size=1, show.legend = TRUE, alpha = .6) +
  geom_smooth(method = "lm", se = FALSE,  size = 1, col="green") +  
  geom_smooth(method="loess", se=TRUE,col="black", size=0.3, level=0.95) +
  theme_minimal()+
  stat_cor(method = "spearman", label.x.npc =0.3, label.y.nrc = 0.30, size=4)+
  stat_ellipse(color="lightgrey")+
  labs(subtitle="Is the SD of a sample is inversely related to the square root of the sample size?", 
       x="number of cases in sample", 
       y="SD of sample", 
       title="Scatterplot")
plot(gg)

plotfilename=paste("/Users/garrybear/Documents/t3/n-vs-SD-experiment/corrN-SD.pdf", sep="")
pdf(plotfilename,6,4)
print(gg, newpage = FALSE)
dev.off()


gg=ggplot(ds)+ 
  aes(x=sampleN,y=sampleSE)+
  geom_point(size=1, show.legend = TRUE, alpha = .6) +
  geom_smooth(method="loess", se=TRUE,col="green", size=0.3, level=0.95) +
  theme_minimal()+
  labs(subtitle="Is SE of a sample is inversely related to the square root of the sample size?", 
       x="number of cases in sample", 
       y="SE of sample", 
       title="Scatterplot"  )
plot(gg)

plotfilename=paste("/Users/garrybear/Documents/t3/n-vs-SD-experiment/corN-SE.pdf", sep="")
pdf(plotfilename,6,4)
print(gg, newpage = FALSE)
dev.off()

cor.test(ds$sampleN, ds$sampleSD, method = "pearson")
cor.test(ds$sampleN, ds$sampleSD, method = "spearman")

