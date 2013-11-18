# This code is to run exploratory analyses on observational data 
# from the Summer 2013 field study in Patagonia, AZ
# This code deals with the hummingbird nest data. Contact for data questions: Monica
# Code developed by: Anusha Shankar, Sarah Supp, and Catherine Graham

## Load packages
library(ggplot2)
library(reshape)
library(RColorBrewer)

##### CHOOSE WORKING DIRECTORY (uncomment the one you like)
## There are multiple .csv files in this folder
#wd = "C://Users/Anusha/Documents/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Excel_CSV_versions/"
wd = "/Users/sarah/Desktop/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Excel_CSV_versions/"
setwd(wd)

## Read in csv files
nest_uncleaned <- read.csv("NestData.csv")
## Removed entry where Species is blank (Id 74) ASK SUSAN? ###################
nest_na.rm <- nest_uncleaned[-74,]
## Subset data from HC and PL/SC, leaving out TNC and EC points. 
### ASK SUSAN if she wants to see without this ###############
s <- c("HC", "PL/SC")
nest <- subset(nest_na.rm, Site == s)
# nest <- nest_na.rm # Uncomment beginning to keep restoration sites in the dataset

##-------- Cleaning and aggregating data
species_site <- aggregate(nest$Species, by=list(nest$Site, nest$Species), 
                          FUN=function(x) x=length(x))
names(species_site) <- c("Site", "Species", "Richness")

m.nestht <- melt(nest, id.vars=c("Site", "Species"), 
                 measure.vars=c("Nest_Height"), na.rm=T)

m.nestgenus <- melt(nest, id.vars=c("Site", "Species"), 
                    measure.vars=c("Substrate_Genus"), na.rm=T)

##--------- Plots

# TODO:
# plot number of individuals of each species found nesting at each site
nests_site <- ggplot(species_site, aes(x=Site, y=Richness, fill=factor(Site))) + geom_bar() + ylab("Nests") +
  facet_grid(~Species) + theme_bw()
nests_site

# plot number of nests found at each site - in first graph
# plot number of individuals, for each species that was observed nesting at each site - see first graph

# boxplots for nest height at the two sites
nest_ht <- ggplot(subset(m.nestht, variable=="Nest_Height"), aes(x=Site, y=value)) + 
  xlab("Site") + ylab("Nest Height") + geom_point() + theme_bw() + facet_grid(~Species)
nest_ht

# plot tree genera that had nests in them at the two sites
nest_trees <- ggplot(m.nestgenus, aes(x=Species, y=value)) + ylab("Tree Genus") + 
  geom_point(pch=18, size=4) + theme_bw() + facet_grid(~Site)
nest_trees

# barplots for nest result at the two sites (num successful vs. depredated, etc.)
nest_result <- ggplot(nest, aes(Site, Final_Result)) + geom_point() + theme_bw()
nest_result

# map locations for nest sites (NOTE need to dbl check UTM zone first)
# boxplots for supporting branch diameter (could be interesting to know range)


##--------- Analyses
# t test to look at differences in nest height between species- Welch Two Sample t-test
t.nestht <- t.test(m.nest$value[m.nest$Species=="BBLH"], m.nest$value[m.nest$Species=="BCHU"])

# Function to report values from t tests
report <- function (nest_ttest) {
t <- nest_ttest$statistic
df <- nest_ttest$parameter
p <- nest_ttest$p.value
print(c(t, df, p))
}
report(t.nestht)