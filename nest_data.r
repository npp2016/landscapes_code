# This code is to run exploratory analyses on observational nest data 
# from the Summer 2013 field study in Patagonia, AZ
# This code deals with the hummingbird nest data. Contact for data questions: Monica
# Code developed by: Anusha Shankar, Sarah Supp, and Catherine Graham

## Load packages
library(ggplot2)
library(reshape)
library(RColorBrewer)

##### CHOOSE WORKING DIRECTORY (uncomment the one you like)
## There are multiple .csv files in this folder
wd = "C://Users/Anusha/Documents/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Excel_CSV_versions/"
#wd = "/Users/sarah/Desktop/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Excel_CSV_versions/"
setwd(wd)

## Read in csv files
nest_uncleaned <- read.csv("NestData.csv")
  nest_na.rm <- nest_uncleaned[-74,]  ## Removed entry where Species is blank (Id 74) ASK SUSAN? ###################
nestreview <- read.csv("ReviewNestData.csv", header = T)
## Subset data from HC and PL/SC, leaving out TNC and EC points. 
### ASK SUSAN if she wants to see without this ###############
s <- c("HC", "PL/SC")
nest <- subset(nest_na.rm, Site == s)
# nest <- nest_na.rm # Uncomment beginning to keep restoration sites in the dataset

## Total number of nests surveyed which we are including in the analysis.
N <- length(nest$Id)

##-------- Cleaning and aggregating data
## Melting everything together wasn't giving proper graphs, need to figure out what went wrong.
species_site <- aggregate(nest$Species, by=list(nest$Site, nest$Species), 
                          FUN=function(x) x=length(x))
names(species_site) <- c("Site", "Species", "Richness")

m.nestht <- melt(nest, id.vars=c("Site", "Species"), 
                 measure.vars="Nest_Height", na.rm=T)

m.nestgenus <- melt(nest, id.vars=c("Site", "Species"), 
                    measure.vars="Substrate_Genus", na.rm=T)

m.treeht <- melt(nest, id.vars=c("Site", "Species"), measure.vars="Substrate_Height", na.rm=T)

agg.stage <- aggregate(nest$Site, by=list(nest$Site, nest$Stage_Found), 
                           FUN=function(x) x=length(x))
names(agg.stage) <- c("Site","Stage_Found","Nests")

agg.result <- aggregate(nest$Site, by=list(nest$Site, nest$Final_Result), 
                          FUN=function(x) x=length(x))
names(agg.result) <- c("Site", "Final_Status", "Nests")

##--------- Plots

# plot number of individuals of each species found nesting at each site
nests_site <- ggplot(species_site, aes(x=Site, y=Richness, fill=factor(Site))) + 
  geom_bar() + ylab("Nests") + facet_grid(~Species) + theme_bw()
nests_site

# plot number of nests found at each site - in first graph
# plot number of individuals per species that was observed nesting at each site - see first graph

# boxplots for nest height at the two sites
nest_ht <- ggplot(m.nestht, aes(x=Site, y=value)) + 
  xlab("Site") + ylab("Nest Height") + geom_boxplot() + theme_bw() + facet_grid(~Species)
nest_ht

# plot tree genera that had nests in them at the two sites
nest_trees <- ggplot(m.nestgenus, aes(x=Species, y=value)) + ylab("Tree Genus") + 
  geom_point(pch=18, size=4) + theme_bw() + facet_grid(~Site)
nest_trees

# Plot nest tree heights
tree_ht <- ggplot(m.treeht, aes(x=Site, y=value)) + 
  xlab("Site") + ylab("Tree Height") + geom_boxplot() + theme_bw()
tree_ht

# Stage the nests were found in
stage_found <- ggplot(agg.stage, aes(Stage_Found, Nests, fill=factor(Stage_Found))) + 
  geom_bar() + theme_bw() + facet_grid(~Site) + 
  theme(axis.text.x=element_text(angle=60, vjust=1, hjust=1))
stage_found

# Barplots for nest result at the two sites (num successful vs. depredated, etc.)
nest_results <- ggplot(agg.result, aes(Final_Status, Nests, fill=factor(Final_Status))) + 
  geom_bar() + theme_bw() + facet_grid(~Site) + 
  theme(axis.text.x=element_text(angle=60, vjust=1, hjust=1))
nest_results

# map locations for nest sites (NOTE need to dbl check UTM zone first)
# boxplots for supporting branch diameter (could be interesting to know range)


##--------- Analyses
# t test to look at differences in nest height between species- Welch Two Sample t-test
t.nestht <- t.test(m.nestht$value[m.nestht$Species=="BBLH"], m.nestht$value[m.nestht$Species=="BCHU"])

# Function to report values from t tests
report <- function (nest_ttest) {
t <- nest_ttest$statistic
df <- nest_ttest$parameter
p <- nest_ttest$p.value
print(c(t, df, p))
}
report(t.nestht)