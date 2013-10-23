# This code is to run exploratory analyses on observational data 
# from the Summer 2013 field study in Patagonia, AZ
# This code deals witht the point count and focal observation data. Intern to contact: Brittany
# Code developed by: Anusha Shankar, Sarah Supp, and Catherine Graham

## Load packages
library(chron)
library(ggplot2)
library(reshape)


##### CHOOSE WORKING DIRECTORY (uncomment the one you like)
#wd = "C://Users//Anusha//Documents//Dropbox//Hummingbirds//Pasantias_Patagonia_2013//Final_Databases_2013//Excel_CSV_versions"
wd = "/Users/sarah/Desktop/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Excel_CSV_versions"
setwd(wd)

## Reading in point count and focal observation data
pointdata <- read.csv("updated_FocalObservationPointCountData.csv", na.strings="NA", sep=",", header=T)


#----------CLEANING AND AGGREGATING THE DATA

## Add a column for julian day, to help plot time series data
for (row in 1:nrow(pointdata)){
  line = pointdata[row,]
  pointdata$julian[row] = julian(line$Month, line$Day, line$Year, origin. = c(month=1, day=1, year=line$Year))
}

## subset only data from the two main landscapes
pointdata = subset(pointdata, Site == "HC" | Site == "PL/SC")

## subset data that does not include surveys where no birds were seen
obs = subset(pointdata, Species.Code!="None")
  obs$Site <- factor(obs$Site, levels = c('HC', 'PL/SC'))
  obs$Species.Code <- factor(obs$Species.Code, levels = c("ANHU", "BBLH", "BCHU", "COHU", "RUHU", "VCHU", "UNHU"))

#subset data that doesn't include species == NA or species == UNHU (for richness, don't care if we don't know identity)
sppdata = subset(obs, Species.Code %in% c('ANHU', 'BBLH', 'BCHU', 'COHU', 'RUHU', 'VCHU'))
  sppdata$Species.Code <- factor(sppdata$Species.Code, levels = c("ANHU", "BBLH", "BCHU", "COHU", "RUHU", "VCHU"))

#subset data for each landscape
hc = subset(obs, Site == "HC")
pl = subset(obs, Site == "PL/SC")

## Aggregate data; count the number of species seen at each session on each day in each site, not counting UNHU and NA
richnesstime <- aggregate(sppdata$Species.Code, by=list(sppdata$julian, sppdata$Session, sppdata$Site),
                      FUN=function(u) length(unique(u)))
names(richnesstime) <- c("julian", "session", "site", "S")
richnesstime$session <- factor(richnesstime$session, levels = c('Morning', 'Midday','Afternoon'),ordered = TRUE)

## Aggregate data; count the number of species seen on each day in each site
richnessday <- aggregate(sppdata$Species.Code, by=list(sppdata$julian, sppdata$Site),
                          FUN=function(u) length(unique(u)))
names(richnessday) <- c("julian", "site", "S")

## Species richness at each site     
richnesssite <- aggregate(sppdata$Species.Code, by=list(sppdata$Site), FUN=function(u) length(unique(u)))
names(richnesssite) <- c("site", "S")

## Total number of individuals of species at each site
spcount = t(table(obs$Species.Code, obs$Site))
spcount = melt(spcount)
  names(spcount) = c("site", "spcode", "N")

## Count the number of individuals seen at each site on each day
n = table(obs$julian, obs$Site)
n = melt(n)
names(n) <- c("julian", "site", "N")
n = subset(n, N!=0) #because this fxn assumed we sampled on the same dates (zeroes not included here)

## Count the number of individuals seen at each site, during each session on each day
ntime = table(obs$julian, obs$Session, obs$Site)
ntime = melt(ntime)
names(ntime) <- c("julian", "session", "site", "N")
ntime$session <- factor(ntime$session,levels = c('Morning', 'Midday','Afternoon'),ordered = TRUE)
ntime = subset(ntime, N!=0) #because we didn't sample on the same days

#---------- PLOTTING THE DATA

## Plot species vs. site
sitespecies <- ggplot(sppdata, aes(x=Site, y=Species.Code)) + ylab("Species") + 
  geom_point(col="indianred") + theme_bw()

## Compare the total number of species seen in each landscape
landscape_richness <- ggplot(data = richnesssite, aes(x = site, y = S, fill = site)) + 
  geom_bar(position = "dodge") + labs(x="",y="Species Richness",fill="Landscape") +
  scale_y_continuous(breaks = round(seq(0, 10, by = 1),1)) + theme_bw()

## Plot the number of individuals of each species seen in the two landscapes
species_counts <- ggplot(data = spcount, aes(x=spcode, y = N, fill = site)) + geom_bar(position = "dodge") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x="Species",y="Count",fill="Site") +
  scale_y_continuous(breaks = round(seq(0, 250, by =25),1)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

## Plot the number of species at each site over time
S_timeseries <- ggplot(richnessday, aes(x=julian, y=S, col=site)) + geom_point() + geom_line() +
  theme_bw() + labs(x="Julian Day", y="Species Richness", col = "Landscape") +
  scale_x_continuous(breaks = round(seq(130, 200, by = 5),1), limits=c(130, 200)) +
  scale_y_continuous(breaks = round(seq(0, 5, by = 1),1), limits=c(1,4))

## Plot the number of species at each site over time, and separately for each time of day
S_timeday <- ggplot(richnesstime, aes(x=julian, y=S, col=site)) + geom_point() + geom_line() +
  theme_bw() + labs(x="Julian Day", y="Species Richness", col = "Landscape") +
  scale_x_continuous(breaks = round(seq(130, 200, by = 5),1), limits=c(130, 200)) +
  scale_y_continuous(breaks = round(seq(1, 8, by = 1),1)) +
  facet_wrap(~session,nrow=3)

## Plot number of species seen at different times of day
session_richness <- ggplot(richnesstime, aes(x=session, y=S, fill=session)) + 
  geom_boxplot() + theme_bw() + facet_wrap(~site) + 
  labs(x="Time of Day", y="Species Richness", col = "Time of Day") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

## Plot number of individualss seen at different times of day
session_abundance <- ggplot(ntime, aes(x=session, y=N, fill=session)) + 
  geom_boxplot() + theme_bw() + facet_wrap(~site) + 
  labs(x="Time of Day", y="Abundance", col = "Time of Day") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
          
## Plot the number of individuals counted at each site over time
N_timeseries <- ggplot(n, aes(x=julian, y=N, col=site)) + geom_point() + geom_line() +
  theme_bw() + labs(x="Julian Day", y="Number of Hummingbirds", col = "Landscape") +
  scale_x_continuous(breaks = round(seq(130, 200, by = 5),1), limits=c(130,200)) +
  scale_y_continuous(breaks = round(seq(0, 80, by = 5),1), limits = c(0,80))

## Plot the number of individuals counted at each site over time and at each times of day
N_timeday <- ggplot(ntime, aes(x=julian, y=N, col=site)) + geom_point() + geom_line() +
  theme_bw() + labs(x="Julian Day", y="Number of Hummingbirds", col = "Landscape") +
  scale_x_continuous(breaks = round(seq(130, 200, by = 10),1)) +
  scale_y_continuous(breaks = round(seq(0, 80, by = 10),1)) +
  facet_wrap(~session,nrow=3)

#----- TODO:
#compare abundances across sites and habitat types
#should we include data from both point counts and focal obs together or separately?
#for loop to chunk data into 2-week periods (label week 1, week 2)
#for loop to indicate days/sessions when 0 species were seen. Not currently included - probably need to write a for loop to do this.
#plot daily abundance scaled by number of transects sampled?
#plot relative abundance of each species for the two sites?

