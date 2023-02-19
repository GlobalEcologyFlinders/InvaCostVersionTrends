################################################################################
## temporal trends in number of database entries, number of species reported, ##
## and annual costs in different versions of the InvaCost database            ##
## Corey Bradshaw, Ismael Soto, Boris Leroy                                   ##
## February 2023                                                              ##
################################################################################

# libraries
library(invacost)
library(dplyr)
library(stringr)
library(tidyr)  
library(rms)

# get versions (requires internet connection)
icDatv1.0 <- getInvaCostVersion("1.0")
icDatv2.1 <- getInvaCostVersion("2.1")
icDatv3.0 <- getInvaCostVersion("3.0")
icDatv4.1 <- getInvaCostVersion("4.1")

#####################################
# number of entries and costs only ##
#####################################
# remove no data
icDatv3.0a <- icDatv3.0[-which(is.na(icDatv3.0$Probable_starting_year_adjusted)), ]
icDatv4.1a <- icDatv4.1[-which(is.na(icDatv4.1$Probable_starting_year_adjusted)), ]
icDatv3.0b <- icDatv3.0a[-which(is.na(icDatv3.0a$Probable_ending_year_adjusted)), ]
icDatv4.1b <- icDatv4.1a[-which(is.na(icDatv4.1a$Probable_ending_year_adjusted)), ]

# expand
icDatv1.0.exp <- expandYearlyCosts(icDatv1.0,startcolumn = "Probable_starting_year_adjusted",endcolumn = "Probable_ending_year_adjusted")
icDatv2.1.exp <- expandYearlyCosts(icDatv2.1,startcolumn = "Probable_starting_year_adjusted",endcolumn = "Probable_ending_year_adjusted")
icDatv3.0.exp <- expandYearlyCosts(icDatv3.0b,startcolumn = "Probable_starting_year_adjusted",endcolumn = "Probable_ending_year_adjusted")
icDatv4.1.exp <- expandYearlyCosts(icDatv4.1b,startcolumn = "Probable_starting_year_adjusted",endcolumn = "Probable_ending_year_adjusted")

# summarise by year
icDatv1.0.summ <- summarizeCosts(icDatv1.0.exp, year.column= "Impact_year", cost.column = "Cost_estimate_per_year_2017_USD_exchange_rate",
                                 in.millions = FALSE,  minimum.year = 1960, maximum.year = 2021, year.breaks=seq(1960,2021,1))

head(icDatv1.0.summ$cost.per.year)
plot(icDatv1.0.summ$cost.per.year$year, icDatv1.0.summ$cost.per.year$cost, type="l", xlab="",ylab="cost (US$)")
plot(icDatv1.0.summ$cost.per.year$year, icDatv1.0.summ$cost.per.year$number_estimates, type="l", xlab="",ylab="number estimates")
#table(icDatv1.0.exp$Impact_year, icDatv1.0.exp$Species)

icDatv2.1.summ <- summarizeCosts(icDatv2.1.exp, year.column= "Impact_year", cost.column = "Cost_estimate_per_year_2017_USD_exchange_rate",
                                 in.millions = FALSE,  minimum.year = 1960, maximum.year = 2021, year.breaks=seq(1960,2021,1))

head(icDatv2.1.summ$cost.per.year)
plot(icDatv2.1.summ$cost.per.year$year, icDatv2.1.summ$cost.per.year$cost, type="l", xlab="",ylab="cost (US$)")
plot(icDatv2.1.summ$cost.per.year$year, icDatv2.1.summ$cost.per.year$number_estimates, type="l", xlab="",ylab="number estimates")

icDatv3.0.summ <- summarizeCosts(icDatv3.0.exp, year.column= "Impact_year", cost.column = "Cost_estimate_per_year_2017_USD_exchange_rate",
                                 in.millions = FALSE,  minimum.year = 1960, maximum.year = 2021, year.breaks=seq(1960,2021,1))

head(icDatv3.0.summ$cost.per.year)
plot(icDatv3.0.summ$cost.per.year$year, icDatv3.0.summ$cost.per.year$cost, type="l", xlab="",ylab="cost (US$)")
plot(icDatv3.0.summ$cost.per.year$year, icDatv3.0.summ$cost.per.year$number_estimates, type="l", xlab="",ylab="number estimates")

icDatv4.1.summ <- summarizeCosts(icDatv4.1.exp, year.column= "Impact_year", cost.column = "Cost_estimate_per_year_2017_USD_exchange_rate",
                                 in.millions = FALSE,  minimum.year = 1960, maximum.year = 2021, year.breaks=seq(1960,2021,1))

head(icDatv4.1.summ$cost.per.year)
plot(icDatv4.1.summ$cost.per.year$year, icDatv4.1.summ$cost.per.year$cost, type="l", xlab="",ylab="cost (US$)")
plot(icDatv4.1.summ$cost.per.year$year, icDatv4.1.summ$cost.per.year$number_estimates, type="l", xlab="",ylab="number estimates")

# create dataframes for entries and costs separately
# entries
entries.out <- data.frame(icDatv4.1.summ$cost.per.year$year, icDatv1.0.summ$cost.per.year$number_estimates, icDatv2.1.summ$cost.per.year$number_estimates,
                          icDatv3.0.summ$cost.per.year$number_estimates, icDatv4.1.summ$cost.per.year$number_estimates)
colnames(entries.out) <- c("year", "v1.0", "v2.1", "v3.0", "v4.1")
write.csv(entries.out, file="versEntries.csv", row.names = F)

# costs
cost.out <- data.frame(icDatv4.1.summ$cost.per.year$year, icDatv1.0.summ$cost.per.year$cost/10^9, icDatv2.1.summ$cost.per.year$cost/10^9,
                       icDatv3.0.summ$cost.per.year$cost/10^9, icDatv4.1.summ$cost.per.year$cost/10^9) # note: cost in billions (hence dividing by 10^9)
colnames(cost.out) <- c("year", "v1.0", "v2.1", "v3.0", "v4.1")
write.csv(cost.out, file="versCost.csv", row.names = F)


#####################################################
# temporal trend in # species reported in InvaCost ##
#####################################################

## v1.0
# clean
icDatv1.0a <- icDatv1.0[-which(is.na(icDatv1.0$Cost_estimate_per_year_2017_USD_exchange_rate)), ]
icDatv1.0b <- icDatv1.0a[!grepl("/", icDatv1.0a$Species),]
icDatv1.0c <- icDatv1.0b[!grepl("sp.", icDatv1.0b$Species),]
icDatv1.0d <- icDatv1.0c[!grepl("spp.", icDatv1.0c$Species),] 

# expand
icDatv1.0.exp2 <- expandYearlyCosts(icDatv1.0d,startcolumn = "Probable_starting_year_adjusted",endcolumn = "Probable_ending_year_adjusted")
min(icDatv1.0.exp2$Impact_year, na.rm=T)
max(icDatv1.0.exp2$Impact_year, na.rm=T)

# filter
icDatv1.0.exp2 <- icDatv1.0.exp2 %>% filter(icDatv1.0.exp2$Impact_year <= 2021)
icDatv1.0.exp2 <- icDatv1.0.exp2 %>% filter(icDatv1.0.exp2$Impact_year >= 1960)
icDatv1.0.exp2$cost <- as.numeric(gsub(",", "", icDatv1.0.exp2$Cost_estimate_per_year_2017_USD_exchange_rate))
icDatv1.0.exp2 <- icDatv1.0.exp2[!is.na(icDatv1.0.exp2$cost),]
icDatv1.0.exp2$cost_bil <- (icDatv1.0.exp2$cost/1000000000)
sum(icDatv1.0.exp2$cost_bil)
nrow(icDatv1.0.exp2)
icDatv1.0.sppXyr <- icDatv1.0.exp2 %>% group_by(Impact_year, Species) %>% summarise(Cost=sum(cost_bil))%>% 
  group_by(Impact_year) %>% summarise(Species=n())
plot(icDatv1.0.sppXyr$Impact_year, icDatv1.0.sppXyr$Species, type="l", xlab="", ylab="spp")

## v2.1
# clean
icDatv2.1a <- icDatv2.1[-which(is.na(icDatv2.1$Cost_estimate_per_year_2017_USD_exchange_rate)), ]
icDatv2.1b <- icDatv2.1a[!grepl("/", icDatv2.1a$Species),]
icDatv2.1c <- icDatv2.1b[!grepl("sp.", icDatv2.1b$Species),]
icDatv2.1d <- icDatv2.1c[!grepl("spp.", icDatv2.1c$Species),] 

# expand
icDatv2.1.exp2 <- expandYearlyCosts(icDatv2.1d,startcolumn = "Probable_starting_year_adjusted",endcolumn = "Probable_ending_year_adjusted")
min(icDatv2.1.exp2$Impact_year, na.rm=T)
max(icDatv2.1.exp2$Impact_year, na.rm=T)

# filter
icDatv2.1.exp2 <- icDatv2.1.exp2 %>% filter(icDatv2.1.exp2$Impact_year <= 2021)
icDatv2.1.exp2 <- icDatv2.1.exp2 %>% filter(icDatv2.1.exp2$Impact_year >= 1960)
icDatv2.1.exp2$cost <- as.numeric(gsub(",", "", icDatv2.1.exp2$Cost_estimate_per_year_2017_USD_exchange_rate))
icDatv2.1.exp2 <- icDatv2.1.exp2[!is.na(icDatv2.1.exp2$cost),]
icDatv2.1.exp2$cost_bil <- (icDatv2.1.exp2$cost/1000000000)
sum(icDatv2.1.exp2$cost_bil)
nrow(icDatv2.1.exp2)
icDatv2.1.sppXyr <- icDatv2.1.exp2 %>% group_by(Impact_year, Species) %>% summarise(Cost=sum(cost_bil))%>% 
  group_by(Impact_year) %>% summarise(Species=n())
plot(icDatv2.1.sppXyr$Impact_year, icDatv2.1.sppXyr$Species, type="l", xlab="", ylab="spp")

## v3.0
# clean
icDatv3.0a <- icDatv3.0[-which(is.na(icDatv3.0$Cost_estimate_per_year_2017_USD_exchange_rate)), ]
icDatv3.0b <- icDatv3.0a[!grepl("/", icDatv3.0a$Species),]
icDatv3.0c <- icDatv3.0b[!grepl("sp.", icDatv3.0b$Species),]
icDatv3.0d <- icDatv3.0c[!grepl("spp.", icDatv3.0c$Species),] 

# expand
icDatv3.0.exp2 <- expandYearlyCosts(icDatv3.0d,startcolumn = "Probable_starting_year_adjusted",endcolumn = "Probable_ending_year_adjusted")
min(icDatv3.0.exp2$Impact_year, na.rm=T)
max(icDatv3.0.exp2$Impact_year, na.rm=T)

# filter
icDatv3.0.exp2 <- icDatv3.0.exp2 %>% filter(icDatv3.0.exp2$Impact_year <= 2021)
icDatv3.0.exp2 <- icDatv3.0.exp2 %>% filter(icDatv3.0.exp2$Impact_year >= 1960)
icDatv3.0.exp2$cost <- as.numeric(gsub(",", "", icDatv3.0.exp2$Cost_estimate_per_year_2017_USD_exchange_rate))
icDatv3.0.exp2 <- icDatv3.0.exp2[!is.na(icDatv3.0.exp2$cost),]
icDatv3.0.exp2$cost_bil <- (icDatv3.0.exp2$cost/1000000000)
sum(icDatv3.0.exp2$cost_bil)
nrow(icDatv3.0.exp2)
icDatv3.0.sppXyr <- icDatv3.0.exp2 %>% group_by(Impact_year, Species) %>% summarise(Cost=sum(cost_bil))%>% 
  group_by(Impact_year) %>% summarise(Species=n())
plot(icDatv3.0.sppXyr$Impact_year, icDatv3.0.sppXyr$Species, type="l", xlab="", ylab="spp")

## v4.1
# clean
icDatv4.1a <- icDatv4.1[-which(is.na(icDatv4.1$Probable_starting_year_adjusted)), ]
icDatv4.1b <- icDatv4.1a[-which(is.na(icDatv4.1a$Probable_ending_year_adjusted)), ]
icDatv4.1c <- icDatv4.1b[-which(is.na(icDatv4.1b$Cost_estimate_per_year_2017_USD_exchange_rate)), ]
icDatv4.1d <- icDatv4.1c[!grepl("/", icDatv4.1c$Species),]
icDatv4.1e <- icDatv4.1d[!grepl("sp.", icDatv4.1d$Species),]
icDatv4.1f <- icDatv4.1e[!grepl("spp.", icDatv4.1e$Species),] 

# expand
icDatv4.1.exp2 <- expandYearlyCosts(icDatv4.1f,startcolumn = "Probable_starting_year_adjusted",endcolumn = "Probable_ending_year_adjusted")
min(icDatv4.1.exp2$Impact_year, na.rm=T)
max(icDatv4.1.exp2$Impact_year, na.rm=T)

# filter
icDatv4.1.exp2 <- icDatv4.1.exp2 %>% filter(icDatv4.1.exp2$Impact_year <= 2021)
icDatv4.1.exp2 <- icDatv4.1.exp2 %>% filter(icDatv4.1.exp2$Impact_year >= 1960)
icDatv4.1.exp2$cost <- as.numeric(gsub(",", "", icDatv4.1.exp2$Cost_estimate_per_year_2017_USD_exchange_rate))
icDatv4.1.exp2 <- icDatv4.1.exp2[!is.na(icDatv4.1.exp2$cost),]
icDatv4.1.exp2$cost_bil <- (icDatv4.1.exp2$cost/1000000000)
sum(icDatv4.1.exp2$cost_bil)
nrow(icDatv4.1.exp2)
icDatv4.1.sppXyr <- icDatv4.1.exp2 %>% group_by(Impact_year, Species) %>% summarise(Cost=sum(cost_bil))%>% 
  group_by(Impact_year) %>% summarise(Species=n())
plot(icDatv4.1.sppXyr$Impact_year, icDatv4.1.sppXyr$Species, type="l", xlab="", ylab="spp")

# temporal trend in species reported output
spp.out <- data.frame(icDatv4.1.sppXyr$Impact_year, icDatv1.0.sppXyr$Species, icDatv2.1.sppXyr$Species, icDatv3.0.sppXyr$Species,
                      icDatv4.1.sppXyr$Species)
colnames(spp.out) <- c("year", "v1.0", "v2.1", "v3.0", "v4.1")
write.csv(spp.out, file="versSpp.csv", row.names = F)


###############################
## cumulative unique species ##
###############################
# latest version of database
data(invacost)

# remove 'diverse' category
invacost <- invacost[-which(invacost$Species == "Diverse/Unspecified"), ]

# remove multiple species entries (e.g. 'Rattus rattus/Mus musculus')
multi_sp_records <- unique(invacost$Species[grep("/", invacost$Species)])
invacost <- invacost[-which(invacost$Species %in% multi_sp_records), ]

# entries with unidentified species:
unidentified_sp <- unique(invacost$Species[grep("sp\\.", invacost$Species)])
unidentified_sp_groups <- unique(invacost$Species[grep("spp\\.", invacost$Species)])

# remove unidentified species
invacost <- invacost[-which(invacost$Species %in% c(unidentified_sp,
                                                    unidentified_sp_groups)), ]
# number of species
# total number of species in invacost 
length(unique(invacost$Species))

# expand the database
invacost <- invacost[-which(is.na(invacost$Probable_starting_year_adjusted)), ]
invacost <- invacost[-which(is.na(invacost$Probable_ending_year_adjusted)), ]

invacostexp <- expandYearlyCosts(invacost,
                                 startcolumn = "Probable_starting_year_adjusted",
                                 endcolumn = "Probable_ending_year_adjusted")

# filter out years later than 2021
invacostexp <- invacostexp[-which(invacostexp$Impact_year > 2021), ]

# number of species per year
results <- data.frame()
cumulative_sp_names <- NULL
for (year in seq(min(invacostexp$Impact_year),
                       max(invacostexp$Impact_year))) {
  subdb <- invacostexp[invacostexp$Impact_year == year, ]
  
  if(any(grep("/", subdb$Species))) {
    sp_names <- unique(unlist(strsplit(subdb$Species,
                                       "/")))
    if(any(sp_names %in% "Unspecified")) {
      sp_names <- sp_names[which(!(sp_names %in% "Unspecified"))]
    }
  } else {
    sp_names <- unique(subdb$Species)
  }
  
  # create vector that accumulates species names over time
  cumulative_sp_names <- c(cumulative_sp_names,
                           sp_names)
  cumulative_sp_names <- unique(cumulative_sp_names)
  
  results <- rbind(results,
                   data.frame(Year = year,
                              Number_species = length(sp_names),
                              Cumulative_number_species = length(cumulative_sp_names)))
}

plot(results$Year[which(results$Year > 1960)], results$Cumulative_number_species[which(results$Year > 1960)], type="l", xlab="", ylab="cumulative unique species")
write.csv(results, file="cumSpp.csv", row.names = F)
