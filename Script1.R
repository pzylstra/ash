# SCRIPT 1: IMPORT AND FORMAT DATA
library(dplyr)
library(readr)

# Summarises data into tables:
# siteStrata lists stratum heights and cover for each site
# siteData lists environmental attributes and delegatensis composition in each site


# Fire regime data
siteData <- read.csv("siteData.csv") %>%
  mutate(sev91 = case_when(dNBR91 > 225 & lastFire03 == 1991 ~ 3,
                           dNBR91 > 25 & lastFire03 == 1991 ~ 2,
                           TRUE ~ 1),
         sev03 = case_when(dNBR03 > 225 & lastFire06 == 2003 ~ 3,
                           dNBR03 > 25 & lastFire06 == 2003 ~ 2,
                           TRUE ~ 1),
         sev06 = case_when(dNBR06 > 225 & lastFire07 == 2006 ~ 3,
                           dNBR06 > 25 & lastFire07 == 2006 ~ 2,
                           TRUE ~ 1),
         sev07 = case_when(dNBR07 > 225 & lastFire13 == 2007 ~ 3,
                           dNBR07 > 25 & lastFire13 == 2007 ~ 2,
                           TRUE ~ 1),
         short = as.numeric(lastFire91 >= 1986)+as.numeric(lastFire03 >= 1998)+
           as.numeric(lastFire06 >= 2001)+as.numeric(lastFire07 >= 2002),
         shortAsh = as.numeric(lastFire91 >= 1971)+as.numeric(lastFire03 >= 1983)+
           as.numeric(lastFire06 >= 1986)+as.numeric(lastFire07 >= 1987),
         highSev = as.numeric(sev91 == 3)+as.numeric(sev03 == 3)+as.numeric(sev06 == 3)+as.numeric(sev07 == 3),
         TSF = surveyYear - lastFire13,
         TSFclass = ceiling(TSF/5)*5,
         lastSev = case_when(lastFire13 == 2007 ~ sev07,
                             lastFire13 == 2006 ~ sev06,
                             lastFire13 == 2003 ~ sev03,
                             TRUE ~ sev91))


# Floristics
smallPlants <- read.csv("under10cm.csv") %>%
  mutate(Numbers = New.number) %>%
  select(Site, Species, Numbers)
# Summarise delegatensis composition
smallDel <- smallPlants[which(smallPlants$Species=="Eucalyptus delegatensis"),] %>%
  mutate(del = Numbers) %>% select(Site, del) 
nSmall <- smallPlants %>%
  group_by(Site) %>%
  summarise_if(is.numeric, sum) %>%
  left_join(smallDel, by = "Site")
nSmall$del[which(is.na(nSmall$del))]<-0
nSmall$delCompY<-100*round(nSmall$del/nSmall$Numbers, 2)
nSmall <- nSmall %>%
  select(Site, delCompY)

largePlants <- read.csv("over10cm.csv") %>%
  select(Site, Species, DBH, Height) %>% # Ash ages From Fig. 2a in Mokany et. al. 2003 Tree Physiol.
  mutate(Age = (Species == "Eucalyptus delegatensis")*round(1.43*Height,0)) %>%
  left_join(siteData, by = "Site") %>%
  mutate(DBHdev = DBH - (2.39*Height))
largePlants$Age[which(largePlants$Age==0)]<-NA

# Summarise delegatensis composition
nLarge <- data.frame(matrix(ncol = 2, nrow = length(unique(largePlants$Site))))
colnames(nLarge) <- c('Site', 'delCompO')
n <- 1
for (site in unique(largePlants$Site)) {
  nLarge$Site[n] <- site
  nLarge$delCompO[n] <- round(length(which(largePlants$Species[largePlants$Site==site]=="Eucalyptus delegatensis"))/
                                length(largePlants$Species[largePlants$Site==site]),2)*100
  n<- n+1
}

delComp <- left_join(nSmall,nLarge, by = "Site")
delComp$delCompO[which(is.na(delComp$delCompO))]<-0

# Sort strata
siteStrataA <- read.csv("siteStrata.csv") %>%
  mutate(UpperHeight = pmax(LowerHeight, UpperHeight, na.rm = TRUE),
         Site = parse_number(SiteNumber),
         medianHeight = (LowerHeight + UpperHeight)/2)
siteStrata <- data.frame()
for (site in unique(siteStrataA$SiteNumber)) {
  s <- siteStrataA[siteStrataA$SiteNumber==site,]
  s <- s[order(s$UpperHeight, s$LowerHeight),]
  s$Stratum[1]<-"NS"
  s$Stratum[nrow(s)]<-"C"
  s$Stratum[s$Stratum!="NS" & s$Stratum!="C"][1]<-"E"
  s$Stratum[s$Stratum!="NS" & s$Stratum!="E" & s$Stratum!="C"][1]<-"M"
  s$Stratum[s$Stratum!="NS" & s$Stratum!="E" & s$Stratum!="M" & s$Stratum!="C"][1]<-"MU"
  siteStrata <- rbind(siteStrata, s)
}

siteStrata <- siteStrata %>%
  select(Site, Stratum, LowerHeight, medianHeight, UpperHeight, PercentCover)
siteData <- siteData %>%
  left_join(delComp, by = "Site")
ashData <- left_join(siteStrata, siteData, by = "Site")

# Divisions
siteMeans <- ashData %>%
  group_by(Site) %>%
  summarise_if(is.numeric, mean) %>%
  select(Elevation, Protection, Aspect)
elevations <- sort(siteMeans$Elevation)
protections <- sort(siteMeans$Protection)
  
altitude <- vector()
altitude[1] <- elevations[length(elevations)/3]
altitude[2] <- elevations[2*length(elevations)/3]

protection <- vector()
protection[1] <- protections[length(protections)/3]
protection[2] <- protections[2*length(protections)/3]

for (row in 1:nrow(ashData)) {
  ashData$altClass[row] <- case_when(ashData$Elevation[row] <= protection[1] ~ "L",
                                     ashData$Elevation[row] <= protection[2] ~ "M",
                                     TRUE ~ "H")
  ashData$protClass[row] <- case_when(ashData$Protection[row] <= protection[1] ~ "L",
                                     ashData$Protection[row] <= protection[2] ~ "M",
                                     TRUE ~ "H")
  ashData$aspClass[row] <- case_when(ashData$Aspect[row] >= 90 & ashData$Aspect[row] <270 ~ "S",
                                      TRUE ~ "N")
}

#####################
# Species composition
spListSmall <- unique(smallPlants$Species)
spListLarge <- unique(largePlants$Species)

largeCount <- data.frame(matrix(nrow = length(unique(largePlants$Site)), ncol = length(spListLarge)))
colnames(largeCount) <- spListLarge
rown <- 1
for (s in unique(largePlants$Site)) {
  largeCount$Site[rown] <- s
  siteDat <- largePlants[largePlants$Site == s,]
  for (sp in spListLarge) {
    spSub <- siteDat[siteDat$Species == sp,]
    largeCount[rown, spListLarge[which(sp == spListLarge)]] <- nrow(spSub)
  }

  rown <- rown + 1
}
########################