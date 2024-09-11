library(sf)
library(foreign)

# Prep data
peaks <- st_read("./data_conso/peak_6700_geoloc.geojson")
peaks <- st_centroid(peaks)
id <- peaks$PEAKID

# Import data from Himalayan DB
# Version 2.72 withAutumn-Winter 2023 update

# Peak data ----
peaks_df <- read.dbf("./input/peaks.DBF")
peaks_df  <- peaks_df[peaks_df$PEAKID %in% id,
                      c("PEAKID", "HEIGHTM", "OPEN", "PYEAR", "PSUMMITERS", "PCOUNTRY")] 

peaks <- merge(peaks, peaks_df, by = "PEAKID", all.x = TRUE)

# Expeditions data ----
exped <- read.dbf("./input/exped.DBF")
exped <- exped[exped$PEAKID %in% id,] # 10337 obs over 11323 repertoried expeditions

## Expedition statistics ----
exped_df <- data.frame(unique(exped$PEAKID)) 
names(exped_df) <- "PEAKID"

# Median time to reach summit TOTDAYS  without 0
tmp <- exped[exped$TOTDAYS > 0,]
tmp <- aggregate(tmp[,"TOTDAYS"], by = list(PEAKID = tmp$PEAKID), 
                      FUN = median)
names(tmp)[2] <- "TOTDAYS"
exped_df <- merge(exped_df, tmp, by = "PEAKID", all.x = TRUE)

# Ascent number (length)
tmp <- aggregate(exped[,"EXPID"], by = list(PEAKID = exped$PEAKID), 
                      FUN = length)
names(tmp)[2] <- "EXPE_TOT"
exped_df <- merge(exped_df, tmp, by = "PEAKID", all.x = TRUE)

# Ascent success
exped$SUCCESS1 <-  as.integer(as.logical(exped$SUCCESS1))
exped$SUCCESS2 <-  as.integer(as.logical(exped$SUCCESS2))
exped$SUCCESS3 <-  as.integer(as.logical(exped$SUCCESS3))
exped$SUCCESS4 <-  as.integer(as.logical(exped$SUCCESS4))
exped$SUCCESS <- exped$SUCCESS1 + exped$SUCCESS2 + exped$SUCCESS3 + exped$SUCCESS4
exped["SUCCESS"][exped["SUCCESS"] >= 1] <- 1
tmp <- exped[exped$SUCCESS >= 1,]
tmp <- aggregate(tmp[,"SUCCESS"], by = list(PEAKID = tmp$PEAKID), 
                 FUN = length)
names(tmp)[2] <- "EXPE_SUCCESS"
exped_df <- merge(exped_df, tmp, by = "PEAKID", all.x = TRUE)

# TOTHIRED > 0 
tmp <- exped[exped$TOTHIRED > 0,]
tmp <- aggregate(tmp[,"TOTHIRED"], by = list(PEAKID = tmp$PEAKID), 
                 FUN = length)
names(tmp)[2] <- "EXPE_HIRED"
exped_df <- merge(exped_df, tmp, by = "PEAKID", all.x = TRUE)

# Deaths
tmp <- exped[exped$MDEATHS > 0,]
tmp <- aggregate(tmp[,"MDEATHS"], by = list(PEAKID = tmp$PEAKID), 
                 FUN = length)
names(tmp)[2] <- "EXPE_DEATH"
exped_df <- merge(exped_df, tmp, by = "PEAKID", all.x = TRUE)


## Members statistics
tmp <- aggregate(exped[,"TOTMEMBERS"], by = list(PEAKID = exped$PEAKID), 
                 FUN = sum)
names(tmp)[2] <- "MEMBERS_TOT"
exped_df <- merge(exped_df, tmp, by = "PEAKID", all.x = TRUE)

tmp <- exped[exped$SUCCESS >= 1,]
tmp <- aggregate(tmp[,"TOTMEMBERS"], by = list(PEAKID = tmp$PEAKID), 
                 FUN = sum)
names(tmp)[2] <- "MEMBERS_SUCCESS"
exped_df <- merge(exped_df, tmp, by = "PEAKID", all.x = TRUE)

tmp <- exped[exped$TOTHIRED > 0,]
tmp <- aggregate(tmp[,"TOTHIRED"], by = list(PEAKID = tmp$PEAKID), 
                 FUN = sum)
names(tmp)[2] <- "MEMBERS_HIRED"
exped_df <- merge(exped_df, tmp, by = "PEAKID", all.x = TRUE)

tmp <- exped[exped$MDEATHS > 0,]
tmp <- aggregate(tmp[,"MDEATHS"], by = list(PEAKID = tmp$PEAKID), 
                 FUN = sum)
names(tmp)[2] <- "MEMBERS_DEATH"
exped_df <- merge(exped_df, tmp, by = "PEAKID", all.x = TRUE)

# Change NA by 0
exped_df[is.na(exped_df)] <- 0

peaks <- merge(peaks, exped_df, by = "PEAKID", all.x = TRUE)

# Aggregate peak data
peaks <- peaks[peaks$PEAKID != "CHMM",]

# Aggregate peaks if necessary (unkown geolocation of subsumits)
peaks <- peaks[order(peaks$PYEAR),]

tmp <- aggregate(peaks[,c("src", "HEIGHTM", "OPEN", "PYEAR",
                          "PSUMMITERS", "PCOUNTRY")],
                 by = list(PKCONSO = peaks$PKCONSO),
                 FUN = head, 1)

tmp2 <- st_set_geometry(peaks, NULL)
tmp3 <- aggregate(tmp2[,c("EXPE_TOT", "EXPE_SUCCESS", "EXPE_HIRED",
                          "EXPE_DEATH", "MEMBERS_TOT", "MEMBERS_SUCCESS",
                          "MEMBERS_HIRED", "MEMBERS_DEATH")],
                  by = list(PKCONSO = tmp2$PKCONSO),
                  FUN = sum)

tmp4 <- aggregate(tmp2[,c("TOTDAYS")],
                  by = list(PKCONSO = tmp2$PKCONSO),
                  FUN = median)
names(tmp4)[2] <- "TOTDAYS"

# Results consolidation
peaks <- merge(tmp, tmp4, by = "PKCONSO", all.x = TRUE)
peaks <- merge(peaks, tmp3, by = "PKCONSO", all.x = TRUE)

# Indicator creation
peaks$EXPE_SIZE <- round(peaks$MEMBERS_TOT / peaks$EXPE_TOT, 2)
peaks$EXPE_SUCCESS_RT <- round(peaks$EXPE_SUCCESS / peaks$EXPE_TOT* 100, 2)
peaks$EXPE_DEATH_RT <- round(peaks$EXPE_DEATH / peaks$EXPE_TOT * 100, 2)
peaks$EXPE_HIRED_RT <- round(peaks$EXPE_HIRED / peaks$EXPE_TOT * 100, 2)
peaks$MEMBERS_SUCCESS_RT <- round(peaks$MEMBERS_SUCCESS / peaks$MEMBERS_TOT* 100, 2)
peaks$MEMBERS_DEATH_RT <- round(peaks$MEMBERS_DEATH / peaks$MEMBERS_TOT * 100, 2)
peaks$MEMBERS_HIRED_RT <- round(peaks$MEMBERS_HIRED / peaks$MEMBERS_TOT * 100, 2)

# For positioning only
peaks$pos <- 1

# Reorder
peaks <- peaks[,c("PKCONSO", "HEIGHTM", "TOTDAYS", "EXPE_TOT", "EXPE_SUCCESS", 
                   "EXPE_HIRED",  "EXPE_DEATH", "EXPE_SIZE", "MEMBERS_TOT",
                  "MEMBERS_SUCCESS", "MEMBERS_HIRED", "MEMBERS_DEATH",
                  "EXPE_SUCCESS_RT", "EXPE_HIRED_RT","EXPE_DEATH_RT", 
                  "MEMBERS_SUCCESS_RT", "MEMBERS_HIRED_RT",
                  "MEMBERS_DEATH_RT", "OPEN", "PYEAR", "PSUMMITERS", "PCOUNTRY",
                  "src", "pos")]

# Export
st_write(peaks, "./data_conso/peak_6700_geoloc_data.geojson")


# Members data ----
members <- read.dbf("./input/members.DBF")
members$un <- paste0(members$FNAME, members$LNAME)
length(unique(members$un))
members$MYEAR <- as.numeric(levels(members$MYEAR))[members$MYEAR]
members$LEADER <-  as.integer(as.logical(members$LEADER))

# Consolidated citizenship with ISO3 codes
# First nationality declared
# See table of correspondance for historical choice made (Czechoslovakia > Cech Republic and not Slovakia)
iso <- read.csv("./data_conso/ISO3_CITIZENSHIP.csv")
members <- merge(members, iso, by = "CITIZEN", all.x = TRUE)

df <- data.frame(unique(members$ISO3)) # used to consolidate citizenship
names(df) <- "ISO3"


# All
tmp <- aggregate(members[,"ISO3"],
                 by = list(ISO3 = members$ISO3),
                 FUN = length)

yy2 <- members[members$SEX == "F",]
tmp2 <- aggregate(yy2[,"ISO3"],
                  by = list(ISO3 = yy2$ISO3),
                  FUN = length)

yy2 <- members[members$LEADER == 1,]
tmp3 <- aggregate(yy2[,"ISO3"],
                  by = list(ISO3 = yy2$ISO3),
                  FUN = length)

head(tmp)
df <- merge(df, tmp, by = "ISO3", all.x = TRUE)
df <- merge(df, tmp2, by = "ISO3", all.x = TRUE)
df <- merge(df, tmp3, by = "ISO3", all.x = TRUE)

names(df)[2:4] <- c("TOT_ALL", "FEMALE_ALL", "LEADER_ALL")


# Exploratory period (before 1949)
yy <- members[members$MYEAR < 1950,]

tmp <- aggregate(yy[,"ISO3"],
                 by = list(ISO3 = yy$ISO3),
                 FUN = length)

df <- merge(df, tmp, by = "ISO3", all.x = TRUE)
names(df)[length(df)] <- "TOT_BEF_1950"

# Expeditionary period (1950-1969)
yy <- members[members$MYEAR >= 1950,]
yy <- yy[yy$MYEAR < 1970,]

tmp <- aggregate(yy[,"ISO3"],
                 by = list(ISO3 = yy$ISO3),
                 FUN = length)

df <- merge(df, tmp, by = "ISO3", all.x = TRUE)
names(df)[length(df)]<- "TOT_1950_1969"

# Transitional period (1970-1989)
yy <- members[members$MYEAR >= 1970,]
yy <- yy[yy$MYEAR < 1989,]

tmp <- aggregate(yy[,"ISO3"],
                 by = list(ISO3 = yy$ISO3),
                 FUN = length)

df <- merge(df, tmp, by = "ISO3", all.x = TRUE)
names(df)[length(df)]<- "TOT_1970_1989"

# Commercial period (after 1990)
yy <- members[members$MYEAR >= 1990,]

tmp <- aggregate(yy[,"ISO3"],
                 by = list(ISO3 = yy$ISO3),
                 FUN = length)
df <- merge(df, tmp, by = "ISO3", all.x = TRUE)

names(df)[length(df)] <- "TOT_AFT_1990"
df[is.na(df)] <- 0


# Median year of ascension
med <- aggregate(members[,"MYEAR"],
                 by = list(ISO3 = members$ISO3),
                 FUN = median)

names(med)[2] <- "MED_YEAR"
df <- merge(df, med, by = "ISO3", all.x = TRUE)

# Join layer
world <- st_read("./input/world_209.geojson")
world <- world[,c("ISO3", "NAMEen", "POP_2023")]
world <- merge(world, df, by = "ISO3", all.x = TRUE)

# Buid indicators (ratios)
world$RT_POP <- world$TOT_ALL / world$POP_2023 * 1000000
world$RT_FEMALE <- world$FEMALE_ALL / world$TOT_ALL * 100
world$RT_LEADER <- world$LEADER_ALL / world$TOT_ALL * 100
world$RT_1950 <- world$TOT_BEF_1950 / world$TOT_ALL * 100
world$RT_1950_1969 <- world$TOT_1950_1969 / world$TOT_ALL * 100
world$RT_1970_1989 <- world$TOT_1970_1989 / world$TOT_ALL * 100
world$RT_1990 <- world$TOT_AFT_1990 / world$TOT_ALL * 100

st_write(world, "./data_conso/world_data.geojson")

names(world)
