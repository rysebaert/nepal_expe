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
                      c("PEAKID", "HEIGHTM", "OPEN", "PYEAR", "PSUMMITERS", "PCOUNTRY")] # 10338 obs

peaks <- merge(peaks, peaks_df, by = "PEAKID", all.x = TRUE)

# Expeditions data ----
exped <- read.dbf("./input/exped.DBF")
exped <- exped[exped$PEAKID %in% id,] # 10338 obs

# Ascent number (length)
exped_df <- aggregate(exped[,"EXPID"], by = list(PEAKID = exped$PEAKID), 
                      FUN = length)
names(exped_df)[2] <- "N_EXPE"

# Ascent success
exped$SUCCESS1 <-  as.integer(as.logical(exped$SUCCESS1))
exped$SUCCESS2 <-  as.integer(as.logical(exped$SUCCESS2))
exped$SUCCESS3 <-  as.integer(as.logical(exped$SUCCESS3))
exped$SUCCESS4 <-  as.integer(as.logical(exped$SUCCESS4))
exped$SUCCESS <- exped$SUCCESS1 + exped$SUCCESS2 + exped$SUCCESS3 + exped$SUCCESS4
exped["SUCCESS"][exped["SUCCESS"] >= 1] <- 1
str(exped)

# MDEATHS if > 0 expe with death
tmp <- exped[exped$SUCCESS >= 1,]
tmp <- aggregate(tmp[,"SUCCESS"], by = list(PEAKID = tmp$PEAKID), 
                 FUN = length)
names(tmp)[2] <- "N_SUCCESS"
exped_df <- merge(exped_df, tmp, by = "PEAKID", all.x = TRUE)

# Median time to reach summit TOTDAYS  without 0
tmp <- exped[exped$TOTDAYS > 0,]
tmp <- aggregate(tmp[,"TOTDAYS"], by = list(PEAKID = tmp$PEAKID), 
                      FUN = median)
names(tmp)[2] <- "TOTDAYS"
exped_df <- merge(exped_df, tmp, by = "PEAKID", all.x = TRUE)

# MDEATHS if > 0 expe with death
tmp <- exped[exped$MDEATHS > 0,]
tmp <- aggregate(tmp[,"MDEATHS"], by = list(PEAKID = tmp$PEAKID), 
                 FUN = length)
names(tmp)[2] <- "EXPE_DEATH"
exped_df <- merge(exped_df, tmp, by = "PEAKID", all.x = TRUE)

tmp <- exped[exped$MDEATHS > 0,]
tmp <- aggregate(tmp[,"MDEATHS"], by = list(PEAKID = tmp$PEAKID), 
                 FUN = sum)
names(tmp)[2] <- "SUM_EXPE_DEATH"
exped_df <- merge(exped_df, tmp, by = "PEAKID", all.x = TRUE)

# TOTHIRED > 0 
tmp <- exped[exped$TOTHIRED > 0,]
tmp <- aggregate(tmp[,"TOTHIRED"], by = list(PEAKID = tmp$PEAKID), 
                 FUN = length)
names(tmp)[2] <- "TOT_HIRED"
exped_df <- merge(exped_df, tmp, by = "PEAKID", all.x = TRUE)

# Expe hired death (N)
tmp <- exped[exped$TOTHIRED > 0,]
tmp <- tmp[tmp$HDEATHS > 0, ]
tmp <- aggregate(tmp[,"HDEATHS"], by = list(PEAKID = tmp$PEAKID), 
                 FUN = length)
names(tmp)[2] <- "EXPE_HIRED_DEATH"
exped_df <- merge(exped_df, tmp, by = "PEAKID", all.x = TRUE)

# Expe hired death (Sum)
tmp <- exped[exped$TOTHIRED > 0,]
tmp <- tmp[tmp$HDEATHS > 0, ]
tmp <- aggregate(tmp[,"HDEATHS"], by = list(PEAKID = tmp$PEAKID), 
                 FUN = sum)
names(tmp)[2] <- "SUM_EXPE_HIRED_DEATH"
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
tmp3 <- aggregate(tmp2[,c("N_EXPE", "N_SUCCESS", "EXPE_DEATH",
                          "SUM_EXPE_DEATH", "TOT_HIRED", "EXPE_HIRED_DEATH",
                          "SUM_EXPE_HIRED_DEATH")],
                  by = list(PKCONSO = tmp2$PKCONSO),
                  FUN = sum)

tmp4 <- aggregate(tmp2[,c("TOTDAYS")],
                  by = list(PKCONSO = tmp2$PKCONSO),
                  FUN = median)

# Results consolidation
peaks <- merge(tmp, tmp4, by = "PKCONSO", all.x = TRUE)
peaks <- merge(peaks, tmp3, by = "PKCONSO", all.x = TRUE)
names(peaks)[8] <- "TOTDAYS"

# Indicator creation
peaks$SUCCESS_RT <- peaks$N_SUCCESS / peaks$N_EXPE * 100
peaks$DEATH_RT <- peaks$EXPE_DEATH / peaks$N_EXPE * 100
peaks$HIRED_RT <- peaks$TOT_HIRED / peaks$N_EXPE * 100
peaks$HIRED_DEATH_RT <- peaks$EXPE_HIRED_DEATH / peaks$TOT_HIRED * 100

# Export
st_write(peaks, "./data_conso/peak_6700_geoloc_data.geojson")


# Members data ----
members <- read.dbf("./input/members.DBF")

members$MYEAR <- as.numeric(levels(members$MYEAR))[members$MYEAR]
df <- data.frame(unique(members$CITIZEN)) # used to consolidate citizenship
names(df) <- "CITIZEN"

members$LEADER <-  as.integer(as.logical(members$LEADER))

# 
# First nationality declared
# See table of correspondance for historical choice made (Czechoslovakia > Cech Republic and not Slovakia)

# All
tmp <- aggregate(members[,"CITIZEN"],
                 by = list(CITIZEN = members$CITIZEN),
                 FUN = length)

yy2 <- members[members$SEX == "M",]
tmp2 <- aggregate(yy2[,"CITIZEN"],
                  by = list(CITIZEN = yy2$CITIZEN),
                  FUN = length)

yy2 <- members[members$LEADER == 1,]
tmp3 <- aggregate(yy2[,"CITIZEN"],
                  by = list(CITIZEN = yy2$CITIZEN),
                  FUN = length)

df <- merge(df, tmp, by = "CITIZEN", all.x = TRUE)
df <- merge(df, tmp2, by = "CITIZEN", all.x = TRUE)
df <- merge(df, tmp3, by = "CITIZEN", all.x = TRUE)

names(df)[2:4] <- c("TOT_ALL", "MALE_ALL", "LEADER_ALL")


# Before 60's
yy <- members[members$MYEAR < 1960,]

tmp <- aggregate(yy[,"CITIZEN"],
                 by = list(CITIZEN = yy$CITIZEN),
                 FUN = length)

yy2 <- yy[yy$SEX == "M",]
tmp2 <- aggregate(yy2[,"CITIZEN"],
                 by = list(CITIZEN = yy2$CITIZEN),
                 FUN = length)

yy2 <- yy[yy$LEADER == 1,]
tmp3 <- aggregate(yy2[,"CITIZEN"],
                  by = list(CITIZEN = yy2$CITIZEN),
                  FUN = length)

df <- merge(df, tmp, by = "CITIZEN", all.x = TRUE)
df <- merge(df, tmp2, by = "CITIZEN", all.x = TRUE)
df <- merge(df, tmp3, by = "CITIZEN", all.x = TRUE)

names(df)[(length(df)-2):length(df)] <-  c("TOT_BEF_1960", "MALE_BEF_1960", "LEADER_BEF_1960")

# 60's - 80'S
yy <- members[members$MYEAR >= 1960,]
yy <- yy[yy$MYEAR < 1980,]

tmp <- aggregate(yy[,"CITIZEN"],
                 by = list(CITIZEN = yy$CITIZEN),
                 FUN = length)

yy2 <- yy[yy$SEX == "M",]
tmp2 <- aggregate(yy2[,"CITIZEN"],
                  by = list(CITIZEN = yy2$CITIZEN),
                  FUN = length)

yy2 <- yy[yy$LEADER == 1,]
tmp3 <- aggregate(yy2[,"CITIZEN"],
                  by = list(CITIZEN = yy2$CITIZEN),
                  FUN = length)

df <- merge(df, tmp, by = "CITIZEN", all.x = TRUE)
df <- merge(df, tmp2, by = "CITIZEN", all.x = TRUE)
df <- merge(df, tmp3, by = "CITIZEN", all.x = TRUE)

names(df)[(length(df)-2):length(df)] <-  c("TOT_1960_1980", "MALE_1960_1980", "LEADER_1960_1980")

# 80's - 2000'S
yy <- members[members$MYEAR >= 1980,]
yy <- yy[yy$MYEAR < 2000,]

tmp <- aggregate(yy[,"CITIZEN"],
                 by = list(CITIZEN = yy$CITIZEN),
                 FUN = length)

yy2 <- yy[yy$SEX == "M",]
tmp2 <- aggregate(yy2[,"CITIZEN"],
                  by = list(CITIZEN = yy2$CITIZEN),
                  FUN = length)

yy2 <- yy[yy$LEADER == 1,]
tmp3 <- aggregate(yy2[,"CITIZEN"],
                  by = list(CITIZEN = yy2$CITIZEN),
                  FUN = length)

df <- merge(df, tmp, by = "CITIZEN", all.x = TRUE)
df <- merge(df, tmp2, by = "CITIZEN", all.x = TRUE)
df <- merge(df, tmp3, by = "CITIZEN", all.x = TRUE)

names(df)[(length(df)-2):length(df)] <-  c("TOT_1980_2000", "MALE_1980_2000", "LEADER_1980_2000")

# After 2000's
yy <- members[members$MYEAR >= 2000,]

tmp <- aggregate(yy[,"CITIZEN"],
                 by = list(CITIZEN = yy$CITIZEN),
                 FUN = length)

yy2 <- yy[yy$SEX == "M",]
tmp2 <- aggregate(yy2[,"CITIZEN"],
                  by = list(CITIZEN = yy2$CITIZEN),
                  FUN = length)

yy2 <- yy[yy$LEADER == 1,]
tmp3 <- aggregate(yy2[,"CITIZEN"],
                  by = list(CITIZEN = yy2$CITIZEN),
                  FUN = length)

df <- merge(df, tmp, by = "CITIZEN", all.x = TRUE)
df <- merge(df, tmp2, by = "CITIZEN", all.x = TRUE)
df <- merge(df, tmp3, by = "CITIZEN", all.x = TRUE)

names(df)[(length(df)-2):length(df)] <-  c("TOT_AFT_2000", "MALE_AFT_2000", "LEADER_AFT_2000")
df[is.na(df)] <- 0

# Median year of ascension
med <- aggregate(members[,"MYEAR"],
                 by = list(CITIZEN = members$CITIZEN),
                 FUN = median)

names(med)[2] <- "MED_YEAR"
df <- merge(df, med, by = "CITIZEN", all.x = TRUE)

# Import consolidated df
iso <- read.csv("ISO3_CITIZENSHIP.csv")
iso <- merge(iso, df, by = "CITIZEN", all.x = TRUE)
iso$MED_YEAR <- iso$MED_YEAR * iso$TOT_ALL

iso <- aggregate(iso[,c(3:length(iso))], 
                 by = list(ISO3 = iso$ISO3),
                 FUN = sum)
iso$MED_YEAR <- iso$MED_YEAR / iso$TOT_ALL

# Join layer
world <- st_read("./input/world_209.geojson")
world <- world[,c("ISO3", "NAMEen", "POP_2023")]
world <- merge(world, iso, by = "ISO3", all.x = TRUE)

st_write(world, "./data_conso/world_data.geojson")


