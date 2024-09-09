library(foreign)
library(sf)

# Imort ----
# Official DB
peak_db <- read.dbf("./input/peaks.DBF")

# Keep only above 6700m / conso
peak_db <- read.csv("./data_conso/peaks_osm_wiki_6700.csv", na.strings = "", dec = ",")
surv <- peak_db[!is.na(peak_db$LAT),] # long lat survey for missing values

# Wikidata ----
# Wiki query for Nepal and surrounding countries
# From https://www.wikidata.org/wiki/Wikidata:SPARQL_query_service/queries/examples#Mountains_over_8000_meters_elevation

# Nepal
q <- "SELECT ?item ?itemLabel ?coord ?elev 
WHERE{
  ?item p:P2044/psn:P2044/wikibase:quantityAmount ?elev ; # normalized height
        wdt:P625 ?coord ;
        wdt:P17 wd:Q837 ;
  FILTER(?elev > 6500) \
                  SERVICE wikibase:label \
                  { \
                    bd:serviceParam wikibase:language \"[AUTO_LANGUAGE], en \". \
                  }  \
}"

q2 <- "SELECT ?item ?itemLabel ?coord ?elev 
WHERE{
  ?item p:P2044/psn:P2044/wikibase:quantityAmount ?elev ; # normalized height
        wdt:P625 ?coord ;
        wdt:P17 wd:Q668 ;
  FILTER(?elev > 6500) \
                  SERVICE wikibase:label \
                  { \
                    bd:serviceParam wikibase:language \"[AUTO_LANGUAGE], en \". \
                  }  \
}"

q3 <- "SELECT ?item ?itemLabel ?coord ?elev 
WHERE{
  ?item p:P2044/psn:P2044/wikibase:quantityAmount ?elev ; # normalized height
        wdt:P625 ?coord ;
        wdt:P17 wd:Q148 ;
  FILTER(?elev > 6500) \
                  SERVICE wikibase:label \
                  { \
                    bd:serviceParam wikibase:language \"[AUTO_LANGUAGE], en \". \
                  }  \
}"
 
 # Get and combine data
nep <- data.frame(WikidataR::query_wikidata(q))
ind <- data.frame(WikidataR::query_wikidata(q2))
chn <- data.frame(WikidataR::query_wikidata(q3))
wiki <- rbind(nep, ind, chn)
wiki <- aggregate(wiki, by = list(wiki$item),
                  FUN = head, 1) 

# Keep only peaks incuded in the DB
wiki <- wiki[wiki$itemLabel %in% peak_db$PKCONSO,]
wiki$src <- "wiki"

# Transform in sf object
geometry <- st_as_sfc(wiki$coord, crs = 4326)
wiki <- st_as_sf(wiki, geometry)

# Feed peak db with geometries
peak_db <- merge(peak_db, wiki[,c("itemLabel", "src")], by.y = "itemLabel",
                 by.x = "PKCONSO", all.x = TRUE )
peak_db <- st_as_sf(peak_db)
peak_db <- peak_db[,c("PEAKID", "PKNAME", "PKCONSO", "src", "geometry")]
input <- peak_db

# Rest to found
peak_rest <- peak_db[is.na(peak_db$src),]
peak_db <- peak_db[!is.na(peak_db$src),]
peak_rest <- st_set_geometry(peak_rest, NULL)
peak_rest$src <- NULL


# OpenStreetMap ----
# Import points from Nepal and neigboring countries 
library(osmextract)
tib <- oe_get(place = "Tibet",
                 layer = "points",
                 extra_tags = c("natural", "peak", "int_name", "name:en", "ele"))

xij <- oe_get(place = "Xinjiang",
                 layer = "points",
                 extra_tags = c("natural", "peak", "int_name","name:en","ele"))

nep <- oe_get(place = "Nepal",
                 layer = "points",
                 extra_tags = c("natural", "peak", "int_name", "name:en", "ele"))

ind <- oe_get(place = "Northern Zone",
                 layer = "points",
                 extra_tags = c("natural", "peak", "int_name","name:en", "ele"))

ind2 <- oe_get(place = "Central Zone",
                 layer = "points",
                 extra_tags = c("natural", "peak", "int_name", "name:en", "ele"))


# Data clean
osm <- rbind(tib, xij, nep, ind, ind2)
osm <- osm[!is.na(osm$natural),]
osm <- osm[osm$natural == "peak",]
osm$ele <- as.numeric(osm$ele)
osm <- osm[!is.na(osm$ele),]
osm <- aggregate(osm, by = list(osm$osm_id),
                  FUN = head, 1)

# Check if 3 names field fit with peak name
osm1 <- osm[osm$name %in% peak_rest$PKCONSO,]
osm2 <- osm[osm$int_name %in% peak_rest$PKCONSO,]
osm3 <- osm[osm$name_en %in% peak_rest$PKCONSO,]
osm <- rbind(osm1, osm2, osm3)
osm <- aggregate(osm, by = list(osm$osm_id),
                  FUN = head, 1)
osm$src <- "osm"

# Bind all OSM information
peak_rest1 <-  merge(peak_rest, osm[,c("src", "name")], by.x = "PKCONSO", by.y = "name", all.x = TRUE)
peak_rest2 <-  merge(peak_rest, osm[,c("src", "int_name")], by.x = "PKCONSO", by.y = "int_name", all.x = TRUE)
peak_rest3 <-  merge(peak_rest, osm[,c("src", "name_en")], by.x = "PKCONSO", by.y = "name_en", all.x = TRUE)
peak_rest1 <- peak_rest1[!is.na(peak_rest1$src),]
peak_rest2 <- peak_rest2[!is.na(peak_rest2$src),]
peak_rest3 <- peak_rest3[!is.na(peak_rest3$src),]
peak_rest <- rbind(peak_rest1, peak_rest2, peak_rest3)
peak_rest <- aggregate(peak_rest, by = list(peak_rest$PEAKID),
                       FUN = head, 1)

# Transform in sf
peak_rest$Group.1 <- NULL
peak_rest <- st_as_sf(peak_rest)
peak_rest <- st_set_crs(peak_rest, 4326)

# Bind with Wikipedia info
peak_db <- rbind(peak_db, peak_rest)

# Personal survey for missing long/lat----
## With https://nepalhimalpeakprofile.org/
surv <- surv[!surv$PEAKID %in% peak_db$PEAKID,]
surv <- st_as_sf(surv, coords = c("LONG", "LAT"), crs = 4326)
surv$src <- "survey"
peak_db <- rbind(peak_db, surv[,names(peak_db)])

# Export ----
peak_db <- peak_db[,c("PEAKID", "PKCONSO", "src", "geometry")]
st_write(peak_db, "./data_conso/peak_6700_geoloc.geojson")
