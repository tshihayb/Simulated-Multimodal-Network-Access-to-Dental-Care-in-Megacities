#######################################################################################
# programmer:   Talal Alshihayb
# Date:         February 19, 2026
# Purpose:      Simulated Multimodal Network Access to Dental Care in Megacities
# Last updated: February 19, 2026 
#######################################################################################


###################################################
###################################################
# Section 1: Preparation before reading the dataset
###################################################
###################################################
{
  # 1.1   Cleaning global environment (remove any previously saved objects in environment)
  rm(list = ls())
  
  # 1.2   Setting the working space so objects can be saved in it without referring to it
  # again in saving functions
  # you can change the path below to a location you prefer
  # Try / or \\ or \ if you are using Mac
  setwd("C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data")
  
  # 1.3   Checking the working space location
  getwd()

  # Load needed packages only
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(labelled)
}

#########################################
#########################################
# Section 2: Reading in data and cleaning
#########################################
#########################################
{
  # Reading in full data after manually adding MOH facilities
  clinics <- read_excel("all facilities.xlsx", sheet = "Sheet1") %>%
    clean_names() %>%
    # Derive with_dental_services from type_with_dental (non-empty => provided dental services)
    mutate(
      with_dental_services = if_else(
        !is.na(type_with_dental) & trimws(as.character(type_with_dental)) != "",
        "Yes", "No"
      )
    ) %>%
    select(
      id, district_in_arabic, district_in_english, new_region,
      type_of_clinic, longitudes, latitudes,
      with_dental_services, type_with_dental, type_without_dental, private_or_public
    ) %>%
    mutate(
      # Convert already coded variables to factors
      type_of_clinic = factor(
        type_of_clinic,
        levels = c(
          "Dental clinic only",
          "Polyclinic/Cosmetic clinic with dental",
          "Polyclinic/Cosmetic clinic without dental",
          "Hospital with dental",
          "Hospital without dental",
          "Primary care center with dental clinic",
          "Specialized dental center"
        )
      ),
      with_dental_services = factor(with_dental_services, levels = c("Yes", "No")),
      type_with_dental = factor(
        type_with_dental,
        levels = c(
          "Dental clinic only",
          "Polyclinic/Cosmetic clinic",
          "Hospital",
          "Primary care center",
          "Specialized dental center"
        )
      ),
      type_without_dental = factor(
        type_without_dental,
        levels = c("Polyclinic/Cosmetic clinic", "Hospital")
      ),
      new_region = factor(new_region, levels = c("North", "East", "Center", "West", "South")),
      private_or_public = factor(private_or_public, levels = c("Private", "Public")),
      longitudes = as.numeric(gsub("[^0-9.\\-]", "", longitudes)),
      latitudes = as.numeric(gsub("[^0-9.\\-]", "", latitudes))
    ) %>%
    set_variable_labels(
      id                   = "ID",
      district_in_arabic   = "District of facility in Arabic",
      district_in_english  = "District of facility in English",
      new_region           = "Official region of facility recently determined by Riyadh Municipality",
      type_of_clinic       = "Type of facility",
      longitudes           = "Longitude",
      latitudes            = "Latitude",
      with_dental_services = "Facility provided dental services",
      type_with_dental     = "Type of facility that provided dental services",
      type_without_dental  = "Type of facility that did not provide dental services",
      private_or_public    = "Private or Public facility"
    )
  
  # Converting coords to numeric
  clinics <- clinics %>%
    mutate(
      longitudes = as.numeric(longitudes),
      latitudes  = as.numeric(latitudes)
    )
}     

##################################################################
##################################################################
# Section 3: Making the Riyadh Map with regions ready for plotting
##################################################################
##################################################################

{
  # Need packages
  library(sf)     #only this was used

  # --- R1-4 build (2026-06-27): Smart App Control (enforce) blocks the unsigned
  #     s2.dll, crashing st_make_valid below. Route sf through GEOS (planar)
  #     instead of s2 (spherical) so s2.dll never loads. All quantitative work is
  #     in projected UTM (EPSG:32638), which never uses s2, so results are
  #     unaffected; this only changes the geometry engine for lon/lat polygon
  #     validation (maps). To revert (e.g. if SAC is disabled): delete this line.
  suppressMessages(sf::sf_use_s2(FALSE))

  # Convert to sf object
  gisdata <- clinics %>%
    st_as_sf(coords = c("longitudes", "latitudes"), crs = 4326)
  
  # 1. Load the Riyadh districts GeoJSON
  riyadh_distr <- st_read("districts.geojson")
  
  # 2. Filter to Riyadh city and fix geometries if needed
  riyadh_only <- riyadh_distr %>%
    filter(city_id == 3) %>%
    mutate(geometry = st_make_valid(geometry))
  
  # 3. Define Al Bashaer manual boundary coordinates
  coords_manual <- matrix(c(
    46.87291145324707,   24.968163214832895,
    46.85218334197998,   24.9918538240452,
    46.851024627685554,  24.99383755957201,
    46.850080490112305,  24.994421005104304,
    46.84879302978516,   24.994693278738193,
    46.842269897460945,  25.000566463128987,
    46.81252956390381,   25.017367665213843,
    46.793818473815925,  25.02502856137325,
    46.78300380706788,   25.028644960423627,
    46.77321910858155,   25.03078364089392,
    46.76510810852051,   25.03171687159886,
    46.76120281219483,   25.031833524937895,
    46.86741828918458,   24.965867823056886,
    46.87063694000244,   24.95968172358859,
    46.86342716217042,   24.938864577975956,
    46.86038017272949,   24.93384459026544,
    46.85720443725586,   24.927189872768054,
    46.857032775878906,  24.920729394011193,
    46.856689453125,     24.919367201667008,
    46.85514450073242,   24.918666639745364,
    46.856217384338386,  24.917537948278824,
    46.85411453247071,   24.91247816994592,
    46.85265541076661,   24.908079886329716,
    46.85265541076661,   24.908040962765924,
    46.847248077392585,  24.905705526469728,
    46.84063911437988,   24.90500488696166,
    46.83917999267579,   24.90990927997662,
    46.83587551116944,   24.91625356266607,
    46.8314552307129,    24.921858056268917,
    46.82411670684815,   24.928201724319663,
    46.810040473937995,  24.9407324281741,
    46.78853988647462,   24.959992981304392,
    46.78643703460694,   24.96248301469683,
    46.78163051605225,   24.969913908591025,
    46.78047180175781,   24.97302619154367,
    46.77472114562989,   24.98990895421319,
    46.76592350006104,   25.017017663614194
  ), ncol = 2, byrow = TRUE)
  
  # 4. Apply the specified point order: 1-12, then 37-13, then back to 1
  coords_ordered <- rbind(
    coords_manual[1:12, ],       # Forward from 1 to 12
    coords_manual[37:13, ],      # Backwards from 37 to 13
    coords_manual[1, ]           # Close the polygon back to point 1
  )
  
  # 5. Build the polygon
  al_bashaer_polygon <- st_polygon(list(coords_ordered)) %>%
    st_sfc(crs = st_crs(riyadh_only))
  
  # Check validity and fix if necessary
  if (!st_is_valid(al_bashaer_polygon)) {
    al_bashaer_polygon <- st_make_valid(al_bashaer_polygon)
  }
  
  # 6. Create sf object for Al Bashaer District
  al_bashaer_sf <- st_sf(
    district_id = NA,
    city_id = 3,
    region_id = NA,
    name_ar = "حي البشائر",
    name_en = "Al Bashaer Dist.",
    geometry = al_bashaer_polygon
  )
  al_bashaer_sf <- al_bashaer_sf[, names(riyadh_only)]
  
  # 7. Combine with existing Riyadh districts
  riyadh_with_bashaer <- rbind(riyadh_only, al_bashaer_sf)
  
  # Rename Al Sidrah Dist. to Asehbaa Dist. and حي السدرة to حي السهباء
  riyadh_with_bashaer <- riyadh_with_bashaer %>%
    mutate(
      name_ar = ifelse(name_ar == "حي السدرة", "حي السهباء", name_ar),
      name_en = ifelse(name_en == "Al Sidrah Dist.", "Asehbaa Dist.", name_en)
    )
  
  # 1. Define Sedrah District manual boundary coordinates
  coords_sedrah <- matrix(c(
    46.723651885986335, 24.831142982954567,
    46.760902404785156, 24.850148046867947,
    46.75867080688477,  24.85388640440865,
    46.7545509338379,   24.85154994418473,
    46.75352096557618,  24.8531075892375,
    46.77429199218751,  24.86354330510114,
    46.775321960449226, 24.860739766457296,
    46.77772521972657,  24.859026461549753,
    46.790084838867195, 24.865412295547756,
    46.7827033996582,   24.87724857884445,
    46.783561706542976, 24.879896013383163,
    46.78184509277344,  24.882387664656264,
    46.780643463134766, 24.883010569622254,
    46.77995681762696,  24.88565788068486,
    46.77206039428711,  24.891419479211137,
    46.76691055297852,  24.892665194900072,
    46.75695419311524,  24.892665194900072,
    46.751461029052734, 24.89437803345151,
    46.7464828491211,   24.893755185818428,
    46.74081802368164,  24.891263763866398,
    46.727943420410156, 24.885346435269415,
    46.725883483886726, 24.88332202092735,
    46.7245101928711,   24.879896013383163,
    46.72931671142578,  24.87226501877319,
    46.73154830932618,  24.866658273481924,
    46.7325782775879,   24.862608799282842,
    46.73360824584962,  24.861830039038882,
    46.73309326171876,  24.860428258239736,
    46.73377990722657,  24.85295182561484,
    46.732406616210945, 24.846876891350952,
    46.72914505004883,  24.838153387193202,
    46.723651885986335, 24.831142982954567  # Closing the polygon
  ), ncol = 2, byrow = TRUE)
  
  # 2. Build the polygon
  sedrah_polygon <- st_polygon(list(coords_sedrah)) %>%
    st_sfc(crs = 4326)  # Assuming WGS84
  
  # Check validity and fix if needed
  if (!st_is_valid(sedrah_polygon)) {
    sedrah_polygon <- st_make_valid(sedrah_polygon)
  }
  
  # 3. Create sf object for Sedrah District
  sedrah_sf <- st_sf(
    district_id = NA,
    city_id = 3,
    region_id = NA,
    name_ar = "سدرة",
    name_en = "Sedrah",
    geometry = sedrah_polygon
  )
  
  # ✅ If you're merging with the Riyadh dataset:
  sedrah_sf <- sedrah_sf[, names(riyadh_with_bashaer)]
  
  # 4. Combine with Riyadh dataset
  riyadh_with_sedrah <- rbind(riyadh_with_bashaer, sedrah_sf)
  
  # ✅ Define the districts to merge
  districts_to_merge <- c("Al Wahah Dist.", "Salahuddin Dist.")
  new_name_en <- "King Salman Dist."
  new_name_ar <- "حي الملك سلمان"
  
  # ✅ Apply the merge
  riyadh_merged <- riyadh_with_sedrah %>%
    mutate(
      name_en = ifelse(name_en %in% districts_to_merge, new_name_en, name_en),
      name_ar = ifelse(name_en == new_name_en, new_name_ar, name_ar)
    ) %>%
    group_by(name_en, name_ar) %>%
    summarise(geometry = st_union(geometry), .groups = "drop")
  
  # Ensuring that coords are valid
  riyadh_merged <- st_make_valid(riyadh_merged)
  
  # Sorting dataset by English name
  riyadh_merged <- riyadh_merged %>%
    arrange(name_en)
  
  # Remove Dist. and حي from names
  riyadh_merged <- riyadh_merged %>%
    mutate(
      name_en = str_remove(name_en, " Dist\\.$"),  # Remove 'Dist.' at end
      name_ar = str_remove(name_ar, "^حي\\s")      # Remove 'حي ' at start
    )
  
  # Getting official names of districts in Arabic and English
  names_dist <- read_excel("2025_06_22_List_of_Riyadh_dsitricts_stripped_of_population.xlsx", sheet = "Clean") %>%
    clean_names() %>% 
    select(district_name_in_arabic,	district_name_in_english,	name_en, region, new_region) %>% 
    arrange(name_en)
  
  # Merging riyadh_merged and names_dist by name_en
  riyadh_merged_2 <- riyadh_merged %>%
    left_join(names_dist, by = "name_en") %>% 
    select(district_name_in_english, district_name_in_arabic, region, new_region,
           geometry) %>% 
    arrange(district_name_in_english)
  
  # Determining range
  health_buffered <- st_buffer(gisdata, dist = 1000)  # in meters
  
  # Controlling the order of region categories
  riyadh_merged_2 <- riyadh_merged_2 %>%
    mutate(region = factor(region, levels = c("North", "East", "Center", "West", "South"))) %>% 
    mutate(new_region = factor(new_region, levels = c("North", "East", "Center", "West", "South")))
  
  # Getting district labels in English and wrap them as text
  district_labels <- riyadh_merged_2 %>%
    st_point_on_surface() %>%                # always inside the polygon
    cbind(st_coordinates(.)) %>%
    mutate(
      wrapped_name = str_wrap(district_name_in_english, width = 12)  # wrap names nicely
    )
  
  # Compute safe label positions inside each district
  district_labels <- riyadh_merged_2 %>%
    st_point_on_surface() %>%                # always inside the polygon
    cbind(st_coordinates(.)) %>%
    mutate(
      wrapped_name = str_wrap(district_name_in_english, width = 12)  # wrap names nicely
    )
  
  # Modifying coardinates
  gis_points <- gisdata %>%
    st_centroid() %>%
    cbind(st_coordinates(.))
  
  # --- NEW GIS-BASED FIX FOR OVERLAPPING POLYGONS ---
  
  # 1. Isolate the two districts involved
  # (Ensure spelling matches your data exactly)
  overlapping_district_name <- "Sedrah"
  overlapped_district_name <- "King Khalid International Airport"
  
  # --- FIX: Get the original column names to ensure match after spatial operation ---
  original_cols <- names(riyadh_merged_2)
  
  sedrah_poly <- riyadh_merged_2 %>%
    filter(district_name_in_english == overlapping_district_name)
  
  airport_poly <- riyadh_merged_2 %>%
    filter(district_name_in_english == overlapped_district_name)
  
  # 2. Isolate all other districts
  other_districts <- riyadh_merged_2 %>%
    filter(district_name_in_english != overlapping_district_name &
             district_name_in_english != overlapped_district_name)
  
  # 3. Perform the "difference" operation
  # This geometrically cuts the 'Sedrah' shape out of the 'Airport' shape
  # We use st_make_valid() to prevent potential geometry errors
  airport_poly_fixed <- st_difference(st_make_valid(airport_poly), st_make_valid(sedrah_poly)) %>%
    # --- FIX: Force the dataframe to have the *exact* same columns as the others ---
    select(all_of(original_cols))
  
  # 4. Recombine all polygons into one clean 'sf' dataframe
  # This new dataframe has no overlaps and matching columns
  riyadh_merged_2 <- rbind(other_districts, airport_poly_fixed, sedrah_poly)
  
  # --- End of new GIS fix ---
}


##########################################
##########################################
# Section 4: Adding Metro and bus Stations
##########################################
##########################################

# Loading in metro stations
stations <- st_read("metro-stations-in-riyadh-by-metro-line-and-station-type-2024.geojson")

# Number of unique stations by station name
n_distinct(stations$metrostationname)

# Or to see the actual count
stations %>% 
  distinct(metrostationname) %>% 
  nrow()

# To see which stations appear multiple times (if any)
stations %>% 
  count(metrostationname, sort = TRUE) %>% 
  filter(n > 1)

# To see total rows vs unique stations
message(paste("Total rows:", nrow(stations)))
message(paste("Unique station names:", n_distinct(stations$metrostationname)))

# Ensure CRS is projected (for correct line geometries)
stations_proj <- st_transform(stations, 32638)  # UTM zone 38N (or suitable projected CRS)

# Build lines by grouping and sorting stations
metro_lines <- stations_proj %>%
  arrange(metroline, stationseq) %>%  # use your actual ordering column
  group_by(metroline) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING") %>%
  st_transform(st_crs(stations))  # back to original CRS

# Adding bus stops and lines

library(sf)
library(dplyr)
library(stringr)

# ==============================================================================
# LOAD & AUGMENT STOPS (Creating 'bus' and 'bus_proj')
# ==============================================================================
message("🌍 Loading and Preparing Stop Database...")

# 1. Load Raw Data (No Filters)
raw_file <- "bus-stops-in-riyadh-by-bus-route-direction-and-shelter-type-2024.geojson"
if(!file.exists(raw_file)) stop("❌ Raw GeoJSON file missing!")

bus_raw <- st_read(raw_file, quiet = TRUE)
# Removed: filter(busroute != "NA")

# 2. Skip Clipping (Use Full Dataset)
bus_clipped <- bus_raw

# 3. Add Manual GPS Stops (The 20 Fixed Points)
new_stops_gps <- tibble(
  stop_id = seq(90001, 90020),
  busstopname = c(
    "Ministry of environment, water, and agriculture (MEWA)", "Al-Qubbah 101", "STC B",
    "National Museum B", "King Salman Oasis", "KSU 509", "KSU 510", "KSU 511", 
    "KSU 612", "KSU 613", "King Abdullah 401", "Al Fazari Station", 
    "Al Kindi Station", "Oud Station", "National Museum G",
    "Al Kharj A", "National Museum C", "National Museum D", "STC C", "Khalid Bin Alwaleed Road B"
  ),
  lat = c(
    24.72043399621593, 24.787921981090214, 24.726138570158177, 24.64564152858629, 24.71772921110732, 
    24.72735304003568, 24.72483707105194, 24.71611324349399, 24.72250008839225, 24.72554590304823, 
    24.70992031722155, 24.68686481360116, 24.68302352101975, 24.67547723057639, 24.64541724316797,
    24.56114998160527, 24.645767926133043, 24.64588348659914, 24.72620616736883, 24.76966960103504
  ),
  lon = c(
    46.75689591126146, 46.77740231102231, 46.66686921448616, 46.71425445883377, 46.63952255108523, 
    46.63320802060602, 46.63508401015387, 46.61899646264261, 46.61566344993564, 46.61750610342756, 
    46.62853756868214, 46.62653422766738, 46.62371700661566, 46.62522047689633, 46.71526078540383,
    46.850585167868296, 46.71445144770808, 46.714649581960145, 46.66702988635819, 46.75814577687092
  )
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(st_crs(bus_clipped)) %>%
  mutate(
    busroute = NA_character_, 
    # CRITICAL: We format code as "MAN-xxxxx" to match the master sequence
    busstopcode = paste0("MAN-", stop_id) 
  ) %>%
  select(busstopname, busstopcode, geometry)

# 4. Create 'bus' (The final pool of all stops)
bus <- bind_rows(
  bus_clipped %>% select(index, busstopname, busstopcode, geometry),
  new_stops_gps
)

# 5. Create 'bus_proj' (Projected version for operations)
bus_proj <- st_transform(bus, 32638)

# ==============================================================================
# LOAD MASTER SEQUENCE (FROM RDS)
# ==============================================================================
rds_file <- "riyadh_bus_routes_by_code.rds"

if(file.exists(rds_file)) {
  message("📂 Loading Master Sequence from RDS file...")
  master_route_codes <- readRDS(rds_file)
  message("✅ Successfully loaded ", length(master_route_codes), " route definitions.")
} else {
  stop("❌ RDS file not found! Please run the 'EXPORT' script first to generate 'riyadh_bus_routes_by_code.rds'.")
}

# ==============================================================================
# FILTER UNUSED STOPS
# ==============================================================================
# 1. Flatten the route list to find every unique code actually used
used_codes_list <- unique(unlist(master_route_codes))

# 2. Filter 'bus' to keep ONLY these stops
bus <- bus %>% 
  filter(busstopcode %in% used_codes_list)

# 3. Update 'bus_proj' to match the filtered list
bus_proj <- st_transform(bus, 32638)

message("🧹 Filtered stop database.")
message("   Kept: ", nrow(bus), " active stops.")

# ==============================================================================
# BUILD 'bus_lines' (The Connected Network)
# ==============================================================================
message("🚀 Building 'bus_lines' from Master Sequence...")

# Function to construct linestrings from the code list
build_geometry <- function(r_key, code_seq, stops_db) {
  
  # Create a temporary index table to handle loops (stop A -> B -> A)
  seq_table <- tibble(
    ordering = 1:length(code_seq),
    busstopcode = code_seq
  )
  
  # Join with spatial data (using bus_proj for accuracy)
  stops_spatial <- stops_db %>% 
    filter(busstopcode %in% code_seq) %>%
    distinct(busstopcode, .keep_all = TRUE)
  
  # Combine to get Ordered Spatial Object
  route_stops_ordered <- seq_table %>%
    left_join(stops_spatial, by = "busstopcode") %>%
    arrange(ordering) %>%
    st_as_sf()
  
  if(any(st_is_empty(route_stops_ordered)) || nrow(route_stops_ordered) < 2) return(NULL)
  
  # Create Line
  route_stops_ordered %>%
    summarise(do_union = FALSE) %>%
    st_cast("LINESTRING") %>%
    mutate(busroute = r_key) # Use the key as the route ID
}

# Execute Builder
lines_list <- lapply(names(master_route_codes), function(x) {
  build_geometry(x, master_route_codes[[x]], bus_proj)
})

# Bind into final object
bus_lines <- bind_rows(lines_list) %>%
  st_transform(st_crs(bus)) # Return to original CRS as requested

# ==============================================================================
# ASSIGN ROUTE TYPES (DYNAMIC LOGIC)
# ==============================================================================
message("🏷️ Assigning Route Attributes...")

bus_lines <- bus_lines %>%
  mutate(
    # 1. Clean the route name to get the base number (e.g. "910 | 1" -> "910")
    clean_route_num = str_trim(str_split(busroute, "\\|", simplify = TRUE)[, 1]),
    
    # 2. Convert to integer for numeric comparison (suppress warnings for non-numeric)
    route_int = suppressWarnings(as.integer(clean_route_num)),
    
    # 3. Apply the logic
    bus_route_type = case_when(
      # Priority 1: Rapid Transit (Specific IDs)
      clean_route_num %in% c("11", "12", "13") ~ "Rapid Transit Route",
      
      # Priority 2: Feeder Routes (900+)
      !is.na(route_int) & route_int >= 900 ~ "Feeder Route",
      
      # Priority 3: Everything else (Community Routes)
      # This captures 7, 8, 150, 680, etc. automatically
      TRUE ~ "Community Bus Route"
    )
  ) %>%
  # Drop helper column
  select(-route_int)

# Check the breakdown to ensure it worked as expected
print(table(bus_lines$bus_route_type))

message("🎉 DONE! Objects 'bus', 'bus_proj', and 'bus_lines' are ready.")


# ==============================================================================
# REPORT: DATA LOSS & CHANGE SUMMARY
# ==============================================================================
library(dplyr)
library(stringr)

message("📊 Generating Network Change Report...")

# ------------------------------------------------------------------------------
# 1. ESTABLISH BASELINE (RAW DATA)
# ------------------------------------------------------------------------------
raw_file <- "bus-stops-in-riyadh-by-bus-route-direction-and-shelter-type-2024.geojson"
raw_data_full <- st_read(raw_file, quiet = TRUE)

# Raw Counts
raw_stops_total <- nrow(raw_data_full)
raw_routes_unique <- unique(as.character(raw_data_full$busroute))
raw_routes_unique <- raw_routes_unique[raw_routes_unique != "NA" & !is.na(raw_routes_unique)]

# ------------------------------------------------------------------------------
# 2. ESTABLISH FINAL STATUS (RECONSTRUCTED DATA)
# ------------------------------------------------------------------------------
# Final Stops (excluding the Manual 20 for "removed" calculation)
final_official_stops <- bus %>% 
  filter(!str_detect(busstopcode, "^MAN-")) %>%
  nrow()

# Manual Stops Added
manual_stops_count <- bus %>% 
  filter(str_detect(busstopcode, "^MAN-")) %>% 
  nrow()

# Final Routes (Base Numbers)
final_routes_unique <- unique(bus_lines$clean_route_num)

# ------------------------------------------------------------------------------
# 3. CALCULATE DIFFERENCES
# ------------------------------------------------------------------------------
# STOPS
stops_removed_count <- raw_stops_total - final_official_stops
stops_net_change <- (final_official_stops + manual_stops_count) - raw_stops_total

# ROUTES
routes_removed_list <- setdiff(raw_routes_unique, final_routes_unique)
routes_added_list   <- setdiff(final_routes_unique, raw_routes_unique)

# ROUTE DIRECTIONALITY (Final Network Only)
direction_stats <- tibble(route_key = names(master_route_codes)) %>%
  mutate(
    route_num = str_trim(str_split(route_key, "\\|", simplify = TRUE)[, 1])
  ) %>%
  group_by(route_num) %>%
  summarise(directions = n()) %>%
  count(directions) %>%
  mutate(type = ifelse(directions == 2, "Bidirectional (2)", "Single Direction (1)"))

# ------------------------------------------------------------------------------
# 4. STOPS BY DIRECTIONALITY
# ------------------------------------------------------------------------------
# Identify which routes are bidirectional vs single direction
route_direction_lookup <- tibble(route_key = names(master_route_codes)) %>%
  mutate(
    route_num = str_trim(str_split(route_key, "\\|", simplify = TRUE)[, 1])
  ) %>%
  group_by(route_num) %>%
  summarise(directions = n(), .groups = "drop") %>%
  mutate(dir_type = ifelse(directions == 2, "Bidirectional", "Single Direction"))

# Get all stops used in each route from master_route_codes
stops_by_route <- tibble(route_key = names(master_route_codes)) %>%
  mutate(
    route_num = str_trim(str_split(route_key, "\\|", simplify = TRUE)[, 1]),
    stops = master_route_codes[route_key]
  ) %>%
  tidyr::unnest(stops) %>%
  rename(busstopcode = stops)

# Join with direction lookup
stops_with_direction <- stops_by_route %>%
  left_join(route_direction_lookup, by = "route_num") %>%
  select(busstopcode, route_num, dir_type) %>%
  distinct()

# Count unique stops by directionality type
# A stop may serve both types - categorize by PRIMARY usage
stop_direction_summary <- stops_with_direction %>%
  group_by(busstopcode) %>%
  summarise(
    serves_bidirectional = any(dir_type == "Bidirectional"),
    serves_single = any(dir_type == "Single Direction"),
    .groups = "drop"
  ) %>%
  mutate(
    stop_category = case_when(
      serves_bidirectional & serves_single ~ "Both Types",
      serves_bidirectional ~ "Bidirectional Only",
      serves_single ~ "Single Direction Only",
      TRUE ~ "Unknown"
    )
  )

stops_direction_counts <- stop_direction_summary %>%
  count(stop_category, name = "stop_count")

# Also get exclusive counts
stops_in_bidirectional <- stops_with_direction %>%
  filter(dir_type == "Bidirectional") %>%
  distinct(busstopcode) %>%
  nrow()

stops_in_single <- stops_with_direction %>%
  filter(dir_type == "Single Direction") %>%
  distinct(busstopcode) %>%
  nrow()

# ------------------------------------------------------------------------------
# PRINT REPORT
# ------------------------------------------------------------------------------
cat("\n======================================================\n")
cat("          RIYADH BUS NETWORK RECONSTRUCTION           \n")
cat("                CHANGE LOG REPORT                     \n")
cat("======================================================\n")

cat("\n🛑 STOPS SUMMARY:\n")
cat(sprintf("  • Original Raw Stops:    %d\n", raw_stops_total))
cat(sprintf("  • Stops REMOVED:         -%d (Unused/Orphaned)\n", stops_removed_count))
cat(sprintf("  • Stops ADDED (Manual):  +%d (User GPS)\n", manual_stops_count))
cat("  ----------------------------------------\n")
cat(sprintf("  • FINAL STOP COUNT:      %d\n", nrow(bus)))

cat("\n🚌 ROUTES SUMMARY:\n")
cat(sprintf("  • Original Raw Routes:   %d\n", length(raw_routes_unique)))
cat(sprintf("  • Routes REMOVED:        -%d\n", length(routes_removed_list)))
if(length(routes_removed_list) > 0) {
  cat(sprintf("    (Dropped IDs: %s)\n", paste(routes_removed_list, collapse = ", ")))
}
cat(sprintf("  • Routes ADDED:          +%d\n", length(routes_added_list)))
cat("  ----------------------------------------\n")
cat(sprintf("  • FINAL ROUTE COUNT:     %d (Unique Route Numbers)\n", length(final_routes_unique)))

cat("\n↔️ DIRECTIONALITY BREAKDOWN (Final Network):\n")
cat("\n  Routes by Direction Type:\n")
print(direction_stats %>% select(Type = type, Count = n))

cat("\n  Stops by Route Direction Type:\n")
cat(sprintf("  • Stops serving Bidirectional routes:     %d\n", stops_in_bidirectional))
cat(sprintf("  • Stops serving Single Direction routes:  %d\n", stops_in_single))
cat("\n  Stop Assignment Detail:\n")
print(stops_direction_counts)

cat("\n======================================================\n")

# --- Interactive bus-route visualizations (mapview/saveWidget/browseURL) below: run only in
#     interactive sessions; skipped under Rscript/batch. None of their objects feed later sections,
#     and this avoids the forward reference to bus_stops_proj (created later in the travel-time stage). ---
if (interactive()) {

# ==============================================================================
# IDENTIFY INTERSECTING STOPS (HUBS)
# ==============================================================================
message("🔍 Identifying exact route intersections at physical stops...")

# 6A. Calculate connectivity per physical stop code
# We group by the unique physical ID (busstopcode) to see how many lines serve it
stop_connectivity <- stops_by_route %>%
  group_by(busstopcode) %>%
  summarise(
    route_count = n_distinct(route_num),
    route_list = paste(sort(unique(route_num)), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(route_count))

# 6B. Isolate EXACT intersections (2 or more routes)
# These are the points where a passenger can transfer without walking
intersecting_stops <- stop_connectivity %>% 
  filter(route_count >= 2)

single_route_stops <- stop_connectivity %>% 
  filter(route_count == 1)

# 6C. Categorize Hub Strength for Reporting
hub_summary <- stop_connectivity %>%
  mutate(hub_type = case_when(
    route_count >= 5 ~ "Major Hub (5+ Routes)",
    route_count >= 3 ~ "Medium Hub (3-4 Routes)",
    route_count == 2 ~ "Minor Intersection (2 Routes)",
    TRUE ~ "Standard Stop (1 Route)"
  )) %>%
  count(hub_type, name = "count")

# 6D. Spatial Join for Mapping
# Linking the connectivity stats back to the geographic coordinates
hubs_geo <- bus_stops_proj %>%
  inner_join(stop_connectivity, by = c("stop_code" = "busstopcode")) %>%
  filter(route_count >= 2) %>%
  # Keep only one record per physical stop to avoid overlapping map points
  distinct(stop_code, .keep_all = TRUE)

message(paste("✅ Identified", nrow(intersecting_stops), "exact intersecting stop locations."))

  # Check the reconstructed route mapping table
  verification_full <- stops_by_route %>%
  filter(busstopcode == "52331")
  
  print(verification_full)

  # Check the reconstructed route mapping table for Stop 52154
  verification_52154 <- stops_by_route %>%
    filter(busstopcode == "52154")
  
  print(verification_52154)
  
  
# ==============================================================================
# MASTER VISUALIZATION: Single-Direction Routes (Official Codes Version)
# ==============================================================================

library(mapview)
library(htmlwidgets)
library(dplyr)
library(sf)
library(stringr)

message("Generating Map with Denominator Sequence Labels...")

# ------------------------------------------------------------------------------
# 1. IDENTIFY STRICT SINGLE-DIRECTION ROUTES
# ------------------------------------------------------------------------------
# We analyze 'bus_lines' to see which route numbers have only 1 variant
route_dir_counts <- bus_lines %>%
  st_drop_geometry() %>%
  group_by(clean_route_num) %>%
  summarise(
    variant_count = n_distinct(busroute), 
    .groups = "drop"
  )

# Whitelist: Routes with exactly 1 variant
single_dir_whitelist <- route_dir_counts %>% 
  filter(variant_count == 1) %>% 
  pull(clean_route_num)

# Blacklist (Legacy/Special cases to exclude)
blacklisted_routes <- c("958", "955", "950", "936", "916", "972", "182")
valid_single_routes <- setdiff(single_dir_whitelist, blacklisted_routes)

message("Mapping ", length(valid_single_routes), " single-direction routes.")

# ------------------------------------------------------------------------------
# 2. FILTER GEOMETRY (LINES)
# ------------------------------------------------------------------------------
# We filter 'bus_lines' to keep only the valid single routes
all_single_edges <- bus_lines %>%
  filter(clean_route_num %in% valid_single_routes)

if(nrow(all_single_edges) == 0) warning("⚠️ No lines found for these routes.")

# ------------------------------------------------------------------------------
# 3. CALCULATE SEQUENCES (Based on Master Codes)
# ------------------------------------------------------------------------------
message("   - Calculating sequences with denominators...")

# Iterate through the Master Code List
sequence_data_list <- lapply(valid_single_routes, function(r_num) {
  
  # Find the key in master_route_codes (it might differ slightly, e.g. trimws)
  # Since single routes usually correspond to the key directly (e.g. "910"), check:
  if (!r_num %in% names(master_route_codes)) return(NULL)
  
  code_seq <- master_route_codes[[r_num]]
  
  if(length(code_seq) == 0) return(NULL)
  
  n_stops <- length(code_seq)
  is_loop <- (code_seq[1] == code_seq[n_stops])
  
  # Generate Labels: "1 / 15", "2 / 15", etc.
  seq_nums <- paste(1:n_stops, "/", n_stops)
  
  # Loop Handling (First stop is Start AND End)
  if(is_loop) {
    seq_nums[1] <- paste0("1 & ", n_stops, " / ", n_stops)
    to_keep <- rep(TRUE, n_stops)
    to_keep[n_stops] <- FALSE # Hide the duplicate last point
  } else {
    to_keep <- rep(TRUE, n_stops)
  }
  
  tibble(
    busstopcode = code_seq,
    route_num = r_num,
    sequence_display = seq_nums,
    keep_row = to_keep
  ) %>% filter(keep_row) %>% select(-keep_row)
})

raw_sequence_df <- bind_rows(sequence_data_list)

# ------------------------------------------------------------------------------
# 4. AGGREGATE LABELS PER STOP
# ------------------------------------------------------------------------------
stop_label_lookup <- raw_sequence_df %>%
  group_by(busstopcode) %>%
  summarise(
    # Create HTML Popup content
    multi_route_label = paste0(
      "<b>Route ", route_num, ":</b> Seq ", sequence_display, 
      collapse = "<br>"
    ),
    primary_route = first(route_num) # For coloring
  )

# Merge with the 'bus' spatial object to get coordinates
sequenced_stops_final <- stop_label_lookup %>%
  left_join(bus %>% select(busstopname, busstopcode, geometry), by = "busstopcode") %>%
  st_as_sf() %>%
  mutate(
    hover_label = paste0(
      "<h3>", busstopname, "</h3>",
      "<b style='color:blue;'>Code: ", busstopcode, "</b><br>",
      "<hr>",
      multi_route_label
    )
  )

# ------------------------------------------------------------------------------
# 5. GENERATE & SAVE MAP
# ------------------------------------------------------------------------------
# Optional: Load boundary if available, else skip
map_layers <- list()
if(exists("riyadh_merged_2")) {
  map_layers$boundary <- mapview(riyadh_merged_2, 
                                 layer.name = "Riyadh Boundary", 
                                 color = "gray80", alpha.regions = 0, lwd = 2)
}

map_layers$routes <- mapview(all_single_edges, 
                             zcol = "clean_route_num", lwd = 5, label = "busroute", 
                             layer.name = "Routes", legend = TRUE)

map_layers$stops <- mapview(sequenced_stops_final, 
                            zcol = "primary_route", cex = 4, 
                            label = "hover_label", 
                            layer.name = "Stops")

# Combine (Boundary + Routes + Stops)
m <- Reduce(`+`, map_layers)

file_name <- "Single_Direction_Routes_Final.html"
full_path <- file.path(getwd(), file_name)

message("Saving final map to: ", full_path)
saveWidget(m@map, file = full_path, selfcontained = TRUE)

if (file.exists(full_path)) {
  message("✅ Success! Opening map...")
  browseURL(full_path)
}

# ==============================================================================
# MASTER VISUALIZATION: Bidirectional Routes (Official Codes Version)
# ==============================================================================

library(mapview)
library(htmlwidgets)
library(dplyr)
library(sf)
library(stringr)

message("Generating Map for Bidirectional Routes (Sorted with Official Codes)...")

# ------------------------------------------------------------------------------
# 1. IDENTIFY BIDIRECTIONAL ROUTES
# ------------------------------------------------------------------------------
# We analyze 'bus_lines' to find route numbers that have >1 variant (e.g. "7 | 1" and "7 | 2")
route_dir_counts <- bus_lines %>%
  st_drop_geometry() %>%
  group_by(clean_route_num) %>%
  summarise(
    variant_count = n_distinct(busroute), 
    .groups = "drop"
  )

# Whitelist: Routes with MORE THAN 1 variant
bidir_whitelist <- route_dir_counts %>% 
  filter(variant_count > 1) %>% 
  pull(clean_route_num)

# Blacklist (As requested in your previous script)
# Added: "17", "340", "440" as per your specific update
blacklisted_routes <- c("958", "955", "950", "936", "916", "972", "17", "340", "440")

valid_bidir_routes <- setdiff(bidir_whitelist, blacklisted_routes)

message("Mapping ", length(valid_bidir_routes), " bidirectional routes.")

# ------------------------------------------------------------------------------
# 2. FILTER LINES & SORT
# ------------------------------------------------------------------------------
all_bidir_edges <- bus_lines %>%
  filter(clean_route_num %in% valid_bidir_routes)

if(nrow(all_bidir_edges) == 0) warning("⚠️ No bidirectional lines found.")

# *** SORTING LOGIC ***
# Ensure the legend is sorted numerically (1, 7, 8... not 1, 10, 11...)
unique_ids <- unique(all_bidir_edges$clean_route_num)
sorted_ids <- unique_ids[order(as.numeric(unique_ids))]

# Apply sorting to the lines object
all_bidir_edges$clean_route_num <- factor(all_bidir_edges$clean_route_num, levels = sorted_ids)

# ------------------------------------------------------------------------------
# 3. CALCULATE SEQUENCES (Based on Master Codes)
# ------------------------------------------------------------------------------
message("   - Calculating sequences for Inbound/Outbound...")

# Identify all specific keys needed (e.g., "7 | 1", "7 | 2")
needed_keys <- unique(all_bidir_edges$busroute)

sequence_data_list <- lapply(needed_keys, function(r_key) {
  
  # Check if key exists in master list
  if (!r_key %in% names(master_route_codes)) return(NULL)
  
  code_seq <- master_route_codes[[r_key]]
  
  if(length(code_seq) == 0) return(NULL)
  
  n_stops <- length(code_seq)
  
  # Generate Labels: "1 / 15", "2 / 15"
  seq_nums <- paste(1:n_stops, "/", n_stops)
  
  tibble(
    busstopcode = code_seq,
    route_key = r_key, # "7 | 1"
    clean_num = trimws(sub("\\|.*", "", r_key)), # "7"
    sequence_display = seq_nums
  )
})

raw_sequence_df <- bind_rows(sequence_data_list)

# ------------------------------------------------------------------------------
# 4. AGGREGATE LABELS & SORT STOPS
# ------------------------------------------------------------------------------
stop_label_lookup <- raw_sequence_df %>%
  group_by(busstopcode) %>%
  summarise(
    # Popup: "7 | 1: Seq 5/20 <br> 7 | 2: Seq 15/20"
    multi_route_label = paste0(
      "<b>", route_key, ":</b> Seq ", sequence_display, 
      collapse = "<br>"
    ),
    # For coloring: take the first route found
    primary_route = first(clean_num)
  )

# Merge with 'bus' spatial object (Official Codes)
sequenced_stops_bidir <- stop_label_lookup %>%
  left_join(bus %>% select(busstopname, busstopcode, geometry), by = "busstopcode") %>%
  st_as_sf() %>%
  mutate(
    hover_label = paste0(
      "<h3>", busstopname, "</h3>",
      "<b style='color:blue;'>Code: ", busstopcode, "</b><br>",
      "<hr>",
      multi_route_label
    )
  )

# Apply Sorting to Stops as well (so colors match lines)
sequenced_stops_bidir$primary_route <- factor(sequenced_stops_bidir$primary_route, levels = sorted_ids)

# ------------------------------------------------------------------------------
# 5. GENERATE & SAVE MAP
# ------------------------------------------------------------------------------
# Optional: Boundary
map_layers <- list()
if(exists("riyadh_merged_2")) {
  map_layers$boundary <- mapview(riyadh_merged_2, 
                                 layer.name = "Riyadh Boundary", 
                                 color = "gray80", alpha.regions = 0, lwd = 2)
}

# Routes
map_layers$routes <- mapview(all_bidir_edges, 
                             zcol = "clean_route_num", 
                             lwd = 4, 
                             label = "busroute", 
                             layer.name = "Routes (Sorted)", 
                             legend = TRUE)

# Stops
map_layers$stops <- mapview(sequenced_stops_bidir, 
                            zcol = "primary_route", 
                            cex = 4, 
                            label = "hover_label", 
                            layer.name = "Stops")

# Combine
m <- Reduce(`+`, map_layers)

file_name <- "Both_Direction_Routes_Sorted.html"
full_path <- file.path(getwd(), file_name)

message("Saving sorted bidirectional map to: ", full_path)
saveWidget(m@map, file = full_path, selfcontained = TRUE)

if (file.exists(full_path)) {
  message("✅ Success! Opening map...")
  browseURL(full_path)
}
}  # end interactive bus-route visualizations (skipped under Rscript/batch)


###########################################################################
###########################################################################
# Section 5: Calculating distance between dental clinics and metro stations
###########################################################################
###########################################################################
{
  # Assign id names for each metro station
  stations_2 <- stations %>% 
    select(index, geometry) %>% 
    mutate(type="Metro station")
  
  # Assign id names for each bus stop
  bus_2 <- bus %>% 
    select(index, geometry) %>% 
    mutate(type="Bus stop")
  
  # Stack the two together
  all <- rbind(stations_2, bus_2)
  
  # Pivot metro stations
  metro_tbl <- all %>%
    filter(type == "Metro station") %>%
    mutate(id = paste0("station", row_number())) %>%
    select(id, everything(), -index)   # keep geometry intact
  
  # Pivot bus stops
  bus_tbl <- all %>%
    filter(type == "Bus stop") %>%
    mutate(id = paste0("stop", row_number())) %>%
    select(id, everything(), -index)   # keep geometry intact
  
  # Stack the two together
  all2 <- rbind(metro_tbl, bus_tbl)
  
  # Converting to 1 row only
  # all2 has: id, type, geometry (sfc)
  out_geom <- all2 %>%
    # capture the geometry into a non-active list-column,
    # then drop sf-ness so we can have multiple geometry columns
    mutate(.geom = st_geometry(.)) %>%
    st_drop_geometry() %>%
    transmute(id = as.character(id),
              type,
              geom = .geom) %>%
    # add a dummy single row key so pivot_wider produces one record
    mutate(.row = 1) %>%
    pivot_wider(
      id_cols = .row,
      names_from = id,                 # or use: names_from = id, names_glue = "{type}_{id}"
      values_from = geom,
      # if any id repeats, keep the first geometry; adjust as needed
      values_fn = list(geom = \(x) x[1])
    ) %>%
    select(-.row)
  
  # Result: one-row tibble; each column is a list-column holding that ID's geometry.
  # Example: out_geom$`12345`[[1]] is the sfc geometry for id 12345
}

#######################################################
#######################################################
# Section 6: Nearest dental clinic, metro, and bus stop
#######################################################
#######################################################

{
  # --- Packages ---------------------------------------------------------------
  library(units)
  #library(purrr)
  
  # --- 0) Parameters ----------------------------------------------------------
  # Use a projected CRS in meters. For Riyadh, UTM Zone 38N is reasonable.
  target_crs <- 32638  # EPSG:32638
  
  # --- 1) Prep input layers ---------------------------------------------------
  # Assumptions:
  # - gisdata: sf (or convertible) with clinic coordinates in 'geometry'
  # - all2   : sf (or convertible) with 'id', 'type' (metro station / bus stop), and 'geometry'
  
  # Clinics
  clinics <- gisdata %>%
    st_as_sf() %>%
    mutate(
      id = dplyr::coalesce(as.character(id), as.character(row_number()))
    ) %>%
    st_transform(target_crs)
  
  # Stops (metro stations + bus stops only)
  stops <- all2 %>%
    st_as_sf() %>%
    mutate(
      stop_id = as.character(id),
      type = tolower(trimws(type))  # normalize
    ) %>%
    filter(type %in% c("metro station", "bus stop")) %>%
    select(stop_id, type, geometry) %>%
    st_transform(target_crs)
  
  # Defensive check
  stopifnot(inherits(clinics, "sf"), inherits(stops, "sf"))
  stopifnot(st_is_longlat(clinics) == FALSE, st_is_longlat(stops) == FALSE)
  
  # --- 2) Full distance matrix (in meters) -----------------------------------
  # Returns units of meters; we'll drop to numeric for tidy table.
  dist_mat <- st_distance(clinics, stops)  # n_clinics x n_stops
  
  # Tidy long table: id, stop_id, type, distance_m
  dist_long <- dist_mat %>%
    units::drop_units() %>%
    as.data.frame() %>%
    setNames(stops$stop_id) %>%
    mutate(id = clinics$id) %>%
    pivot_longer(
      cols = -id,
      names_to = "stop_id",
      values_to = "distance_m"
    ) %>%
    left_join(stops %>% st_drop_geometry(), by = "stop_id") %>%
    relocate(id, stop_id, type, distance_m)
  
  # --- 3) Nearest overall stop per clinic ------------------------------------
  nearest_any <- dist_long %>%
    group_by(id) %>%
    slice_min(distance_m, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    rename(
      nearest_any_stop_id = stop_id,
      nearest_any_type    = type,
      nearest_any_m       = distance_m
    )
  
  # --- 4) Nearest by type (metro vs bus) -------------------------------------
  nearest_metro <- dist_long %>%
    filter(type == "metro station") %>%
    group_by(id) %>%
    slice_min(distance_m, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(
      id,
      nearest_metro_stop_id = stop_id,
      nearest_metro_m       = distance_m
    )
  
  nearest_bus <- dist_long %>%
    filter(type == "bus stop") %>%
    group_by(id) %>%
    slice_min(distance_m, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(
      id,
      nearest_bus_stop_id = stop_id,
      nearest_bus_m       = distance_m
    )
  
  # --- 5) Attach nearest info back to clinics (as sf) -------------------------
  clinics_with_nearest <- clinics %>%
    st_drop_geometry() %>%
    left_join(nearest_any,  by = "id") %>%
    left_join(nearest_metro, by = "id") %>%
    left_join(nearest_bus,   by = "id") %>%
    # Optionally attach coordinates of nearest ANY stop
    left_join(
      stops %>%
        mutate(any_stop_x = st_coordinates(.)[,1],
               any_stop_y = st_coordinates(.)[,2]) %>%
        st_drop_geometry() %>%
        select(stop_id, any_stop_x, any_stop_y),
      by = c("nearest_any_stop_id" = "stop_id")
    ) %>%
    # Restore clinic geometry
    bind_cols(geometry = st_geometry(clinics)) %>%
    st_as_sf(crs = target_crs)
}

###########################################################################
###########################################################################
# Section 7: BUILD + SAVE + LOAD contracted dodgr graph (graph_c) from 
# local .osm.pbf (UPDATED: Uses Populated Districts for Boundary Coverage)
###########################################################################
###########################################################################

{
  library(sf)
  library(dplyr)
  library(dodgr)
  library(osmextract)
  library(osmdata)
  library(readxl)   # <--- Added
  library(janitor)  # <--- Added
  
  # ---- Parameters ----
  target_crs    <- 32638
  profile       <- "motorcar"  # "motorcar" or "foot"
  pbf_file      <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data/gcc-states-251211.osm.pbf"
  out_rds       <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data/graph_c_motorcar_clipped.rds"
  
  # Path to population file (Check this path matches your system)
  pop_file      <- "2025_06_22_List_of_Riyadh_dsitricts.xlsx"
  pop_sheet     <- "Sheet2"
  
  stopifnot(file.exists(pbf_file))
  
  # ---- 1) Build boundary to clip (Robust Method) ----
  message("Building robust boundary...")
  
  # A. Prepare Clinics & Stops
  clinics <- gisdata %>%
    st_as_sf() %>%
    mutate(id = dplyr::coalesce(as.character(id), as.character(row_number()))) %>%
    st_transform(target_crs)
  
  stops <- all2 %>%
    st_as_sf() %>%
    mutate(type = tolower(trimws(type))) %>%
    filter(type %in% c("metro station", "bus stop")) %>%
    select(geometry) %>%
    st_transform(target_crs)
  
  # B. Prepare Populated Districts
  # (Requires 'riyadh_merged_2' to be loaded in environment)
  # --- POP-SAFE GUARD: only build the boundary+graph (which reads population) if the
  #     supplied graph is absent. With the prebuilt graph present, population is never read. ---
  if (!file.exists(out_rds)) {
  if (!exists("riyadh_merged_2")) stop("Error: 'riyadh_merged_2' shapefile is missing. Please load it first.")

  pop_df <- readxl::read_excel(pop_file, sheet = pop_sheet) %>%
    janitor::clean_names() %>%
    dplyr::select(district_name_in_english, population) %>%
    mutate(population = as.numeric(gsub("[^0-9.]", "", as.character(population))))
  
  riyadh_populated <- riyadh_merged_2 %>%
    st_make_valid() %>%
    left_join(pop_df, by = "district_name_in_english") %>%
    filter(!is.na(population), population > 0) %>%
    st_transform(target_crs)
  
  # C. Combine Everything + Buffer
  combined_geoms <- c(
    st_geometry(clinics),
    st_geometry(stops),
    st_geometry(riyadh_populated) # <--- Critical: Include districts in boundary
  )
  
  boundary <- st_union(combined_geoms) %>%
    st_convex_hull() %>%
    st_buffer(dist = 5000) %>%   # 5 km buffer is sufficient since districts are included
    st_make_valid()
  
  boundary_ll <- st_transform(boundary, 4326)
  message("Boundary built successfully.")
  
  # ---- 2) Read OSM lines from LOCAL PBF, clipped ----
  osm_lines_ll <- oe_read(
    file_path = pbf_file,
    layer = "lines",
    boundary = boundary_ll,
    boundary_type = "spat",
    quiet = TRUE
  )
  
  # Ensure highway is present
  if (!("highway" %in% names(osm_lines_ll)) && ("other_tags" %in% names(osm_lines_ll))) {
    osm_lines_ll$highway <- osmdata::hstore_get_value(osm_lines_ll$other_tags, "highway")
  }
  
  # ---- 3) Clean roads (KEEP IN EPSG:4326 for dodgr) ----
  roads_ll <- osm_lines_ll %>%
    st_make_valid() %>%
    filter(!is.na(highway)) %>%
    mutate(
      highway = tolower(trimws(as.character(highway))),
      highway = sub(";.*$", "", highway)
    ) %>%
    filter(!st_is_empty(.)) %>%
    filter(st_geometry_type(.) %in% c("LINESTRING", "MULTILINESTRING")) %>%
    st_cast("LINESTRING", warn = FALSE) %>%
    filter(!st_is_empty(.))
  
  # Keep routable highway classes
  if (profile == "motorcar") {
    roads_ll <- roads_ll %>%
      filter(highway %in% c(
        "motorway","motorway_link",
        "trunk","trunk_link",
        "primary","primary_link",
        "secondary","secondary_link",
        "tertiary","tertiary_link",
        "unclassified","residential",
        "living_street","service"
      ))
  } else { # foot
    roads_ll <- roads_ll %>%
      filter(highway %in% c(
        "motorway","motorway_link",
        "trunk","trunk_link",
        "primary","primary_link",
        "secondary","secondary_link",
        "tertiary","tertiary_link",
        "unclassified","residential",
        "living_street","service",
        "footway","path","pedestrian","steps","track"
      ))
  }
  
  cat("\nroads_ll n =", nrow(roads_ll), "\n")
  if (nrow(roads_ll) == 0) stop("roads_ll is empty after filtering routable highways.")
  
  # ---- 4) Weight + contract ----
  graph <- dodgr::weight_streetnet(roads_ll, wt_profile = profile, type_col = "highway")
  cat("\ngraph n =", nrow(graph), "\n")
  if (nrow(graph) == 0) stop("weight_streetnet produced an empty graph.")
  
  graph_c <- dodgr::dodgr_contract_graph(graph)
  
  # ---- 5) Save contracted graph ----
  saveRDS(graph_c, out_rds)
  cat("\nSaved graph_c to:\n", out_rds, "\n")
  } # end POP-SAFE guard (built only because supplied graph was absent)

  # ---- 6) Load contracted graph (sanity check) ----
  graph_c <- readRDS(out_rds)
  cat("\nLoaded graph_c from:\n", out_rds, "\n")
  cat("graph_c rows:", nrow(graph_c), "\n")
  
  # Return object in case you're running inside a chunk
  graph_c
}

#######################################################
#######################################################
# Section 8: DIRECTED, NO PENALTIES, ROAD-SHAPED ROUTES
#######################################################
#######################################################

library(sf)
library(dplyr)
library(tibble)
library(ggplot2)
library(dodgr)
library(osmextract)
library(osmdata)
library(lwgeom)
library(grid)

# ------------------------------------------------------------------------------
# 0) PARAMETERS
# ------------------------------------------------------------------------------
target_crs <- 32638
profile    <- "motorcar"

pbf_file <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data/gcc-states-251211.osm.pbf"

out_rds_c <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data/graph_c_motorcar_clipped_OPTION1_directed.rds"
out_rds_w <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data/graph_w_motorcar_clipped_OPTION1_directed.rds"
meta_rds <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data/clinics_geo_vs_network_4strata_with_meta.rds"

stopifnot(file.exists(pbf_file))
stopifnot(exists("gisdata"), exists("all2"))

# # ------------------------------------------------------------------------------
# # 1) BUILD CLIP BOUNDARY
# # ------------------------------------------------------------------------------
# clinics <- gisdata |> st_as_sf() |> st_transform(target_crs)
# stops <- all2 |> st_as_sf() |>
#   mutate(type = tolower(trimws(type))) |>
#   filter(type %in% c("bus stop","metro station")) |>
#   st_transform(target_crs)
# 
# # FIX: Use c() to combine geometries only, ignoring mismatched columns
# combined_geoms <- c(st_geometry(clinics), st_geometry(stops))
# 
# # Create the boundary from the combined geometry
# boundary <- st_union(combined_geoms) |>
#   st_convex_hull() |>
#   st_buffer(10000) |>
#   st_make_valid()
# 
# boundary_ll <- st_transform(boundary, 4326)

# ------------------------------------------------------------------------------
# 1) BUILD CLIP BOUNDARY (UPDATED: INCLUDES DISTRICTS)
# ------------------------------------------------------------------------------
# Ensure required libraries for data reading are loaded
library(readxl)
library(janitor)

clinics <- gisdata |> st_as_sf() |> st_transform(target_crs)
stops <- all2 |> st_as_sf() |>
  mutate(type = tolower(trimws(type))) |>
  filter(type %in% c("bus stop","metro station")) |>
  st_transform(target_crs)

# --- NEW: Load Populated Districts to ensure full coverage ---
# (Assumes 'riyadh_merged_2' exists in your environment)
# --- POP-SAFE GUARD: only build the boundary+directed graphs (which reads population) if the
#     supplied graphs are absent. With the prebuilt graphs present, population is never read. ---
if (!file.exists(out_rds_c) || !file.exists(out_rds_w)) {
if (!exists("riyadh_merged_2")) stop("riyadh_merged_2 is missing. Load district shapefile first.")

pop_file  <- "2025_06_22_List_of_Riyadh_dsitricts.xlsx"
pop_sheet <- "Sheet2"

pop <- readxl::read_excel(pop_file, sheet = pop_sheet) %>%
  janitor::clean_names() %>%
  dplyr::select(district_name_in_english, population) %>%
  mutate(population = as.numeric(gsub("[^0-9.]", "", as.character(population))))

riyadh_populated <- riyadh_merged_2 %>%
  st_make_valid() %>%
  left_join(pop, by = "district_name_in_english") %>%
  filter(!is.na(population), population > 0) %>%
  st_transform(target_crs)

# FIX: Combine Clinics + Stops + DISTRICTS
combined_geoms <- c(
  st_geometry(clinics), 
  st_geometry(stops), 
  st_geometry(riyadh_populated) # <--- The Critical Addition
)

# Create the boundary: Convex Hull + 5km Buffer
# (Reduced buffer from 10km to 5km since districts are now explicitly included)
boundary <- st_union(combined_geoms) |>
  st_convex_hull() |>
  st_buffer(5000) |>
  st_make_valid()

boundary_ll <- st_transform(boundary, 4326)

message("Boundary updated. Now includes full district extent + 5km buffer.")

# ------------------------------------------------------------------------------
# 2) READ + CLEAN OSM ROADS
# ------------------------------------------------------------------------------
osm_lines_ll <- oe_read(
  pbf_file,
  layer = "lines",
  boundary = boundary_ll,
  boundary_type = "spat",
  extra_tags = "oneway",  # <--- NEW: Automatically extracts 'oneway' into its own column
  quiet = TRUE
)

# osm_lines_ll <- oe_read(
#   pbf_file,
#   layer = "lines",
#   boundary = boundary_ll,
#   boundary_type = "spat",
#   quiet = TRUE
# )
# 
# osm_lines_ll$oneway <- osmdata::hstore_get_value(osm_lines_ll$other_tags, "oneway")

roads_ll <- osm_lines_ll |>
  filter(!is.na(highway)) |>
  mutate(
    highway = tolower(highway),
    highway = sub(";.*$", "", highway),
    oneway  = tolower(oneway)
  ) |>
  filter(highway %in% c(
    "motorway","motorway_link","trunk","trunk_link",
    "primary","primary_link","secondary","secondary_link",
    "tertiary","tertiary_link","residential","unclassified",
    "service","living_street","road"
  )) |>
  st_cast("LINESTRING", warn = FALSE)

cat("roads_ll n =", nrow(roads_ll), "\n")
print(table(roads_ll$oneway, useNA = "ifany"))

# ------------------------------------------------------------------------------
# 3) BUILD DIRECTED GRAPHS
# ------------------------------------------------------------------------------
graph_w <- dodgr::weight_streetnet(roads_ll, wt_profile = profile)
graph_c <- dodgr::dodgr_contract_graph(graph_w)

saveRDS(graph_w, out_rds_w)
saveRDS(graph_c, out_rds_c)
} # end POP-SAFE guard (built only because supplied directed graphs were absent)

graph_w <- readRDS(out_rds_w)
graph_c <- readRDS(out_rds_c)

cat("graph_w rows:", nrow(graph_w), "\n")
cat("graph_c rows:", nrow(graph_c), "\n")


# ------------------------------------------------------------------------------
# 4) BUILD ROADS_SF FROM graph_w (IMPORTANT for directed routing)
# ------------------------------------------------------------------------------
gw_df <- tibble::as_tibble(graph_w)

# Extract columns
roads_df <- gw_df %>%
  transmute(
    from_id  = as.character(from_id),
    to_id    = as.character(to_id),
    from_lon = as.numeric(from_lon),
    from_lat = as.numeric(from_lat),
    to_lon   = as.numeric(to_lon),
    to_lat   = as.numeric(to_lat),
    highway  = as.character(highway) # <--- KEEP THIS COLUMN
  )

# Build geometry list
geom_sfc <- mapply(
  function(x1, y1, x2, y2) {
    sf::st_linestring(matrix(c(x1, y1, x2, y2), ncol = 2, byrow = TRUE))
  },
  roads_df$from_lon, roads_df$from_lat, roads_df$to_lon, roads_df$to_lat,
  SIMPLIFY = FALSE
) |>
  sf::st_sfc(crs = 4326)

# COMBINE data + geometry into sf object correctly
roads_sf <- sf::st_sf(roads_df, geometry = geom_sfc)

# Transform and calculate lengths
roads_sf <- sf::st_transform(roads_sf, target_crs)
roads_sf$edge_len_m <- as.numeric(sf::st_length(roads_sf))


# ------------------------------------------------------------------------------
# 5) HELPER FUNCTIONS
# ------------------------------------------------------------------------------
orient_edge_geom_to_from <- function(e) {
  g  <- sf::st_geometry(e)[[1]]
  xy <- sf::st_coordinates(g)[, 1:2]
  A  <- xy[1, ]
  B  <- xy[nrow(xy), ]
  
  from_pt <- sf::st_as_sf(
    data.frame(lon = e$from_lon[1], lat = e$from_lat[1]),
    coords = c("lon", "lat"), crs = 4326
  ) |> sf::st_transform(sf::st_crs(e))
  
  F <- sf::st_coordinates(from_pt)[1, 1:2]
  
  if (sum((B - F)^2) < sum((A - F)^2)) {
    sf::st_geometry(e) <- sf::st_sfc(
      lwgeom::st_reverse(g),
      crs = sf::st_crs(e)
    )
  }
  e
}

vertex_id_on_graph <- function(pt_sf_m, graph_obj) {
  
  # point in lon/lat for match_pts_to_graph
  pt_ll <- sf::st_transform(pt_sf_m, 4326)
  xy_ll <- sf::st_coordinates(pt_ll)
  
  # match gives EDGE ROW index
  i <- dodgr::match_pts_to_graph(graph_obj, xy_ll)
  
  # candidate endpoints of that edge
  f_id <- as.character(graph_obj$from_id[i])
  t_id <- as.character(graph_obj$to_id[i])
  
  # decide which endpoint is actually closer to the point
  # do it in meters (target_crs)
  pt_m <- sf::st_transform(pt_ll, target_crs)
  pxy  <- sf::st_coordinates(pt_m)[1, 1:2]
  
  # build endpoint points (lon/lat -> target_crs)
  f_sf <- sf::st_as_sf(
    data.frame(lon = graph_obj$from_lon[i], lat = graph_obj$from_lat[i]),
    coords = c("lon", "lat"), crs = 4326
  ) |> sf::st_transform(target_crs)
  
  t_sf <- sf::st_as_sf(
    data.frame(lon = graph_obj$to_lon[i], lat = graph_obj$to_lat[i]),
    coords = c("lon", "lat"), crs = 4326
  ) |> sf::st_transform(target_crs)
  
  fxy <- sf::st_coordinates(f_sf)[1, 1:2]
  txy <- sf::st_coordinates(t_sf)[1, 1:2]
  
  df2 <- sum((pxy - fxy)^2)
  dt2 <- sum((pxy - txy)^2)
  
  if (dt2 < df2) t_id else f_id
}


route_line_between_vertices <- function(v_from, v_to) {
  
  v_from <- as.character(v_from)
  v_to   <- as.character(v_to)
  
  # --- get vertex sequence (robust) ---
  p <- try(dodgr::dodgr_paths(graph_w, from = v_from, to = v_to), silent = TRUE)
  if (inherits(p, "try-error") || is.null(p) || length(p) == 0) return(NULL)
  if (length(p[[1]]) == 0) return(NULL)
  
  v_seq <- p[[1]][[1]]
  v_seq <- as.character(v_seq)
  if (length(v_seq) < 2) return(NULL)
  
  # --- build id -> lon/lat lookup (from BOTH from/to endpoints) ---
  vtbl_from <- unique(data.frame(
    id  = as.character(graph_w$from_id),
    lon = graph_w$from_lon,
    lat = graph_w$from_lat
  ))
  
  vtbl_to <- unique(data.frame(
    id  = as.character(graph_w$to_id),
    lon = graph_w$to_lon,
    lat = graph_w$to_lat
  ))
  
  vtbl <- rbind(vtbl_from, vtbl_to)
  vtbl <- vtbl[!duplicated(vtbl$id), ]
  
  coords <- vtbl[match(v_seq, vtbl$id), c("lon","lat")]
  coords <- as.matrix(coords)
  
  if (anyNA(coords)) return(NULL)
  
  sfc <- sf::st_sfc(sf::st_linestring(coords), crs = 4326) |>
    sf::st_transform(target_crs)
  
  sf::st_sf(geometry = sfc)
}


vertex_shortest_route_dir_w <- function(from_pt_m, to_pt_m) {
  
  v_from <- vertex_id_on_graph(from_pt_m, graph_w)
  v_to   <- vertex_id_on_graph(to_pt_m,   graph_w)
  
  dm <- dodgr::dodgr_dists(graph_w, from = v_from, to = v_to)[1,1]
  
  get_v_sf <- function(v_id) {
    i_from <- match(v_id, graph_w$from_id)
    if (!is.na(i_from)) {
      lon <- graph_w$from_lon[i_from]; lat <- graph_w$from_lat[i_from]
    } else {
      i_to <- match(v_id, graph_w$to_id)
      if (is.na(i_to)) return(NULL)
      lon <- graph_w$to_lon[i_to]; lat <- graph_w$to_lat[i_to]
    }
    
    st_as_sf(data.frame(lon = lon, lat = lat),
             coords = c("lon","lat"), crs = 4326) |>
      st_transform(target_crs)
  }
  
  list(
    net_m = as.numeric(dm),
    from_v = get_v_sf(v_from),
    to_v   = get_v_sf(v_to),
    route_sf = route_line_between_vertices(v_from, v_to)
  )
}

project_point_to_edge <- function(pt, edge) {
  np <- st_nearest_points(st_geometry(pt), st_geometry(edge))
  xy <- st_coordinates(np[[1]])
  st_as_sf(data.frame(geometry = st_sfc(st_point(xy[2,1:2]),
                                        crs = st_crs(pt))))
}

edge_fraction <- function(edge, proj) {
  xy <- st_coordinates(edge)[,1:2]
  A <- xy[1,]; B <- xy[nrow(xy),]
  P <- st_coordinates(proj)[1,]
  t <- sum((P-A)*(B-A)) / sum((B-A)^2)
  max(0, min(1, t))
}

edge_segment_geom <- function(edge, a, b) {
  st_as_sf(data.frame(
    geometry = st_sfc(
      lwgeom::st_linesubstring(st_geometry(edge)[[1]], min(a,b), max(a,b)),
      crs = st_crs(edge)
    )
  ))
}

trim_route_to_point <- function(route_sf_m, pb_sf_m, snap_tol_m = 5) {
  
  if (is.null(route_sf_m)) {
    return(list(route_trim = NULL, trim_len_m = NA_real_))
  }
  
  crs_use <- sf::st_crs(route_sf_m)
  rgeom <- sf::st_geometry(route_sf_m)
  
  # Clean geometry extraction
  if (length(rgeom) == 0 || all(sf::st_is_empty(rgeom))) {
    return(list(route_trim = NULL, trim_len_m = NA_real_))
  }
  
  # Flatten to LINESTRING
  rline <- suppressWarnings(sf::st_cast(rgeom, "LINESTRING"))
  
  # If MULTILINESTRING, merge it first
  if (length(rline) > 1) {
    rline_merged <- try(sf::st_line_merge(sf::st_combine(rline)), silent = TRUE)
    if (!inherits(rline_merged, "try-error")) {
      rline <- suppressWarnings(sf::st_cast(rline_merged, "LINESTRING"))
    }
  }
  
  # Safety fallback if casting failed
  if (length(rline) == 0) {
    out <- sf::st_as_sf(route_sf_m)
    return(list(route_trim = out, trim_len_m = as.numeric(sf::st_length(out))))
  }
  r_ls <- rline[1] # Take the first linestring (route should be continuous)
  
  # 1. Snap the point to the line exactly
  np <- sf::st_nearest_points(sf::st_geometry(pb_sf_m), r_ls)
  np_xy <- sf::st_coordinates(np[[1]])
  on_xy <- np_xy[2, 1:2] # The point on the line
  on_pt <- sf::st_sfc(sf::st_point(on_xy), crs = crs_use)
  
  # 2. Attempt exact split
  sp <- try(lwgeom::st_split(r_ls, on_pt), silent = TRUE)
  parts <- suppressWarnings(sf::st_collection_extract(sp, "LINESTRING"))
  
  # 3. ROBUST FALLBACK: If split returned 1 part (failed to cut), use a tiny buffer "blade"
  if (length(parts) < 2) {
    # Create a 1mm buffer around the point to act as a cutting blade
    blade <- sf::st_buffer(on_pt, dist = 0.001) 
    sp_blade <- try(lwgeom::st_split(r_ls, blade), silent = TRUE)
    parts <- suppressWarnings(sf::st_collection_extract(sp_blade, "LINESTRING"))
  }
  
  # 4. Select the correct segment (closest to start)
  if (length(parts) >= 2) {
    r_xy <- sf::st_coordinates(r_ls)
    start_pt <- sf::st_sfc(sf::st_point(r_xy[1, 1:2]), crs = crs_use)
    
    # Calculate distance from route start to the start of each segment
    # The valid segment starts at 0 distance from the route start.
    starts <- lapply(parts, function(ls) {
      xy <- sf::st_coordinates(ls)
      sf::st_sfc(sf::st_point(xy[1, 1:2]), crs = crs_use)
    })
    
    d <- vapply(starts, function(s) as.numeric(sf::st_distance(s, start_pt))[1], numeric(1))
    
    # We want the segment that STARTS at the route origin (distance ~ 0)
    keep_idx <- which.min(d)
    
    candidate <- parts[[keep_idx]]
    
    route_trim <- sf::st_sf(geometry = sf::st_sfc(candidate, crs = crs_use))
    trim_len_m <- as.numeric(sf::st_length(route_trim))
    return(list(route_trim = route_trim, trim_len_m = trim_len_m))
  }
  
  # Fallback: Return original if all splitting attempts failed
  out <- sf::st_sf(geometry = sf::st_sfc(r_ls, crs = crs_use))
  list(route_trim = out, trim_len_m = as.numeric(sf::st_length(out)))
}

# ------------------------------------------------------------------------------
# UPDATED FUNCTION: Includes logic to trim overshoot
# ------------------------------------------------------------------------------
edge_shortest_route_between_projections_dir <- function(from_pt_m, to_pt_m, tol_on_route_m = 5) {
  
  ea <- roads_sf[sf::st_nearest_feature(from_pt_m, roads_sf), ]
  eb <- roads_sf[sf::st_nearest_feature(to_pt_m,   roads_sf), ]
  
  # orient_edge_geom_to_from MUST be defined in your script (it is in your Section 5)
  ea <- orient_edge_geom_to_from(ea)
  eb <- orient_edge_geom_to_from(eb)
  
  pa <- project_point_to_edge(from_pt_m, ea)
  pb <- project_point_to_edge(to_pt_m,   eb)
  
  fa <- edge_fraction(ea, pa)
  fb <- edge_fraction(eb, pb)
  
  a_from <- as.character(ea$from_id[1]); a_to <- as.character(ea$to_id[1])
  b_from <- as.character(eb$from_id[1]); b_to <- as.character(eb$to_id[1])
  
  len_a <- as.numeric(ea$edge_len_m[1])
  len_b <- as.numeric(eb$edge_len_m[1])
  
  # Start partial: proj -> a_to
  a_edge_part_m <- len_a * (1 - fa)
  seg_from <- edge_segment_geom(ea, fa, 1)
  
  # End partial: b_from -> proj
  b_edge_part_m <- len_b * fb
  seg_to <- edge_segment_geom(eb, 0, fb)
  
  dm <- dodgr::dodgr_dists(graph_w, from = a_to, to = b_from)[1, 1]
  net_m <- as.numeric(dm)
  
  net_route <- NULL
  
  if (is.finite(net_m)) {
    net_route <- route_line_between_vertices(a_to, b_from)
    
    # --- CHECK: If destination point (pb) is strictly ON the calculated route ---
    if (!is.null(net_route)) {
      d_pb <- as.numeric(sf::st_distance(sf::st_geometry(pb), sf::st_geometry(net_route)))
      
      # Use generous tolerance (e.g., 2m) to catch the overshoot
      if (is.finite(d_pb) && d_pb <= max(2, tol_on_route_m)) {
        
        # Call the robust trim function
        tr <- trim_route_to_point(net_route, pb, snap_tol_m = 5)
        
        # Only update if trim worked and didn't result in NA
        if (!is.na(tr$trim_len_m)) {
          net_route <- tr$route_trim
          net_m     <- tr$trim_len_m
          
          # Since we trimmed the route TO the projection point, 
          # we REMOVE the edge segment from the node back to the point.
          b_edge_part_m <- 0
          seg_to <- NULL 
        }
      }
    }
  } else {
    # If no network path found
    return(list(
      proj_from = pa, proj_to = pb,
      seg_from = seg_from, seg_to = seg_to, net_route = NULL,
      dist_edge_only_m = a_edge_part_m + b_edge_part_m,
      dist_net_m = NA_real_, dist_total_m = NA_real_,
      best_a_end = a_to, best_b_end = b_from
    ))
  }
  
  best_total <- a_edge_part_m + net_m + b_edge_part_m
  
  # Debug print
  print(tibble::tibble(
    a_end = a_to, b_end = b_from,
    a_edge_part_m = a_edge_part_m,
    net_m = net_m,
    b_edge_part_m = b_edge_part_m,
    total_m = best_total
  ))
  
  list(
    proj_from = pa,
    proj_to   = pb,
    seg_from  = seg_from,
    seg_to    = seg_to,
    net_route = net_route,
    dist_edge_only_m = a_edge_part_m + b_edge_part_m,
    dist_net_m = net_m,
    dist_total_m = best_total,
    best_a_end = a_to,
    best_b_end = b_from
  )
}


clip_roads_to_bbox <- function(roads, geoms, pad = 260) {
  stopifnot(inherits(roads, "sf"))
  crs_roads <- sf::st_crs(roads)
  
  # keep sf/sfc/sfg
  keep <- vapply(geoms, function(g) inherits(g, c("sf", "sfc", "sfg")), logical(1))
  geoms <- geoms[keep]
  if (length(geoms) == 0) stop("clip_roads_to_bbox: no valid sf/sfc/sfg geometries provided.")
  
  # helper: get a cleaned sfc in roads CRS
  as_clean_sfc <- function(g) {
    s <- if (inherits(g, "sf")) sf::st_geometry(g) else g
    if (inherits(s, "sfg")) s <- sf::st_sfc(s, crs = NA)
    
    if (length(s) == 0) return(NULL)
    if (all(sf::st_is_empty(s))) return(NULL)
    
    # drop Z/M if present (prevents c.sfc/bbox issues)
    s <- sf::st_zm(s, drop = TRUE, what = "ZM")
    
    crs_s <- sf::st_crs(s)
    if (is.na(crs_s)) {
      sf::st_crs(s) <- crs_roads
    } else if (crs_s != crs_roads) {
      s <- sf::st_transform(s, crs_roads)
    }
    
    s
  }
  
  s_list <- lapply(geoms, as_clean_sfc)
  s_list <- s_list[!vapply(s_list, is.null, logical(1))]
  if (length(s_list) == 0) stop("clip_roads_to_bbox: all geometries were empty after filtering.")
  
  # ---- bbox reduce (NO do.call(c, ...)) ----
  bbs <- lapply(s_list, sf::st_bbox)
  bb <- bbs[[1]]
  if (length(bbs) > 1) {
    for (i in 2:length(bbs)) {
      bb["xmin"] <- min(bb["xmin"], bbs[[i]]["xmin"], na.rm = TRUE)
      bb["ymin"] <- min(bb["ymin"], bbs[[i]]["ymin"], na.rm = TRUE)
      bb["xmax"] <- max(bb["xmax"], bbs[[i]]["xmax"], na.rm = TRUE)
      bb["ymax"] <- max(bb["ymax"], bbs[[i]]["ymax"], na.rm = TRUE)
    }
  }
  
  bb[c("xmin","ymin")] <- bb[c("xmin","ymin")] - pad
  bb[c("xmax","ymax")] <- bb[c("xmax","ymax")] + pad
  
  bb_sfc <- sf::st_as_sfc(bb)
  sf::st_crs(bb_sfc) <- crs_roads
  
  suppressWarnings(sf::st_intersection(roads, bb_sfc))
}


# ------------------------------------------------------------------------------
# 5b) BUILD CASE POINTS (Embedded)
# ------------------------------------------------------------------------------

# 1. Load results to find case IDs
res_fac <- readRDS(meta_rds)
comparison_all <- res_fac$results$comparison_all

# 2. Identify the IDs for Case A (net=0) and Case B (Normal)
subset_df <- comparison_all |> 
  mutate(
    stratum = tolower(stratum), 
    label   = tolower(label), 
    type    = tolower(type),
    id      = as.character(id)
  ) |> 
  filter(stratum == "ownership", label == "private", type == "bus stop")

id_caseA <- subset_df |> filter(net_km == 0) |> slice(1) |> pull(id)
id_caseB <- subset_df |> filter(net_km > 0)  |> slice(1) |> pull(id)

# 3. Helper to bundle Clinic + Nearest Stop
get_case_pts <- function(target_id) {
  # Get the specific clinic
  clin_pt <- clinics |> filter(as.character(id) == as.character(target_id))
  
  # Find nearest bus stop to this clinic (Euclidean match)
  stops_subset <- stops |> filter(type == "bus stop")
  nearest_idx <- st_nearest_feature(clin_pt, stops_subset)
  stop_pt <- stops_subset[nearest_idx, ]
  
  list(clin = clin_pt, stop = stop_pt)
}

# 4. Build the final objects
caseA_pts <- get_case_pts(id_caseA)
caseB_pts <- get_case_pts(id_caseB)

cat("CASE A Points (ID:", id_caseA, ") built successfully.\n")
cat("CASE B Points (ID:", id_caseB, ") built successfully.\n")

# ------------------------------------------------------------------------------
# 6) PLOTTING FUNCTION
# ------------------------------------------------------------------------------
plot_zoom_dir <- function(from_pt, to_pt, out_png, title_prefix, zoom_pad_m = 400) {
  
  arrow_style <- grid::arrow(type="closed", length=grid::unit(3,"mm"))
  
  from_m <- st_transform(from_pt, target_crs)
  to_m   <- st_transform(to_pt,   target_crs)
  
  re <- edge_shortest_route_between_projections_dir(from_m, to_m)
  
  message("re$dist_net_m = ", re$dist_net_m)
  message("len(re$net_route) = ",
          if (is.null(re$net_route)) "NULL" else as.numeric(sf::st_length(sf::st_geometry(re$net_route))))
  message("len(re$seg_from) = ",
          if (is.null(re$seg_from)) "NULL" else as.numeric(sf::st_length(sf::st_geometry(re$seg_from))))
  message("len(re$seg_to) = ",
          if (is.null(re$seg_to)) "NULL" else as.numeric(sf::st_length(sf::st_geometry(re$seg_to))))
  message("re$dist_total_m = ", re$dist_total_m)
  
  # "vertex" route = the network middle between the SAME endpoints used by edge method
  rv <- list(
    net_m = re$dist_net_m,
    from_v = NULL,
    to_v   = NULL,
    route_sf = route_line_between_vertices(re$best_a_end, re$best_b_end)
  )
  
  geoms <- list(
    from_m, to_m,
    rv$from_v, rv$to_v,
    re$proj_from, re$proj_to,
    rv$route_sf, re$net_route,
    re$seg_from, re$seg_to
  )
  
  roads_clip <- clip_roads_to_bbox(
    roads_sf,
    list(from_m, to_m, rv$from_v, rv$to_v, re$proj_from, re$proj_to,
         rv$route_sf, re$net_route, re$seg_from, re$seg_to),
    pad = zoom_pad_m
  )
  
  edge_parts <- list(re$seg_from, re$net_route, re$seg_to)
  edge_parts <- edge_parts[!sapply(edge_parts, is.null)]
  edge_path  <- if (length(edge_parts)) do.call(rbind, edge_parts) else NULL
  
  p <- ggplot() +
    geom_sf(data=roads_clip, color="grey40", linewidth=0.25) +
    geom_sf(data=rv$route_sf, colour="gold", linewidth=1.4,
            arrow=arrow_style, lineend="round") +
    { if (!is.null(edge_path)) geom_sf(data=edge_path, colour="orange", linewidth=1.6,
                                       arrow=arrow_style, lineend="round") } +
    geom_sf(data=from_m, color="red", size=1.3) +
    geom_sf(data=to_m, color="cyan3", size=1.3) +
    geom_sf(data=rv$from_v, shape=4, size=3) +
    geom_sf(data=rv$to_v,   shape=4, size=3) +
    geom_sf(data=re$proj_from, shape=15, size=2) +
    geom_sf(data=re$proj_to,   shape=15, size=2) +
    labs(
      title=title_prefix,
      subtitle=paste0(
        "Net(mid)=", round(rv$net_m,1),
        " m | Edge(total)=", round(re$dist_total_m,1)," m"
      )
    ) +
    coord_sf(expand=FALSE) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      # force WHITE everywhere
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      
      # also important for some devices
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key        = element_rect(fill = "white", colour = NA)
    )
  
  ggsave(out_png, p, width=12, height=8, dpi=300, bg = "white")
  p
}

# ------------------------------------------------------------------------------
# 7) RUN
# ------------------------------------------------------------------------------
plot_zoom_dir(caseA_pts$clin, caseA_pts$stop,
              "CASE_A_clinic_to_bus_OPTION1.png",
              "CASE A — Clinic → Bus")

plot_zoom_dir(caseA_pts$stop, caseA_pts$clin,
              "CASE_A_bus_to_clinic_OPTION1.png",
              "CASE A — Bus → Clinic")

plot_zoom_dir(caseB_pts$clin, caseB_pts$stop,
              "CASE_B_clinic_to_bus_OPTION1.png",
              "CASE B — Clinic → Bus")

plot_zoom_dir(caseB_pts$stop, caseB_pts$clin,
              "CASE_B_bus_to_clinic_OPTION1.png",
              "CASE B — Bus → Clinic")

# Function to extract Lat/Lon and create a Google Maps link
get_coords_info <- function(pts_list, label) {
  # Transform to WGS84 for Google Maps compatibility
  clin_wgs84 <- st_transform(pts_list$clin, 4326)
  stop_wgs84 <- st_transform(pts_list$stop, 4326)
  
  # Extract coordinates
  c_coords <- st_coordinates(clin_wgs84)
  s_coords <- st_coordinates(stop_wgs84)
  
  cat(paste0("\n--- ", label, " ---\n"))
  cat(sprintf("Clinic: %f, %f\n", c_coords[2], c_coords[1])) # Lat, Lon
  cat(sprintf("Bus Stop: %f, %f\n", s_coords[2], s_coords[1]))
  cat(sprintf("Google Maps Link: https://www.google.com/maps/dir/'%f,%f'/'%f,%f'/\n", 
              c_coords[2], c_coords[1], s_coords[2], s_coords[1]))
}

# Execute for your cases
get_coords_info(caseA_pts, "CASE A (Net=0)")
get_coords_info(caseB_pts, "CASE B (Normal)")

# ------------------------------------------------------------------------------
# 5c) FIND CASES WITH START vs END OVERSHOOT
# ------------------------------------------------------------------------------

# To find cases where overshoot is at start vs end, you need to look at
# the relationship between seg_from and seg_to distances

# Option 1: Add a function to classify overshoot location
classify_overshoot <- function(from_pt, to_pt) {
  from_m <- st_transform(from_pt, target_crs)
  to_m   <- st_transform(to_pt, target_crs)
  
  re <- edge_shortest_route_between_projections_dir(from_m, to_m)
  
  seg_from_len <- if (is.null(re$seg_from)) 0 else as.numeric(st_length(re$seg_from))
  seg_to_len   <- if (is.null(re$seg_to)) 0 else as.numeric(st_length(re$seg_to))
  
  list(
    seg_from_len = seg_from_len,
    seg_to_len = seg_to_len,
    overshoot_at = if (seg_from_len > seg_to_len) "start" else "end",
    ratio = seg_from_len / max(seg_to_len, 0.001)
  )
}

# Option 2: Sample several cases and find ones with start overshoot
find_start_overshoot_cases <- function(subset_df, clinics, stops, n_sample = 20) {
  
  filtered_df <- subset_df |> filter(net_km > 0)
  n_to_sample <- min(n_sample, nrow(filtered_df))
  candidates <- slice_sample(filtered_df, n = n_to_sample)
  
  results <- list()
  
  for (i in seq_len(nrow(candidates))) {
    target_id <- candidates$id[i]
    pts <- get_case_pts(target_id)
    
    # Check clinic -> stop direction
    info <- tryCatch(
      classify_overshoot(pts$clin, pts$stop),
      error = function(e) NULL
    )
    
    if (!is.null(info)) {
      results[[length(results) + 1]] <- list(
        id = target_id,
        direction = "clinic_to_stop",
        seg_from = info$seg_from_len,
        seg_to = info$seg_to_len,
        overshoot_at = info$overshoot_at
      )
    }
  }
  
  bind_rows(results)
}

# Run the search
overshoot_df <- find_start_overshoot_cases(subset_df, clinics, stops)

# Find cases with clear start overshoot (seg_from >> seg_to)
start_overshoot_cases <- overshoot_df |> 
  
  filter(overshoot_at == "start", seg_from > 50) |>  # at least 50m overshoot
  arrange(desc(seg_from))

cat("Found", nrow(start_overshoot_cases), "cases with start overshoot\n")

# Pick top candidates
if (nrow(start_overshoot_cases) > 0) {
  id_caseC <- start_overshoot_cases$id[1]
  caseC_pts <- get_case_pts(id_caseC)
  
  plot_zoom_dir(caseC_pts$clin, caseC_pts$stop,
                "CASE_C_start_overshoot.png",
                "CASE C — Start Overshoot")
}

get_coords_info(caseC_pts, "CASE C — Start Overshoot")



#########################################
#########################################
# Section 9: Plotting Road netwrok in Map
#########################################
#########################################

library(ggplot2)
library(sf)
library(ggnewscale)
library(ragg)
library(ggspatial)
library(dplyr)
library(tibble)
library(dodgr)


  # ---- 0a) Pre-processing ----
  riyadh_regions <- riyadh_merged_2 %>%
    sf::st_make_valid() %>%
    dplyr::group_by(new_region) %>%
    dplyr::summarise(do_union = TRUE, .groups = "drop")
  
  clinics_density_data <- gisdata %>%
    dplyr::filter(with_dental_services == "Yes") %>%
    dplyr::mutate(
      X = sf::st_coordinates(.)[,1],
      Y = sf::st_coordinates(.)[,2]
    )
  
  facet_labels <- clinics_density_data %>%
    dplyr::count(private_or_public) %>%
    dplyr::mutate(label = paste0(private_or_public, " (n = ", n, ")")) %>%
    { setNames(.$label, .$private_or_public) }
  
  center_x <- mean(clinics_density_data$X, na.rm = TRUE)
  center_y <- mean(clinics_density_data$Y, na.rm = TRUE)
  
  metro_dummy <- tibble(
    metroline = rep(c("Line1", "Line2", "Line3", "Line4", "Line5", "Line6"), each = 2),
    x_dummy = center_x,
    y_dummy = center_y
  )
  
  # ---- Roads layer (from roads / roads_ll / graph / graph_c) ----
  target_crs <- st_crs(riyadh_merged_2)

  roads_out <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data/roads_sf_riyadh_clipped_32638.rds"
  # EFFICIENCY: rebuild roads_sf (slow; uses graph + districts, no population) only if the
  # supplied clipped roads file is absent. With it present, just load it below.
  if (!file.exists(roads_out)) {
  graph_c <- readRDS("C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data/graph_c_motorcar_clipped.rds")

  roads_sf <- NULL
  if (exists("roads")) {
    roads_sf <- roads %>% st_as_sf()
  } else if (exists("roads_ll")) {
    roads_sf <- roads_ll %>% st_as_sf()
  } else if (exists("graph_c")) {
    roads_sf <- dodgr::dodgr_to_sf(graph_c) %>% st_as_sf()
  } else if (exists("graph")) {
    roads_sf <- dodgr::dodgr_to_sf(graph) %>% st_as_sf()
  } else {
    stop("No roads object found. Provide `roads`/`roads_ll` (sf) or `graph`/`graph_c` (dodgr).")
  }
  
  roads_sf <- roads_sf %>% st_transform(target_crs)
  
  # ---- Keep busy (incl residential/service) + thin minor roads slightly ----
  # We'll map highway -> class and draw linewidth by class.
  # If highway missing (e.g., from dodgr_to_sf), we still plot with a single thin linewidth.
  if ("highway" %in% names(roads_sf)) {
    
    roads_sf <- roads_sf %>%
      mutate(highway = tolower(trimws(as.character(highway)))) %>%
      mutate(highway = sub(";.*$", "", highway)) %>%
      filter(highway %in% c(
        "motorway","trunk","primary","secondary","tertiary",
        "motorway_link","trunk_link","primary_link","secondary_link","tertiary_link",
        "residential","service","unclassified","living_street"
      )) %>%
      mutate(
        road_class = case_when(
          highway %in% c("motorway","motorway_link") ~ "motorway",
          highway %in% c("trunk","trunk_link")       ~ "trunk",
          highway %in% c("primary","primary_link")   ~ "primary",
          highway %in% c("secondary","secondary_link") ~ "secondary",
          highway %in% c("tertiary","tertiary_link") ~ "tertiary",
          highway %in% c("residential","unclassified","living_street") ~ "minor",
          highway %in% c("service") ~ "service",
          TRUE ~ "minor"
        )
      )
    
  } else {
    roads_sf$road_class <- "road"
  }
  
  # ---- Clip roads to Riyadh boundary ----
  riyadh_boundary <- riyadh_merged_2 %>%
    sf::st_make_valid() %>%
    sf::st_union() %>%
    sf::st_transform(sf::st_crs(roads_sf))
  
  # prefilter -> speeds up intersection
  keep_idx <- sf::st_intersects(roads_sf, riyadh_boundary, sparse = FALSE)[,1]
  roads_sf <- roads_sf[keep_idx, , drop = FALSE]
  roads_sf <- sf::st_intersection(roads_sf, riyadh_boundary)

  # ---- Save for reuse ----
  saveRDS(roads_sf, roads_out)

  cat("Saved roads_sf to:\n", roads_out, "\n")
  } # end EFFICIENCY guard (rebuilt only because supplied roads_sf was absent)

  # load road_sf
  roads_sf <- readRDS("C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data/roads_sf_riyadh_clipped_32638.rds")
  
  # ---- 1) Build the plot ----
  p_map <- ggplot() +
    
    # --- 1a. Base Map Outlines ---
    geom_sf(data = riyadh_merged_2, fill = NA, color = "black", linewidth = 0.2) +
    
    # --- 1b. ALL Roads (Controlled Widths & Colors) ---
    geom_sf(
      data = roads_sf, 
      aes(linewidth = road_class, color = road_class), 
      show.legend = FALSE  # <--- Legend removed here
    ) +
    scale_linewidth_manual(
      values = c(
        motorway  = 0.6,
        trunk     = 0.45,
        primary   = 0.35,
        secondary = 0.25,
        tertiary  = 0.20,
        minor     = 0.15,
        service   = 0.10,
        road      = 0.10
      ),
      guide = "none"       # <--- Legend guide suppressed
    ) +
    scale_color_manual(
      values = c(
        motorway  = "grey0",
        trunk     = "grey15",
        primary   = "grey30",
        secondary = "grey45",
        tertiary  = "grey60",
        minor     = "grey75",
        service   = "grey90",
        road      = "grey90"
      ),
      guide = "none"
    ) +
    
    # --- Annotation ---
    annotation_scale(
      location = "bl", width_hint = 0.2, style = "ticks", text_cex = 1.5,
      pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")
    ) +
    annotation_north_arrow(
      location = "tl", which_north = "true",
      height = unit(1.2, "cm"), width = unit(1.2, "cm"),
      pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
      style = north_arrow_fancy_orienteering
    ) +
    
    coord_sf(expand = FALSE) +
    
    # ---- Theme ----
  theme_minimal(base_size = 18, base_family = "serif") +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border     = element_blank(),
      panel.grid       = element_blank(),
      
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      axis.title       = element_blank(),
      
      plot.title       = element_text(size = 36, face = "bold", hjust = 0.5, margin = margin(b = 20)),
      
      # --- UPDATED: Legend completely removed ---
      legend.position  = "none", 
      plot.margin      = margin(10,10,10,10)
    ) +
    labs(title = "Riyadh City Road Network")
  
  p_map
  
  # ---- Export ----
  agg_tiff(
    filename = "Riyadh_map_with_roads.tif",
    width = 24, height = 14, units = "in", res = 300, compression = "lzw"
  )
  print(p_map)
  dev.off()

###########################################################################  
###########################################################################
# Section 10: GEO vs NETWORK distances (clinic <-> nearest metro/bus)
# METHOD: ROBUST EDGE SNAPPING (2-Combo Check)
# This version explicitly checks paths to BOTH ends of the destination edge.
# This mathematically solves the "Overshoot" issue where a route might
# drive past the building to turn around.
###########################################################################
###########################################################################
  
{
  library(sf)
  library(dplyr)
  library(tidyr)
  library(units)
  library(dodgr)
  library(tibble)
  library(stringr)
  library(lwgeom)
  
  # ---- Parameters ----
  target_crs    <- 32638
  pbf_graph_rds <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data/graph_c_motorcar_clipped_OPTION1_directed.rds"
  out_dir       <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data"
  out_file      <- file.path(out_dir, "clinics_geo_vs_network_ROBUST_DIRECTED.rds")

  # EFFICIENCY: rebuild facility distances (slow directed routing; no population) only if the
  # supplied file is absent. With it present, skip the build; downstream reads out_file from disk.
  if (!file.exists(out_file)) {
  if (!file.exists(pbf_graph_rds)) stop("Graph file not found.")
  graph_c <- readRDS(pbf_graph_rds)

  # ---- 1) BUILD ROADS_SF ----
  message("Building roads_sf...")
  gw_df <- tibble::as_tibble(graph_c)
  
  roads_df <- gw_df %>%
    transmute(
      edge_id  = seq_len(n()),
      from_id  = as.character(from_id),
      to_id    = as.character(to_id),
      from_lon = as.numeric(from_lon),
      from_lat = as.numeric(from_lat),
      to_lon   = as.numeric(to_lon),
      to_lat   = as.numeric(to_lat)
    )
  
  geom_sfc <- mapply(
    function(x1, y1, x2, y2) {
      sf::st_linestring(matrix(c(x1, y1, x2, y2), ncol = 2, byrow = TRUE))
    },
    roads_df$from_lon, roads_df$from_lat, roads_df$to_lon, roads_df$to_lat,
    SIMPLIFY = FALSE
  ) |>
    sf::st_sfc(crs = 4326)
  
  roads_sf <- sf::st_sf(roads_df, geometry = geom_sfc)
  roads_sf <- sf::st_transform(roads_sf, target_crs)
  roads_sf$edge_len_m <- as.numeric(sf::st_length(roads_sf))
  
  # ---- 2) PREPARE DATA ----
  stops <- all2 %>%
    st_as_sf() %>%
    mutate(stop_id = as.character(id), type = tolower(trimws(type))) %>%
    filter(type %in% c("metro station", "bus stop")) %>%
    select(stop_id, type, geometry) %>%
    st_transform(target_crs)
  
  clinics_all <- gisdata %>%
    st_as_sf() %>%
    filter(with_dental_services == "Yes") %>%
    mutate(id = dplyr::coalesce(as.character(id), as.character(row_number()))) %>%
    st_transform(target_crs)
  
  # Columns check
  own_col <- dplyr::case_when(
    "private_or_public" %in% names(clinics_all) ~ "private_or_public",
    "privtae_or_public" %in% names(clinics_all) ~ "privtae_or_public",
    TRUE ~ NA_character_
  )
  if (is.na(own_col)) stop("Ownership column missing")
  
  clinics_all <- clinics_all %>%
    mutate(
      ownership = tolower(trimws(as.character(.data[[own_col]]))),
      region    = tolower(trimws(as.character(new_region))),
      subtype   = tolower(trimws(as.character(type_with_dental)))
    )
  
  clinics_private <- clinics_all %>% filter(ownership == "private")
  clinics_public  <- clinics_all %>% filter(ownership == "public")
  
  # ---- 3) ROBUST BATCH CALCULATION (FIXED) ----
  calc_edge_dist_batch_robust <- function(pairs_df, clinics_sub, stops_sub) {
    
    # A. Snap Clinics
    clin_geom <- clinics_sub %>% filter(id %in% pairs_df$id) %>% select(id)
    # Ensure strict order matching for mapply
    idx_c <- st_nearest_feature(clin_geom, roads_sf)
    edges_c <- roads_sf[idx_c, ]
    
    projs_c <- mapply(function(pt, line) {
      # Wrap BOTH in st_sfc with the target_crs so they compare correctly
      pt_sfc   <- st_sfc(pt, crs = target_crs)
      line_sfc <- st_sfc(line, crs = target_crs)
      
      # Get nearest points line (returns sfc of length 1)
      np <- st_nearest_points(pt_sfc, line_sfc)
      
      # Cast to points (Start=Clinic, End=Projection)
      pts <- st_cast(np, "POINT")
      
      # FIX: Use [[2]] to extract the raw 'sfg' geometry of the projection
      pts[[2]]
    }, st_geometry(clin_geom), st_geometry(edges_c), SIMPLIFY = FALSE)
    
    projs_c <- st_sfc(projs_c, crs = target_crs)
    
    # Calc fraction (fa)
    pts_A_c <- st_transform(st_as_sf(edges_c %>% select(from_lon, from_lat), coords=c("from_lon","from_lat"), crs=4326), target_crs)
    fa <- as.numeric(st_distance(pts_A_c, st_as_sf(projs_c), by_element = TRUE)) / edges_c$edge_len_m
    fa <- pmax(0, pmin(1, fa))
    
    # B. Snap Stops
    stop_geom <- stops_sub %>% filter(stop_id %in% pairs_df$nearest_stop_id_geo) %>% select(stop_id)
    idx_s <- st_nearest_feature(stop_geom, roads_sf)
    edges_s <- roads_sf[idx_s, ]
    
    projs_s <- mapply(function(pt, line) {
      pt_sfc   <- st_sfc(pt, crs = target_crs)
      line_sfc <- st_sfc(line, crs = target_crs)
      np <- st_nearest_points(pt_sfc, line_sfc)
      pts <- st_cast(np, "POINT")
      
      # FIX: Use [[2]] here as well
      pts[[2]]
    }, st_geometry(stop_geom), st_geometry(edges_s), SIMPLIFY = FALSE)
    
    projs_s <- st_sfc(projs_s, crs = target_crs)
    
    # Calc fraction (fb)
    pts_A_s <- st_transform(st_as_sf(edges_s %>% select(from_lon, from_lat), coords=c("from_lon","from_lat"), crs=4326), target_crs)
    fb <- as.numeric(st_distance(pts_A_s, st_as_sf(projs_s), by_element = TRUE)) / edges_s$edge_len_m
    fb <- pmax(0, pmin(1, fb))
    
    # C. Join Data
    res_c <- tibble(id = clin_geom$id, edge_idx_c = idx_c, c_from = edges_c$from_id, c_to = edges_c$to_id, len_c = edges_c$edge_len_m, fa = fa)
    res_s <- tibble(stop_id = stop_geom$stop_id, edge_idx_s = idx_s, s_from = edges_s$from_id, s_to = edges_s$to_id, len_s = edges_s$edge_len_m, fb = fb)
    
    calc_df <- pairs_df %>%
      left_join(res_c, by = "id") %>%
      left_join(res_s, by = c("nearest_stop_id_geo" = "stop_id"))
    
    # D. MATRIX CALCULATIONS
    
    # -- 1. Clinic -> Stop --
    u_src <- unique(calc_df$c_to)
    u_dst_from <- unique(calc_df$s_from)
    u_dst_to   <- unique(calc_df$s_to)
    
    u_src <- u_src[!is.na(u_src)]; u_dst_from <- u_dst_from[!is.na(u_dst_from)]; u_dst_to <- u_dst_to[!is.na(u_dst_to)]
    
    dm_c2s_from <- if(length(u_src) && length(u_dst_from)) dodgr_dists(graph_c, from=u_src, to=u_dst_from) else matrix(NA, nrow=length(u_src), ncol=length(u_dst_from))
    dm_c2s_to   <- if(length(u_src) && length(u_dst_to))   dodgr_dists(graph_c, from=u_src, to=u_dst_to)   else matrix(NA, nrow=length(u_src), ncol=length(u_dst_to))
    
    # Robust matrix lookup
    lookup <- function(mat, r_ids, c_ids) {
      if(length(mat) <= 1 && is.na(mat[1])) return(rep(NA, length(r_ids)))
      r_idx <- match(r_ids, rownames(mat))
      c_idx <- match(c_ids, colnames(mat))
      mat[cbind(r_idx, c_idx)]
    }
    
    val_c2s_from <- lookup(dm_c2s_from, calc_df$c_to, calc_df$s_from)
    val_c2s_to   <- lookup(dm_c2s_to,   calc_df$c_to, calc_df$s_to)
    
    d_c2s_opt1 <- (calc_df$len_c * (1 - calc_df$fa)) + val_c2s_from + (calc_df$len_s * calc_df$fb)
    d_c2s_opt2 <- (calc_df$len_c * (1 - calc_df$fa)) + val_c2s_to   + (calc_df$len_s * (1 - calc_df$fb))
    
    dist_c2s_normal <- pmin(d_c2s_opt1, d_c2s_opt2, na.rm = FALSE)
    
    # -- 2. Stop -> Clinic --
    u_src_s <- unique(calc_df$s_to)
    u_dst_c_from <- unique(calc_df$c_from)
    u_dst_c_to   <- unique(calc_df$c_to)
    
    u_src_s <- u_src_s[!is.na(u_src_s)]; u_dst_c_from <- u_dst_c_from[!is.na(u_dst_c_from)]; u_dst_c_to <- u_dst_c_to[!is.na(u_dst_c_to)]
    
    dm_s2c_from <- if(length(u_src_s) && length(u_dst_c_from)) dodgr_dists(graph_c, from=u_src_s, to=u_dst_c_from) else matrix(NA, nrow=length(u_src_s), ncol=length(u_dst_c_from))
    dm_s2c_to   <- if(length(u_src_s) && length(u_dst_c_to))   dodgr_dists(graph_c, from=u_src_s, to=u_dst_c_to)   else matrix(NA, nrow=length(u_src_s), ncol=length(u_dst_c_to))
    
    val_s2c_from <- lookup(dm_s2c_from, calc_df$s_to, calc_df$c_from)
    val_s2c_to   <- lookup(dm_s2c_to,   calc_df$s_to, calc_df$c_to)
    
    d_s2c_opt1 <- (calc_df$len_s * (1 - calc_df$fb)) + val_s2c_from + (calc_df$len_c * calc_df$fa)
    d_s2c_opt2 <- (calc_df$len_s * (1 - calc_df$fb)) + val_s2c_to   + (calc_df$len_c * (1 - calc_df$fa))
    
    dist_s2c_normal <- pmin(d_s2c_opt1, d_s2c_opt2, na.rm = FALSE)
    
    # E. Same Edge Logic
    same_edge <- (calc_df$edge_idx_c == calc_df$edge_idx_s)
    c_upstream <- (calc_df$fa <= calc_df$fb)
    
    dist_c2s <- ifelse(same_edge & c_upstream, 
                       (calc_df$fb - calc_df$fa) * calc_df$len_c, 
                       dist_c2s_normal)
    
    dist_s2c <- ifelse(same_edge & !c_upstream, 
                       (calc_df$fa - calc_df$fb) * calc_df$len_s, 
                       dist_s2c_normal)
    
    calc_df$net_km_c2s <- as.numeric(dist_c2s) / 1000
    calc_df$net_km_s2c <- as.numeric(dist_s2c) / 1000
    
    calc_df %>% select(id, nearest_stop_id_geo, net_km_c2s, net_km_s2c)
  }
  
  # ---- 4) RUNNER ----
  run_subset <- function(clinics_sf, stratum, label) {
    if (nrow(clinics_sf) == 0) return(NULL)
    
    # Geo Dist
    dist_mat <- st_distance(clinics_sf, stops)
    geo_long <- as.data.frame(units::drop_units(dist_mat)) %>%
      setNames(stops$stop_id) %>%
      mutate(id = clinics_sf$id) %>%
      pivot_longer(cols = -id, names_to = "stop_id", values_to = "geo_m") %>%
      left_join(stops %>% st_drop_geometry(), by = "stop_id") %>%
      mutate(geo_km = geo_m / 1000)
    
    nearest_geo <- geo_long %>%
      group_by(id, type) %>%
      slice_min(geo_m, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(id, type, nearest_stop_id_geo = stop_id, geo_km)
    
    rm(dist_mat, geo_long); gc()
    
    # Net Dist (Batch)
    results_list <- list()
    for(t in unique(nearest_geo$type)) {
      sub_pairs <- nearest_geo %>% filter(type == t)
      curr_clins <- clinics_sf %>% filter(id %in% sub_pairs$id)
      curr_stops <- stops %>% filter(stop_id %in% sub_pairs$nearest_stop_id_geo)
      
      dists <- calc_edge_dist_batch_robust(sub_pairs, curr_clins, curr_stops)
      results_list[[t]] <- sub_pairs %>% left_join(dists, by = c("id", "nearest_stop_id_geo"))
    }
    comp_fast <- bind_rows(results_list)
    
    # Comparisons
    comparison <- comp_fast %>%
      mutate(
        stratum = stratum,
        label   = label,
        ratio_c2s = net_km_c2s / geo_km,
        ratio_s2c = net_km_s2c / geo_km
      )
    
    scaling <- comparison %>%
      group_by(stratum, label, type) %>%
      summarise(
        n = n(),
        geo_mean = mean(geo_km, na.rm=TRUE),
        
        # C -> S
        net_c2s_mean = mean(net_km_c2s, na.rm=TRUE),
        scale_c2s = net_c2s_mean / geo_mean,
        
        # S -> C
        net_s2c_mean = mean(net_km_s2c, na.rm=TRUE),
        scale_s2c = net_s2c_mean / geo_mean,
        .groups = "drop"
      )
    
    list(comparison = comparison, scaling = scaling)
  }
  
  # ---- 5) EXECUTE ----
  # (1) Private/Public
  res_priv <- run_subset(clinics_private, "ownership", "private")
  res_pub  <- run_subset(clinics_public,  "ownership", "public")
  
  # (2) Region
  res_region <- list()
  for (r in unique(na.omit(clinics_all$region))) {
    res_region[[paste0("reg_", r)]] <- run_subset(clinics_all %>% filter(region == r), "region", r)
  }
  
  # (3) Private Subtype
  res_priv_sub <- list()
  for (s in unique(na.omit(clinics_private$subtype))) {
    res_priv_sub[[paste0("priv_", s)]] <- run_subset(clinics_private %>% filter(subtype == s), "private_subtype", s)
  }
  
  # (4) Public Subtype
  res_pub_sub <- list()
  for (s in unique(na.omit(clinics_public$subtype))) {
    res_pub_sub[[paste0("pub_", s)]] <- run_subset(clinics_public %>% filter(subtype == s), "public_subtype", s)
  }
  
  # Combine
  all_res <- c(list(res_priv, res_pub), res_region, res_priv_sub, res_pub_sub)
  comparison_all <- bind_rows(lapply(all_res, function(x) x$comparison))
  scaling_all    <- bind_rows(lapply(all_res, function(x) x$scaling))
  
  saveRDS(list(results = list(comparison_all=comparison_all, scaling_all=scaling_all)), file = out_file)

  cat("Done. Saved to:", out_file, "\n")
  print(scaling_all)
  } # end EFFICIENCY guard (rebuilt only because supplied facility-distance file was absent)
}

# ------------------------------------------------------------------------------
# DIAGNOSTIC: Check for Missing or Unreachable Routes
# ------------------------------------------------------------------------------

# 1. Filter for any row where C->S OR S->C is missing/infinite
# (Diagnostic only runs when facility distances were rebuilt this session — i.e. Section 10's
#  comparison_all with c2s/s2c columns is in scope, not the meta case-points comparison_all.)
if (exists("comparison_all") && all(c("net_km_c2s", "net_km_s2c") %in% names(comparison_all))) {
missing_routes <- comparison_all %>%
  filter(
    is.na(net_km_c2s) | is.infinite(net_km_c2s) |
      is.na(net_km_s2c) | is.infinite(net_km_s2c)
  )

# 2. Print Summary
if (nrow(missing_routes) == 0) {
  message("\n✅ SUCCESS: All clinics have valid network distances in BOTH directions.")
} else {
  message("\n⚠️  WARNING: Found ", nrow(missing_routes), " clinic-stop pairs with missing distances.")
  
  # Breakdown of the issue
  print(
    missing_routes %>%
      summarise(
        total_problem_rows = n(),
        missing_c2s = sum(is.na(net_km_c2s) | is.infinite(net_km_c2s)),
        missing_s2c = sum(is.na(net_km_s2c) | is.infinite(net_km_s2c)),
        missing_BOTH = sum(
          (is.na(net_km_c2s) | is.infinite(net_km_c2s)) & 
            (is.na(net_km_s2c) | is.infinite(net_km_s2c))
        )
      )
  )
  
  # 3. View the specific problem rows (first 10)
  message("\nFirst 10 problematic clinics:")
  print(head(missing_routes %>% select(id, type, geo_km, net_km_c2s, net_km_s2c), 10))
  
  # Optional: Save problematic IDs to investigate later
  saveRDS(missing_routes, "missing_routes_debug.rds")
}
} # end diagnostic (only runs when facility distances were rebuilt this session)

################################################################################
################################################################################
# Section 11: 10,000 UNIFORM random points (Populated Districts) -> Destinations
# METHOD: ROBUST EDGE SNAPPING (Micro-routing)
# DIRECTIONS: Both Point->Target and Target->Point
# Saves: random_points_geo_vs_network_EDGE_METHOD_populated_only.rds
################################################################################
################################################################################
  
{
  library(sf)
  library(dplyr)
  library(tidyr)
  library(units)
  library(dodgr)
  library(tibble)
  library(matrixStats)
  library(readxl)
  library(janitor)
  library(stringr)
  library(lwgeom)
  
  # --------------------------------------------------------------------------
  # PARAMETERS
  # --------------------------------------------------------------------------
  target_crs    <- 32638
  n_points      <- 10000
  seed          <- 1234
  
  pbf_graph_rds <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data/graph_c_motorcar_clipped_OPTION1_directed.rds"
  pop_file      <- "2025_06_22_List_of_Riyadh_dsitricts.xlsx"
  pop_sheet     <- "Sheet2"
  
  out_dir <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data"
  out_rds <- file.path(out_dir, "random_points_geo_vs_network_EDGE_METHOD_populated_only.rds")
  
  set.seed(seed)

  # --- POP-SAFE GUARD: rebuild (reads population to sample points) only if the supplied
  #     random-points file is absent. With it present, population is never read. ---
  if (!file.exists(out_rds)) {

  # --------------------------------------------------------------------------
  # 0) LOAD GRAPH & BUILD ROADS_SF
  # --------------------------------------------------------------------------
  stopifnot(file.exists(pbf_graph_rds))
  graph_c <- readRDS(pbf_graph_rds)
  
  message("Building roads_sf from graph...")
  gw_df <- tibble::as_tibble(graph_c)
  
  roads_df <- gw_df %>%
    transmute(
      edge_id  = seq_len(n()),
      from_id  = as.character(from_id),
      to_id    = as.character(to_id),
      from_lon = as.numeric(from_lon),
      from_lat = as.numeric(from_lat),
      to_lon   = as.numeric(to_lon),
      to_lat   = as.numeric(to_lat)
    )
  
  geom_sfc <- mapply(
    function(x1, y1, x2, y2) {
      sf::st_linestring(matrix(c(x1, y1, x2, y2), ncol = 2, byrow = TRUE))
    },
    roads_df$from_lon, roads_df$from_lat, roads_df$to_lon, roads_df$to_lat,
    SIMPLIFY = FALSE
  ) |>
    sf::st_sfc(crs = 4326)
  
  roads_sf <- sf::st_sf(roads_df, geometry = geom_sfc)
  roads_sf <- sf::st_transform(roads_sf, target_crs)
  roads_sf$edge_len_m <- as.numeric(sf::st_length(roads_sf))
  
  # --------------------------------------------------------------------------
  # 1) PREPARE POPULATED BOUNDARY & RANDOM POINTS
  # --------------------------------------------------------------------------
  stopifnot(file.exists(pop_file))
  
  pop <- readxl::read_excel(pop_file, sheet = pop_sheet) %>%
    janitor::clean_names() %>%
    dplyr::select(district_name_in_english, population) %>%
    mutate(population = as.numeric(gsub("[^0-9.]", "", as.character(population))))
  
  riyadh_merged_2_pop <- riyadh_merged_2 %>%
    st_make_valid() %>%
    left_join(pop, by = "district_name_in_english")
  
  riyadh_boundary_populated <- riyadh_merged_2_pop %>%
    filter(!is.na(population), population > 0) %>%
    st_union() %>%
    st_transform(target_crs) %>%
    st_make_valid()
  
  message("Simulating random points...")
  pts <- st_sample(riyadh_boundary_populated, size = n_points, type = "random") %>%
    st_as_sf() %>%
    st_set_crs(st_crs(riyadh_boundary_populated)) %>%
    mutate(id = paste0("pt", row_number())) # Use 'id' for consistency with helper
  
  # --------------------------------------------------------------------------
  # 2) PREPARE DESTINATIONS
  # --------------------------------------------------------------------------
  clinics_all <- gisdata %>%
    st_as_sf() %>%
    filter(with_dental_services == "Yes") %>%
    mutate(
      cid = dplyr::coalesce(as.character(id), as.character(row_number())),
      private_or_public = tolower(trimws(as.character(private_or_public)))
    ) %>%
    st_transform(target_crs)
  
  clinics_private <- clinics_all %>% filter(private_or_public == "private") %>% 
    mutate(target_id = paste0("priv_", cid)) %>% select(target_id, geometry)
  
  clinics_public <- clinics_all %>% filter(private_or_public == "public") %>% 
    mutate(target_id = paste0("pub_", cid)) %>% select(target_id, geometry)
  
  bus_sf <- bus %>% st_as_sf() %>% st_transform(target_crs) %>% 
    mutate(target_id = paste0("bus_", row_number())) %>% select(target_id, geometry)
  
  metro_sf <- stations %>% st_as_sf() %>% st_transform(target_crs) %>% 
    mutate(target_id = paste0("metro_", row_number())) %>% select(target_id, geometry)
  
  # --------------------------------------------------------------------------
  # 3) HELPER: ROBUST EDGE DISTANCE BATCH CALCULATION
  # --------------------------------------------------------------------------
  calc_edge_dist_batch_robust <- function(pairs_df, origin_sf, target_sf) {
    # pairs_df must have: id (origin), dest_id_geo (target)
    
    # A. Snap Origins (Random Points)
    # Using mapply with st_cast(..., "POINT")[[2]] to get raw geometry
    pts_geom <- origin_sf %>% filter(id %in% pairs_df$id) %>% select(id)
    idx_c <- st_nearest_feature(pts_geom, roads_sf)
    edges_c <- roads_sf[idx_c, ]
    
    projs_c <- mapply(function(pt, line) {
      pt_sfc   <- st_sfc(pt, crs = target_crs)
      line_sfc <- st_sfc(line, crs = target_crs)
      st_cast(st_nearest_points(pt_sfc, line_sfc), "POINT")[[2]]
    }, st_geometry(pts_geom), st_geometry(edges_c), SIMPLIFY = FALSE)
    projs_c <- st_sfc(projs_c, crs = target_crs)
    
    pts_A_c <- st_transform(st_as_sf(edges_c %>% select(from_lon, from_lat), coords=c("from_lon","from_lat"), crs=4326), target_crs)
    fa <- as.numeric(st_distance(pts_A_c, st_as_sf(projs_c), by_element = TRUE)) / edges_c$edge_len_m
    fa <- pmax(0, pmin(1, fa))
    
    # B. Snap Targets (Clinics/Stops)
    dest_geom <- target_sf %>% filter(target_id %in% pairs_df$dest_id_geo) %>% select(target_id)
    idx_s <- st_nearest_feature(dest_geom, roads_sf)
    edges_s <- roads_sf[idx_s, ]
    
    projs_s <- mapply(function(pt, line) {
      pt_sfc   <- st_sfc(pt, crs = target_crs)
      line_sfc <- st_sfc(line, crs = target_crs)
      st_cast(st_nearest_points(pt_sfc, line_sfc), "POINT")[[2]]
    }, st_geometry(dest_geom), st_geometry(edges_s), SIMPLIFY = FALSE)
    projs_s <- st_sfc(projs_s, crs = target_crs)
    
    pts_A_s <- st_transform(st_as_sf(edges_s %>% select(from_lon, from_lat), coords=c("from_lon","from_lat"), crs=4326), target_crs)
    fb <- as.numeric(st_distance(pts_A_s, st_as_sf(projs_s), by_element = TRUE)) / edges_s$edge_len_m
    fb <- pmax(0, pmin(1, fb))
    
    # C. Prepare Data
    res_c <- tibble(id = pts_geom$id, edge_idx_c = idx_c, c_from = edges_c$from_id, c_to = edges_c$to_id, len_c = edges_c$edge_len_m, fa = fa)
    res_s <- tibble(target_id = dest_geom$target_id, edge_idx_s = idx_s, s_from = edges_s$from_id, s_to = edges_s$to_id, len_s = edges_s$edge_len_m, fb = fb)
    
    calc_df <- pairs_df %>%
      left_join(res_c, by = "id") %>%
      left_join(res_s, by = c("dest_id_geo" = "target_id"))
    
    # D. MATRIX LOOKUPS
    lookup <- function(mat, r_ids, c_ids) {
      if(length(mat) <= 1 && is.na(mat[1])) return(rep(NA, length(r_ids)))
      mat[cbind(match(r_ids, rownames(mat)), match(c_ids, colnames(mat)))]
    }
    
    # -- 1. Point -> Target (p2t) --
    # Exit Point at c_to
    u_src <- unique(calc_df$c_to[!is.na(calc_df$c_to)])
    u_dst_from <- unique(calc_df$s_from[!is.na(calc_df$s_from)])
    u_dst_to   <- unique(calc_df$s_to[!is.na(calc_df$s_to)])
    
    dm_p2t_from <- if(length(u_src) && length(u_dst_from)) dodgr_dists(graph_c, from=u_src, to=u_dst_from) else matrix(NA)
    dm_p2t_to   <- if(length(u_src) && length(u_dst_to))   dodgr_dists(graph_c, from=u_src, to=u_dst_to)   else matrix(NA)
    
    val_p2t_from <- lookup(dm_p2t_from, calc_df$c_to, calc_df$s_from)
    val_p2t_to   <- lookup(dm_p2t_to,   calc_df$c_to, calc_df$s_to)
    
    d_p2t_opt1 <- (calc_df$len_c * (1 - calc_df$fa)) + val_p2t_from + (calc_df$len_s * calc_df$fb)
    d_p2t_opt2 <- (calc_df$len_c * (1 - calc_df$fa)) + val_p2t_to   + (calc_df$len_s * (1 - calc_df$fb))
    dist_p2t_normal <- pmin(d_p2t_opt1, d_p2t_opt2, na.rm = FALSE)
    
    # -- 2. Target -> Point (t2p) --
    # Exit Target at s_to
    u_src_s <- unique(calc_df$s_to[!is.na(calc_df$s_to)])
    u_dst_c_from <- unique(calc_df$c_from[!is.na(calc_df$c_from)])
    u_dst_c_to   <- unique(calc_df$c_to[!is.na(calc_df$c_to)])
    
    dm_t2p_from <- if(length(u_src_s) && length(u_dst_c_from)) dodgr_dists(graph_c, from=u_src_s, to=u_dst_c_from) else matrix(NA)
    dm_t2p_to   <- if(length(u_src_s) && length(u_dst_c_to))   dodgr_dists(graph_c, from=u_src_s, to=u_dst_c_to)   else matrix(NA)
    
    val_t2p_from <- lookup(dm_t2p_from, calc_df$s_to, calc_df$c_from)
    val_t2p_to   <- lookup(dm_t2p_to,   calc_df$s_to, calc_df$c_to)
    
    d_t2p_opt1 <- (calc_df$len_s * (1 - calc_df$fb)) + val_t2p_from + (calc_df$len_c * calc_df$fa)
    d_t2p_opt2 <- (calc_df$len_s * (1 - calc_df$fb)) + val_t2p_to   + (calc_df$len_c * (1 - calc_df$fa))
    dist_t2p_normal <- pmin(d_t2p_opt1, d_t2p_opt2, na.rm = FALSE)
    
    # E. Same Edge & Upstream Logic
    same_edge <- (calc_df$edge_idx_c == calc_df$edge_idx_s)
    p_upstream <- (calc_df$fa <= calc_df$fb)
    
    dist_p2t <- ifelse(same_edge & p_upstream, (calc_df$fb - calc_df$fa) * calc_df$len_c, dist_p2t_normal)
    dist_t2p <- ifelse(same_edge & !p_upstream, (calc_df$fa - calc_df$fb) * calc_df$len_s, dist_t2p_normal)
    
    calc_df$net_km_p2t <- as.numeric(dist_p2t) / 1000
    calc_df$net_km_t2p <- as.numeric(dist_t2p) / 1000
    
    calc_df %>% select(id, dest_id_geo, net_km_p2t, net_km_t2p)
  }
  
  # --------------------------------------------------------------------------
  # 4) RUN ANALYSIS
  # --------------------------------------------------------------------------
  
  # A. Identify Targets (Nearest/Median) using Geo
  get_nearest_geo <- function(pts_sf, targets_sf, type_name) {
    dm <- st_distance(pts_sf, targets_sf) %>% units::drop_units()
    idx <- max.col(-as.matrix(dm), ties.method = "first")
    tibble(id = pts_sf$id, dest_type = type_name, dest_id_geo = targets_sf$target_id[idx], 
           geo_km = as.numeric(dm[cbind(seq_len(nrow(pts_sf)), idx)])/1000)
  }
  
  get_median_geo <- function(pts_sf, targets_sf, type_name) {
    dm <- st_distance(pts_sf, targets_sf) %>% units::drop_units()
    dm_mat <- as.matrix(dm)
    row_med <- matrixStats::rowMedians(dm_mat, na.rm=TRUE)
    idx <- max.col(-abs(dm_mat - row_med), ties.method = "first")
    tibble(id = pts_sf$id, dest_type = type_name, dest_id_geo = targets_sf$target_id[idx], 
           geo_km = as.numeric(dm_mat[cbind(seq_len(nrow(pts_sf)), idx)])/1000)
  }
  
  message("Calculating Geo targets...")
  targets <- bind_rows(
    get_nearest_geo(pts, clinics_private, "nearest_priv"),
    get_nearest_geo(pts, clinics_public,  "nearest_pub"),
    get_median_geo (pts, clinics_private, "median_priv"),
    get_median_geo (pts, clinics_public,  "median_pub"),
    get_nearest_geo(pts, bus_sf,          "nearest_bus"),
    get_nearest_geo(pts, metro_sf,        "nearest_metro")
  )
  
  # B. Calculate Network Distances (Batch by Type)
  message("Calculating Edge Network distances...")
  
  results_list <- list()
  for(d_type in unique(targets$dest_type)) {
    message("  Processing: ", d_type)
    sub_pairs <- targets %>% filter(dest_type == d_type)
    
    # Identify which target SF object to use
    curr_targets <- switch(d_type,
                           "nearest_priv" = clinics_private, "median_priv" = clinics_private,
                           "nearest_pub"  = clinics_public,  "median_pub"  = clinics_public,
                           "nearest_bus"  = bus_sf,
                           "nearest_metro"= metro_sf
    )
    
    # Filter only relevant points/targets to speed up snapping
    curr_pts <- pts %>% filter(id %in% sub_pairs$id)
    curr_targets_sub <- curr_targets %>% filter(target_id %in% sub_pairs$dest_id_geo)
    
    dists <- calc_edge_dist_batch_robust(sub_pairs, curr_pts, curr_targets_sub)
    results_list[[d_type]] <- sub_pairs %>% left_join(dists, by = c("id", "dest_id_geo"))
  }
  
  comparison <- bind_rows(results_list) %>%
    mutate(
      ratio_p2t = net_km_p2t / geo_km,
      ratio_t2p = net_km_t2p / geo_km
    )
  
  scaling <- comparison %>%
    group_by(dest_type) %>%
    summarise(
      n = n(),
      mean_geo = mean(geo_km, na.rm=TRUE),
      
      # Point -> Target
      mean_net_p2t = mean(net_km_p2t, na.rm=TRUE),
      scale_p2t = mean_net_p2t / mean_geo,
      
      # Target -> Point
      mean_net_t2p = mean(net_km_t2p, na.rm=TRUE),
      scale_t2p = mean_net_t2p / mean_geo,
      .groups = "drop"
    )
  
  # --------------------------------------------------------------------------
  # 5) SAVE
  # --------------------------------------------------------------------------
  saveRDS(list(
    pts = pts,
    comparison = comparison,
    scaling = scaling,
    metadata = list(date=Sys.Date(), method="Robust Edge Snapping")
  ), file = out_rds)
  
  cat("\nSuccess. Saved to:", out_rds, "\n")
  print(scaling)
  } # end POP-SAFE guard (built only because supplied random-points file was absent)
}


# --------------------------------------------------------------------------
# 10) DIAGNOSTIC: Check for Missing Routes
# --------------------------------------------------------------------------

# Filter for rows where either direction failed
# (Diagnostic only runs when the points were rebuilt this session; skipped in pop-safe mode.)
if (exists("comparison") && exists("riyadh_boundary_populated")) {
missing_routes <- comparison %>%
  filter(
    is.na(net_km_p2t) | is.infinite(net_km_p2t) |
      is.na(net_km_t2p) | is.infinite(net_km_t2p)
  )

if (nrow(missing_routes) == 0) {
  message("\n✅ SUCCESS: All 10,000 random points have valid routes in BOTH directions.")
} else {
  message("\n⚠️  WARNING: Found ", nrow(missing_routes), " missing routes.")
  
  # Summary of failure types
  print(
    missing_routes %>%
      group_by(dest_type) %>%
      summarise(
        total_failures = n(),
        missing_p2t = sum(is.na(net_km_p2t) | is.infinite(net_km_p2t)),
        missing_t2p = sum(is.na(net_km_t2p) | is.infinite(net_km_t2p)),
        missing_BOTH = sum(
          (is.na(net_km_p2t) | is.infinite(net_km_p2t)) & 
            (is.na(net_km_t2p) | is.infinite(net_km_t2p))
        )
      )
  )
  
  message("\nFirst 10 problem rows:")
  print(head(missing_routes, 10))
  
  # Optional: Map these points to see if they are in "islands"
  library(ggplot2)
  ggplot() + 
     geom_sf(data = riyadh_boundary_populated, fill = NA, color = "grey") +
     geom_sf(data = pts %>% filter(id %in% missing_routes$pt_id), color = "red") +
     ggtitle("Locations of Failed Routes")
}
} # end diagnostic (only runs when points were rebuilt this session)

#########################################################################
#########################################################################
# Section 12: 10,000 POPULATION-WEIGHTED random points -> Destinations
# METHOD: ROBUST EDGE SNAPPING (Micro-routing)
# DIRECTIONS: Both Point->Target and Target->Point
# Saves: random_points_geo_vs_network_EDGE_METHOD_pop_weighted_NO_LCC.rds
#########################################################################
######################################################################### 

{
  library(sf)
  library(dplyr)
  library(tidyr)
  library(units)
  library(dodgr)
  library(tibble)
  library(matrixStats)
  library(readxl)
  library(janitor)
  library(stringr)
  library(lwgeom)
  
  # --------------------------------------------------------------------------
  # PARAMETERS
  # --------------------------------------------------------------------------
  target_crs    <- 32638
  n_points      <- 10000
  seed          <- 1234
  
  # Update paths if needed
  pbf_graph_rds <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data/graph_c_motorcar_clipped_OPTION1_directed.rds"
  pop_file      <- "2025_06_22_List_of_Riyadh_dsitricts.xlsx"
  pop_sheet     <- "Sheet2"
  
  out_dir <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data"
  out_rds <- file.path(out_dir, "random_points_geo_vs_network_EDGE_METHOD_pop_weighted_NO_LCC.rds")
  
  set.seed(seed)

  # --- POP-SAFE GUARD: rebuild (reads population to weight-sample points) only if the supplied
  #     random-points file is absent. With it present, population is never read. ---
  if (!file.exists(out_rds)) {

  # --------------------------------------------------------------------------
  # 0) LOAD GRAPH (NO FILTERING)
  # --------------------------------------------------------------------------
  stopifnot(file.exists(pbf_graph_rds))
  graph_c <- readRDS(pbf_graph_rds)
  
  # --------------------------------------------------------------------------
  # 1) BUILD ROADS_SF
  # --------------------------------------------------------------------------
  message("Building roads_sf from graph...")
  gw_df <- tibble::as_tibble(graph_c)
  
  roads_df <- gw_df %>%
    transmute(
      edge_id  = seq_len(n()),
      from_id  = as.character(from_id),
      to_id    = as.character(to_id),
      from_lon = as.numeric(from_lon),
      from_lat = as.numeric(from_lat),
      to_lon   = as.numeric(to_lon),
      to_lat   = as.numeric(to_lat)
    )
  
  geom_sfc <- mapply(
    function(x1, y1, x2, y2) {
      sf::st_linestring(matrix(c(x1, y1, x2, y2), ncol = 2, byrow = TRUE))
    },
    roads_df$from_lon, roads_df$from_lat, roads_df$to_lon, roads_df$to_lat,
    SIMPLIFY = FALSE
  ) |>
    sf::st_sfc(crs = 4326)
  
  roads_sf <- sf::st_sf(roads_df, geometry = geom_sfc)
  roads_sf <- sf::st_transform(roads_sf, target_crs)
  roads_sf$edge_len_m <- as.numeric(sf::st_length(roads_sf))
  
  # --------------------------------------------------------------------------
  # 2) PREPARE POPULATED BOUNDARY & RANDOM POINTS (WEIGHTED)
  # --------------------------------------------------------------------------
  stopifnot(file.exists(pop_file))
  
  pop_to_numeric <- function(x) {
    x_chr <- gsub("[^0-9]", "", as.character(x))
    suppressWarnings(as.numeric(x_chr))
  }
  
  pop <- readxl::read_excel(pop_file, sheet = pop_sheet) %>%
    janitor::clean_names() %>%
    select(district_name_in_english, population) %>%
    mutate(population = pop_to_numeric(population))
  
  districts_pop <- riyadh_merged_2 %>%
    st_make_valid() %>%
    left_join(pop, by = "district_name_in_english") %>%
    mutate(population = pop_to_numeric(population)) %>%
    filter(!is.na(population), population > 0) %>%
    st_transform(target_crs)
  
  message("Sampling population-weighted points...")
  sampled_idx <- sample(
    x       = seq_len(nrow(districts_pop)),
    size    = n_points,
    replace = TRUE,
    prob    = districts_pop$population
  )
  
  sampled_districts <- districts_pop[sampled_idx, ]
  
  pts_list <- lapply(seq_len(n_points), function(i) {
    st_sample(sampled_districts[i, ], size = 1, type = "random")
  })
  
  pts <- do.call(c, pts_list) %>%
    st_as_sf() %>%
    st_set_crs(target_crs) %>%
    mutate(id = paste0("pt", row_number()))
  
  # --------------------------------------------------------------------------
  # 3) PREPARE DESTINATIONS
  # --------------------------------------------------------------------------
  clinics_all <- gisdata %>%
    st_as_sf() %>%
    filter(with_dental_services == "Yes") %>%
    mutate(
      cid = dplyr::coalesce(as.character(id), as.character(row_number())),
      private_or_public = tolower(trimws(as.character(private_or_public)))
    ) %>%
    st_transform(target_crs)
  
  clinics_private <- clinics_all %>% filter(private_or_public == "private") %>% 
    mutate(target_id = paste0("priv_", cid)) %>% select(target_id, geometry)
  
  clinics_public <- clinics_all %>% filter(private_or_public == "public") %>% 
    mutate(target_id = paste0("pub_", cid)) %>% select(target_id, geometry)
  
  bus_sf <- bus %>% st_as_sf() %>% st_transform(target_crs) %>% 
    mutate(target_id = paste0("bus_", row_number())) %>% select(target_id, geometry)
  
  metro_sf <- stations %>% st_as_sf() %>% st_transform(target_crs) %>% 
    mutate(target_id = paste0("metro_", row_number())) %>% select(target_id, geometry)
  
  # --------------------------------------------------------------------------
  # 4) HELPER: ROBUST EDGE DISTANCE BATCH CALCULATION
  # --------------------------------------------------------------------------
  calc_edge_dist_batch_robust <- function(pairs_df, origin_sf, target_sf) {
    
    # A. Snap Origins (Random Points)
    pts_geom <- origin_sf %>% filter(id %in% pairs_df$id) %>% select(id)
    idx_c <- st_nearest_feature(pts_geom, roads_sf)
    edges_c <- roads_sf[idx_c, ]
    
    projs_c <- mapply(function(pt, line) {
      pt_sfc   <- st_sfc(pt, crs = target_crs)
      line_sfc <- st_sfc(line, crs = target_crs)
      # FIX: Use [[2]] to extract raw geometry
      st_cast(st_nearest_points(pt_sfc, line_sfc), "POINT")[[2]]
    }, st_geometry(pts_geom), st_geometry(edges_c), SIMPLIFY = FALSE)
    projs_c <- st_sfc(projs_c, crs = target_crs)
    
    pts_A_c <- st_transform(st_as_sf(edges_c %>% select(from_lon, from_lat), coords=c("from_lon","from_lat"), crs=4326), target_crs)
    fa <- as.numeric(st_distance(pts_A_c, st_as_sf(projs_c), by_element = TRUE)) / edges_c$edge_len_m
    fa <- pmax(0, pmin(1, fa))
    
    # B. Snap Targets (Clinics/Stops)
    dest_geom <- target_sf %>% filter(target_id %in% pairs_df$dest_id_geo) %>% select(target_id)
    idx_s <- st_nearest_feature(dest_geom, roads_sf)
    edges_s <- roads_sf[idx_s, ]
    
    projs_s <- mapply(function(pt, line) {
      pt_sfc   <- st_sfc(pt, crs = target_crs)
      line_sfc <- st_sfc(line, crs = target_crs)
      st_cast(st_nearest_points(pt_sfc, line_sfc), "POINT")[[2]]
    }, st_geometry(dest_geom), st_geometry(edges_s), SIMPLIFY = FALSE)
    projs_s <- st_sfc(projs_s, crs = target_crs)
    
    pts_A_s <- st_transform(st_as_sf(edges_s %>% select(from_lon, from_lat), coords=c("from_lon","from_lat"), crs=4326), target_crs)
    fb <- as.numeric(st_distance(pts_A_s, st_as_sf(projs_s), by_element = TRUE)) / edges_s$edge_len_m
    fb <- pmax(0, pmin(1, fb))
    
    # C. Prepare Data
    res_c <- tibble(id = pts_geom$id, edge_idx_c = idx_c, c_from = edges_c$from_id, c_to = edges_c$to_id, len_c = edges_c$edge_len_m, fa = fa)
    res_s <- tibble(target_id = dest_geom$target_id, edge_idx_s = idx_s, s_from = edges_s$from_id, s_to = edges_s$to_id, len_s = edges_s$edge_len_m, fb = fb)
    
    calc_df <- pairs_df %>%
      left_join(res_c, by = "id") %>%
      left_join(res_s, by = c("dest_id_geo" = "target_id"))
    
    # D. MATRIX LOOKUPS (ROBUST)
    lookup <- function(mat, r_ids, c_ids) {
      if(length(mat) <= 1 && is.na(mat[1])) return(rep(NA, length(r_ids)))
      mat[cbind(match(r_ids, rownames(mat)), match(c_ids, colnames(mat)))]
    }
    
    # -- 1. Point -> Target (p2t) --
    u_src <- unique(calc_df$c_to[!is.na(calc_df$c_to)])
    u_dst_from <- unique(calc_df$s_from[!is.na(calc_df$s_from)])
    u_dst_to   <- unique(calc_df$s_to[!is.na(calc_df$s_to)])
    
    dm_p2t_from <- if(length(u_src) && length(u_dst_from)) dodgr_dists(graph_c, from=u_src, to=u_dst_from) else matrix(NA)
    dm_p2t_to   <- if(length(u_src) && length(u_dst_to))   dodgr_dists(graph_c, from=u_src, to=u_dst_to)   else matrix(NA)
    
    val_p2t_from <- lookup(dm_p2t_from, calc_df$c_to, calc_df$s_from)
    val_p2t_to   <- lookup(dm_p2t_to,   calc_df$c_to, calc_df$s_to)
    
    d_p2t_opt1 <- (calc_df$len_c * (1 - calc_df$fa)) + val_p2t_from + (calc_df$len_s * calc_df$fb)
    d_p2t_opt2 <- (calc_df$len_c * (1 - calc_df$fa)) + val_p2t_to   + (calc_df$len_s * (1 - calc_df$fb))
    dist_p2t_normal <- pmin(d_p2t_opt1, d_p2t_opt2, na.rm = FALSE)
    
    # -- 2. Target -> Point (t2p) --
    u_src_s <- unique(calc_df$s_to[!is.na(calc_df$s_to)])
    u_dst_c_from <- unique(calc_df$c_from[!is.na(calc_df$c_from)])
    u_dst_c_to   <- unique(calc_df$c_to[!is.na(calc_df$c_to)])
    
    dm_t2p_from <- if(length(u_src_s) && length(u_dst_c_from)) dodgr_dists(graph_c, from=u_src_s, to=u_dst_c_from) else matrix(NA)
    dm_t2p_to   <- if(length(u_src_s) && length(u_dst_c_to))   dodgr_dists(graph_c, from=u_src_s, to=u_dst_c_to)   else matrix(NA)
    
    val_t2p_from <- lookup(dm_t2p_from, calc_df$s_to, calc_df$c_from)
    val_t2p_to   <- lookup(dm_t2p_to,   calc_df$s_to, calc_df$c_to)
    
    d_t2p_opt1 <- (calc_df$len_s * (1 - calc_df$fb)) + val_t2p_from + (calc_df$len_c * calc_df$fa)
    d_t2p_opt2 <- (calc_df$len_s * (1 - calc_df$fb)) + val_t2p_to   + (calc_df$len_c * (1 - calc_df$fa))
    dist_t2p_normal <- pmin(d_t2p_opt1, d_t2p_opt2, na.rm = FALSE)
    
    # E. Same Edge Logic
    same_edge <- (calc_df$edge_idx_c == calc_df$edge_idx_s)
    p_upstream <- (calc_df$fa <= calc_df$fb)
    
    dist_p2t <- ifelse(same_edge & p_upstream, (calc_df$fb - calc_df$fa) * calc_df$len_c, dist_p2t_normal)
    dist_t2p <- ifelse(same_edge & !p_upstream, (calc_df$fa - calc_df$fb) * calc_df$len_s, dist_t2p_normal)
    
    calc_df$net_km_p2t <- as.numeric(dist_p2t) / 1000
    calc_df$net_km_t2p <- as.numeric(dist_t2p) / 1000
    
    calc_df %>% select(id, dest_id_geo, net_km_p2t, net_km_t2p)
  }
  
  # --------------------------------------------------------------------------
  # 5) RUN ANALYSIS
  # --------------------------------------------------------------------------
  
  # A. Identify Targets
  get_nearest_geo <- function(pts_sf, targets_sf, type_name) {
    dm <- st_distance(pts_sf, targets_sf) %>% units::drop_units()
    idx <- max.col(-as.matrix(dm), ties.method = "first")
    tibble(id = pts_sf$id, dest_type = type_name, dest_id_geo = targets_sf$target_id[idx], 
           geo_km = as.numeric(dm[cbind(seq_len(nrow(pts_sf)), idx)])/1000)
  }
  
  get_median_geo <- function(pts_sf, targets_sf, type_name) {
    dm <- st_distance(pts_sf, targets_sf) %>% units::drop_units()
    dm_mat <- as.matrix(dm)
    row_med <- matrixStats::rowMedians(dm_mat, na.rm=TRUE)
    idx <- max.col(-abs(dm_mat - row_med), ties.method = "first")
    tibble(id = pts_sf$id, dest_type = type_name, dest_id_geo = targets_sf$target_id[idx], 
           geo_km = as.numeric(dm_mat[cbind(seq_len(nrow(pts_sf)), idx)])/1000)
  }
  
  message("Calculating Geo targets...")
  targets <- bind_rows(
    get_nearest_geo(pts, clinics_private, "nearest_priv"),
    get_nearest_geo(pts, clinics_public,  "nearest_pub"),
    get_median_geo (pts, clinics_private, "median_priv"),
    get_median_geo (pts, clinics_public,  "median_pub"),
    get_nearest_geo(pts, bus_sf,          "nearest_bus"),
    get_nearest_geo(pts, metro_sf,        "nearest_metro")
  )
  
  # B. Calculate Network Distances
  message("Calculating Edge Network distances...")
  
  results_list <- list()
  for(d_type in unique(targets$dest_type)) {
    message("  Processing: ", d_type)
    sub_pairs <- targets %>% filter(dest_type == d_type)
    
    curr_targets <- switch(d_type,
                           "nearest_priv" = clinics_private, "median_priv" = clinics_private,
                           "nearest_pub"  = clinics_public,  "median_pub"  = clinics_public,
                           "nearest_bus"  = bus_sf,
                           "nearest_metro"= metro_sf
    )
    
    curr_pts <- pts %>% filter(id %in% sub_pairs$id)
    curr_targets_sub <- curr_targets %>% filter(target_id %in% sub_pairs$dest_id_geo)
    
    dists <- calc_edge_dist_batch_robust(sub_pairs, curr_pts, curr_targets_sub)
    results_list[[d_type]] <- sub_pairs %>% left_join(dists, by = c("id", "dest_id_geo"))
  }
  
  comparison <- bind_rows(results_list) %>%
    mutate(
      ratio_p2t = net_km_p2t / geo_km,
      ratio_t2p = net_km_t2p / geo_km
    )
  
  scaling <- comparison %>%
    group_by(dest_type) %>%
    summarise(
      n = n(),
      mean_geo = mean(geo_km, na.rm=TRUE),
      mean_net_p2t = mean(net_km_p2t, na.rm=TRUE),
      scale_p2t = mean_net_p2t / mean_geo,
      mean_net_t2p = mean(net_km_t2p, na.rm=TRUE),
      scale_t2p = mean_net_t2p / mean_geo,
      .groups = "drop"
    )
  
  # --------------------------------------------------------------------------
  # 6) SAVE & CHECK
  # --------------------------------------------------------------------------
  saveRDS(list(pts=pts, comparison=comparison, scaling=scaling, 
               metadata=list(date=Sys.Date(), method="Robust Edge NO LCC")), file=out_rds)
  
  cat("\nSuccess. Saved to:", out_rds, "\n")
  print(scaling)
  
  # Diagnostic: Check for missing routes
  missing_routes <- comparison %>%
    filter(
      is.na(net_km_p2t) | is.infinite(net_km_p2t) |
        is.na(net_km_t2p) | is.infinite(net_km_t2p)
    )
  
  if (nrow(missing_routes) == 0) {
    message("\n✅ All routes valid.")
  } else {
    message("\n⚠️  WARNING: ", nrow(missing_routes), " routes failed.")
    print(head(missing_routes, 5))
  }
  } # end POP-SAFE guard (built only because supplied random-points file was absent)
}

###################################################################################
###################################################################################
# Section 13: PUBLICATION TABLE: Directional Distance Analysis
#
# Inputs:
#   1. Clinics (Robust Edge Directed)
#   2. Random Points (Populated Only + Pop Weighted) - Edge Method
#
# Outputs:
#   - Word Document with columns:
# From | To | n (Geo) | Avg Geo | n (F->T) | Avg Net F->T | n (T->F) | Avg Net T->F
###################################################################################
###################################################################################

library(dplyr)
library(stringr)
library(flextable)
library(officer)
library(tibble)

# --------------------------------------------------------------------------
# 1. FILE PATHS
# --------------------------------------------------------------------------
base_dir <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data"

# Facility File
fac_rds <- file.path(base_dir, "clinics_geo_vs_network_ROBUST_DIRECTED.rds")

# Random Point Files
rnd_designs <- tibble::tribble(
  ~design,                               ~design_order, ~file,
  "Uniform in populated districts only", 2L,            file.path(base_dir, "random_points_geo_vs_network_EDGE_METHOD_populated_only.rds"),
  "Population-weighted",                 3L,            file.path(base_dir, "random_points_geo_vs_network_EDGE_METHOD_pop_weighted_NO_LCC.rds")
)

out_docx <- file.path(base_dir, "Distance_Directional_Summary_Table_Merged.docx")

# --------------------------------------------------------------------------
# 2. LOAD & NORMALIZE DATA
# --------------------------------------------------------------------------
stopifnot(file.exists(fac_rds))

# --- Load Facilities ---
raw_fac <- readRDS(fac_rds)$results$comparison_all
dat_fac <- raw_fac %>%
  mutate(
    net_ft = net_km_c2s, # Clinic -> Stop
    net_tf = net_km_s2c  # Stop -> Clinic
  ) %>%
  select(stratum, label, type, geo_km, net_ft, net_tf)

# --- Load Random Points ---
dat_rnd_list <- lapply(seq_len(nrow(rnd_designs)), function(i) {
  fpath <- rnd_designs$file[i]
  if (!file.exists(fpath)) { warning("File not found: ", fpath); return(NULL) }
  
  raw <- readRDS(fpath)$comparison
  raw %>%
    mutate(
      stratum = "Random",
      label   = rnd_designs$design[i],
      type    = dest_type,
      design_order = rnd_designs$design_order[i],
      net_ft  = net_km_p2t, # Point -> Target
      net_tf  = net_km_t2p  # Target -> Point
    ) %>%
    select(stratum, label, type, design_order, geo_km, net_ft, net_tf)
})
dat_rnd <- bind_rows(dat_rnd_list)

# ==========================================================================
# 2b. NEW FACILITY ANCHORS FOR RANDOM POINTS  (Revision item R1-5)
# --------------------------------------------------------------------------
# Adds 4 new destination types per random-point design:
#   farthest_priv, farthest_pub  (100th-percentile / farthest facility)
#   random_priv,   random_pub    (random facility, mean of N seeded draws)
# These flow through calc_stats()/lbl_to()/final_df automatically as new rows.
#
# LOCKED METHOD (see memory cdoe-revision-analysis-plan "R1-5 TABLE 1"):
#   * Computed ON THE EXISTING sampled points ($pts in each design's .rds) so
#     the byte-exact reproduction of the existing nearest/median/bus/metro rows
#     is preserved -- the random-point files are NEVER regenerated (regen would
#     re-sample the points and break the certified manuscript reproduction).
#   * Routing methodology is lifted verbatim from the (pop-guarded, normally
#     skipped) generation blocks: same directed graph
#     (graph_c_motorcar_clipped_OPTION1_directed.rds), same roads_sf build,
#     same clinic build, same calc_edge_dist_batch_robust edge-snapping router.
#   * get_farthest_geo mirrors get_nearest_geo but picks MAX straight-line.
#   * get_random_geo draws N facilities/point uniformly from ALL of that
#     ownership; per draw -> geo + directed network distance; averaged per
#     point (ratio is thus POOLED across draws at the table-aggregate level).
#     Monte-Carlo SE of the N-draw mean is reported to confirm N is adequate.
#
# Pop-safety: reads only the existing points + facilities + road graph; never
#   touches the population table.  RNG state is saved/restored around the block
#   so all downstream seeded simulations stay byte-identical.
#
# N_RANDOM_DRAWS env: 3 = fast preview (default), 10 = final.  Result cached to
#   Data/table1_new_anchors_N<N>.rds; set FORCE_TABLE1_ANCHORS=1 to recompute.
# ==========================================================================
N_RANDOM_DRAWS <- suppressWarnings(as.integer(Sys.getenv("N_RANDOM_DRAWS", "3")))
if (is.na(N_RANDOM_DRAWS) || N_RANDOM_DRAWS < 1L) N_RANDOM_DRAWS <- 3L
RANDOM_ANCHOR_SEED <- 1234L
anchor_cache <- file.path(base_dir, sprintf("table1_new_anchors_N%d.rds", N_RANDOM_DRAWS))

if (file.exists(anchor_cache) && Sys.getenv("FORCE_TABLE1_ANCHORS") != "1") {
  .anchor_bundle <- readRDS(anchor_cache)
  cat("\n[TABLE1 R1-5] Loaded cached new anchors (N =", N_RANDOM_DRAWS, "):\n  ", anchor_cache, "\n")
} else {
  cat("\n[TABLE1 R1-5] Computing new facility anchors for random points",
      "(N_RANDOM_DRAWS =", N_RANDOM_DRAWS, ") ...\n")

  # --- Save RNG state so downstream seeded artifacts stay byte-identical ---
  .had_seed <- exists(".Random.seed", envir = .GlobalEnv)
  if (.had_seed) .saved_seed <- get(".Random.seed", envir = .GlobalEnv)

  .anchor_bundle <- local({
    suppressMessages({
      library(sf); library(dplyr); library(dodgr); library(tibble)
      library(matrixStats); library(units)
    })
    target_crs <- 32638

    # ---- Routing graph + roads_sf (IDENTICAL to the generation blocks) ----
    graph_rds <- file.path(base_dir, "graph_c_motorcar_clipped_OPTION1_directed.rds")
    stopifnot(file.exists(graph_rds))
    graph_c <- readRDS(graph_rds)

    gw_df <- tibble::as_tibble(graph_c)
    roads_df <- gw_df %>%
      transmute(
        edge_id  = seq_len(n()),
        from_id  = as.character(from_id),
        to_id    = as.character(to_id),
        from_lon = as.numeric(from_lon),
        from_lat = as.numeric(from_lat),
        to_lon   = as.numeric(to_lon),
        to_lat   = as.numeric(to_lat)
      )
    geom_sfc <- mapply(
      function(x1, y1, x2, y2) sf::st_linestring(matrix(c(x1, y1, x2, y2), ncol = 2, byrow = TRUE)),
      roads_df$from_lon, roads_df$from_lat, roads_df$to_lon, roads_df$to_lat,
      SIMPLIFY = FALSE
    ) |> sf::st_sfc(crs = 4326)
    roads_sf <- sf::st_sf(roads_df, geometry = geom_sfc)
    roads_sf <- sf::st_transform(roads_sf, target_crs)
    roads_sf$edge_len_m <- as.numeric(sf::st_length(roads_sf))

    # ---- Clinics (IDENTICAL build to the generation blocks) ----
    clinics_all <- gisdata %>%
      st_as_sf() %>%
      filter(with_dental_services == "Yes") %>%
      mutate(
        cid = dplyr::coalesce(as.character(id), as.character(row_number())),
        private_or_public = tolower(trimws(as.character(private_or_public)))
      ) %>%
      st_transform(target_crs)
    clinics_private <- clinics_all %>% filter(private_or_public == "private") %>%
      mutate(target_id = paste0("priv_", cid)) %>% select(target_id, geometry)
    clinics_public  <- clinics_all %>% filter(private_or_public == "public")  %>%
      mutate(target_id = paste0("pub_",  cid)) %>% select(target_id, geometry)
    cat("  Facilities: private =", nrow(clinics_private),
        " public =", nrow(clinics_public), "\n")

    # ---- Edge-snapping micro-router (VERBATIM from generation blocks) ----
    calc_edge_dist_batch_robust <- function(pairs_df, origin_sf, target_sf) {
      # pairs_df must have: id (origin), dest_id_geo (target)

      # A. Snap Origins
      pts_geom <- origin_sf %>% filter(id %in% pairs_df$id) %>% select(id)
      idx_c <- st_nearest_feature(pts_geom, roads_sf)
      edges_c <- roads_sf[idx_c, ]

      projs_c <- mapply(function(pt, line) {
        pt_sfc   <- st_sfc(pt, crs = target_crs)
        line_sfc <- st_sfc(line, crs = target_crs)
        st_cast(st_nearest_points(pt_sfc, line_sfc), "POINT")[[2]]
      }, st_geometry(pts_geom), st_geometry(edges_c), SIMPLIFY = FALSE)
      projs_c <- st_sfc(projs_c, crs = target_crs)

      pts_A_c <- st_transform(st_as_sf(edges_c %>% select(from_lon, from_lat), coords=c("from_lon","from_lat"), crs=4326), target_crs)
      fa <- as.numeric(st_distance(pts_A_c, st_as_sf(projs_c), by_element = TRUE)) / edges_c$edge_len_m
      fa <- pmax(0, pmin(1, fa))

      # B. Snap Targets
      dest_geom <- target_sf %>% filter(target_id %in% pairs_df$dest_id_geo) %>% select(target_id)
      idx_s <- st_nearest_feature(dest_geom, roads_sf)
      edges_s <- roads_sf[idx_s, ]

      projs_s <- mapply(function(pt, line) {
        pt_sfc   <- st_sfc(pt, crs = target_crs)
        line_sfc <- st_sfc(line, crs = target_crs)
        st_cast(st_nearest_points(pt_sfc, line_sfc), "POINT")[[2]]
      }, st_geometry(dest_geom), st_geometry(edges_s), SIMPLIFY = FALSE)
      projs_s <- st_sfc(projs_s, crs = target_crs)

      pts_A_s <- st_transform(st_as_sf(edges_s %>% select(from_lon, from_lat), coords=c("from_lon","from_lat"), crs=4326), target_crs)
      fb <- as.numeric(st_distance(pts_A_s, st_as_sf(projs_s), by_element = TRUE)) / edges_s$edge_len_m
      fb <- pmax(0, pmin(1, fb))

      # C. Prepare Data
      res_c <- tibble(id = pts_geom$id, edge_idx_c = idx_c, c_from = edges_c$from_id, c_to = edges_c$to_id, len_c = edges_c$edge_len_m, fa = fa)
      res_s <- tibble(target_id = dest_geom$target_id, edge_idx_s = idx_s, s_from = edges_s$from_id, s_to = edges_s$to_id, len_s = edges_s$edge_len_m, fb = fb)

      calc_df <- pairs_df %>%
        left_join(res_c, by = "id") %>%
        left_join(res_s, by = c("dest_id_geo" = "target_id"))

      # D. MATRIX LOOKUPS
      lookup <- function(mat, r_ids, c_ids) {
        if(length(mat) <= 1 && is.na(mat[1])) return(rep(NA, length(r_ids)))
        mat[cbind(match(r_ids, rownames(mat)), match(c_ids, colnames(mat)))]
      }

      # -- 1. Point -> Target (p2t) --
      u_src <- unique(calc_df$c_to[!is.na(calc_df$c_to)])
      u_dst_from <- unique(calc_df$s_from[!is.na(calc_df$s_from)])
      u_dst_to   <- unique(calc_df$s_to[!is.na(calc_df$s_to)])

      dm_p2t_from <- if(length(u_src) && length(u_dst_from)) dodgr_dists(graph_c, from=u_src, to=u_dst_from) else matrix(NA)
      dm_p2t_to   <- if(length(u_src) && length(u_dst_to))   dodgr_dists(graph_c, from=u_src, to=u_dst_to)   else matrix(NA)

      val_p2t_from <- lookup(dm_p2t_from, calc_df$c_to, calc_df$s_from)
      val_p2t_to   <- lookup(dm_p2t_to,   calc_df$c_to, calc_df$s_to)

      d_p2t_opt1 <- (calc_df$len_c * (1 - calc_df$fa)) + val_p2t_from + (calc_df$len_s * calc_df$fb)
      d_p2t_opt2 <- (calc_df$len_c * (1 - calc_df$fa)) + val_p2t_to   + (calc_df$len_s * (1 - calc_df$fb))
      dist_p2t_normal <- pmin(d_p2t_opt1, d_p2t_opt2, na.rm = FALSE)

      # -- 2. Target -> Point (t2p) --
      u_src_s <- unique(calc_df$s_to[!is.na(calc_df$s_to)])
      u_dst_c_from <- unique(calc_df$c_from[!is.na(calc_df$c_from)])
      u_dst_c_to   <- unique(calc_df$c_to[!is.na(calc_df$c_to)])

      dm_t2p_from <- if(length(u_src_s) && length(u_dst_c_from)) dodgr_dists(graph_c, from=u_src_s, to=u_dst_c_from) else matrix(NA)
      dm_t2p_to   <- if(length(u_src_s) && length(u_dst_c_to))   dodgr_dists(graph_c, from=u_src_s, to=u_dst_c_to)   else matrix(NA)

      val_t2p_from <- lookup(dm_t2p_from, calc_df$s_to, calc_df$c_from)
      val_t2p_to   <- lookup(dm_t2p_to,   calc_df$s_to, calc_df$c_to)

      d_t2p_opt1 <- (calc_df$len_s * (1 - calc_df$fb)) + val_t2p_from + (calc_df$len_c * calc_df$fa)
      d_t2p_opt2 <- (calc_df$len_s * (1 - calc_df$fb)) + val_t2p_to   + (calc_df$len_c * (1 - calc_df$fa))
      dist_t2p_normal <- pmin(d_t2p_opt1, d_t2p_opt2, na.rm = FALSE)

      # E. Same Edge & Upstream Logic
      same_edge <- (calc_df$edge_idx_c == calc_df$edge_idx_s)
      p_upstream <- (calc_df$fa <= calc_df$fb)

      dist_p2t <- ifelse(same_edge & p_upstream, (calc_df$fb - calc_df$fa) * calc_df$len_c, dist_p2t_normal)
      dist_t2p <- ifelse(same_edge & !p_upstream, (calc_df$fa - calc_df$fb) * calc_df$len_s, dist_t2p_normal)

      calc_df$net_km_p2t <- as.numeric(dist_p2t) / 1000
      calc_df$net_km_t2p <- as.numeric(dist_t2p) / 1000

      calc_df %>% select(id, dest_id_geo, net_km_p2t, net_km_t2p)
    }

    # ---- Geo anchor selectors ----
    # get_nearest_geo: verbatim (used only to VALIDATE the lifted router below)
    get_nearest_geo <- function(pts_sf, targets_sf, type_name) {
      dm <- st_distance(pts_sf, targets_sf) %>% units::drop_units()
      idx <- max.col(-as.matrix(dm), ties.method = "first")
      tibble(id = pts_sf$id, dest_type = type_name, dest_id_geo = targets_sf$target_id[idx],
             geo_km = as.numeric(dm[cbind(seq_len(nrow(pts_sf)), idx)])/1000)
    }
    # get_farthest_geo: mirror of get_nearest_geo but MAX straight-line (+dm).
    get_farthest_geo <- function(pts_sf, targets_sf, type_name) {
      dm <- st_distance(pts_sf, targets_sf) %>% units::drop_units()
      idx <- max.col(as.matrix(dm), ties.method = "first")
      tibble(id = pts_sf$id, dest_type = type_name, dest_id_geo = targets_sf$target_id[idx],
             geo_km = as.numeric(dm[cbind(seq_len(nrow(pts_sf)), idx)])/1000)
    }
    # get_random_geo: N independent seeded draws/point from ALL facilities of
    # that ownership; long frame (id, draw, dest_type, dest_id_geo, geo_km).
    get_random_geo <- function(pts_sf, targets_sf, type_name, n_draws, base_seed) {
      dm <- st_distance(pts_sf, targets_sf) %>% units::drop_units() %>% as.matrix()
      n_pts <- nrow(pts_sf); n_fac <- ncol(dm)
      lst <- vector("list", n_draws)
      for (d in seq_len(n_draws)) {
        set.seed(base_seed + d)
        sel <- sample.int(n_fac, n_pts, replace = TRUE)
        lst[[d]] <- tibble(
          id = pts_sf$id, draw = d, dest_type = type_name,
          dest_id_geo = targets_sf$target_id[sel],
          geo_km = dm[cbind(seq_len(n_pts), sel)] / 1000
        )
      }
      bind_rows(lst)
    }

    # ---- Attach directed network distance to a (id, dest_id_geo) frame ----
    # calc_edge_dist_batch_robust preserves input row order, so we can bind by
    # position; the stopifnot guards that invariant.
    attach_net <- function(df, pts_sf, targets_sf) {
      net <- calc_edge_dist_batch_robust(df %>% select(id, dest_id_geo), pts_sf, targets_sf)
      stopifnot(identical(as.character(net$id), as.character(df$id)),
                identical(as.character(net$dest_id_geo), as.character(df$dest_id_geo)))
      df$net_km_p2t <- net$net_km_p2t
      df$net_km_t2p <- net$net_km_t2p
      df
    }

    # ---- Per-point summary of the N-draw random anchor (+ Monte-Carlo SE) ----
    summarise_random <- function(long_df) {
      long_df %>%
        group_by(id, dest_type) %>%
        summarise(
          n_geo  = sum(!is.na(geo_km)),
          n_p2t  = sum(!is.na(net_km_p2t)),
          n_t2p  = sum(!is.na(net_km_t2p)),
          m_geo  = mean(geo_km,      na.rm = TRUE),
          m_p2t  = mean(net_km_p2t,  na.rm = TRUE),
          m_t2p  = mean(net_km_t2p,  na.rm = TRUE),
          sd_geo = stats::sd(geo_km,     na.rm = TRUE),
          sd_p2t = stats::sd(net_km_p2t, na.rm = TRUE),
          sd_t2p = stats::sd(net_km_t2p, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          se_geo = sd_geo / sqrt(pmax(n_geo, 1)),   # per-point MC SE of the mean
          se_p2t = sd_p2t / sqrt(pmax(n_p2t, 1)),
          se_t2p = sd_t2p / sqrt(pmax(n_t2p, 1))
        )
    }

    designs_out     <- list()
    diag_out        <- list()
    assignments_out <- list()

    for (i in seq_len(nrow(rnd_designs))) {
      fpath <- rnd_designs$file[i]
      if (!file.exists(fpath)) { warning("Anchor block: file not found: ", fpath); next }
      d_label <- rnd_designs$design[i]
      d_order <- rnd_designs$design_order[i]
      raw_i   <- readRDS(fpath)
      pts_i   <- raw_i$pts
      cmp_i   <- raw_i$comparison
      cat("\n  [", d_label, "] points =", nrow(pts_i), "\n")

      # --- VALIDATION: reproduce stored nearest_priv net on a 200-pt subset ---
      # (Confirms the lifted graph/roads_sf/snapping reproduces the stored
      #  distances exactly => the new anchors use a faithful router.)
      val_ids  <- head(pts_i$id, 200)
      pts_val  <- pts_i %>% filter(id %in% val_ids)
      near_chk <- attach_net(get_nearest_geo(pts_val, clinics_private, "nearest_priv"),
                             pts_val, clinics_private)
      stored   <- cmp_i %>% filter(dest_type == "nearest_priv", id %in% val_ids) %>%
        transmute(id, s_geo = geo_km, s_p2t = net_km_p2t, s_t2p = net_km_t2p)
      chk <- near_chk %>% left_join(stored, by = "id")
      cat(sprintf("    [validate] nearest_priv max|delta| geo/p2t/t2p = %.2e / %.2e / %.2e\n",
                  max(abs(chk$geo_km     - chk$s_geo), na.rm = TRUE),
                  max(abs(chk$net_km_p2t - chk$s_p2t), na.rm = TRUE),
                  max(abs(chk$net_km_t2p - chk$s_t2p), na.rm = TRUE)))

      # --- FARTHEST anchors (single target per point) ---
      cat("    computing farthest_priv / farthest_pub ...\n")
      far_priv <- attach_net(get_farthest_geo(pts_i, clinics_private, "farthest_priv"), pts_i, clinics_private)
      far_pub  <- attach_net(get_farthest_geo(pts_i, clinics_public,  "farthest_pub"),  pts_i, clinics_public)

      # --- RANDOM anchors (N draws/point from ALL facilities, then averaged) ---
      cat("    computing random_priv / random_pub  (N =", N_RANDOM_DRAWS, "draws/point) ...\n")
      rnd_priv_long <- attach_net(get_random_geo(pts_i, clinics_private, "random_priv", N_RANDOM_DRAWS, RANDOM_ANCHOR_SEED),        pts_i, clinics_private)
      rnd_pub_long  <- attach_net(get_random_geo(pts_i, clinics_public,  "random_pub",  N_RANDOM_DRAWS, RANDOM_ANCHOR_SEED + 1000L), pts_i, clinics_public)
      rnd_priv_pp <- summarise_random(rnd_priv_long)
      rnd_pub_pp  <- summarise_random(rnd_pub_long)

      # --- Per-point frame -> dat_rnd-compatible rows (net_ft=p2t, net_tf=t2p) ---
      per_point_all <- bind_rows(
        far_priv    %>% transmute(id, dest_type, geo_km, net_km_p2t, net_km_t2p),
        far_pub     %>% transmute(id, dest_type, geo_km, net_km_p2t, net_km_t2p),
        rnd_priv_pp %>% transmute(id, dest_type, geo_km = m_geo, net_km_p2t = m_p2t, net_km_t2p = m_t2p),
        rnd_pub_pp  %>% transmute(id, dest_type, geo_km = m_geo, net_km_p2t = m_p2t, net_km_t2p = m_t2p)
      )
      designs_out[[i]] <- per_point_all %>% transmute(
        stratum = "Random",
        label   = d_label,
        type    = dest_type,
        design_order = d_order,
        geo_km  = geo_km,
        net_ft  = net_km_p2t,
        net_tf  = net_km_t2p
      )

      # --- Diagnostics (printed to local run log; pop-free distances only) ---
      far_diag <- bind_rows(
        far_priv %>% summarise(anchor = "farthest_priv",
                               n_reach_p2t = sum(!is.na(net_km_p2t)), n_reach_t2p = sum(!is.na(net_km_t2p)),
                               mean_geo = mean(geo_km, na.rm=TRUE), mean_p2t = mean(net_km_p2t, na.rm=TRUE), mean_t2p = mean(net_km_t2p, na.rm=TRUE)),
        far_pub  %>% summarise(anchor = "farthest_pub",
                               n_reach_p2t = sum(!is.na(net_km_p2t)), n_reach_t2p = sum(!is.na(net_km_t2p)),
                               mean_geo = mean(geo_km, na.rm=TRUE), mean_p2t = mean(net_km_p2t, na.rm=TRUE), mean_t2p = mean(net_km_t2p, na.rm=TRUE))
      ) %>% mutate(design = d_label, .before = 1)

      # Grand-mean MC SE = sqrt(sum_p se_p^2)/n_pts  (SE of the across-points mean
      # used in Table 1; tiny because it averages out per-point draw noise).
      rnd_diag <- bind_rows(
        rnd_priv_pp %>% summarise(anchor = "random_priv",
            mean_geo = mean(m_geo, na.rm=TRUE), mean_p2t = mean(m_p2t, na.rm=TRUE), mean_t2p = mean(m_t2p, na.rm=TRUE),
            grand_se_p2t = sqrt(sum(se_p2t^2, na.rm=TRUE))/n(), grand_se_t2p = sqrt(sum(se_t2p^2, na.rm=TRUE))/n(),
            perpt_se_p2t_mean = mean(se_p2t, na.rm=TRUE), perpt_se_p2t_med = stats::median(se_p2t, na.rm=TRUE), perpt_se_p2t_max = max(se_p2t, na.rm=TRUE)),
        rnd_pub_pp %>% summarise(anchor = "random_pub",
            mean_geo = mean(m_geo, na.rm=TRUE), mean_p2t = mean(m_p2t, na.rm=TRUE), mean_t2p = mean(m_t2p, na.rm=TRUE),
            grand_se_p2t = sqrt(sum(se_p2t^2, na.rm=TRUE))/n(), grand_se_t2p = sqrt(sum(se_t2p^2, na.rm=TRUE))/n(),
            perpt_se_p2t_mean = mean(se_p2t, na.rm=TRUE), perpt_se_p2t_med = stats::median(se_p2t, na.rm=TRUE), perpt_se_p2t_max = max(se_p2t, na.rm=TRUE))
      ) %>% mutate(design = d_label, n_draws = N_RANDOM_DRAWS,
                   rel_grand_se_p2t = grand_se_p2t / mean_p2t, .before = 1)

      diag_out[[i]] <- list(farthest = far_diag, random = rnd_diag)

      # --- Persist per-point/per-draw clinic assignments so the travel-time
      #     pipeline (Sections 18/19) reuses the SAME draws (Table 1 <-> Table 2
      #     consistency). far: 1 clinic/point; rnd: N clinics/point. ---
      assignments_out[[i]] <- list(
        design       = d_label,
        design_order = d_order,
        far = bind_rows(far_priv, far_pub) %>%
          select(id, dest_type, dest_id_geo, geo_km, net_km_p2t, net_km_t2p),
        rnd = bind_rows(rnd_priv_long, rnd_pub_long) %>%
          select(id, draw, dest_type, dest_id_geo, geo_km, net_km_p2t, net_km_t2p)
      )
    }

    assignments_named <- Filter(Negate(is.null), assignments_out)
    assignments_named <- setNames(assignments_named,
                                  vapply(assignments_named, `[[`, character(1), "design"))

    list(
      rows          = bind_rows(designs_out),
      diag_farthest = bind_rows(lapply(diag_out, `[[`, "farthest")),
      diag_random   = bind_rows(lapply(diag_out, `[[`, "random")),
      assignments   = assignments_named,
      n_draws       = N_RANDOM_DRAWS
    )
  })

  # --- Restore RNG state ---
  if (.had_seed) {
    assign(".Random.seed", .saved_seed, envir = .GlobalEnv)
  } else if (exists(".Random.seed", envir = .GlobalEnv)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }

  saveRDS(.anchor_bundle, anchor_cache)
  cat("[TABLE1 R1-5] Cached new anchors ->", anchor_cache, "\n")
}

# --- Surface the new-anchor diagnostics in the (local) run log ---
cat("\n[TABLE1 R1-5] New-anchor diagnostics (km).  Farthest (single facility):\n")
print(as.data.frame(.anchor_bundle$diag_farthest), row.names = FALSE)
cat("\n[TABLE1 R1-5] Random anchor (mean of", .anchor_bundle$n_draws,
    "draws) + Monte-Carlo SE.  grand_se_* = SE of the across-points mean shown in",
    "Table 1; rel_grand_se_p2t = grand_se_p2t/mean_p2t:\n")
print(as.data.frame(.anchor_bundle$diag_random), row.names = FALSE)
cat("[TABLE1 R1-5] (If rel_grand_se_p2t is well under ~0.01 the draw count is",
    "ample for the Table 1 means; per-point SE shows the raw per-point draw noise.)\n\n")

# --- Append the 8 new random-point rows; existing rows are untouched ---
dat_rnd <- bind_rows(dat_rnd, .anchor_bundle$rows)

# --------------------------------------------------------------------------
# 3. AGGREGATION FUNCTION
# --------------------------------------------------------------------------
calc_stats <- function(df, group_vars) {
  df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      n_geo   = sum(!is.na(geo_km)),
      avg_geo = mean(geo_km, na.rm = TRUE),
      
      n_ft    = sum(!is.na(net_ft)),
      avg_ft  = mean(net_ft, na.rm = TRUE),
      
      n_tf    = sum(!is.na(net_tf)),
      avg_tf  = mean(net_tf, na.rm = TRUE),
      
      .groups = "drop"
    )
}

# --------------------------------------------------------------------------
# 4. PREPARE TABLES
# --------------------------------------------------------------------------

# Labels
lbl_fac <- function(s, l) {
  s <- tolower(trimws(s)); l <- tolower(trimws(l))
  case_when(
    s=="ownership" & l=="private" ~ "Private dental facility",
    s=="ownership" & l=="public"  ~ "Public dental facility",
    s=="region" ~ paste0("Dental facility — Region: ", str_to_title(l)),
    s=="private_subtype" ~ paste0("Private dental facility — Subtype: ", str_to_title(l)),
    s=="public_subtype" ~ paste0("Public dental facility — Subtype: ", str_to_title(l)),
    TRUE ~ paste0(s, ": ", l)
  )
}

lbl_to <- function(x) {
  x <- tolower(trimws(x))
  case_when(
    str_detect(x, "bus") ~ "Nearest bus stop",
    str_detect(x, "metro") ~ "Nearest metro station",
    str_detect(x, "nearest_priv") ~ "Nearest private dental facility",
    str_detect(x, "median_priv") ~ "Median-distance private dental facility",
    str_detect(x, "farthest_priv") ~ "Farthest private dental facility",
    str_detect(x, "random_priv") ~ "Random private dental facility",
    str_detect(x, "nearest_pub") ~ "Nearest public dental facility",
    str_detect(x, "median_pub") ~ "Median-distance public dental facility",
    str_detect(x, "farthest_pub") ~ "Farthest public dental facility",
    str_detect(x, "random_pub") ~ "Random public dental facility",
    TRUE ~ str_to_sentence(x)
  )
}

# Process Facilities
stats_fac <- dat_fac %>%
  calc_stats(c("stratum", "label", "type")) %>%
  mutate(
    Point1 = lbl_fac(stratum, label),
    Point2 = lbl_to(type),
    sec_ord = case_when(stratum=="ownership" & label=="private"~1, stratum=="ownership" & label=="public"~2, stratum=="region"~3, stratum=="private_subtype"~4, TRUE~5),
    sub_ord = paste0(stratum, "_", label),
    to_ord  = if_else(str_detect(Point2, "bus"), 1, 2)
  )

# Process Random Points
stats_rnd <- dat_rnd %>%
  calc_stats(c("label", "type", "design_order")) %>%
  mutate(
    Point1 = paste0("Random point — ", label),
    Point2 = lbl_to(type),
    sec_ord = 6,
    sub_ord = as.character(design_order),
    to_ord  = match(Point2, c("Nearest bus stop", "Nearest metro station", "Nearest private dental facility", "Median-distance private dental facility", "Nearest public dental facility", "Median-distance public dental facility", "Farthest private dental facility", "Farthest public dental facility", "Random private dental facility", "Random public dental facility"))
  )

# --------------------------------------------------------------------------
# 5. MERGE, CALCULATE RATIOS & FORMAT
# --------------------------------------------------------------------------
final_df <- bind_rows(stats_fac, stats_rnd) %>%
  arrange(sec_ord, sub_ord, to_ord) %>%
  mutate(
    # Calculate Ratios
    ratio_ft = avg_ft / avg_geo,
    ratio_tf = avg_tf / avg_geo
  ) %>%
  transmute(
    `1st Point` = Point1,
    `2nd Point` = Point2,
    `n (pairs)` = n_geo,
    `Avg Geo Dist (km)` = sprintf("%.1f", avg_geo),
    
    `n (1st->2nd)` = n_ft,
    `Avg Net 1st->2nd (km)` = sprintf("%.1f", avg_ft),
    `Ratio (1st->2nd)` = sprintf("%.1f", ratio_ft),
    
    `n (2nd->1st)` = n_tf,
    `Avg Net 2nd->1st (km)` = sprintf("%.1f", avg_tf),
    `Ratio (2nd->1st)` = sprintf("%.1f", ratio_tf)
  )

# --------------------------------------------------------------------------
# 6. EXPORT TO WORD (Grouped zebra shading + MERGED ROWS)
# --------------------------------------------------------------------------

library(flextable)
library(officer)

# ---- Journal-style table title ----
table_title <- "Table 1. Directional distance analysis (geometric vs directed network distances) using the robust edge method."
table_note  <- "Values are mean distances in kilometers; ratios represent network-to-geometric distance."

# ---- Grouped zebra shading (by 1st Point) ----
# We calculate this BEFORE merging, as merging affects visual rows but the background logic holds
grp <- final_df$`1st Point`
grp_id <- match(grp, unique(grp))
bg_vec <- ifelse(grp_id %% 2 == 1, "#FFFFFF", "#E6E6E6")

ft <- flextable(final_df) %>%
  theme_booktabs() %>%
  font(fontname = "Arial", part = "all") %>%
  bold(part = "header") %>%
  fontsize(size = 9, part = "all") %>%
  padding(padding = 3, part = "all") %>%
  align(j = 1:2, align = "left", part = "all") %>%
  align(j = 3:10, align = "center", part = "all") %>%
  
  # Apply Zebra Background
  bg(i = seq_len(nrow(final_df)), bg = bg_vec, part = "body") %>%
  
  # *** MERGE ROWS based on the 1st Column ***
  merge_v(j = "1st Point") %>%   # Merges consecutive identical values in column 1
  valign(j = "1st Point", valign = "center", part = "body") %>% # Center text vertically
  
  # Fix Borders: Add a horizontal line between the groups to separate merged blocks clearly
  border_inner_h(part = "body", border = fp_border(color="white", width = 0)) %>% # Clean inner lines
  fix_border_issues() %>% # Ensures outer borders are correct after merging
  
  autofit() %>%
  set_table_properties(width = 1, layout = "autofit")

ft

# Print to Word
doc <- read_docx() %>%
  body_add_par(table_title, style = "Table Caption") %>%
  body_add_par(table_note, style = "Normal") %>%
  body_add_flextable(ft)

print(doc, target = out_docx)
cat("\nReport with merged rows generated successfully:\n", out_docx, "\n")

# --- Optional early exit for fast Table 1 verification (default OFF) ---
# STOP_AFTER_TABLE1=1 stops here, right after Table 1 is written, so the R1-5
# anchors can be verified without running the multi-hour S18/S19 pipeline.
if (Sys.getenv("STOP_AFTER_TABLE1") == "1") {
  cat("[RUN STATUS] STOP_AFTER_TABLE1=1 -- Table 1 written, exiting before Section 14.\n")
  quit(save = "no", status = 0)
}


##############################################################################
##############################################################################
# Section 14: Geometric distance from nearest metro station/bus stop to dental
# facilities and their actual road network distances
# Stratified by sector and region
# Stratified by private and public sector subtypes
##############################################################################
##############################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(ragg)

# ==============================================================================
# 1. FONT & STYLE CONFIGURATION
# ==============================================================================

# --- Font Family ---
FONT_FAMILY <- "sans" # "sans" (Arial), "serif" (Times), "mono" (Courier)

# --- Text Sizes ---
SIZE_SUPER_TITLE <- 34   # Main Figure Title
SIZE_SUB_TITLE   <- 28   # Panel Titles / Subtitles
SIZE_TAG         <- 50   # Panel Tags (A, B, C, D) <-- INCREASED!
SIZE_FACET       <- 24   # Facet Labels ("Bus", "Metro")
SIZE_AXIS_TITLE  <- 20   # Axis Labels ("Distance (km)")
SIZE_AXIS_TEXT   <- 18   # Tick Labels
SIZE_MEAN_TEXT   <- 6    # Mean Value Labels (Numbers above diamonds)

# --- Boldness ---
FACE_SUPER_TITLE <- "bold"
FACE_SUB_TITLE   <- "bold"
FACE_TAG         <- "bold" # <-- ENSURED BOLD
FACE_FACET       <- "bold"
FACE_AXIS_TITLE  <- "bold"
FACE_AXIS_TEXT   <- "plain"

# ==============================================================================
# 2. DATA PREPARATION
# ==============================================================================

# --- A. Load Actual Network Distances ---
robust_rds <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data/clinics_geo_vs_network_ROBUST_DIRECTED.rds"
if(!file.exists(robust_rds)) stop("Robust RDS not found!")
res_robust <- readRDS(robust_rds)
comparison_all <- res_robust$results$comparison_all

# *** STRICT DEDUPLICATION ***
net_data <- comparison_all %>%
  mutate(id = as.character(id), type = tolower(trimws(type))) %>%
  group_by(id, type) %>% 
  slice(1) %>% 
  ungroup() %>%
  select(id, type, net_km_c2s, net_km_s2c)

# --- B. Load Geometric Data ---
if(!exists("clinics_with_nearest")) stop("clinics_with_nearest object missing!")

clinics_df <- clinics_with_nearest %>%
  { if (inherits(., "sf")) sf::st_drop_geometry(.) else . } %>%
  filter(with_dental_services != "No") %>%
  mutate(id = dplyr::coalesce(as.character(id), as.character(row_number()))) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    private_or_public = tools::toTitleCase(tolower(trimws(as.character(private_or_public)))),
    type_with_dental  = tools::toTitleCase(tolower(trimws(as.character(type_with_dental)))),
    new_region        = tools::toTitleCase(tolower(trimws(as.character(new_region)))),
    geo_metro_km = nearest_metro_m / 1000,
    geo_bus_km   = nearest_bus_m   / 1000
  )

# --- C. Merge Data ---
df_long <- clinics_df %>%
  select(id, private_or_public, type_with_dental, new_region, geo_metro_km, geo_bus_km) %>%
  pivot_longer(cols = c(geo_metro_km, geo_bus_km), names_to = "t_raw", values_to = "geo_km") %>%
  mutate(type = ifelse(grepl("metro", t_raw), "metro station", "bus stop"))

df_final <- df_long %>%
  left_join(net_data, by = c("id", "type")) %>%
  mutate(
    dist_c2s = net_km_c2s,
    dist_s2c = net_km_s2c,
    Transit_Type = tools::toTitleCase(gsub(" station| stop", "", type))
  )

print(paste("Total Rows in Plot Data:", nrow(df_final)))

# ==============================================================================
# 3. PLOTTING FUNCTIONS
# ==============================================================================
sec_lvls <- c("Private", "Public")
reg_lvls <- c("Center", "South", "East", "North", "West")
prv_lvls <- c("Dental Clinic Only", "Polyclinic/Cosmetic Clinic", "Hospital")
pub_lvls <- c("Primary Care Center", "Specialized Dental Center")

# --- Helper: Common Theme Settings ---
custom_theme <- theme_minimal(base_size = 12, base_family = FONT_FAMILY) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    
    # Apply Config
    plot.title    = element_text(size = SIZE_SUB_TITLE,  face = FACE_SUB_TITLE, hjust = 0.5),
    strip.text    = element_text(size = SIZE_FACET,      face = FACE_FACET),
    axis.title    = element_text(size = SIZE_AXIS_TITLE, face = FACE_AXIS_TITLE),
    axis.text.y   = element_text(size = SIZE_AXIS_TEXT,  face = FACE_AXIS_TEXT),
    axis.text.x   = element_text(size = SIZE_AXIS_TEXT,  face = FACE_AXIS_TEXT, angle = 45, hjust = 1)
  )

# --- Function A: GEOMETRIC (Scales = Free) ---
make_plot_geo <- function(data, x_var, x_lvls, y_col, y_max, title, x_lab) {
  df_plot <- data %>%
    mutate(.x = factor(.data[[x_var]], levels = x_lvls)) %>%
    filter(!is.na(.x)) %>%
    group_by(Transit_Type, .x) %>%
    mutate(label = paste0(.x, "\n(n=", n(), ")")) %>%
    ungroup()
  
  lbl_ord <- df_plot %>% distinct(.x, label) %>% arrange(.x) %>% pull(label)
  df_plot$label <- factor(df_plot$label, levels = lbl_ord)
  
  ggplot(df_plot, aes(x = label, y = .data[[y_col]], fill = .data[[x_var]])) +
    geom_violin(trim = FALSE, alpha = 0.5, scale = "width") +
    geom_jitter(height = 0, width = 0.1, alpha = 0.1, size = 1) +
    geom_boxplot(width = 0.15, alpha = 0.5, outlier.shape = NA, fill = "white") +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "black") +
    stat_summary(fun = mean, geom = "text", aes(label=sprintf("%.1f", after_stat(y))), 
                 vjust=-1.5, size=SIZE_MEAN_TEXT, family=FONT_FAMILY) +
    
    facet_wrap(~Transit_Type, scales = "free") + 
    coord_cartesian(ylim = c(0, y_max)) +
    scale_fill_viridis_d(option = "plasma", end = 0.9) +
    labs(title = title, y = "Distance (km)", x = x_lab) +
    
    custom_theme
}

# --- Function B: NETWORK (Fixed Scales) ---
make_plot_net <- function(data, x_var, x_lvls, y_col, y_max, title, x_lab) {
  df_plot <- data %>%
    mutate(.x = factor(.data[[x_var]], levels = x_lvls)) %>%
    filter(!is.na(.x)) %>%
    group_by(Transit_Type, .x) %>%
    mutate(label = paste0(.x, "\n(n=", n(), ")")) %>%
    ungroup()
  
  lbl_ord <- df_plot %>% distinct(.x, label) %>% arrange(.x) %>% pull(label)
  df_plot$label <- factor(df_plot$label, levels = lbl_ord)
  
  ggplot(df_plot, aes(x = label, y = .data[[y_col]], fill = .data[[x_var]])) +
    geom_violin(trim = FALSE, alpha = 0.5, scale = "width") +
    geom_jitter(height = 0, width = 0.1, alpha = 0.1, size = 1) +
    geom_boxplot(width = 0.15, alpha = 0.5, outlier.shape = NA, fill = "white") +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "black") +
    stat_summary(fun = mean, geom = "text", aes(label=sprintf("%.1f", after_stat(y))), 
                 vjust=-1.5, size=SIZE_MEAN_TEXT, family=FONT_FAMILY) +
    
    facet_wrap(~Transit_Type) + # Fixed scales
    coord_cartesian(ylim = c(0, y_max)) +
    scale_fill_viridis_d(option = "plasma", end = 0.9) +
    labs(title = title, y = "Distance (km)", x = x_lab) +
    
    custom_theme
}

# ==============================================================================
# 4. GENERATE FIGURES
# ==============================================================================

# --- Theme for Annotation (Tags, Main Title, Subtitle) ---
annot_theme <- theme(
  text = element_text(family = FONT_FAMILY),
  plot.title    = element_text(size = SIZE_SUPER_TITLE, face = FACE_SUPER_TITLE, hjust = 0.5),
  # Plain, slightly smaller subtitle
  plot.subtitle = element_text(size = SIZE_SUB_TITLE - 4, face = "plain", hjust = 0.5), 
  # Tags (A, B, C, D) - Explicitly Bold and Large
  plot.tag      = element_text(size = SIZE_TAG, face = "bold") 
)

# --- New Explanatory Subtitle ---
sub_text <- "Violin: Density | Boxplot: Median & Interquartile range | Diamond: Mean"

# --- FIGURE 1 ---
p1_sec <- make_plot_geo(df_final, "private_or_public", sec_lvls, "geo_km", 5, "By Sector", "Sector")
p1_reg <- make_plot_geo(df_final, "new_region", reg_lvls, "geo_km", 7, "By Region", "Region")
fig1 <- (p1_sec / p1_reg) + 
  plot_annotation(
    title = "Geometric Distance to Nearest Transit by Sector and Region", 
    subtitle = sub_text, 
    tag_levels = "A", 
    theme = annot_theme
  )
agg_tiff("Fig1_Geometric_SectorRegion.tiff", width=16, height=18, units="in", res=300, compression="lzw", background="white"); print(fig1); dev.off()

# --- FIGURE 2 ---
p2_prv <- make_plot_geo(filter(df_final, private_or_public=="Private"), "type_with_dental", prv_lvls, "geo_km", 5, "By Private Subtype", "Subtype")
p2_pub <- make_plot_geo(filter(df_final, private_or_public=="Public"), "type_with_dental", pub_lvls, "geo_km", 7, "By Public Subtype", "Subtype")
fig2 <- (p2_prv / p2_pub) + 
  plot_annotation(
    title = "Geometric Distance to Nearest Transit by Sector Subtypes", 
    subtitle = sub_text, 
    tag_levels = "A", 
    theme = annot_theme
  )
agg_tiff("Fig2_Geometric_Subtypes.tiff", width=16, height=18, units="in", res=300, compression="lzw", background="white"); print(fig2); dev.off()

# --- FIGURE 3 ---
# Updated Titles: [Category]: [Direction]
p3_sec_s2c <- make_plot_net(df_final, "private_or_public", sec_lvls, "dist_s2c", 6, "By Sector: Nearest Transit \u2192 Facility", "Sector")
p3_sec_c2s <- make_plot_net(df_final, "private_or_public", sec_lvls, "dist_c2s", 6, "By Sector: Facility \u2192 Nearest Transit", "Sector")
p3_reg_s2c <- make_plot_net(df_final, "new_region", reg_lvls, "dist_s2c", 8, "By Region: Nearest Transit \u2192 Facility", "Region")
p3_reg_c2s <- make_plot_net(df_final, "new_region", reg_lvls, "dist_c2s", 8, "By Region: Facility \u2192 Nearest Transit", "Region")

# 2. REMOVE Y-AXIS TITLE for the right-side plots
p3_sec_c2s <- p3_sec_c2s + ylab(NULL)
p3_reg_c2s <- p3_reg_c2s + ylab(NULL)

fig3 <- (p3_sec_s2c + p3_sec_c2s) / (p3_reg_s2c + p3_reg_c2s) +
  plot_annotation(
    title = "Actual Directional Road Network Distance by Sector and Region", 
    subtitle = sub_text, 
    tag_levels = "A", 
    theme = annot_theme
  )
agg_tiff("Fig3_Network_SectorRegion.tiff", width=20, height=20, units="in", res=300, compression="lzw", background="white"); print(fig3); dev.off()

# --- FIGURE 4 ---
# Updated Titles: [Category]: [Direction]
p4_prv_s2c <- make_plot_net(filter(df_final, private_or_public=="Private"), "type_with_dental", prv_lvls, "dist_s2c", 6, "By Private Subtype: Nearest Transit \u2192 Facility", "Subtype")
p4_prv_c2s <- make_plot_net(filter(df_final, private_or_public=="Private"), "type_with_dental", prv_lvls, "dist_c2s", 6, "By Private Subtype: Facility \u2192 Nearest Transit", "Subtype")
p4_pub_s2c <- make_plot_net(filter(df_final, private_or_public=="Public"), "type_with_dental", pub_lvls, "dist_s2c", 8, "By Public Subtype: Nearest Transit \u2192 Facility", "Subtype")
p4_pub_c2s <- make_plot_net(filter(df_final, private_or_public=="Public"), "type_with_dental", pub_lvls, "dist_c2s", 8, "By Public Subtype: Facility \u2192 Nearest Transit", "Subtype")

# 2. REMOVE Y-AXIS TITLE for the right-side plots
p4_prv_c2s <- p4_prv_c2s + ylab(NULL)
p4_pub_c2s <- p4_pub_c2s + ylab(NULL)

fig4 <- (p4_prv_s2c + p4_prv_c2s) / (p4_pub_s2c + p4_pub_c2s) +
  plot_annotation(
    title = "Actual Directional Road Network Distance by Sector Subtypes", 
    subtitle = sub_text, 
    tag_levels = "A", 
    theme = annot_theme
  )
agg_tiff("Fig4_Network_Subtypes.tiff", width=20, height=20, units="in", res=300, compression="lzw", background="white"); print(fig4); dev.off()

cat("\n✅ Final Figures Updated: Titles for Fig 3 and 4 include direction arrows.\n")


# ==============================================================================
# VERIFICATION: ANALYSIS OF DIRECTIONAL DIFFERENCES (FACILITY <-> TRANSIT)
# ==============================================================================

library(dplyr)
library(ggplot2)
library(ragg)

# 1. Load Data
robust_rds <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data/clinics_geo_vs_network_ROBUST_DIRECTED.rds"
if(!file.exists(robust_rds)) stop("File not found! Check the path.")

res_robust <- readRDS(robust_rds)
comparison_all <- res_robust$results$comparison_all

# 2. Strict Deduplication
check_df <- comparison_all %>%
  mutate(id = as.character(id), type = tolower(trimws(type))) %>%
  group_by(id, type) %>% 
  slice(1) %>% 
  ungroup() %>%
  select(id, type, net_km_c2s, net_km_s2c)

# 3. Calculate Differences
check_df <- check_df %>%
  mutate(
    # Calculate absolute difference
    diff_val = net_km_c2s - net_km_s2c,
    # Check if effectively identical (allow tiny float precision error)
    is_identical = abs(diff_val) < 0.001,
    status = ifelse(is_identical, "Exact Same", "Different")
  )

# 4. SAVE TEXT REPORT
sink("Directional_Difference_Report.txt")
cat("--- SUMMARY OF DIRECTIONAL DIFFERENCES ---\n\n")

cat("1. COUNT of Identical vs. Different Routes:\n")
print(table(check_df$status))

cat("\n2. STATISTICS of the Difference (Facility->Transit MINUS Transit->Facility):\n")
cat("   (Positive means driving TO the transit is longer)\n")
print(summary(check_df$diff_val))

cat("\n3. SAMPLE of Differences (First 10 non-identical rows):\n")
print(head(filter(check_df, !is_identical), 10))
sink() # Stop writing to file

# 5. SAVE PLOT IMAGE
# Scatter plot: x = To Transit, y = From Transit
# Red line = Perfect symmetry (x=y)
p <- ggplot(check_df, aes(x = net_km_c2s, y = net_km_s2c)) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1, linetype = "dashed") +
  geom_point(alpha = 0.4, size = 1.5) +
  labs(
    title = "Comparison of Directional Road Distances",
    subtitle = "Red Line = Identical Distance. Points off the line indicate one-way streets/detours.",
    x = "Distance: Facility -> Transit (km)",
    y = "Distance: Transit -> Facility (km)"
  ) +
  theme_minimal(base_size = 14) +
  coord_fixed() # Ensures 1km on X looks the same as 1km on Y

agg_tiff("Directional_Difference_Plot.tiff", width = 8, height = 8, units = "in", res = 300, compression = "lzw", background = "white")
print(p)
dev.off()

cat("\n✅ DONE. Check your folder for:\n   - 'Directional_Difference_Report.txt'\n   - 'Directional_Difference_Plot.tiff'\n")



###########################################################################
# Section 15: Cumulative percent of percentage of clinics within 0.2 - 1 km
# of nearest bus stop or metro station stratified by private vs public, 
# private subtype, public subtype, and region
###########################################################################
###########################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(ragg)
library(scales)

# ==============================================================================
# 1. FONT & STYLE CONFIGURATION
# ==============================================================================
FONT_FAMILY      <- "sans"

# --- Text Sizes ---
SIZE_SUPER_TITLE <- 34
SIZE_SUB_TITLE   <- 28
SIZE_TAG         <- 70
SIZE_FACET       <- 26
SIZE_AXIS_TITLE  <- 22
SIZE_AXIS_TEXT   <- 18

# --- Legend Control (UPDATED) ---
SIZE_LEGEND_TITLE <- 22   # Font size for Legend Title
SIZE_LEGEND_TEXT  <- 20   # Font size for Legend Items
SIZE_LEGEND_KEY_H <- 2  # Height of legend shapes (cm)
SIZE_LEGEND_KEY_W <- 2  # Width of legend shapes (cm)

# --- Font Faces ---
FACE_SUPER_TITLE <- "bold"
FACE_SUB_TITLE   <- "bold"
FACE_TAG         <- "bold"
FACE_FACET       <- "bold"
FACE_AXIS_TITLE  <- "bold"
FACE_AXIS_TEXT   <- "plain"

# ==============================================================================
# 2. DATA PREPARATION (ROBUST)
# ==============================================================================

# --- Define Ordered Levels (User Specified) ---
sec_lvls <- c("Private", "Public")
reg_lvls <- c("Center", "South", "East", "North", "West")
prv_lvls <- c("Dental Clinic Only", "Polyclinic/Cosmetic Clinic", "Hospital")
pub_lvls <- c("Primary Care Center", "Specialized Dental Center")

# --- A. Load Data ---
robust_rds <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data/clinics_geo_vs_network_ROBUST_DIRECTED.rds"
if(!file.exists(robust_rds)) stop("Robust RDS not found!")
res_robust <- readRDS(robust_rds)
comparison_all <- res_robust$results$comparison_all

# Deduplicate Network Data
net_data <- comparison_all %>%
  mutate(id = as.character(id), type = tolower(trimws(type))) %>%
  group_by(id, type) %>% slice(1) %>% ungroup() %>%
  select(id, type, net_km_c2s, net_km_s2c)

if(!exists("clinics_with_nearest")) stop("clinics_with_nearest object missing!")

# --- Ensure clinics_df is cleaned and FACTORS are applied ---
clinics_df <- clinics_with_nearest %>%
  as.data.frame() %>%
  select(-matches("^geometry$|^geom$")) %>%
  filter(with_dental_services != "No") %>%
  mutate(id = dplyr::coalesce(as.character(id), as.character(row_number()))) %>%
  distinct(id, .keep_all=TRUE) %>%
  mutate(
    # 1. Clean Strings first
    private_or_public = tools::toTitleCase(tolower(trimws(as.character(private_or_public)))),
    new_region        = tools::toTitleCase(tolower(trimws(as.character(new_region)))),
    type_with_dental  = tools::toTitleCase(tolower(trimws(as.character(type_with_dental)))),
    
    # 2. Convert to Factors with Explicit Levels
    private_or_public = factor(private_or_public, levels = sec_lvls),
    new_region        = factor(new_region, levels = reg_lvls),
    
    geo_metro_km = nearest_metro_m / 1000,
    geo_bus_km   = nearest_bus_m    / 1000
  )

# Join Network Data
net_joined <- clinics_df %>%
  select(id, private_or_public, type_with_dental, new_region) %>%
  left_join(net_data, by = "id") %>%
  mutate(Transit_Type = tools::toTitleCase(gsub(" station| stop", "", type)))

# --- B. Calculation Function ---
calc_cumulative <- function(data, group_var, dist_col, threshold_seq = seq(0.2, 1.0, 0.1)) {
  data <- as.data.frame(data)
  
  # 1. Get Totals (N) per group
  totals <- data %>%
    group_by(.data[[group_var]]) %>%
    summarise(total_n = n(), .groups = "drop")
  
  # 2. Grid of Groups x Thresholds
  g_vals <- if(is.factor(data[[group_var]])) levels(data[[group_var]]) else unique(data[[group_var]])
  g_vals <- g_vals[g_vals %in% unique(data[[group_var]])]
  
  grid <- expand_grid(
    Group = g_vals,
    Threshold = threshold_seq
  )
  
  # 3. Calculate Counts
  res <- grid %>%
    left_join(totals, by = c("Group" = group_var))
  
  res$Count <- mapply(function(g, t) {
    vals <- data[[dist_col]][data[[group_var]] == g]
    sum(vals <= t, na.rm = TRUE)
  }, res$Group, res$Threshold)
  
  # 4. Final Percentage
  res <- res %>% mutate(Percentage = Count / total_n)
  
  res$Group <- factor(res$Group, levels = g_vals)
  
  return(res)
}

# --- C. Generate Plot Data ---

# 1. GEOMETRIC
get_geo_data <- function(grp_col, strat_label, force_levels = NULL) {
  df <- bind_rows(
    calc_cumulative(clinics_df, grp_col, "geo_bus_km") %>% mutate(Transit="Bus"),
    calc_cumulative(clinics_df, grp_col, "geo_metro_km") %>% mutate(Transit="Metro")
  ) %>% mutate(Stratum = strat_label)
  
  if(!is.null(force_levels)) {
    df$Group <- factor(df$Group, levels = force_levels)
  }
  return(df)
}

df_geo_sec <- get_geo_data("private_or_public", "Sector", sec_lvls)
df_geo_reg <- get_geo_data("new_region", "Region", reg_lvls)
df_geo_prv <- get_geo_data("type_with_dental", "Private", prv_lvls) %>% filter(Group %in% prv_lvls)
df_geo_pub <- get_geo_data("type_with_dental", "Public", pub_lvls) %>% filter(Group %in% pub_lvls)

# 2. NETWORK (Directional)
get_net_data <- function(grp_col, strat_label, force_levels = NULL) {
  d_bus   <- net_joined %>% filter(Transit_Type == "Bus")
  d_metro <- net_joined %>% filter(Transit_Type == "Metro")
  
  s2c_bus   <- calc_cumulative(d_bus, grp_col, "net_km_s2c") %>% mutate(Transit="Bus", Direction="Nearest Transit -> Facility")
  s2c_metro <- calc_cumulative(d_metro, grp_col, "net_km_s2c") %>% mutate(Transit="Metro", Direction="Nearest Transit -> Facility")
  c2s_bus   <- calc_cumulative(d_bus, grp_col, "net_km_c2s") %>% mutate(Transit="Bus", Direction="Facility -> Nearest Transit")
  c2s_metro <- calc_cumulative(d_metro, grp_col, "net_km_c2s") %>% mutate(Transit="Metro", Direction="Facility -> Nearest Transit")
  
  df <- bind_rows(s2c_bus, s2c_metro, c2s_bus, c2s_metro) %>% mutate(Stratum = strat_label)
  
  if(!is.null(force_levels)) {
    df$Group <- factor(df$Group, levels = force_levels)
  }
  return(df)
}

df_net_sec <- get_net_data("private_or_public", "Sector", sec_lvls)
df_net_reg <- get_net_data("new_region", "Region", reg_lvls)
df_net_prv <- get_net_data("type_with_dental", "Private", prv_lvls) %>% filter(Group %in% prv_lvls)
df_net_pub <- get_net_data("type_with_dental", "Public", pub_lvls) %>% filter(Group %in% pub_lvls)

# ==============================================================================
# 3. PLOTTING FUNCTIONS
# ==============================================================================

# --- Theme (UPDATED WITH LEGEND SIZES) ---
cumul_theme <- theme_minimal(base_size = 12, base_family = FONT_FAMILY) +
  theme(
    legend.position = "bottom",
    
    # --- Legend Font Sizes ---
    legend.title = element_text(size = SIZE_LEGEND_TITLE, face = "bold"),
    legend.text  = element_text(size = SIZE_LEGEND_TEXT),
    
    # --- Legend Shape/Key Sizes ---
    legend.key.height = unit(SIZE_LEGEND_KEY_H, "cm"),
    legend.key.width  = unit(SIZE_LEGEND_KEY_W, "cm"),
    
    plot.title = element_text(size = SIZE_SUB_TITLE, face = FACE_SUB_TITLE, hjust = 0.5),
    strip.text = element_text(size = SIZE_FACET, face = FACE_FACET),
    axis.title = element_text(size = SIZE_AXIS_TITLE, face = FACE_AXIS_TITLE),
    axis.text  = element_text(size = SIZE_AXIS_TEXT, face = FACE_AXIS_TEXT),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# --- Helper Labels ---
get_labels <- function(data) {
  unique_groups <- data %>% 
    distinct(Group, total_n) %>%
    arrange(Group)
  
  stats::setNames(
    paste0(unique_groups$Group, " (n=", unique_groups$total_n, ")"),
    as.character(unique_groups$Group)
  )
}

# --- A. Single Plot (Geometric) ---
make_cumul_single <- function(data, title) {
  lbls <- get_labels(data) 
  
  ggplot(data, aes(x = Threshold, y = Percentage, color = Group, linetype = Transit, shape = Transit)) +
    geom_line(linewidth = 1) +
    geom_point(size = 6) +
    scale_y_continuous(labels = percent_format(accuracy=1), limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(0.2, 1.0, 0.1)) +
    scale_color_viridis_d(option = "plasma", end = 0.9, labels = lbls) +
    labs(title = title, x = "Distance Threshold (km)", y = "Cumulative % of facilities within distance\nthreshold") +
    guides(
      linetype = guide_legend(order = 1),
      shape    = guide_legend(order = 1),
      color    = guide_legend(order = 2)
    ) +
    cumul_theme
}

# --- B. Split Directional Plot (Network - MERGED TITLES) ---
make_cumul_dir_split <- function(data, title) {
  lbls <- get_labels(data) 
  
  # *** Create Combined Titles Here ***
  t_left  <- paste0(title, ": Nearest Transit \u2192 Facility")
  t_right <- paste0(title, ": Facility \u2192 Nearest Transit")
  
  p_left <- ggplot(data %>% filter(Direction == "Nearest Transit -> Facility"), 
                   aes(x = Threshold, y = Percentage, color = Group, linetype = Transit, shape = Transit)) +
    geom_line(linewidth = 1) +
    geom_point(size = 5) +
    scale_y_continuous(labels = percent_format(accuracy=1), limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(0.2, 1.0, 0.2)) +
    scale_color_viridis_d(option = "plasma", end = 0.9, labels = lbls) +
    
    # Use Combined Title
    labs(title = t_left, x = "Threshold (km)", y = "Cumulative % of facilities within distance\nthreshold") +
    
    guides(
      linetype = guide_legend(order = 1),
      shape    = guide_legend(order = 1),
      color    = guide_legend(order = 2)
    ) +
    cumul_theme + theme(legend.position = "none")
  
  p_right <- ggplot(data %>% filter(Direction == "Facility -> Nearest Transit"), 
                    aes(x = Threshold, y = Percentage, color = Group, linetype = Transit, shape = Transit)) +
    geom_line(linewidth = 1) +
    geom_point(size = 6) +
    scale_y_continuous(labels = percent_format(accuracy=1), limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(0.2, 1.0, 0.2)) +
    scale_color_viridis_d(option = "plasma", end = 0.9, labels = lbls) +
    
    # Use Combined Title
    labs(title = t_right, x = "Threshold (km)", y = NULL) +
    
    guides(
      linetype = guide_legend(order = 1),
      shape    = guide_legend(order = 1),
      color    = guide_legend(order = 2)
    ) +
    cumul_theme + 
    theme(
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(),
      legend.box = "vertical"
    )
  
  (p_left + p_right) + 
    plot_layout(widths = c(1, 1), guides = 'collect') + 
    plot_annotation(
      theme = theme(
        legend.position = "bottom"
      )
    )
}

# ==============================================================================
# 4. GENERATE 4 FIGURES
# ==============================================================================

global_theme <- theme(
  text = element_text(family = FONT_FAMILY),
  plot.title = element_text(size = SIZE_SUPER_TITLE, face = FACE_SUPER_TITLE, hjust = 0.5),
  plot.subtitle = element_text(size = SIZE_SUB_TITLE - 4, face = "plain", hjust = 0.5),
  plot.tag = element_text(size = SIZE_TAG, face = FACE_TAG)
)

sub_txt <- "Line/Shape: Transit Type (Circle=Bus, Triangle=Metro) | Color: Group"

# ------------------------------------------------------------------------------
# NEW: Define Legend Overrides here
# This tells ggplot: "In the legend, make lines width 2 and points size 4"
# ------------------------------------------------------------------------------
legend_override <- guides(
  # Transit (Shape & Linetype) -> First
  linetype = guide_legend(order = 1, override.aes = list(linewidth = 2)),
  shape    = guide_legend(order = 1, override.aes = list(size = 6)),
  
  # Group (Color) -> Second
  color    = guide_legend(order = 2, override.aes = list(linewidth = 2, size = 6))
)

# --- FIGURE 5: Geometric (Sector + Region) ---
p5_sec <- make_cumul_single(df_geo_sec, "By Sector")
p5_reg <- make_cumul_single(df_geo_reg, "By Region")
fig5 <- (p5_sec / p5_reg) + 
  plot_annotation(title = "Cumulative Geometric Distance between Facility and\nNearest Transit by Sector & Region", tag_levels = "A", theme = global_theme) & legend_override  # <--- THIS WAS MISSING
fig5
agg_tiff("Fig5_Cumulative_Geo_SectorRegion.tiff", width=22, height=20, units="in", res=300, compression="lzw", background="white"); print(fig5); dev.off()

# --- FIGURE 6: Geometric (Subtypes) ---
p6_prv <- make_cumul_single(df_geo_prv, "By Private Subtype")
p6_pub <- make_cumul_single(df_geo_pub, "By Public Subtype")
fig6 <- (p6_prv / p6_pub) + 
  plot_annotation(title = "Cumulative Geometric Distance between Facility and\nNearest Transit by Sector Subtypes", tag_levels = "A", theme = global_theme) & legend_override  # <--- THIS WAS MISSING
agg_tiff("Fig6_Cumulative_Geo_Subtypes.tiff", width=22, height=20, units="in", res=300, compression="lzw", background="white"); print(fig6); dev.off()

# --- FIGURE 7: Network (Sector + Region) ---
p7_sec <- make_cumul_dir_split(df_net_sec, "By Sector")
p7_reg <- make_cumul_dir_split(df_net_reg, "By Region")
fig7 <- (p7_sec / p7_reg) + 
  plot_annotation(title = "Cumulative Actual Directional Road Network Distance between\nFacility and Nearest Transit by Sector & Region", tag_levels = "A", theme = global_theme) & legend_override  # <--- THIS WAS MISSING
agg_tiff("Fig7_Cumulative_Net_SectorRegion.tiff", width=22, height=20, units="in", res=300, compression="lzw", background="white"); print(fig7); dev.off()

# --- FIGURE 8: Network (Subtypes) ---
p8_prv <- make_cumul_dir_split(df_net_prv, "By Private Subtype")
p8_pub <- make_cumul_dir_split(df_net_pub, "By Public Subtype")
fig8 <- (p8_prv / p8_pub) + 
  plot_annotation(title = "Cumulative Actual Directional Road Network Distance between\nFacility and Nearest Transit by Sector Subtypes", tag_levels = "A", theme = global_theme) & legend_override  # <--- THIS WAS MISSING
agg_tiff("Fig8_Cumulative_Net_Subtypes.tiff", width=22, height=20, units="in", res=300, compression="lzw", background="white"); print(fig8); dev.off()

cat("\n✅ Figures 5-8 Generated with updated Legend Sizes and Merged Titles.\n")

#########################################################
#########################################################
# Section 16: LOAD, COMBINE & ENRICH SAVED Bus ROUTE DATA
#########################################################
#########################################################

library(dplyr)
library(sf)

# --- 1. SETUP: FIND SAVED FILES ---
# Scans your current folder for any files ending in _Full_Spatial_Data.rds
rds_files <- list.files(pattern = "Route_.*_Full_Spatial_Data\\.rds$", full.names = TRUE)

if(length(rds_files) == 0) {
  stop("❌ No .rds files found! Check your working directory (getwd()).")
}

message(paste("📂 Found", length(rds_files), "route files. Loading..."))

# --- 2. EXTRACT RAW DATA FROM FILES ---
# This function pulls the distance and status from the saved list
extract_route_table <- function(file_path) {
  
  # Load the file
  seg_list <- readRDS(file_path)
  
  # Clean the filename to get the Route ID (e.g., "7_1")
  clean_name <- gsub("Route_|_Full_Spatial_Data\\.rds", "", basename(file_path))
  
  # Create a temporary dataframe
  route_df <- data.frame()
  
  for(i in 1:length(seg_list)) {
    item <- seg_list[[i]]
    res  <- item$res
    
    # Safely extract distance (handle missing values or different list structures)
    dist_val <- NA
    if(!is.null(res$dist_total_m)) {
      dist_val <- round(res$dist_total_m, 2)
    } else if(!is.null(res$net_route)) {
      dist_val <- round(as.numeric(st_length(res$net_route)), 2)
    }
    
    # Determine status
    status <- if(!is.na(dist_val)) "✅ OK" else "❌ FAIL"
    
    # Bind to dataframe (ONLY ID, Seq, Distance, Status for now)
    route_df <- rbind(route_df, data.frame(
      Route_ID    = clean_name,
      Segment_Seq = i,
      Distance_m  = dist_val,
      Status      = status,
      stringsAsFactors = FALSE
    ))
  }
  return(route_df)
}

# Run the extraction on all files found
raw_combined_data <- data.frame()
for(f in rds_files) {
  raw_combined_data <- rbind(raw_combined_data, extract_route_table(f))
}

message("✅ Raw data loaded. Now reconstructing metadata...")

# --- 3. RECONSTRUCT METADATA (Stop Names/Codes) ---

# A. Create Metadata Lookup Table
meta_list <- list()

# Helper to match IDs (e.g. "7 | 1" -> "7_1")
get_clean_id <- function(id) {
  x <- gsub(" \\| ", "_", id)
  gsub("[^A-Za-z0-9_-]", "", x)
}

# Iterate through master_route_codes to build the "Correct" sequence
for(raw_id in names(master_route_codes)) {
  
  codes <- master_route_codes[[raw_id]]
  clean_id <- get_clean_id(raw_id)
  
  # If route has stops, create sequence pairs
  if(length(codes) > 1) {
    df <- data.frame(
      Route_ID    = clean_id,
      Segment_Seq = 1:(length(codes)-1),
      From_Code   = codes[1:(length(codes)-1)],
      To_Code     = codes[2:length(codes)],
      stringsAsFactors = FALSE
    )
    meta_list[[clean_id]] <- df
  }
}
route_metadata <- do.call(rbind, meta_list)

# B. Get Stop Names from 'bus_proj'
# We create a simple lookup table (Code -> Name)
stops_lookup <- bus_proj %>%
  st_drop_geometry() %>%
  select(busstopcode, busstopname) %>%
  distinct(busstopcode, .keep_all = TRUE)

# C. Attach Names to Metadata
route_metadata <- route_metadata %>%
  left_join(stops_lookup, by = c("From_Code" = "busstopcode")) %>%
  rename(From_Name = busstopname) %>%
  left_join(stops_lookup, by = c("To_Code" = "busstopcode")) %>%
  rename(To_Name = busstopname)

# --- 4. FINAL MERGE & SAVE ---
# Join the calculated results (Distance) with the metadata (Names/Codes)
final_dataset <- raw_combined_data %>%
  left_join(route_metadata, by = c("Route_ID", "Segment_Seq")) %>%
  select(Route_ID, Segment_Seq, From_Name, To_Name, Distance_m, Status, From_Code, To_Code)

message("✅ COMPLETE! Final dataset created.")
print(head(final_dataset))

# Save to CSV
write.csv(final_dataset, "Final_Routes_Processed.csv", row.names = FALSE)
message("💾 Saved as 'Final_Routes_Processed.csv'")

######################################################################
######################################################################
# Section 17: EXTRACT, FIX, REBUILD, AND VISUALIZE Bus Routes
# From RDS files → Gap-free routes → Final figures with classification
######################################################################
######################################################################

library(dplyr)
library(sf)
library(tibble)
library(ggplot2)
library(ggnewscale)
library(ragg)
library(ggspatial)

# ==============================================================================
# PART 1: EXTRACT SPATIAL DATA FROM RDS FILES
# ==============================================================================

message("🗺️ Extracting spatial data from RDS files...")

# --- 1. SETUP ---
rds_files <- list.files(pattern = "Route_.*_Full_Spatial_Data\\.rds$", full.names = TRUE)
message(paste("📂 Found", length(rds_files), "route files."))

# --- 2. EXTRACTION FUNCTION ---
extract_spatial_data <- function(file_path) {
  
  seg_list <- readRDS(file_path)
  clean_name <- gsub("Route_|_Full_Spatial_Data\\.rds", "", basename(file_path))
  
  # Parse route number and direction
  parts <- strsplit(clean_name, "_")[[1]]
  route_num <- parts[1]
  direction <- if(length(parts) > 1) parts[2] else "1"
  
  # Storage
  segment_lines <- list()
  snapped_points <- list()
  
  for(i in seq_along(seg_list)) {
    item <- seg_list[[i]]
    res <- item$res
    
    # --- Extract Line Geometry ---
    if(!is.null(res$net_route) && inherits(res$net_route, "sf")) {
      seg_geom <- st_union(res$net_route)
      
      segment_lines[[i]] <- tibble(
        route_id = clean_name,
        route_num = route_num,
        direction = direction,
        segment_seq = i,
        geometry = seg_geom
      )
    }
    
    # --- Extract Snapped Points from proj_from and proj_to ---
    if(!is.null(res$proj_from) && inherits(res$proj_from, "sf")) {
      snapped_points[[paste0(i, "_from")]] <- tibble(
        route_id = clean_name,
        route_num = route_num,
        direction = direction,
        segment_seq = i,
        point_type = "from",
        stop_code = if(!is.null(item$start_m)) item$start_m$busstopcode[1] else NA_character_,
        stop_name = if(!is.null(item$start_m)) item$start_m$busstopname[1] else NA_character_,
        geometry = st_geometry(res$proj_from)[1]
      )
    }
    
    if(!is.null(res$proj_to) && inherits(res$proj_to, "sf")) {
      snapped_points[[paste0(i, "_to")]] <- tibble(
        route_id = clean_name,
        route_num = route_num,
        direction = direction,
        segment_seq = i,
        point_type = "to",
        stop_code = if(!is.null(item$end_m)) item$end_m$busstopcode[1] else NA_character_,
        stop_name = if(!is.null(item$end_m)) item$end_m$busstopname[1] else NA_character_,
        geometry = st_geometry(res$proj_to)[1]
      )
    }
  }
  
  list(
    lines = segment_lines,
    points = snapped_points
  )
}

# --- 3. PROCESS ALL FILES ---
all_lines <- list()
all_points <- list()

for(f in rds_files) {
  tryCatch({
    result <- extract_spatial_data(f)
    all_lines <- c(all_lines, result$lines)
    all_points <- c(all_points, result$points)
  }, error = function(e) {
    message(paste("⚠️ Error in", basename(f), ":", e$message))
  })
}

message(paste("✅ Extracted", length(all_lines), "line segments"))
message(paste("✅ Extracted", length(all_points), "snapped points"))

# --- 4. COMBINE INTO SF OBJECTS ---

# A. Bus Route Segments
bus_route_segments_sf <- bind_rows(all_lines) %>%
  st_as_sf(crs = 32638)  # Original CRS is UTM 32638

# B. Snapped Bus Stops (unique points only)
bus_stops_network_sf <- bind_rows(all_points) %>%
  st_as_sf(crs = 32638) %>%
  distinct(stop_code, .keep_all = TRUE)

message("✅ Spatial objects created:")
message(paste("   • bus_route_segments_sf:", nrow(bus_route_segments_sf), "segments"))
message(paste("   • bus_stops_network_sf:", nrow(bus_stops_network_sf), "unique stops"))

# ==============================================================================
# PART 2: FIX SEGMENT GAPS
# ==============================================================================

message("\n🔧 Fixing gaps between segments...")

fix_segment_gaps_for_route <- function(route_id_val, all_segments) {
  
  segments_df <- all_segments %>%
    filter(route_id == route_id_val) %>%
    arrange(segment_seq)
  
  if(nrow(segments_df) <= 1) return(segments_df)
  
  fixed_geoms <- list()
  fixed_geoms[[1]] <- segments_df$geometry[[1]]
  
  for(i in 2:nrow(segments_df)) {
    # Get end point of previous segment
    prev_coords <- st_coordinates(fixed_geoms[[i-1]])
    prev_end <- prev_coords[nrow(prev_coords), 1:2]
    
    # Get current segment coordinates
    curr_coords <- st_coordinates(segments_df$geometry[[i]])
    
    # Replace first point with previous segment's end point
    curr_coords[1, 1:2] <- prev_end
    
    # Create new linestring
    fixed_geoms[[i]] <- st_linestring(curr_coords[, 1:2])
  }
  
  # Create new sf object
  segments_df$geometry <- st_sfc(fixed_geoms, crs = st_crs(all_segments))
  
  return(segments_df)
}

# Apply to all routes
unique_routes <- unique(bus_route_segments_sf$route_id)
fixed_list <- list()

for(rid in unique_routes) {
  tryCatch({
    fixed_list[[rid]] <- fix_segment_gaps_for_route(rid, bus_route_segments_sf)
  }, error = function(e) {
    message(paste("⚠️ Error fixing route", rid, ":", e$message))
    # Keep original if fix fails
    fixed_list[[rid]] <- bus_route_segments_sf %>% filter(route_id == rid)
  })
}

bus_route_segments_sf <- bind_rows(fixed_list) %>% st_as_sf()

message(paste("✅ Fixed", length(unique_routes), "routes"))

# Verify fix
all_gaps_check <- list()
for(rid in unique(bus_route_segments_sf$route_id)[1:10]) {
  segs <- bus_route_segments_sf %>% filter(route_id == rid) %>% arrange(segment_seq)
  if(nrow(segs) > 1) {
    for(i in 1:(nrow(segs)-1)) {
      curr_coords <- st_coordinates(segs$geometry[[i]])
      next_coords <- st_coordinates(segs$geometry[[i+1]])
      dist <- sqrt((curr_coords[nrow(curr_coords),1] - next_coords[1,1])^2 + 
                     (curr_coords[nrow(curr_coords),2] - next_coords[1,2])^2)
      if(dist > 0.1) {
        all_gaps_check[[length(all_gaps_check)+1]] <- data.frame(route = rid, gap = dist)
      }
    }
  }
}
message(paste("   Remaining gaps in sample:", length(all_gaps_check)))

# ==============================================================================
# PART 3: REBUILD COMPLETE ROUTES FROM FIXED SEGMENTS
# ==============================================================================

message("\n🔨 Rebuilding complete routes from fixed segments...")

# Define loop routes based on documentation
doc_loop_routes <- c("910", "912", "913", "914", "915", "920", "921", "922", "923", "924", 
                     "925", "926", "927", "930", "931", "932", "933", "934", "935", "937", 
                     "938", "940", "942", "943", "944", "945", "946", "947", "948", "951", 
                     "952", "953", "954", "956", "957", "960", "961", "962", "970", "971", 
                     "973", "974", "975", "980", "981", "982", "983", "984", "990")

# Build route line function
build_route_line <- function(segments_df, close_loop = FALSE) {
  
  segments_df <- segments_df %>% arrange(segment_seq)
  all_coords <- list()
  
  for(i in 1:nrow(segments_df)) {
    coords <- st_coordinates(segments_df$geometry[[i]])
    
    if(i == 1) {
      # First segment - add all coordinates
      all_coords[[i]] <- coords[, 1:2]
    } else {
      # Skip first point to avoid duplicates (segments are now connected)
      if(nrow(coords) > 1) {
        all_coords[[i]] <- coords[-1, 1:2, drop = FALSE]
      }
    }
  }
  
  combined_coords <- do.call(rbind, all_coords)
  
  # Close loop if needed
  if(close_loop) {
    combined_coords <- rbind(combined_coords, combined_coords[1, , drop = FALSE])
  }
  
  st_linestring(combined_coords)
}

# Rebuild all routes
routes_list <- list()

for(rid in unique(bus_route_segments_sf$route_id)) {
  
  segments <- bus_route_segments_sf %>% 
    filter(route_id == rid) %>%
    arrange(segment_seq)
  
  route_num <- segments$route_num[1]
  is_loop <- route_num %in% doc_loop_routes
  
  tryCatch({
    route_line <- build_route_line(segments, close_loop = is_loop)
    
    routes_list[[rid]] <- tibble(
      route_id = rid,
      route_num = route_num,
      direction = segments$direction[1],
      n_segments = nrow(segments),
      is_loop = is_loop,
      geometry = st_sfc(route_line, crs = st_crs(bus_route_segments_sf))
    )
  }, error = function(e) {
    message(paste("⚠️ Error building route", rid, ":", e$message))
  })
}

bus_routes_rebuilt <- bind_rows(routes_list) %>%
  st_as_sf()

message(paste("✅ Rebuilt", nrow(bus_routes_rebuilt), "complete routes"))

# ==============================================================================
# PART 4: TRANSFORM CRS TO MATCH OTHER LAYERS
# ==============================================================================

message("\n🔄 Transforming CRS to EPSG:4326...")

bus_routes_rebuilt <- st_transform(bus_routes_rebuilt, crs = 4326)
bus_stops_network_sf <- st_transform(bus_stops_network_sf, crs = 4326)

message(paste("   • bus_routes_rebuilt CRS:", st_crs(bus_routes_rebuilt)$epsg))
message(paste("   • bus_stops_network_sf CRS:", st_crs(bus_stops_network_sf)$epsg))

# ==============================================================================
# PART 5: ENSURE ALL LAYERS HAVE CONSISTENT CRS
# ==============================================================================

message("\n🗺️ Preparing riyadh_regions and ensuring CRS consistency...")

# Create riyadh_regions from riyadh_merged_2
riyadh_regions <- riyadh_merged_2 %>%
  sf::st_make_valid() %>%
  dplyr::group_by(new_region) %>%
  dplyr::summarise(do_union = TRUE, .groups = "drop")

message("\n🗺️ Ensuring CRS consistency across all layers...")

master_crs <- st_crs(riyadh_regions)

riyadh_merged_2      <- st_transform(riyadh_merged_2, master_crs)
riyadh_regions       <- st_transform(riyadh_regions, master_crs)
bus_routes_rebuilt   <- st_transform(bus_routes_rebuilt, master_crs)
bus_stops_network_sf <- st_transform(bus_stops_network_sf, master_crs)
metro_lines          <- st_transform(metro_lines, master_crs)
stations             <- st_transform(stations, master_crs)

message("✅ All layers transformed to master CRS")

# ==============================================================================
# PART 6: CLASSIFY BUS ROUTES
# ==============================================================================

message("\n🏷️ Classifying bus routes...")

bus_routes_rebuilt <- bus_routes_rebuilt %>%
  mutate(route_type = ifelse(route_id %in% c("11_1", "12_1", "13_1", "11_2", "12_2", "13_2"), 
                             "Rapid Transit", 
                             "Standard"))

bus_stops_network_sf <- bus_stops_network_sf %>%
  mutate(route_type = ifelse(route_id %in% c("11_1", "12_1", "13_1", "11_2", "12_2", "13_2"), 
                             "Rapid Transit", 
                             "Standard"))

n_rapid <- sum(bus_routes_rebuilt$route_type == "Rapid Transit")
n_standard <- sum(bus_routes_rebuilt$route_type == "Standard")

message(paste("   • Rapid Transit:", n_rapid))
message(paste("   • Standard:", n_standard))

# ==============================================================================
# PART 7: CREATE FIGURE 1 - BUS NETWORK + METRO
# ==============================================================================

message("\n🎨 Creating Figure 1: Bus Network + Metro...")

# Create dummy data for metro legend
center_x <- mean(st_coordinates(bus_stops_network_sf)[,1], na.rm = TRUE)
center_y <- mean(st_coordinates(bus_stops_network_sf)[,2], na.rm = TRUE)

metro_dummy <- data.frame(
  metroline = rep(c("Line1", "Line2", "Line3", "Line4", "Line5", "Line6"), each = 2),
  x_dummy = center_x, 
  y_dummy = center_y
)

p_fig1 <- ggplot() +
  
  # --- Base Map ---
  geom_sf(data = riyadh_merged_2, fill = "white", color = "black", linewidth = 0.1) +
  geom_sf(data = riyadh_regions, fill = NA, color = "black", linewidth = 0.6) +
  
  # --- Bus Network (Classified) ---
  geom_sf(data = bus_routes_rebuilt, aes(color = route_type, linewidth = route_type), 
          alpha = 0.7, show.legend = FALSE) +
  geom_sf(data = bus_stops_network_sf, aes(color = route_type, size = route_type), 
          shape = 16, alpha = 0.6, show.legend = FALSE) +
  
  # Dummy layers for combined bus legend (lines + points)
  geom_line(data = data.frame(x = c(center_x, center_x + 0.01), y = c(center_y, center_y),
                              route_type = c("Rapid Transit", "Standard")),
            aes(x = x, y = y, color = route_type, linewidth = route_type), alpha = 0) +
  geom_point(data = data.frame(x = center_x, y = center_y, 
                               route_type = c("Rapid Transit", "Standard")),
             aes(x = x, y = y, color = route_type, size = route_type), 
             shape = 16, alpha = 0) +
  
  scale_color_manual(
    name = "Bus Route",
    values = c("Rapid Transit" = "#8B4513", "Standard" = "#006064"),
    guide = guide_legend(
      order = 2, title.position = "top",
      override.aes = list(
        alpha = 1, 
        linewidth = c(2, 1), 
        size = c(8, 6),
        shape = 16,
        linetype = "solid"
      )
    )
  ) +
  
  scale_linewidth_manual(
    values = c("Rapid Transit" = 0.9, "Standard" = 0.5),
    guide = "none"
  ) +
  
  scale_size_manual(
    values = c("Rapid Transit" = 1.5, "Standard" = 0.5),
    guide = "none"
  ) +
  
  # --- Metro Network ---
  ggnewscale::new_scale_color() +
  
  geom_sf(data = metro_lines, aes(color = as.character(metroline)), 
          linewidth = 1, alpha = 0.8, show.legend = FALSE) +
  geom_sf(data = stations, aes(color = as.character(metroline)), 
          size = 3, shape = 16, alpha = 1, show.legend = FALSE) +
  
  geom_line(data = metro_dummy, aes(x = x_dummy, y = y_dummy, color = as.character(metroline)),
            alpha = 0, inherit.aes = FALSE) +
  geom_point(data = metro_dummy, aes(x = x_dummy, y = y_dummy, color = as.character(metroline)),
             alpha = 0, inherit.aes = FALSE) +
  
  scale_color_manual(
    name = "Metro Line",
    values = c("Line1"="#00ADE5", "Line2"="#F0493A", "Line3"="#F68D39", 
               "Line4"="#FFD105", "Line5"="#43B649", "Line6"="#984C9D"),
    labels = c("1","2","3","4","5","6"),
    guide = guide_legend(
      order = 1, nrow = 1, title.position = "top",
      override.aes = list(alpha = 1, linewidth = 2, size = 8,
                          shape = 16, linetype = "solid")
    )
  ) +
  
  # --- Annotations ---
  annotation_scale(location = "bl", width_hint = 0.2, style = "ticks", text_cex = 1.2,
                   pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         height = unit(1.2, "cm"), width = unit(1.2, "cm"),
                         pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm"),
                         style = north_arrow_fancy_orienteering) +
  
  coord_sf(expand = FALSE) +
  
  # --- Theme ---
  theme_minimal(base_size = 18, base_family = "serif") +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border     = element_blank(),
    panel.grid       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    axis.title       = element_blank(),
    plot.title       = element_text(size = 32, face = "bold", hjust = 0.5, margin = margin(b = 20)),
    legend.position  = "bottom",
    legend.box       = "horizontal",
    legend.box.just  = "top",
    legend.spacing.x = unit(0.5, "cm"),
    legend.key       = element_blank(),
    legend.background = element_rect(fill = "white", color = NA),
    legend.title     = element_text(size = 22, face = "bold"),
    legend.text      = element_text(size = 18),
    legend.key.size  = unit(1.2, "cm"),
    plot.margin      = margin(10, 10, 10, 10)
  ) +
  labs(title = "Riyadh Public Transit Network: Metro Lines & Bus Routes")

p_fig1

# Save Figure 1 as TIFF
ggsave(
  filename = "Figure1_Bus_Metro_Network.tiff",
  plot = p_fig1,
  width = 24, 
  height = 14, 
  units = "in", 
  dpi = 300,
  compression = "lzw"
)

# Save Figure 1
ggsave(
  filename = "Figure1_Bus_Metro_Network.png",
  plot = p_fig1,
  width = 24, 
  height = 14, 
  units = "in", 
  dpi = 300
)

message("✅ Saved: Figure1_Bus_Metro_Network.png")

# ==============================================================================
# PART 8: CREATE FIGURE 2 - BUS + METRO + CLINIC DENSITY
# ==============================================================================

message("\n🎨 Creating Figure 2: Bus + Metro + Clinic Density...")

# Prepare clinic density data
clinics_density_data <- gisdata %>% 
  filter(with_dental_services == "Yes") %>%
  st_transform(master_crs) %>%
  mutate(
    X = st_coordinates(.)[,1],
    Y = st_coordinates(.)[,2]
  )

# Create facet labels
facet_labels <- clinics_density_data %>% 
  st_drop_geometry() %>%
  dplyr::count(private_or_public) %>%
  mutate(label = paste0(private_or_public, " (n = ", n, ")")) %>%
  { setNames(.$label, .$private_or_public) }

p_fig2 <- ggplot() +
  
  # --- Base Map ---
  geom_sf(data = riyadh_merged_2, fill = NA, color = "black", linewidth = 0.2) +
  geom_sf(data = riyadh_regions, fill = NA, color = "black", linewidth = 0.8) +
  
  # --- Density Layer ---
  stat_density_2d(
    data = clinics_density_data,
    aes(x = X, y = Y, fill = after_stat(level), alpha = after_stat(level)),
    geom = "polygon", bins = 20, adjust = 1.2,
    color = "black", linewidth = 0.1    
  ) +
  scale_fill_viridis_c(
    option = "mako", direction = 1, name = "Facility Density",
    guide = guide_colorbar(
      order = 1, direction = "horizontal",
      barwidth = unit(8, "cm"), barheight = unit(0.6, "cm"),
      title.position = "top", title.hjust = 0.5
    )
  ) +
  scale_alpha_continuous(range = c(0.05, 0.7), guide = "none") +
  
  # --- Bus Network ---
  ggnewscale::new_scale_color() +
  
  geom_sf(data = bus_routes_rebuilt, aes(color = route_type, linewidth = route_type), 
          alpha = 0.7, show.legend = FALSE) +
  geom_sf(data = bus_stops_network_sf, aes(color = route_type, size = route_type), 
          shape = 16, alpha = 0.6, show.legend = FALSE) +
  
  geom_line(data = data.frame(x = c(center_x, center_x + 0.01), y = c(center_y, center_y),
                              route_type = c("Rapid Transit", "Standard")),
            aes(x = x, y = y, color = route_type, linewidth = route_type), alpha = 0) +
  geom_point(data = data.frame(x = center_x, y = center_y, 
                               route_type = c("Rapid Transit", "Standard")),
             aes(x = x, y = y, color = route_type, size = route_type), 
             shape = 16, alpha = 0) +
  
  scale_color_manual(
    name = "Bus Route",
    values = c("Rapid Transit" = "#8B4513", "Standard" = "#006064"),
    guide = guide_legend(
      order = 3, title.position = "top",
      override.aes = list(
        alpha = 1, 
        linewidth = c(2, 1),
        size = c(8, 6),
        shape = 16,
        linetype = "solid"
      )
    )
  ) +
  
  scale_linewidth_manual(
    values = c("Rapid Transit" = 0.9, "Standard" = 0.5),
    guide = "none"
  ) +
  
  scale_size_manual(
    values = c("Rapid Transit" = 1.5, "Standard" = 0.5),
    guide = "none"
  ) +
  
  # --- Metro Network ---
  ggnewscale::new_scale_color() +
  
  geom_sf(data = metro_lines, aes(color = as.character(metroline)), 
          linewidth = 1, alpha = 0.8, show.legend = FALSE) +
  geom_sf(data = stations, aes(color = as.character(metroline)), 
          size = 3, shape = 16, alpha = 1, show.legend = FALSE) +
  
  geom_line(data = metro_dummy, aes(x = x_dummy, y = y_dummy, color = as.character(metroline)),
            alpha = 0, inherit.aes = FALSE) +
  geom_point(data = metro_dummy, aes(x = x_dummy, y = y_dummy, color = as.character(metroline)),
             alpha = 0, inherit.aes = FALSE) +
  
  scale_color_manual(
    name = "Metro Line",
    values = c("Line1"="#00ADE5", "Line2"="#F0493A", "Line3"="#F68D39", 
               "Line4"="#FFD105", "Line5"="#43B649", "Line6"="#984C9D"),
    labels = c("1","2","3","4","5","6"),
    guide = guide_legend(
      order = 2, nrow = 1, title.position = "top",
      override.aes = list(alpha = 1, linewidth = 2, size = 8, shape = 16)
    )
  ) +
  
  # --- Facet & Annotations ---
  facet_wrap(~ private_or_public, labeller = labeller(private_or_public = facet_labels)) +
  
  annotation_scale(location = "bl", width_hint = 0.2, style = "ticks", text_cex = 1.5,
                   pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         height = unit(1.2, "cm"), width = unit(1.2, "cm"),
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_fancy_orienteering) +
  
  coord_sf(expand = FALSE) +
  
  # --- Theme ---
  theme_minimal(base_size = 18, base_family = "serif") +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border     = element_blank(),
    panel.grid       = element_blank(),
    panel.spacing    = unit(2, "cm"),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    axis.title       = element_blank(),
    strip.text       = element_text(size = 30, face = "bold", margin = margin(b = 10)),
    strip.background = element_rect(fill = "white", color = NA),
    plot.title       = element_text(size = 36, face = "bold", hjust = 0.5, margin = margin(b = 20)),
    legend.position  = "bottom",
    legend.box       = "horizontal",
    legend.box.just  = "top",
    legend.spacing.x = unit(0.5, "cm"),
    legend.key       = element_blank(),
    legend.background = element_rect(fill = "white", color = NA),
    legend.title     = element_text(size = 24, face = "bold"),
    legend.text      = element_text(size = 20),
    legend.key.size  = unit(1.5, "cm"),
    plot.margin      = margin(10, 10, 10, 10)
  ) +
  labs(title = "Dental Facility Density in Relation to Public Transit Network")

p_fig2

# Save Figure 1 as TIFF
ggsave(
  filename = "Figure2_Bus_Metro_Clinic_Density.tiff",
  plot = p_fig2,
  width = 24, 
  height = 14, 
  units = "in", 
  dpi = 300,
  compression = "lzw"
)

# Save Figure 2
ggsave(
  filename = "Figure2_Bus_Metro_Clinic_Density.png",
  plot = p_fig2,
  width = 24, 
  height = 14, 
  units = "in", 
  dpi = 300
)

message("✅ Saved: Figure2_Bus_Metro_Clinic_Density.png")

# ==============================================================================
# PART 9: CREATE FIGURE 3 Density with transit and random points by design
# and sector 
# ==============================================================================

library(ggplot2)
library(sf)
library(dplyr)
library(ggnewscale)
library(patchwork)

message("\n🎨 Creating Figure 3: 2x2 Panel (Aligned Legends)...")

# ------------------------------------------------------------------------------
# 1. LOAD DATA
# ------------------------------------------------------------------------------
raw_design_A <- readRDS("random_points_geo_vs_network_EDGE_METHOD_populated_only.rds")
raw_design_B <- readRDS("random_points_geo_vs_network_EDGE_METHOD_pop_weighted_NO_LCC.rds")
rp_design_A <- raw_design_A$pts
rp_design_B <- raw_design_B$pts

clinics_density_data <- gisdata %>% 
  filter(with_dental_services == "Yes") %>%
  st_transform(master_crs) %>%
  mutate(X = st_coordinates(.)[,1], Y = st_coordinates(.)[,2])

# ------------------------------------------------------------------------------
# 2. CALCULATE GLOBAL DENSITY LIMITS
# ------------------------------------------------------------------------------
gg_common <- ggplot(clinics_density_data, aes(x = X, y = Y)) +
  stat_density_2d(geom = "raster", contour = FALSE, n = 100, adjust = 1.2)
plot_build <- ggplot_build(gg_common)
density_values <- plot_build$data[[1]]$density
global_limits <- c(min(density_values), max(density_values))

# ------------------------------------------------------------------------------
# 3. DEFINE PLOTTING FUNCTION
# ------------------------------------------------------------------------------
center_x <- 46.7
center_y <- 24.7

# --- Dummy Data for Legends ---
metro_dummy <- data.frame(
  x_dummy = center_x, 
  y_dummy = center_y, 
  metroline = unique(metro_lines$metroline) 
)

bus_dummy <- data.frame(
  x = c(center_x, center_x),
  y = c(center_y, center_y),
  route_type = c("Rapid Transit", "Standard")
)

create_panel <- function(sector_name, rp_data, plot_title, density_limits) {
  
  panel_data <- clinics_density_data %>% filter(private_or_public == sector_name)
  n_count <- nrow(panel_data)
  facet_lab <- paste0(sector_name, " (n = ", n_count, ")")
  names(facet_lab) <- sector_name 
  
  p <- ggplot() +
    # --- Base Map ---
    geom_sf(data = riyadh_merged_2, fill = NA, color = "black", linewidth = 0.2) +
    geom_sf(data = riyadh_regions, fill = NA, color = "black", linewidth = 0.8) +
    
    # --- Density Layer ---
    stat_density_2d(
      data = panel_data,
      aes(x = X, y = Y, fill = after_stat(level), alpha = after_stat(level)),
      geom = "polygon", bins = 20, adjust = 1.2,
      color = "black", linewidth = 0.1    
    ) +
    scale_fill_viridis_c(
      option = "mako", direction = 1, name = "Facility Density",
      limits = density_limits,
      guide = guide_colorbar(
        order = 1, direction = "horizontal",
        barwidth = unit(10, "cm"), 
        barheight = unit(0.8, "cm"),
        title.position = "top", title.hjust = 0.5
      )
    ) +
    scale_alpha_continuous(range = c(0.05, 0.7), guide = "none") +
    
    # --- Bus Network ---
    ggnewscale::new_scale_color() +
    
    # Actual Data
    geom_sf(data = bus_routes_rebuilt, aes(color = route_type, linewidth = route_type), 
            alpha = 0.7, show.legend = FALSE) +
    geom_sf(data = bus_stops_network_sf, aes(color = route_type, size = route_type), 
            shape = 16, alpha = 0.6, show.legend = FALSE) +
    
    # DUMMY LEGEND LAYERS (Line + Point)
    geom_line(data = bus_dummy, aes(x = x, y = y, color = route_type, linewidth = route_type), 
              alpha = 0) +
    geom_point(data = bus_dummy, aes(x = x, y = y, color = route_type, size = route_type), 
               alpha = 0) +
    
    scale_color_manual(
      name = "Bus Route",
      values = c("Rapid Transit" = "#8B4513", "Standard" = "#006064"),
      guide = guide_legend(
        order = 3, title.position = "top",
        override.aes = list(
          alpha = 1, 
          linewidth = c(2, 1.2), 
          size = c(6, 4),        
          shape = 16,            
          linetype = "solid"     
        )
      )
    ) +
    scale_linewidth_manual(values = c("Rapid Transit" = 0.9, "Standard" = 0.5), guide = "none") +
    scale_size_manual(values = c("Rapid Transit" = 1.0, "Standard" = 0.5), guide = "none") +
    
    # --- Random Starting Points ---
    ggnewscale::new_scale_color() +
    geom_sf(data = rp_data, aes(color = "Random Point"), 
            shape = 17, size = 1.0, alpha = 0.4, show.legend = FALSE) +
    geom_point(data = data.frame(x = center_x, y = center_y, label = "Random Point"),
               aes(x = x, y = y, color = label), shape = 17, size = 4, alpha = 0) +
    
    scale_color_manual(
      # [UPDATED] Use a space " " as title to force alignment with other titled legends
      name = " ", 
      values = c("Random Point" = "grey30"),
      guide = guide_legend(
        order = 4, title.position = "top",
        override.aes = list(alpha = 1, size = 6, shape = 17, color = "grey30")
      )
    ) +
    
    # --- Metro Network ---
    ggnewscale::new_scale_color() +
    
    # Actual Data
    geom_sf(data = metro_lines, aes(color = as.character(metroline)), 
            linewidth = 1, alpha = 0.8, show.legend = FALSE) +
    geom_sf(data = stations, aes(color = as.character(metroline)), 
            size = 2.5, shape = 16, alpha = 1, show.legend = FALSE) +
    
    # DUMMY LEGEND LAYERS (Line + Point)
    geom_line(data = metro_dummy, aes(x = x_dummy, y = y_dummy, color = as.character(metroline)),
              alpha = 0, inherit.aes = FALSE) +
    geom_point(data = metro_dummy, aes(x = x_dummy, y = y_dummy, color = as.character(metroline)),
               alpha = 0, inherit.aes = FALSE) +
    
    scale_color_manual(
      name = "Metro Line",
      values = c("Line1"="#00ADE5", "Line2"="#F0493A", "Line3"="#F68D39", 
                 "Line4"="#FFD105", "Line5"="#43B649", "Line6"="#984C9D"),
      labels = c("1","2","3","4","5","6"),
      guide = guide_legend(
        order = 2, nrow = 1, title.position = "top",
        override.aes = list(
          alpha = 1, 
          linewidth = 1.5,   
          size = 5,          
          shape = 16,        
          linetype = "solid" 
        )
      )
    ) +
    
    # --- Facet & Layout ---
    facet_wrap(~ private_or_public, labeller = labeller(private_or_public = facet_lab)) +
    annotation_scale(location = "bl", width_hint = 0.15, style = "ticks", text_cex = 1.2,
                     pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm")) +
    coord_sf(expand = FALSE) +
    
    # --- Theme ---
    theme_minimal(base_size = 14, base_family = "serif") +
    theme(
      plot.title       = element_text(size = 24, face = "bold", hjust = 0.5, margin = margin(b = 10)),
      panel.border     = element_blank(),
      panel.grid       = element_blank(),
      strip.text       = element_text(size = 24, face = "bold", margin = margin(b = 5)),
      strip.background = element_rect(fill = "white", color = NA),
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      axis.title       = element_blank(),
      legend.position  = "bottom"
    ) +
    labs(title = plot_title)
  
  return(p)
}

# ------------------------------------------------------------------------------
# 4. GENERATE PANELS
# ------------------------------------------------------------------------------
message("   Generating panels...")
p1 <- create_panel("Private", rp_design_A, "Design A: Uniform in Populated Areas", global_limits)
p2 <- create_panel("Private", rp_design_B, "Design B: Population Weighted", global_limits)
p3 <- create_panel("Public", rp_design_A, "", global_limits)
p4 <- create_panel("Public", rp_design_B, "", global_limits)

# ------------------------------------------------------------------------------
# 5. COMBINE WITH PATCHWORK
# ------------------------------------------------------------------------------
message("   Assembling 2x2 Grid...")
layout_2x2 <- (p1 + p2) / (p3 + p4) +
  plot_layout(guides = "collect") + 
  plot_annotation(
    title = "Dental Facility Density in Relation to Public Transit and Random Starting Points",
    subtitle = "Comparison of two random point simulation designs",
    theme = theme(
      plot.title = element_text(size = 36, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 26, hjust = 0.5, margin = margin(b = 20))
    )
  ) &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    
    # [UPDATED] Align legend boxes to the top to respect the title spacing
    legend.box.just = "top", 
    
    legend.spacing.x = unit(0.8, 'cm'),
    panel.border = element_blank(),
    legend.title = element_text(size = 22, face = "bold"),
    legend.text = element_text(size = 18),
    legend.key.size = unit(1.2, "cm")
  )

# ------------------------------------------------------------------------------
# 6. SAVE AS TIFF
# ------------------------------------------------------------------------------
ggsave(
  filename = "Figure3_2x2_Panel_Aligned.tiff", 
  plot = layout_2x2,
  width = 23, 
  height = 19, 
  units = "in", 
  dpi = 300,
  compression = "lzw"
)

message("✅ Saved: Figure3_2x2_Panel_Aligned.tiff")

# ==============================================================================
# SUMMARY
# ==============================================================================

message(paste0("\n", strrep("=", 80)))
message("=== COMPLETE WORKFLOW FINISHED ===")
message(strrep("=", 80))
message(paste("Total routes processed:", nrow(bus_routes_rebuilt)))
message(paste("  - Rapid Transit:", n_rapid))
message(paste("  - Standard Routes:", n_standard))
message(paste("Total unique stops:", nrow(bus_stops_network_sf)))
message("\nFigures created:")
message("  1. Figure1_Bus_Metro_Network.png")
message("  2. Figure2_Bus_Metro_Clinic_Density.png")
message("\n✅ All tasks completed successfully!")

#############################################################################
#############################################################################
# Section 18: MULTIMODAL TRANSIT ACCESSIBILITY (unweighted random points)
# Changes from original:
# 1. Use pre-defined bus sequences from bus_route_segments_sf
# 2. Segment distances = geometry length (post-fix)
# 3. Transfer definitions:
#    - Metro-metro: exact station name match (0 weight)
#    - Bus-bus exact: same stop name (0 weight)
#    - Bus-bus non-exact: <=50m walk network distance (actual walk as weight)
#    - Metro-bus: <=50m walk network distance (actual walk as weight)
# 4. Remove bus-only path, keep metro-only and multimodal
# 5. "On route" variable instead of "same direction/side"
# 6. Tie-breaking: Metro > Rapid Transit > Standard Bus
# 7. L1 and L3: Use pre-calculated road network distances from RDS files
# 8. Walk penalty ONLY for transfers (bus-bus non-exact, metro-bus)
#############################################################################
#############################################################################

# ==============================================================================
# 0) LIBRARIES + GLOBAL SETTINGS
# ==============================================================================

library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(igraph)
library(tibble)
library(dodgr)
library(purrr)

sf::sf_use_s2(FALSE)
set.seed(1234)

target_crs <- 32638

# Transfer parameters
walk_transfer_threshold_m <- 100 # For bus-bus non-exact and metro-bus

message(paste(rep("=", 60), collapse = ""))
message("SAMPLE TEST: 10 Random Points - Multimodal Accessibility")
message(paste(rep("=", 60), collapse = ""))

# ==============================================================================
# 1) VERIFY REQUIRED OBJECTS IN ENVIRONMENT
# ==============================================================================

message("\n[Checking] Checking required objects in environment...")

req_objects <- c(
  "bus_route_segments_sf",   # From first script (fixed segments)
  "bus_stops_network_sf",    # From first script (snapped stops)
  "riyadh_merged_2",         # Region polygons
  "gisdata",                 # Dental clinics
  "stations",                # Metro stations
  "metro_lines",             # Metro lines
  "bus"                      # Original bus data (for reference)
)

missing <- req_objects[!sapply(req_objects, exists)]
if(length(missing) > 0) {
  stop("Missing required objects: ", paste(missing, collapse = ", "))
}

message("All required objects found.")

# ==============================================================================
# 2) LOAD DATA FILES
# ==============================================================================

message("\n[Loading] Loading data files...")

# File paths
rp_rds <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data/random_points_geo_vs_network_EDGE_METHOD_populated_only.rds"
fac_rds <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data/clinics_geo_vs_network_ROBUST_DIRECTED.rds"
roads_fp <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data/roads_sf_riyadh_clipped_32638.rds"

stopifnot(file.exists(rp_rds), file.exists(fac_rds), file.exists(roads_fp))

res_rp <- readRDS(rp_rds)
res_fac <- readRDS(fac_rds)
roads_sf <- readRDS(roads_fp)

message("Data files loaded.")

# ==============================================================================
# 2B) EXTRACT PRE-CALCULATED L1 AND L3 DISTANCES
# ==============================================================================

message("\n[Extracting] Extracting pre-calculated L1 and L3 distances...")

# L1 distances: RP to nearest transit (from res_rp$comparison)
rp_comparison <- res_rp$comparison %>%
  mutate(id = as.character(id))

# Extract L1 distances by destination type
L1_metro_lookup <- rp_comparison %>%
  filter(dest_type == "nearest_metro") %>%
  select(id, geo_km, net_km_p2t) %>%
  rename(L1_metro_geo_km = geo_km, L1_metro_net_km = net_km_p2t)

L1_bus_lookup <- rp_comparison %>%
  filter(dest_type == "nearest_bus") %>%
  select(id, geo_km, net_km_p2t) %>%
  rename(L1_bus_geo_km = geo_km, L1_bus_net_km = net_km_p2t)

# Extract clinic targets for each RP (which clinic is nearest/median)
# dest_id_geo format: "priv_820" or "pub_123" -> need to extract "820" or "123"
clinic_targets <- rp_comparison %>%
  filter(dest_type %in% c("nearest_priv", "median_priv", "nearest_pub", "median_pub")) %>%
  select(id, dest_type, dest_id_geo) %>%
  mutate(
    # Extract numeric clinic ID by removing prefix
    clinic_id = gsub("^(priv_|pub_)", "", dest_id_geo)
  )

message(paste("   - Clinic target records:", nrow(clinic_targets)))

# L3 distances: Transit to clinic (from res_fac$results$comparison_all)
# IMPORTANT: Deduplicate - each clinic appears multiple times due to strata
fac_comparison <- res_fac$results$comparison_all %>%
  mutate(id = as.character(id))

# Extract L3 by transit type - deduplicate to one record per clinic
L3_metro_lookup <- fac_comparison %>%
  filter(type == "metro station") %>%
  distinct(id, .keep_all = TRUE) %>%  # One record per clinic
  select(id, geo_km, net_km_s2c) %>%
  rename(clinic_id = id, L3_metro_geo_km = geo_km, L3_metro_net_km = net_km_s2c)

L3_bus_lookup <- fac_comparison %>%
  filter(type == "bus stop") %>%
  distinct(id, .keep_all = TRUE) %>%  # One record per clinic
  select(id, geo_km, net_km_s2c) %>%
  rename(clinic_id = id, L3_bus_geo_km = geo_km, L3_bus_net_km = net_km_s2c)

message(paste("   - L1 metro records:", nrow(L1_metro_lookup)))
message(paste("   - L1 bus records:", nrow(L1_bus_lookup)))
message(paste("   - L3 metro records (unique clinics):", nrow(L3_metro_lookup)))
message(paste("   - L3 bus records (unique clinics):", nrow(L3_bus_lookup)))

# ==============================================================================
# 2C) CHECK MISSING NETWORK DISTANCES & CALCULATE IMPUTATION RATIOS
# ==============================================================================

message("\n[Checking] Checking missing network distances by destination type...")

# Get all destination types
all_dest_types <- c("nearest_bus", "nearest_metro", "nearest_priv", "median_priv", "nearest_pub", "median_pub")

# Calculate missing counts and imputation ratios for each destination type
imputation_info <- rp_comparison %>%
  group_by(dest_type) %>%
  summarise(
    n_total = n(),
    n_valid_p2t = sum(!is.na(net_km_p2t) & !is.infinite(net_km_p2t)),
    n_missing_p2t = n_total - n_valid_p2t,
    mean_geo_km = mean(geo_km, na.rm = TRUE),
    mean_net_p2t_km = mean(net_km_p2t, na.rm = TRUE),
    ratio_p2t = mean_net_p2t_km / mean_geo_km,
    .groups = "drop"
  )

message("\n   Missingness and Imputation Ratios (RP -> Destination):")
print(imputation_info)

# Create lookup for imputation ratios
ratio_lookup <- imputation_info %>%
  select(dest_type, ratio_p2t) %>%
  tibble::deframe()

# L3 distances: Check by transit type AND clinic ownership (private/public)
# Need to join with clinic ownership info
L3_with_ownership <- fac_comparison %>%
  distinct(id, type, .keep_all = TRUE) %>%
  mutate(
    ownership = case_when(
      label == "private" ~ "private",
      label == "public" ~ "public",
      TRUE ~ stratum  # fallback
    )
  )

L3_missing_info <- L3_with_ownership %>%
  group_by(type, ownership) %>%
  summarise(
    n_total = n(),
    n_valid_s2c = sum(!is.na(net_km_s2c) & !is.infinite(net_km_s2c)),
    n_missing_s2c = n_total - n_valid_s2c,
    mean_geo_km = mean(geo_km, na.rm = TRUE),
    mean_net_s2c_km = mean(net_km_s2c, na.rm = TRUE),
    ratio_s2c = mean_net_s2c_km / mean_geo_km,
    .groups = "drop"
  )

message("\n   Missingness and Imputation Ratios (Transit -> Clinic by ownership):")
print(L3_missing_info)

# Extract L3 ratios by transit type and ownership
ratio_L3_metro_priv <- L3_missing_info %>% 
  filter(type == "metro station", ownership == "private") %>% 
  pull(ratio_s2c)
ratio_L3_metro_pub <- L3_missing_info %>% 
  filter(type == "metro station", ownership == "public") %>% 
  pull(ratio_s2c)
ratio_L3_bus_priv <- L3_missing_info %>% 
  filter(type == "bus stop", ownership == "private") %>% 
  pull(ratio_s2c)
ratio_L3_bus_pub <- L3_missing_info %>% 
  filter(type == "bus stop", ownership == "public") %>% 
  pull(ratio_s2c)

# Fallback values
if(length(ratio_L3_metro_priv) == 0 || is.na(ratio_L3_metro_priv)) ratio_L3_metro_priv <- 1.3
if(length(ratio_L3_metro_pub) == 0 || is.na(ratio_L3_metro_pub)) ratio_L3_metro_pub <- 1.3
if(length(ratio_L3_bus_priv) == 0 || is.na(ratio_L3_bus_priv)) ratio_L3_bus_priv <- 1.3
if(length(ratio_L3_bus_pub) == 0 || is.na(ratio_L3_bus_pub)) ratio_L3_bus_pub <- 1.3

message(paste("\n   - L3 Metro -> Private clinic ratio:", round(ratio_L3_metro_priv, 3)))
message(paste("   - L3 Metro -> Public clinic ratio:", round(ratio_L3_metro_pub, 3)))
message(paste("   - L3 Bus -> Private clinic ratio:", round(ratio_L3_bus_priv, 3)))
message(paste("   - L3 Bus -> Public clinic ratio:", round(ratio_L3_bus_pub, 3)))

# ==============================================================================
# 3) PREPARE SPATIAL LAYERS (Project to target CRS)
# ==============================================================================

message("\n[Preparing] Preparing spatial layers...")

riyadh_proj <- st_transform(riyadh_merged_2, target_crs)
clinics_proj <- st_transform(gisdata, target_crs)
stations_proj <- st_transform(stations, target_crs)
roads_proj <- st_transform(roads_sf, target_crs)

# Bus data - use the pre-processed objects from first script
bus_segments_proj <- st_transform(bus_route_segments_sf, target_crs)
bus_stops_proj <- st_transform(bus_stops_network_sf, target_crs)

# IMPORTANT: Add unique bus_stop_id for graph construction
bus_stops_proj <- bus_stops_proj %>%
  mutate(bus_stop_id = row_number())

# Filter to only clinics WITH dental services (766 clinics)
clinics_dental <- clinics_proj %>% filter(with_dental_services == "Yes")

# Classify dental clinics by ownership
clinics_private <- clinics_dental %>% filter(private_or_public == "Private")
clinics_public <- clinics_dental %>% filter(private_or_public == "Public")

message(paste("   - Metro stations:", nrow(stations_proj)))
message(paste("   - Bus stops (network):", nrow(bus_stops_proj)))
message(paste("   - Bus segments:", nrow(bus_segments_proj)))
message(paste("   - Total clinics with dental services:", nrow(clinics_dental)))
message(paste("   - Private dental clinics:", nrow(clinics_private)))
message(paste("   - Public dental clinics:", nrow(clinics_public)))

# ... [End of Section 3] ...
message(paste("   - Public dental clinics:", nrow(clinics_public)))

# ==============================================================================
# 🚨 EMERGENCY PATCH: RESTORE DIRECTIONAL HUBS
# ==============================================================================

message("\n🚑 Patching bus data to restore DIRECTIONAL stops...")

if(exists("all_points")) {
  
  # 1. Rebuild from raw data
  bus_stops_network_sf <- bind_rows(all_points) %>%
    st_as_sf(crs = 32638) %>%
    # CRITICAL CHANGE: distinct by 'route_id' (not route_num)
    # This ensures we keep "Route 10 Outbound" AND "Route 10 Inbound"
    distinct(stop_code, route_id, .keep_all = TRUE) 
  
  # 2. Update the projected object
  target_crs <- 32638
  bus_stops_proj <- st_transform(bus_stops_network_sf, target_crs) %>%
    mutate(bus_stop_id = row_number()) 
  
  message(paste("✅ Bus data patched. Total stop instances:", nrow(bus_stops_proj)))
  
} else {
  stop("❌ 'all_points' is missing. You must re-run Part 1 extraction.")
}

# ==============================================================================
# 4) PREPARE ALL POINTS FOR COMPUTATION (Minimal Change)
# ==============================================================================

message("\n[Processing] Preparing ALL points for full computation...")

# Load all random points
rp_all <- res_rp$pts %>%
  st_transform(target_crs) %>%
  mutate(id = as.character(id))

# --- CHANGE START ---
# Instead of sampling 100, we use the entire dataset
rp_sample <- rp_all
# --- CHANGE END ---

message(paste("    - Total points to process:", nrow(rp_sample)))

# ==============================================================================
# 5) FIX SEGMENT GAPS & CALCULATE DISTANCES
# ==============================================================================

message("\n[Fixing] Fixing gaps and calculating corrected distances...")

# --- 5A. The Gap-Fixing Function ---
# This physically snaps the start of segment(i) to the end of segment(i-1)
fix_route_geometry <- function(all_segments) {
  unique_routes <- unique(all_segments$route_id)
  fixed_list <- list()
  
  for(rid in unique_routes) {
    route_segs <- all_segments %>% 
      filter(route_id == rid) %>% 
      arrange(segment_seq)
    
    if(nrow(route_segs) <= 1) {
      fixed_list[[rid]] <- route_segs
      next
    }
    
    # Extract coordinates for the whole route
    geoms <- st_geometry(route_segs)
    
    for(i in 2:length(geoms)) {
      # Get the PREVIOUS segment's end point
      prev_coords <- st_coordinates(geoms[[i-1]])
      prev_end <- prev_coords[nrow(prev_coords), 1:2]
      
      # Get CURRENT segment's coordinates
      curr_coords <- st_coordinates(geoms[[i]])
      
      # FIX: Force current start to match previous end
      curr_coords[1, 1:2] <- prev_end
      
      # Re-create the linestring
      geoms[[i]] <- st_linestring(curr_coords[, 1:2])
    }
    
    route_segs$geometry <- st_sfc(geoms, crs = st_crs(all_segments))
    fixed_list[[rid]] <- route_segs
  }
  return(bind_rows(fixed_list))
}

# --- 5B. Execution ---

# 1. Apply the physical geometry fix
bus_segments_fixed <- fix_route_geometry(bus_segments_proj)

# 2. Measure the NEW lengths (post-fix)
# This includes the distance covered by the closed gaps
bus_segments_with_dist <- bus_segments_fixed %>%
  mutate(
    # Measure length of the corrected geometry
    corrected_dist_m = as.numeric(st_length(geometry))
  )

# --- 5C. Verification ---
total_fixed <- nrow(bus_segments_with_dist)
mean_dist <- round(mean(bus_segments_with_dist$corrected_dist_m, na.rm = TRUE), 1)

message(paste("   - Geometry snapping complete for", total_fixed, "segments."))
message(paste("   - Mean corrected segment distance:", mean_dist, "m"))

# Check for any zero-length segments created by error
zeros <- sum(bus_segments_with_dist$corrected_dist_m < 0.1)
if(zeros > 0) message(paste("   Warning:", zeros, "segments have near-zero length."))

# ==============================================================================
# 6) CLASSIFY BUS ROUTES (Rapid Transit vs Standard)
# ==============================================================================

message("\n[Classifying] Classifying bus routes...")

rapid_transit_routes <- c("11", "12", "13")

bus_segments_with_dist <- bus_segments_with_dist %>%
  mutate(route_type = ifelse(route_num %in% rapid_transit_routes, 
                             "Rapid Transit", 
                             "Standard"))

bus_stops_proj <- bus_stops_proj %>%
  mutate(route_type = ifelse(route_num %in% rapid_transit_routes,
                             "Rapid Transit",
                             "Standard"))

n_rapid <- sum(bus_segments_with_dist$route_type == "Rapid Transit")
n_standard <- sum(bus_segments_with_dist$route_type == "Standard")

message(paste("   - Rapid Transit segments:", n_rapid))
message(paste("   - Standard segments:", n_standard))

# ==============================================================================
# 7) BUILD WALK NETWORK FROM ROADS
# ==============================================================================

message("\n[Building] Building walk network from roads...")

# Convert roads to dodgr format for walking distance calculations
# Using all roads (no filtering as per user request)

roads_for_walk <- roads_proj %>%
  st_cast("LINESTRING") %>%
  filter(!st_is_empty(geometry))

# Create dodgr graph
walk_graph <- weight_streetnet(
  roads_for_walk, 
  wt_profile = "foot",
  id_col = "edge_id"
)

message(paste("   - Walk network edges:", nrow(walk_graph)))

# Function to calculate walk distance between two points
calc_walk_distance <- function(from_coords, to_coords, graph = walk_graph) {
  
  # from_coords and to_coords should be matrices with columns X, Y
  if(is.null(from_coords) || is.null(to_coords)) return(NA_real_)
  if(nrow(from_coords) == 0 || nrow(to_coords) == 0) return(NA_real_)
  
  tryCatch({
    d <- dodgr_dists(
      graph,
      from = from_coords,
      to = to_coords
    )
    as.numeric(d[1, 1])
  }, error = function(e) {
    NA_real_
  })
}

message("Walk network ready.")


# ==============================================================================
# FULL UPDATED CODE (PARTS 8 → 15) — START TO FINISH ✅
#
# UPDATES:
#   1. DISTINCT BUS ROUTES: Now counts "Route + Direction" combinations.
#      (Route 9 East and Route 9 West count as 2 distinct routes).
#   2. RESTRICTIONS: No Start/End Transfers, No Consecutive Walks (Bridging).
#   3. METRICS: Strict Intermediate count (Hops - 1).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(tidyr)
  library(purrr)
  library(igraph)
  library(flextable)
})

# ==============================================================================
# GLOBAL SETTINGS
# ==============================================================================
BUS_MULT   <- 1.10    # +10%
BRT_MULT   <- 1.00    # neutral
METRO_MULT <- 0.90    # -10%

TOL_PCT <- 0.10       # Phase II tolerance: within +10% of shortest total chain

# ==============================================================================
# 8) BUILD TRANSFER NETWORK (DYNAMIC BRT + METRO INTEGRATION)
# ==============================================================================
message("\n[8] Building transfer network (Dynamic BRT logic + Metro integration)...")

# ---- Required objects ----
stopifnot(exists("stations_proj"))
stopifnot(exists("bus_stops_proj"))

# ---- Ensure Metro station IDs exist ----
if(!"stn_id" %in% names(stations_proj)) {
  stations_proj <- stations_proj %>% mutate(stn_id = row_number())
}

# ---- Ensure BUS stop IDs exist ----
stopifnot("bus_stop_id" %in% names(bus_stops_proj))
stopifnot("stop_code"   %in% names(bus_stops_proj))
stopifnot("route_num"   %in% names(bus_stops_proj))
stopifnot("route_id"    %in% names(bus_stops_proj))

# ---- Ensure route_type exists at bus stop level ----
if(!"route_type" %in% names(bus_stops_proj)) {
  brt_routes <- c("11","12","13")
  bus_stops_proj <- bus_stops_proj %>%
    mutate(
      route_num  = as.character(route_num),
      route_type = ifelse(route_num %in% brt_routes, "Rapid Transit", "Standard")
    )
} else {
  bus_stops_proj <- bus_stops_proj %>% mutate(route_num = as.character(route_num))
}

# ---- Ensure direction exists ----
if(!"direction" %in% names(bus_stops_proj)) {
  bus_stops_proj$direction <- "Unknown"
}

# ------------------------------------------------------------------------------
# 8A. METRO-METRO TRANSFERS (Interchanges)
# ------------------------------------------------------------------------------
message("   [8A] Metro-Metro interchanges...")

stopifnot("metrostationname" %in% names(stations_proj))

metro_metro_transfers <- stations_proj %>%
  st_drop_geometry() %>%
  group_by(metrostationname) %>%
  filter(n() > 1) %>%
  summarise(ids = list(stn_id), .groups = "drop") %>%
  mutate(transfers = purrr::map(ids, ~{
    pairs <- combn(sort(.x), 2)
    data.frame(from = pairs[1,], to = pairs[2,], transfer_type = "metro_metro")
  })) %>%
  tidyr::unnest(transfers) %>%
  select(from, to, transfer_type)

message(paste("      - Metro-Metro links:", nrow(metro_metro_transfers)))

# ------------------------------------------------------------------------------
# 8B. BUS-BUS EXACT TRANSFERS (Same stop_code across multiple routes)
# ------------------------------------------------------------------------------
message("   [8B] Bus-Bus physical intersections...")

bus_nodes <- bus_stops_proj %>% st_drop_geometry()

multi_route_hubs <- bus_nodes %>%
  group_by(stop_code) %>%
  filter(n_distinct(route_id) > 1) %>%
  summarise(all_node_ids = list(bus_stop_id), .groups = "drop")

if(nrow(multi_route_hubs) > 0) {
  bus_bus_exact_transfers <- do.call(rbind, lapply(multi_route_hubs$all_node_ids, function(x) {
    if(length(x) < 2) return(NULL)
    pairs <- combn(sort(x), 2)
    data.frame(from = pairs[1,], to = pairs[2,], transfer_type = "bus_bus_physical")
  }))
} else {
  bus_bus_exact_transfers <- data.frame(from=integer(), to=integer(), transfer_type=character())
}

message(paste("      - Bus-Bus physical links:", nrow(bus_bus_exact_transfers)))

# ------------------------------------------------------------------------------
# 8C. METRO-BUS TRANSFERS (200m threshold)
# ------------------------------------------------------------------------------
message("   [8C] Metro-Bus transfers (200m threshold)...")

walk_threshold_m <- 200
nb_metro_bus <- st_is_within_distance(stations_proj, bus_stops_proj, dist = walk_threshold_m)

metro_bus_transfers <- data.frame()

for(i in seq_along(nb_metro_bus)) {
  j_vec <- nb_metro_bus[[i]]
  if(length(j_vec) > 0) {
    for(j in j_vec) {
      dist_euc <- as.numeric(st_distance(stations_proj[i,], bus_stops_proj[j,]))
      metro_bus_transfers <- rbind(metro_bus_transfers, data.frame(
        from = stations_proj$stn_id[i],
        to   = bus_stops_proj$bus_stop_id[j],
        dist_m = dist_euc,
        transfer_type = "metro_bus"
      ))
    }
  }
}

message(paste("      - Metro-Bus links (200m):", nrow(metro_bus_transfers)))

# ------------------------------------------------------------------------------
# 8D. MANUAL METRO-BUS CONNECTIONS
# ------------------------------------------------------------------------------
message("   [8D] Manual Metro-Bus connections...")

metro_al_iman <- stations_proj %>% filter(metrostationname == "Al Iman Hospital") %>% pull(stn_id)
metro_khurais <- stations_proj %>% filter(metrostationname == "Khurais Road") %>% pull(stn_id)

manual_metro_bus <- data.frame(
  from = c(ifelse(length(metro_al_iman) > 0, metro_al_iman[1], NA),
           ifelse(length(metro_khurais) > 0, metro_khurais[1], NA)),
  to   = c(1512, 1982),
  dist_m = c(170, 20),
  transfer_type = c("metro_bus_manual", "metro_bus_manual")
) %>%
  filter(!is.na(from) & !is.na(to))

message(paste("      - Manual Metro-Bus links:", nrow(manual_metro_bus)))

# ------------------------------------------------------------------------------
# 8E. BUS-BUS PROXIMITY TRANSFERS (BRT <-> Standard within 100m)
# ------------------------------------------------------------------------------
message("   [8E] Bus-Bus proximity transfers (BRT <-> Standard)...")

brt_routes <- c("11", "12", "13")
brt_stops <- bus_stops_proj %>% filter(route_num %in% brt_routes)
std_stops <- bus_stops_proj %>% filter(!route_num %in% brt_routes)

bus_bus_proximity_transfers <- data.frame()

if(nrow(brt_stops) > 0 && nrow(std_stops) > 0) {
  
  bus_prox_matrix <- st_is_within_distance(brt_stops, std_stops, dist = 100)
  
  for(i in seq_along(bus_prox_matrix)) {
    j_vec <- bus_prox_matrix[[i]]
    
    if(length(j_vec) > 0) {
      for(j in j_vec) {
        if(brt_stops$stop_code[i] == std_stops$stop_code[j]) next
        
        dist <- as.numeric(st_distance(brt_stops[i,], std_stops[j,]))
        
        # BRT -> Standard
        bus_bus_proximity_transfers <- rbind(bus_bus_proximity_transfers, data.frame(
          from = brt_stops$bus_stop_id[i],
          to   = std_stops$bus_stop_id[j],
          dist_m = round(dist),
          transfer_type = "bus_bus_proximity"
        ))
        
        # Standard -> BRT
        bus_bus_proximity_transfers <- rbind(bus_bus_proximity_transfers, data.frame(
          from = std_stops$bus_stop_id[j],
          to   = brt_stops$bus_stop_id[i],
          dist_m = round(dist),
          transfer_type = "bus_bus_proximity"
        ))
      }
    }
  }
}

message(paste("      - Bus-Bus proximity links (BRT):", nrow(bus_bus_proximity_transfers)))

# ------------------------------------------------------------------------------
# 8E.2. NEW: STANDARD BUS <-> STANDARD BUS PROXIMITY (100m)
# ------------------------------------------------------------------------------
message("   [8E.2] Standard Bus <-> Standard Bus proximity transfers (100m)...")

bus_bus_standard_transfers <- data.frame()

if(nrow(std_stops) > 0) {
  
  std_prox_matrix <- st_is_within_distance(std_stops, std_stops, dist = 100)
  transfer_list_std <- list()
  
  for(i in seq_along(std_prox_matrix)) {
    j_vec <- std_prox_matrix[[i]]
    
    # Filter out self
    j_vec <- j_vec[j_vec != i]
    
    if(length(j_vec) > 0) {
      
      # --- RESTRICTION LOGIC ---
      curr_route <- std_stops$route_num[i]
      curr_dir   <- std_stops$direction[i]
      
      neigh_routes <- std_stops$route_num[j_vec]
      neigh_dirs   <- std_stops$direction[j_vec]
      
      # Allow if Route is DIFFERENT OR Direction is DIFFERENT
      valid_mask <- (neigh_routes != curr_route) | (neigh_dirs != curr_dir)
      
      j_vec_valid <- j_vec[valid_mask]
      
      if(length(j_vec_valid) > 0) {
        dists <- as.numeric(st_distance(std_stops[i,], std_stops[j_vec_valid,]))
        
        transfer_list_std[[i]] <- data.frame(
          from = std_stops$bus_stop_id[i],
          to   = std_stops$bus_stop_id[j_vec_valid],
          dist_m = round(dists),
          transfer_type = "bus_bus_standard",
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  bus_bus_standard_transfers <- bind_rows(transfer_list_std)
}

message(paste("      - Bus-Bus proximity links (Standard):", nrow(bus_bus_standard_transfers)))

# ------------------------------------------------------------------------------
# 8F. MASTER TRANSFER MERGE
# ------------------------------------------------------------------------------
message("   [8F] Finalizing Master Transfer Table...")

master_transfers <- bind_rows(
  if(nrow(bus_bus_exact_transfers) > 0) bus_bus_exact_transfers %>% mutate(dist_m = 0)  else NULL,
  if(nrow(metro_metro_transfers) > 0)   metro_metro_transfers   %>% mutate(dist_m = 50) else NULL,
  if(nrow(metro_bus_transfers) > 0)     metro_bus_transfers else NULL,
  manual_metro_bus,
  bus_bus_proximity_transfers,
  bus_bus_standard_transfers
) %>%
  group_by(from, to) %>%
  arrange(dist_m) %>%
  slice(1) %>%
  ungroup()

message(paste("\n[OK] Transfer network complete! Total links:", nrow(master_transfers)))

# ==============================================================================
# 9) BUILD UNIFIED MULTIMODAL GRAPH (WEIGHTED FOR PRIORITY)
# ==============================================================================
message("\n[9] Building unified multimodal graph (Priority Weights ±30%)...")

# ------------------------------------------------------------------------------
# 9A. METRO LINE EDGES
# ------------------------------------------------------------------------------
stopifnot("metroline" %in% names(stations_proj))
stopifnot("stationseq" %in% names(stations_proj))

stn_xy <- st_coordinates(stations_proj)
stations_proj$X <- stn_xy[, 1]
stations_proj$Y <- stn_xy[, 2]

edges_metro_line <- stations_proj %>%
  st_drop_geometry() %>%
  arrange(metroline, stationseq) %>%
  group_by(metroline) %>%
  mutate(to_id = lead(stn_id), X2 = lead(X), Y2 = lead(Y)) %>%
  ungroup() %>%
  filter(!is.na(to_id)) %>%
  transmute(
    from = paste0("M_", stn_id),
    to   = paste0("M_", to_id),
    weight = sqrt((X - X2)^2 + (Y - Y2)^2),
    edge_type = "metro_line",
    route_type = "Metro",
    route_code = as.character(metroline),
    unique_route_key = as.character(metroline) # Metro "Line 1" is unique enough
  )

# ------------------------------------------------------------------------------
# 9B. BUS ROUTE EDGES (WITH DIRECTION MERGED FOR UNIQUE KEY)
# ------------------------------------------------------------------------------
if(!exists("all_points")) stop("❌ Error: 'all_points' object is missing.")
if(!exists("bus_segments_with_dist") && !exists("bus_route_segments_sf")) stop("❌ Need bus_segments_with_dist OR bus_route_segments_sf.")

raw_sequence <- bind_rows(all_points) %>%
  st_drop_geometry() %>%
  select(route_id, segment_seq, point_type, stop_code)

node_dictionary <- bus_stops_proj %>%
  st_drop_geometry() %>%
  select(route_id, stop_code, bus_stop_id) %>%
  distinct(route_id, stop_code, .keep_all = TRUE)

bus_lookup_final <- raw_sequence %>%
  left_join(node_dictionary, by = c("route_id", "stop_code")) %>%
  select(route_id, segment_seq, point_type, bus_stop_id)

input_segments <- if(exists("bus_route_segments_sf")) bus_route_segments_sf else bus_segments_with_dist

if(!"corrected_dist_m" %in% names(input_segments)) {
  input_segments$corrected_dist_m <- as.numeric(st_length(input_segments$geometry))
}

if(!"route_type" %in% names(input_segments)) {
  rapid_transit_routes <- c("11", "12", "13")
  input_segments <- input_segments %>%
    mutate(route_num = as.character(route_num)) %>%
    mutate(route_type = ifelse(route_num %in% rapid_transit_routes, "Rapid Transit", "Standard"))
}

edges_bus_route <- input_segments %>%
  st_drop_geometry() %>%
  select(route_id, segment_seq, corrected_dist_m, route_type, route_num) %>%
  left_join(bus_lookup_final %>% filter(point_type == "from"), by = c("route_id", "segment_seq")) %>%
  rename(from_bus_id = bus_stop_id) %>%
  left_join(bus_lookup_final %>% filter(point_type == "to"), by = c("route_id", "segment_seq")) %>%
  rename(to_bus_id = bus_stop_id) %>%
  filter(!is.na(from_bus_id) & !is.na(to_bus_id)) %>%
  # MERGE DIRECTION FROM BUS_STOPS TO CREATE UNIQUE KEY
  left_join(bus_stops_proj %>% st_drop_geometry() %>% select(bus_stop_id, direction), 
            by = c("from_bus_id" = "bus_stop_id")) %>%
  mutate(
    unique_route_key = paste(route_num, direction, sep = " - ") # "944 - Eastbound"
  ) %>%
  transmute(
    from = paste0("B_", from_bus_id),
    to   = paste0("B_", to_bus_id),
    weight = corrected_dist_m,
    edge_type = "bus_route",
    route_type = route_type,
    route_code = as.character(route_num),
    unique_route_key = unique_route_key
  )

# ------------------------------------------------------------------------------
# 9C. TRANSFER EDGES
# ------------------------------------------------------------------------------
# Helper to add NA attributes for consistency
add_na_attrs <- function(df) {
  df %>% mutate(route_code = NA_character_, unique_route_key = NA_character_)
}

edges_metro_transfer <- if (exists("metro_metro_transfers") && nrow(metro_metro_transfers) > 0) {
  metro_metro_transfers %>%
    transmute(from = paste0("M_", from), to = paste0("M_", to), weight = 50, edge_type = "metro_metro_transfer", route_type = "Transfer") %>% add_na_attrs()
} else { data.frame() }

edges_bus_exact <- if (exists("bus_bus_exact_transfers") && nrow(bus_bus_exact_transfers) > 0) {
  bus_bus_exact_transfers %>%
    transmute(from = paste0("B_", from), to = paste0("B_", to), weight = 0, edge_type = "bus_bus_exact", route_type = "Transfer") %>% add_na_attrs()
} else { data.frame() }

edges_metro_bus <- if (exists("metro_bus_transfers") && nrow(metro_bus_transfers) > 0) {
  metro_bus_transfers %>%
    transmute(from = paste0("M_", from), to = paste0("B_", to), weight = dist_m, edge_type = "metro_bus", route_type = "Transfer") %>% add_na_attrs()
} else { data.frame() }

edges_metro_bus_manual <- if (exists("manual_metro_bus") && nrow(manual_metro_bus) > 0) {
  manual_metro_bus %>%
    transmute(from = paste0("M_", from), to = paste0("B_", to), weight = dist_m, edge_type = "metro_bus_manual", route_type = "Transfer") %>% add_na_attrs()
} else { data.frame() }

edges_bus_proximity <- if (exists("bus_bus_proximity_transfers") && nrow(bus_bus_proximity_transfers) > 0) {
  bus_bus_proximity_transfers %>%
    transmute(from = paste0("B_", from), to = paste0("B_", to), weight = dist_m, edge_type = "bus_bus_proximity", route_type = "Transfer") %>% add_na_attrs()
} else { data.frame() }

edges_bus_standard <- if (exists("bus_bus_standard_transfers") && nrow(bus_bus_standard_transfers) > 0) {
  bus_bus_standard_transfers %>%
    transmute(from = paste0("B_", from), to = paste0("B_", to), weight = dist_m, edge_type = "bus_bus_standard", route_type = "Transfer") %>% add_na_attrs()
} else { data.frame() }

# ------------------------------------------------------------------------------
# 9D. ASSEMBLE GRAPH
# ------------------------------------------------------------------------------
message("   [9D] Assembling final directed graph + Priority weights...")

edges_bidirectional_source <- bind_rows(
  edges_metro_line,
  edges_metro_transfer,
  edges_bus_exact,
  edges_metro_bus,
  edges_metro_bus_manual,
  edges_bus_proximity,
  edges_bus_standard
)

edges_oneway_source <- edges_bus_route

# Make transfer + metro edges bidirectional (keep attributes)
edges_bidir_expanded <- bind_rows(
  edges_bidirectional_source,
  edges_bidirectional_source %>% rename(from = to, to = from)
) %>%
  distinct(from, to, edge_type, route_type, .keep_all = TRUE)

all_edges_final <- bind_rows(edges_bidir_expanded, edges_oneway_source)

all_edges_final <- all_edges_final %>%
  mutate(
    priority_weight = case_when(
      route_type == "Standard"      ~ weight * BUS_MULT,
      route_type == "Rapid Transit" ~ weight * BRT_MULT,
      route_type == "Metro"         ~ weight * METRO_MULT,
      TRUE                          ~ weight
    )
  )

g_multimodal <- graph_from_data_frame(all_edges_final, directed = TRUE)
attr(g_multimodal, "ck") <- "mm"   # cache tag for memoized path lookups

E(g_multimodal)$weight <- all_edges_final$priority_weight
E(g_multimodal)$real_distance <- all_edges_final$weight
E(g_multimodal)$edge_type     <- all_edges_final$edge_type
E(g_multimodal)$route_type    <- all_edges_final$route_type
E(g_multimodal)$unique_route_key <- all_edges_final$unique_route_key # <--- CRITICAL

message(paste("[OK] Multimodal Graph Built: V:", vcount(g_multimodal), "E:", ecount(g_multimodal)))

# Metro-only
metro_subset <- bind_rows(edges_metro_line, edges_metro_transfer)
g_metro_only <- NULL

if(nrow(metro_subset) > 0) {
  metro_edges_bidir <- bind_rows(
    metro_subset,
    metro_subset %>% rename(from = to, to = from)
  ) %>%
    distinct(from, to, edge_type, route_type, .keep_all = TRUE)
  
  g_metro_only <- graph_from_data_frame(metro_edges_bidir, directed = TRUE)
  attr(g_metro_only, "ck") <- "mo"   # cache tag for memoized path lookups
  E(g_metro_only)$weight    <- metro_edges_bidir$weight
  E(g_metro_only)$edge_type <- metro_edges_bidir$edge_type
  E(g_metro_only)$unique_route_key <- metro_edges_bidir$unique_route_key
  
  message("[OK] Metro-only graph built.")
} else {
  message("[WARN] Metro-only graph is empty.")
}

# ==============================================================================
# 10) IMPUTATION RATIOS
# ==============================================================================
stopifnot(exists("ratio_lookup"))

# ==============================================================================
# 11) HELPER FUNCTIONS (FINAL: MASKED TRANSFERS + FEEDER LINE ADJUSTMENT)
# ==============================================================================
message("\n[11] Defining helper functions...")

get_L1_from_lookup <- function(rp_id, transit_type, L1_metro_lookup, L1_bus_lookup, ratio_lookup) {
  # FAST: precomputed named vectors (.l1m_*/.l1b_*) replace per-call dplyr::filter; same semantics.
  key <- as.character(rp_id)
  if(transit_type == "metro") {
    if(!(key %in% names(.l1m_geo))) return(NA_real_)
    net <- .l1m_net[[key]]
    if(!is.na(net) && !is.infinite(net)) return(net * 1000)
    ratio <- ratio_lookup["nearest_metro"]; if(is.na(ratio)) ratio <- 1.3
    return(.l1m_geo[[key]] * ratio * 1000)
  } else {
    if(!(key %in% names(.l1b_geo))) return(NA_real_)
    net <- .l1b_net[[key]]
    if(!is.na(net) && !is.infinite(net)) return(net * 1000)
    ratio <- ratio_lookup["nearest_bus"]; if(is.na(ratio)) ratio <- 1.3
    return(.l1b_geo[[key]] * ratio * 1000)
  }
}

get_L3_from_lookup <- function(rp_id, dest_type, transit_type, clinic_targets, L3_metro_lookup, L3_bus_lookup, ratio_L3_metro_priv, ratio_L3_metro_pub, ratio_L3_bus_priv, ratio_L3_bus_pub) {
  # FAST: precomputed named vectors (.ct_clinic_id/.l3m_*/.l3b_*) replace per-call dplyr::filter; same semantics.
  tkey <- paste0(rp_id, "||", dest_type)
  if(!(tkey %in% names(.ct_clinic_id))) return(NA_real_)
  clinic_id <- .ct_clinic_id[[tkey]]
  is_private <- grepl("priv", dest_type)
  if(transit_type == "metro") {
    if(!(clinic_id %in% names(.l3m_geo))) return(NA_real_)
    net <- .l3m_net[[clinic_id]]
    if(!is.na(net) && !is.infinite(net)) return(net * 1000)
    ratio <- if(is_private) ratio_L3_metro_priv else ratio_L3_metro_pub
    return(.l3m_geo[[clinic_id]] * ratio * 1000)
  } else {
    if(!(clinic_id %in% names(.l3b_geo))) return(NA_real_)
    net <- .l3b_net[[clinic_id]]
    if(!is.na(net) && !is.infinite(net)) return(net * 1000)
    ratio <- if(is_private) ratio_L3_bus_priv else ratio_L3_bus_pub
    return(.l3b_geo[[clinic_id]] * ratio * 1000)
  }
}

find_nearest_transit <- function(point_sf, transit_sf, transit_type) {
  nearest_idx <- st_nearest_feature(point_sf, transit_sf)
  node_id <- if(transit_type == "metro") paste0("M_", transit_sf$stn_id[nearest_idx]) else paste0("B_", transit_sf$bus_stop_id[nearest_idx])
  list(idx = nearest_idx, node_id = node_id)
}

# ------------------------------------------------------------------------------
# 11B. DISTANCE & VALIDATION (Calculates Weight/Distance Only)
# ------------------------------------------------------------------------------
.impl_l2r <- function(from_node, to_node, graph) {
  if(is.na(from_node) || is.na(to_node)) return(NA_real_)
  if(!from_node %in% V(graph)$name || !to_node %in% V(graph)$name) return(NA_real_)
  if(from_node == to_node) return(0)
  
  path_obj <- suppressWarnings(shortest_paths(graph, from = from_node, to = to_node, weights = E(graph)$weight, output = "epath"))
  edge_seq <- path_obj$epath[[1]]
  if(length(edge_seq) == 0) return(NA_real_)
  
  e_types <- E(graph)$edge_type[edge_seq]
  e_dists <- as.numeric(E(graph)$real_distance[edge_seq]) 
  
  # --- 1. Block Forbidden Transfers ---
  forbidden_transfers <- c("bus_bus_exact", "metro_bus", 
                           "metro_bus_manual", "bus_bus_proximity", "bus_bus_standard")
  
  if(e_types[1] %in% forbidden_transfers) return(NA_real_)
  if(e_types[length(e_types)] %in% forbidden_transfers) return(NA_real_)
  
  # --- 2. Block Consecutive Transfers ---
  all_transfer_types <- c("metro_metro_transfer", forbidden_transfers)
  is_transfer <- e_types %in% all_transfer_types
  
  if(length(is_transfer) > 1 && any(is_transfer & dplyr::lag(is_transfer, default=FALSE))) return(NA_real_)
  
  # --- 3. MASKING: Ignore cost for Metro-Metro transfer if it's First or Last edge ---
  cost_mask <- rep(TRUE, length(edge_seq))
  if(e_types[1] == "metro_metro_transfer") cost_mask[1] <- FALSE
  if(length(e_types) > 1 && e_types[length(e_types)] == "metro_metro_transfer") cost_mask[length(e_types)] <- FALSE
  
  sum(e_dists[cost_mask], na.rm = TRUE)
}

.impl_l2s <- function(from_node, to_node, graph) {
  if(is.na(from_node) || is.na(to_node) || is.null(graph)) return(NA_real_)
  if(!from_node %in% V(graph)$name || !to_node %in% V(graph)$name) return(NA_real_)
  if(from_node == to_node) return(0)
  d <- suppressWarnings(distances(graph, v = from_node, to = to_node, weights = E(graph)$weight))
  if(is.infinite(d[1, 1])) return(NA_real_)
  d[1, 1]
}

check_on_route_via_distances <- function(d_rp, d_cl, d_total, tol = 1) {
  if(is.na(d_rp) || is.na(d_cl) || is.na(d_total)) return(NA)
  abs((d_rp + d_cl) - d_total) <= tol
}

get_combo_priority <- function(l1_mode, l3_mode, l1_rt, l3_rt) {
  is_brt_start <- (l1_rt == "Rapid Transit"); is_brt_end <- (l3_rt == "Rapid Transit")
  if(l1_mode == "metro" && l3_mode == "metro") return(10)
  if((l1_mode == "bus" && l3_mode == "metro" && is_brt_start) || (l1_mode == "metro" && l3_mode == "bus" && is_brt_end)) return(20)
  if((l1_mode == "bus" && l3_mode == "metro" && !is_brt_start) || (l1_mode == "metro" && l3_mode == "bus" && !is_brt_end)) return(30)
  if(l1_mode == "bus" && l3_mode == "bus") {
    if(is_brt_start && is_brt_end) return(40)
    if(xor(is_brt_start, is_brt_end)) return(50)
    return(60)
  }
  999
}

select_best_by_pct_tolerance <- function(df_candidates, tol_pct = 0.30) {
  if (is.null(df_candidates) || nrow(df_candidates) == 0) return(NULL)
  if (!("total" %in% names(df_candidates))) return(NULL)
  df_candidates <- df_candidates %>% dplyr::filter(!is.na(.data$total))
  if (nrow(df_candidates) == 0) return(NULL)
  min_total <- min(df_candidates$total, na.rm = TRUE)
  thr <- min_total * (1 + tol_pct)
  df_allowed <- df_candidates %>% dplyr::filter(.data$total <= thr) %>% dplyr::arrange(.data$prio, .data$total)
  if (nrow(df_allowed) == 0) return(NULL)
  df_allowed[1, , drop = FALSE]
}

# ------------------------------------------------------------------------------
# 11C. PATH DETAILS (Updated: Counts & Line Adjustments)
# ------------------------------------------------------------------------------
.impl_pd <- function(graph, from_node, to_node) {
  res <- list(n_transfers=0, n_bus_transfers=0, n_metro_transfers=0, n_mode_switches=0, n_bus_routes=0, n_metro_lines=0, n_stops=0, has_brt=FALSE, dist_metro_m=0, dist_brt_m=0, dist_bus_std_m=0, dist_walk_transfer_m=0, seg_str_metro="")
  if(is.na(from_node) || is.na(to_node)) return(res)
  if(!from_node %in% V(graph)$name || !to_node %in% V(graph)$name) return(res)
  if(from_node == to_node) return(res)
  path_obj <- suppressWarnings(shortest_paths(graph, from = from_node, to = to_node, weights = E(graph)$weight, output = "epath"))
  edge_seq <- path_obj$epath[[1]]
  if(length(edge_seq) == 0) return(res)
  e_types  <- E(graph)$edge_type[edge_seq]; e_dists  <- suppressWarnings(as.numeric(E(graph)$real_distance[edge_seq])); e_routes <- E(graph)$route_code[edge_seq]
  
  # --- VALIDATION ---
  forbidden_transfers <- c("bus_bus_exact", "metro_bus", 
                           "metro_bus_manual", "bus_bus_proximity", "bus_bus_standard")
  
  if(e_types[1] %in% forbidden_transfers) return(res)
  if(e_types[length(e_types)] %in% forbidden_transfers) return(res)
  
  all_transfer_types <- c("metro_metro_transfer", forbidden_transfers)
  is_tr <- e_types %in% all_transfer_types
  
  if(length(is_tr) > 1 && any(is_tr & dplyr::lag(is_tr, default=FALSE))) return(res)
  
  # --- MASKING LOGIC FOR COUNTS ---
  count_mask <- rep(TRUE, length(e_types))
  if(e_types[1] == "metro_metro_transfer") count_mask[1] <- FALSE
  if(length(e_types) > 1 && e_types[length(e_types)] == "metro_metro_transfer") count_mask[length(e_types)] <- FALSE
  
  # --- COUNTS ---
  res$n_metro_transfers <- sum(e_types == "metro_metro_transfer" & count_mask)
  res$n_bus_transfers   <- sum(e_types %in% c("bus_bus_exact", "bus_bus_proximity", "bus_bus_standard"))
  res$n_mode_switches   <- sum(e_types %in% c("metro_bus", "metro_bus_manual"))
  res$n_transfers       <- res$n_metro_transfers + res$n_bus_transfers + res$n_mode_switches
  
  keys <- E(graph)$unique_route_key[edge_seq]
  res$n_bus_routes  <- dplyr::n_distinct(keys[e_types == "bus_route" & !is.na(keys)])
  
  # --- LINE COUNT ADJUSTMENT (Feeder Logic) ---
  res$n_metro_lines <- dplyr::n_distinct(keys[e_types == "metro_line" & !is.na(keys)])
  
  # If we started with a transfer (masked), and have >1 line, ignore the first 'feeder' line
  if(e_types[1] == "metro_metro_transfer" && res$n_metro_lines > 1) {
    res$n_metro_lines <- res$n_metro_lines - 1
  }
  # If we ended with a transfer (masked), and still have >1 line, ignore the last 'feeder' line
  if(length(e_types) > 1 && e_types[length(e_types)] == "metro_metro_transfer" && res$n_metro_lines > 1) {
    res$n_metro_lines <- res$n_metro_lines - 1
  }
  
  res$n_stops       <- sum(e_types %in% c("metro_line", "bus_route"))
  
  mask_metro <- e_types == "metro_line"
  mask_bus   <- e_types == "bus_route"
  
  # Apply count_mask to the walk distance calculation too
  mask_walk  <- (e_types %in% all_transfer_types) & count_mask 
  
  safe_routes <- as.character(e_routes); mask_brt_route <- safe_routes %in% c("11", "12", "13")
  mask_brt_leg <- mask_bus & mask_brt_route; mask_std_leg <- mask_bus & !mask_brt_route
  
  res$dist_metro_m <- sum(e_dists[mask_metro], na.rm=TRUE)
  res$dist_brt_m <- sum(e_dists[mask_brt_leg], na.rm=TRUE) 
  res$dist_bus_std_m <- sum(e_dists[mask_std_leg], na.rm=TRUE) 
  res$dist_walk_transfer_m <- sum(e_dists[mask_walk], na.rm=TRUE)
  
  if(any(mask_metro)) {
    metro_segments <- e_dists[mask_metro]; metro_segments <- metro_segments[!is.na(metro_segments)]
    if(length(metro_segments) > 0) res$seg_str_metro <- paste(round(metro_segments, 1), collapse = ";")
  }
  res$has_brt <- any(mask_brt_leg)
  res
}

# ==============================================================================
# 12) PART 12 — PROCESS SAMPLE POINTS (FINAL)
# ==============================================================================
message("\n[12] Processing random points (saving Metro/BRT/Std/Walk + Segments)...")

# ------------------------------------------------------------------------------
# PRECOMPUTE FAST LOOKUPS (replaces in-loop dplyr::filter + per-clinic st_nearest_feature).
# O(1) named-vector lookups; vectorized nearest-transit done once. Exact-equivalent results.
# ------------------------------------------------------------------------------
if (!exists(".PCACHE")) .PCACHE <- new.env(hash = TRUE, parent = emptyenv())  # persistent igraph path/distance cache
# Memoizing wrappers over the igraph routing functions (.impl_*): cache by (from,to[,graph tag]).
# Routes are invariant across points sharing access/clinic nodes AND across all wait/parking
# sensitivity scenarios, so caching is exact and reused everywhere.
get_L2_distance_simple <- function(from_node, to_node, graph) {
  if (is.na(from_node) || is.na(to_node)) return(NA_real_)
  .k <- paste0("l2s", from_node, "", to_node)
  .h <- .PCACHE[[.k]]; if (!is.null(.h)) return(.h)
  .r <- .impl_l2s(from_node, to_node, graph); .PCACHE[[.k]] <- .r; .r
}
get_L2_distance_real <- function(from_node, to_node, graph) {
  if (is.na(from_node) || is.na(to_node)) return(NA_real_)
  .k <- paste0("l2r", from_node, "", to_node)
  .h <- .PCACHE[[.k]]; if (!is.null(.h)) return(.h)
  .r <- .impl_l2r(from_node, to_node, graph); .PCACHE[[.k]] <- .r; .r
}
get_path_details <- function(graph, from_node, to_node) {
  .k <- paste0("pd", attr(graph, "ck"), "", from_node, "", to_node)
  .h <- .PCACHE[[.k]]; if (!is.null(.h)) return(.h)
  .r <- .impl_pd(graph, from_node, to_node); .PCACHE[[.k]] <- .r; .r
}
.l1m_geo <- setNames(L1_metro_lookup$L1_metro_geo_km, as.character(L1_metro_lookup$id))
.l1m_net <- setNames(L1_metro_lookup$L1_metro_net_km, as.character(L1_metro_lookup$id))
.l1b_geo <- setNames(L1_bus_lookup$L1_bus_geo_km,     as.character(L1_bus_lookup$id))
.l1b_net <- setNames(L1_bus_lookup$L1_bus_net_km,     as.character(L1_bus_lookup$id))
.l3m_geo <- setNames(L3_metro_lookup$L3_metro_geo_km, as.character(L3_metro_lookup$clinic_id))
.l3m_net <- setNames(L3_metro_lookup$L3_metro_net_km, as.character(L3_metro_lookup$clinic_id))
.l3b_geo <- setNames(L3_bus_lookup$L3_bus_geo_km,     as.character(L3_bus_lookup$clinic_id))
.l3b_net <- setNames(L3_bus_lookup$L3_bus_net_km,     as.character(L3_bus_lookup$clinic_id))
.ct_clinic_id <- setNames(as.character(clinic_targets$clinic_id),  paste0(clinic_targets$id, "||", clinic_targets$dest_type))
.ct_dest_geo  <- setNames(as.character(clinic_targets$dest_id_geo), paste0(clinic_targets$id, "||", clinic_targets$dest_type))
.rc_net <- setNames(rp_comparison$net_km_p2t, paste0(rp_comparison$id, "||", rp_comparison$dest_type))
.rc_geo <- setNames(rp_comparison$geo_km,     paste0(rp_comparison$id, "||", rp_comparison$dest_type))
# Vectorized nearest-transit: all random points at once
.rp_m_idx <- st_nearest_feature(rp_sample, stations_proj)
.rp_b_idx <- st_nearest_feature(rp_sample, bus_stops_proj)
.rp_metro_node <- setNames(paste0("M_", stations_proj$stn_id[.rp_m_idx]),       as.character(rp_sample$id))
.rp_bus_node   <- setNames(paste0("B_", bus_stops_proj$bus_stop_id[.rp_b_idx]), as.character(rp_sample$id))
.rp_bus_rt_v   <- as.character(bus_stops_proj$route_type[.rp_b_idx]); .rp_bus_rt_v[is.na(.rp_bus_rt_v)] <- "Standard"
.rp_bus_rt     <- setNames(.rp_bus_rt_v, as.character(rp_sample$id))
# Vectorized nearest-transit: all target clinics at once, keyed by dest_id_geo ("priv_X"/"pub_X")
.cl_all <- rbind(
  clinics_private %>% transmute(.dkey = paste0("priv_", id)),
  clinics_public  %>% transmute(.dkey = paste0("pub_",  id))
)
.cl_m_idx <- st_nearest_feature(.cl_all, stations_proj)
.cl_b_idx <- st_nearest_feature(.cl_all, bus_stops_proj)
.cl_metro_node <- setNames(paste0("M_", stations_proj$stn_id[.cl_m_idx]),       .cl_all$.dkey)
.cl_bus_node   <- setNames(paste0("B_", bus_stops_proj$bus_stop_id[.cl_b_idx]), .cl_all$.dkey)
.cl_bus_rt_v   <- as.character(bus_stops_proj$route_type[.cl_b_idx]); .cl_bus_rt_v[is.na(.cl_bus_rt_v)] <- "Standard"
.cl_bus_rt     <- setNames(.cl_bus_rt_v, .cl_all$.dkey)
message("   [precompute] fast lookups + vectorized nearest-transit ready.")

# ------------------------------------------------------------------------------
# MM-REACH FIX (UNWEIGHTED) — opt-in shortest-VALID-path re-route diagnostic.
# Placed RIGHT AFTER the precompute (graph g_multimodal + access-node lookups are
# ready) and BEFORE the loop / Part 15's Table 2 docx writes, so a
# STOP_AFTER_MM_REACH run never opens those files (no "<file> open in Word" crash)
# and finishes fast. The certified .impl_l2r takes the single shortest multimodal
# path and rejects it (NA) when it violates the transfer rules; this re-routes the
# non-reachable trips with the shortest VALID path and reports recovery. Gated,
# additive, OFF by default; writes R14_mmreach_unweighted.{rds,docx} only.
# Env: RUN_MM_REACH=1, MM_REACH_K (20), MM_REACH_RANDOM (0=skip random),
#      STOP_AFTER_MM_REACH=1 to exit right after.
# ------------------------------------------------------------------------------
if (Sys.getenv("RUN_MM_REACH") == "1") {
  .mmf_code <- if (exists("base_dir")) file.path(dirname(base_dir), "Code") else file.path(getwd(), "Code")
  .mmf <- file.path(.mmf_code, "_r14_mm_reach_fix.R")
  if (file.exists(.mmf)) {
    source(.mmf)
    .mmk <- suppressWarnings(as.integer(Sys.getenv("MM_REACH_K"))); if (is.na(.mmk)) .mmk <- 20L
    r14_mm_reach_fix("unweighted", K = .mmk, include_random = (Sys.getenv("MM_REACH_RANDOM") != "0"))
  } else message("[MM-REACH] _r14_mm_reach_fix.R not found at ", .mmf_code, "; skipping.")
  if (Sys.getenv("STOP_AFTER_MM_REACH") == "1") {
    message("[RUN STATUS] STOP_AFTER_MM_REACH -- MM-REACH diagnostic complete, exiting.")
    quit(save = "no", status = 0)
  }
}

dest_types <- c("nearest_priv", "median_priv", "nearest_pub", "median_pub")
results <- list()

if(!exists("rp_sample")) stop("rp_sample not found.")

if (Sys.getenv("VALIDATE_S18_CAP") == "1") { rp_sample <- rp_sample[1:min(300L, nrow(rp_sample)), ]; message("VALIDATION subset: ", nrow(rp_sample), " points") }

if (Sys.getenv("SKIP_S18_LOOP") == "1") {
  message("SKIP_S18_LOOP=1: reusing existing full-10k Section-18 results from disk; skipping the per-point network loop.")
  results_df <- readRDS("sample_test_results.rds")
} else {
for(i in 1:nrow(rp_sample)) {

  # Progress tracker
  if(i %% 100 == 0) message(paste("--- Processing point", i, "of", nrow(rp_sample), "at", Sys.time(), "---"))
  
  rp_id <- rp_sample$id[i]
  .rpk <- as.character(rp_id)

  # L1
  L1_metro_m <- get_L1_from_lookup(rp_id, "metro", L1_metro_lookup, L1_bus_lookup, ratio_lookup)
  L1_bus_m   <- get_L1_from_lookup(rp_id, "bus",   L1_metro_lookup, L1_bus_lookup, ratio_lookup)

  # FAST: precomputed per-point nearest transit (replaces find_nearest_transit)
  near_metro_rp <- list(node_id = .rp_metro_node[[.rpk]])
  near_bus_rp   <- list(node_id = .rp_bus_node[[.rpk]])
  near_bus_rt_type <- .rp_bus_rt[[.rpk]]

  for(dest_type in dest_types) {

    .tk <- paste0(rp_id, "||", dest_type)
    if(!(.tk %in% names(.ct_clinic_id))) next

    clinic_id <- .ct_clinic_id[[.tk]]
    .dkey <- .ct_dest_geo[[.tk]]
    if(!(.dkey %in% names(.cl_metro_node))) next

    # L3
    L3_metro_m <- get_L3_from_lookup(rp_id, dest_type, "metro", clinic_targets, L3_metro_lookup, L3_bus_lookup, ratio_L3_metro_priv, ratio_L3_metro_pub, ratio_L3_bus_priv, ratio_L3_bus_pub)
    L3_bus_m   <- get_L3_from_lookup(rp_id, dest_type, "bus",   clinic_targets, L3_metro_lookup, L3_bus_lookup, ratio_L3_metro_priv, ratio_L3_metro_pub, ratio_L3_bus_priv, ratio_L3_bus_pub)

    # FAST: precomputed per-clinic nearest transit (replaces find_nearest_transit)
    near_metro_cl <- list(node_id = .cl_metro_node[[.dkey]])
    near_bus_cl   <- list(node_id = .cl_bus_node[[.dkey]])
    near_bus_cl_rt_type <- .cl_bus_rt[[.dkey]]

    # Comparison Baseline
    road_dist_m <- NA_real_
    if(.tk %in% names(.rc_net)) {
      val_km <- .rc_net[[.tk]]
      imp_factor <- 1.3
      if(grepl("priv", dest_type)) {
        imp_factor <- if(grepl("nearest", dest_type)) ratio_lookup["nearest_priv"] else ratio_lookup["median_priv"]
      } else {
        imp_factor <- if(grepl("nearest", dest_type)) ratio_lookup["nearest_pub"] else ratio_lookup["median_pub"]
      }
      if(is.na(imp_factor)) imp_factor <- 1.3
      if(is.na(val_km) || is.infinite(val_km)) val_km <- .rc_geo[[.tk]] * imp_factor
      road_dist_m <- val_km * 1000
    }
    
    # METRO ONLY
    metro_same_stn <- (near_metro_rp$node_id == near_metro_cl$node_id)
    if(metro_same_stn) {
      L2_mo  <- NA_real_; tot_mo <- NA_real_; mo_mets <- list(n_transfers=NA, n_lines=NA, n_stops=NA)
    } else {
      L2_mo <- get_L2_distance_simple(near_metro_rp$node_id, near_metro_cl$node_id, g_metro_only)
      if(is.na(L2_mo)) {
        tot_mo <- NA_real_; mo_mets <- list(n_transfers=NA, n_lines=NA, n_stops=NA)
      } else {
        tot_mo <- L1_metro_m + L2_mo + L3_metro_m
        det_mo <- get_path_details(g_metro_only, near_metro_rp$node_id, near_metro_cl$node_id)
        mo_mets <- list(n_transfers = det_mo$n_transfers, n_lines = det_mo$n_metro_lines, n_stops = det_mo$n_stops)
      }
    }
    metro_closer <- (L1_metro_m < road_dist_m)
    metro_on_route <- check_on_route_via_distances(L1_metro_m, L3_metro_m, road_dist_m)
    
    # MULTIMODAL
    invalid_metro_bus <- (near_bus_cl$node_id == near_bus_rp$node_id)
    invalid_bus_metro <- (near_metro_cl$node_id == near_metro_rp$node_id)
    
    combos <- list(
      list(type="Metro-Metro", l1=L1_metro_m, l3=L3_metro_m, l1_mode="metro", l3_mode="metro", s=near_metro_rp$node_id, e=near_metro_cl$node_id),
      list(type="Metro-Bus",   l1=L1_metro_m, l3=L3_bus_m,   l1_mode="metro", l3_mode="bus",   s=near_metro_rp$node_id, e=near_bus_cl$node_id),
      list(type="Bus-Metro",   l1=L1_bus_m,   l3=L3_metro_m, l1_mode="bus",   l3_mode="metro", s=near_bus_rp$node_id,   e=near_metro_cl$node_id),
      list(type="Bus-Bus",     l1=L1_bus_m,   l3=L3_bus_m,   l1_mode="bus",   l3_mode="bus",   s=near_bus_rp$node_id,   e=near_bus_cl$node_id)
    )
    
    combo_rows <- list()
    for(cmb in combos) {
      if(cmb$s == cmb$e) next
      if(cmb$type == "Metro-Bus" && invalid_metro_bus) next
      if(cmb$type == "Bus-Metro" && invalid_bus_metro) next
      l2 <- get_L2_distance_real(cmb$s, cmb$e, g_multimodal)
      if(is.na(l2)) next
      tot <- cmb$l1 + l2 + cmb$l3
      prio <- get_combo_priority(l1_mode = cmb$l1_mode, l3_mode = cmb$l3_mode, l1_rt = near_bus_rt_type, l3_rt = near_bus_cl_rt_type)
      combo_rows[[length(combo_rows)+1]] <- data.frame(type=cmb$type, l1_mode=cmb$l1_mode, l3_mode=cmb$l3_mode, s=cmb$s, e=cmb$e, l1=cmb$l1, l2=l2, l3=cmb$l3, total=tot, prio=prio, stringsAsFactors=FALSE)
    }
    combos_df <- bind_rows(combo_rows)
    best_row  <- select_best_by_pct_tolerance(combos_df, tol_pct = TOL_PCT)
    
    min_tot <- NA_real_; best_prio <- NA_integer_; best_combo <- NULL
    if(!is.null(best_row) && nrow(best_row) == 1) {
      min_tot <- best_row$total[1]; best_prio <- best_row$prio[1]
      best_combo <- list(type=best_row$type[1], l1_mode=best_row$l1_mode[1], l3_mode=best_row$l3_mode[1], s=best_row$s[1], e=best_row$e[1], l1=best_row$l1[1], l2=best_row$l2[1], l3=best_row$l3[1], total=best_row$total[1])
    }
    
    # --- GET PRECISE DISTANCES ---
    mm_mets <- list(n_bt=NA, n_mt=NA, n_ms=NA, n_br=NA, n_ml=NA, n_st=NA)
    mm_same_acc <- FALSE; mm_closer <- NA; mm_on_route <- NA
    
    mm_dist_metro <- 0; mm_dist_brt <- 0; mm_dist_std <- 0; mm_dist_walk <- 0
    mm_seg_str <- ""
    mm_has_brt <- FALSE
    
    if(!is.na(min_tot) && !is.null(best_combo)) {
      det_mm <- get_path_details(g_multimodal, best_combo$s, best_combo$e)
      
      mm_mets$n_bt <- det_mm$n_bus_transfers
      mm_mets$n_mt <- det_mm$n_metro_transfers
      mm_mets$n_ms <- det_mm$n_mode_switches
      mm_mets$n_br <- det_mm$n_bus_routes
      mm_mets$n_ml <- det_mm$n_metro_lines
      mm_mets$n_st <- det_mm$n_stops
      mm_has_brt   <- det_mm$has_brt 
      
      # 4-WAY SPLIT
      mm_dist_metro <- det_mm$dist_metro_m
      mm_dist_brt   <- det_mm$dist_brt_m
      mm_dist_std   <- det_mm$dist_bus_std_m
      mm_dist_walk  <- det_mm$dist_walk_transfer_m
      
      # SEGMENT STRING
      mm_seg_str    <- det_mm$seg_str_metro
      
      mm_same_acc <- (best_combo$s == best_combo$e)
      mm_closer   <- (best_combo$l1 < road_dist_m)
      mm_on_route <- check_on_route_via_distances(best_combo$l1, best_combo$l3, road_dist_m)
    } else {
      s_metro <- near_metro_rp$node_id; e_metro <- near_metro_cl$node_id
      s_bus   <- near_bus_rp$node_id;   e_bus    <- near_bus_cl$node_id
      if((s_metro == e_metro) && (s_bus == e_bus)) mm_same_acc <- TRUE
    }
    
    if(is.na(tot_mo) && is.na(min_tot)) {
      best_mode <- NA; best_total <- NA_real_
    } else if(is.na(min_tot)) {
      best_mode <- "Metro-only"; best_total <- tot_mo
    } else if(is.na(tot_mo)) {
      best_mode <- "Multimodal"; best_total <- min_tot
    } else {
      final_df <- bind_rows(data.frame(mode="Metro-only", total=tot_mo, prio=10, stringsAsFactors=FALSE), data.frame(mode="Multimodal", total=min_tot, prio=best_prio, stringsAsFactors=FALSE))
      best_final <- select_best_by_pct_tolerance(final_df, tol_pct = TOL_PCT)
      best_mode <- best_final$mode[1]; best_total <- best_final$total[1]
    }
    
    results[[length(results) + 1]] <- data.frame(
      rp_id = rp_id, dest_type = dest_type, road_dist_m = road_dist_m,
      metro_only_total_m = tot_mo,
      metro_L1_m = L1_metro_m, metro_L2_m = L2_mo, metro_L3_m = L3_metro_m,
      metro_transfers = mo_mets$n_transfers, metro_lines = mo_mets$n_lines,
      metro_dwell = if(is.na(mo_mets$n_stops)) NA else max(0, mo_mets$n_stops - 1),
      metro_same_stn = metro_same_stn, metro_closer = metro_closer, metro_on_route = metro_on_route,
      
      multi_total_m = min_tot, multi_path_type = if(is.null(best_combo) || is.na(min_tot)) NA else best_combo$type,
      
      # PHYSICS INPUTS
      mm_dist_metro_m = mm_dist_metro,
      mm_dist_brt_m   = mm_dist_brt,
      mm_dist_std_m   = mm_dist_std,
      mm_dist_walk_m  = mm_dist_walk,
      mm_metro_segments = mm_seg_str,
      
      multi_L1_mode = if(is.null(best_combo) || is.na(min_tot)) NA else best_combo$l1_mode,
      multi_L1_m = if(is.null(best_combo) || is.na(min_tot)) NA else best_combo$l1,
      multi_L2_m = if(is.null(best_combo) || is.na(min_tot)) NA else best_combo$l2,
      multi_L3_m = if(is.null(best_combo) || is.na(min_tot)) NA else best_combo$l3,
      mm_bus_tr = mm_mets$n_bt, mm_metro_tr = mm_mets$n_mt, mm_mode_sw = mm_mets$n_ms,
      mm_bus_rt = mm_mets$n_br, mm_metro_ln = mm_mets$n_ml,
      mm_tot_stops = if(is.na(mm_mets$n_st)) NA else max(0, mm_mets$n_st - 1),
      
      multi_has_brt = mm_has_brt,
      mm_same_acc = mm_same_acc, mm_closer = mm_closer, mm_on_route = mm_on_route,
      best_mode = best_mode, best_total_m = best_total, stringsAsFactors = FALSE
    )
  }
}

results_df <- bind_rows(results)
}
saveRDS(results_df, "sample_test_results.rds")
message("[OK] Part 12 complete.")

# ==============================================================================
# 13) DISPLAY RESULTS
# ==============================================================================
message("\n[13] Summary stats:")
print(results_df %>% group_by(dest_type) %>% summarise(mean_best_km = mean(best_total_m, na.rm=TRUE)/1000, metro_share = mean(best_mode=="Metro-only", na.rm=TRUE), .groups="drop"))

# ==============================================================================
# 14) SAVE OUTPUTS
# ==============================================================================
message("\n[14] Saving outputs...")
saveRDS(g_multimodal, "g_multimodal_sample.rds")
saveRDS(g_metro_only, "g_metro_only_sample.rds")
saveRDS(results_df,  "sample_test_results.rds")
message("[OK] Saved: g_multimodal_sample.rds, sample_test_results.rds")

# ==============================================================================
# SETUP: LOAD LIBRARIES & DATA
# ==============================================================================
suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(flextable)
  library(officer)
  library(viridisLite)
})

message("📂 Loading saved data...")

# Check if file exists
if (!file.exists("sample_test_results.rds")) {
  stop("❌ Error: 'sample_test_results.rds' not found. Please run Parts 8-14 first.")
}

# Load the results dataframe
results_df <- readRDS("sample_test_results.rds")

message(paste("✅ Data loaded with", nrow(results_df), "rows."))

# ==============================================================================
# 14b) R1-5 NEW ANCHORS — travel-time / accessibility for farthest + random
# ------------------------------------------------------------------------------
# Additive: computes the SAME results_df schema for the 4 new dest_types
# (farthest_priv/pub, random_priv/pub) on the SAME points, reusing the SAME
# clinic draws as Table 1 (loaded from table1_new_anchors_N<N>.rds) and the
# in-scope precomputed lookups + memoized routers. The existing 4-anchor loop,
# its cache (sample_test_results.rds) and the original Table 2 are untouched
# (byte-identical). Random rows are kept PER-DRAW and labelled random_priv/pub
# so downstream stats POOL across draws. Cached to
# sample_test_results_newanchors_N<N>.rds (FORCE_S18_NEWANCHORS=1 to recompute).
# ==============================================================================
N_RANDOM_DRAWS <- suppressWarnings(as.integer(Sys.getenv("N_RANDOM_DRAWS", "3")))
if (is.na(N_RANDOM_DRAWS) || N_RANDOM_DRAWS < 1L) N_RANDOM_DRAWS <- 3L
.s18_data_dir <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data"
.t1_cache  <- file.path(.s18_data_dir, sprintf("table1_new_anchors_N%d.rds", N_RANDOM_DRAWS))
.na_cache  <- file.path(.s18_data_dir, sprintf("sample_test_results_newanchors_N%d.rds", N_RANDOM_DRAWS))

if (file.exists(.na_cache) && Sys.getenv("FORCE_S18_NEWANCHORS") != "1") {
  .new_anchor_results <- readRDS(.na_cache)
  message(sprintf("[S18 R1-5] Loaded cached new-anchor results (N=%d): %d rows", N_RANDOM_DRAWS, nrow(.new_anchor_results)))
} else if (!file.exists(.t1_cache)) {
  warning("[S18 R1-5] Table 1 anchor cache not found (", .t1_cache, "); skipping new-anchor travel-time. Run Table 1 first with matching N_RANDOM_DRAWS.")
  .new_anchor_results <- results_df[0, ]
} else {
  message(sprintf("[S18 R1-5] Computing new-anchor travel time (N=%d draws) ...", N_RANDOM_DRAWS))

  # ---- L3 lookup by clinic_id (mirrors get_L3_from_lookup, no .ct_* dependency) ----
  .L3_from_clinic <- function(clinic_id, transit_type, is_private) {
    if (transit_type == "metro") {
      if (!(clinic_id %in% names(.l3m_geo))) return(NA_real_)
      net <- .l3m_net[[clinic_id]]
      if (!is.na(net) && !is.infinite(net)) return(net * 1000)
      ratio <- if (is_private) ratio_L3_metro_priv else ratio_L3_metro_pub
      return(.l3m_geo[[clinic_id]] * ratio * 1000)
    } else {
      if (!(clinic_id %in% names(.l3b_geo))) return(NA_real_)
      net <- .l3b_net[[clinic_id]]
      if (!is.na(net) && !is.infinite(net)) return(net * 1000)
      ratio <- if (is_private) ratio_L3_bus_priv else ratio_L3_bus_pub
      return(.l3b_geo[[clinic_id]] * ratio * 1000)
    }
  }

  # ---- Travel-time for ONE (point, clinic): faithful copy of the Part-12 loop body ----
  .tt_one <- function(rp_id, dest_type, dkey, clinic_id, road_dist_m,
                      L1_metro_m, L1_bus_m, near_metro_rp_id, near_bus_rp_id, near_bus_rt_type) {
    is_priv <- grepl("priv", dest_type)
    L3_metro_m <- .L3_from_clinic(clinic_id, "metro", is_priv)
    L3_bus_m   <- .L3_from_clinic(clinic_id, "bus",   is_priv)
    near_metro_cl_id    <- .cl_metro_node[[dkey]]
    near_bus_cl_id      <- .cl_bus_node[[dkey]]
    near_bus_cl_rt_type <- .cl_bus_rt[[dkey]]

    metro_same_stn <- (near_metro_rp_id == near_metro_cl_id)
    if (metro_same_stn) {
      L2_mo <- NA_real_; tot_mo <- NA_real_; mo_mets <- list(n_transfers=NA, n_lines=NA, n_stops=NA)
    } else {
      L2_mo <- get_L2_distance_simple(near_metro_rp_id, near_metro_cl_id, g_metro_only)
      if (is.na(L2_mo)) {
        tot_mo <- NA_real_; mo_mets <- list(n_transfers=NA, n_lines=NA, n_stops=NA)
      } else {
        tot_mo <- L1_metro_m + L2_mo + L3_metro_m
        det_mo <- get_path_details(g_metro_only, near_metro_rp_id, near_metro_cl_id)
        mo_mets <- list(n_transfers=det_mo$n_transfers, n_lines=det_mo$n_metro_lines, n_stops=det_mo$n_stops)
      }
    }
    metro_closer   <- (L1_metro_m < road_dist_m)
    metro_on_route <- check_on_route_via_distances(L1_metro_m, L3_metro_m, road_dist_m)

    invalid_metro_bus <- (near_bus_cl_id == near_bus_rp_id)
    invalid_bus_metro <- (near_metro_cl_id == near_metro_rp_id)
    combos <- list(
      list(type="Metro-Metro", l1=L1_metro_m, l3=L3_metro_m, l1_mode="metro", l3_mode="metro", s=near_metro_rp_id, e=near_metro_cl_id),
      list(type="Metro-Bus",   l1=L1_metro_m, l3=L3_bus_m,   l1_mode="metro", l3_mode="bus",   s=near_metro_rp_id, e=near_bus_cl_id),
      list(type="Bus-Metro",   l1=L1_bus_m,   l3=L3_metro_m, l1_mode="bus",   l3_mode="metro", s=near_bus_rp_id,   e=near_metro_cl_id),
      list(type="Bus-Bus",     l1=L1_bus_m,   l3=L3_bus_m,   l1_mode="bus",   l3_mode="bus",   s=near_bus_rp_id,   e=near_bus_cl_id)
    )
    combo_rows <- list()
    for (cmb in combos) {
      if (cmb$s == cmb$e) next
      if (cmb$type == "Metro-Bus" && invalid_metro_bus) next
      if (cmb$type == "Bus-Metro" && invalid_bus_metro) next
      l2 <- get_L2_distance_real(cmb$s, cmb$e, g_multimodal)
      if (is.na(l2)) next
      tot <- cmb$l1 + l2 + cmb$l3
      prio <- get_combo_priority(l1_mode=cmb$l1_mode, l3_mode=cmb$l3_mode, l1_rt=near_bus_rt_type, l3_rt=near_bus_cl_rt_type)
      combo_rows[[length(combo_rows)+1]] <- data.frame(type=cmb$type, l1_mode=cmb$l1_mode, l3_mode=cmb$l3_mode, s=cmb$s, e=cmb$e, l1=cmb$l1, l2=l2, l3=cmb$l3, total=tot, prio=prio, stringsAsFactors=FALSE)
    }
    combos_df <- bind_rows(combo_rows)
    best_row  <- select_best_by_pct_tolerance(combos_df, tol_pct = TOL_PCT)
    min_tot <- NA_real_; best_prio <- NA_integer_; best_combo <- NULL
    if (!is.null(best_row) && nrow(best_row) == 1) {
      min_tot <- best_row$total[1]; best_prio <- best_row$prio[1]
      best_combo <- list(type=best_row$type[1], l1_mode=best_row$l1_mode[1], l3_mode=best_row$l3_mode[1], s=best_row$s[1], e=best_row$e[1], l1=best_row$l1[1], l2=best_row$l2[1], l3=best_row$l3[1], total=best_row$total[1])
    }
    mm_mets <- list(n_bt=NA, n_mt=NA, n_ms=NA, n_br=NA, n_ml=NA, n_st=NA)
    mm_same_acc <- FALSE; mm_closer <- NA; mm_on_route <- NA
    mm_dist_metro <- 0; mm_dist_brt <- 0; mm_dist_std <- 0; mm_dist_walk <- 0
    mm_seg_str <- ""; mm_has_brt <- FALSE
    if (!is.na(min_tot) && !is.null(best_combo)) {
      det_mm <- get_path_details(g_multimodal, best_combo$s, best_combo$e)
      mm_mets$n_bt <- det_mm$n_bus_transfers; mm_mets$n_mt <- det_mm$n_metro_transfers
      mm_mets$n_ms <- det_mm$n_mode_switches; mm_mets$n_br <- det_mm$n_bus_routes
      mm_mets$n_ml <- det_mm$n_metro_lines;   mm_mets$n_st <- det_mm$n_stops
      mm_has_brt   <- det_mm$has_brt
      mm_dist_metro <- det_mm$dist_metro_m; mm_dist_brt <- det_mm$dist_brt_m
      mm_dist_std   <- det_mm$dist_bus_std_m; mm_dist_walk <- det_mm$dist_walk_transfer_m
      mm_seg_str    <- det_mm$seg_str_metro
      mm_same_acc <- (best_combo$s == best_combo$e)
      mm_closer   <- (best_combo$l1 < road_dist_m)
      mm_on_route <- check_on_route_via_distances(best_combo$l1, best_combo$l3, road_dist_m)
    } else {
      if ((near_metro_rp_id == near_metro_cl_id) && (near_bus_rp_id == near_bus_cl_id)) mm_same_acc <- TRUE
    }
    if (is.na(tot_mo) && is.na(min_tot)) {
      best_mode <- NA; best_total <- NA_real_
    } else if (is.na(min_tot)) {
      best_mode <- "Metro-only"; best_total <- tot_mo
    } else if (is.na(tot_mo)) {
      best_mode <- "Multimodal"; best_total <- min_tot
    } else {
      final_df <- bind_rows(data.frame(mode="Metro-only", total=tot_mo, prio=10, stringsAsFactors=FALSE), data.frame(mode="Multimodal", total=min_tot, prio=best_prio, stringsAsFactors=FALSE))
      best_final <- select_best_by_pct_tolerance(final_df, tol_pct = TOL_PCT)
      best_mode <- best_final$mode[1]; best_total <- best_final$total[1]
    }
    data.frame(
      rp_id = rp_id, dest_type = dest_type, road_dist_m = road_dist_m,
      metro_only_total_m = tot_mo,
      metro_L1_m = L1_metro_m, metro_L2_m = L2_mo, metro_L3_m = L3_metro_m,
      metro_transfers = mo_mets$n_transfers, metro_lines = mo_mets$n_lines,
      metro_dwell = if(is.na(mo_mets$n_stops)) NA else max(0, mo_mets$n_stops - 1),
      metro_same_stn = metro_same_stn, metro_closer = metro_closer, metro_on_route = metro_on_route,
      multi_total_m = min_tot, multi_path_type = if(is.null(best_combo) || is.na(min_tot)) NA else best_combo$type,
      mm_dist_metro_m = mm_dist_metro, mm_dist_brt_m = mm_dist_brt, mm_dist_std_m = mm_dist_std, mm_dist_walk_m = mm_dist_walk,
      mm_metro_segments = mm_seg_str,
      multi_L1_mode = if(is.null(best_combo) || is.na(min_tot)) NA else best_combo$l1_mode,
      multi_L1_m = if(is.null(best_combo) || is.na(min_tot)) NA else best_combo$l1,
      multi_L2_m = if(is.null(best_combo) || is.na(min_tot)) NA else best_combo$l2,
      multi_L3_m = if(is.null(best_combo) || is.na(min_tot)) NA else best_combo$l3,
      mm_bus_tr = mm_mets$n_bt, mm_metro_tr = mm_mets$n_mt, mm_mode_sw = mm_mets$n_ms,
      mm_bus_rt = mm_mets$n_br, mm_metro_ln = mm_mets$n_ml,
      mm_tot_stops = if(is.na(mm_mets$n_st)) NA else max(0, mm_mets$n_st - 1),
      multi_has_brt = mm_has_brt,
      mm_same_acc = mm_same_acc, mm_closer = mm_closer, mm_on_route = mm_on_route,
      best_mode = best_mode, best_total_m = best_total, stringsAsFactors = FALSE
    )
  }

  # ---- Load Table 1 clinic draws for the UNWEIGHTED (uniform) design ----
  .t1 <- readRDS(.t1_cache)
  .asg <- .t1$assignments[["Uniform in populated districts only"]]
  if (is.null(.asg)) stop("[S18 R1-5] Uniform-design assignments missing in ", .t1_cache)

  worklist <- bind_rows(
    .asg$far %>% transmute(rp_id = as.character(id), dest_type, dest_id_geo, road_net_km = net_km_p2t, road_geo_km = geo_km),
    .asg$rnd %>% transmute(rp_id = as.character(id), dest_type, dest_id_geo, road_net_km = net_km_p2t, road_geo_km = geo_km)
  )
  # keep only points that have precomputed transit nodes (handles VALIDATE_S18_CAP subset)
  worklist <- worklist %>% filter(rp_id %in% names(.rp_metro_node))

  # pooled per-anchor imputation ratio (ratio-of-means; fallback 1.3)
  .anchor_ratio <- worklist %>% group_by(dest_type) %>%
    summarise(r = mean(road_net_km, na.rm=TRUE) / mean(road_geo_km, na.rm=TRUE), .groups="drop")
  .anchor_ratio_v <- setNames(.anchor_ratio$r, .anchor_ratio$dest_type)
  worklist <- worklist %>%
    mutate(
      clinic_id = gsub("^(priv_|pub_)", "", dest_id_geo),
      .imp = unname(.anchor_ratio_v[dest_type]),
      .imp = ifelse(is.na(.imp) | is.infinite(.imp), 1.3, .imp),
      road_dist_m = ifelse(!is.na(road_net_km) & !is.infinite(road_net_km),
                           road_net_km * 1000, road_geo_km * .imp * 1000)
    )

  # per-id L1 (computed once per point)
  uids <- unique(worklist$rp_id)
  .l1m_per <- setNames(vapply(uids, function(u) get_L1_from_lookup(u, "metro", L1_metro_lookup, L1_bus_lookup, ratio_lookup), numeric(1)), uids)
  .l1b_per <- setNames(vapply(uids, function(u) get_L1_from_lookup(u, "bus",   L1_metro_lookup, L1_bus_lookup, ratio_lookup), numeric(1)), uids)

  # ---- VALIDATION: reproduce stored nearest_priv travel time on a subset ----
  .vids <- intersect(head(rp_sample$id, 150), names(.rp_metro_node))
  if (length(.vids) > 0) {
    .vchk <- lapply(.vids, function(u) {
      .tk <- paste0(u, "||nearest_priv")
      if (!(.tk %in% names(.ct_clinic_id))) return(NULL)
      cid  <- .ct_clinic_id[[.tk]]; dk <- .ct_dest_geo[[.tk]]
      rdm  <- NA_real_
      if (.tk %in% names(.rc_net)) {
        vkm <- .rc_net[[.tk]]; impf <- ratio_lookup["nearest_priv"]; if (is.na(impf)) impf <- 1.3
        if (is.na(vkm) || is.infinite(vkm)) vkm <- .rc_geo[[.tk]] * impf
        rdm <- vkm * 1000
      }
      .tt_one(u, "nearest_priv", dk, cid, rdm, .l1m_per[[u]], .l1b_per[[u]],
              .rp_metro_node[[u]], .rp_bus_node[[u]], .rp_bus_rt[[u]])
    })
    .vchk <- bind_rows(.vchk)
    .vref <- results_df %>% filter(dest_type == "nearest_priv", rp_id %in% .vchk$rp_id) %>%
      select(rp_id, ref_best = best_total_m, ref_mo = metro_only_total_m, ref_mm = multi_total_m)
    .vcmp <- .vchk %>% select(rp_id, best_total_m, metro_only_total_m, multi_total_m) %>% left_join(.vref, by="rp_id")
    message(sprintf("[S18 R1-5] [validate] nearest_priv max|delta| best/mo/mm = %.3e / %.3e / %.3e (n=%d)",
                    max(abs(.vcmp$best_total_m - .vcmp$ref_best), na.rm=TRUE),
                    max(abs(.vcmp$metro_only_total_m - .vcmp$ref_mo), na.rm=TRUE),
                    max(abs(.vcmp$multi_total_m - .vcmp$ref_mm), na.rm=TRUE),
                    nrow(.vcmp)))
  }

  # ---- Compute travel time for every work row ----
  message(sprintf("[S18 R1-5] routing %d (point,clinic) rows across %d points ...", nrow(worklist), length(uids)))
  .out <- vector("list", nrow(worklist))
  .wl  <- worklist  # local copy for speed
  for (k in seq_len(nrow(.wl))) {
    if (k %% 20000 == 0) message(sprintf("   [S18 R1-5] %d / %d", k, nrow(.wl)))
    u  <- .wl$rp_id[k]
    .out[[k]] <- .tt_one(u, .wl$dest_type[k], .wl$dest_id_geo[k], .wl$clinic_id[k], .wl$road_dist_m[k],
                         .l1m_per[[u]], .l1b_per[[u]], .rp_metro_node[[u]], .rp_bus_node[[u]], .rp_bus_rt[[u]])
  }
  .new_anchor_results <<- bind_rows(.out)
  saveRDS(.new_anchor_results, .na_cache)
  message(sprintf("[S18 R1-5] new-anchor results: %d rows -> %s", nrow(.new_anchor_results), .na_cache))

  # ---- Monte-Carlo SE of the random travel-time means (confirm N adequate) ----
  .rnd_se <- .new_anchor_results %>%
    filter(dest_type %in% c("random_priv","random_pub")) %>%
    mutate(rp_id = as.character(rp_id)) %>%
    left_join(.asg$rnd %>% transmute(rp_id = as.character(id), dest_type, dest_id_geo, draw), by = c("rp_id","dest_type")) %>%
    group_by(dest_type, rp_id) %>%
    summarise(m = mean(best_total_m, na.rm=TRUE),
              s = stats::sd(best_total_m, na.rm=TRUE),
              nd = sum(!is.na(best_total_m)), .groups="drop") %>%
    mutate(se = s / sqrt(pmax(nd,1))) %>%
    group_by(dest_type) %>%
    summarise(mean_best_km = mean(m, na.rm=TRUE)/1000,
              grand_se_km  = sqrt(sum(se^2, na.rm=TRUE))/n()/1000,
              rel_grand_se = (sqrt(sum(se^2, na.rm=TRUE))/n()) / mean(m, na.rm=TRUE), .groups="drop")
  message("[S18 R1-5] Random travel-time Monte-Carlo SE (grand mean over points):")
  print(as.data.frame(.rnd_se), row.names = FALSE)
}

# Append new-anchor rows so the new Table 2 variants pick them up.
# (Original Table 2 / Part 16 select dest_types explicitly -> unaffected/byte-identical.)
if (nrow(.new_anchor_results) > 0) {
  results_df <- bind_rows(results_df, .new_anchor_results)
  message(sprintf("[S18 R1-5] results_df augmented to %d rows (%d dest_types).",
                  nrow(results_df), dplyr::n_distinct(results_df$dest_type)))
}

# ==============================================================================
# 15) FINAL TABLE GENERATION
# ==============================================================================
message("\n[15] Building final flextable...")

msd <- function(v, scale=1, digits=1) {
  v <- as.numeric(v)
  if(length(v) == 0 || all(is.na(v))) return("-")
  v <- v / scale
  paste0(formatC(mean(v, na.rm=TRUE), format="f", digits=digits, big.mark=","), " (", formatC(sd(v, na.rm=TRUE), format="f", digits=digits, big.mark=","), ")")
}

npct <- function(cond) {
  if(length(cond) == 0) return("-")
  n <- sum(cond, na.rm=TRUE)
  N <- sum(!is.na(cond))
  if(N == 0) return("-")
  paste0(formatC(n, format="d", big.mark=","), " (", sprintf("%.1f%%", 100*n/N), ")")
}

get_column_stats <- function(df, d_type) {
  dat <- df %>% filter(dest_type == d_type)
  sub_mo <- dat %>% filter(!is.na(metro_only_total_m))
  sub_mm <- dat %>% filter(!is.na(multi_total_m))
  
  c(
    msd(dat$road_dist_m, scale=1000),
    npct(!is.na(dat$metro_only_total_m)), npct(is.na(dat$metro_only_total_m)), npct(dat$metro_same_stn == TRUE), npct(is.na(dat$metro_only_total_m) & dat$metro_same_stn == FALSE),
    msd(sub_mo$metro_only_total_m, scale=1000), msd((sub_mo$metro_L1_m/sub_mo$metro_only_total_m)*100), msd((sub_mo$metro_L2_m/sub_mo$metro_only_total_m)*100), msd((sub_mo$metro_L3_m/sub_mo$metro_only_total_m)*100), npct(sub_mo$metro_only_total_m < sub_mo$road_dist_m),
    msd(sub_mo$metro_transfers), msd(sub_mo$metro_lines), msd(sub_mo$metro_dwell), npct(sub_mo$metro_closer == TRUE), 
    
    # REPLACEMENT 1: Ratio of Total chain to Road Distance (1 decimal point)
    msd(sub_mo$metro_only_total_m / sub_mo$road_dist_m, digits=1),
    
    npct(!is.na(dat$multi_total_m)), npct(is.na(dat$multi_total_m)), npct(dat$mm_same_acc == TRUE), npct(is.na(dat$multi_total_m) & dat$mm_same_acc == FALSE),
    msd(sub_mm$multi_total_m, scale=1000), msd((sub_mm$multi_L1_m/sub_mm$multi_total_m)*100), msd((sub_mm$multi_L2_m/sub_mm$multi_total_m)*100), msd((sub_mm$multi_L3_m/sub_mm$multi_total_m)*100), npct(sub_mm$multi_total_m < sub_mm$road_dist_m),
    npct(sub_mm$mm_closer == TRUE), 
    
    # REPLACEMENT 2: Ratio of Total best path to Road Distance (1 decimal point)
    msd(sub_mm$multi_total_m / sub_mm$road_dist_m, digits=1),
    
    npct(sub_mm$multi_path_type == "Bus-Bus"), npct(sub_mm$multi_path_type == "Bus-Metro"), npct(sub_mm$multi_path_type == "Metro-Bus"), npct(sub_mm$multi_path_type == "Metro-Metro"),
    msd(sub_mm$mm_bus_tr), msd(sub_mm$mm_metro_tr), msd(ifelse(sub_mm$multi_path_type == "Bus-Metro", 1, 0)), msd(ifelse(sub_mm$multi_path_type == "Metro-Bus", 1, 0)), msd(sub_mm$mm_mode_sw), msd(sub_mm$mm_bus_tr + sub_mm$mm_metro_tr + sub_mm$mm_mode_sw),
    msd(sub_mm$mm_bus_rt), msd(sub_mm$mm_metro_ln), msd(sub_mm$mm_tot_stops),
    npct(dat$best_mode == "Metro-only"), npct(dat$best_mode == "Multimodal")
  )
}

vars <- c(
  "Actual road distance in km [RP->Facility] mean (SD)",
  "Available (non-missing total chain) n (%)", "Missing (total chain) n (%)", "Station nearest RP = station nearest facility n (%)", "Non-reachable through metro n (%)",
  "Total chain in km mean (SD)", "L1 share (RP->Metro) mean % (SD)", "L2 share (Metro->Metro) mean % (SD)", "L3 share (Metro->Facility) mean % (SD)", "Total chain shorter than actual road distance [RP->Facility] n (%)",
  "Number of station transfers mean (SD)", "Number of distinct metro lines mean (SD)", "Number of intermediate metro stations mean (SD)", "Station nearest RP closer to RP than facility n (%)", 
  
  # REPLACEMENT 1 LABEL
  "Ratio of total chain to actual road distance [RP->Facility] mean (SD)",
  
  "Available (non-missing total chain) n (%)", "Missing (total chain) n (%)", "Access node nearest RP = access node nearest facility n (%)", "Non-reachable through transit n (%)",
  "Total best path using both networks in km mean (SD)", "L1 share (RP->Access) mean % (SD)", "L2 share (Access->Access) mean % (SD)", "L3 share (Access->Facility) mean % (SD)", "Total chain shorter than actual road distance [RP->Facility] n (%)",
  "Access node nearest RP closer to RP than facility n (%)", 
  
  # REPLACEMENT 2 LABEL
  "Ratio of total best path to actual road distance [RP->Facility] mean (SD)",
  
  "Bus start and Bus end n (%)", "Bus start and Metro end n (%)", "Metro start and Bus end n (%)", "Metro start and Metro end n (%)",
  "Number of bus route changes mean (SD)", "Number of metro line changes mean (SD)", "Number of mode switches from Bus->Metro mean (SD)", "Number of mode switches from Metro->Bus mean (SD)", "Number of mode switches total mean (SD)", "Number of total route and line changes and mode switches mean (SD)",
  "Number of distinct bus routes used mean (SD)", "Number of distinct metro lines used mean (SD)", "Number of intermediate stops/stations mean (SD)",
  "Metro-only n (%)", "Both networks n (%)"
)

groups <- c(
  "General",
  rep("Metro Accessibility", 14),        # Reduced from 15
  rep("Multimodal Shortest Path", 11),   # Reduced from 12
  rep("Best Mode (Both Networks)", 4),
  rep("Transfers (Best Multimodal Path)", 6),
  rep("Best Path Composition (Distinct Routes/Lines)", 3),
  rep("Best Class (Metro vs Bus vs Both)", 2)
)

df_tbl <- data.frame(
  Group = groups,
  Variable = vars,
  Priv_Nearest  = get_column_stats(results_df, "nearest_priv"),
  Pub_Nearest   = get_column_stats(results_df, "nearest_pub"),
  Priv_Specific = get_column_stats(results_df, "median_priv"),
  Pub_Specific  = get_column_stats(results_df, "median_pub"),
  stringsAsFactors = FALSE
)

library(flextable)
library(officer)

# Define the standard border line (black, 1.5pt for main rules)
std_border <- fp_border(color = "black", width = 1.5)
thin_border <- fp_border(color = "black", width = 1)

ft <- as_grouped_data(df_tbl, groups = "Group") %>%
  as_flextable() %>%
  set_header_labels(
    Variable = "Variable", 
    Priv_Nearest = "Private", 
    Pub_Nearest = "Public", 
    Priv_Specific = "Private", 
    Pub_Specific = "Public"
  ) %>%
  add_header_row(
    values = c("", "Nearest Facility", "Median-distance Facility"), 
    colwidths = c(1, 2, 2)
  ) %>%
  # Reset theme to zebra with publication colors
  theme_zebra(
    odd_header = "transparent", 
    odd_body = "#EFEFEF", 
    even_body = "#FFFFFF"
  ) %>%
  # Fix the invisible headers: Black text on White background
  color(part = "header", color = "black") %>%
  bold(part = "header") %>%
  
  # Apply Publication-style Horizontal Rules
  hline_top(part = "header", border = std_border) %>%      # Line above headers
  hline(i = 1, part = "header", border = thin_border) %>% # Line below "Target" row
  hline_bottom(part = "header", border = thin_border) %>%  # Line below the headers
  hline_bottom(part = "body", border = std_border) %>%    # Line at the very bottom
  
  # Layout adjustments
  align(j = 2:5, align = "center", part = "all") %>% 
  padding(padding = 3, part = "all") %>%
  autofit()

ft

# Save as Word
save_as_docx(ft, path = "Accessibility_metrics_for_random_points_unweighted.docx")

# ------------------------------------------------------------------------------
# 15b) R1-5 — Table 2 WITH new anchors (both versions; original above unchanged)
#   ALL8 = nearest/median/farthest/random x private/public (8 data columns)
#   NEW4 = farthest/random x private/public (supplementary 4 data columns)
# Reuses get_column_stats()/vars/groups; random_* columns POOL over all draws.
# ------------------------------------------------------------------------------
.build_acc_ft <- function(df_tbl, sublabels, hdr_values, hdr_colwidths) {
  data_cols <- setdiff(names(df_tbl), c("Group", "Variable"))
  lab_list  <- as.list(c("Variable", sublabels)); names(lab_list) <- c("Variable", data_cols)
  as_grouped_data(df_tbl, groups = "Group") %>%
    as_flextable() %>%
    set_header_labels(values = lab_list) %>%
    add_header_row(values = hdr_values, colwidths = hdr_colwidths) %>%
    theme_zebra(odd_header = "transparent", odd_body = "#EFEFEF", even_body = "#FFFFFF") %>%
    color(part = "header", color = "black") %>%
    bold(part = "header") %>%
    hline_top(part = "header", border = std_border) %>%
    hline(i = 1, part = "header", border = thin_border) %>%
    hline_bottom(part = "header", border = thin_border) %>%
    hline_bottom(part = "body", border = std_border) %>%
    align(j = 2:(length(data_cols) + 1), align = "center", part = "all") %>%
    padding(padding = 3, part = "all") %>%
    autofit()
}

if (all(c("farthest_priv", "farthest_pub", "random_priv", "random_pub") %in% results_df$dest_type)) {
  # --- ALL8: all anchors side-by-side ---
  df_tbl_all <- data.frame(
    Group = groups, Variable = vars,
    Priv_Nearest  = get_column_stats(results_df, "nearest_priv"),
    Pub_Nearest   = get_column_stats(results_df, "nearest_pub"),
    Priv_Median   = get_column_stats(results_df, "median_priv"),
    Pub_Median    = get_column_stats(results_df, "median_pub"),
    Priv_Farthest = get_column_stats(results_df, "farthest_priv"),
    Pub_Farthest  = get_column_stats(results_df, "farthest_pub"),
    Priv_Random   = get_column_stats(results_df, "random_priv"),
    Pub_Random    = get_column_stats(results_df, "random_pub"),
    stringsAsFactors = FALSE
  )
  ft_all <- .build_acc_ft(
    df_tbl_all, rep(c("Private", "Public"), 4),
    c("", "Nearest Facility", "Median-distance Facility", "Farthest Facility", "Random Facility"),
    c(1, 2, 2, 2, 2)
  )
  save_as_docx(ft_all, path = "Accessibility_metrics_for_random_points_unweighted_ALL8.docx")
  message("[S18 R1-5] Wrote 8-column Table 2 -> Accessibility_metrics_for_random_points_unweighted_ALL8.docx")

  # --- NEW4: supplementary table, just the new anchors ---
  df_tbl_new <- data.frame(
    Group = groups, Variable = vars,
    Priv_Farthest = get_column_stats(results_df, "farthest_priv"),
    Pub_Farthest  = get_column_stats(results_df, "farthest_pub"),
    Priv_Random   = get_column_stats(results_df, "random_priv"),
    Pub_Random    = get_column_stats(results_df, "random_pub"),
    stringsAsFactors = FALSE
  )
  ft_new <- .build_acc_ft(
    df_tbl_new, rep(c("Private", "Public"), 2),
    c("", "Farthest Facility", "Random Facility"), c(1, 2, 2)
  )
  save_as_docx(ft_new, path = "Accessibility_metrics_for_random_points_unweighted_NEW4.docx")
  message("[S18 R1-5] Wrote supplementary Table 2 -> Accessibility_metrics_for_random_points_unweighted_NEW4.docx")
} else {
  message("[S18 R1-5] New anchors absent from results_df; skipping ALL8/NEW4 Table 2 variants.")
}

# ------------------------------------------------------------------------------
# MM-REACH FULL CORRECTION (UNWEIGHTED) — opt-in constrained-graph re-route (R2-3f).
# Re-routes EVERY multimodal-non-reachable trip (all 8 anchors INCLUDING random)
# with the shortest-VALID-path transfer-state node-split router (single Dijkstra
# per source -> fast) and rebuilds a CORRECTED Table 2. Reproduction-safe: writes
# *_CORRECTED.docx + R14_mmreach_full_* only; the certified results_df / ALL8
# Table 2 above are untouched. See Code/_r14_mm_reach_full.R. Runs on results_df
# (the df the ALL8 table above is built from). Placed here so a STOP_AFTER_TABLE2
# run performs the unweighted correction and exits before Part 16 / Section 19.
# Env: RUN_MM_FULL=1 ; MM_FULL_RANDOM (0 = skip random, default include) ;
#      STOP_AFTER_MM_FULL=1 exits after the WEIGHTED correction in Section 19.
# ------------------------------------------------------------------------------
if (Sys.getenv("RUN_MM_FULL") == "1") {
  .mmf_code <- if (exists("base_dir")) file.path(dirname(base_dir), "Code") else file.path(getwd(), "Code")
  .mmf <- file.path(.mmf_code, "_r14_mm_reach_full.R")
  if (file.exists(.mmf)) {
    source(.mmf)
    r14_mm_reach_full("unweighted", include_random = (Sys.getenv("MM_FULL_RANDOM") != "0"))
    # corrected R14 chain break-even (multimodal panels of Fig_R14) from the corrected combos
    if (Sys.getenv("MM_FULL_CHAIN") != "0") {
      for (.dep in c("_r14_imputation.R","_r13_permode.R","_r14_chain_be.R")) { .d <- file.path(.mmf_code, .dep); if (file.exists(.d)) source(.d) }
      if (exists("r14_chain_be")) r14_chain_be("unweighted", corrected = TRUE)
    }
  } else message("[MM-FULL] _r14_mm_reach_full.R not found at ", .mmf_code, "; skipping.")
}

# --- Optional early exit for fast Section-18 Table 2 verification (default OFF) ---
# STOP_AFTER_TABLE2=1 stops here (after the unweighted accessibility tables are
# written) so the new anchors can be checked without the Part-16 / Section-19 run.
if (Sys.getenv("STOP_AFTER_TABLE2") == "1") {
  message("[RUN STATUS] STOP_AFTER_TABLE2=1 -- Section-18 Table 2 variants written, exiting before Part 16.")
  quit(save = "no", status = 0)
}

# ==============================================================================
# 16) PART 16 — TRAVEL TIME ANALYSIS (EXACT PHYSICS + 10x WAIT AVERAGING)
# Called 19933 and confirmed that metro trains every 5-10 minutes (rush hour
# every 5-8 minutes) so take average 7.5 minutes 
# For buses every 15-25 minutes so take average 20 minutes
# Also confirmed that max speed of buses is 80 km/h
# ==============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(viridisLite)
})

message("\n", paste(rep("=", 60), collapse = ""))
message("PART 16: TRAVEL TIME ANALYSIS (Monte Carlo Wait Averaging)")
message(paste(rep("=", 60), collapse = ""))

# -----------------------------------------------------------------------------
# 0) SETTINGS & PHYSICS CONSTANTS
# -----------------------------------------------------------------------------
SEED <- 12345
N_SIM_ITER <- 1000  # Number of simulation iterations to average wait times
speeds <- seq(5, 80, 1)

speed_walk  <- 4     # km/h
speed_metro_max <- 80         
metro_accel_rate <- 1.2       
metro_max_speed_ms <- speed_metro_max * 1000 / 3600  

# Kinematics
metro_time_to_max_speed <- metro_max_speed_ms / metro_accel_rate
metro_dist_to_max_speed_m <- 0.5 * metro_accel_rate * metro_time_to_max_speed^2
metro_dist_to_max_speed_km <- metro_dist_to_max_speed_m / 1000
metro_min_segment_for_cruise <- 2 * metro_dist_to_max_speed_km

brt_speed_cap <- 80        
brt_gap_factor <- 0.5      
stop_penalty_min <- 0.5  
metro_wait_max <- 7.5    
bus_wait_max   <- 20     

# --- ROBUST HELPERS ---
as_num <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  suppressWarnings(as.numeric(x))
}

calc_brt_speed <- function(traffic_speed) {
  speed <- traffic_speed + (brt_gap_factor * (brt_speed_cap - traffic_speed))
  pmin(speed, brt_speed_cap)
}

tm <- function(dist_km, speed_kmh) { (dist_km / speed_kmh) * 60 }

split_legs <- function(total_km, pct_l1, pct_l2) {
  total_km <- as_num(total_km)
  p1 <- as_num(pct_l1) / 100; p2 <- as_num(pct_l2) / 100
  p1[is.na(p1)] <- 0; p2[is.na(p2)] <- 0
  l1 <- total_km * p1; l2 <- total_km * p2; l3 <- total_km - l1 - l2
  list(l1 = l1, l2 = l2, l3 = l3)
}

# -----------------------------------------------------------------------------
# 1) PREPARE DATA & PRE-CALC METRO SEGMENTS
# -----------------------------------------------------------------------------
message("\n[16.1] Preparing input data...")

if (!exists("results_df")) stop("❌ results_df not found. Run Part 12 first.")

if(!"mm_metro_segments" %in% names(results_df)) results_df$mm_metro_segments <- ""

# --- KINEMATIC HELPER (One Segment) ---
calc_segment_time_min <- function(seg_m) {
  if(is.na(seg_m) || seg_m <= 0) return(0)
  seg_km <- seg_m / 1000
  
  if (seg_km >= metro_min_segment_for_cruise) {
    accel_time_hr <- metro_time_to_max_speed / 3600
    cruise_dist_km <- seg_km - (2 * metro_dist_to_max_speed_km)
    cruise_time_hr <- cruise_dist_km / speed_metro_max
    total_time_hr <- (2 * accel_time_hr) + cruise_time_hr
  } else {
    half_dist_m <- (seg_km * 1000) / 2
    time_each_phase_s <- sqrt(2 * half_dist_m / metro_accel_rate)
    total_time_hr <- (2 * time_each_phase_s) / 3600
  }
  return(total_time_hr * 60) # minutes
}

# --- PARSE & SUM SEGMENTS ---
message("   - Pre-calculating precise metro segment times...")

process_metro_string <- function(s) {
  if(is.na(s) || s == "") return(0)
  parts <- as.numeric(unlist(strsplit(s, ";")))
  parts <- parts[!is.na(parts) & parts > 0]
  if(length(parts) == 0) return(0)
  sum(sapply(parts, calc_segment_time_min))
}

metro_times_precalc <- sapply(results_df$mm_metro_segments, process_metro_string)

df_analysis <- results_df %>%
  mutate(
    road_dist_km   = as_num(road_dist_m) / 1000,
    chain_metro_km = as_num(metro_only_total_m) / 1000,
    chain_multi_km = as_num(multi_total_m) / 1000,
    
    dist_brt_km   = replace(as_num(mm_dist_brt_m)/1000, is.na(as_num(mm_dist_brt_m)), 0),
    dist_std_km   = replace(as_num(mm_dist_std_m)/1000, is.na(as_num(mm_dist_std_m)), 0),
    
    # EXACT METRO TIME
    metro_time_precise_min = metro_times_precalc,
    
    # EXACT WALK TRANSFER
    multi_transfer_walk_km = if("mm_dist_walk_m" %in% names(.)) {
      as_num(mm_dist_walk_m) / 1000
    } else {
      (coalesce(as_num(mm_bus_tr), 0) + coalesce(as_num(mm_metro_tr), 0) + coalesce(as_num(mm_mode_sw), 0)) * 0.05
    },
    multi_transfer_walk_km = replace(multi_transfer_walk_km, is.na(multi_transfer_walk_km), 0),
    
    # Splits
    multi_total_safe = ifelse(is.na(as_num(multi_total_m)) | as_num(multi_total_m) == 0, 1, as_num(multi_total_m)),
    pct_multi_l1 = (as_num(multi_L1_m) / multi_total_safe) * 100,
    pct_multi_l2 = (as_num(multi_L2_m) / multi_total_safe) * 100,
    
    metro_total_safe = ifelse(is.na(as_num(metro_only_total_m)) | as_num(metro_only_total_m) == 0, 1, as_num(metro_only_total_m)),
    pct_metro_l1 = (as_num(metro_L1_m) / metro_total_safe) * 100,
    pct_metro_l2 = (as_num(metro_L2_m) / metro_total_safe) * 100,
    metro_transfer_walk_km = coalesce(as_num(metro_transfers), 0) * 0.05,
    
    metro_only_transfers = coalesce(as_num(metro_transfers), 0),
    metro_dwell = coalesce(as_num(metro_dwell), 0),
    multi_dwell = coalesce(as_num(mm_tot_stops), 0),
    path_type = multi_path_type
  )

message(paste("   - Analysis records:", nrow(df_analysis)))

scenario_map <- list(
  "nearest_priv" = list(type = "Private", target = "Nearest"),
  "median_priv"  = list(type = "Private", target = "Specific"),
  "nearest_pub"  = list(type = "Public",  target = "Nearest"),
  "median_pub"   = list(type = "Public",  target = "Specific")
)

# -----------------------------------------------------------------------------
# 2) MAIN SIMULATION LOOP (With 10x Monte Carlo)
# -----------------------------------------------------------------------------
message(sprintf("\n[16.2] Calculating individual travel times (Averaging %d random runs)...", N_SIM_ITER))

indiv_list <- list()

for (dest in names(scenario_map)) {
  sc <- scenario_map[[dest]]
  type <- sc$type
  tgt  <- sc$target
  
  dat <- df_analysis %>% filter(dest_type == dest)
  if (nrow(dat) == 0) next
  
  sub_car   <- dat %>% filter(!is.na(road_dist_km))
  sub_metro <- dat %>% filter(!is.na(chain_metro_km))
  sub_multi <- dat %>% filter(!is.na(chain_multi_km))
  
  N_car   <- nrow(sub_car)
  N_metro <- nrow(sub_metro)
  N_multi <- nrow(sub_multi)
  
  seed_base <- SEED + match(dest, names(scenario_map)) * 1000
  
  # --- A. METRO PRE-CALC (Average Wait Time) ---
  if (N_metro > 0) {
    metro_stop_pen <- stop_penalty_min * sub_metro$metro_dwell
    metro_legs <- split_legs(sub_metro$chain_metro_km, sub_metro$pct_metro_l1, sub_metro$pct_metro_l2)
    metro_l2_walk <- pmin(pmax(sub_metro$metro_transfer_walk_km, 0, na.rm=TRUE), pmax(metro_legs$l2, 0, na.rm=TRUE), na.rm=TRUE)
    metro_l2_ride <- pmax(metro_legs$l2 - metro_l2_walk, 0, na.rm=TRUE)
    
    # 10x Averaging for Metro Wait
    # We calculate the random wait N_SIM_ITER times and take the mean
    set.seed(seed_base + 10)
    metro_wait_avg <- numeric(N_metro)
    
    n_transfers_vec <- pmax(0L, as.integer(sub_metro$metro_only_transfers), na.rm=TRUE)
    n_boardings_vec <- n_transfers_vec + 1L
    
    for(i in 1:N_metro) {
      nb <- n_boardings_vec[i]
      # Simulate N_SIM_ITER times
      waits <- replicate(N_SIM_ITER, sum(runif(nb, 0, metro_wait_max)))
      metro_wait_avg[i] <- mean(waits)
    }
    
    # Approx speed agg for Metro-Only mode
    calc_agg_metro_speed <- function(dist_km) {
      t_hr <- numeric(length(dist_km))
      for(i in seq_along(dist_km)) {
        d <- dist_km[i]; if(is.na(d)) { t_hr[i] <- NA; next }
        d <- max(0.01, d)
        if(d >= metro_min_segment_for_cruise) {
          t_hr[i] <- (2 * metro_time_to_max_speed/3600) + (d - 2*metro_dist_to_max_speed_km)/speed_metro_max
        } else {
          t_hr[i] <- (2 * sqrt(2 * (d*1000/2) / metro_accel_rate)) / 3600
        }
      }
      pmin(pmax(dist_km/t_hr, 20), 80)
    }
    metro_spd_agg <- calc_agg_metro_speed(metro_l2_ride)
  }
  
  # --- B. MULTIMODAL PRE-CALC (Average Wait Time) ---
  if (N_multi > 0) {
    multi_stop_pen <- stop_penalty_min * sub_multi$multi_dwell
    multi_legs <- split_legs(sub_multi$chain_multi_km, sub_multi$pct_multi_l1, sub_multi$pct_multi_l2)
    
    # 10x Averaging for Multimodal Wait
    multi_wait_avg <- numeric(N_multi)
    set.seed(seed_base + 20)
    
    for (j in 1:N_multi) {
      pt <- sub_multi$path_type[j]
      n_metro_tr <- coalesce(sub_multi$mm_metro_tr[j], 0)
      n_bus_tr   <- coalesce(sub_multi$mm_bus_tr[j], 0)
      n_mode_sw  <- coalesce(sub_multi$mm_mode_sw[j], 0)
      
      if (is.na(pt)) { multi_wait_avg[j] <- 0; next }
      
      n_mb <- 0; n_bb <- 0
      if (pt == "Metro-Metro") { n_mb = 1 + n_metro_tr } 
      else if (pt == "Bus-Bus") { n_bb = 1 + n_bus_tr }
      else if (pt == "Metro-Bus") { n_mb = 1 + n_metro_tr; n_bb = n_mode_sw + n_bus_tr }
      else if (pt == "Bus-Metro") { n_bb = 1 + n_bus_tr; n_mb = n_mode_sw + n_metro_tr }
      
      # Simulate N_SIM_ITER times
      waits <- replicate(N_SIM_ITER, {
        w_m <- if (n_mb > 0) sum(runif(n_mb, 0, metro_wait_max)) else 0
        w_b <- if (n_bb > 0) sum(runif(n_bb, 0, bus_wait_max)) else 0
        w_m + w_b
      })
      multi_wait_avg[j] <- mean(waits)
    }
  }
  
  # --- SPEED LOOP ---
  for (s in speeds) {
    brt_physics_speed <- calc_brt_speed(s)
    std_physics_speed <- s
    
    # 1. Car
    if (N_car > 0) {
      t_vec <- tm(sub_car$road_dist_km, s)
      if(length(t_vec) == N_car) {
        indiv_list[[length(indiv_list)+1]] <- data.frame(
          Type=type, Target=tgt, Speed=s, Mode="Car-only (direct to clinic)", 
          Time=t_vec, Time_Deterministic=t_vec 
        )
      }
    }
    
    # 2a. Metro (Car-init)
    if (N_metro > 0) {
      det_t <- tm(metro_legs$l1, s) + tm(metro_l2_ride, metro_spd_agg) + 
        tm(metro_l2_walk, speed_walk) + tm(metro_legs$l3, speed_walk) + metro_stop_pen
      
      # Add Averaged Wait
      t_vec <- det_t + metro_wait_avg
      
      if (length(t_vec) == N_metro) {
        indiv_list[[length(indiv_list)+1]] <- data.frame(
          Type=type, Target=tgt, Speed=s, Mode="Car-initiated Metro-only", 
          Time=t_vec, Time_Deterministic=det_t
        )
      }
    }
    
    # 2b. Metro (Walk-init)
    if (N_metro > 0) {
      det_t <- tm(metro_legs$l1, speed_walk) + 
        tm(metro_l2_ride, metro_spd_agg) + 
        tm(metro_l2_walk, speed_walk) + 
        tm(metro_legs$l3, speed_walk) + 
        metro_stop_pen
      
      t_vec <- det_t + metro_wait_avg
      
      if (length(t_vec) == N_metro) {
        indiv_list[[length(indiv_list)+1]] <- data.frame(
          Type=type, Target=tgt, Speed=s, Mode="Walk-initiated Metro-only", 
          Time=t_vec, Time_Deterministic=det_t
        )
      }
    }
    
    # 3. Multimodal (Car-init)
    if (N_multi > 0) {
      t_brt <- (sub_multi$dist_brt_km / brt_physics_speed) * 60
      t_std <- (sub_multi$dist_std_km / std_physics_speed) * 60
      t_metro <- sub_multi$metro_time_precise_min
      t_transfer <- (sub_multi$multi_transfer_walk_km / speed_walk) * 60
      l2_time_exact <- t_brt + t_std + t_metro + t_transfer
      
      det_t <- tm(multi_legs$l1, s) + l2_time_exact + 
        tm(multi_legs$l3, speed_walk) + multi_stop_pen
      
      # Add Averaged Wait
      t_vec <- det_t + multi_wait_avg
      
      indiv_list[[length(indiv_list)+1]] <- data.frame(
        Type=type, Target=tgt, Speed=s, Mode="Car-initiated Multimodal", 
        Time=t_vec, Time_Deterministic=det_t
      )
    }
    
    # 4. Multimodal (Walk-init)
    if (N_multi > 0) {
      det_t <- tm(multi_legs$l1, speed_walk) + l2_time_exact + 
        tm(multi_legs$l3, speed_walk) + multi_stop_pen
      
      t_vec <- det_t + multi_wait_avg
      
      indiv_list[[length(indiv_list)+1]] <- data.frame(
        Type=type, Target=tgt, Speed=s, Mode="Walk-initiated Multimodal", 
        Time=t_vec, Time_Deterministic=det_t
      )
    }
  }
}

df_indiv <- bind_rows(indiv_list) %>%
  mutate(Type=as.character(Type), Target=as.character(Target), Speed=as.numeric(Speed), Time=as.numeric(Time), Time_Deterministic=as.numeric(Time_Deterministic))

message(paste("\n[OK] Individual travel time records:", nrow(df_indiv)))

message("\nPART 16 COMPLETE. Run Part 17 for visualization.")

# ==============================================================================
# 17) PART 17 — STATISTICAL VISUALIZATION & OUTPUT
# ==============================================================================

message("\n", paste(rep("=", 60), collapse = ""))
message("PART 17: VISUALIZATION & STATISTICS")
message(paste(rep("=", 60), collapse = ""))

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(viridisLite)
})

# -----------------------------------------------------------------------------
# 1. PREPARE & AGGREGATE DATA
# -----------------------------------------------------------------------------
message("\n[17.1] Preparing data for visualization...")

# IMPORTANT: Ensure Mode_family and Initiation exist in df_indiv FIRST
# This prevents the "object 'Mode_family' not found" error during subsequent steps
df_indiv <- df_indiv %>%
  mutate(
    Mode_family = case_when(
      grepl("^Car-only", Mode) ~ "Car-only (direct to clinic)",
      grepl("Metro-only", Mode) ~ "Metro-only",
      grepl("Multimodal", Mode) ~ "Multimodal",
      TRUE ~ Mode
    ),
    Initiation = ifelse(grepl("^Walk-initiated", Mode), "Walk-initiated", "Car-initiated")
  )

# Define factors for plotting order
mode_order <- c("Car-only (direct to clinic)", "Metro-only", "Multimodal")
df_indiv$Mode_family <- factor(df_indiv$Mode_family, levels = mode_order)
df_indiv$Initiation <- factor(df_indiv$Initiation, levels = c("Car-initiated", "Walk-initiated"))

# Create Summary Dataframe (Mean Time per Speed)
df_sum <- df_indiv %>%
  group_by(Type, Target, Speed, Mode_family, Initiation) %>%
  summarise(Time_mean = mean(Time, na.rm=TRUE), .groups="drop")

# Save Data
saveRDS(df_indiv, "travel_time_individual.rds")
saveRDS(df_sum, "travel_time_summary.rds")


# -----------------------------------------------------------------------------
# 1. SETUP & LOAD DATA
# -----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(viridisLite)
})

message("📂 Loading analysis data...")

if (!file.exists("travel_time_individual.rds") || !file.exists("travel_time_summary.rds")) {
  stop("❌ Error: Saved RDS files not found. Please run Part 16 first.")
}

df_indiv <- readRDS("travel_time_individual.rds")
df_sum   <- readRDS("travel_time_summary.rds")

message("✅ Data loaded.")

# --- Re-Define Global Constants needed for plotting ---
speeds <- seq(5, 80, 1) # Needed for Physics plots
mode_order <- c("Car-only (direct to clinic)", "Metro-only", "Multimodal")

# Ensure factor levels are correct for coloring
df_sum$Mode_family <- factor(df_sum$Mode_family, levels = mode_order)
df_sum$Initiation <- factor(df_sum$Initiation, levels = c("Car-initiated", "Walk-initiated"))

df_indiv$Mode_family <- factor(df_indiv$Mode_family, levels = mode_order)
df_indiv$Initiation <- factor(df_indiv$Initiation, levels = c("Car-initiated", "Walk-initiated"))

# -----------------------------------------------------------------------------
# 2. SETUP PLOTTING THEME
# -----------------------------------------------------------------------------
base_theme <- theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.5),
    legend.position = "bottom", 
    legend.box = "horizontal",
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    strip.background = element_blank(),
    legend.key.width = unit(1.5, "cm")
  )

plasma3 <- viridisLite::plasma(3, end = 0.9)
mode_colors <- c("Car-only (direct to clinic)"="grey50", "Metro-only"=plasma3[1], "Multimodal"=plasma3[2])

# -----------------------------------------------------------------------------
# 3. FIGURE 1: MEAN TRAVEL TIME
# -----------------------------------------------------------------------------
message("\n[17.2] Generating Figure 1 (Mean Travel Time)...")

# p_mean <- ggplot(df_sum, aes(x = Speed, y = Time_mean / 60, color = Mode_family, linetype = Initiation)) +
#   geom_line(linewidth = 1.2, alpha = 1) +
#   
#   # --- CHANGE START ---
#   facet_grid(Type ~ Target, 
#              labeller = labeller(Target = c("Nearest" = "Nearest", 
#                                             "Specific" = "Median-distance"))) +
#   # --- CHANGE END ---
# 
#   scale_color_manual(values = mode_colors) +
#   scale_linetype_manual(values = c("solid", "longdash")) +
#   scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 0.5), expand = c(0, 0)) +
#   scale_x_continuous(limits = c(5, 80), breaks = seq(10, 80, 10)) +
#   labs(
#     title = "Mean Travel Time from Random Point to Nearest/Median-distance Dental Facility by Sector",
#     x = "Average speed for car/standard bus (km/h)", 
#     y = "Mean travel time (hours)",
#     color = "Travel mode", 
#     linetype = "Initiation"
#   ) +
#   base_theme +
#   # --- ADD THIS LINE ---
#   theme(panel.spacing.y = unit(1, "lines")) +
#   guides(color = guide_legend(override.aes = list(linewidth = 1.3)), 
#          linetype = guide_legend(override.aes = list(linewidth = 1.3)))

p_mean <- ggplot(df_sum, aes(x = Speed, y = Time_mean / 60, color = Mode_family, linetype = Initiation)) +
  geom_line(linewidth = 1.2, alpha = 1) +
  
  # --- Facet Grid ---
  facet_grid(Type ~ Target, 
             labeller = labeller(Target = c("Nearest" = "Nearest", 
                                            "Specific" = "Median-distance"))) +
  
  scale_color_manual(values = mode_colors) +
  scale_linetype_manual(values = c("solid", "longdash")) +
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 0.5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(5, 80), breaks = seq(10, 80, 10)) +
  labs(
    title = "Mean Travel Time from Random Point to Nearest/Median-distance Dental Facility by Sector",
    x = "Average speed for car/standard bus (km/h)", 
    y = "Mean travel time (hours)",
    color = "Travel mode", 
    linetype = "Initiation"
  ) +
  base_theme +
  
  # --- UPDATED THEME ---
  theme(
    panel.spacing.y = unit(2, "lines"),
    
    panel.spacing.x = unit(1, "lines"),
    
    # 1. Remove the box around the panel
    panel.border = element_blank(),
    
    # 2. Remove the X and Y axis lines
    axis.line = element_blank(),
    
    # 3. Remove the tick marks
    axis.ticks = element_blank()
  ) +
  
  guides(color = guide_legend(override.aes = list(linewidth = 1.3)), 
         linetype = guide_legend(override.aes = list(linewidth = 1.3)))

p_mean

ggsave("Fig_mean_travel_time.tiff", p_mean, width = 14, height = 9, dpi = 300)

# -----------------------------------------------------------------------------
# 4. FIGURE 2: PERCENTAGE FASTER (WITH INTEGRATED BREAKEVEN LINES)
# -----------------------------------------------------------------------------
message("\n[17.3] Generating Figure 2 (Percentage Faster with Integrated Breakeven)...")

car_label <- "Car-only (direct to clinic)"
modes_to_compare <- c("Metro-only", "Multimodal")

# 1. Pivot Wide for Comparison (as before)
df_numbered <- df_indiv %>%
  filter(Mode_family %in% c(car_label, modes_to_compare)) %>%
  group_by(Type, Target, Speed, Mode_family, Initiation) %>%
  mutate(trip_id = row_number()) %>%
  ungroup()

df_wide <- df_numbered %>%
  select(Type, Target, Speed, trip_id, Mode_family, Initiation, Time) %>%
  pivot_wider(names_from = Mode_family, values_from = Time)

if (car_label %in% names(df_wide)) {
  
  df_cmp <- df_wide %>%
    pivot_longer(cols = any_of(modes_to_compare), names_to = "Mode_family", values_to = "Time_mode") %>%
    mutate(
      Time_car = .data[[car_label]], 
      delta_min = Time_car - Time_mode, 
      faster = !is.na(delta_min) & delta_min > 0
    )
  
  # Calculate Percentage Faster
  pct_faster <- df_cmp %>% 
    group_by(Type, Target, Speed, Mode_family, Initiation) %>%
    summarise(pct_faster = 100 * mean(faster, na.rm = TRUE), .groups = "drop") %>%
    mutate(Mode_family = factor(Mode_family, levels = modes_to_compare))
  
  # 2. CALCULATE BREAKEVEN POINTS (X-intercepts where Y=50)
  breakeven_points <- pct_faster %>%
    filter(Initiation == "Car-initiated") %>%
    group_by(Type, Target, Mode_family) %>%
    summarise(
      speed_at_50 = if (sum(!is.na(pct_faster)) >= 2 && max(pct_faster, na.rm = TRUE) >= 50) {
        tryCatch(approx(x = pct_faster, y = Speed, xout = 50, ties = mean)$y, error = function(e) NA_real_)
      } else { NA_real_ },
      .groups = "drop"
    ) %>%
    filter(!is.na(speed_at_50))
  
  # 3. BUILD THE PLOT
  p_pct <- ggplot(pct_faster, aes(x = Speed, y = pct_faster, color = Mode_family, linetype = Initiation)) +
    # Reference line at 50%
    geom_hline(yintercept = 50, color = "gray80", linetype = "dotted") +
    
    # --- FIXED BREAKEVEN VERTICAL LINES ---
    geom_vline(data = breakeven_points, 
               aes(xintercept = speed_at_50, color = Mode_family), 
               linetype = "dashed", linewidth = 0.4, alpha = 0.6,
               inherit.aes = FALSE,
               show.legend = FALSE) +
    
    # --- FIXED BREAKEVEN LABELS ON X-AXIS ---
    geom_text(data = breakeven_points, 
              aes(x = speed_at_50, y = 0, label = sprintf("%.0f", speed_at_50), color = Mode_family),
              vjust = -0.5, hjust = -0.2, size = 3, fontface = "bold", 
              show.legend = FALSE, inherit.aes = FALSE) +
    
    geom_line(linewidth = 1.2) + 
    facet_grid(Type ~ Target, 
               labeller = labeller(Target = c("Nearest" = "Nearest", 
                                              "Specific" = "Median-distance"))) +
    scale_color_manual(values = mode_colors) + 
    scale_linetype_manual(values = c("solid", "longdash")) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 0)) +
    scale_x_continuous(limits = c(5, 80), breaks = seq(10, 80, 10)) +
    labs(
      title = "Percentage of Public Transit Trips Faster than Direct Driving",
      subtitle = "Dashed vertical lines indicate the traffic speed at which 50% of transit trips become slower than direct-driving.",
      x = "Average speed for car/standard bus (km/h)", 
      y = "Percentage faster than car (%)",
      color = "Travel mode", 
      linetype = "Initiation"
    ) +
    base_theme +
    
    # --- UPDATED THEME ---
    theme(
      panel.spacing.y = unit(2, "lines"),
      panel.spacing.x = unit(1, "lines"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      
      # 1. Remove the box around the panel
      panel.border = element_blank(),
      
      # 2. Remove the X and Y axis lines
      axis.line = element_blank(),
      
      # 3. Remove the tick marks
      axis.ticks = element_blank()
    ) +
    
    guides(color = guide_legend(override.aes = list(linewidth = 1.3)), 
           linetype = guide_legend(override.aes = list(linewidth = 1.3)))
  
  ggsave("Fig_percent_faster_integrated.tiff", p_pct, width = 14, height = 9, dpi = 300)
}

# -----------------------------------------------------------------------------
# 5. FIGURE 3: MEAN TIME SAVED (Car-Initiated Only)
# -----------------------------------------------------------------------------
message("\n[17.4] Generating Figure 3 (Time Saved)...")

if (exists("df_cmp")) {
  df_savings_trend <- df_cmp %>% 
    filter(delta_min > 0, Initiation == "Car-initiated") %>%
    group_by(Type, Target, Speed, Mode_family) %>%
    summarise(mean_saved_hours = mean(delta_min, na.rm = TRUE) / 60, .groups = "drop") %>%
    mutate(
      Target = factor(Target, levels = c("Nearest", "Specific")), 
      Type = factor(Type, levels = c("Private", "Public"))
    )
  
  # p_savings_trend <- ggplot(df_savings_trend, aes(x = Speed, y = mean_saved_hours, color = Mode_family)) +
  #   geom_line(linewidth = 1.2, alpha = 1) + 
  #   # --- CHANGE START ---
  #   facet_grid(Type ~ Target, 
  #              labeller = labeller(Target = c("Nearest" = "Nearest", 
  #                                             "Specific" = "Median-distance")), drop = FALSE) +
  #   # --- CHANGE END ---
  #   scale_color_manual(values = mode_colors) +
  #   scale_y_continuous(limits = c(0, NA), breaks = seq(0, 4, 0.5), expand = c(0, 0.1), name = "Mean time saved (hours)") +
  #   scale_x_continuous(limits = c(5, 80), breaks = seq(10, 80, 10), name = "Average speed for car/standard bus (km/h)") +
  #   labs(title = "Mean Time Saved for Car-Initiated Transit Trips Faster than Direct Driving", color = "Travel mode") +
  #   base_theme + 
  #   # --- ADD THIS LINE ---
  #   theme(panel.spacing.y = unit(1, "lines")) +
  #   guides(color = guide_legend(override.aes = list(linewidth = 1.5)))
  
  p_savings_trend <- ggplot(df_savings_trend, aes(x = Speed, y = mean_saved_hours, color = Mode_family)) +
    geom_line(linewidth = 1.2, alpha = 1) + 
    
    # --- Facet Grid ---
    facet_grid(Type ~ Target, 
               labeller = labeller(Target = c("Nearest" = "Nearest", 
                                              "Specific" = "Median-distance")), drop = FALSE) +
    
    scale_color_manual(values = mode_colors) +
    scale_y_continuous(limits = c(0, NA), breaks = seq(0, 4, 0.5), expand = c(0, 0.1), name = "Mean time saved (hours)") +
    scale_x_continuous(limits = c(5, 80), breaks = seq(10, 80, 10), name = "Average speed for car/standard bus (km/h)") +
    labs(title = "Mean Time Saved for Car-Initiated Transit Trips Faster than Direct Driving", color = "Travel mode") +
    base_theme + 
    
    # --- UPDATED THEME ---
    theme(
      panel.spacing.y = unit(2, "lines"),
      
      panel.spacing.x = unit(1, "lines"),
      
      # 1. Remove the box around the panel
      panel.border = element_blank(),
      
      # 2. Remove the X and Y axis lines
      axis.line = element_blank(),
      
      # 3. Remove the tick marks
      axis.ticks = element_blank()
    ) +
    
    guides(color = guide_legend(override.aes = list(linewidth = 1.5)))
  
  ggsave("Fig_mean_time_savings.tiff", p_savings_trend, width = 14, height = 9, dpi = 300)
}

# -----------------------------------------------------------------------------
# 6. FIGURE 4: PHYSICS MODEL ILLUSTRATION
# -----------------------------------------------------------------------------
message("\n[17.6] Generating Figure 5 (Physics Models)...")

# 5a. Bus Physics
brt_speed_demo <- data.frame(Traffic_Speed = speeds) %>%
  mutate(
    # Create Speed Columns
    `Rapid Transit` = Traffic_Speed + (0.5 * (80 - Traffic_Speed)), 
    `Rapid Transit` = pmin(`Rapid Transit`, 80),
    `Standard` = Traffic_Speed
  ) %>%
  pivot_longer(cols = c(`Rapid Transit`, `Standard`), names_to = "Mode", values_to = "Effective_Speed")

p_speed_traffic <- ggplot(brt_speed_demo, aes(x = Traffic_Speed, y = Effective_Speed, color = Mode, linetype = Mode)) +
  geom_line(linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey50") +
  
  # --- Updated Scales ---
  scale_color_manual(values = c("Rapid Transit" = "#0D0887", "Standard" = "#CC4778")) +
  scale_linetype_manual(values = c("Rapid Transit" = "solid", "Standard" = "dashed")) +
  
  scale_x_continuous(limits = c(0, 85), breaks = seq(10, 80, 10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 85), breaks = seq(10, 80, 10), expand = c(0, 0)) +
  
  # --- Updated Labels: "Mode" -> "Bus type" ---
  labs(
    title = "Average Bus Speed vs. Traffic Speed",
    x = "Traffic speed (km/h)", 
    y = "Average speed (km/h)", 
    color = "Bus type",    # <--- Changed Here
    linetype = "Bus type"  # <--- Changed Here
  ) +
  
  base_theme + 
  
  # --- Clean Theme ---
  theme(
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  ) +
  
  guides(color = guide_legend(override.aes = list(linewidth = 1.5)), 
         linetype = guide_legend(override.aes = list(linewidth = 1.5)))

ggsave("Fig_speed_model_traffic.tiff", p_speed_traffic, width = 14, height = 9, dpi = 600)

# 5b. Metro Physics (Kinematics)
# Redefine helper locally for safety in standalone Part 17 run
calc_metro_kinematics <- function(seg_km) {
  accel <- 1.2; vmax_kmh <- 80
  time_to_max <- (vmax_kmh/3.6) / accel
  dist_to_max_km <- (0.5 * accel * time_to_max^2) / 1000
  min_cruise_km <- 2 * dist_to_max_km
  
  if (seg_km >= min_cruise_km) {
    total_time_hr <- (2 * time_to_max/3600) + (seg_km - min_cruise_km)/vmax_kmh
  } else {
    total_time_hr <- (2 * sqrt(2 * (seg_km*1000/2) / accel)) / 3600
  }
  return(seg_km / total_time_hr)
}

metro_distances <- seq(0.1, 5, 0.05)
metro_speed_demo <- data.frame(
  Segment_km = metro_distances, 
  Metro_Speed = sapply(metro_distances, calc_metro_kinematics)
)

p_speed_metro <- ggplot(metro_speed_demo, aes(x = Segment_km, y = Metro_Speed)) +
  geom_line(linewidth = 1.2, color = "#0D0887") +
  geom_vline(xintercept = 0.411, linetype = "dotted", color = "red") + 
  geom_hline(yintercept = 80, linetype = "dashed", color = "grey50") +
  scale_y_continuous(limits = c(20, 85), breaks = seq(20, 80, 10)) +
  scale_x_continuous(breaks = seq(0, 5, 0.5)) +
  labs(
    title = "Metro Movement Speed vs. Metro Segment Length",
    subtitle = "The gray dashed line represents the maximum operating speed (80 km/h).\nThe red dotted line represents the minimum distance required to reach maximum speed (0.41 km).",
    x = "Segment length (km)", 
    y = "Average movement speed (km/h)"
  ) +
  base_theme +
  
  # --- UPDATED THEME ---
  theme(
    # 1. Remove the box around the panel
    panel.border = element_blank(),
    
    # 2. Remove the X and Y axis lines
    axis.line = element_blank(),
    
    # 3. Remove the tick marks
    axis.ticks = element_blank()
  )

ggsave("Fig_speed_model_metro.tiff", p_speed_metro, width = 14, height = 9, dpi = 600)

message("\n[DONE] All Figures Generated Successfully.")

# ==============================================================================
# 17b) R1-5 — TRAVEL-TIME MINUTES for the new anchors (additive; Section 18)
# ------------------------------------------------------------------------------
# Mean travel time for farthest_/random_ (priv/pub) by speed/mode, computed from
# the new-anchor rows already in df_analysis (results_df was augmented in 14b).
# Memory-bounded: aggregates to MEANS per speed (no per-row df_indiv inflation),
# closed-form expected wait (= E[sum U(0,max)] = n_boardings*max/2; the original
# 1000-iter sim estimates this same mean), random POOLED over its N draws. The
# certified Figures 2-4 + travel_time_individual.rds are untouched; new outputs:
#   Fig_mean_travel_time_ALL8.tiff / _NEW4.tiff  and  TravelTime_summary_new_anchors_unweighted.docx
# ==============================================================================
.tt_new_dts <- c("farthest_priv","farthest_pub","random_priv","random_pub")
if (exists("df_analysis") && exists("df_sum") && all(.tt_new_dts %in% df_analysis$dest_type)) {
  message("[S18 R1-5] Computing travel-time minutes for the 4 new anchors ...")

  # Mean travel time per speed/mode/initiation for ONE anchor's df_analysis rows
  # (random: rows are per-draw -> the means POOL across draws). Closed-form wait.
  .tt_anchor_summaries <- function(dat, type, tgt) {
    sub_car   <- dat %>% filter(!is.na(road_dist_km))
    sub_metro <- dat %>% filter(!is.na(chain_metro_km))
    sub_multi <- dat %>% filter(!is.na(chain_multi_km))
    N_car <- nrow(sub_car); N_metro <- nrow(sub_metro); N_multi <- nrow(sub_multi)
    if (N_metro > 0) {
      metro_stop_pen <- stop_penalty_min * sub_metro$metro_dwell
      metro_legs <- split_legs(sub_metro$chain_metro_km, sub_metro$pct_metro_l1, sub_metro$pct_metro_l2)
      metro_l2_walk <- pmin(pmax(sub_metro$metro_transfer_walk_km,0,na.rm=TRUE), pmax(metro_legs$l2,0,na.rm=TRUE), na.rm=TRUE)
      metro_l2_ride <- pmax(metro_legs$l2 - metro_l2_walk, 0, na.rm=TRUE)
      metro_wait_avg <- (pmax(0L, as.integer(sub_metro$metro_only_transfers)) + 1L) * (metro_wait_max/2)
      metro_spd_agg <- calc_agg_metro_speed(metro_l2_ride)
    }
    if (N_multi > 0) {
      multi_stop_pen <- stop_penalty_min * sub_multi$multi_dwell
      multi_legs <- split_legs(sub_multi$chain_multi_km, sub_multi$pct_multi_l1, sub_multi$pct_multi_l2)
      pt <- sub_multi$path_type
      n_mtr <- coalesce(as_num(sub_multi$mm_metro_tr),0); n_btr <- coalesce(as_num(sub_multi$mm_bus_tr),0); n_msw <- coalesce(as_num(sub_multi$mm_mode_sw),0)
      n_mb <- ifelse(pt=="Metro-Metro", 1+n_mtr, ifelse(pt=="Metro-Bus", 1+n_mtr, ifelse(pt=="Bus-Metro", n_msw+n_mtr, 0)))
      n_bb <- ifelse(pt=="Bus-Bus", 1+n_btr, ifelse(pt=="Metro-Bus", n_msw+n_btr, ifelse(pt=="Bus-Metro", 1+n_btr, 0)))
      n_mb[is.na(pt)] <- 0; n_bb[is.na(pt)] <- 0
      multi_wait_avg <- n_mb*(metro_wait_max/2) + n_bb*(bus_wait_max/2)
    }
    rows <- list()
    add <- function(mf, ini, tv) if (length(tv) > 0) rows[[length(rows)+1]] <<- data.frame(Type=type, Target=tgt, Speed=NA_real_, Mode_family=mf, Initiation=ini, Time_mean=mean(tv, na.rm=TRUE), stringsAsFactors=FALSE)
    for (s in speeds) {
      if (N_car > 0)   { car_t <- tm(sub_car$road_dist_km, s); rows[[length(rows)+1]] <- data.frame(Type=type, Target=tgt, Speed=s, Mode_family="Car-only (direct to clinic)", Initiation="Car-initiated", Time_mean=mean(car_t, na.rm=TRUE), stringsAsFactors=FALSE) }
      if (N_metro > 0) {
        base_m <- tm(metro_l2_ride, metro_spd_agg) + tm(metro_l2_walk, speed_walk) + tm(metro_legs$l3, speed_walk) + metro_stop_pen + metro_wait_avg
        rows[[length(rows)+1]] <- data.frame(Type=type, Target=tgt, Speed=s, Mode_family="Metro-only", Initiation="Car-initiated",  Time_mean=mean(tm(metro_legs$l1, s) + base_m, na.rm=TRUE), stringsAsFactors=FALSE)
        rows[[length(rows)+1]] <- data.frame(Type=type, Target=tgt, Speed=s, Mode_family="Metro-only", Initiation="Walk-initiated", Time_mean=mean(tm(metro_legs$l1, speed_walk) + base_m, na.rm=TRUE), stringsAsFactors=FALSE)
      }
      if (N_multi > 0) {
        l2_time <- (sub_multi$dist_brt_km/calc_brt_speed(s))*60 + (sub_multi$dist_std_km/s)*60 + sub_multi$metro_time_precise_min + (sub_multi$multi_transfer_walk_km/speed_walk)*60
        base_mm <- l2_time + tm(multi_legs$l3, speed_walk) + multi_stop_pen + multi_wait_avg
        rows[[length(rows)+1]] <- data.frame(Type=type, Target=tgt, Speed=s, Mode_family="Multimodal", Initiation="Car-initiated",  Time_mean=mean(tm(multi_legs$l1, s) + base_mm, na.rm=TRUE), stringsAsFactors=FALSE)
        rows[[length(rows)+1]] <- data.frame(Type=type, Target=tgt, Speed=s, Mode_family="Multimodal", Initiation="Walk-initiated", Time_mean=mean(tm(multi_legs$l1, speed_walk) + base_mm, na.rm=TRUE), stringsAsFactors=FALSE)
      }
    }
    bind_rows(rows)
  }

  .scen_new <- list(farthest_priv=c("Private","Farthest"), farthest_pub=c("Public","Farthest"),
                    random_priv=c("Private","Random"),     random_pub=c("Public","Random"))
  df_sum_new <- bind_rows(lapply(names(.scen_new), function(dt) {
    .tt_anchor_summaries(df_analysis %>% filter(dest_type == dt), .scen_new[[dt]][1], .scen_new[[dt]][2])
  }))
  df_sum_all <- bind_rows(df_sum, df_sum_new)

  # --- VALIDATION: replay physics on an EXISTING anchor; diff vs original df_sum
  #     should be tiny (only closed-form vs 1000-iter-sim wait differ). ---
  if ("nearest_priv" %in% df_analysis$dest_type) {
    .ref_np <- df_sum %>% filter(Type=="Private", Target=="Nearest") %>%
      transmute(Speed, Mode_family=as.character(Mode_family), Initiation=as.character(Initiation), tt_ref=Time_mean)
    .chk_np <- .tt_anchor_summaries(df_analysis %>% filter(dest_type=="nearest_priv"), "Private","Nearest") %>%
      transmute(Speed, Mode_family=as.character(Mode_family), Initiation=as.character(Initiation), tt_new=Time_mean) %>%
      inner_join(.ref_np, by=c("Speed","Mode_family","Initiation"))
    message(sprintf("[S18 R1-5] [validate] nearest_priv travel-time max|delta| vs original df_sum = %.3f min (expect small; closed-form vs sim wait)",
                    max(abs(.chk_np$tt_new - .chk_np$tt_ref), na.rm=TRUE)))
  }

  # --- Figures (new files; certified Fig_mean_travel_time.tiff untouched) ---
  .mk_mean_fig <- function(dsum, targ_levels, fname, ttl) {
    d <- dsum %>% mutate(Mode_family = factor(Mode_family, levels = mode_order),
                         Initiation  = factor(Initiation, levels = c("Car-initiated","Walk-initiated")),
                         Target      = factor(Target, levels = targ_levels))
    p <- ggplot(d, aes(x = Speed, y = Time_mean/60, color = Mode_family, linetype = Initiation)) +
      geom_line(linewidth = 1.1) +
      facet_grid(Type ~ Target, labeller = labeller(Target = c("Nearest"="Nearest","Specific"="Median-distance","Farthest"="Farthest","Random"="Random"))) +
      scale_color_manual(values = mode_colors) + scale_linetype_manual(values = c("solid","longdash")) +
      scale_x_continuous(limits = c(5,80), breaks = seq(10,80,10)) +
      labs(title = ttl, x = "Average speed for car/standard bus (km/h)", y = "Mean travel time (hours)", color = "Travel mode", linetype = "Initiation") +
      base_theme + theme(panel.spacing.y = unit(1.5,"lines"), panel.spacing.x = unit(1,"lines"), panel.border = element_blank(), axis.line = element_blank(), axis.ticks = element_blank())
    ggsave(fname, p, width = 16, height = 9, dpi = 300)
    message("[S18 R1-5] wrote ", fname)
  }
  .mk_mean_fig(df_sum_all, c("Nearest","Specific","Farthest","Random"), "Fig_mean_travel_time_ALL8.tiff",
               "Mean Travel Time by Anchor (Nearest / Median / Farthest / Random), Unweighted")
  .mk_mean_fig(df_sum_new, c("Farthest","Random"), "Fig_mean_travel_time_NEW4.tiff",
               "Mean Travel Time to Farthest / Random Dental Facility, Unweighted")

  # --- Break-even traffic speed (mean car time crosses mean transit time) + ref-speed means ---
  S_REF <- 40
  .breakeven <- function(dsum, type, tgt, mf, ini) {
    car <- dsum %>% filter(Type==type, Target==tgt, Mode_family=="Car-only (direct to clinic)", Initiation=="Car-initiated") %>% arrange(Speed)
    tr  <- dsum %>% filter(Type==type, Target==tgt, Mode_family==mf, Initiation==ini) %>% arrange(Speed)
    if (nrow(car) < 2 || nrow(tr) < 2) return(NA_real_)
    d <- car$Time_mean - tr$Time_mean   # >0 => car slower (transit faster)
    if (all(d > 0, na.rm=TRUE) || all(d < 0, na.rm=TRUE)) return(NA_real_)
    tryCatch(approx(x = d, y = car$Speed, xout = 0, ties = mean)$y, error = function(e) NA_real_)
  }
  .ref <- function(dsum, type, tgt, mf, ini) {
    v <- dsum %>% filter(Type==type, Target==tgt, Mode_family==mf, Initiation==ini, Speed==S_REF) %>% pull(Time_mean)
    if (length(v)==0) NA_real_ else v[1]
  }
  anchors_tab <- tibble::tribble(
    ~dest_type, ~Type, ~Target,
    "nearest_priv","Private","Nearest", "median_priv","Private","Specific", "farthest_priv","Private","Farthest", "random_priv","Private","Random",
    "nearest_pub","Public","Nearest",   "median_pub","Public","Specific",   "farthest_pub","Public","Farthest",   "random_pub","Public","Random"
  )
  tt_summary <- anchors_tab %>% rowwise() %>% mutate(
    car_min_ref      = round(.ref(df_sum_all, Type, Target, "Car-only (direct to clinic)","Car-initiated"), 1),
    metro_walk_min   = round(.ref(df_sum_all, Type, Target, "Metro-only","Walk-initiated"), 1),
    multi_walk_ref   = round(.ref(df_sum_all, Type, Target, "Multimodal","Walk-initiated"), 1),
    breakeven_metro  = round(.breakeven(df_sum_all, Type, Target, "Metro-only","Car-initiated"), 1),
    breakeven_multi  = round(.breakeven(df_sum_all, Type, Target, "Multimodal","Car-initiated"), 1)
  ) %>% ungroup() %>%
    transmute(Anchor = paste(Type, Target), `Car (min, 40 km/h)`=car_min_ref, `Metro-only (min, walk)`=metro_walk_min,
              `Multimodal (min, walk, 40 km/h)`=multi_walk_ref, `Break-even speed: metro (km/h)`=breakeven_metro, `Break-even speed: multimodal (km/h)`=breakeven_multi)

  # Random travel-time Monte-Carlo SE (per-point mean over draws -> grand SE) at S_REF, walk-init best transit
  .rng_mcse <- bind_rows(lapply(c("random_priv","random_pub"), function(dt) {
    d <- df_analysis %>% filter(dest_type == dt)
    if (!("rp_id" %in% names(d))) return(NULL)
    ms <- d %>% filter(!is.na(chain_metro_km)); mm <- d %>% filter(!is.na(chain_multi_km))
    # walk-init metro + multimodal at S_REF per row, then best per row, grouped by point
    per <- d %>% mutate(
      t_metro = ifelse(!is.na(chain_metro_km),
        tm(pmax(split_legs(chain_metro_km, pct_metro_l1, pct_metro_l2)$l1,0), speed_walk) +
        tm(pmax(split_legs(chain_metro_km, pct_metro_l1, pct_metro_l2)$l2,0), 40) +
        tm(pmax(split_legs(chain_metro_km, pct_metro_l1, pct_metro_l2)$l3,0), speed_walk) +
        (pmax(0L,as.integer(metro_only_transfers))+1L)*(metro_wait_max/2), NA_real_),
      t_multi = ifelse(!is.na(chain_multi_km),
        tm(pmax(split_legs(chain_multi_km, pct_multi_l1, pct_multi_l2)$l1,0), speed_walk) +
        ((dist_brt_km/calc_brt_speed(40))*60 + (dist_std_km/40)*60 + metro_time_precise_min + (multi_transfer_walk_km/speed_walk)*60) +
        tm(pmax(split_legs(chain_multi_km, pct_multi_l1, pct_multi_l2)$l3,0), speed_walk) +
        stop_penalty_min*multi_dwell, NA_real_),
      t_best = pmin(t_metro, t_multi, na.rm = TRUE)
    ) %>% group_by(rp_id) %>% summarise(m = mean(t_best, na.rm=TRUE), s = stats::sd(t_best, na.rm=TRUE), nd = sum(!is.na(t_best)), .groups="drop") %>%
      mutate(se = s/sqrt(pmax(nd,1)))
    tibble(anchor = dt, mean_best_min = mean(per$m, na.rm=TRUE), grand_se_min = sqrt(sum(per$se^2, na.rm=TRUE))/nrow(per))
  }))

  library(flextable); library(officer)
  save_as_docx(flextable(as.data.frame(tt_summary)) %>% theme_booktabs() %>% autofit(),
               path = "TravelTime_summary_new_anchors_unweighted.docx")
  message("[S18 R1-5] wrote TravelTime_summary_new_anchors_unweighted.docx")
  message("[S18 R1-5] Travel-time summary (min) by anchor:"); print(as.data.frame(tt_summary), row.names = FALSE)
  message("[S18 R1-5] Random travel-time Monte-Carlo SE (walk-init best transit @", S_REF, " km/h):"); print(as.data.frame(.rng_mcse), row.names = FALSE)
  saveRDS(df_sum_all, "travel_time_summary_ALL8_unweighted.rds")
} else {
  message("[S18 R1-5] New anchors not present in df_analysis; skipping travel-time minutes block.")
}

# ------------------------------------------------------------------------------
# R1-4 / R2-3f — imputation sensitivity (UNWEIGHTED). Gated, additive, default OFF.
# Sources _r14_imputation.R + _r14_orchestrate.R and runs the 3-method
# (complete-case / ratio / PMM-MI) network-distance comparison, break-even
# sensitivity, imputation accounting, and reachable-vs-all-trips denominator
# bounds. Writes R14_*_unweighted.{rds,docx} into Data/; certified outputs
# untouched. Needs pkg 'mice'. Env: RUN_R14=1 to enable, N_MICE (default 20).
# ------------------------------------------------------------------------------
if (Sys.getenv("RUN_R14") == "1") {
  .r14_code <- if (exists("base_dir")) file.path(dirname(base_dir), "Code") else file.path(getwd(), "Code")
  .r14_orch <- file.path(.r14_code, "_r14_orchestrate.R")
  if (file.exists(.r14_orch)) {
    source(.r14_orch); r14_orchestrate("unweighted")
    .pm <- file.path(.r14_code, "_r13_permode.R"); if (file.exists(.pm)) source(.pm)         # per-mode engine
    .r14_cb <- file.path(.r14_code, "_r14_chain_be.R"); if (file.exists(.r14_cb)) { source(.r14_cb); r14_chain_be("unweighted") }  # L1-chain + 50%-crossing
  } else message("[R1-4] _r14_orchestrate.R not found at ", .r14_code, "; skipping unweighted.")
}
# ------------------------------------------------------------------------------
# R1-3 — modelling-assumption sensitivity (UNWEIGHTED). Gated, additive, OFF by
# default. Cheap post-processing sweep on df_analysis; writes R13_*_unweighted.
# ------------------------------------------------------------------------------
if (Sys.getenv("RUN_R13") == "1") {
  .r13_code <- if (exists("base_dir")) file.path(dirname(base_dir), "Code") else file.path(getwd(), "Code")
  .r13_orch <- file.path(.r13_code, "_r13_orchestrate.R")
  if (file.exists(.r13_orch)) {
    source(.r13_orch); r13_orchestrate("unweighted")
  } else message("[R1-3] _r13_orchestrate.R not found at ", .r13_code, "; skipping unweighted.")
}

# R1-3 ENHANCED routing capture (UNWEIGHTED). Gated, additive, OFF by default.
# Needs the warm .PCACHE => run with the S18 loop ENABLED (no SKIP_S18_LOOP).
if (Sys.getenv("RUN_R13_ENH") == "1") {
  .r13_code <- if (exists("base_dir")) file.path(dirname(base_dir), "Code") else file.path(getwd(), "Code")
  .r13_enh  <- file.path(.r13_code, "_r13_enhanced.R")
  if (file.exists(.r13_enh)) {
    .r13_orch <- file.path(.r13_code, "_r13_orchestrate.R")
    if (file.exists(.r13_orch)) source(.r13_orch)   # r13_breakeven + .fin_* helpers
    .r13_se <- file.path(.r13_code, "_r13_enh_se.R"); if (file.exists(.r13_se)) source(.r13_se)  # SE-over-draws
    source(.r13_enh)
    r13_enh_capture("unweighted", asg_key = "Uniform in populated districts only", t1_cache = .t1_cache)
    r13_enh_orchestrate("unweighted", asg_key = "Uniform in populated districts only", t1_cache = .t1_cache)
  } else message("[R1-3-ENH] _r13_enhanced.R not found at ", .r13_code, "; skipping unweighted.")
}

# R1-3 BOUNDS — combined-assumption Fig 2/3/4 bands (UNWEIGHTED). Gated, additive, OFF
# by default. Works off R13_combos_unweighted.rds; no routing.
if (Sys.getenv("RUN_R13_BOUNDS") == "1") {
  .r13_code <- if (exists("base_dir")) file.path(dirname(base_dir), "Code") else file.path(getwd(), "Code")
  for (.f in c("_r13_orchestrate.R","_r13_enhanced.R","_r13_bounds.R")) { .p <- file.path(.r13_code, .f); if (file.exists(.p)) source(.p) }
  if (exists("r13_bounds")) r13_bounds("unweighted") else message("[R1-3-BOUNDS] _r13_bounds.R not found; skipping unweighted.")
}

# === Validation gate: when VALIDATE_S18=1, stop after Section 18 so its outputs can be
#     diffed against the pre-refactor baseline. Unset (default) runs the full pipeline. ===
if (Sys.getenv("VALIDATE_S18") == "1") { message("VALIDATION_STOP: Section 18 complete."); quit(save = "no", status = 0) }

##################################################################################
##################################################################################
# Section 19: MULTIMODAL TRANSIT ACCESSIBILITY (POPULATION-WEIGHTED)
# Changes from original:
# 1. Input: Uses 'random_points_geo_vs_network_EDGE_METHOD_pop_weighted_NO_LCC.rds'
# 2. Outputs: All files (RDS, DOCX, TIFF) renamed with "_weighted" suffix
# 3. Figures: Titles updated to reflect "Weighted" analysis
##################################################################################
##################################################################################

# ==============================================================================
# 0) LIBRARIES + GLOBAL SETTINGS
# ==============================================================================

library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(igraph)
library(tibble)
library(dodgr)
library(purrr)
library(flextable)
library(officer)
library(ggplot2)
library(viridisLite)

sf::sf_use_s2(FALSE)
set.seed(1234)

target_crs <- 32638

# Transfer parameters
walk_transfer_threshold_m <- 100 # For bus-bus non-exact and metro-bus

message(paste(rep("=", 60), collapse = ""))
message("FULL RUN: All Points - Weighted Multimodal Accessibility")
message(paste(rep("=", 60), collapse = ""))

# ==============================================================================
# 1) VERIFY REQUIRED OBJECTS IN ENVIRONMENT
# ==============================================================================

message("\n[Checking] Checking required objects in environment...")

req_objects <- c(
  "bus_route_segments_sf",   # From first script (fixed segments)
  "bus_stops_network_sf",    # From first script (snapped stops)
  "riyadh_merged_2",         # Region polygons
  "gisdata",                 # Dental clinics
  "stations",                # Metro stations
  "metro_lines",             # Metro lines
  "bus",                     # Original bus data (for reference)
  "all_points"               # Raw points for direction recovery
)

missing <- req_objects[!sapply(req_objects, exists)]
if(length(missing) > 0) {
  stop("Missing required objects: ", paste(missing, collapse = ", "))
}

message("All required objects found.")

# ==============================================================================
# 2) LOAD DATA FILES
# ==============================================================================

message("\n[Loading] Loading data files...")

# --- UPDATED PATH FOR WEIGHTED ANALYSIS ---
rp_rds <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data/random_points_geo_vs_network_EDGE_METHOD_pop_weighted_NO_LCC.rds"
fac_rds <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data/clinics_geo_vs_network_ROBUST_DIRECTED.rds"
roads_fp <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data/roads_sf_riyadh_clipped_32638.rds"

stopifnot(file.exists(rp_rds), file.exists(fac_rds), file.exists(roads_fp))

res_rp <- readRDS(rp_rds)
res_fac <- readRDS(fac_rds)
roads_sf <- readRDS(roads_fp)

message("Data files loaded.")

# ==============================================================================
# 2B) EXTRACT PRE-CALCULATED L1 AND L3 DISTANCES
# ==============================================================================

message("\n[Extracting] Extracting pre-calculated L1 and L3 distances...")

# L1 distances: RP to nearest transit (from res_rp$comparison)
rp_comparison <- res_rp$comparison %>%
  mutate(id = as.character(id))

# Extract L1 distances by destination type
L1_metro_lookup <- rp_comparison %>%
  filter(dest_type == "nearest_metro") %>%
  select(id, geo_km, net_km_p2t) %>%
  rename(L1_metro_geo_km = geo_km, L1_metro_net_km = net_km_p2t)

L1_bus_lookup <- rp_comparison %>%
  filter(dest_type == "nearest_bus") %>%
  select(id, geo_km, net_km_p2t) %>%
  rename(L1_bus_geo_km = geo_km, L1_bus_net_km = net_km_p2t)

# Extract clinic targets for each RP (which clinic is nearest/median)
clinic_targets <- rp_comparison %>%
  filter(dest_type %in% c("nearest_priv", "median_priv", "nearest_pub", "median_pub")) %>%
  select(id, dest_type, dest_id_geo) %>%
  mutate(
    clinic_id = gsub("^(priv_|pub_)", "", dest_id_geo)
  )

message(paste("   - Clinic target records:", nrow(clinic_targets)))

# L3 distances: Transit to clinic (from res_fac$results$comparison_all)
fac_comparison <- res_fac$results$comparison_all %>%
  mutate(id = as.character(id))

# Extract L3 by transit type - deduplicate to one record per clinic
L3_metro_lookup <- fac_comparison %>%
  filter(type == "metro station") %>%
  distinct(id, .keep_all = TRUE) %>%  
  select(id, geo_km, net_km_s2c) %>%
  rename(clinic_id = id, L3_metro_geo_km = geo_km, L3_metro_net_km = net_km_s2c)

L3_bus_lookup <- fac_comparison %>%
  filter(type == "bus stop") %>%
  distinct(id, .keep_all = TRUE) %>%  
  select(id, geo_km, net_km_s2c) %>%
  rename(clinic_id = id, L3_bus_geo_km = geo_km, L3_bus_net_km = net_km_s2c)

message(paste("   - L1 metro records:", nrow(L1_metro_lookup)))
message(paste("   - L1 bus records:", nrow(L1_bus_lookup)))
message(paste("   - L3 metro records (unique clinics):", nrow(L3_metro_lookup)))
message(paste("   - L3 bus records (unique clinics):", nrow(L3_bus_lookup)))

# ==============================================================================
# 2C) CHECK MISSING NETWORK DISTANCES & CALCULATE IMPUTATION RATIOS
# ==============================================================================

message("\n[Checking] Checking missing network distances by destination type...")

# Calculate imputation ratios for each destination type
imputation_info <- rp_comparison %>%
  group_by(dest_type) %>%
  summarise(
    n_total = n(),
    n_valid_p2t = sum(!is.na(net_km_p2t) & !is.infinite(net_km_p2t)),
    n_missing_p2t = n_total - n_valid_p2t,
    mean_geo_km = mean(geo_km, na.rm = TRUE),
    mean_net_p2t_km = mean(net_km_p2t, na.rm = TRUE),
    ratio_p2t = mean_net_p2t_km / mean_geo_km,
    .groups = "drop"
  )

message("\n   Missingness and Imputation Ratios (RP -> Destination):")
print(imputation_info)

# Create lookup for imputation ratios
ratio_lookup <- imputation_info %>%
  select(dest_type, ratio_p2t) %>%
  tibble::deframe()

# L3 distances: Check by transit type AND clinic ownership
L3_with_ownership <- fac_comparison %>%
  distinct(id, type, .keep_all = TRUE) %>%
  mutate(
    ownership = case_when(
      label == "private" ~ "private",
      label == "public" ~ "public",
      TRUE ~ stratum 
    )
  )

L3_missing_info <- L3_with_ownership %>%
  group_by(type, ownership) %>%
  summarise(
    n_total = n(),
    n_valid_s2c = sum(!is.na(net_km_s2c) & !is.infinite(net_km_s2c)),
    n_missing_s2c = n_total - n_valid_s2c,
    mean_geo_km = mean(geo_km, na.rm = TRUE),
    mean_net_s2c_km = mean(net_km_s2c, na.rm = TRUE),
    ratio_s2c = mean_net_s2c_km / mean_geo_km,
    .groups = "drop"
  )

message("\n   Missingness and Imputation Ratios (Transit -> Clinic by ownership):")
print(L3_missing_info)

# Extract L3 ratios
ratio_L3_metro_priv <- L3_missing_info %>% filter(type == "metro station", ownership == "private") %>% pull(ratio_s2c)
ratio_L3_metro_pub <- L3_missing_info %>% filter(type == "metro station", ownership == "public") %>% pull(ratio_s2c)
ratio_L3_bus_priv <- L3_missing_info %>% filter(type == "bus stop", ownership == "private") %>% pull(ratio_s2c)
ratio_L3_bus_pub <- L3_missing_info %>% filter(type == "bus stop", ownership == "public") %>% pull(ratio_s2c)

# Fallbacks
if(length(ratio_L3_metro_priv) == 0 || is.na(ratio_L3_metro_priv)) ratio_L3_metro_priv <- 1.3
if(length(ratio_L3_metro_pub) == 0 || is.na(ratio_L3_metro_pub)) ratio_L3_metro_pub <- 1.3
if(length(ratio_L3_bus_priv) == 0 || is.na(ratio_L3_bus_priv)) ratio_L3_bus_priv <- 1.3
if(length(ratio_L3_bus_pub) == 0 || is.na(ratio_L3_bus_pub)) ratio_L3_bus_pub <- 1.3

# --- rp_sample (weighted points): the one point-dependent object here; set it
#     BEFORE the reuse gate so it exists on both branches. ---
rp_sample <- res_rp$pts %>% st_transform(target_crs) %>% dplyr::mutate(id = as.character(id))
message(paste("    - Total weighted points to process:", nrow(rp_sample)))

# === REUSE GATE: REUSE_S18_TRANSIT=1 reuses S18's transit network/graph/helpers
#     (identical + point-independent) instead of the ~35-min redundant rebuild below.
#     Default (unset) runs the full rebuild exactly as before. ===
if (Sys.getenv("REUSE_S18_TRANSIT") == "1") {
  .reuse_need <- c("stations_proj","bus_stops_proj","clinics_private","clinics_public",
                   "g_multimodal","g_metro_only","get_L1_from_lookup","get_L3_from_lookup",
                   "get_L2_distance_simple","get_L2_distance_real","get_path_details",
                   "get_combo_priority","check_on_route_via_distances",
                   "select_best_by_pct_tolerance","TOL_PCT")
  .reuse_miss <- .reuse_need[!vapply(.reuse_need, exists, logical(1))]
  if (length(.reuse_miss) > 0) stop("REUSE_S18_TRANSIT=1 but missing S18 objects (S18 must run first in same process): ", paste(.reuse_miss, collapse = ", "))
  message("[REUSE_S18_TRANSIT] Reusing S18 transit network + graph + helpers; skipping the redundant rebuild.")
} else {

# ==============================================================================
# 3) PREPARE SPATIAL LAYERS & EMERGENCY PATCH
# ==============================================================================

message("\n[Preparing] Preparing spatial layers...")

riyadh_proj <- st_transform(riyadh_merged_2, target_crs)
clinics_proj <- st_transform(gisdata, target_crs)
stations_proj <- st_transform(stations, target_crs)
roads_proj <- st_transform(roads_sf, target_crs)

# EMERGENCY PATCH: RESTORE DIRECTIONAL HUBS
message("\n🚑 Patching bus data to restore DIRECTIONAL stops...")
if(exists("all_points")) {
  bus_stops_network_sf <- bind_rows(all_points) %>%
    st_as_sf(crs = 32638) %>%
    distinct(stop_code, route_id, .keep_all = TRUE) 
  
  bus_stops_proj <- st_transform(bus_stops_network_sf, target_crs) %>%
    mutate(bus_stop_id = row_number()) 
  
  message(paste("✅ Bus data patched. Total stop instances:", nrow(bus_stops_proj)))
} else {
  stop("❌ 'all_points' is missing. You must re-run Part 1 extraction.")
}

bus_segments_proj <- st_transform(bus_route_segments_sf, target_crs)

# Filter clinics
clinics_dental <- clinics_proj %>% filter(with_dental_services == "Yes")
clinics_private <- clinics_dental %>% filter(private_or_public == "Private")
clinics_public <- clinics_dental %>% filter(private_or_public == "Public")

message(paste("   - Metro stations:", nrow(stations_proj)))
message(paste("   - Bus stops (network):", nrow(bus_stops_proj)))
message(paste("   - Bus segments:", nrow(bus_segments_proj)))
message(paste("   - Private dental clinics:", nrow(clinics_private)))
message(paste("   - Public dental clinics:", nrow(clinics_public)))

# ==============================================================================
# 4) PREPARE ALL POINTS
# ==============================================================================

message("\n[Processing] Preparing ALL weighted points for full computation...")

rp_all <- res_rp$pts %>%
  st_transform(target_crs) %>%
  mutate(id = as.character(id))

rp_sample <- rp_all

message(paste("    - Total points to process:", nrow(rp_sample)))

# ==============================================================================
# 5) FIX SEGMENT GAPS & CALCULATE DISTANCES
# ==============================================================================

message("\n[Fixing] Fixing gaps and calculating corrected distances...")

fix_route_geometry <- function(all_segments) {
  unique_routes <- unique(all_segments$route_id)
  fixed_list <- list()
  
  for(rid in unique_routes) {
    route_segs <- all_segments %>% 
      filter(route_id == rid) %>% 
      arrange(segment_seq)
    
    if(nrow(route_segs) <= 1) {
      fixed_list[[rid]] <- route_segs
      next
    }
    
    geoms <- st_geometry(route_segs)
    for(i in 2:length(geoms)) {
      prev_coords <- st_coordinates(geoms[[i-1]])
      prev_end <- prev_coords[nrow(prev_coords), 1:2]
      curr_coords <- st_coordinates(geoms[[i]])
      curr_coords[1, 1:2] <- prev_end
      geoms[[i]] <- st_linestring(curr_coords[, 1:2])
    }
    
    route_segs$geometry <- st_sfc(geoms, crs = st_crs(all_segments))
    fixed_list[[rid]] <- route_segs
  }
  return(bind_rows(fixed_list))
}

bus_segments_fixed <- fix_route_geometry(bus_segments_proj)
bus_segments_with_dist <- bus_segments_fixed %>%
  mutate(corrected_dist_m = as.numeric(st_length(geometry)))

message(paste("   - Geometry snapping complete. Mean segment dist:", round(mean(bus_segments_with_dist$corrected_dist_m, na.rm=T),1), "m"))

# ==============================================================================
# 6) CLASSIFY BUS ROUTES
# ==============================================================================

message("\n[Classifying] Classifying bus routes...")

rapid_transit_routes <- c("11", "12", "13")

bus_segments_with_dist <- bus_segments_with_dist %>%
  mutate(route_type = ifelse(route_num %in% rapid_transit_routes, "Rapid Transit", "Standard"))

bus_stops_proj <- bus_stops_proj %>%
  mutate(route_type = ifelse(route_num %in% rapid_transit_routes, "Rapid Transit", "Standard"))

n_rapid <- sum(bus_segments_with_dist$route_type == "Rapid Transit")
n_standard <- sum(bus_segments_with_dist$route_type == "Standard")
message(paste("   - Rapid Transit segments:", n_rapid))
message(paste("   - Standard segments:", n_standard))

# ==============================================================================
# 7) BUILD WALK NETWORK
# ==============================================================================

message("\n[Building] Building walk network from roads...")

roads_for_walk <- roads_proj %>%
  st_cast("LINESTRING") %>%
  filter(!st_is_empty(geometry))

walk_graph <- weight_streetnet(roads_for_walk, wt_profile = "foot", id_col = "edge_id")

message(paste("   - Walk network edges:", nrow(walk_graph)))

calc_walk_distance <- function(from_coords, to_coords, graph = walk_graph) {
  if(is.null(from_coords) || is.null(to_coords)) return(NA_real_)
  if(nrow(from_coords) == 0 || nrow(to_coords) == 0) return(NA_real_)
  tryCatch({
    d <- dodgr_dists(graph, from = from_coords, to = to_coords)
    as.numeric(d[1, 1])
  }, error = function(e) { NA_real_ })
}

# ==============================================================================
# 8) BUILD TRANSFER NETWORK
# ==============================================================================
message("\n[8] Building transfer network (Dynamic BRT logic + Metro integration)...")

if(!"stn_id" %in% names(stations_proj)) {
  stations_proj <- stations_proj %>% mutate(stn_id = row_number())
}

# 8A. METRO-METRO TRANSFERS
message("   [8A] Metro-Metro interchanges...")
metro_metro_transfers <- stations_proj %>%
  st_drop_geometry() %>%
  group_by(metrostationname) %>%
  filter(n() > 1) %>%
  summarise(ids = list(stn_id), .groups = "drop") %>%
  mutate(transfers = purrr::map(ids, ~{
    pairs <- combn(sort(.x), 2)
    data.frame(from = pairs[1,], to = pairs[2,], transfer_type = "metro_metro")
  })) %>%
  tidyr::unnest(transfers) %>%
  select(from, to, transfer_type)

message(paste("       - Metro-Metro links:", nrow(metro_metro_transfers)))

# 8B. BUS-BUS EXACT TRANSFERS
message("   [8B] Bus-Bus physical intersections...")
bus_nodes <- bus_stops_proj %>% st_drop_geometry()
multi_route_hubs <- bus_nodes %>%
  group_by(stop_code) %>%
  filter(n_distinct(route_id) > 1) %>%
  summarise(all_node_ids = list(bus_stop_id), .groups = "drop")

if(nrow(multi_route_hubs) > 0) {
  bus_bus_exact_transfers <- do.call(rbind, lapply(multi_route_hubs$all_node_ids, function(x) {
    if(length(x) < 2) return(NULL)
    pairs <- combn(sort(x), 2)
    data.frame(from = pairs[1,], to = pairs[2,], transfer_type = "bus_bus_physical")
  }))
} else {
  bus_bus_exact_transfers <- data.frame(from=integer(), to=integer(), transfer_type=character())
}

message(paste("       - Bus-Bus physical links:", nrow(bus_bus_exact_transfers)))

# 8C. METRO-BUS TRANSFERS (200m)
message("   [8C] Metro-Bus transfers (200m threshold)...")
walk_threshold_m <- 200
nb_metro_bus <- st_is_within_distance(stations_proj, bus_stops_proj, dist = walk_threshold_m)

metro_bus_transfers <- data.frame()
for(i in seq_along(nb_metro_bus)) {
  j_vec <- nb_metro_bus[[i]]
  if(length(j_vec) > 0) {
    for(j in j_vec) {
      dist_euc <- as.numeric(st_distance(stations_proj[i,], bus_stops_proj[j,]))
      metro_bus_transfers <- rbind(metro_bus_transfers, data.frame(
        from = stations_proj$stn_id[i],
        to   = bus_stops_proj$bus_stop_id[j],
        dist_m = dist_euc,
        transfer_type = "metro_bus"
      ))
    }
  }
}
message(paste("       - Metro-Bus links (200m):", nrow(metro_bus_transfers)))

# 8D. MANUAL METRO-BUS CONNECTIONS
message("   [8D] Manual Metro-Bus connections...")
metro_al_iman <- stations_proj %>% filter(metrostationname == "Al Iman Hospital") %>% pull(stn_id)
metro_khurais <- stations_proj %>% filter(metrostationname == "Khurais Road") %>% pull(stn_id)

manual_metro_bus <- data.frame(
  from = c(ifelse(length(metro_al_iman) > 0, metro_al_iman[1], NA),
           ifelse(length(metro_khurais) > 0, metro_khurais[1], NA)),
  to   = c(1512, 1982),
  dist_m = c(170, 20),
  transfer_type = c("metro_bus_manual", "metro_bus_manual")
) %>% filter(!is.na(from) & !is.na(to))

# 8E. BUS-BUS PROXIMITY (BRT <-> STD)
message("   [8E] Bus-Bus proximity transfers...")
brt_routes <- c("11", "12", "13")
brt_stops <- bus_stops_proj %>% filter(route_num %in% brt_routes)
std_stops <- bus_stops_proj %>% filter(!route_num %in% brt_routes)
bus_bus_proximity_transfers <- data.frame()

if(nrow(brt_stops) > 0 && nrow(std_stops) > 0) {
  bus_prox_matrix <- st_is_within_distance(brt_stops, std_stops, dist = 100)
  for(i in seq_along(bus_prox_matrix)) {
    j_vec <- bus_prox_matrix[[i]]
    if(length(j_vec) > 0) {
      for(j in j_vec) {
        if(brt_stops$stop_code[i] == std_stops$stop_code[j]) next
        dist <- as.numeric(st_distance(brt_stops[i,], std_stops[j,]))
        bus_bus_proximity_transfers <- rbind(bus_bus_proximity_transfers, 
                                             data.frame(from=brt_stops$bus_stop_id[i], to=std_stops$bus_stop_id[j], dist_m=round(dist), transfer_type="bus_bus_proximity"),
                                             data.frame(from=std_stops$bus_stop_id[j], to=brt_stops$bus_stop_id[i], dist_m=round(dist), transfer_type="bus_bus_proximity"))
      }
    }
  }
}

# 8E.2. STANDARD BUS <-> STANDARD BUS PROXIMITY
message("   [8E.2] Standard Bus <-> Standard Bus proximity transfers...")
bus_bus_standard_transfers <- data.frame()
if(nrow(std_stops) > 0) {
  std_prox_matrix <- st_is_within_distance(std_stops, std_stops, dist = 100)
  transfer_list_std <- list()
  for(i in seq_along(std_prox_matrix)) {
    j_vec <- std_prox_matrix[[i]]
    j_vec <- j_vec[j_vec != i]
    if(length(j_vec) > 0) {
      curr_route <- std_stops$route_num[i]; curr_dir <- std_stops$direction[i]
      neigh_routes <- std_stops$route_num[j_vec]; neigh_dirs <- std_stops$direction[j_vec]
      valid_mask <- (neigh_routes != curr_route) | (neigh_dirs != curr_dir)
      j_vec_valid <- j_vec[valid_mask]
      if(length(j_vec_valid) > 0) {
        dists <- as.numeric(st_distance(std_stops[i,], std_stops[j_vec_valid,]))
        transfer_list_std[[i]] <- data.frame(from=std_stops$bus_stop_id[i], to=std_stops$bus_stop_id[j_vec_valid], dist_m=round(dists), transfer_type="bus_bus_standard", stringsAsFactors=FALSE)
      }
    }
  }
  bus_bus_standard_transfers <- bind_rows(transfer_list_std)
}

# 8F. MASTER TRANSFER MERGE
message("   [8F] Finalizing Master Transfer Table...")
master_transfers <- bind_rows(
  if(nrow(bus_bus_exact_transfers) > 0) bus_bus_exact_transfers %>% mutate(dist_m = 0) else NULL,
  if(nrow(metro_metro_transfers) > 0)   metro_metro_transfers   %>% mutate(dist_m = 50) else NULL,
  if(nrow(metro_bus_transfers) > 0)     metro_bus_transfers else NULL,
  manual_metro_bus,
  bus_bus_proximity_transfers,
  bus_bus_standard_transfers
) %>% group_by(from, to) %>% arrange(dist_m) %>% slice(1) %>% ungroup()

message(paste("\n[OK] Transfer network complete! Total links:", nrow(master_transfers)))

# ==============================================================================
# 9) BUILD UNIFIED MULTIMODAL GRAPH
# ==============================================================================
message("\n[9] Building unified multimodal graph (Priority Weights ±30%)...")

BUS_MULT <- 1.10; BRT_MULT <- 1.00; METRO_MULT <- 0.90
TOL_PCT <- 0.10

# 9A. METRO LINE EDGES
stn_xy <- st_coordinates(stations_proj)
stations_proj$X <- stn_xy[, 1]; stations_proj$Y <- stn_xy[, 2]
edges_metro_line <- stations_proj %>% st_drop_geometry() %>%
  arrange(metroline, stationseq) %>% group_by(metroline) %>%
  mutate(to_id = lead(stn_id), X2 = lead(X), Y2 = lead(Y)) %>% ungroup() %>%
  filter(!is.na(to_id)) %>%
  transmute(from = paste0("M_", stn_id), to = paste0("M_", to_id), weight = sqrt((X-X2)^2 + (Y-Y2)^2), edge_type = "metro_line", route_type = "Metro", route_code = as.character(metroline), unique_route_key = as.character(metroline))

# 9B. BUS ROUTE EDGES
raw_sequence <- bind_rows(all_points) %>% st_drop_geometry() %>% select(route_id, segment_seq, point_type, stop_code)
node_dictionary <- bus_stops_proj %>% st_drop_geometry() %>% select(route_id, stop_code, bus_stop_id) %>% distinct(route_id, stop_code, .keep_all = TRUE)
bus_lookup_final <- raw_sequence %>% left_join(node_dictionary, by = c("route_id", "stop_code")) %>% select(route_id, segment_seq, point_type, bus_stop_id)

edges_bus_route <- bus_segments_with_dist %>% st_drop_geometry() %>%
  select(route_id, segment_seq, corrected_dist_m, route_type, route_num) %>%
  left_join(bus_lookup_final %>% filter(point_type == "from"), by = c("route_id", "segment_seq")) %>% rename(from_bus_id = bus_stop_id) %>%
  left_join(bus_lookup_final %>% filter(point_type == "to"), by = c("route_id", "segment_seq")) %>% rename(to_bus_id = bus_stop_id) %>%
  filter(!is.na(from_bus_id) & !is.na(to_bus_id)) %>%
  left_join(bus_stops_proj %>% st_drop_geometry() %>% select(bus_stop_id, direction), by = c("from_bus_id" = "bus_stop_id")) %>%
  mutate(unique_route_key = paste(route_num, direction, sep = " - ")) %>%
  transmute(from = paste0("B_", from_bus_id), to = paste0("B_", to_bus_id), weight = corrected_dist_m, edge_type = "bus_route", route_type = route_type, route_code = as.character(route_num), unique_route_key = unique_route_key)

# 9C. TRANSFER EDGES
add_na_attrs <- function(df) { df %>% mutate(route_code = NA_character_, unique_route_key = NA_character_) }

edges_metro_transfer <- if(nrow(metro_metro_transfers)>0) metro_metro_transfers %>% transmute(from=paste0("M_",from), to=paste0("M_",to), weight=50, edge_type="metro_metro_transfer", route_type="Transfer") %>% add_na_attrs() else data.frame()
edges_bus_exact <- if(nrow(bus_bus_exact_transfers)>0) bus_bus_exact_transfers %>% transmute(from=paste0("B_",from), to=paste0("B_",to), weight=0, edge_type="bus_bus_exact", route_type="Transfer") %>% add_na_attrs() else data.frame()
edges_metro_bus <- if(nrow(metro_bus_transfers)>0) metro_bus_transfers %>% transmute(from=paste0("M_",from), to=paste0("B_",to), weight=dist_m, edge_type="metro_bus", route_type="Transfer") %>% add_na_attrs() else data.frame()
edges_metro_bus_manual <- if(nrow(manual_metro_bus)>0) manual_metro_bus %>% transmute(from=paste0("M_",from), to=paste0("B_",to), weight=dist_m, edge_type="metro_bus_manual", route_type="Transfer") %>% add_na_attrs() else data.frame()
edges_bus_proximity <- if(nrow(bus_bus_proximity_transfers)>0) bus_bus_proximity_transfers %>% transmute(from=paste0("B_",from), to=paste0("B_",to), weight=dist_m, edge_type="bus_bus_proximity", route_type="Transfer") %>% add_na_attrs() else data.frame()
edges_bus_standard <- if(nrow(bus_bus_standard_transfers)>0) bus_bus_standard_transfers %>% transmute(from=paste0("B_",from), to=paste0("B_",to), weight=dist_m, edge_type="bus_bus_standard", route_type="Transfer") %>% add_na_attrs() else data.frame()

# 9D. ASSEMBLE GRAPH
edges_bidirectional_source <- bind_rows(edges_metro_line, edges_metro_transfer, edges_bus_exact, edges_metro_bus, edges_metro_bus_manual, edges_bus_proximity, edges_bus_standard)
edges_oneway_source <- edges_bus_route

edges_bidir_expanded <- bind_rows(edges_bidirectional_source, edges_bidirectional_source %>% rename(from=to, to=from)) %>% distinct(from, to, edge_type, route_type, .keep_all=TRUE)
all_edges_final <- bind_rows(edges_bidir_expanded, edges_oneway_source) %>%
  mutate(priority_weight = case_when(route_type == "Standard" ~ weight * BUS_MULT, route_type == "Rapid Transit" ~ weight * BRT_MULT, route_type == "Metro" ~ weight * METRO_MULT, TRUE ~ weight))

g_multimodal <- graph_from_data_frame(all_edges_final, directed = TRUE)
attr(g_multimodal, "ck") <- "mm"   # cache tag for memoized path lookups
E(g_multimodal)$weight <- all_edges_final$priority_weight
E(g_multimodal)$real_distance <- all_edges_final$weight
E(g_multimodal)$edge_type <- all_edges_final$edge_type
E(g_multimodal)$route_type <- all_edges_final$route_type
E(g_multimodal)$unique_route_key <- all_edges_final$unique_route_key

# Metro-only graph
metro_subset <- bind_rows(edges_metro_line, edges_metro_transfer)
g_metro_only <- NULL
if(nrow(metro_subset) > 0) {
  metro_edges_bidir <- bind_rows(metro_subset, metro_subset %>% rename(from=to, to=from)) %>% distinct(from, to, edge_type, route_type, .keep_all=TRUE)
  g_metro_only <- graph_from_data_frame(metro_edges_bidir, directed = TRUE)
  attr(g_metro_only, "ck") <- "mo"   # cache tag for memoized path lookups
  E(g_metro_only)$weight <- metro_edges_bidir$weight
  E(g_metro_only)$edge_type <- metro_edges_bidir$edge_type
  E(g_metro_only)$unique_route_key <- metro_edges_bidir$unique_route_key
}

# ==============================================================================
# 11) HELPER FUNCTIONS (FINAL: MASKED METRO START/END + LINE ADJUSTMENT)
# ==============================================================================
message("\n[11] Defining helper functions...")

get_L1_from_lookup <- function(rp_id, transit_type, L1_metro_lookup, L1_bus_lookup, ratio_lookup) {
  # FAST: precomputed named vectors (.l1m_*/.l1b_*) replace per-call dplyr::filter; same semantics.
  key <- as.character(rp_id)
  if(transit_type == "metro") {
    if(!(key %in% names(.l1m_geo))) return(NA_real_)
    net <- .l1m_net[[key]]
    if(!is.na(net) && !is.infinite(net)) return(net * 1000)
    ratio <- ratio_lookup["nearest_metro"]; if(is.na(ratio)) ratio <- 1.3
    return(.l1m_geo[[key]] * ratio * 1000)
  } else {
    if(!(key %in% names(.l1b_geo))) return(NA_real_)
    net <- .l1b_net[[key]]
    if(!is.na(net) && !is.infinite(net)) return(net * 1000)
    ratio <- ratio_lookup["nearest_bus"]; if(is.na(ratio)) ratio <- 1.3
    return(.l1b_geo[[key]] * ratio * 1000)
  }
}

get_L3_from_lookup <- function(rp_id, dest_type, transit_type, clinic_targets, L3_metro_lookup, L3_bus_lookup, ratio_L3_metro_priv, ratio_L3_metro_pub, ratio_L3_bus_priv, ratio_L3_bus_pub) {
  # FAST: precomputed named vectors (.ct_clinic_id/.l3m_*/.l3b_*) replace per-call dplyr::filter; same semantics.
  tkey <- paste0(rp_id, "||", dest_type)
  if(!(tkey %in% names(.ct_clinic_id))) return(NA_real_)
  clinic_id <- .ct_clinic_id[[tkey]]
  is_private <- grepl("priv", dest_type)
  if(transit_type == "metro") {
    if(!(clinic_id %in% names(.l3m_geo))) return(NA_real_)
    net <- .l3m_net[[clinic_id]]
    if(!is.na(net) && !is.infinite(net)) return(net * 1000)
    ratio <- if(is_private) ratio_L3_metro_priv else ratio_L3_metro_pub
    return(.l3m_geo[[clinic_id]] * ratio * 1000)
  } else {
    if(!(clinic_id %in% names(.l3b_geo))) return(NA_real_)
    net <- .l3b_net[[clinic_id]]
    if(!is.na(net) && !is.infinite(net)) return(net * 1000)
    ratio <- if(is_private) ratio_L3_bus_priv else ratio_L3_bus_pub
    return(.l3b_geo[[clinic_id]] * ratio * 1000)
  }
}

find_nearest_transit <- function(point_sf, transit_sf, transit_type) {
  nearest_idx <- st_nearest_feature(point_sf, transit_sf)
  node_id <- if(transit_type == "metro") paste0("M_", transit_sf$stn_id[nearest_idx]) else paste0("B_", transit_sf$bus_stop_id[nearest_idx])
  list(idx = nearest_idx, node_id = node_id)
}

# ------------------------------------------------------------------------------
# 11B. DISTANCE & VALIDATION (Updated: Ignore 50m penalty at start/end)
# ------------------------------------------------------------------------------
.impl_l2r <- function(from_node, to_node, graph) {
  if(is.na(from_node) || is.na(to_node)) return(NA_real_)
  if(!from_node %in% V(graph)$name || !to_node %in% V(graph)$name) return(NA_real_)
  if(from_node == to_node) return(0)
  
  path_obj <- suppressWarnings(shortest_paths(graph, from = from_node, to = to_node, weights = E(graph)$weight, output = "epath"))
  edge_seq <- path_obj$epath[[1]]
  if(length(edge_seq) == 0) return(NA_real_)
  
  e_types <- E(graph)$edge_type[edge_seq]
  e_dists <- as.numeric(E(graph)$real_distance[edge_seq]) # Needed for masking sum
  
  # --- 1. Block Forbidden Transfers ---
  forbidden_transfers <- c("bus_bus_exact", "metro_bus", 
                           "metro_bus_manual", "bus_bus_proximity", "bus_bus_standard")
  
  if(e_types[1] %in% forbidden_transfers) return(NA_real_)
  if(e_types[length(e_types)] %in% forbidden_transfers) return(NA_real_)
  
  # --- 2. Block Consecutive Transfers ---
  all_transfer_types <- c("metro_metro_transfer", forbidden_transfers)
  is_transfer <- e_types %in% all_transfer_types
  
  if(length(is_transfer) > 1 && any(is_transfer & dplyr::lag(is_transfer, default=FALSE))) return(NA_real_)
  
  # --- 3. MASKING: Ignore cost for Metro-Metro transfer if it's First or Last edge ---
  cost_mask <- rep(TRUE, length(edge_seq))
  if(e_types[1] == "metro_metro_transfer") cost_mask[1] <- FALSE
  if(length(e_types) > 1 && e_types[length(e_types)] == "metro_metro_transfer") cost_mask[length(e_types)] <- FALSE
  
  sum(e_dists[cost_mask], na.rm = TRUE)
}

.impl_l2s <- function(from_node, to_node, graph) {
  if(is.na(from_node) || is.na(to_node) || is.null(graph)) return(NA_real_)
  if(!from_node %in% V(graph)$name || !to_node %in% V(graph)$name) return(NA_real_)
  if(from_node == to_node) return(0)
  d <- suppressWarnings(distances(graph, v = from_node, to = to_node, weights = E(graph)$weight))
  if(is.infinite(d[1, 1])) return(NA_real_)
  d[1, 1]
}

check_on_route_via_distances <- function(d_rp, d_cl, d_total, tol = 1) {
  if(is.na(d_rp) || is.na(d_cl) || is.na(d_total)) return(NA)
  abs((d_rp + d_cl) - d_total) <= tol
}

get_combo_priority <- function(l1_mode, l3_mode, l1_rt, l3_rt) {
  is_brt_start <- (l1_rt == "Rapid Transit"); is_brt_end <- (l3_rt == "Rapid Transit")
  if(l1_mode == "metro" && l3_mode == "metro") return(10)
  if((l1_mode == "bus" && l3_mode == "metro" && is_brt_start) || (l1_mode == "metro" && l3_mode == "bus" && is_brt_end)) return(20)
  if((l1_mode == "bus" && l3_mode == "metro" && !is_brt_start) || (l1_mode == "metro" && l3_mode == "bus" && !is_brt_end)) return(30)
  if(l1_mode == "bus" && l3_mode == "bus") {
    if(is_brt_start && is_brt_end) return(40)
    if(xor(is_brt_start, is_brt_end)) return(50)
    return(60)
  }
  999
}

select_best_by_pct_tolerance <- function(df_candidates, tol_pct = 0.30) {
  if (is.null(df_candidates) || nrow(df_candidates) == 0) return(NULL)
  if (!("total" %in% names(df_candidates))) return(NULL)
  df_candidates <- df_candidates %>% dplyr::filter(!is.na(.data$total))
  if (nrow(df_candidates) == 0) return(NULL)
  min_total <- min(df_candidates$total, na.rm = TRUE)
  thr <- min_total * (1 + tol_pct)
  df_allowed <- df_candidates %>% dplyr::filter(.data$total <= thr) %>% dplyr::arrange(.data$prio, .data$total)
  if (nrow(df_allowed) == 0) return(NULL)
  df_allowed[1, , drop = FALSE]
}

# ------------------------------------------------------------------------------
# 11C. PATH DETAILS (Updated: Masked Counts + Line Adjustment)
# ------------------------------------------------------------------------------
.impl_pd <- function(graph, from_node, to_node) {
  res <- list(n_transfers=0, n_bus_transfers=0, n_metro_transfers=0, n_mode_switches=0, n_bus_routes=0, n_metro_lines=0, n_stops=0, has_brt=FALSE, dist_metro_m=0, dist_brt_m=0, dist_bus_std_m=0, dist_walk_transfer_m=0, seg_str_metro="")
  if(is.na(from_node) || is.na(to_node)) return(res)
  if(!from_node %in% V(graph)$name || !to_node %in% V(graph)$name) return(res)
  if(from_node == to_node) return(res)
  path_obj <- suppressWarnings(shortest_paths(graph, from = from_node, to = to_node, weights = E(graph)$weight, output = "epath"))
  edge_seq <- path_obj$epath[[1]]
  if(length(edge_seq) == 0) return(res)
  e_types  <- E(graph)$edge_type[edge_seq]; e_dists  <- suppressWarnings(as.numeric(E(graph)$real_distance[edge_seq])); e_routes <- E(graph)$route_code[edge_seq]
  
  # --- VALIDATION ---
  forbidden_transfers <- c("bus_bus_exact", "metro_bus", 
                           "metro_bus_manual", "bus_bus_proximity", "bus_bus_standard")
  
  if(e_types[1] %in% forbidden_transfers) return(res)
  if(e_types[length(e_types)] %in% forbidden_transfers) return(res)
  
  all_transfer_types <- c("metro_metro_transfer", forbidden_transfers)
  is_tr <- e_types %in% all_transfer_types
  
  if(length(is_tr) > 1 && any(is_tr & dplyr::lag(is_tr, default=FALSE))) return(res)
  
  # --- MASKING LOGIC FOR COUNTS ---
  # Create a mask to IGNORE the first/last edge if it is a metro_metro_transfer
  count_mask <- rep(TRUE, length(e_types))
  if(e_types[1] == "metro_metro_transfer") count_mask[1] <- FALSE
  if(length(e_types) > 1 && e_types[length(e_types)] == "metro_metro_transfer") count_mask[length(e_types)] <- FALSE
  
  # --- UPDATED COUNTS (Apply mask) ---
  res$n_metro_transfers <- sum(e_types == "metro_metro_transfer" & count_mask)
  res$n_bus_transfers   <- sum(e_types %in% c("bus_bus_exact", "bus_bus_proximity", "bus_bus_standard"))
  res$n_mode_switches   <- sum(e_types %in% c("metro_bus", "metro_bus_manual"))
  res$n_transfers       <- res$n_metro_transfers + res$n_bus_transfers + res$n_mode_switches
  
  keys <- E(graph)$unique_route_key[edge_seq]
  res$n_bus_routes  <- dplyr::n_distinct(keys[e_types == "bus_route" & !is.na(keys)])
  
  # --- LINE COUNT ADJUSTMENT (Feeder Logic) ---
  res$n_metro_lines <- dplyr::n_distinct(keys[e_types == "metro_line" & !is.na(keys)])
  
  # If we started with a transfer (masked), and have >1 line, ignore the first 'feeder' line
  if(e_types[1] == "metro_metro_transfer" && res$n_metro_lines > 1) {
    res$n_metro_lines <- res$n_metro_lines - 1
  }
  # If we ended with a transfer (masked), and still have >1 line, ignore the last 'feeder' line
  if(length(e_types) > 1 && e_types[length(e_types)] == "metro_metro_transfer" && res$n_metro_lines > 1) {
    res$n_metro_lines <- res$n_metro_lines - 1
  }
  
  res$n_stops       <- sum(e_types %in% c("metro_line", "bus_route"))
  
  mask_metro <- e_types == "metro_line"
  mask_bus   <- e_types == "bus_route"
  
  # Apply count_mask to the walk distance calculation too
  mask_walk  <- (e_types %in% all_transfer_types) & count_mask 
  
  safe_routes <- as.character(e_routes); mask_brt_route <- safe_routes %in% c("11", "12", "13")
  mask_brt_leg <- mask_bus & mask_brt_route; mask_std_leg <- mask_bus & !mask_brt_route
  
  res$dist_metro_m <- sum(e_dists[mask_metro], na.rm=TRUE)
  res$dist_brt_m <- sum(e_dists[mask_brt_leg], na.rm=TRUE) 
  res$dist_bus_std_m <- sum(e_dists[mask_std_leg], na.rm=TRUE) 
  res$dist_walk_transfer_m <- sum(e_dists[mask_walk], na.rm=TRUE)
  
  if(any(mask_metro)) {
    metro_segments <- e_dists[mask_metro]; metro_segments <- metro_segments[!is.na(metro_segments)]
    if(length(metro_segments) > 0) res$seg_str_metro <- paste(round(metro_segments, 1), collapse = ";")
  }
  res$has_brt <- any(mask_brt_leg)
  res
}

}  # ===== end REUSE_S18_TRANSIT gate (else-branch = full transit rebuild) =====

# ==============================================================================
# 12) PART 12 — PROCESS WEIGHTED POINTS (FINAL)
# ==============================================================================
message("\n[12] Processing weighted random points...")

# ------------------------------------------------------------------------------
# PRECOMPUTE FAST LOOKUPS for the weighted design (mirrors Section 18; the memoizing
# wrappers and .PCACHE persist from Section 18, and the transit graph is identical).
# ------------------------------------------------------------------------------
.l1m_geo <- setNames(L1_metro_lookup$L1_metro_geo_km, as.character(L1_metro_lookup$id))
.l1m_net <- setNames(L1_metro_lookup$L1_metro_net_km, as.character(L1_metro_lookup$id))
.l1b_geo <- setNames(L1_bus_lookup$L1_bus_geo_km,     as.character(L1_bus_lookup$id))
.l1b_net <- setNames(L1_bus_lookup$L1_bus_net_km,     as.character(L1_bus_lookup$id))
.l3m_geo <- setNames(L3_metro_lookup$L3_metro_geo_km, as.character(L3_metro_lookup$clinic_id))
.l3m_net <- setNames(L3_metro_lookup$L3_metro_net_km, as.character(L3_metro_lookup$clinic_id))
.l3b_geo <- setNames(L3_bus_lookup$L3_bus_geo_km,     as.character(L3_bus_lookup$clinic_id))
.l3b_net <- setNames(L3_bus_lookup$L3_bus_net_km,     as.character(L3_bus_lookup$clinic_id))
.ct_clinic_id <- setNames(as.character(clinic_targets$clinic_id),  paste0(clinic_targets$id, "||", clinic_targets$dest_type))
.ct_dest_geo  <- setNames(as.character(clinic_targets$dest_id_geo), paste0(clinic_targets$id, "||", clinic_targets$dest_type))
.rc_net <- setNames(rp_comparison$net_km_p2t, paste0(rp_comparison$id, "||", rp_comparison$dest_type))
.rc_geo <- setNames(rp_comparison$geo_km,     paste0(rp_comparison$id, "||", rp_comparison$dest_type))
.rp_m_idx <- st_nearest_feature(rp_sample, stations_proj)
.rp_b_idx <- st_nearest_feature(rp_sample, bus_stops_proj)
.rp_metro_node <- setNames(paste0("M_", stations_proj$stn_id[.rp_m_idx]),       as.character(rp_sample$id))
.rp_bus_node   <- setNames(paste0("B_", bus_stops_proj$bus_stop_id[.rp_b_idx]), as.character(rp_sample$id))
.rp_bus_rt_v   <- as.character(bus_stops_proj$route_type[.rp_b_idx]); .rp_bus_rt_v[is.na(.rp_bus_rt_v)] <- "Standard"
.rp_bus_rt     <- setNames(.rp_bus_rt_v, as.character(rp_sample$id))
.cl_all <- rbind(
  clinics_private %>% transmute(.dkey = paste0("priv_", id)),
  clinics_public  %>% transmute(.dkey = paste0("pub_",  id))
)
.cl_m_idx <- st_nearest_feature(.cl_all, stations_proj)
.cl_b_idx <- st_nearest_feature(.cl_all, bus_stops_proj)
.cl_metro_node <- setNames(paste0("M_", stations_proj$stn_id[.cl_m_idx]),       .cl_all$.dkey)
.cl_bus_node   <- setNames(paste0("B_", bus_stops_proj$bus_stop_id[.cl_b_idx]), .cl_all$.dkey)
.cl_bus_rt_v   <- as.character(bus_stops_proj$route_type[.cl_b_idx]); .cl_bus_rt_v[is.na(.cl_bus_rt_v)] <- "Standard"
.cl_bus_rt     <- setNames(.cl_bus_rt_v, .cl_all$.dkey)
message("   [precompute] weighted-design lookups + vectorized nearest-transit ready.")

dest_types <- c("nearest_priv", "median_priv", "nearest_pub", "median_pub")
results <- list()

if(!exists("rp_sample")) stop("rp_sample not found.")

if (Sys.getenv("SKIP_S19_LOOP") == "1") {
  message("SKIP_S19_LOOP=1: reusing existing sample_test_results_weighted.rds; skipping the weighted per-point loop.")
  results_df <- readRDS("sample_test_results_weighted.rds")
} else {
for(i in 1:nrow(rp_sample)) {
  if(i %% 100 == 0) message(paste("--- Processing point", i, "of", nrow(rp_sample), "---"))
  rp_id <- rp_sample$id[i]; .rpk <- as.character(rp_id)

  L1_metro_m <- get_L1_from_lookup(rp_id, "metro", L1_metro_lookup, L1_bus_lookup, ratio_lookup)
  L1_bus_m   <- get_L1_from_lookup(rp_id, "bus",   L1_metro_lookup, L1_bus_lookup, ratio_lookup)

  near_metro_rp <- list(node_id = .rp_metro_node[[.rpk]])
  near_bus_rp   <- list(node_id = .rp_bus_node[[.rpk]])
  near_bus_rt_type <- .rp_bus_rt[[.rpk]]

  for(dest_type in dest_types) {
    .tk <- paste0(rp_id, "||", dest_type)
    if(!(.tk %in% names(.ct_clinic_id))) next
    clinic_id <- .ct_clinic_id[[.tk]]
    .dkey <- .ct_dest_geo[[.tk]]
    if(!(.dkey %in% names(.cl_metro_node))) next

    L3_metro_m <- get_L3_from_lookup(rp_id, dest_type, "metro", clinic_targets, L3_metro_lookup, L3_bus_lookup, ratio_L3_metro_priv, ratio_L3_metro_pub, ratio_L3_bus_priv, ratio_L3_bus_pub)
    L3_bus_m   <- get_L3_from_lookup(rp_id, dest_type, "bus",   clinic_targets, L3_metro_lookup, L3_bus_lookup, ratio_L3_metro_priv, ratio_L3_metro_pub, ratio_L3_bus_priv, ratio_L3_bus_pub)

    near_metro_cl <- list(node_id = .cl_metro_node[[.dkey]])
    near_bus_cl   <- list(node_id = .cl_bus_node[[.dkey]])
    near_bus_cl_rt_type <- .cl_bus_rt[[.dkey]]

    road_dist_m <- NA_real_
    if(.tk %in% names(.rc_net)) {
      val_km <- .rc_net[[.tk]]
      imp_factor <- if(grepl("priv", dest_type)) (if(grepl("nearest", dest_type)) ratio_lookup["nearest_priv"] else ratio_lookup["median_priv"]) else (if(grepl("nearest", dest_type)) ratio_lookup["nearest_pub"] else ratio_lookup["median_pub"])
      if(is.na(imp_factor)) imp_factor <- 1.3
      if(is.na(val_km) || is.infinite(val_km)) val_km <- .rc_geo[[.tk]] * imp_factor
      road_dist_m <- val_km * 1000
    }
    
    metro_same_stn <- (near_metro_rp$node_id == near_metro_cl$node_id)
    if(metro_same_stn) {
      L2_mo <- NA_real_; tot_mo <- NA_real_; mo_mets <- list(n_transfers=NA, n_lines=NA, n_stops=NA)
    } else {
      L2_mo <- get_L2_distance_simple(near_metro_rp$node_id, near_metro_cl$node_id, g_metro_only)
      if(is.na(L2_mo)) { tot_mo <- NA_real_; mo_mets <- list(n_transfers=NA, n_lines=NA, n_stops=NA) } else {
        tot_mo <- L1_metro_m + L2_mo + L3_metro_m
        det_mo <- get_path_details(g_metro_only, near_metro_rp$node_id, near_metro_cl$node_id)
        mo_mets <- list(n_transfers = det_mo$n_transfers, n_lines = det_mo$n_metro_lines, n_stops = det_mo$n_stops)
      }
    }
    metro_closer <- (L1_metro_m < road_dist_m)
    metro_on_route <- check_on_route_via_distances(L1_metro_m, L3_metro_m, road_dist_m)
    
    invalid_metro_bus <- (near_bus_cl$node_id == near_bus_rp$node_id); invalid_bus_metro <- (near_metro_cl$node_id == near_metro_rp$node_id)
    combos <- list(
      list(type="Metro-Metro", l1=L1_metro_m, l3=L3_metro_m, l1_mode="metro", l3_mode="metro", s=near_metro_rp$node_id, e=near_metro_cl$node_id),
      list(type="Metro-Bus",   l1=L1_metro_m, l3=L3_bus_m,   l1_mode="metro", l3_mode="bus",   s=near_metro_rp$node_id, e=near_bus_cl$node_id),
      list(type="Bus-Metro",   l1=L1_bus_m,   l3=L3_metro_m, l1_mode="bus",   l3_mode="metro", s=near_bus_rp$node_id,   e=near_metro_cl$node_id),
      list(type="Bus-Bus",     l1=L1_bus_m,   l3=L3_bus_m,   l1_mode="bus",   l3_mode="bus",   s=near_bus_rp$node_id,   e=near_bus_cl$node_id)
    )
    combo_rows <- list()
    for(cmb in combos) {
      if(cmb$s == cmb$e) next
      if(cmb$type == "Metro-Bus" && invalid_metro_bus) next
      if(cmb$type == "Bus-Metro" && invalid_bus_metro) next
      l2 <- get_L2_distance_real(cmb$s, cmb$e, g_multimodal)
      if(is.na(l2)) next
      tot <- cmb$l1 + l2 + cmb$l3
      prio <- get_combo_priority(l1_mode = cmb$l1_mode, l3_mode = cmb$l3_mode, l1_rt = near_bus_rt_type, l3_rt = near_bus_cl_rt_type)
      combo_rows[[length(combo_rows)+1]] <- data.frame(type=cmb$type, l1_mode=cmb$l1_mode, l3_mode=cmb$l3_mode, s=cmb$s, e=cmb$e, l1=cmb$l1, l2=l2, l3=cmb$l3, total=tot, prio=prio, stringsAsFactors=FALSE)
    }
    combos_df <- bind_rows(combo_rows)
    best_row  <- select_best_by_pct_tolerance(combos_df, tol_pct = TOL_PCT)
    min_tot <- NA_real_; best_prio <- NA_integer_; best_combo <- NULL
    if(!is.null(best_row) && nrow(best_row) == 1) {
      min_tot <- best_row$total[1]; best_prio <- best_row$prio[1]
      best_combo <- list(type=best_row$type[1], l1_mode=best_row$l1_mode[1], l3_mode=best_row$l3_mode[1], s=best_row$s[1], e=best_row$e[1], l1=best_row$l1[1], l2=best_row$l2[1], l3=best_row$l3[1], total=best_row$total[1])
    }
    
    mm_mets <- list(n_bt=NA, n_mt=NA, n_ms=NA, n_br=NA, n_ml=NA, n_st=NA)
    mm_same_acc <- FALSE; mm_closer <- NA; mm_on_route <- NA
    mm_dist_metro <- 0; mm_dist_brt <- 0; mm_dist_std <- 0; mm_dist_walk <- 0; mm_seg_str <- ""; mm_has_brt <- FALSE
    
    if(!is.na(min_tot) && !is.null(best_combo)) {
      det_mm <- get_path_details(g_multimodal, best_combo$s, best_combo$e)
      mm_mets$n_bt <- det_mm$n_bus_transfers; mm_mets$n_mt <- det_mm$n_metro_transfers; mm_mets$n_ms <- det_mm$n_mode_switches
      mm_mets$n_br <- det_mm$n_bus_routes; mm_mets$n_ml <- det_mm$n_metro_lines; mm_mets$n_st <- det_mm$n_stops; mm_has_brt <- det_mm$has_brt 
      mm_dist_metro <- det_mm$dist_metro_m; mm_dist_brt <- det_mm$dist_brt_m; mm_dist_std <- det_mm$dist_bus_std_m; mm_dist_walk <- det_mm$dist_walk_transfer_m
      mm_seg_str <- det_mm$seg_str_metro
      mm_same_acc <- (best_combo$s == best_combo$e); mm_closer <- (best_combo$l1 < road_dist_m); mm_on_route <- check_on_route_via_distances(best_combo$l1, best_combo$l3, road_dist_m)
    } else {
      if((near_metro_rp$node_id == near_metro_cl$node_id) && (near_bus_rp$node_id == near_bus_cl$node_id)) mm_same_acc <- TRUE
    }
    
    if(is.na(tot_mo) && is.na(min_tot)) { best_mode <- NA; best_total <- NA_real_
    } else if(is.na(min_tot)) { best_mode <- "Metro-only"; best_total <- tot_mo
    } else if(is.na(tot_mo)) { best_mode <- "Multimodal"; best_total <- min_tot
    } else {
      final_df <- bind_rows(data.frame(mode="Metro-only", total=tot_mo, prio=10, stringsAsFactors=FALSE), data.frame(mode="Multimodal", total=min_tot, prio=best_prio, stringsAsFactors=FALSE))
      best_final <- select_best_by_pct_tolerance(final_df, tol_pct = TOL_PCT)
      best_mode <- best_final$mode[1]; best_total <- best_final$total[1]
    }
    
    results[[length(results) + 1]] <- data.frame(
      rp_id = rp_id, dest_type = dest_type, road_dist_m = road_dist_m,
      metro_only_total_m = tot_mo, metro_L1_m = L1_metro_m, metro_L2_m = L2_mo, metro_L3_m = L3_metro_m,
      metro_transfers = mo_mets$n_transfers, metro_lines = mo_mets$n_lines, metro_dwell = if(is.na(mo_mets$n_stops)) NA else max(0, mo_mets$n_stops - 1),
      metro_same_stn = metro_same_stn, metro_closer = metro_closer, metro_on_route = metro_on_route,
      multi_total_m = min_tot, multi_path_type = if(is.null(best_combo) || is.na(min_tot)) NA else best_combo$type,
      mm_dist_metro_m = mm_dist_metro, mm_dist_brt_m = mm_dist_brt, mm_dist_std_m = mm_dist_std, mm_dist_walk_m = mm_dist_walk, mm_metro_segments = mm_seg_str,
      multi_L1_mode = if(is.null(best_combo) || is.na(min_tot)) NA else best_combo$l1_mode,
      multi_L1_m = if(is.null(best_combo) || is.na(min_tot)) NA else best_combo$l1,
      multi_L2_m = if(is.null(best_combo) || is.na(min_tot)) NA else best_combo$l2,
      multi_L3_m = if(is.null(best_combo) || is.na(min_tot)) NA else best_combo$l3,
      mm_bus_tr = mm_mets$n_bt, mm_metro_tr = mm_mets$n_mt, mm_mode_sw = mm_mets$n_ms,
      mm_bus_rt = mm_mets$n_br, mm_metro_ln = mm_mets$n_ml, mm_tot_stops = if(is.na(mm_mets$n_st)) NA else max(0, mm_mets$n_st - 1),
      multi_has_brt = mm_has_brt, mm_same_acc = mm_same_acc, mm_closer = mm_closer, mm_on_route = mm_on_route,
      best_mode = best_mode, best_total_m = best_total, stringsAsFactors = FALSE
    )
  }
}

results_df <- bind_rows(results)
}
saveRDS(results_df, "sample_test_results_weighted.rds")
message("[OK] Part 12 complete.")

# ==============================================================================
# 13) DISPLAY RESULTS
# ==============================================================================
message("\n[13] Summary stats:")
print(results_df %>% group_by(dest_type) %>% summarise(mean_best_km = mean(best_total_m, na.rm=TRUE)/1000, metro_share = mean(best_mode=="Metro-only", na.rm=TRUE), .groups="drop"))

# ==============================================================================
# 14) SAVE OUTPUTS
# ==============================================================================
message("\n[14] Saving outputs...")
saveRDS(g_multimodal, "g_multimodal_sample_weighted.rds")
saveRDS(g_metro_only, "g_metro_only_sample_weighted.rds")
saveRDS(results_df,  "sample_test_results_weighted.rds")

# ==============================================================================
# SETUP: LOAD LIBRARIES & DATA
# ==============================================================================
suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(flextable)
  library(officer)
  library(viridisLite)
})

message("📂 Loading saved data...")

# Check if file exists
if (!file.exists("sample_test_results_weighted.rds")) {
  stop("❌ Error: 'ssample_test_results_weightedrds' not found. Please run Parts 8-14 first.")
}

# Load the results dataframe
results_df <- readRDS("sample_test_results_weighted.rds")

message(paste("✅ Data loaded with", nrow(results_df), "rows."))

# ==============================================================================
# 14b) R1-5 NEW ANCHORS — travel-time / accessibility (WEIGHTED; mirrors S18 14b)
#   Computes farthest_/random_ (priv/pub) on the WEIGHTED points, reusing the
#   weighted clinic draws (table1_new_anchors$assignments[["Population-weighted"]])
#   and S19's in-scope weighted lookups/routers. Augments results_df BEFORE the
#   phantom-point patch so results_df_clean inherits the new anchors. Cached to
#   sample_test_results_weighted_newanchors_N<N>.rds (FORCE_S19_NEWANCHORS=1).
# ==============================================================================
N_RANDOM_DRAWS <- suppressWarnings(as.integer(Sys.getenv("N_RANDOM_DRAWS", "3")))
if (is.na(N_RANDOM_DRAWS) || N_RANDOM_DRAWS < 1L) N_RANDOM_DRAWS <- 3L
.s19_data_dir <- "C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities/Data"
.t1_cache_w <- file.path(.s19_data_dir, sprintf("table1_new_anchors_N%d.rds", N_RANDOM_DRAWS))
.na_cache_w <- file.path(.s19_data_dir, sprintf("sample_test_results_weighted_newanchors_N%d.rds", N_RANDOM_DRAWS))

if (file.exists(.na_cache_w) && Sys.getenv("FORCE_S19_NEWANCHORS") != "1") {
  .new_anchor_results_w <- readRDS(.na_cache_w)
  message(sprintf("[S19 R1-5] Loaded cached weighted new-anchor results (N=%d): %d rows", N_RANDOM_DRAWS, nrow(.new_anchor_results_w)))
} else if (!file.exists(.t1_cache_w)) {
  warning("[S19 R1-5] Table 1 anchor cache not found (", .t1_cache_w, "); skipping weighted new-anchor travel-time.")
  .new_anchor_results_w <- results_df[0, ]
} else {
  message(sprintf("[S19 R1-5] Computing weighted new-anchor travel time (N=%d draws) ...", N_RANDOM_DRAWS))

  .L3_from_clinic <- function(clinic_id, transit_type, is_private) {
    if (transit_type == "metro") {
      if (!(clinic_id %in% names(.l3m_geo))) return(NA_real_)
      net <- .l3m_net[[clinic_id]]
      if (!is.na(net) && !is.infinite(net)) return(net * 1000)
      ratio <- if (is_private) ratio_L3_metro_priv else ratio_L3_metro_pub
      return(.l3m_geo[[clinic_id]] * ratio * 1000)
    } else {
      if (!(clinic_id %in% names(.l3b_geo))) return(NA_real_)
      net <- .l3b_net[[clinic_id]]
      if (!is.na(net) && !is.infinite(net)) return(net * 1000)
      ratio <- if (is_private) ratio_L3_bus_priv else ratio_L3_bus_pub
      return(.l3b_geo[[clinic_id]] * ratio * 1000)
    }
  }

  .tt_one <- function(rp_id, dest_type, dkey, clinic_id, road_dist_m,
                      L1_metro_m, L1_bus_m, near_metro_rp_id, near_bus_rp_id, near_bus_rt_type) {
    is_priv <- grepl("priv", dest_type)
    L3_metro_m <- .L3_from_clinic(clinic_id, "metro", is_priv)
    L3_bus_m   <- .L3_from_clinic(clinic_id, "bus",   is_priv)
    near_metro_cl_id    <- .cl_metro_node[[dkey]]
    near_bus_cl_id      <- .cl_bus_node[[dkey]]
    near_bus_cl_rt_type <- .cl_bus_rt[[dkey]]

    metro_same_stn <- (near_metro_rp_id == near_metro_cl_id)
    if (metro_same_stn) {
      L2_mo <- NA_real_; tot_mo <- NA_real_; mo_mets <- list(n_transfers=NA, n_lines=NA, n_stops=NA)
    } else {
      L2_mo <- get_L2_distance_simple(near_metro_rp_id, near_metro_cl_id, g_metro_only)
      if (is.na(L2_mo)) {
        tot_mo <- NA_real_; mo_mets <- list(n_transfers=NA, n_lines=NA, n_stops=NA)
      } else {
        tot_mo <- L1_metro_m + L2_mo + L3_metro_m
        det_mo <- get_path_details(g_metro_only, near_metro_rp_id, near_metro_cl_id)
        mo_mets <- list(n_transfers=det_mo$n_transfers, n_lines=det_mo$n_metro_lines, n_stops=det_mo$n_stops)
      }
    }
    metro_closer   <- (L1_metro_m < road_dist_m)
    metro_on_route <- check_on_route_via_distances(L1_metro_m, L3_metro_m, road_dist_m)

    invalid_metro_bus <- (near_bus_cl_id == near_bus_rp_id)
    invalid_bus_metro <- (near_metro_cl_id == near_metro_rp_id)
    combos <- list(
      list(type="Metro-Metro", l1=L1_metro_m, l3=L3_metro_m, l1_mode="metro", l3_mode="metro", s=near_metro_rp_id, e=near_metro_cl_id),
      list(type="Metro-Bus",   l1=L1_metro_m, l3=L3_bus_m,   l1_mode="metro", l3_mode="bus",   s=near_metro_rp_id, e=near_bus_cl_id),
      list(type="Bus-Metro",   l1=L1_bus_m,   l3=L3_metro_m, l1_mode="bus",   l3_mode="metro", s=near_bus_rp_id,   e=near_metro_cl_id),
      list(type="Bus-Bus",     l1=L1_bus_m,   l3=L3_bus_m,   l1_mode="bus",   l3_mode="bus",   s=near_bus_rp_id,   e=near_bus_cl_id)
    )
    combo_rows <- list()
    for (cmb in combos) {
      if (cmb$s == cmb$e) next
      if (cmb$type == "Metro-Bus" && invalid_metro_bus) next
      if (cmb$type == "Bus-Metro" && invalid_bus_metro) next
      l2 <- get_L2_distance_real(cmb$s, cmb$e, g_multimodal)
      if (is.na(l2)) next
      tot <- cmb$l1 + l2 + cmb$l3
      prio <- get_combo_priority(l1_mode=cmb$l1_mode, l3_mode=cmb$l3_mode, l1_rt=near_bus_rt_type, l3_rt=near_bus_cl_rt_type)
      combo_rows[[length(combo_rows)+1]] <- data.frame(type=cmb$type, l1_mode=cmb$l1_mode, l3_mode=cmb$l3_mode, s=cmb$s, e=cmb$e, l1=cmb$l1, l2=l2, l3=cmb$l3, total=tot, prio=prio, stringsAsFactors=FALSE)
    }
    combos_df <- bind_rows(combo_rows)
    best_row  <- select_best_by_pct_tolerance(combos_df, tol_pct = TOL_PCT)
    min_tot <- NA_real_; best_prio <- NA_integer_; best_combo <- NULL
    if (!is.null(best_row) && nrow(best_row) == 1) {
      min_tot <- best_row$total[1]; best_prio <- best_row$prio[1]
      best_combo <- list(type=best_row$type[1], l1_mode=best_row$l1_mode[1], l3_mode=best_row$l3_mode[1], s=best_row$s[1], e=best_row$e[1], l1=best_row$l1[1], l2=best_row$l2[1], l3=best_row$l3[1], total=best_row$total[1])
    }
    mm_mets <- list(n_bt=NA, n_mt=NA, n_ms=NA, n_br=NA, n_ml=NA, n_st=NA)
    mm_same_acc <- FALSE; mm_closer <- NA; mm_on_route <- NA
    mm_dist_metro <- 0; mm_dist_brt <- 0; mm_dist_std <- 0; mm_dist_walk <- 0
    mm_seg_str <- ""; mm_has_brt <- FALSE
    if (!is.na(min_tot) && !is.null(best_combo)) {
      det_mm <- get_path_details(g_multimodal, best_combo$s, best_combo$e)
      mm_mets$n_bt <- det_mm$n_bus_transfers; mm_mets$n_mt <- det_mm$n_metro_transfers
      mm_mets$n_ms <- det_mm$n_mode_switches; mm_mets$n_br <- det_mm$n_bus_routes
      mm_mets$n_ml <- det_mm$n_metro_lines;   mm_mets$n_st <- det_mm$n_stops
      mm_has_brt   <- det_mm$has_brt
      mm_dist_metro <- det_mm$dist_metro_m; mm_dist_brt <- det_mm$dist_brt_m
      mm_dist_std   <- det_mm$dist_bus_std_m; mm_dist_walk <- det_mm$dist_walk_transfer_m
      mm_seg_str    <- det_mm$seg_str_metro
      mm_same_acc <- (best_combo$s == best_combo$e)
      mm_closer   <- (best_combo$l1 < road_dist_m)
      mm_on_route <- check_on_route_via_distances(best_combo$l1, best_combo$l3, road_dist_m)
    } else {
      if ((near_metro_rp_id == near_metro_cl_id) && (near_bus_rp_id == near_bus_cl_id)) mm_same_acc <- TRUE
    }
    if (is.na(tot_mo) && is.na(min_tot)) {
      best_mode <- NA; best_total <- NA_real_
    } else if (is.na(min_tot)) {
      best_mode <- "Metro-only"; best_total <- tot_mo
    } else if (is.na(tot_mo)) {
      best_mode <- "Multimodal"; best_total <- min_tot
    } else {
      final_df <- bind_rows(data.frame(mode="Metro-only", total=tot_mo, prio=10, stringsAsFactors=FALSE), data.frame(mode="Multimodal", total=min_tot, prio=best_prio, stringsAsFactors=FALSE))
      best_final <- select_best_by_pct_tolerance(final_df, tol_pct = TOL_PCT)
      best_mode <- best_final$mode[1]; best_total <- best_final$total[1]
    }
    data.frame(
      rp_id = rp_id, dest_type = dest_type, road_dist_m = road_dist_m,
      metro_only_total_m = tot_mo,
      metro_L1_m = L1_metro_m, metro_L2_m = L2_mo, metro_L3_m = L3_metro_m,
      metro_transfers = mo_mets$n_transfers, metro_lines = mo_mets$n_lines,
      metro_dwell = if(is.na(mo_mets$n_stops)) NA else max(0, mo_mets$n_stops - 1),
      metro_same_stn = metro_same_stn, metro_closer = metro_closer, metro_on_route = metro_on_route,
      multi_total_m = min_tot, multi_path_type = if(is.null(best_combo) || is.na(min_tot)) NA else best_combo$type,
      mm_dist_metro_m = mm_dist_metro, mm_dist_brt_m = mm_dist_brt, mm_dist_std_m = mm_dist_std, mm_dist_walk_m = mm_dist_walk,
      mm_metro_segments = mm_seg_str,
      multi_L1_mode = if(is.null(best_combo) || is.na(min_tot)) NA else best_combo$l1_mode,
      multi_L1_m = if(is.null(best_combo) || is.na(min_tot)) NA else best_combo$l1,
      multi_L2_m = if(is.null(best_combo) || is.na(min_tot)) NA else best_combo$l2,
      multi_L3_m = if(is.null(best_combo) || is.na(min_tot)) NA else best_combo$l3,
      mm_bus_tr = mm_mets$n_bt, mm_metro_tr = mm_mets$n_mt, mm_mode_sw = mm_mets$n_ms,
      mm_bus_rt = mm_mets$n_br, mm_metro_ln = mm_mets$n_ml,
      mm_tot_stops = if(is.na(mm_mets$n_st)) NA else max(0, mm_mets$n_st - 1),
      multi_has_brt = mm_has_brt,
      mm_same_acc = mm_same_acc, mm_closer = mm_closer, mm_on_route = mm_on_route,
      best_mode = best_mode, best_total_m = best_total, stringsAsFactors = FALSE
    )
  }

  .t1w <- readRDS(.t1_cache_w)
  .asgw <- .t1w$assignments[["Population-weighted"]]
  if (is.null(.asgw)) stop("[S19 R1-5] Population-weighted assignments missing in ", .t1_cache_w)

  worklist <- bind_rows(
    .asgw$far %>% transmute(rp_id = as.character(id), dest_type, dest_id_geo, road_net_km = net_km_p2t, road_geo_km = geo_km),
    .asgw$rnd %>% transmute(rp_id = as.character(id), dest_type, dest_id_geo, road_net_km = net_km_p2t, road_geo_km = geo_km)
  ) %>% filter(rp_id %in% names(.rp_metro_node))

  .anchor_ratio <- worklist %>% group_by(dest_type) %>%
    summarise(r = mean(road_net_km, na.rm=TRUE) / mean(road_geo_km, na.rm=TRUE), .groups="drop")
  .anchor_ratio_v <- setNames(.anchor_ratio$r, .anchor_ratio$dest_type)
  worklist <- worklist %>%
    mutate(clinic_id = gsub("^(priv_|pub_)", "", dest_id_geo),
           .imp = unname(.anchor_ratio_v[dest_type]),
           .imp = ifelse(is.na(.imp) | is.infinite(.imp), 1.3, .imp),
           road_dist_m = ifelse(!is.na(road_net_km) & !is.infinite(road_net_km), road_net_km * 1000, road_geo_km * .imp * 1000))

  uids <- unique(worklist$rp_id)
  .l1m_per <- setNames(vapply(uids, function(u) get_L1_from_lookup(u, "metro", L1_metro_lookup, L1_bus_lookup, ratio_lookup), numeric(1)), uids)
  .l1b_per <- setNames(vapply(uids, function(u) get_L1_from_lookup(u, "bus",   L1_metro_lookup, L1_bus_lookup, ratio_lookup), numeric(1)), uids)

  .vids <- intersect(head(rp_sample$id, 150), names(.rp_metro_node))
  if (length(.vids) > 0) {
    .vchk <- bind_rows(lapply(.vids, function(u) {
      .tk <- paste0(u, "||nearest_priv"); if (!(.tk %in% names(.ct_clinic_id))) return(NULL)
      cid <- .ct_clinic_id[[.tk]]; dk <- .ct_dest_geo[[.tk]]; rdm <- NA_real_
      if (.tk %in% names(.rc_net)) { vkm <- .rc_net[[.tk]]; impf <- ratio_lookup["nearest_priv"]; if (is.na(impf)) impf <- 1.3; if (is.na(vkm) || is.infinite(vkm)) vkm <- .rc_geo[[.tk]] * impf; rdm <- vkm * 1000 }
      .tt_one(u, "nearest_priv", dk, cid, rdm, .l1m_per[[u]], .l1b_per[[u]], .rp_metro_node[[u]], .rp_bus_node[[u]], .rp_bus_rt[[u]])
    }))
    .vref <- results_df %>% filter(dest_type == "nearest_priv", rp_id %in% .vchk$rp_id) %>% select(rp_id, ref_best = best_total_m, ref_mo = metro_only_total_m, ref_mm = multi_total_m)
    .vcmp <- .vchk %>% select(rp_id, best_total_m, metro_only_total_m, multi_total_m) %>% left_join(.vref, by="rp_id")
    message(sprintf("[S19 R1-5] [validate] nearest_priv max|delta| best/mo/mm = %.3e / %.3e / %.3e (n=%d)",
                    max(abs(.vcmp$best_total_m - .vcmp$ref_best), na.rm=TRUE), max(abs(.vcmp$metro_only_total_m - .vcmp$ref_mo), na.rm=TRUE),
                    max(abs(.vcmp$multi_total_m - .vcmp$ref_mm), na.rm=TRUE), nrow(.vcmp)))
  }

  message(sprintf("[S19 R1-5] routing %d (point,clinic) rows across %d points ...", nrow(worklist), length(uids)))
  .out <- vector("list", nrow(worklist)); .wl <- worklist
  for (k in seq_len(nrow(.wl))) {
    if (k %% 20000 == 0) message(sprintf("   [S19 R1-5] %d / %d", k, nrow(.wl)))
    u <- .wl$rp_id[k]
    .out[[k]] <- .tt_one(u, .wl$dest_type[k], .wl$dest_id_geo[k], .wl$clinic_id[k], .wl$road_dist_m[k],
                         .l1m_per[[u]], .l1b_per[[u]], .rp_metro_node[[u]], .rp_bus_node[[u]], .rp_bus_rt[[u]])
  }
  .new_anchor_results_w <- bind_rows(.out)
  saveRDS(.new_anchor_results_w, .na_cache_w)
  message(sprintf("[S19 R1-5] weighted new-anchor results: %d rows -> %s", nrow(.new_anchor_results_w), .na_cache_w))
}

if (nrow(.new_anchor_results_w) > 0) {
  results_df <- bind_rows(results_df, .new_anchor_results_w)
  message(sprintf("[S19 R1-5] results_df augmented to %d rows (%d dest_types).", nrow(results_df), dplyr::n_distinct(results_df$dest_type)))
}

# ==============================================================================
# 15) FINAL TABLE GENERATION (WEIGHTED + HARDCODED PATCH)
# ==============================================================================
message("\n[15] Building final weighted flextable with Hardcoded Patch...")

# --- STEP 1: APPLY HARDCODED PATCH ---
if(exists("results_df")) {
  message("Applying hardcoded patch for specific phantom loop points...")
  
  # The 4 population-weighted phantom origins that landed EXACTLY on a private
  # dental facility (nearest_priv road_dist = 0 km).
  target_ids <- c("pt1624", "pt1029", "pt5807", "pt859")

  results_df_clean <- results_df %>%
    mutate(
      # 2026-07-11 FIX: restrict the exclusion to the GENUINE artifact only — the
      # nearest_priv trip of each phantom origin. Because the origin sits on the
      # facility (road_dist = 0), its "multimodal" route is a spurious boarding
      # loop back to the same clinic, so nearest_priv multimodal must stay NA.
      # Every OTHER destination from these origins is an ordinary long-distance
      # trip (verified: 6-48 km, valid Metro/Bus chains) and MUST be left intact
      # so the MM-reach correction re-routes it like any other origin. The prior
      # patch over-extended the artifact to ALL destinations of these 4 points.
      is_target = rp_id %in% target_ids & dest_type == "nearest_priv",
      
      # 1. Set "Access node nearest RP = access node nearest facility" to TRUE
      mm_same_acc = ifelse(is_target, TRUE, mm_same_acc),
      
      # 2. Exclude from Multimodal Distances, Ratios, and Counts (Set to NA)
      multi_total_m   = ifelse(is_target, NA_real_, multi_total_m),
      multi_L1_m      = ifelse(is_target, NA_real_, multi_L1_m),
      multi_L2_m      = ifelse(is_target, NA_real_, multi_L2_m),
      multi_L3_m      = ifelse(is_target, NA_real_, multi_L3_m),
      mm_closer       = ifelse(is_target, NA, mm_closer),
      multi_path_type = ifelse(is_target, NA_character_, multi_path_type),
      
      # Exclude counts
      mm_bus_tr       = ifelse(is_target, NA_real_, mm_bus_tr),
      mm_metro_tr     = ifelse(is_target, NA_real_, mm_metro_tr),
      mm_mode_sw      = ifelse(is_target, NA_real_, mm_mode_sw),
      mm_bus_rt       = ifelse(is_target, NA_real_, mm_bus_rt),
      mm_metro_ln     = ifelse(is_target, NA_real_, mm_metro_ln),
      mm_tot_stops    = ifelse(is_target, NA_real_, mm_tot_stops),
      
      # 3. Handle "Best Class" (Metro vs Bus vs Both)
      # - If Metro chain exists -> "Metro-only"
      # - If Metro chain missing -> NA (Excluded from variable)
      best_mode = case_when(
        is_target & !is.na(metro_only_total_m) ~ "Metro-only",
        is_target & is.na(metro_only_total_m)  ~ NA_character_,
        TRUE ~ best_mode # Keep original for all other points
      )
    ) %>%
    select(-is_target) # Clean up helper column
  
  message("Patch applied to points: ", paste(target_ids, collapse=", "))
  
} else {
  stop("results_df not found. Please run Part 12 first.")
}

# --- STEP 2: DEFINE STATS FUNCTIONS ---

msd <- function(v, scale=1, digits=1) { 
  v <- as.numeric(v)
  if(length(v) == 0 || all(is.na(v))) return("-")
  v <- v / scale
  paste0(formatC(mean(v, na.rm=TRUE), format="f", digits=digits, big.mark=","), 
         " (", formatC(sd(v, na.rm=TRUE), format="f", digits=digits, big.mark=","), ")") 
}

npct <- function(cond) { 
  if(length(cond) == 0) return("-")
  n <- sum(cond, na.rm=TRUE)
  N <- sum(!is.na(cond))
  if(N == 0) return("-")
  paste0(formatC(n, format="d", big.mark=","), " (", sprintf("%.1f%%", 100*n/N), ")") 
}

get_column_stats <- function(df, d_type) {
  dat <- df %>% filter(dest_type == d_type)
  sub_mo <- dat %>% filter(!is.na(metro_only_total_m))
  sub_mm <- dat %>% filter(!is.na(multi_total_m))
  
  # Ratio helper
  get_ratio <- function(num, denom) {
    valid <- !is.na(num) & !is.na(denom) & denom > 0
    if(sum(valid) == 0) return("-")
    r <- num[valid] / denom[valid]
    # Filter out extreme outliers if any remain
    r <- r[r < 100] 
    msd(r, digits=1)
  }
  
  c(
    msd(dat$road_dist_m, scale=1000),
    npct(!is.na(dat$metro_only_total_m)), 
    npct(is.na(dat$metro_only_total_m)), 
    npct(dat$metro_same_stn == TRUE), 
    npct(is.na(dat$metro_only_total_m) & dat$metro_same_stn == FALSE),
    
    msd(sub_mo$metro_only_total_m, scale=1000), 
    msd((sub_mo$metro_L1_m/sub_mo$metro_only_total_m)*100), 
    msd((sub_mo$metro_L2_m/sub_mo$metro_only_total_m)*100), 
    msd((sub_mo$metro_L3_m/sub_mo$metro_only_total_m)*100), 
    npct(sub_mo$metro_only_total_m < sub_mo$road_dist_m),
    
    msd(sub_mo$metro_transfers), 
    msd(sub_mo$metro_lines), 
    msd(sub_mo$metro_dwell), 
    npct(sub_mo$metro_closer == TRUE), 
    get_ratio(sub_mo$metro_only_total_m, sub_mo$road_dist_m),
    
    npct(!is.na(dat$multi_total_m)), 
    npct(is.na(dat$multi_total_m)), 
    npct(dat$mm_same_acc == TRUE), 
    npct(is.na(dat$multi_total_m) & dat$mm_same_acc == FALSE),
    
    msd(sub_mm$multi_total_m, scale=1000), 
    msd((sub_mm$multi_L1_m/sub_mm$multi_total_m)*100), 
    msd((sub_mm$multi_L2_m/sub_mm$multi_total_m)*100), 
    msd((sub_mm$multi_L3_m/sub_mm$multi_total_m)*100), 
    npct(sub_mm$multi_total_m < sub_mm$road_dist_m),
    npct(sub_mm$mm_closer == TRUE), 
    get_ratio(sub_mm$multi_total_m, sub_mm$road_dist_m),
    
    npct(sub_mm$multi_path_type == "Bus-Bus"), 
    npct(sub_mm$multi_path_type == "Bus-Metro"), 
    npct(sub_mm$multi_path_type == "Metro-Bus"), 
    npct(sub_mm$multi_path_type == "Metro-Metro"),
    
    msd(sub_mm$mm_bus_tr), 
    msd(sub_mm$mm_metro_tr), 
    msd(ifelse(sub_mm$multi_path_type == "Bus-Metro", 1, 0)), 
    msd(ifelse(sub_mm$multi_path_type == "Metro-Bus", 1, 0)), 
    msd(sub_mm$mm_mode_sw), 
    msd(sub_mm$mm_bus_tr + sub_mm$mm_metro_tr + sub_mm$mm_mode_sw),
    
    msd(sub_mm$mm_bus_rt), 
    msd(sub_mm$mm_metro_ln), 
    msd(sub_mm$mm_tot_stops),
    
    npct(dat$best_mode == "Metro-only"), 
    npct(dat$best_mode == "Multimodal")
  )
}

# --- STEP 3: DEFINE VARIABLES & GROUPS ---

vars <- c(
  "Actual road distance in km [RP->Facility] mean (SD)",
  "Available (non-missing total chain) n (%)", "Missing (total chain) n (%)", "Station nearest RP = station nearest facility n (%)", "Non-reachable through metro n (%)",
  "Total chain in km mean (SD)", "L1 share (RP->Metro) mean % (SD)", "L2 share (Metro->Metro) mean % (SD)", "L3 share (Metro->Facility) mean % (SD)", "Total chain shorter than actual road distance [RP->Facility] n (%)",
  "Number of station transfers mean (SD)", "Number of distinct metro lines mean (SD)", "Number of intermediate metro stations mean (SD)", "Station nearest RP closer to RP than facility n (%)", 
  "Ratio of total chain to actual road distance [RP->Facility] mean (SD)",
  
  "Available (non-missing total chain) n (%)", "Missing (total chain) n (%)", "Access node nearest RP = access node nearest facility n (%)", "Non-reachable through transit n (%)",
  "Total best path using both networks in km mean (SD)", "L1 share (RP->Access) mean % (SD)", "L2 share (Access->Access) mean % (SD)", "L3 share (Access->Facility) mean % (SD)", "Total chain shorter than actual road distance [RP->Facility] n (%)",
  "Access node nearest RP closer to RP than facility n (%)", 
  "Ratio of total best path to actual road distance [RP->Facility] mean (SD)",
  
  "Bus start and Bus end n (%)", "Bus start and Metro end n (%)", "Metro start and Bus end n (%)", "Metro start and Metro end n (%)",
  "Number of bus route changes mean (SD)", "Number of metro line changes mean (SD)", "Number of mode switches from Bus->Metro mean (SD)", "Number of mode switches from Metro->Bus mean (SD)", "Number of mode switches total mean (SD)", "Number of total route and line changes and mode switches mean (SD)",
  "Number of distinct bus routes used mean (SD)", "Number of distinct metro lines used mean (SD)", "Number of intermediate stops/stations mean (SD)",
  "Metro-only n (%)", "Both networks n (%)"
)

groups <- c(
  "General",
  rep("Metro Accessibility", 14),
  rep("Multimodal Shortest Path", 11),
  rep("Best Mode (Both Networks)", 4),
  rep("Transfers (Best Multimodal Path)", 6),
  rep("Best Path Composition (Distinct Routes/Lines)", 3),
  rep("Best Class (Metro vs Bus vs Both)", 2)
)

# --- STEP 4: BUILD TABLE ---

df_tbl <- data.frame(
  Group = groups,
  Variable = vars,
  Priv_Nearest  = get_column_stats(results_df_clean, "nearest_priv"),
  Pub_Nearest   = get_column_stats(results_df_clean, "nearest_pub"),
  Priv_Specific = get_column_stats(results_df_clean, "median_priv"),
  Pub_Specific  = get_column_stats(results_df_clean, "median_pub"),
  stringsAsFactors = FALSE
)

std_border <- fp_border(color = "black", width = 1.5)
thin_border <- fp_border(color = "black", width = 1)

ft <- as_grouped_data(df_tbl, groups = "Group") %>% 
  as_flextable() %>%
  set_header_labels(Variable = "Variable", Priv_Nearest = "Private", Pub_Nearest = "Public", Priv_Specific = "Private", Pub_Specific = "Public") %>%
  add_header_row(values = c("", "Nearest Facility", "Median-distance Facility"), colwidths = c(1, 2, 2)) %>%
  theme_zebra(odd_header = "transparent", odd_body = "#EFEFEF", even_body = "#FFFFFF") %>%
  color(part = "header", color = "black") %>% bold(part = "header") %>%
  hline_top(part = "header", border = std_border) %>% hline(i = 1, part = "header", border = thin_border) %>%
  hline_bottom(part = "header", border = thin_border) %>% hline_bottom(part = "body", border = std_border) %>%
  align(j = 2:5, align = "center", part = "all") %>% padding(padding = 3, part = "all") %>% autofit()

ft

save_as_docx(ft, path = "Accessibility_metrics_for_random_points_weighted_FIXED.docx")
message("Table generated and saved as 'Accessibility_metrics_for_random_points_weighted_FIXED.docx'")

# ------------------------------------------------------------------------------
# 15b) R1-5 — weighted Table 2 WITH new anchors (both versions; original above
# unchanged). Reuses global .build_acc_ft + S19 vars/groups/get_column_stats;
# random_* columns POOL over draws. Uses results_df_clean (patched).
# ------------------------------------------------------------------------------
if (exists(".build_acc_ft") && all(c("farthest_priv","farthest_pub","random_priv","random_pub") %in% results_df_clean$dest_type)) {
  df_tbl_all <- data.frame(
    Group = groups, Variable = vars,
    Priv_Nearest  = get_column_stats(results_df_clean, "nearest_priv"),
    Pub_Nearest   = get_column_stats(results_df_clean, "nearest_pub"),
    Priv_Median   = get_column_stats(results_df_clean, "median_priv"),
    Pub_Median    = get_column_stats(results_df_clean, "median_pub"),
    Priv_Farthest = get_column_stats(results_df_clean, "farthest_priv"),
    Pub_Farthest  = get_column_stats(results_df_clean, "farthest_pub"),
    Priv_Random   = get_column_stats(results_df_clean, "random_priv"),
    Pub_Random    = get_column_stats(results_df_clean, "random_pub"),
    stringsAsFactors = FALSE
  )
  ft_all <- .build_acc_ft(df_tbl_all, rep(c("Private","Public"), 4),
    c("", "Nearest Facility", "Median-distance Facility", "Farthest Facility", "Random Facility"), c(1,2,2,2,2))
  save_as_docx(ft_all, path = "Accessibility_metrics_for_random_points_weighted_ALL8.docx")
  message("[S19 R1-5] Wrote 8-column weighted Table 2 -> Accessibility_metrics_for_random_points_weighted_ALL8.docx")

  df_tbl_new <- data.frame(
    Group = groups, Variable = vars,
    Priv_Farthest = get_column_stats(results_df_clean, "farthest_priv"),
    Pub_Farthest  = get_column_stats(results_df_clean, "farthest_pub"),
    Priv_Random   = get_column_stats(results_df_clean, "random_priv"),
    Pub_Random    = get_column_stats(results_df_clean, "random_pub"),
    stringsAsFactors = FALSE
  )
  ft_new <- .build_acc_ft(df_tbl_new, rep(c("Private","Public"), 2),
    c("", "Farthest Facility", "Random Facility"), c(1,2,2))
  save_as_docx(ft_new, path = "Accessibility_metrics_for_random_points_weighted_NEW4.docx")
  message("[S19 R1-5] Wrote supplementary weighted Table 2 -> Accessibility_metrics_for_random_points_weighted_NEW4.docx")
} else {
  message("[S19 R1-5] New anchors absent / .build_acc_ft missing; skipping weighted ALL8/NEW4 Table 2.")
}

# ------------------------------------------------------------------------------
# MM-REACH FULL CORRECTION (WEIGHTED) — mirror of the S18 hook, run on
# results_df_clean (the phantom-patched df the weighted Table 2 above is built
# from; the 4 phantom points carry mm_same_acc=TRUE so the re-route auto-excludes
# them). Writes the corrected weighted Table 2 + before/after summary. If
# STOP_AFTER_MM_FULL=1, exits here (both weightings corrected).
# ------------------------------------------------------------------------------
if (Sys.getenv("RUN_MM_FULL") == "1") {
  .mmf_code <- if (exists("base_dir")) file.path(dirname(base_dir), "Code") else file.path(getwd(), "Code")
  .mmf <- file.path(.mmf_code, "_r14_mm_reach_full.R")
  if (file.exists(.mmf) && exists("results_df_clean")) {
    source(.mmf)
    r14_mm_reach_full("weighted", include_random = (Sys.getenv("MM_FULL_RANDOM") != "0"), rdf = results_df_clean)
    if (Sys.getenv("MM_FULL_CHAIN") != "0") {
      for (.dep in c("_r14_imputation.R","_r13_permode.R","_r14_chain_be.R")) { .d <- file.path(.mmf_code, .dep); if (file.exists(.d)) source(.d) }
      if (exists("r14_chain_be")) r14_chain_be("weighted", corrected = TRUE)
    }
  } else message("[MM-FULL] weighted correction skipped (_r14_mm_reach_full.R or results_df_clean missing).")
  if (Sys.getenv("STOP_AFTER_MM_FULL") == "1") {
    message("[RUN STATUS] STOP_AFTER_MM_FULL -- MM-REACH full correction complete (both weightings), exiting.")
    quit(save = "no", status = 0)
  }
}

# ==============================================================================
# 16) PART 16 — TRAVEL TIME ANALYSIS (WEIGHTED)
# ==============================================================================
message("\n", paste(rep("=", 60), collapse = ""))
message("PART 16: TRAVEL TIME ANALYSIS (Weighted)")
message(paste(rep("=", 60), collapse = ""))

SEED <- 12345; N_SIM_ITER <- 1000; speeds <- seq(5, 80, 1)
speed_walk <- 4; speed_metro_max <- 80; metro_accel_rate <- 1.2; metro_max_speed_ms <- speed_metro_max * 1000 / 3600
metro_time_to_max_speed <- metro_max_speed_ms / metro_accel_rate; metro_dist_to_max_speed_m <- 0.5 * metro_accel_rate * metro_time_to_max_speed^2; metro_dist_to_max_speed_km <- metro_dist_to_max_speed_m / 1000
metro_min_segment_for_cruise <- 2 * metro_dist_to_max_speed_km; brt_speed_cap <- 80; brt_gap_factor <- 0.5; stop_penalty_min <- 0.5; metro_wait_max <- 7.5; bus_wait_max <- 20

as_num <- function(x) { if (is.null(x) || length(x) == 0) return(NA_real_); suppressWarnings(as.numeric(x)) }
calc_brt_speed <- function(traffic_speed) { speed <- traffic_speed + (brt_gap_factor * (brt_speed_cap - traffic_speed)); pmin(speed, brt_speed_cap) }
tm <- function(dist_km, speed_kmh) { (dist_km / speed_kmh) * 60 }
split_legs <- function(total_km, pct_l1, pct_l2) { total_km <- as_num(total_km); p1 <- as_num(pct_l1) / 100; p2 <- as_num(pct_l2) / 100; p1[is.na(p1)] <- 0; p2[is.na(p2)] <- 0; l1 <- total_km * p1; l2 <- total_km * p2; l3 <- total_km - l1 - l2; list(l1 = l1, l2 = l2, l3 = l3) }
calc_segment_time_min <- function(seg_m) { if(is.na(seg_m) || seg_m <= 0) return(0); seg_km <- seg_m / 1000; if (seg_km >= metro_min_segment_for_cruise) { accel_time_hr <- metro_time_to_max_speed / 3600; cruise_dist_km <- seg_km - (2 * metro_dist_to_max_speed_km); cruise_time_hr <- cruise_dist_km / speed_metro_max; total_time_hr <- (2 * accel_time_hr) + cruise_time_hr } else { half_dist_m <- (seg_km * 1000) / 2; time_each_phase_s <- sqrt(2 * half_dist_m / metro_accel_rate); total_time_hr <- (2 * time_each_phase_s) / 3600 }; return(total_time_hr * 60) }
process_metro_string <- function(s) { if(is.na(s) || s == "") return(0); parts <- as.numeric(unlist(strsplit(s, ";"))); parts <- parts[!is.na(parts) & parts > 0]; if(length(parts) == 0) return(0); sum(sapply(parts, calc_segment_time_min)) }

if(!"mm_metro_segments" %in% names(results_df)) results_df$mm_metro_segments <- ""
metro_times_precalc <- sapply(results_df$mm_metro_segments, process_metro_string)

df_analysis <- results_df %>% mutate(road_dist_km = as_num(road_dist_m)/1000, chain_metro_km = as_num(metro_only_total_m)/1000, chain_multi_km = as_num(multi_total_m)/1000, dist_brt_km = replace(as_num(mm_dist_brt_m)/1000, is.na(as_num(mm_dist_brt_m)), 0), dist_std_km = replace(as_num(mm_dist_std_m)/1000, is.na(as_num(mm_dist_std_m)), 0), metro_time_precise_min = metro_times_precalc, multi_transfer_walk_km = if("mm_dist_walk_m" %in% names(.)) as_num(mm_dist_walk_m)/1000 else (coalesce(as_num(mm_bus_tr),0)+coalesce(as_num(mm_metro_tr),0)+coalesce(as_num(mm_mode_sw),0))*0.05, multi_transfer_walk_km = replace(multi_transfer_walk_km, is.na(multi_transfer_walk_km), 0), multi_total_safe = ifelse(is.na(as_num(multi_total_m)) | as_num(multi_total_m)==0, 1, as_num(multi_total_m)), pct_multi_l1 = (as_num(multi_L1_m)/multi_total_safe)*100, pct_multi_l2 = (as_num(multi_L2_m)/multi_total_safe)*100, metro_total_safe = ifelse(is.na(as_num(metro_only_total_m)) | as_num(metro_only_total_m)==0, 1, as_num(metro_only_total_m)), pct_metro_l1 = (as_num(metro_L1_m)/metro_total_safe)*100, pct_metro_l2 = (as_num(metro_L2_m)/metro_total_safe)*100, metro_transfer_walk_km = coalesce(as_num(metro_transfers),0)*0.05, metro_only_transfers = coalesce(as_num(metro_transfers),0), metro_dwell = coalesce(as_num(metro_dwell),0), multi_dwell = coalesce(as_num(mm_tot_stops),0), path_type = multi_path_type)

scenario_map <- list("nearest_priv"=list(type="Private", target="Nearest"), "median_priv"=list(type="Private", target="Specific"), "nearest_pub"=list(type="Public", target="Nearest"), "median_pub"=list(type="Public", target="Specific"))
indiv_list <- list()
for (dest in names(scenario_map)) {
  sc <- scenario_map[[dest]]; type <- sc$type; tgt <- sc$target
  dat <- df_analysis %>% filter(dest_type == dest); if (nrow(dat) == 0) next
  sub_car <- dat %>% filter(!is.na(road_dist_km)); sub_metro <- dat %>% filter(!is.na(chain_metro_km)); sub_multi <- dat %>% filter(!is.na(chain_multi_km))
  N_car <- nrow(sub_car); N_metro <- nrow(sub_metro); N_multi <- nrow(sub_multi)
  seed_base <- SEED + match(dest, names(scenario_map)) * 1000
  
  if (N_metro > 0) {
    metro_stop_pen <- stop_penalty_min * sub_metro$metro_dwell; metro_legs <- split_legs(sub_metro$chain_metro_km, sub_metro$pct_metro_l1, sub_metro$pct_metro_l2)
    metro_l2_walk <- pmin(pmax(sub_metro$metro_transfer_walk_km, 0, na.rm=TRUE), pmax(metro_legs$l2, 0, na.rm=TRUE), na.rm=TRUE); metro_l2_ride <- pmax(metro_legs$l2 - metro_l2_walk, 0, na.rm=TRUE)
    set.seed(seed_base + 10); metro_wait_avg <- numeric(N_metro); n_boardings_vec <- pmax(0L, as.integer(sub_metro$metro_only_transfers), na.rm=TRUE) + 1L
    for(i in 1:N_metro) { waits <- replicate(N_SIM_ITER, sum(runif(n_boardings_vec[i], 0, metro_wait_max))); metro_wait_avg[i] <- mean(waits) }
    calc_agg_metro_speed <- function(dist_km) { t_hr <- numeric(length(dist_km)); for(i in seq_along(dist_km)) { d <- dist_km[i]; if(is.na(d)) { t_hr[i] <- NA; next }; d <- max(0.01, d); if(d >= metro_min_segment_for_cruise) { t_hr[i] <- (2*metro_time_to_max_speed/3600) + (d - 2*metro_dist_to_max_speed_km)/speed_metro_max } else { t_hr[i] <- (2*sqrt(2*(d*1000/2)/metro_accel_rate))/3600 } }; pmin(pmax(dist_km/t_hr, 20), 80) }
    metro_spd_agg <- calc_agg_metro_speed(metro_l2_ride)
  }
  
  if (N_multi > 0) {
    multi_stop_pen <- stop_penalty_min * sub_multi$multi_dwell; multi_legs <- split_legs(sub_multi$chain_multi_km, sub_multi$pct_multi_l1, sub_multi$pct_multi_l2)
    multi_wait_avg <- numeric(N_multi); set.seed(seed_base + 20)
    for (j in 1:N_multi) {
      pt <- sub_multi$path_type[j]; n_metro_tr <- coalesce(sub_multi$mm_metro_tr[j], 0); n_bus_tr <- coalesce(sub_multi$mm_bus_tr[j], 0); n_mode_sw <- coalesce(sub_multi$mm_mode_sw[j], 0)
      if (is.na(pt)) { multi_wait_avg[j] <- 0; next }
      n_mb <- 0; n_bb <- 0
      if (pt == "Metro-Metro") { n_mb = 1 + n_metro_tr } else if (pt == "Bus-Bus") { n_bb = 1 + n_bus_tr } else if (pt == "Metro-Bus") { n_mb = 1 + n_metro_tr; n_bb = n_mode_sw + n_bus_tr } else if (pt == "Bus-Metro") { n_bb = 1 + n_bus_tr; n_mb = n_mode_sw + n_metro_tr }
      waits <- replicate(N_SIM_ITER, { w_m <- if (n_mb > 0) sum(runif(n_mb, 0, metro_wait_max)) else 0; w_b <- if (n_bb > 0) sum(runif(n_bb, 0, bus_wait_max)) else 0; w_m + w_b }); multi_wait_avg[j] <- mean(waits)
    }
  }
  
  for (s in speeds) {
    brt_physics_speed <- calc_brt_speed(s); std_physics_speed <- s
    if (N_car > 0) { t_vec <- tm(sub_car$road_dist_km, s); if(length(t_vec) == N_car) indiv_list[[length(indiv_list)+1]] <- data.frame(Type=type, Target=tgt, Speed=s, Mode="Car-only (direct to clinic)", Time=t_vec, Time_Deterministic=t_vec) }
    if (N_metro > 0) {
      det_t <- tm(metro_legs$l1, s) + tm(metro_l2_ride, metro_spd_agg) + tm(metro_l2_walk, speed_walk) + tm(metro_legs$l3, speed_walk) + metro_stop_pen; t_vec <- det_t + metro_wait_avg; if (length(t_vec) == N_metro) indiv_list[[length(indiv_list)+1]] <- data.frame(Type=type, Target=tgt, Speed=s, Mode="Car-initiated Metro-only", Time=t_vec, Time_Deterministic=det_t)
      det_t <- tm(metro_legs$l1, speed_walk) + tm(metro_l2_ride, metro_spd_agg) + tm(metro_l2_walk, speed_walk) + tm(metro_legs$l3, speed_walk) + metro_stop_pen; t_vec <- det_t + metro_wait_avg; if (length(t_vec) == N_metro) indiv_list[[length(indiv_list)+1]] <- data.frame(Type=type, Target=tgt, Speed=s, Mode="Walk-initiated Metro-only", Time=t_vec, Time_Deterministic=det_t)
    }
    if (N_multi > 0) {
      t_brt <- (sub_multi$dist_brt_km/brt_physics_speed)*60; t_std <- (sub_multi$dist_std_km/std_physics_speed)*60; t_metro <- sub_multi$metro_time_precise_min; t_transfer <- (sub_multi$multi_transfer_walk_km/speed_walk)*60; l2_time_exact <- t_brt + t_std + t_metro + t_transfer
      det_t <- tm(multi_legs$l1, s) + l2_time_exact + tm(multi_legs$l3, speed_walk) + multi_stop_pen; t_vec <- det_t + multi_wait_avg; indiv_list[[length(indiv_list)+1]] <- data.frame(Type=type, Target=tgt, Speed=s, Mode="Car-initiated Multimodal", Time=t_vec, Time_Deterministic=det_t)
      det_t <- tm(multi_legs$l1, speed_walk) + l2_time_exact + tm(multi_legs$l3, speed_walk) + multi_stop_pen; t_vec <- det_t + multi_wait_avg; indiv_list[[length(indiv_list)+1]] <- data.frame(Type=type, Target=tgt, Speed=s, Mode="Walk-initiated Multimodal", Time=t_vec, Time_Deterministic=det_t)
    }
  }
}
df_indiv <- bind_rows(indiv_list) %>% mutate(Type=as.character(Type), Target=as.character(Target), Speed=as.numeric(Speed), Time=as.numeric(Time), Time_Deterministic=as.numeric(Time_Deterministic))
message(paste("\n[OK] Individual travel time records:", nrow(df_indiv)))

# ==============================================================================
# 17) PART 17 — WEIGHTED VISUALIZATION & STATISTICS
# ==============================================================================
message("\n[17] Generating weighted figures...")

df_indiv <- df_indiv %>% mutate(Mode_family = case_when(grepl("^Car-only", Mode) ~ "Car-only (direct to clinic)", grepl("Metro-only", Mode) ~ "Metro-only", grepl("Multimodal", Mode) ~ "Multimodal", TRUE ~ Mode), Initiation = ifelse(grepl("^Walk-initiated", Mode), "Walk-initiated", "Car-initiated"))
df_sum <- df_indiv %>% group_by(Type, Target, Speed, Mode_family, Initiation) %>% summarise(Time_mean = mean(Time, na.rm=TRUE), .groups="drop")
saveRDS(df_indiv, "travel_time_individual_weighted.rds"); saveRDS(df_sum, "travel_time_summary_weighted.rds")

# -----------------------------------------------------------------------------
# 1. SETUP & LOAD DATA
# -----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(viridisLite)
})

message("📂 Loading analysis data...")

if (!file.exists("travel_time_individual_weighted.rds") || !file.exists("travel_time_summary_weighted.rds")) {
  stop("❌ Error: Saved RDS files not found. Please run Part 16 first.")
}

df_indiv <- readRDS("travel_time_individual_weighted.rds")
df_sum   <- readRDS("travel_time_summary_weighted.rds")

message("✅ Data loaded.")

# --- Re-Define Global Constants needed for plotting ---
speeds <- seq(5, 80, 1) # Needed for Physics plots
mode_order <- c("Car-only (direct to clinic)", "Metro-only", "Multimodal")

# Ensure factor levels are correct for coloring
df_sum$Mode_family <- factor(df_sum$Mode_family, levels = mode_order)
df_sum$Initiation <- factor(df_sum$Initiation, levels = c("Car-initiated", "Walk-initiated"))

df_indiv$Mode_family <- factor(df_indiv$Mode_family, levels = mode_order)
df_indiv$Initiation <- factor(df_indiv$Initiation, levels = c("Car-initiated", "Walk-initiated"))
###################################


base_theme <- theme_bw(base_size = 14) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "grey85", linewidth = 0.5), legend.position = "bottom", legend.box = "horizontal", plot.title = element_text(face = "bold"), axis.title = element_text(face = "bold"), legend.title = element_text(face = "bold"), strip.text = element_text(face = "bold"), strip.background = element_blank(), legend.key.width = unit(1.5, "cm"))
plasma3 <- viridisLite::plasma(3, end = 0.9); mode_colors <- c("Car-only (direct to clinic)"="grey50", "Metro-only"=plasma3[1], "Multimodal"=plasma3[2])

# Figure 1: Mean Time
# p_mean <- ggplot(df_sum, aes(x = Speed, y = Time_mean / 60, color = Mode_family, linetype = Initiation)) + geom_line(linewidth = 1.2, alpha = 1) + facet_grid(Type ~ Target, labeller = labeller(Target = c("Nearest" = "Nearest", "Specific" = "Median-distance"))) + scale_color_manual(values = mode_colors) + scale_linetype_manual(values = c("solid", "longdash")) + scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 0.5), expand = c(0, 0)) + scale_x_continuous(limits = c(5, 80), breaks = seq(10, 80, 10)) +
#   labs(title = "Mean Travel Time (Weighted) from Random Point to Dental Facility", x = "Average speed for car/standard bus (km/h)", y = "Mean travel time (hours)", color = "Travel mode", linetype = "Initiation") + base_theme + theme(panel.spacing.y = unit(1, "lines"))

p_mean <- ggplot(df_sum, aes(x = Speed, y = Time_mean / 60, color = Mode_family, linetype = Initiation)) +
  geom_line(linewidth = 1.2, alpha = 1) +
  
  # --- Facet Grid ---
  facet_grid(Type ~ Target, 
             labeller = labeller(Target = c("Nearest" = "Nearest", 
                                            "Specific" = "Median-distance"))) +
  
  scale_color_manual(values = mode_colors) +
  scale_linetype_manual(values = c("solid", "longdash")) +
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, 0.5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(5, 80), breaks = seq(10, 80, 10)) +
  labs(
    title = "Mean Travel Time from Weighted Random Point to Dental Facility",
    x = "Average speed for car/standard bus (km/h)", 
    y = "Mean travel time (hours)",
    color = "Travel mode", 
    linetype = "Initiation"
  ) +
  base_theme +
  
  # --- UPDATED THEME ---
  theme(
    panel.spacing.y = unit(2, "lines"),
    
    # --- INCREASE HORIZONTAL SPACING (Left vs Right) ---
    panel.spacing.x = unit(1, "lines"),
    
    # 1. Remove the box around the panel
    panel.border = element_blank(),
    
    # 2. Remove the X and Y axis lines
    axis.line = element_blank(),
    
    # 3. Remove the tick marks
    axis.ticks = element_blank()
  ) +
  
  guides(color = guide_legend(override.aes = list(linewidth = 1.3)), 
         linetype = guide_legend(override.aes = list(linewidth = 1.3)))

p_mean

ggsave("Fig_mean_travel_time_weighted.tiff", p_mean, width = 14, height = 9, dpi = 600)

# Figure 2: % Faster
car_label <- "Car-only (direct to clinic)"; modes_to_compare <- c("Metro-only", "Multimodal")
df_numbered <- df_indiv %>% filter(Mode_family %in% c(car_label, modes_to_compare)) %>% group_by(Type, Target, Speed, Mode_family, Initiation) %>% mutate(trip_id = row_number()) %>% ungroup()
df_wide <- df_numbered %>% select(Type, Target, Speed, trip_id, Mode_family, Initiation, Time) %>% pivot_wider(names_from = Mode_family, values_from = Time)
if (car_label %in% names(df_wide)) {
  df_cmp <- df_wide %>% pivot_longer(cols = any_of(modes_to_compare), names_to = "Mode_family", values_to = "Time_mode") %>% mutate(Time_car = .data[[car_label]], delta_min = Time_car - Time_mode, faster = !is.na(delta_min) & delta_min > 0)
  pct_faster <- df_cmp %>% group_by(Type, Target, Speed, Mode_family, Initiation) %>% summarise(pct_faster = 100 * mean(faster, na.rm = TRUE), .groups = "drop") %>% mutate(Mode_family = factor(Mode_family, levels = modes_to_compare))
  breakeven_points <- pct_faster %>% filter(Initiation == "Car-initiated") %>% group_by(Type, Target, Mode_family) %>% summarise(speed_at_50 = if (sum(!is.na(pct_faster)) >= 2 && max(pct_faster, na.rm = TRUE) >= 50) tryCatch(approx(x = pct_faster, y = Speed, xout = 50, ties = mean)$y, error = function(e) NA_real_) else NA_real_, .groups = "drop") %>% filter(!is.na(speed_at_50))
  
  # p_pct <- ggplot(pct_faster, aes(x = Speed, y = pct_faster, color = Mode_family, linetype = Initiation)) + geom_hline(yintercept = 50, color = "gray80", linetype = "dotted") + geom_vline(data = breakeven_points, aes(xintercept = speed_at_50, color = Mode_family), linetype = "dashed", linewidth = 0.4, alpha = 0.6, inherit.aes = FALSE, show.legend = FALSE) + geom_text(data = breakeven_points, aes(x = speed_at_50, y = 0, label = sprintf("%.0f", speed_at_50), color = Mode_family), vjust = -0.5, hjust = -0.2, size = 3, fontface = "bold", show.legend = FALSE, inherit.aes = FALSE) +
  #   geom_line(linewidth = 1.2) + facet_grid(Type ~ Target, labeller = labeller(Target = c("Nearest" = "Nearest", "Specific" = "Median-distance"))) + scale_color_manual(values = mode_colors) + scale_linetype_manual(values = c("solid", "longdash")) + scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 0)) + scale_x_continuous(limits = c(5, 80), breaks = seq(10, 80, 10)) +
  #   labs(title = "Percentage of Weighted Transit Trips Faster than Direct Driving", subtitle = "Dashed vertical lines indicate traffic speed where 50% of weighted trips are slower than driving.", x = "Average speed for car/standard bus (km/h)", y = "Percentage faster than car (%)", color = "Travel mode", linetype = "Initiation") + base_theme + theme(panel.spacing.y = unit(1, "lines"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  p_pct <- ggplot(pct_faster, aes(x = Speed, y = pct_faster, color = Mode_family, linetype = Initiation)) +
    # Reference line at 50%
    geom_hline(yintercept = 50, color = "gray80", linetype = "dotted") +
    
    # Breakeven Vertical Lines
    geom_vline(data = breakeven_points, 
               aes(xintercept = speed_at_50, color = Mode_family), 
               linetype = "dashed", linewidth = 0.4, alpha = 0.6,
               inherit.aes = FALSE, show.legend = FALSE) +
    
    # Breakeven Labels
    geom_text(data = breakeven_points, 
              aes(x = speed_at_50, y = 0, label = sprintf("%.0f", speed_at_50), color = Mode_family),
              vjust = -0.5, hjust = -0.2, size = 3, fontface = "bold", 
              show.legend = FALSE, inherit.aes = FALSE) +
    
    geom_line(linewidth = 1.2) + 
    
    # --- Facet Grid ---
    facet_grid(Type ~ Target, 
               labeller = labeller(Target = c("Nearest" = "Nearest", 
                                              "Specific" = "Median-distance"))) +
    
    scale_color_manual(values = mode_colors) +
    scale_linetype_manual(values = c("solid", "longdash")) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 0)) +
    scale_x_continuous(limits = c(5, 80), breaks = seq(10, 80, 10)) +
    
    labs(
      title = "Percentage of Weighted Transit Trips Faster than Direct Driving",
      subtitle = "Dashed vertical lines indicate traffic speed where 50% of weighted trips are slower than driving.",
      x = "Average speed for car/standard bus (km/h)", 
      y = "Percentage faster than car (%)",
      color = "Travel mode", 
      linetype = "Initiation"
    ) +
    base_theme +
    
    # --- UPDATED THEME ---
    theme(
      panel.spacing.y = unit(2, "lines"),
      panel.spacing.x = unit(1, "lines"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      
      # 1. Remove the box around the panel
      panel.border = element_blank(),
      
      # 2. Remove the X and Y axis lines
      axis.line = element_blank(),
      
      # 3. Remove the tick marks
      axis.ticks = element_blank()
    ) +
    
    guides(color = guide_legend(override.aes = list(linewidth = 1.3)), 
           linetype = guide_legend(override.aes = list(linewidth = 1.3)))
  
  ggsave("Fig_percent_faster_integrated_weighted.tiff", p_pct, width = 14, height = 9, dpi = 600)
}

# Figure 3: Time Saved
if (exists("df_cmp")) {
  df_savings_trend <- df_cmp %>% filter(delta_min > 0, Initiation == "Car-initiated") %>% group_by(Type, Target, Speed, Mode_family) %>% summarise(mean_saved_hours = mean(delta_min, na.rm = TRUE) / 60, .groups = "drop") %>% mutate(Target = factor(Target, levels = c("Nearest", "Specific")), Type = factor(Type, levels = c("Private", "Public")))
  # p_savings_trend <- ggplot(df_savings_trend, aes(x = Speed, y = mean_saved_hours, color = Mode_family)) + geom_line(linewidth = 1.2, alpha = 1) + facet_grid(Type ~ Target, labeller = labeller(Target = c("Nearest" = "Nearest", "Specific" = "Median-distance")), drop = FALSE) + scale_color_manual(values = mode_colors) + scale_y_continuous(limits = c(0, NA), breaks = seq(0, 4, 0.5), expand = c(0, 0.1), name = "Mean time saved (hours)") + scale_x_continuous(limits = c(5, 80), breaks = seq(10, 80, 10), name = "Average speed for car/standard bus (km/h)") +
  #   labs(title = "Mean Time Saved for Weighted Car-Initiated Transit Trips Faster than Direct Driving", color = "Travel mode") + base_theme + theme(panel.spacing.y = unit(1, "lines"))
  
  p_savings_trend <- ggplot(df_savings_trend, aes(x = Speed, y = mean_saved_hours, color = Mode_family)) +
    geom_line(linewidth = 1.2, alpha = 1) + 
    
    # --- Facet Grid ---
    facet_grid(Type ~ Target, 
               labeller = labeller(Target = c("Nearest" = "Nearest", 
                                              "Specific" = "Median-distance")), drop = FALSE) +
    
    scale_color_manual(values = mode_colors) +
    scale_y_continuous(limits = c(0, NA), breaks = seq(0, 4, 0.5), expand = c(0, 0.1), name = "Mean time saved (hours)") +
    scale_x_continuous(limits = c(5, 80), breaks = seq(10, 80, 10), name = "Average speed for car/standard bus (km/h)") +
    
    labs(
      title = "Mean Time Saved for Weighted Car-Initiated Transit Trips Faster than Direct Driving", 
      color = "Travel mode"
    ) +
    
    base_theme + 
    
    # --- UPDATED THEME ---
    theme(
      panel.spacing.y = unit(2, "lines"),
      
      panel.spacing.x = unit(1, "lines"),
      
      # 1. Remove the box around the panel
      panel.border = element_blank(),
      
      # 2. Remove the X and Y axis lines
      axis.line = element_blank(),
      
      # 3. Remove the tick marks
      axis.ticks = element_blank()
    ) +
    
    guides(color = guide_legend(override.aes = list(linewidth = 1.5)))
  
  ggsave("Fig_mean_time_savings_weighted.tiff", p_savings_trend, width = 14, height = 9, dpi = 600)
}

message("\n✅ Weighted analysis complete. All outputs saved with '_weighted' suffix.")

# ------------------------------------------------------------------------------
# 17b) R1-5 — WEIGHTED travel-time minutes for the new anchors (mirrors S18 17b)
# Reuses the global helpers defined in S18 (.tt_anchor_summaries/.mk_mean_fig/
# .breakeven/.ref) on the weighted df_analysis/df_sum; certified weighted Figs
# untouched. New: Fig_mean_travel_time_weighted_ALL8/NEW4.tiff + summary docx.
# ------------------------------------------------------------------------------
.tt_new_dts <- c("farthest_priv","farthest_pub","random_priv","random_pub")
if (exists("df_analysis") && exists("df_sum") && exists(".tt_anchor_summaries") &&
    exists(".mk_mean_fig") && all(.tt_new_dts %in% df_analysis$dest_type)) {
  message("[S19 R1-5] Computing weighted travel-time minutes for the 4 new anchors ...")
  .scen_new <- list(farthest_priv=c("Private","Farthest"), farthest_pub=c("Public","Farthest"),
                    random_priv=c("Private","Random"),     random_pub=c("Public","Random"))
  df_sum_new <- bind_rows(lapply(names(.scen_new), function(dt)
    .tt_anchor_summaries(df_analysis %>% filter(dest_type == dt), .scen_new[[dt]][1], .scen_new[[dt]][2])))
  df_sum_all <- bind_rows(df_sum, df_sum_new)

  if ("nearest_priv" %in% df_analysis$dest_type) {
    .ref_np <- df_sum %>% filter(Type=="Private", Target=="Nearest") %>%
      transmute(Speed, Mode_family=as.character(Mode_family), Initiation=as.character(Initiation), tt_ref=Time_mean)
    .chk_np <- .tt_anchor_summaries(df_analysis %>% filter(dest_type=="nearest_priv"), "Private","Nearest") %>%
      transmute(Speed, Mode_family=as.character(Mode_family), Initiation=as.character(Initiation), tt_new=Time_mean) %>%
      inner_join(.ref_np, by=c("Speed","Mode_family","Initiation"))
    message(sprintf("[S19 R1-5] [validate] nearest_priv travel-time max|delta| vs weighted df_sum = %.3f min", max(abs(.chk_np$tt_new - .chk_np$tt_ref), na.rm=TRUE)))
  }

  .mk_mean_fig(df_sum_all, c("Nearest","Specific","Farthest","Random"), "Fig_mean_travel_time_weighted_ALL8.tiff",
               "Mean Travel Time by Anchor (Nearest / Median / Farthest / Random), Population-weighted")
  .mk_mean_fig(df_sum_new, c("Farthest","Random"), "Fig_mean_travel_time_weighted_NEW4.tiff",
               "Mean Travel Time to Farthest / Random Dental Facility, Population-weighted")

  S_REF <- 40
  anchors_tab <- tibble::tribble(
    ~Type, ~Target, "Private","Nearest","Private","Specific","Private","Farthest","Private","Random",
    "Public","Nearest","Public","Specific","Public","Farthest","Public","Random")
  tt_summary <- anchors_tab %>% rowwise() %>% mutate(
    car_min_ref     = round(.ref(df_sum_all, Type, Target, "Car-only (direct to clinic)","Car-initiated"), 1),
    metro_walk_min  = round(.ref(df_sum_all, Type, Target, "Metro-only","Walk-initiated"), 1),
    multi_walk_ref  = round(.ref(df_sum_all, Type, Target, "Multimodal","Walk-initiated"), 1),
    breakeven_metro = round(.breakeven(df_sum_all, Type, Target, "Metro-only","Car-initiated"), 1),
    breakeven_multi = round(.breakeven(df_sum_all, Type, Target, "Multimodal","Car-initiated"), 1)
  ) %>% ungroup() %>%
    transmute(Anchor = paste(Type, Target), `Car (min, 40 km/h)`=car_min_ref, `Metro-only (min, walk)`=metro_walk_min,
              `Multimodal (min, walk, 40 km/h)`=multi_walk_ref, `Break-even speed: metro (km/h)`=breakeven_metro, `Break-even speed: multimodal (km/h)`=breakeven_multi)
  library(flextable); library(officer)
  save_as_docx(flextable(as.data.frame(tt_summary)) %>% theme_booktabs() %>% autofit(), path = "TravelTime_summary_new_anchors_weighted.docx")
  saveRDS(df_sum_all, "travel_time_summary_ALL8_weighted.rds")
  message("[S19 R1-5] wrote Fig_mean_travel_time_weighted_ALL8/NEW4.tiff + TravelTime_summary_new_anchors_weighted.docx")
  print(as.data.frame(tt_summary), row.names = FALSE)
} else {
  message("[S19 R1-5] New anchors / helpers not available; skipping weighted travel-time minutes.")
}

# ------------------------------------------------------------------------------
# R1-4 / R2-3f — imputation sensitivity (WEIGHTED). Gated, additive, default OFF.
# Mirror of the S18 hook on the weighted df_analysis/df_sum; writes
# R14_*_weighted.{rds,docx}. STOP_AFTER_R14=1 exits cleanly after both sections.
# ------------------------------------------------------------------------------
if (Sys.getenv("RUN_R14") == "1") {
  .r14_code <- if (exists("base_dir")) file.path(dirname(base_dir), "Code") else file.path(getwd(), "Code")
  .r14_orch <- file.path(.r14_code, "_r14_orchestrate.R")
  if (file.exists(.r14_orch)) {
    source(.r14_orch); r14_orchestrate("weighted")
    .pm <- file.path(.r14_code, "_r13_permode.R"); if (file.exists(.pm)) source(.pm)         # per-mode engine
    .r14_cb <- file.path(.r14_code, "_r14_chain_be.R"); if (file.exists(.r14_cb)) { source(.r14_cb); r14_chain_be("weighted") }  # L1-chain + 50%-crossing
  } else message("[R1-4] _r14_orchestrate.R not found at ", .r14_code, "; skipping weighted.")
}
# ------------------------------------------------------------------------------
# R1-3 — modelling-assumption sensitivity (WEIGHTED). Gated, additive, default OFF.
# Cheap post-processing sweep on the weighted df_analysis; writes R13_*_weighted.
# ------------------------------------------------------------------------------
if (Sys.getenv("RUN_R13") == "1") {
  .r13_code <- if (exists("base_dir")) file.path(dirname(base_dir), "Code") else file.path(getwd(), "Code")
  .r13_orch <- file.path(.r13_code, "_r13_orchestrate.R")
  if (file.exists(.r13_orch)) {
    source(.r13_orch); r13_orchestrate("weighted")
  } else message("[R1-3] _r13_orchestrate.R not found at ", .r13_code, "; skipping weighted.")
}

# R1-3 ENHANCED routing capture (WEIGHTED). Gated, additive, OFF by default.
# Needs the warm .PCACHE => run with the S19 loop ENABLED (no SKIP_S19_LOOP).
if (Sys.getenv("RUN_R13_ENH") == "1") {
  .r13_code <- if (exists("base_dir")) file.path(dirname(base_dir), "Code") else file.path(getwd(), "Code")
  .r13_enh  <- file.path(.r13_code, "_r13_enhanced.R")
  if (file.exists(.r13_enh)) {
    .r13_orch <- file.path(.r13_code, "_r13_orchestrate.R")
    if (file.exists(.r13_orch)) source(.r13_orch)   # r13_breakeven + .fin_* helpers
    .r13_se <- file.path(.r13_code, "_r13_enh_se.R"); if (file.exists(.r13_se)) source(.r13_se)  # SE-over-draws
    source(.r13_enh)
    r13_enh_capture("weighted", asg_key = "Population-weighted", t1_cache = .t1_cache_w)
    r13_enh_orchestrate("weighted", asg_key = "Population-weighted", t1_cache = .t1_cache_w)
  } else message("[R1-3-ENH] _r13_enhanced.R not found at ", .r13_code, "; skipping weighted.")
}

# R1-3 BOUNDS — combined-assumption Fig 2/3/4 bands (WEIGHTED). Gated, additive, OFF default.
if (Sys.getenv("RUN_R13_BOUNDS") == "1") {
  .r13_code <- if (exists("base_dir")) file.path(dirname(base_dir), "Code") else file.path(getwd(), "Code")
  for (.f in c("_r13_orchestrate.R","_r13_enhanced.R","_r13_bounds.R")) { .p <- file.path(.r13_code, .f); if (file.exists(.p)) source(.p) }
  if (exists("r13_bounds")) r13_bounds("weighted") else message("[R1-3-BOUNDS] _r13_bounds.R not found; skipping weighted.")
}

if (Sys.getenv("STOP_AFTER_R14") == "1" || Sys.getenv("STOP_AFTER_R13") == "1") {
  message("[RUN STATUS] STOP_AFTER_R14/R13 -- R1-4/R1-3 sensitivity complete, exiting.")
  quit(save = "no", status = 0)
}















# ==============================================================================
# DIAGNOSTIC: CHECK SAMPLE SIZES VS MEAN SAVINGS
# Paste this AFTER 'df_cmp' is created in Figure 2, but BEFORE the plotting code
# ==============================================================================

if (exists("df_cmp")) {
  message("\n--- DIAGNOSTIC: Checking Sample Sizes for High Speeds ---")
  
  diagnostic_stats <- df_cmp %>%
    # Focus on Car-initiated as that is usually where the "Speed > 30" issue happens
    filter(Initiation == "Car-initiated") %>% 
    group_by(Type, Target, Mode_family, Speed) %>%
    summarise(
      # 1. Total trips in simulation at this speed
      Total_Trips = n(),
      
      # 2. FREQUENCY: How many were actually faster? 
      # If this is < 5, the mean is unstable.
      N_Faster = sum(faster, na.rm = TRUE),
      
      # 3. PERCENTAGE: The curve in Figure 1
      Pct_Faster = mean(faster, na.rm = TRUE) * 100,
      
      # 4. MEAN SAVINGS: The curve in Figure 2
      Mean_Time_Saved = mean(delta_min[faster], na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    # Filter to show only the "Noisy" area (e.g., Speed >= 30 km/h)
    filter(Speed >= 30) %>%
    arrange(Type, Target, Mode_family, Speed)
  
  # Print the check to the console
  if (interactive()) View(as.data.frame(diagnostic_stats)) else print(as.data.frame(diagnostic_stats))
  
  # Optional: Save to CSV to view in Excel if the list is too long
  write.csv(diagnostic_stats, "diagnostic_high_speed_noise.csv", row.names = FALSE)
  message("Diagnostic table saved to 'diagnostic_high_speed_noise.csv'")
}


# ==============================================================================
# DIAGNOSTIC: FULL DOOR-TO-DOOR ITINERARY (L1 + L2 + L3)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(igraph)
})

# ------------------------------------------------------------------------------
# 1. SETUP PHYSICS CONSTANTS
# ------------------------------------------------------------------------------
TEST_SPEED_KMH   <- 40   
SPEED_WALK_KMH   <- 4
METRO_ACCEL      <- 1.2
METRO_MAX_KMH    <- 80
BRT_GAP_FACTOR   <- 0.5
BRT_CAP          <- 80

metro_max_ms <- METRO_MAX_KMH / 3.6
time_to_max  <- metro_max_ms / METRO_ACCEL
dist_to_max_m <- 0.5 * METRO_ACCEL * time_to_max^2
min_cruise_dist_m <- 2 * dist_to_max_m

calc_segment_time_min <- function(seg_m) {
  if(is.na(seg_m) || seg_m <= 0) return(0)
  seg_km <- seg_m / 1000
  if (seg_km >= min_cruise_dist_m/1000) {
    accel_time_hr <- (time_to_max) / 3600
    cruise_dist_km <- seg_km - (2 * dist_to_max_m/1000)
    cruise_time_hr <- cruise_dist_km / METRO_MAX_KMH
    total_time_hr <- (2 * accel_time_hr) + cruise_time_hr
  } else {
    half_dist_m <- (seg_km * 1000) / 2
    time_each_phase_s <- sqrt(2 * half_dist_m / METRO_ACCEL)
    total_time_hr <- (2 * time_each_phase_s) / 3600
  }
  return(total_time_hr * 60)
}

get_brt_speed <- function(traffic_speed) {
  speed <- traffic_speed + (BRT_GAP_FACTOR * (BRT_CAP - traffic_speed))
  min(speed, BRT_CAP)
}

# ------------------------------------------------------------------------------
# 2. HELPER: RESOLVE NODE NAMES
# ------------------------------------------------------------------------------
get_node_label <- function(node_str, stations, stops) {
  if(is.na(node_str)) return("Unknown")
  if(grepl("^M_", node_str)) {
    sid <- as.numeric(sub("M_", "", node_str))
    row <- stations[stations$stn_id == sid, ]
    if(nrow(row) > 0) {
      if("metrostationname" %in% names(row)) return(paste0(row$metrostationname, " (Metro)"))
      if("station_name" %in% names(row)) return(paste0(row$station_name, " (Metro)"))
    }
    return(paste("Station", sid))
  } else if (grepl("^B_", node_str)) {
    bid <- as.numeric(sub("B_", "", node_str))
    row <- stops[stops$bus_stop_id == bid, ]
    if(nrow(row) > 0) {
      if("stop_name" %in% names(row) && !is.na(row$stop_name) && row$stop_name != "") return(paste0(row$stop_name, " (Bus)"))
      return(paste("Stop", row$stop_code))
    }
    return(paste("BusNode", bid))
  }
  return(node_str)
}

# ------------------------------------------------------------------------------
# 3. LOAD DATA & SELECT TRIP
# ------------------------------------------------------------------------------
if(!file.exists("travel_time_individual.rds")) stop("❌ Part 16 output not found.")
df_part16 <- readRDS("travel_time_individual.rds")

if(!exists("results_df")) stop("Load 'sample_test_results.rds'")
if(!exists("g_multimodal")) stop("Load 'g_multimodal_sample.rds'")
if(!exists("stations_proj") || !exists("bus_stops_proj")) stop("Load projections.")
if(!exists("clinic_targets")) stop("Load 'clinic_targets'.")

target_id <- "pt2369"
scenario  <- "nearest_pub"

target_trip <- results_df %>% filter(rp_id == target_id, dest_type == scenario) %>% head(1)
if(nrow(target_trip) == 0) stop("Trip pt2369 not found.")

# ------------------------------------------------------------------------------
# 4. RECONSTRUCT PATH
# ------------------------------------------------------------------------------
target_meta <- clinic_targets %>% filter(id == target_id, dest_type == scenario)
clinic_ref <- target_meta$dest_id_geo
clinic_id  <- as.numeric(gsub("^(priv_|pub_)", "", clinic_ref))

rp_pt <- rp_sample %>% filter(id == target_id)
clinic_pt <- if(grepl("priv", scenario)) clinics_private %>% filter(id == clinic_id) else clinics_public %>% filter(id == clinic_id)

path_type <- target_trip$multi_path_type

if(grepl("^Metro", path_type)) {
  s_idx <- st_nearest_feature(rp_pt, stations_proj)
  s_node <- paste0("M_", stations_proj$stn_id[s_idx])
} else {
  s_idx <- st_nearest_feature(rp_pt, bus_stops_proj)
  s_node <- paste0("B_", bus_stops_proj$bus_stop_id[s_idx])
}

if(grepl("Metro$", path_type)) {
  e_idx <- st_nearest_feature(clinic_pt, stations_proj)
  e_node <- paste0("M_", stations_proj$stn_id[e_idx])
} else {
  e_idx <- st_nearest_feature(clinic_pt, bus_stops_proj)
  e_node <- paste0("B_", bus_stops_proj$bus_stop_id[e_idx])
}

# ------------------------------------------------------------------------------
# 5. PRINT FULL ITINERARY (L1 -> L2 -> L3)
# ------------------------------------------------------------------------------
message("\n================================================================================")
message(sprintf(" DOOR-TO-DOOR ITINERARY: %s -> %s", target_id, clinic_ref))
message("================================================================================")
message(sprintf("%-4s | %-12s | %-25s -> %-25s | %-7s | %-9s | %s", 
                "Step", "Mode", "From", "To", "Dist(m)", "Speed", "Time"))
message("--------------------------------------------------------------------------------")

step_counter <- 1
total_accumulated_time <- 0

# --- L1: ACCESS WALK ---
d_l1 <- as.numeric(target_trip$multi_L1_m)
t_l1 <- (d_l1 / 1000 / SPEED_WALK_KMH) * 60
start_label <- get_node_label(s_node, stations_proj, bus_stops_proj)

message(sprintf("%02d   | %-12s | %-25s -> %-25s | %4.0f m  | %4.1f km/h | %5.2f min", 
                step_counter, "WALK (Acc)", "Origin", substr(start_label, 1, 25), d_l1, 4.0, t_l1))

step_counter <- step_counter + 1
total_accumulated_time <- total_accumulated_time + t_l1

# --- L2: TRANSIT NETWORK ---
path_obj <- shortest_paths(g_multimodal, s_node, e_node, weights=E(g_multimodal)$weight, output="both")
edge_seq <- path_obj$epath[[1]]
node_seq <- path_obj$vpath[[1]]

for(i in seq_along(edge_seq)) {
  e_idx <- edge_seq[i]
  edge_data <- E(g_multimodal)[e_idx]
  
  from_id <- names(node_seq[i])
  to_id   <- names(node_seq[i+1])
  
  from_label <- get_node_label(from_id, stations_proj, bus_stops_proj)
  to_label   <- get_node_label(to_id, stations_proj, bus_stops_proj)
  
  dist <- edge_data$real_distance
  type <- edge_data$edge_type
  rt   <- as.character(edge_data$route_code)
  
  step_speed <- 0
  step_time  <- 0
  mode_label <- ""
  
  if(type == "metro_line") {
    step_time  <- calc_segment_time_min(dist)
    step_speed <- (dist / 1000) / (step_time / 60)
    mode_label <- paste("METRO", rt)
  } else if (type == "bus_route") {
    if(!is.na(rt) && rt %in% c("11", "12", "13")) {
      step_speed <- get_brt_speed(TEST_SPEED_KMH)
      mode_label <- paste("BRT", rt)
    } else {
      step_speed <- TEST_SPEED_KMH
      mode_label <- paste("BUS", rt)
    }
    step_time <- (dist / 1000 / step_speed) * 60
  } else {
    step_speed <- SPEED_WALK_KMH
    step_time  <- (dist / 1000 / step_speed) * 60
    mode_label <- "WALK (Tr)"
  }
  
  message(sprintf("%02d   | %-12s | %-25s -> %-25s | %4.0f m  | %4.1f km/h | %5.2f min", 
                  step_counter, mode_label, substr(from_label, 1, 25), substr(to_label, 1, 25), dist, step_speed, step_time))
  
  step_counter <- step_counter + 1
  total_accumulated_time <- total_accumulated_time + step_time
}

# --- L3: EGRESS WALK ---
d_l3 <- as.numeric(target_trip$multi_L3_m)
t_l3 <- (d_l3 / 1000 / SPEED_WALK_KMH) * 60
end_label <- get_node_label(e_node, stations_proj, bus_stops_proj)

message(sprintf("%02d   | %-12s | %-25s -> %-25s | %4.0f m  | %4.1f km/h | %5.2f min", 
                step_counter, "WALK (Egr)", substr(end_label, 1, 25), "Destination", d_l3, 4.0, t_l3))

total_accumulated_time <- total_accumulated_time + t_l3

# --- DWELL PENALTY ---
t_dwell <- as.numeric(target_trip$mm_tot_stops) * 0.5
total_accumulated_time <- total_accumulated_time + t_dwell

message("--------------------------------------------------------------------------------")
message(sprintf("   + Dwell Time (Penalty):                                           %5.2f min", t_dwell))
message("--------------------------------------------------------------------------------")
message(sprintf("GRAND TOTAL TIME:                                                 %5.2f min", total_accumulated_time))

# ------------------------------------------------------------------------------
# 6. FINAL VERIFICATION
# ------------------------------------------------------------------------------
valid_trips <- results_df %>% filter(dest_type == scenario, !is.na(multi_total_m))
trip_rank <- which(valid_trips$rp_id == target_id)
p16_subset <- df_part16 %>% 
  filter(Type == "Public", Target == "Nearest", Speed == TEST_SPEED_KMH, Mode == "Walk-initiated Multimodal")
p16_val <- p16_subset$Time_Deterministic[trip_rank]

message("\n[AUDIT COMPARISON]")
message(sprintf("Calculated Ledger:     %8.4f min", total_accumulated_time))
message(sprintf("Part 16 Simulation:    %8.4f min", p16_val))

if(abs(total_accumulated_time - p16_val) < 0.001) message("✅ PERFECT MATCH CONFIRMED") else message("❌ MISMATCH")




# ==============================================================================
# DIAGNOSTIC: RANDOM TRIP AUDIT (Full Door-to-Door Ledger)
# Purpose: Pick a RANDOM trip and verify its physics against Part 16
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(igraph)
})

# 1. SETUP PHYSICS CONSTANTS (Must match Part 16 EXACTLY)
# ------------------------------------------------------------------------------
TEST_SPEED_KMH   <- 40   # Scenario: Moderate Traffic
SPEED_WALK_KMH   <- 4
METRO_ACCEL      <- 1.2
METRO_MAX_KMH    <- 80
BRT_GAP_FACTOR   <- 0.5
BRT_CAP          <- 80

# Kinematics Helpers
metro_max_ms <- METRO_MAX_KMH / 3.6
time_to_max  <- metro_max_ms / METRO_ACCEL
dist_to_max_m <- 0.5 * METRO_ACCEL * time_to_max^2
min_cruise_dist_m <- 2 * dist_to_max_m

calc_segment_time_min <- function(seg_m) {
  if(is.na(seg_m) || seg_m <= 0) return(0)
  seg_km <- seg_m / 1000
  if (seg_km >= min_cruise_dist_m/1000) {
    accel_time_hr <- (time_to_max) / 3600
    cruise_dist_km <- seg_km - (2 * dist_to_max_m/1000)
    cruise_time_hr <- cruise_dist_km / METRO_MAX_KMH
    total_time_hr <- (2 * accel_time_hr) + cruise_time_hr
  } else {
    half_dist_m <- (seg_km * 1000) / 2
    time_each_phase_s <- sqrt(2 * half_dist_m / METRO_ACCEL)
    total_time_hr <- (2 * time_each_phase_s) / 3600
  }
  return(total_time_hr * 60)
}

get_brt_speed <- function(traffic_speed) {
  speed <- traffic_speed + (BRT_GAP_FACTOR * (BRT_CAP - traffic_speed))
  min(speed, BRT_CAP)
}

get_node_label <- function(node_str, stations, stops) {
  if(is.na(node_str)) return("Unknown")
  if(grepl("^M_", node_str)) {
    sid <- as.numeric(sub("M_", "", node_str))
    row <- stations[stations$stn_id == sid, ]
    if(nrow(row) > 0) {
      if("metrostationname" %in% names(row)) return(paste0(row$metrostationname, " (Metro)"))
      if("station_name" %in% names(row)) return(paste0(row$station_name, " (Metro)"))
    }
    return(paste("Station", sid))
  } else if (grepl("^B_", node_str)) {
    bid <- as.numeric(sub("B_", "", node_str))
    row <- stops[stops$bus_stop_id == bid, ]
    if(nrow(row) > 0) {
      if("stop_name" %in% names(row) && !is.na(row$stop_name) && row$stop_name != "") return(paste0(row$stop_name, " (Bus)"))
      return(paste("Stop", row$stop_code))
    }
    return(paste("BusNode", bid))
  }
  return(node_str)
}

# 2. LOAD DATA
# ------------------------------------------------------------------------------
if(!file.exists("travel_time_individual.rds")) stop("❌ Part 16 output not found.")
df_part16 <- readRDS("travel_time_individual.rds")

if(!exists("results_df")) stop("Load 'sample_test_results.rds'")
if(!exists("g_multimodal")) stop("Load 'g_multimodal_sample.rds'")
if(!exists("stations_proj") || !exists("bus_stops_proj")) stop("Load projections.")
if(!exists("clinic_targets")) stop("Load 'clinic_targets'.")

# 3. SELECT RANDOM VALID TRIP
# ------------------------------------------------------------------------------
# Filter for trips that actually used transit (not NA)
# R1-5 fix (2026-06-28): results_df now also carries the new farthest/random
# anchors, which are absent from clinic_targets (originals only) -> the metadata
# lookup at ~L10842 would stop() when the random draw landed on a new anchor
# (it did, halting the run AFTER all deliverables were written). This audit is a
# non-reproducible (time-seeded) path-visualisation diagnostic, so restricting it
# to the original clinic anchors is harmless and keeps the run exiting cleanly.
.audit_anchors <- c("nearest_priv","median_priv","nearest_pub","median_pub")
candidates <- results_df %>% filter(!is.na(multi_total_m), dest_type %in% .audit_anchors)

# --- RANDOM SAMPLER ---
# You can prioritize specific types if you want, e.g., filter(multi_path_type == "Metro-Bus")
# For now, we pick completely randomly from all successful routes.
set.seed(Sys.time()) # Ensure randomness every run
target_trip <- candidates[sample(nrow(candidates), 1), ]

target_id <- target_trip$rp_id
scenario  <- target_trip$dest_type

message(paste(rep("=", 80), collapse = ""))
message(sprintf(" 🎲 RANDOM AUDIT SELECTED: %s", target_id))
message(sprintf("    Scenario: %s", scenario))
message(sprintf("    Path Type: %s", target_trip$multi_path_type))
message(paste(rep("=", 80), collapse = ""))

# 4. RECONSTRUCT PATH
# ------------------------------------------------------------------------------
target_meta <- clinic_targets %>% filter(id == target_id, dest_type == scenario)
if(nrow(target_meta) == 0) stop("Metadata not found for this scenario.")

clinic_ref <- target_meta$dest_id_geo
clinic_id  <- as.numeric(gsub("^(priv_|pub_)", "", clinic_ref))

rp_pt <- rp_sample %>% filter(id == target_id)
clinic_pt <- if(grepl("priv", scenario)) clinics_private %>% filter(id == clinic_id) else clinics_public %>% filter(id == clinic_id)

path_type <- target_trip$multi_path_type

# Find Start Node
if(grepl("^Metro", path_type)) {
  s_idx <- st_nearest_feature(rp_pt, stations_proj)
  s_node <- paste0("M_", stations_proj$stn_id[s_idx])
} else {
  s_idx <- st_nearest_feature(rp_pt, bus_stops_proj)
  s_node <- paste0("B_", bus_stops_proj$bus_stop_id[s_idx])
}

# Find End Node
if(grepl("Metro$", path_type)) {
  e_idx <- st_nearest_feature(clinic_pt, stations_proj)
  e_node <- paste0("M_", stations_proj$stn_id[e_idx])
} else {
  e_idx <- st_nearest_feature(clinic_pt, bus_stops_proj)
  e_node <- paste0("B_", bus_stops_proj$bus_stop_id[e_idx])
}

# 5. PRINT ITINERARY
# ------------------------------------------------------------------------------
message(sprintf("\n%-4s | %-12s | %-25s -> %-25s | %-7s | %-9s | %s", 
                "Step", "Mode", "From", "To", "Dist(m)", "Speed", "Time"))
message("----------------------------------------------------------------------------------------------------")

step_counter <- 1
total_accumulated_time <- 0

# --- L1: ACCESS WALK ---
d_l1 <- as.numeric(target_trip$multi_L1_m)
t_l1 <- (d_l1 / 1000 / SPEED_WALK_KMH) * 60
start_label <- get_node_label(s_node, stations_proj, bus_stops_proj)

message(sprintf("%02d   | %-12s | %-25s -> %-25s | %4.0f m  | %4.1f km/h | %5.2f min", 
                step_counter, "WALK (Acc)", "Origin", substr(start_label, 1, 25), d_l1, 4.0, t_l1))

step_counter <- step_counter + 1
total_accumulated_time <- total_accumulated_time + t_l1

# --- L2: TRANSIT NETWORK ---
# Handle case where Start Node == End Node (Rare, but possible if clinic is next to origin stop)
if(s_node != e_node) {
  path_obj <- shortest_paths(g_multimodal, s_node, e_node, weights=E(g_multimodal)$weight, output="both")
  edge_seq <- path_obj$epath[[1]]
  node_seq <- path_obj$vpath[[1]]
  
  if(length(edge_seq) > 0) {
    for(i in seq_along(edge_seq)) {
      e_idx <- edge_seq[i]
      edge_data <- E(g_multimodal)[e_idx]
      
      from_id <- names(node_seq[i])
      to_id   <- names(node_seq[i+1])
      
      from_label <- get_node_label(from_id, stations_proj, bus_stops_proj)
      to_label   <- get_node_label(to_id, stations_proj, bus_stops_proj)
      
      dist <- edge_data$real_distance
      type <- edge_data$edge_type
      rt   <- as.character(edge_data$route_code)
      
      step_speed <- 0
      step_time  <- 0
      mode_label <- ""
      
      if(type == "metro_line") {
        step_time  <- calc_segment_time_min(dist)
        step_speed <- (dist / 1000) / (step_time / 60)
        mode_label <- paste("METRO", rt)
      } else if (type == "bus_route") {
        if(!is.na(rt) && rt %in% c("11", "12", "13")) {
          step_speed <- get_brt_speed(TEST_SPEED_KMH)
          mode_label <- paste("BRT", rt)
        } else {
          step_speed <- TEST_SPEED_KMH
          mode_label <- paste("BUS", rt)
        }
        step_time <- (dist / 1000 / step_speed) * 60
      } else {
        step_speed <- SPEED_WALK_KMH
        step_time  <- (dist / 1000 / step_speed) * 60
        mode_label <- "WALK (Tr)"
      }
      
      message(sprintf("%02d   | %-12s | %-25s -> %-25s | %4.0f m  | %4.1f km/h | %5.2f min", 
                      step_counter, mode_label, substr(from_label, 1, 25), substr(to_label, 1, 25), dist, step_speed, step_time))
      
      step_counter <- step_counter + 1
      total_accumulated_time <- total_accumulated_time + step_time
    }
  }
} else {
  message("     [Info] Origin Stop is same as Destination Stop (No L2 movement)")
}

# --- L3: EGRESS WALK ---
d_l3 <- as.numeric(target_trip$multi_L3_m)
t_l3 <- (d_l3 / 1000 / SPEED_WALK_KMH) * 60
end_label <- get_node_label(e_node, stations_proj, bus_stops_proj)

message(sprintf("%02d   | %-12s | %-25s -> %-25s | %4.0f m  | %4.1f km/h | %5.2f min", 
                step_counter, "WALK (Egr)", substr(end_label, 1, 25), "Destination", d_l3, 4.0, t_l3))

total_accumulated_time <- total_accumulated_time + t_l3

# --- DWELL PENALTY ---
t_dwell <- as.numeric(target_trip$mm_tot_stops) * 0.5
total_accumulated_time <- total_accumulated_time + t_dwell

message("----------------------------------------------------------------------------------------------------")
message(sprintf("   + Dwell Time (Penalty):                                                               %5.2f min", t_dwell))
message("----------------------------------------------------------------------------------------------------")
message(sprintf("GRAND TOTAL TIME:                                                                     %5.2f min", total_accumulated_time))

# 6. FINAL VERIFICATION
# ------------------------------------------------------------------------------
# Map Scenario to Part 16 "Type" and "Target"
p16_type <- if(grepl("priv", scenario)) "Private" else "Public"
p16_target <- if(grepl("nearest", scenario)) "Nearest" else "Specific"

# Get Relative Rank
valid_trips_in_scenario <- results_df %>% filter(dest_type == scenario, !is.na(multi_total_m))
trip_rank <- which(valid_trips_in_scenario$rp_id == target_id)

p16_subset <- df_part16 %>% 
  filter(Type == p16_type, Target == p16_target, Speed == TEST_SPEED_KMH, Mode == "Walk-initiated Multimodal")

# Safety Check: Rank within bounds
if(trip_rank <= nrow(p16_subset)) {
  p16_val <- p16_subset$Time_Deterministic[trip_rank]
  
  message("\n[AUDIT RESULT]")
  message(sprintf("Calculated Ledger:     %8.4f min", total_accumulated_time))
  message(sprintf("Part 16 Simulation:    %8.4f min", p16_val))
  
  if(abs(total_accumulated_time - p16_val) < 0.001) {
    message("✅ PERFECT MATCH CONFIRMED") 
  } else {
    message("❌ MISMATCH")
    message(sprintf("   Diff: %.4f min", abs(total_accumulated_time - p16_val)))
  }
} else {
  message("\n⚠️ Could not find matching row in Simulation Output (Rank mismatch).")
}


# ==============================================================================
# DIAGNOSTIC: RANDOM BUS/BRT AUDIT
# Purpose: Find a trip using BUS/BRT and verify the physics
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(igraph)
})

# 1. SETUP PHYSICS CONSTANTS (Must match Part 16 EXACTLY)
# ------------------------------------------------------------------------------
TEST_SPEED_KMH   <- 10   # Scenario: Moderate Traffic
SPEED_WALK_KMH   <- 4
METRO_ACCEL      <- 1.2
METRO_MAX_KMH    <- 80
BRT_GAP_FACTOR   <- 0.5
BRT_CAP          <- 80

# Kinematics Helpers
metro_max_ms <- METRO_MAX_KMH / 3.6
time_to_max  <- metro_max_ms / METRO_ACCEL
dist_to_max_m <- 0.5 * METRO_ACCEL * time_to_max^2
min_cruise_dist_m <- 2 * dist_to_max_m

calc_segment_time_min <- function(seg_m) {
  if(is.na(seg_m) || seg_m <= 0) return(0)
  seg_km <- seg_m / 1000
  if (seg_km >= min_cruise_dist_m/1000) {
    accel_time_hr <- (time_to_max) / 3600
    cruise_dist_km <- seg_km - (2 * dist_to_max_m/1000)
    cruise_time_hr <- cruise_dist_km / METRO_MAX_KMH
    total_time_hr <- (2 * accel_time_hr) + cruise_time_hr
  } else {
    half_dist_m <- (seg_km * 1000) / 2
    time_each_phase_s <- sqrt(2 * half_dist_m / METRO_ACCEL)
    total_time_hr <- (2 * time_each_phase_s) / 3600
  }
  return(total_time_hr * 60)
}

get_brt_speed <- function(traffic_speed) {
  # Formula: Traffic + 0.5 * (80 - Traffic)
  speed <- traffic_speed + (BRT_GAP_FACTOR * (BRT_CAP - traffic_speed))
  min(speed, BRT_CAP)
}

get_node_label <- function(node_str, stations, stops) {
  if(is.na(node_str)) return("Unknown")
  if(grepl("^M_", node_str)) {
    sid <- as.numeric(sub("M_", "", node_str))
    row <- stations[stations$stn_id == sid, ]
    if(nrow(row) > 0) {
      if("metrostationname" %in% names(row)) return(paste0(row$metrostationname, " (Metro)"))
      if("station_name" %in% names(row)) return(paste0(row$station_name, " (Metro)"))
    }
    return(paste("Station", sid))
  } else if (grepl("^B_", node_str)) {
    bid <- as.numeric(sub("B_", "", node_str))
    row <- stops[stops$bus_stop_id == bid, ]
    if(nrow(row) > 0) {
      if("stop_name" %in% names(row) && !is.na(row$stop_name) && row$stop_name != "") return(paste0(row$stop_name, " (Bus)"))
      return(paste("Stop", row$stop_code))
    }
    return(paste("BusNode", bid))
  }
  return(node_str)
}

# 2. LOAD DATA
# ------------------------------------------------------------------------------
if(!file.exists("travel_time_individual.rds")) stop("❌ Part 16 output not found.")
df_part16 <- readRDS("travel_time_individual.rds")

if(!exists("results_df")) stop("Load 'sample_test_results.rds'")
if(!exists("g_multimodal")) stop("Load 'g_multimodal_sample.rds'")
if(!exists("stations_proj") || !exists("bus_stops_proj")) stop("Load projections.")
if(!exists("clinic_targets")) stop("Load 'clinic_targets'.")

# 3. FILTER FOR BUS/BRT TRIPS
# ------------------------------------------------------------------------------
# We filter for trips where EITHER Standard Bus OR BRT distance > 0
candidates <- results_df %>% 
  filter(!is.na(multi_total_m)) %>%
  filter(as.numeric(mm_dist_brt_m) > 0 | as.numeric(mm_dist_std_m) > 0)

if(nrow(candidates) == 0) stop("No bus trips found in the sample results.")

# Pick One Randomly
set.seed(Sys.time()) 
target_trip <- candidates[sample(nrow(candidates), 1), ]

target_id <- target_trip$rp_id
scenario  <- target_trip$dest_type

message(paste(rep("=", 80), collapse = ""))
message(sprintf(" 🚌 RANDOM BUS/BRT AUDIT SELECTED: %s", target_id))
message(sprintf("    Scenario: %s", scenario))
message(sprintf("    Path Type: %s", target_trip$multi_path_type))
message(sprintf("    BRT Distance: %.0f m", as.numeric(target_trip$mm_dist_brt_m)))
message(sprintf("    Std Bus Dist: %.0f m", as.numeric(target_trip$mm_dist_std_m)))
message(paste(rep("=", 80), collapse = ""))

# 4. RECONSTRUCT PATH
# ------------------------------------------------------------------------------
target_meta <- clinic_targets %>% filter(id == target_id, dest_type == scenario)
clinic_ref <- target_meta$dest_id_geo
clinic_id  <- as.numeric(gsub("^(priv_|pub_)", "", clinic_ref))

rp_pt <- rp_sample %>% filter(id == target_id)
clinic_pt <- if(grepl("priv", scenario)) clinics_private %>% filter(id == clinic_id) else clinics_public %>% filter(id == clinic_id)

path_type <- target_trip$multi_path_type

if(grepl("^Metro", path_type)) {
  s_idx <- st_nearest_feature(rp_pt, stations_proj)
  s_node <- paste0("M_", stations_proj$stn_id[s_idx])
} else {
  s_idx <- st_nearest_feature(rp_pt, bus_stops_proj)
  s_node <- paste0("B_", bus_stops_proj$bus_stop_id[s_idx])
}

if(grepl("Metro$", path_type)) {
  e_idx <- st_nearest_feature(clinic_pt, stations_proj)
  e_node <- paste0("M_", stations_proj$stn_id[e_idx])
} else {
  e_idx <- st_nearest_feature(clinic_pt, bus_stops_proj)
  e_node <- paste0("B_", bus_stops_proj$bus_stop_id[e_idx])
}

# 5. PRINT ITINERARY
# ------------------------------------------------------------------------------
message(sprintf("\n%-4s | %-12s | %-25s -> %-25s | %-7s | %-9s | %s", 
                "Step", "Mode", "From", "To", "Dist(m)", "Speed", "Time"))
message("----------------------------------------------------------------------------------------------------")

step_counter <- 1
total_accumulated_time <- 0

# --- L1: ACCESS WALK ---
d_l1 <- as.numeric(target_trip$multi_L1_m)
t_l1 <- (d_l1 / 1000 / SPEED_WALK_KMH) * 60
start_label <- get_node_label(s_node, stations_proj, bus_stops_proj)

message(sprintf("%02d   | %-12s | %-25s -> %-25s | %4.0f m  | %4.1f km/h | %5.2f min", 
                step_counter, "WALK (Acc)", "Origin", substr(start_label, 1, 25), d_l1, 4.0, t_l1))

step_counter <- step_counter + 1
total_accumulated_time <- total_accumulated_time + t_l1

# --- L2: TRANSIT NETWORK ---
if(s_node != e_node) {
  path_obj <- shortest_paths(g_multimodal, s_node, e_node, weights=E(g_multimodal)$weight, output="both")
  edge_seq <- path_obj$epath[[1]]
  node_seq <- path_obj$vpath[[1]]
  
  if(length(edge_seq) > 0) {
    for(i in seq_along(edge_seq)) {
      e_idx <- edge_seq[i]
      edge_data <- E(g_multimodal)[e_idx]
      
      from_id <- names(node_seq[i])
      to_id   <- names(node_seq[i+1])
      
      from_label <- get_node_label(from_id, stations_proj, bus_stops_proj)
      to_label   <- get_node_label(to_id, stations_proj, bus_stops_proj)
      
      dist <- edge_data$real_distance
      type <- edge_data$edge_type
      rt   <- as.character(edge_data$route_code)
      
      step_speed <- 0
      step_time  <- 0
      mode_label <- ""
      
      if(type == "metro_line") {
        step_time  <- calc_segment_time_min(dist)
        step_speed <- (dist / 1000) / (step_time / 60)
        mode_label <- paste("METRO", rt)
      } else if (type == "bus_route") {
        # --- BUS PHYSICS CHECK ---
        if(!is.na(rt) && rt %in% c("11", "12", "13")) {
          step_speed <- get_brt_speed(TEST_SPEED_KMH)
          mode_label <- paste("BRT", rt)
        } else {
          step_speed <- TEST_SPEED_KMH
          mode_label <- paste("BUS", rt)
        }
        step_time <- (dist / 1000 / step_speed) * 60
      } else {
        step_speed <- SPEED_WALK_KMH
        step_time  <- (dist / 1000 / step_speed) * 60
        mode_label <- "WALK (Tr)"
      }
      
      message(sprintf("%02d   | %-12s | %-25s -> %-25s | %4.0f m  | %4.1f km/h | %5.2f min", 
                      step_counter, mode_label, substr(from_label, 1, 25), substr(to_label, 1, 25), dist, step_speed, step_time))
      
      step_counter <- step_counter + 1
      total_accumulated_time <- total_accumulated_time + step_time
    }
  }
} else {
  message("     [Info] Origin Stop is same as Destination Stop (No L2 movement)")
}

# --- L3: EGRESS WALK ---
d_l3 <- as.numeric(target_trip$multi_L3_m)
t_l3 <- (d_l3 / 1000 / SPEED_WALK_KMH) * 60
end_label <- get_node_label(e_node, stations_proj, bus_stops_proj)

message(sprintf("%02d   | %-12s | %-25s -> %-25s | %4.0f m  | %4.1f km/h | %5.2f min", 
                step_counter, "WALK (Egr)", substr(end_label, 1, 25), "Destination", d_l3, 4.0, t_l3))

total_accumulated_time <- total_accumulated_time + t_l3

# --- DWELL PENALTY ---
t_dwell <- as.numeric(target_trip$mm_tot_stops) * 0.5
total_accumulated_time <- total_accumulated_time + t_dwell

message("----------------------------------------------------------------------------------------------------")
message(sprintf("   + Dwell Time (Penalty):                                                               %5.2f min", t_dwell))
message("----------------------------------------------------------------------------------------------------")
message(sprintf("GRAND TOTAL TIME:                                                                     %5.2f min", total_accumulated_time))

# 6. FINAL VERIFICATION
# ------------------------------------------------------------------------------
# Map Scenario to Part 16 "Type" and "Target"
p16_type <- if(grepl("priv", scenario)) "Private" else "Public"
p16_target <- if(grepl("nearest", scenario)) "Nearest" else "Specific"

# Get Relative Rank
valid_trips_in_scenario <- results_df %>% filter(dest_type == scenario, !is.na(multi_total_m))
trip_rank <- which(valid_trips_in_scenario$rp_id == target_id)

p16_subset <- df_part16 %>% 
  filter(Type == p16_type, Target == p16_target, Speed == TEST_SPEED_KMH, Mode == "Walk-initiated Multimodal")

if(trip_rank <= nrow(p16_subset)) {
  p16_val <- p16_subset$Time_Deterministic[trip_rank]
  
  message("\n[AUDIT RESULT]")
  message(sprintf("Calculated Ledger:     %8.4f min", total_accumulated_time))
  message(sprintf("Part 16 Simulation:    %8.4f min", p16_val))
  
  if(abs(total_accumulated_time - p16_val) < 0.001) {
    message("✅ PERFECT MATCH CONFIRMED") 
  } else {
    message("❌ MISMATCH")
    message(sprintf("   Diff: %.4f min", abs(total_accumulated_time - p16_val)))
  }
} else {
  message("\n⚠️ Could not find matching row in Simulation Output (Rank mismatch).")
}
