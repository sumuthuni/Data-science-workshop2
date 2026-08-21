# Excercise_Estuary fish

# Load packages
library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)
library(here)
library(kable)


#Make directory
dir.create("data", showWarnings = FALSE)
dir.create("code", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)
dir.create("docs", showWarnings = FALSE)

# Phase1

catch_file <- here("data", "estuary_catch_log (1).xlsx")

# Get all sheet names
sheet_names <- excel_sheets(catch_file)

# Read every sheet and combine them into one data frame
catch_raw <- sheet_names |>
  map_dfr(~ read_excel(catch_file, sheet = .x))

# Check
glimpse(catch_raw)


#
# 2. Clean site and species names
#

catch_clean <- catch_raw |>
  mutate(
    # Convert site names to lowercase and underscores
    site = site |>
      str_trim() |>
      str_to_lower() |>
      str_replace_all("\\s+", "_"),
    
    # Convert species names to lowercase and underscores
    species = species |>
      str_trim() |>
      str_to_lower() |>
      str_replace_all("\\s+", "_"),
    
    # Make sure date is a proper Date
    date = as.Date(date)
  )

# Check cleaned names
unique(catch_clean$site)
unique(catch_clean$species)


# 
# 3. Read and clean estuary metadata
# 

metadata <- read_csv(
  here("data", "estuary_metadata (1).csv")
)

metadata_clean <- metadata |>
  mutate(
    site_name = site_name |>
      str_trim() |>
      str_to_lower() |>
      str_replace_all("\\s+", "_"),
    
    zone = str_to_title(str_trim(zone))
  )

print(metadata_clean)


# 
# 4. Read and clean species dictionary
# 

species_dictionary <- read_csv(
  here("data", "species_dictionary (1).csv")
)

species_dictionary_clean <- species_dictionary |>
  mutate(
    common_name = common_name |>
      str_trim() |>
      str_to_lower() |>
      str_replace_all("\\s+", "_"),
    
    scientific_name = str_trim(scientific_name)
  )

print(species_dictionary_clean)


# ------------------------------------------------------------
# 5. Read and clean sonde data
# ------------------------------------------------------------

sonde_raw <- read_csv(
  here("data", "estuary_sonde_data (1).csv")
)

sonde_clean <- sonde_raw |>
  mutate(
    # Standardise site names
    site = site |>
      str_trim() |>
      str_to_lower() |>
      str_replace_all("\\s+", "_"),
    
    # Convert messy character timestamps into Date-Time
    timestamp = dmy_hm(timestamp),
    
    # Convert hardware error -999 to true NA
    turbidity = na_if(turbidity, -999.0)
  )

# Confirm that -999 values are gone
sum(sonde_clean$turbidity == -999, na.rm = TRUE)


# ============================================================
# PHASE 2: RELATIONAL ARCHITECTURE
# ============================================================

# ------------------------------------------------------------
# 6. Summarise sonde data to daily averages
# ------------------------------------------------------------

daily_water <- sonde_clean |>
  mutate(
    date = as.Date(floor_date(timestamp, unit = "day"))
  ) |>
  group_by(site, date) |>
  summarise(
    mean_temperature = mean(temperature, na.rm = TRUE),
    mean_salinity = mean(salinity, na.rm = TRUE),
    mean_turbidity = mean(turbidity, na.rm = TRUE),
    .groups = "drop"
  )

print(daily_water)


# ------------------------------------------------------------
# 7. Replace common species names with scientific names
# ------------------------------------------------------------

catch_taxonomy <- catch_clean |>
  left_join(
    species_dictionary_clean,
    by = c("species" = "common_name")
  )

# Check that taxonomy joined correctly
catch_taxonomy |>
  distinct(species, scientific_name)


# ------------------------------------------------------------
# 8. Join catch data with spatial metadata
# ------------------------------------------------------------

catch_spatial <- catch_taxonomy |>
  left_join(
    metadata_clean,
    by = c("site" = "site_name")
  )


# ------------------------------------------------------------
# 9. Add daily environmental data
# ------------------------------------------------------------

master_prezero <- catch_spatial |>
  left_join(
    daily_water,
    by = c("site", "date")
  )

glimpse(master_prezero)


# ============================================================
# PHASE 3: ZERO-CATCH FRAMEWORK
# ============================================================

# Create every combination of:
# sampling date × site × species
#
# Missing catch records mean zero fish caught.

master_complete <- master_prezero |>
  select(
    site,
    date,
    scientific_name,
    count,
    lat,
    lon,
    zone,
    mean_temperature,
    mean_salinity,
    mean_turbidity
  ) |>
  complete(
    site,
    date,
    scientific_name
  ) |>
  mutate(
    count = coalesce(count, 0)
  )


# ------------------------------------------------------------
# Reattach site-level and daily environmental information
# after complete()
# ------------------------------------------------------------

site_info <- metadata_clean |>
  rename(site = site_name)

master_dataset <- master_complete |>
  select(site, date, scientific_name, count) |>
  left_join(
    site_info,
    by = "site"
  ) |>
  left_join(
    daily_water,
    by = c("site", "date")
  ) |>
  arrange(date, site, scientific_name)


# Check final master dataset
glimpse(master_dataset)

# Check no legacy -999 errors remain
any(master_dataset == -999, na.rm = TRUE)

# Check scientific names
unique(master_dataset$scientific_name)


# ============================================================
# SAVE CLEAN MASTER DATASET
# ============================================================

write_csv(
  master_dataset,
  here("output", "clean_estuary_master_dataset.csv")
)


# ============================================================
# PHASE 4: STATISTICAL EXTRACTION
# ============================================================

# Calculate mean, standard deviation and standard error
# for fish count and salinity for each species and zone.

summary_table <- master_dataset |>
  group_by(scientific_name, zone) |>
  summarise(
    n = n(),
    
    mean_fish_count = mean(count, na.rm = TRUE),
    sd_fish_count = sd(count, na.rm = TRUE),
    se_fish_count = sd_fish_count / sqrt(n),
    
    mean_salinity = mean(mean_salinity, na.rm = TRUE),
    sd_salinity = sd(mean_salinity, na.rm = TRUE),
    se_salinity = sd_salinity / sqrt(n),
    
    .groups = "drop"
  )

print(summary_table)


# Save summary table
write_csv(
  summary_table,
  here("output", "estuary_summary_table.csv")
)


# Optional publication-ready table in R Markdown/Quarto
knitr::kable(
  summary_table,
  digits = 2,
  caption = "Fish abundance and salinity summary by species and estuary zone"
)


# ============================================================
# PHASE 5: VISUAL COMMUNICATION
# ============================================================

# Set estuary zones in spatial order
master_dataset <- master_dataset |>
  mutate(
    zone = factor(
      zone,
      levels = c(
        "Upstream",
        "Middle",
        "Downstream",
        "Marine"
      )
    )
  )


# ------------------------------------------------------------
# Multi-panel plot:
# fish abundance along the salinity gradient
# ------------------------------------------------------------

estuary_plot <- ggplot(
  master_dataset,
  aes(
    x = mean_salinity,
    y = count,
    colour = zone
  )
) +
  geom_point(
    alpha = 0.65,
    size = 2
  ) +
  
  geom_smooth(
    method = "loess",
    se = TRUE,
    linewidth = 0.8
  ) +
  
  facet_wrap(
    ~ scientific_name,
    scales = "free_y"
  ) +
  
  labs(
    title = "Fish abundance along the estuarine salinity gradient",
    subtitle = "Daily fish catches across upstream, middle, downstream and marine zones",
    x = "Mean daily salinity",
    y = "Fish abundance (count)",
    colour = "Estuary zone",
    caption = "Fish catch, spatial metadata and sonde monitoring data"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = element_text(
      size = 12
    ),
    
    strip.text = element_text(
      face = "italic",
      size = 11
    ),
    
    legend.position = "bottom",
    
    panel.grid.minor = element_blank()
  )

estuary_plot


# Save publication-ready figure
ggsave(
  here("output", "fish_abundance_salinity_gradient.png"),
  plot = estuary_plot,
  width = 10,
  height = 7,
  dpi = 300
)