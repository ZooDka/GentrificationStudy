
#gain_df has 2012 and 2023 values only so you can compare start data to finish data
#acs_long_complete has the final dataset with all years in it in long format
#acs_clean has the data by year in wide format


library(tidycensus)
library(purrr)
library(stringr)
library(janitor)
library(tidyr)
library(ggplot2)
library(fixest)
library(tibble)
library(dplyr)
library(ggridges)
library(scales)
library(fixest)
library(ineq)
library(ranger)
library(vip)
library(tidyverse)
library(sf)
library(tigris)
library(ineq)
library(patchwork)
library(viridis)
library(ggrepel)



# load key from ~/.Renviron
readRenviron("~/.Renviron")
if(Sys.getenv("CENSUS_API_KEY") == "") stop("Set CENSUS_API_KEY in ~/.Renviron before running.")
#options(tigris_use_cache = TRUE)






#variables to pull (named vector)
vars <- c(
  # Income + Poverty
  median_income          = "B19013_001",
  poverty_total          = "B17001_001",
  poverty_below          = "B17001_002",
  income_dist_10k        = "B19001_002",
  income_dist_200k       = "B19001_017",

  # Housing costs
  median_gross_rent      = "B25064_001",
  median_home_value      = "B25077_001",

  # Rent burden
  rent_burden_30_35      = "B25070_007",
  rent_burden_35_40      = "B25070_008",
  rent_burden_40_50      = "B25070_009",
  rent_burden_50_plus    = "B25070_010",

  # Housing tenure
  owner_occupied         = "B25003_002",
  renter_occupied        = "B25003_003",

  # Overcrowding
  overcrowded            = "B25014_005",

  # Demographics
  total_population       = "B01003_001",
  median_age             = "B01002_001",
  white_pop              = "B02001_002",
  black_pop              = "B02001_003",
  asian_pop              = "B02001_005",
  hispanic_pop           = "B03002_012",

  # Education
  total_edu              = "B15003_001",
  edu_bachelors          = "B15003_022",
  edu_graduate           = "B15003_023",

  # Employment
  labor_total            = "B23025_001",
  labor_employed         = "B23025_003",
  labor_unemployed       = "B23025_005",

  # Occupation categories (professional indicator)
  prof_occ               = "C24010_004"
)

states <- c("NY","NJ","CA")   # change as needed
years  <- 2011:2023
geography <- "tract"











# safe wrapper to pull one state-year (wide)
pull_state_year <- function(state, year) {
  message("Pulling ", state, " - ", year)
  out <- tryCatch({
    get_acs(
      geography = geography,
      variables = vars,
      year = year,
      survey = "acs5",
      state = state,
      geometry = FALSE,
      output = "wide"
    ) |>
      mutate(year = as.integer(year), state = state)
  }, error = function(e){
    message("ERROR pulling ", state, " ", year, ": ", e$message)
    return(NULL)
  })
  out
}






# Pull all state-year combinations into acs_raw
acs_raw <- map_df(years, function(y) {
  map_df(states, ~pull_state_year(.x, y))
})







# Normalize column names and create canonical names
acs_norm <- acs_raw

names(acs_norm) <- names(acs_norm) |>

  str_replace("([Ee])$", "_e") |>
  str_replace("([Mm])$", "_m") |>
  tolower()


for(nm in names(vars)) {
  cand1 <- paste0(nm, "_e")
  cand2 <- paste0(vars[[nm]] |> tolower(), "_e")
  if(cand1 %in% names(acs_norm)) {
    # ok present already
  } else if(cand2 %in% names(acs_norm)) {
    # copy
    acs_norm[[cand1]] <- acs_norm[[cand2]]
  } else {
    # not present; leave NA column so later code can detect missing
    acs_norm[[cand1]] <- NA_real_
  }
  # Repeat for _m (margin of error)
  cand1m <- paste0(nm, "_m")
  cand2m <- paste0(vars[[nm]] |> tolower(), "_m")
  if(!(cand1m %in% names(acs_norm))) {
    if(cand2m %in% names(acs_norm)) acs_norm[[cand1m]] <- acs_norm[[cand2m]] else acs_norm[[cand1m]] <- NA_real_
  }
}

# Also standardize geoid/name columns: make sure geoid exists and is named geoid (uppercase/lowercase variations)
possible_geoid <- c("geoid","geo_id","id","tract","GEOID","tractce")
found_geoid <- intersect(tolower(names(acs_norm)), tolower(possible_geoid))
if(length(found_geoid) == 0) stop("No GEOID-like column found in acs_raw")
# pick the first match and copy to geoid
geoid_col <- names(acs_norm)[which(tolower(names(acs_norm)) == found_geoid[1])]
acs_norm <- acs_norm |> rename(geoid = !!sym(geoid_col))











# build acs_clean and derived indicators
acs_clean <- acs_norm |>
  mutate(
    # keep identifier columns
    GEOID = geoid,
    state = as.character(stat_e),
    year = as.integer(year),

    # income and rent estimates (from canonical names)
    median_income = median_income_e,
    median_gross_rent = median_gross_rent_e,

    # poverty
    poverty_total = poverty_total_e,
    poverty_below = poverty_below_e,
    poverty_rate = if_else(!is.na(poverty_total) & poverty_total > 0,
                           poverty_below / poverty_total, NA_real_),

    # education
    total_edu = total_edu_e,
    edu_bachelors = edu_bachelors_e,
    edu_graduate = edu_graduate_e,
    college_share = if_else(!is.na(total_edu) & total_edu > 0,
                            (edu_bachelors + edu_graduate) / total_edu, NA_real_),

    # population & race
    total_population = total_population_e,
    white_pop = white_pop_e,
    black_pop = black_pop_e,
    asian_pop = asian_pop_e,
    hispanic_pop = hispanic_pop_e,
    white_share = if_else(!is.na(total_population) & total_population>0, white_pop/total_population, NA_real_),
    black_share = if_else(!is.na(total_population) & total_population>0, black_pop/total_population, NA_real_),
    asian_share = if_else(!is.na(total_population) & total_population>0, asian_pop/total_population, NA_real_),
    hispanic_share = if_else(!is.na(total_population) & total_population>0, hispanic_pop/total_population, NA_real_),

    # housing tenure & rent burden
    owner_occ = owner_occupied_e,
    renter_occ = renter_occupied_e,
    owner_rate = if_else(!is.na(owner_occ + renter_occ) & (owner_occ + renter_occ)>0,
                         owner_occ / (owner_occ + renter_occ), NA_real_),

    rent_burden_total = rowSums(across(c(rent_burden_30_35_e, rent_burden_35_40_e, rent_burden_40_50_e, rent_burden_50_plus_e)),
                                na.rm = TRUE),
    rent_burden_rate = if_else(!is.na(owner_occ + renter_occ) & (owner_occ + renter_occ)>0,
                               rent_burden_total / (owner_occ + renter_occ), NA_real_),

    # employment
    labor_total = labor_total_e,
    labor_employed = labor_employed_e,
    labor_unemployed = labor_unemployed_e,
    unemployment_rate = if_else(!is.na(labor_total) & labor_total>0,
                                labor_unemployed / labor_total, NA_real_),

    # occupations
    prof_occ = prof_occ_e,
    prof_occ_rate = if_else(!is.na(labor_total) & labor_total>0,
                            prof_occ / labor_total, NA_real_)
  ) |>
  select(GEOID, NAME = nam_e, stat_e, year,
         median_income, median_gross_rent, median_home_value_e = median_home_value_e,
         poverty_total, poverty_below, poverty_rate,
         total_population, median_age_e = median_age_e,
         college_share, owner_rate, renter_occ, rent_burden_rate,
         unemployment_rate, prof_occ_rate,
         white_share, black_share, asian_share, hispanic_share,
         everything())  # keep everything else for traceability








#CPI inflation adjustment factors.

#How CPI is calculated: adj_factor(year) = CPI_target_year / CPI_year. Our target year is 2023.

#The CPI valued are derived from https://www.bls.gov/cpi/tables/supplemental-files/historical-cpi-u-202412.pdf?


cpi_adj <- tibble(year = 2011:2023, adj_factor = c(224.939/304.702, 229.594/304.702,  232.957/304.702,  236.736/304.702,  237.017/304.702, 240.007/304.702,  245.120/304.702,  251.107/304.702,  255.657/304.702, 258.811/304.702, 270.970/304.702, 292.655/304.702 , 304.702/304.702))

acs_clean <- acs_clean |>
  left_join(cpi_adj, by = "year") |>
  mutate(
    real_income = if_else(!is.na(median_income) & median_income>0, median_income * adj_factor, NA_real_),
    real_rent   = if_else(!is.na(median_gross_rent) & median_gross_rent>0, median_gross_rent * adj_factor, NA_real_)
  )







#Percent Missing Values
missing_summary <- acs_clean |>
  summarize(across(everything(), ~mean(is.na(.))*100)) |>
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "pct_missing") |>
  arrange(desc(pct_missing))

missing_summary |> head(20)   # show top 20 missing





#Missingness by State
missing_by_state <- acs_clean |>
  group_by(state) |>
  summarize(across(where(is.numeric), ~mean(is.na(.))*100))

missing_by_state





#Missing by year
missing_by_year <- acs_clean |>
  group_by(year) |>
  summarize(across(where(is.numeric), ~mean(is.na(.))*100))

missing_by_year






#Drop columns with more than 40% missing values
threshold <- 0.40
acs_clean <- acs_clean[, colMeans(is.na(acs_clean)) < threshold]






#Impute numeric columns with median
num_cols <- names(acs_clean)[sapply(acs_clean, is.numeric)]

for (col in num_cols) {
  med <- median(acs_clean[[col]], na.rm = TRUE)
  acs_clean[[col]][is.na(acs_clean[[col]])] <- med
}







#Impute categorical values with mode
Mode <- function(x) names(sort(table(x), decreasing = TRUE))[1]

cat_cols <- names(acs_clean)[sapply(acs_clean, is.character)]

for (col in cat_cols) {
  mode_val <- Mode(acs_clean[[col]])
  acs_clean[[col]][is.na(acs_clean[[col]])] <- mode_val
}

















#All variables are also the correct type.
#Now lets strictly compare 2012 to 2023 data.
years_present <- sort(unique(acs_clean$year))
if(!all(c(2012,2023) %in% years_present)) {
  stop("Either 2012 or 2023 is missing from acs_clean$year. Years present: ", paste(years_present, collapse = ", "))
}

gain_df <- acs_clean |>
  filter(year %in% c(2012,2023)) |>
  # select canonical values to pivot
  select(GEOID, state, year, real_income, total_population, college_share) |>
  # coerce year to a label used in names
  mutate(year = paste0("yr", year)) |>
  pivot_wider(
    names_from = year,
    values_from = c(real_income, total_population, college_share),
    names_glue = "{.value}_{year}"
  ) |>
  # ensure columns exist
  mutate(
    income_diff = real_income_yr2023 - real_income_yr2012,
    pct_income_change = income_diff / real_income_yr2012
  ) |>
  arrange(desc(income_diff))

# Quick check:
names(gain_df)[str_detect(names(gain_df),"real_income")]
head(gain_df,6)










#Now lets integrate with TIGER/Line shapefiles and plot the median_income, median_gross_rent, poverty_rate and college_share variables.





options(tigris_use_cache = TRUE)


load_shapes <- function(state_abbrev, years) {
  shape_list <- lapply(years, function(y) {
    tracts(state = state_abbrev, year = y) |>
      st_transform(4326) |>
      mutate(year = y)
  })
  bind_rows(shape_list)
}


#merge shapes + ACS data

merge_shapes_acs <- function(shapes, acs_data, state_abbrev, years) {
  acs_sub <- acs_data |>
    filter(state == state_abbrev, year %in% years) |>
    mutate(GEOID = as.character(GEOID))

  shapes |>
    left_join(acs_sub, by = c("GEOID", "year"))
}


plot_variable_change <- function(
    state_abbrev,
    acs_data,
    variable,
    years = c(2012, 2023),
    title_label = NULL
) {
  if (is.null(title_label)) title_label <- variable

  shapes <- load_shapes(state_abbrev, years)
  map_data <- merge_shapes_acs(shapes, acs_data, state_abbrev, years)

  ggplot(map_data) +
    geom_sf(aes(fill = .data[[variable]]), color = NA) +
    scale_fill_viridis_c() +
    facet_wrap(~ year) +
    theme_minimal() +
    labs(
      title = paste0(title_label, " — ", state_abbrev,
                     " (", paste(years, collapse = " vs "), ")"),
      fill = title_label
    )
}


#  Gentrification Map (Percent change 2012 - 2023)
plot_gentrification <- function(
    state_abbrev,
    acs_data,
    variable,
    years = c(2012, 2023),
    title_label = NULL,
    cap = 200      # percent-change cap (+/-)
) {
  library(dplyr)
  library(tidyr)
  library(sf)
  library(tigris)
  library(ggplot2)
  library(viridis)

  if (is.null(title_label)) {
    title_label <- paste("Gentrification (% change in", variable, ")")
  }


  acs_sub <- acs_data |>
    filter(state == state_abbrev, year %in% years) |>
    mutate(GEOID = as.character(GEOID)) |>
    select(GEOID, year, value = !!sym(variable))

  acs_wide <- acs_sub |>
    pivot_wider(names_from = year, values_from = value, names_prefix = "yr_")


  # CLEAN the data

  acs_wide <- acs_wide |>
    mutate(
      # Remove suppressed or absurd denominators
      yr_2012 = ifelse(yr_2012 <= 50, NA, yr_2012),
      yr_2023 = ifelse(yr_2023 <= 50, NA, yr_2023),

      # Compute % change
      pct_change_raw = (yr_2023 - yr_2012) / yr_2012 * 100,

      # Cap extreme values
      pct_change = ifelse(
        abs(pct_change_raw) > cap,
        NA,
        pct_change_raw
      )
    )


  # Load shapes

  shapes_2023 <- tracts(state = state_abbrev, year = 2023) |>
    st_transform(4326)

  # Join shapes + data
  map_data <- shapes_2023 |>
    left_join(acs_wide, by = "GEOID")


  ggplot(map_data) +
    geom_sf(aes(fill = pct_change), color = NA) +
    scale_fill_viridis_c(
      option = "plasma",
      na.value = "grey80",
      limits = c(-cap, cap),
      oob = scales::squish
    ) +
    theme_minimal(base_size = 14) +
    labs(
      title = paste0("Gentrification Map — ", state_abbrev),
      subtitle = paste("Percent Change in", variable, "(2012 -> 2023)\n(capped at ±", cap, "%)"),
      fill = "% Change"
    )
}






year_seq <- 2012:2023


acs_long_complete <- acs_clean |>
  select(GEOID, state, year, real_income, total_population, college_share) |>
  mutate(year = as.integer(year)) |>
  complete(GEOID, year = year_seq, fill = list(
    real_income = NA,
    total_population = NA,
    college_share = NA
  )) |>
  group_by(GEOID) |>
  fill(state, .direction = "downup") |>  # recover state even if not in all years
  ungroup()

# Quick check:
table(acs_long_complete$year)

# ----------------------------------------------------------

years_present <- sort(unique(acs_long_complete$year))
if(!all(c(2012,2023) %in% years_present)) {
  stop("Missing required years; years present: ",
       paste(years_present, collapse = ", "))
}

gain_df <- acs_long_complete |>
  filter(year %in% c(2012, 2023)) |>
  mutate(year = paste0("yr", year)) |>
  pivot_wider(
    names_from = year,
    values_from = c(real_income, total_population, college_share),
    names_glue = "{.value}_{year}"
  ) |>
  mutate(
    income_diff       = real_income_yr2023 - real_income_yr2012,
    pct_income_change = income_diff / real_income_yr2012
  ) |>
  arrange(desc(income_diff))

# Check output columns:
names(gain_df)[str_detect(names(gain_df), "real_income")]



data_folder <- "/Users/zogaduka/Documents/GraduateSchool/semester1/EDA/FinalProject/GentrificationAnalysis/data"

# --- Saving Dataframes ---

# 1. Save the raw, unnormalized data
saveRDS(acs_raw, file = file.path(data_folder, "acs_raw.rds"))

# 2. Save the fully cleaned, wide-format data (by year)
saveRDS(acs_clean, file = file.path(data_folder, "acs_clean.rds"))

# 3. Save the full, long-format dataset for 2011-2023
saveRDS(acs_long_complete, file = file.path(data_folder, "acs_long_complete.rds"))

# 4. Save the 2012 vs 2023 comparison data
saveRDS(gain_df, file = file.path(data_folder, "gain_df.rds"))





