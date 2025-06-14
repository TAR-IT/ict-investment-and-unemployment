###########################
# Install & load packages #
###########################

# Pakete definieren und installieren, falls notwendig
required_packages <- c(
  "ggplot2", "zoo", "plm", "dplyr",
  "modelsummary", "knitr", "kableExtra"
)

# Fehlende Pakete installieren
new_packages <- required_packages[!sapply(required_packages,
                                          requireNamespace,
                                          quietly = TRUE)]
if (length(new_packages)) install.packages(new_packages)

# Alle Pakete laden
invisible(lapply(required_packages, library, character.only = TRUE))

####################
# Define functions #
####################

# Function for describing variables
describe_variables <- function(data, variables) {
  options(scipen = 999) # Deactivate scientific notation
  described_variables <- data.frame()
  for (var_name in variables) {
    variable <- data[[var_name]]
    if (is.numeric(variable)) {
      summary_stats <- data.frame(
        Variable = var_name,
        Min = round(min(variable, na.rm = TRUE), 2),
        Max = round(max(variable, na.rm = TRUE), 2),
        Mean = round(mean(variable, na.rm = TRUE), 2),
        Median = round(median(variable, na.rm = TRUE), 2),
        SD = round(sd(variable, na.rm = TRUE), 2),
        N = sum(!is.na(variable))
      )
      described_variables <- rbind(described_variables, summary_stats)
    } else {
      warning(paste("Variable", var_name,
                    "is not numeric and was skipped."))
    }
  }
  return(described_variables)
}

# Function for describing categorial variables
describe_welfarestate <- function(data, variable) {
  table_data <- table(data[[variable]])
  freq_table <- data.frame(
    Kategorie = names(table_data),
    Anzahl = as.numeric(table_data),
    Prozent = round(as.numeric(table_data) / sum(table_data) * 100, 2)
  )
  return(freq_table)
}

# Function for descriptive visualisation per country
plot_country_data <- function(data, country_name) {
  required_vars <- c("ICT_INVEST_SHARE_GDP",
                     "UNEMPLOYMENT_RATE_PERCENT",
                     "YEAR_OF_OBSERVATION", "SUBJECT")
  missing_vars <- setdiff(required_vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop(paste("Missing columns in dataset:",
               paste(missing_vars, collapse = ", ")))
  }
  # Filter data
  filtered_data <- data %>%
    filter(REFERENCE_AREA == country_name) %>%
    mutate(SUBJECT = recode(SUBJECT,
                            "Tertiary"
                            = "High education",
                            "Upper secondary, non-tertiary"
                            = "Medium education",
                            "Below upper secondary"
                            = "Low education")) %>%
    mutate(SUBJECT = factor(SUBJECT, levels = c("Low education",
                                                "Medium education",
                                                "High education")))
  if (nrow(filtered_data) == 0) {
    stop(paste("No data found for", country_name,
               "in REFERENCE_AREA. Available values:",
               paste(unique(data$REFERENCE_AREA), collapse = ", ")))
  }
  max_invest <- max(filtered_data$ICT_INVEST_SHARE_GDP, na.rm = TRUE)
  max_unemp <- max(filtered_data$UNEMPLOYMENT_RATE_PERCENT, na.rm = TRUE)
  ggplot(filtered_data, aes(x = YEAR_OF_OBSERVATION)) +
    geom_line(aes(y = ICT_INVEST_SHARE_GDP,
                  color = "ICT investment (GDP share)"), size = 1) +
    geom_line(aes(y = UNEMPLOYMENT_RATE_PERCENT / max_unemp * max_invest,
                  color = "Unemployment rate"),
              size = 1, linetype = "dashed") +
    facet_wrap(~SUBJECT, scales = "free_y") +
    labs(
      x = "Year", y = "ICT investment (GDP share)",
      color = "Indicator"
    ) +
    theme_minimal() +
    scale_x_continuous(
      breaks = seq(min(filtered_data$YEAR_OF_OBSERVATION, na.rm = TRUE),
                   max(filtered_data$YEAR_OF_OBSERVATION, na.rm = TRUE), 5)
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(sec.axis = sec_axis(~ . / max_invest * max_unemp,
                                           name = "Unemployment rate (%)"))
}

####################
# Global variables #
####################

# Country list for the analysis
selected_countries <- c(
  "Australia", "Austria", "Belgium", "Bulgaria", "Brazil",
  "Canada", "Croatia", "Czechia", "Denmark", "Estonia",
  "Finland", "France", "Germany", "Greece", "Hungary",
  "Iceland", "Italy", "Ireland","Latvia", "Lithuania",
  "Luxembourg", "Netherlands", "New Zealand", "Norway",
  "Poland", "Portugal", "Romania", "Spain", "Sweden",
  "Switzerland", "Türkiye", "Slovak Republic", "Slovenia",
  "United Kingdom", "United States"
)

# List of variables for the analysis
variables <- c(
  "UNEMPLOYMENT_RATE_PERCENT", "ICT_INVEST_SHARE_GDP",
  "GDP_PER_CAPITA", "PERCENT_EMPLOYEES_TUD",
  "PERCENT_TERTIARY_EDUCATION", "REGULATION_STRICTNESS"
)

# Shortened variables for models
coef_labels <- c(
  # Variables
  "ICT_INVEST_SHARE_GDP" = "ICT investment",
  "ICT_INVEST_SHARE_GDP_L1" = "ICT investment (1Y lag)",
  "ICT_INVEST_SHARE_GDP_L2" = "ICT investment (2Y lag)",
  "ICT_INVEST_SHARE_GDP_L3" = "ICT investment (3Y lag)",
  "ICT_INVEST_SHARE_GDP_L4" = "ICT investment (4Y lag)",
  "ICT_INVEST_SHARE_GDP_L5" = "ICT investment (5Y lag)",
  "ICT_INVEST_SHARE_GDP_L6" = "ICT investment (6Y lag)",
  "ICT_INVEST_SHARE_GDP_L7" = "ICT investment (7Y lag)",
  "ICT_INVEST_SHARE_GDP_L8" = "ICT investment (8Y lag)",
  
  # Interactions
  "ICT_INVEST_SHARE_GDP:WELFARE_STATECentral European" =
    "ICT × Central European",
  "ICT_INVEST_SHARE_GDP:WELFARE_STATENordic" =
    "ICT × Nordic",
  "ICT_INVEST_SHARE_GDP:WELFARE_STATEPost-socialist" =
    "ICT × Post-socialist",
  "ICT_INVEST_SHARE_GDP:WELFARE_STATESouthern European" =
    "ICT × Southern European",
  
  "ICT_INVEST_SHARE_GDP_L1:WELFARE_STATECentral European" =
    "ICT 1Y lag × Central European",
  "ICT_INVEST_SHARE_GDP_L1:WELFARE_STATENordic" =
    "ICT 1Y lag × Nordic",
  "ICT_INVEST_SHARE_GDP_L1:WELFARE_STATEPost-socialist" =
    "ICT 1Y lag × Post-socialist",
  "ICT_INVEST_SHARE_GDP_L1:WELFARE_STATESouthern European" =
    "ICT 1Y lag × Southern European",
  
  "ICT_INVEST_SHARE_GDP_L2:WELFARE_STATECentral European" =
    "ICT 2Y lag × Central European",
  "ICT_INVEST_SHARE_GDP_L2:WELFARE_STATENordic" =
    "ICT 2Y lag × Nordic",
  "ICT_INVEST_SHARE_GDP_L2:WELFARE_STATEPost-socialist" =
    "ICT 2Y lag × Post-socialist",
  "ICT_INVEST_SHARE_GDP_L2:WELFARE_STATESouthern European" =
    "ICT 2Y lag × Southern European",
  
  "ICT_INVEST_SHARE_GDP_L3:WELFARE_STATECentral European" =
    "ICT 3Y lag × Central European",
  "ICT_INVEST_SHARE_GDP_L3:WELFARE_STATENordic" =
    "ICT 3Y lag × Nordic",
  "ICT_INVEST_SHARE_GDP_L3:WELFARE_STATEPost-socialist" =
    "ICT 3Y lag × Post-socialist",
  "ICT_INVEST_SHARE_GDP_L3:WELFARE_STATESouthern European" =
    "ICT 3Y lag × Southern European",
  
  "ICT_INVEST_SHARE_GDP_L4:WELFARE_STATECentral European" =
    "ICT 4Y lag × Central European",
  "ICT_INVEST_SHARE_GDP_L4:WELFARE_STATENordic" =
    "ICT 4Y lag × Nordic",
  "ICT_INVEST_SHARE_GDP_L4:WELFARE_STATEPost-socialist" =
    "ICT 4Y lag × Post-socialist",
  "ICT_INVEST_SHARE_GDP_L4:WELFARE_STATESouthern European" =
    "ICT 4Y lag × Southern European",
  
  "ICT_INVEST_SHARE_GDP_L5:WELFARE_STATECentral European" =
    "ICT 5Y lag × Central European",
  "ICT_INVEST_SHARE_GDP_L5:WELFARE_STATENordic" =
    "ICT 5Y lag × Nordic",
  "ICT_INVEST_SHARE_GDP_L5:WELFARE_STATEPost-socialist" =
    "ICT 5Y lag × Post-socialist",
  "ICT_INVEST_SHARE_GDP_L5:WELFARE_STATESouthern European" =
    "ICT 5Y lag × Southern European",
  
  "ICT_INVEST_SHARE_GDP_L6:WELFARE_STATECentral European" =
    "ICT 6Y lag × Central European",
  "ICT_INVEST_SHARE_GDP_L6:WELFARE_STATENordic" =
    "ICT 6Y lag × Nordic",
  "ICT_INVEST_SHARE_GDP_L6:WELFARE_STATEPost-socialist" =
    "ICT 6Y lag × Post-socialist",
  "ICT_INVEST_SHARE_GDP_L6:WELFARE_STATESouthern European" =
    "ICT 6Y lag × Southern European",
  
  "ICT_INVEST_SHARE_GDP_L7:WELFARE_STATECentral European" =
    "ICT 7Y lag × Central European",
  "ICT_INVEST_SHARE_GDP_L7:WELFARE_STATENordic" =
    "ICT 7Y lag × Nordic",
  "ICT_INVEST_SHARE_GDP_L7:WELFARE_STATEPost-socialist" =
    "ICT 7Y lag × Post-socialist",
  "ICT_INVEST_SHARE_GDP_L7:WELFARE_STATESouthern European" =
    "ICT 7Y lag × Southern European",
  
  "ICT_INVEST_SHARE_GDP_L8:WELFARE_STATECentral European" =
    "ICT 8Y lag × Central European",
  "ICT_INVEST_SHARE_GDP_L8:WELFARE_STATENordic" =
    "ICT 8Y lag × Nordic",
  "ICT_INVEST_SHARE_GDP_L8:WELFARE_STATEPost-socialist" =
    "ICT 8Y lag × Post-socialist",
  "ICT_INVEST_SHARE_GDP_L8:WELFARE_STATESouthern European" =
    "ICT 8Y lag × Southern European",
  
  # Controls
  "GDP_PER_CAPITA" = "GDP per capita",
  "PERCENT_TERTIARY_EDUCATION" = "% tertiary education",
  "REGULATION_STRICTNESS" = "Employment protection strictness",
  "PERCENT_EMPLOYEES_TUD" = "% trade union density"
)

#################################
# Loading and cleaning datasets #
#################################

# OECD dataset for unemployment rate
data_unemp <- read.table(
  "data/OECD_unemployment_rates_by_education_level_annual_2000-2022.csv",
  header = TRUE, sep = ";", dec = ",", fileEncoding = "UTF-8"
) %>%
  # Filter by countries and time period
  filter(
    Country %in% selected_countries,
    TIME_PERIOD >= 2005 & TIME_PERIOD <= 2022
  ) %>%
  # Welfare state categorization
  mutate(
    WELFARE_STATE = case_when(
      Country %in% c("Denmark", "Sweden",
                     "Norway", "Finland", "Iceland") ~ "Nordic",
      Country %in% c("Germany", "France", "Austria",
                     "Belgium", "Netherlands",
                     "Luxembourg", "Switzerland") ~ "Central European",
      Country %in% c("United States", "United Kingdom",
                     "Canada", "Australia", "New Zealand",
                     "Ireland") ~ "Anglo-Saxon",
      Country %in% c("Italy", "Spain", "Portugal",
                     "Greece") ~ "Southern European",
      Country %in% c("Poland", "Czechia", "Hungary",
                     "Slovak Republic", "Slovenia",
                     "Estonia", "Latvia", "Lithuania",
                     "Romania", "Bulgaria") ~ "Post-socialist",
      TRUE ~ "Other"  # If countries are not categorized
    ),
    WELFARE_STATE = as.factor(WELFARE_STATE),
    REFERENCE_AREA = as.character(Country),
    SUBJECT = as.character(Subject),
    YEAR_OF_OBSERVATION = as.integer(TIME_PERIOD),
    UNEMPLOYMENT_RATE_PERCENT = OBS_VALUE
  ) %>%
  # Delete unnessecary rows and reorder
  select(REFERENCE_AREA, WELFARE_STATE, YEAR_OF_OBSERVATION,
         SUBJECT, UNEMPLOYMENT_RATE_PERCENT)

# OECD dataset for ICT investments
data_ictinvest <- read.table(
  "data/OECD_ict_investment_share_of_gdp_2000-2022.csv",
  header = TRUE, sep = ";", dec = ",", fileEncoding = "UTF-8"
) %>%
  filter(
    Country %in% selected_countries,
    Year >= 2005 & Year <= 2022,
    Unit.of.measure == "Share of GDP",
    Breakdown == "Total ICT investment"
  ) %>%
  mutate(
    REFERENCE_AREA = as.character(Country),
    YEAR_OF_OBSERVATION = as.integer(Year),
    ICT_INVEST_SHARE_GDP = Value
  ) %>%
  select(REFERENCE_AREA, YEAR_OF_OBSERVATION, ICT_INVEST_SHARE_GDP)

# OECD dataset for GDPs
data_gdp <- read.table(
  "data/OECD_gdp_2000-2023.csv",
  header = TRUE, sep = ";", dec = ",", fileEncoding = "UTF-8"
) %>%
  filter(
    Reference.area %in% selected_countries,
    TIME_PERIOD >= 2005 & TIME_PERIOD <= 2022
  ) %>%
  mutate(
    REFERENCE_AREA = as.character(Reference.area),
    YEAR_OF_OBSERVATION = as.integer(TIME_PERIOD),
    GDP_PER_CAPITA = OBS_VALUE / 1000
  ) %>%
  select(REFERENCE_AREA, YEAR_OF_OBSERVATION, GDP_PER_CAPITA)

# OECD dataset for percentage in tertiary education
data_pte <- read.table(
  "data/OEDC_adults-educational-attainment-distribution_2000-2022.csv",
  header = TRUE, sep = ";", dec = ",", fileEncoding = "UTF-8"
) %>%
  filter(
    Reference.area %in% selected_countries,
    TIME_PERIOD >= 2005 & TIME_PERIOD <= 2022
  ) %>%
  mutate(
    REFERENCE_AREA = as.character(Reference.area),
    YEAR_OF_OBSERVATION = as.integer(TIME_PERIOD),
    PERCENT_TERTIARY_EDUCATION = OBS_VALUE
  ) %>%
  select(REFERENCE_AREA, YEAR_OF_OBSERVATION, PERCENT_TERTIARY_EDUCATION)

# OECD dataset for regulation strictness
data_reg <- read.table(
  "data/OECD_strictness-of-employment-protection_2000-2022.csv",
  header = TRUE, sep = ";", dec = ",", fileEncoding = "UTF-8"
) %>%
  filter(
    Reference.area %in% selected_countries,
    TIME_PERIOD >= 2005 & TIME_PERIOD <= 2022
  ) %>%
  mutate(
    REFERENCE_AREA = as.character(Reference.area),
    YEAR_OF_OBSERVATION = as.integer(TIME_PERIOD),
    REGULATION_STRICTNESS = OBS_VALUE
  ) %>%
  select(REFERENCE_AREA, YEAR_OF_OBSERVATION, REGULATION_STRICTNESS)

# OECD dataset for trade union densitry
data_tud <- read.table(
  "data/OECD_trade_union_density_2000-2022.csv",
  header = TRUE, sep = ";", dec = ",", fileEncoding = "UTF-8"
) %>%
  filter(
    Reference.area %in% selected_countries,
    TIME_PERIOD >= 2005 & TIME_PERIOD <= 2022
  ) %>%
  mutate(
    REFERENCE_AREA = as.character(Reference.area),
    YEAR_OF_OBSERVATION = as.integer(TIME_PERIOD),
    PERCENT_EMPLOYEES_TUD = OBS_VALUE
  ) %>%
  select(REFERENCE_AREA, YEAR_OF_OBSERVATION, PERCENT_EMPLOYEES_TUD)

####################
# Merging datasets #
####################

# One dataset with all variables
merged_data <- data_ictinvest %>%
  left_join(data_unemp, by = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")) %>%
  left_join(data_gdp, by = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")) %>%
  left_join(data_pte, by = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")) %>%
  left_join(data_reg, by = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")) %>%
  left_join(data_tud, by = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")) %>%
  group_by(REFERENCE_AREA) %>%
  arrange(YEAR_OF_OBSERVATION) %>%  # Sort for interpolation
  mutate(
    PERCENT_EMPLOYEES_TUD =
      na.approx(as.numeric(PERCENT_EMPLOYEES_TUD),
                na.rm = FALSE,
                rule = 2),
    PERCENT_TERTIARY_EDUCATION =
      na.approx(as.numeric(PERCENT_TERTIARY_EDUCATION),
                na.rm = FALSE,
                rule = 2),
    REGULATION_STRICTNESS =
      na.approx(as.numeric(REGULATION_STRICTNESS),
                na.rm = FALSE,
                rule = 2),
    # Variables for lagged ICT investment
    ICT_INVEST_SHARE_GDP_L1 = lag(ICT_INVEST_SHARE_GDP, 1),
    ICT_INVEST_SHARE_GDP_L2 = lag(ICT_INVEST_SHARE_GDP, 2),
    ICT_INVEST_SHARE_GDP_L3 = lag(ICT_INVEST_SHARE_GDP, 3),
    ICT_INVEST_SHARE_GDP_L4 = lag(ICT_INVEST_SHARE_GDP, 4),
    ICT_INVEST_SHARE_GDP_L5 = lag(ICT_INVEST_SHARE_GDP, 5),
    ICT_INVEST_SHARE_GDP_L6 = lag(ICT_INVEST_SHARE_GDP, 6),
    ICT_INVEST_SHARE_GDP_L7 = lag(ICT_INVEST_SHARE_GDP, 7),
    ICT_INVEST_SHARE_GDP_L8 = lag(ICT_INVEST_SHARE_GDP, 8)
  ) %>%
  ungroup() %>%
  mutate(
    SUBJECT = recode(
      SUBJECT,
      "Below upper secondary" = "Low education",
      "Upper secondary, non-tertiary" = "Medium education",
      "Tertiary" = "High education"
    ),
    # Dummy variable for year fixed effects
    YEAR_FACTOR = relevel(factor(YEAR_OF_OBSERVATION), ref = "2005")
  )

####################
# Variable summary #
####################

# Describing variables
described_variables <- describe_variables(merged_data, variables)
described_welfarestates <- describe_welfarestate(merged_data, "WELFARE_STATE")

# Show and save results
print(described_variables)
writeLines(
  kable(described_variables,
    format = "latex",
    booktabs = TRUE,
    caption = "Summary of variables"
  ) %>%
    kable_styling(latex_options = c("hold_position")),
  "../TeX/assets/variables_summary.tex"
)

# Summary of Welfare state variable
print(described_welfarestates)
writeLines(
  kable(described_welfarestates,
    format = "latex",
    booktabs = TRUE,
    caption = "Overview of the distribution of welfare state types"
  ) %>%
    kable_styling(latex_options = c("hold_position")),
  "../TeX/assets/variables_welfarestate.tex"
)

#############################################
# Filter data according to education groups #
#############################################

filter_data_low     <- subset(merged_data, SUBJECT ==
                                "Low education")
filter_data_medium  <- subset(merged_data, SUBJECT ==
                                "Medium education")
filter_data_high    <- subset(merged_data, SUBJECT ==
                                "High education")

#############################
# Models without time delay #
#############################

model_low_nolag             <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP * WELFARE_STATE +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS +
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_low, model = "within",
  index = c("REFERENCE_AREA")
)

model_medium_nolag          <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP * WELFARE_STATE +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS +
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_medium, model = "within",
  index = c("REFERENCE_AREA")
)

model_high_nolag            <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP * WELFARE_STATE +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS +
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_high, model = "within",
  index = c("REFERENCE_AREA")
)

###########################
# Models with time delay  #
###########################

# 1-year-lag
model_low_1ylag             <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L1 * WELFARE_STATE +
    YEAR_FACTOR + GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_low, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_medium_1ylag          <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L1 * WELFARE_STATE +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS +
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_medium, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_high_1ylag            <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L1 * WELFARE_STATE +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_high, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

# 2-year-lag
model_low_2ylag             <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L2 * WELFARE_STATE +
    YEAR_FACTOR + GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_low, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_medium_2ylag          <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L2 * WELFARE_STATE +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS +
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_medium, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_high_2ylag            <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L2 * WELFARE_STATE +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_high, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

# 3-year-lag
model_low_3ylag             <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L3 * WELFARE_STATE +
    YEAR_FACTOR + GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_low, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_medium_3ylag          <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L3 * WELFARE_STATE +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS +
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_medium, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_high_3ylag            <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L3 * WELFARE_STATE +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_high, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

# 4-year-lag
model_low_4ylag             <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L4 * WELFARE_STATE +
    YEAR_FACTOR + GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_low, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_medium_4ylag          <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L4 * WELFARE_STATE +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS +
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_medium, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_high_4ylag            <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L4 * WELFARE_STATE +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_high, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

# 5-year-lag
model_low_5ylag <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L5 * WELFARE_STATE +
    YEAR_FACTOR + GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_low, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_medium_5ylag <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L5 * WELFARE_STATE +
    YEAR_FACTOR + GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS +
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_medium, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_high_5ylag <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L5 * WELFARE_STATE +
    YEAR_FACTOR + GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_high, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

# 6-year-lag
model_low_6ylag <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L6 * WELFARE_STATE +
    YEAR_FACTOR + GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_low, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_medium_6ylag <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L6 * WELFARE_STATE +
    YEAR_FACTOR + GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS +
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_medium, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_high_6ylag <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L6 * WELFARE_STATE +
    YEAR_FACTOR + GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_high, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

# 7-year-lag
model_low_7ylag <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L7 * WELFARE_STATE +
    YEAR_FACTOR + GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_low, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_medium_7ylag <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L7 * WELFARE_STATE +
    YEAR_FACTOR + GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS +
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_medium, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_high_7ylag <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L7 * WELFARE_STATE +
    YEAR_FACTOR + GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_high, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

# 8-year-lag
model_low_8ylag <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L8 * WELFARE_STATE +
    YEAR_FACTOR + GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_low, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_medium_8ylag <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L8 * WELFARE_STATE +
    YEAR_FACTOR + GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS +
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_medium, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_high_8ylag <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L8 * WELFARE_STATE +
    YEAR_FACTOR + GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_high, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)


########################################
# Summary of models #
########################################

# List of models with no lag
models_nolag <- list(
  "Low\neducation\n(No lag)" =
    model_low_nolag,
  "Medium\neducation\n(No lag)" =
    model_medium_nolag,
  "High\neducation\n(No lag)" =
    model_high_nolag
)

# List for models with lag
models_1ylag <- list(
  "Low\neducation" = model_low_1ylag,
  "Medium\neducation" = model_medium_1ylag,
  "High\neducation" = model_high_1ylag
)

models_2ylag <- list(
  "Low\neducation" = model_low_2ylag,
  "Medium\neducation" = model_medium_2ylag,
  "High\neducation" = model_high_2ylag
)

models_3ylag <- list(
  "Low\neducation" = model_low_3ylag,
  "Medium\neducation" = model_medium_3ylag,
  "High\neducation" = model_high_3ylag
)

models_4ylag <- list(
  "Low\neducation" = model_low_4ylag,
  "Medium\neducation" = model_medium_4ylag,
  "High\neducation" = model_high_4ylag
)

models_5ylag <- list(
  "Low\neducation" = model_low_5ylag,
  "Medium\neducation" = model_medium_5ylag,
  "High\neducation" = model_high_5ylag
)

models_6ylag <- list(
  "Low\neducation" = model_low_6ylag,
  "Medium\neducation" = model_medium_6ylag,
  "High\neducation" = model_high_6ylag
)

models_7ylag <- list(
  "Low\neducation" = model_low_7ylag,
  "Medium\neducation" = model_medium_7ylag,
  "High\neducation" = model_high_7ylag
)

models_8ylag <- list(
  "Low\neducation" = model_low_8ylag,
  "Medium\neducation" = model_medium_8ylag,
  "High\neducation" = model_high_8ylag
)

# Show and save results (without YEAR_FACTOR)
msummary(models_nolag, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         coef_map = coef_labels)
msummary(models_nolag, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         coef_map = coef_labels,
         output = "../TeX/assets/models_nolag.tex")

msummary(models_1ylag, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         coef_map = coef_labels)
msummary(models_1ylag, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         coef_map = coef_labels,
         output = "../TeX/assets/models_1ylag.tex")

msummary(models_2ylag, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         coef_map = coef_labels)
msummary(models_2ylag, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         coef_map = coef_labels,
         output = "../TeX/assets/models_2ylag.tex")

msummary(models_3ylag, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         coef_map = coef_labels)
msummary(models_3ylag, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         coef_map = coef_labels,
         output = "../TeX/assets/models_3ylag.tex")

msummary(models_4ylag, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         coef_map = coef_labels)
msummary(models_4ylag, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         coef_map = coef_labels,
         output = "../TeX/assets/models_4ylag.tex")

msummary(models_5ylag, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         coef_map = coef_labels)
msummary(models_5ylag, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         coef_map = coef_labels,
         output = "../TeX/assets/models_5ylag.tex")

msummary(models_6ylag, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         coef_map = coef_labels)
msummary(models_6ylag, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         coef_map = coef_labels,
         output = "../TeX/assets/models_6ylag.tex")

msummary(models_7ylag, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         coef_map = coef_labels)
msummary(models_7ylag, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         coef_map = coef_labels,
         output = "../TeX/assets/models_7ylag.tex")

msummary(models_8ylag, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         coef_map = coef_labels)
msummary(models_8ylag, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         coef_map = coef_labels,
         output = "../TeX/assets/models_8ylag.tex")