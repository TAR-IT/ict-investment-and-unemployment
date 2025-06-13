##########################################
#      Packages installieren & laden     #
##########################################

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

##########################################
#         Funktionen definieren          #
##########################################

# Funktion zur Beschreibung der Variablen
describe_variables <- function(data, variables) {
  options(scipen = 999) # Deaktiviert wissenschaftliche Notation
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
                    "ist nicht numerisch und wurde übersprungen."))
    }
  }
  return(described_variables)
}

# Funktion zur Beschreibung kategorialer Variablen
describe_welfarestate <- function(data, variable) {
  table_data <- table(data[[variable]])
  freq_table <- data.frame(
    Kategorie = names(table_data),
    Anzahl = as.numeric(table_data),
    Prozent = round(as.numeric(table_data) / sum(table_data) * 100, 2)
  )
  return(freq_table)
}

# Funktion für deskriptive Visualisierung pro Land
plot_country_data <- function(data, country_name) {
  # Mapping für deutsche Ländernamen in den Plots
  country_map <- c("Spain" = "Spanien", "Poland" = "Polen",
                   "Sweden" = "Schweden", "Germany" = "Deutschland",
                   "United Kingdom" = "Großbritannien")
  # Falls das Land in der Map existiert, nutze den deutschen Namen
  display_name <- ifelse(country_name %in% names(country_map),
                         country_map[[country_name]], country_name)
  # Prüfe, ob die Variablen existieren
  required_vars <- c("ICT_INVEST_SHARE_GDP",
                     "UNEMPLOYMENT_RATE_PERCENT",
                     "YEAR_OF_OBSERVATION", "SUBJECT")
  missing_vars <- setdiff(required_vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop(paste("Fehlende Spalten im Datensatz:",
               paste(missing_vars, collapse = ", ")))
  }
  # Daten filtern
  filtered_data <- data %>%
    filter(REFERENCE_AREA == country_name) %>%
    mutate(SUBJECT = recode(SUBJECT,
                            "Tertiary" =
                              "hohes Bildungsniveau",
                            "Upper secondary, non-tertiary" =
                              "mittleres Bildungsniveau",
                            "Below upper secondary" =
                              "niedriges Bildungsniveau")) %>%
    mutate(SUBJECT = factor(SUBJECT, levels = c("niedriges Bildungsniveau",
                                                "mittleres Bildungsniveau",
                                                "hohes Bildungsniveau")))
  # Prüfen, ob nach dem Filtern noch Daten vorhanden sind
  if (nrow(filtered_data) == 0) {
    stop(paste("Keine Daten gefunden für", country_name,
               "in REFERENCE_AREA. Verfügbare Werte:",
               paste(unique(data$REFERENCE_AREA), collapse = ", ")))
  }
  # Skaliere die Werte nur, wenn sie existieren
  max_invest <- max(filtered_data$ICT_INVEST_SHARE_GDP, na.rm = TRUE)
  max_unemp <- max(filtered_data$UNEMPLOYMENT_RATE_PERCENT, na.rm = TRUE)
  ggplot(filtered_data, aes(x = YEAR_OF_OBSERVATION)) +
    geom_line(aes(y = ICT_INVEST_SHARE_GDP,
                  color = "ICT-Investitionen (BIP-Anteil)"), size = 1) +
    geom_line(aes(y = UNEMPLOYMENT_RATE_PERCENT / max_unemp * max_invest,
                  color = "Arbeitslosenquote"),
              size = 1,
              linetype = "dashed") +
    facet_wrap(~SUBJECT, scales = "free_y") +
    labs(
      x = "Jahr", y = "ICT-Investitionen (BIP-Anteil)",
      color = "Indikator"
    ) +
    theme_minimal() +
    scale_x_continuous(
      breaks = seq(min(filtered_data$YEAR_OF_OBSERVATION,
                       na.rm = TRUE),
                   max(filtered_data$YEAR_OF_OBSERVATION,
                       na.rm = TRUE), 5)
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(sec.axis = sec_axis(~ . / max_invest * max_unemp,
                                           name = "Arbeitslosenquote (%)"))
}

##########################################
#           Globale Variablen            #
##########################################

# Liste aller Länder für die Analyse
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

# Liste aller Variablen für die Analyse
variables <- c(
  "UNEMPLOYMENT_RATE_PERCENT", "ICT_INVEST_SHARE_GDP",
  "GDP_PER_CAPITA", "PERCENT_EMPLOYEES_TUD",
  "PERCENT_TERTIARY_EDUCATION", "REGULATION_STRICTNESS"
)

#############################################
# Bereinigung und Zusammenführung der Daten #
#############################################

# OECD-Datensatz zu Arbeitslosenquoten
data_unemp <- read.table(
  "data/OECD_unemployment_rates_by_education_level_annual_2000-2022.csv",
  header = TRUE, sep = ";", dec = ",", fileEncoding = "UTF-8"
) %>%
  # Filtern nach ausgewählten Ländern und Zeiträumen
  filter(
    Country %in% selected_countries,
    TIME_PERIOD >= 2005 & TIME_PERIOD <= 2022
  ) %>%
  # Wohlfahrtsstaat-Kategorisierung und Typkonvertierung
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
      TRUE ~ "Other"  # Falls Länder fehlen, werden sie kategorisiert
    ),
    WELFARE_STATE = as.factor(WELFARE_STATE),
    REFERENCE_AREA = as.character(Country),
    SUBJECT = as.character(Subject),
    YEAR_OF_OBSERVATION = as.integer(TIME_PERIOD),
    UNEMPLOYMENT_RATE_PERCENT = OBS_VALUE
  ) %>%
  # Entferne nicht benötigte Spalten und ordne sie neu
  select(REFERENCE_AREA, WELFARE_STATE, YEAR_OF_OBSERVATION,
         SUBJECT, UNEMPLOYMENT_RATE_PERCENT)

# OECD-Datensatz zu ICT-Investments
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

# OECD-Datensatz zu GDPs
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

# OECD-Datensatz zu Percentage of Tertiary Education
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

# OECD-Datensatz zu Regulation Strictness
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

# OECD-Datensatz zu Trade Union Density
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

##################################
# Zusammenführung der Datensätze #
##################################

# Einen Datensatz mit allen Daten erstellen
merged_data <- data_ictinvest %>%
  left_join(data_unemp, by = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")) %>%
  left_join(data_gdp, by = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")) %>%
  left_join(data_pte, by = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")) %>%
  left_join(data_reg, by = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")) %>%
  left_join(data_tud, by = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")) %>%
  group_by(REFERENCE_AREA) %>%
  arrange(YEAR_OF_OBSERVATION) %>%  # Sortierung für Interpolation
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
    ICT_INVEST_SHARE_GDP_L1 = lag(ICT_INVEST_SHARE_GDP, 1),
    ICT_INVEST_SHARE_GDP_L2 = lag(ICT_INVEST_SHARE_GDP, 2),
    ICT_INVEST_SHARE_GDP_L3 = lag(ICT_INVEST_SHARE_GDP, 3),
    ICT_INVEST_SHARE_GDP_L4 = lag(ICT_INVEST_SHARE_GDP, 4)
  ) %>%
  ungroup() %>%  # Entgruppierung wegen Interpolation
  mutate(
    SUBJECT = recode(
      SUBJECT,
      "Below upper secondary" = "niedriges Bildungsniveau",
      "Upper secondary, non-tertiary" = "mittleres Bildungsniveau",
      "Tertiary" = "hohes Bildungsniveau"
    ),
    # Dummy-Variable für Jahresfixeffekte
    YEAR_FACTOR = relevel(factor(YEAR_OF_OBSERVATION), ref = "2005")
  )

##########################################
#         Übersicht über Variablen       #
##########################################

# Beschreibung der Variablen
described_variables <- describe_variables(merged_data, variables)
described_welfarestates <- describe_welfarestate(merged_data, "WELFARE_STATE")

# Ergebnisse anzeigen und speichern
print(described_variables)
writeLines(
  kable(described_variables,
    format = "latex",
    booktabs = TRUE,
    caption = "Zusammenfassung der Variablen"
  ) %>%
    kable_styling(latex_options = c("hold_position")),
  "../TeX/assets/variables.tex"
)

# Erstelle die Tabelle für WELFARE_STATE
print(described_welfarestates)
writeLines(
  kable(described_welfarestates,
    format = "latex",
    booktabs = TRUE,
    caption = "Übersicht über die Verteilung der Wohlfahrtsstaatentypen"
  ) %>%
    kable_styling(latex_options = c("hold_position")),
  "../TeX/assets/variables_welfare.tex"
)

##########################################
#       Länderspezifische Analyse        #
##########################################

# Spanien als Südeuropäischer Wohlfahrtsstaat
plot_spain    <- plot_country_data(merged_data, "Spain")
print(plot_spain)
ggsave("../TeX/assets/plot_spain.png",
       plot = plot_spain, width = 12, height = 3)

# Polen als postsozialistischer Wohlfahrtsstaat
plot_poland   <- plot_country_data(merged_data, "Poland")
print(plot_poland)
ggsave("../TeX/assets/plot_poland.png",
       plot = plot_poland, width = 12, height = 3)

# Schweden als nordischer Wohlfahrtsstaat
plot_sweden   <- plot_country_data(merged_data, "Sweden")
print(plot_sweden)
ggsave("../TeX/assets/plot_sweden.png",
       plot = plot_sweden, width = 12, height = 3)

# Deutschland als konservativer Wohlfahrtsstaat
plot_germany  <- plot_country_data(merged_data, "Germany")
print(plot_germany)
ggsave("../TeX/assets/plot_germany.png",
       plot = plot_germany, width = 12, height = 3)

# Großbritannien als angelsächsischer Wohlfahrtsstaat
plot_uk       <- plot_country_data(merged_data, "United Kingdom")
print(plot_uk)
ggsave("../TeX/assets/plot_uk.png",
       plot = plot_uk, width = 12, height = 3)

##########################################
#   Daten nach Bildungsgruppen filtern   #
##########################################

filter_data_low     <- subset(merged_data, SUBJECT ==
                                "niedriges Bildungsniveau")
filter_data_medium  <- subset(merged_data, SUBJECT ==
                                "mittleres Bildungsniveau")
filter_data_high    <- subset(merged_data, SUBJECT ==
                                "hohes Bildungsniveau")

#################################
# Modelle mit Kontrollvariablen #
#################################

# Fixed Effects Modelle (mit Kontrollvariablen)
model_low_fe_control    <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS +
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_low,
  model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_medium_fe_control <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS +
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_medium,
  model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_high_fe_control   <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS +
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_high,
  model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

############################################################
# Fixed Effects Modelle mit Interaktionen und Jahresfaktor #
############################################################

model_low_fe_interaction    <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP * WELFARE_STATE +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS +
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_low, model = "within",
  index = c("REFERENCE_AREA")
)

model_medium_fe_interaction <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP * WELFARE_STATE +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS +
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_medium, model = "within",
  index = c("REFERENCE_AREA")
)

model_high_fe_interaction   <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP * WELFARE_STATE +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS +
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_high, model = "within",
  index = c("REFERENCE_AREA")
)

#########################################################
# Fixed Effects Modelle mit 1-Jahres-Lag & Interaktion  #
#########################################################

model_low_fe_lagged1 <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L1 * WELFARE_STATE +
    YEAR_FACTOR + GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_low, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_medium_fe_lagged1 <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L1 * WELFARE_STATE +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS +
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_medium, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_high_fe_lagged1 <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L1 * WELFARE_STATE +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_high, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

#########################################################
# Fixed Effects Modelle mit 2-Jahres-Lag & Interaktion  #
#########################################################

model_low_fe_lagged2 <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L2 * WELFARE_STATE +
    YEAR_FACTOR + GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_low, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_medium_fe_lagged2 <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L2 * WELFARE_STATE +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS +
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_medium, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_high_fe_lagged2 <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L2 * WELFARE_STATE +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_high, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

#########################################################
# Fixed Effects Modelle mit 3-Jahres-Lag & Interaktion  #
#########################################################

model_low_fe_lagged3 <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L3 * WELFARE_STATE +
    YEAR_FACTOR + GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_low, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_medium_fe_lagged3 <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L3 * WELFARE_STATE +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS +
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_medium, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_high_fe_lagged3 <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L3 * WELFARE_STATE +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_high, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

#########################################################
# Fixed Effects Modelle mit 4-Jahres-Lag & Interaktion  #
#########################################################

model_low_fe_lagged4 <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L4 * WELFARE_STATE +
    YEAR_FACTOR + GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_low, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_medium_fe_lagged4 <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L4 * WELFARE_STATE +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS +
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_medium, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

model_high_fe_lagged4 <- plm(
  UNEMPLOYMENT_RATE_PERCENT ~ ICT_INVEST_SHARE_GDP_L4 * WELFARE_STATE +
    YEAR_FACTOR +
    GDP_PER_CAPITA +
    PERCENT_TERTIARY_EDUCATION +
    REGULATION_STRICTNESS + 
    PERCENT_EMPLOYEES_TUD,
  data = filter_data_high, model = "within",
  index = c("REFERENCE_AREA", "YEAR_OF_OBSERVATION")
)

########################################
# Übersicht der Modelle und Ergebnisse #
########################################

# Liste für Kontrollmodelle
control_models      <- list(
  "niedriges\nBildungsniv.\n(Kontrolle)" = model_low_fe_control,
  "mittleres\nBildungsniv.\n(Kontrolle)" = model_medium_fe_control,
  "hohes\nBildungsniv.\n(Kontrolle)" = model_high_fe_control
)

# Liste für Interaktionsmodelle
interaction_models  <- list(
  "niedriges\nBildungsniv.\n(Interaktion)" =
    model_low_fe_interaction,
  "mittleres\nBildungsniv.\n(Interaktion)" =
    model_medium_fe_interaction,
  "hohes\nBildungsniv.\n(Interaktion)" =
    model_high_fe_interaction
)

# Liste für 1-Jahres-Lag-Modelle
lagged1_models <- list(
  "niedriges Bildungsniveau (1-Y-Lag)" = model_low_fe_lagged1,
  "mittleres Bildungsniveau (1-Y-Lag)" = model_medium_fe_lagged1,
  "hohes Bildungsniveau (1-Y-Lag)" = model_high_fe_lagged1
)

# Liste für 2-Jahres-Lag-Modelle
lagged2_models <- list(
  "niedriges Bildungsniveau (2-Y-Lag)" = model_low_fe_lagged2,
  "mittleres Bildungsniveau (2-Y-Lag)" = model_medium_fe_lagged2,
  "hohes Bildungsniveau (2-Y-Lag)" = model_high_fe_lagged2
)

# Liste für 3-Jahres-Lag-Modelle
lagged3_models <- list(
  "niedriges Bildungsniveau (3-Y-Lag)" = model_low_fe_lagged3,
  "mittleres Bildungsniveau (3-Y-Lag)" = model_medium_fe_lagged3,
  "hohes Bildungsniveau (3-Y-Lag)" = model_high_fe_lagged3
)

# Liste für 4-Jahres-Lag-Modelle
lagged4_models <- list(
  "niedriges Bildungsniveau (4-Y-Lag)" = model_low_fe_lagged4,
  "mittleres Bildungsniveau (4-Y-Lag)" = model_medium_fe_lagged4,
  "hohes Bildungsniveau (4-Y-Lag)" = model_high_fe_lagged4
)

# Ergebnisse anzeigen und speichern (ohne YEAR_FACTOR)
msummary(control_models, stars = TRUE,
         coef_omit = "YEAR_FACTOR")
msummary(control_models, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         output = "../TeX/assets/models_control.tex")

msummary(interaction_models, stars = TRUE,
         coef_omit = "YEAR_FACTOR")
msummary(interaction_models, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         output = "../TeX/assets/models_interaction.tex")

msummary(lagged1_models, stars = TRUE,
         coef_omit = "YEAR_FACTOR")
msummary(lagged1_models, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         output = "../TeX/assets/models_lagged1.tex")

msummary(lagged2_models, stars = TRUE,
         coef_omit = "YEAR_FACTOR")
msummary(lagged2_models, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         output = "../TeX/assets/models_lagged2.tex")

msummary(lagged3_models, stars = TRUE,
         coef_omit = "YEAR_FACTOR")
msummary(lagged3_models, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         output = "../TeX/assets/models_lagged3.tex")

msummary(lagged4_models, stars = TRUE,
         coef_omit = "YEAR_FACTOR")
msummary(lagged4_models, stars = TRUE,
         coef_omit = "YEAR_FACTOR",
         output = "../TeX/assets/models_lagged4.tex")