# nolint start

# charger les paquets
library(epidemics)
library(socialmixr)
library(tidyverse)

# charger les données de l'enquête
survey_data <- socialmixr::polymod

# générer une matrice de contacts
cm_results <- socialmixr::contact_matrix(
  survey = survey_data,
  countries = "United Kingdom",
  age.limits = c(0, 15, 65),
  symmetric = TRUE
)

# prépare la matrice de contact
cm_matrix <- t(cm_results$matrix)

# prépare le vecteur de la démographie
demography_vector <- cm_results$demography$population
names(demography_vector) <- rownames(cm_matrix)

# conditions initiales: une personne sur un million est infecté
initial_i <- 1e-6
initial_conditions <- c(
  S = 1 - initial_i,
  E = 0,
  I = initial_i,
  R = 0,
  V = 0
)

# crée la matrice de conditions initiales pour chaque tranche d'âge
initial_conditions <- base::rbind(
  initial_conditions,
  initial_conditions,
  initial_conditions
)
rownames(initial_conditions) <- rownames(cm_matrix)

# prépare la population à modéliser comme étant touchée par l'épidémie
uk_population <- epidemics::population(
  name = "UK",
  contact_matrix = cm_matrix,
  demography_vector = demography_vector,
  initial_conditions = initial_conditions
)

# périodes de temp
preinfectious_period <- 4.0
infectious_period <- 5.5
basic_reproduction <- 2.7

# taux
infectiousness_rate <- 1.0 / preinfectious_period
recovery_rate <- 1.0 / infectious_period
transmission_rate <- basic_reproduction * recovery_rate

# exécuter une variante de base du modèle sans intervention
output_baseline <- epidemics::model_default(
  population = uk_population,
  transmission_rate = transmission_rate,
  infectiousness_rate = infectiousness_rate,
  recovery_rate = recovery_rate,
  time_end = 300, increment = 1.0
)

output_baseline

# défi ------------------------
# Exécutez cette visualisation du modèle de base à l'aide de ggplot2
# Puis partagez-la avec le tuteur

output_baseline %>%
  filter(compartment == "infectious") %>%
  ggplot(aes(
    x = time,
    y = value,
    linetype = demography_group,
    colour = compartment
  )) +
  geom_line()

# ----------------------------------

# nolint end