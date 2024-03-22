## Reduce number of taxa in historical data
## Rarer taxa are combined to improve predictability

rm(list = ls())

# Load full dataset
load('data/processed/PLS/total_matched.RData')

# Make frequency table of each taxon
freq <- table(taxon_matched$species)
freq <- as.data.frame(freq)

# Plot with natural cut-off for combining species
freq |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = reorder(Var1, -Freq), 
                                 y = Freq), 
                    stat = 'identity') +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 10000)) +
  ggplot2::xlab('') + ggplot2::ylab('Frequency') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

# Group
taxon_matched <- taxon_matched |>
  # Remove two taxa that are only present in the northeast and not in the Midwest
  dplyr::filter(species != 'chestnut',
                species != 'cypress') |>
  # Group all rare taxa into other hardwood group
  dplyr::mutate(species = dplyr::if_else(species == 'alder', 'other_hardwood', species),
                species = dplyr::if_else(species == 'locust', 'other_hardwood', species),
                species = dplyr::if_else(species == 'mulberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'unknown_tree', 'other_hardwood', species),
                species = dplyr::if_else(species == 'sycamore', 'other_hardwood', species),
                species = dplyr::if_else(species == 'hackberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'cherry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'buckeye', 'other_hardwood', species),
                species = dplyr::if_else(species == 'willow', 'other_hardwood', species),
                species = dplyr::if_else(species == 'dogwood', 'other_hardwood', species),
                species = dplyr::if_else(species == 'blackgum_sweetgum', 'other_hardwood', species),
                species = dplyr::if_else(species == 'walnut', 'other_hardwood', species))

# Make a second frequency table
freq2 <- table(taxon_matched$species)
freq2 <- as.data.frame(freq2)

freq2 |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = reorder(Var1, -Freq),
                                 y = Freq),
                    stat = 'identity') +
  ggplot2::xlab('') + ggplot2::ylab('Frequency') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

# This looks much better. We will apply this to each state

# Save
save(taxon_matched, ecosystem_matched, file = 'data/processed/PLS/total_matched.RData')

## Individual states

# Illinois
load('data/processed/PLS/illinois_matched.RData')

# Make frequency table of each taxon
freq <- table(illinois_matched$species)
freq <- as.data.frame(freq)

# Plot with natural cut-off for combining species
freq |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = reorder(Var1, -Freq), 
                                 y = Freq), 
                    stat = 'identity') +
  ggplot2::xlab('') + ggplot2::ylab('Frequency') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

# Group
illinois_matched <- illinois_matched |>
  # Remove two taxa that are only present in the northeast and not in the Midwest
  dplyr::filter(species != 'chestnut',
                species != 'cypress') |>
  # Group all rare taxa into other hardwood group
  dplyr::mutate(species = dplyr::if_else(species == 'alder', 'other_hardwood', species),
                species = dplyr::if_else(species == 'locust', 'other_hardwood', species),
                species = dplyr::if_else(species == 'mulberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'unknown_tree', 'other_hardwood', species),
                species = dplyr::if_else(species == 'sycamore', 'other_hardwood', species),
                species = dplyr::if_else(species == 'hackberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'cherry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'buckeye', 'other_hardwood', species),
                species = dplyr::if_else(species == 'willow', 'other_hardwood', species),
                species = dplyr::if_else(species == 'dogwood', 'other_hardwood', species),
                species = dplyr::if_else(species == 'blackgum_sweetgum', 'other_hardwood', species),
                species = dplyr::if_else(species == 'walnut', 'other_hardwood', species))

# Make a second frequency table
freq2 <- table(illinois_matched$species)
freq2 <- as.data.frame(freq2)

freq2 |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = reorder(Var1, -Freq),
                                 y = Freq),
                    stat = 'identity') +
  ggplot2::xlab('') + ggplot2::ylab('Frequency') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

# Save
save(illinois_matched, illinois_ecosystem_matched, file = 'data/processed/PLS/illinois_matched.RData')

# Indiana
load('data/processed/PLS/indiana_matched.RData')

# Make frequency table of each taxon
freq <- table(indiana_matched$species)
freq <- as.data.frame(freq)

# Plot with natural cut-off for combining species
freq |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = reorder(Var1, -Freq), 
                                 y = Freq), 
                    stat = 'identity') +
  ggplot2::xlab('') + ggplot2::ylab('Frequency') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

# Group
indiana_matched <- indiana_matched |>
  # Remove two taxa that are only present in the northeast and not in the Midwest
  dplyr::filter(species != 'chestnut',
                species != 'cypress') |>
  # Group all rare taxa into other hardwood group
  dplyr::mutate(species = dplyr::if_else(species == 'alder', 'other_hardwood', species),
                species = dplyr::if_else(species == 'locust', 'other_hardwood', species),
                species = dplyr::if_else(species == 'mulberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'unknown_tree', 'other_hardwood', species),
                species = dplyr::if_else(species == 'sycamore', 'other_hardwood', species),
                species = dplyr::if_else(species == 'hackberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'cherry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'buckeye', 'other_hardwood', species),
                species = dplyr::if_else(species == 'willow', 'other_hardwood', species),
                species = dplyr::if_else(species == 'dogwood', 'other_hardwood', species),
                species = dplyr::if_else(species == 'blackgum_sweetgum', 'other_hardwood', species),
                species = dplyr::if_else(species == 'walnut', 'other_hardwood', species))

# Make a second frequency table
freq2 <- table(indiana_matched$species)
freq2 <- as.data.frame(freq2)

freq2 |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = reorder(Var1, -Freq),
                                 y = Freq),
                    stat = 'identity') +
  ggplot2::xlab('') + ggplot2::ylab('Frequency') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

# This looks much better. We will apply this to each state

# Save
save(indiana_matched, indiana_ecosystem_matched, file = 'data/processed/PLS/indiana_matched.RData')

# Michigan
load('data/processed/PLS/lowmichigan_matched.RData')

# Make frequency table of each taxon
freq <- table(lowmichigan_matched$species)
freq <- as.data.frame(freq)

# Plot with natural cut-off for combining species
freq |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = reorder(Var1, -Freq), 
                                 y = Freq), 
                    stat = 'identity') +
  ggplot2::xlab('') + ggplot2::ylab('Frequency') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

# Group
lowmichigan_matched <- lowmichigan_matched |>
  # Remove two taxa that are only present in the northeast and not in the Midwest
  dplyr::filter(species != 'chestnut',
                species != 'cypress') |>
  # Group all rare taxa into other hardwood group
  dplyr::mutate(species = dplyr::if_else(species == 'alder', 'other_hardwood', species),
                species = dplyr::if_else(species == 'locust', 'other_hardwood', species),
                species = dplyr::if_else(species == 'mulberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'unknown_tree', 'other_hardwood', species),
                species = dplyr::if_else(species == 'sycamore', 'other_hardwood', species),
                species = dplyr::if_else(species == 'hackberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'cherry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'buckeye', 'other_hardwood', species),
                species = dplyr::if_else(species == 'willow', 'other_hardwood', species),
                species = dplyr::if_else(species == 'dogwood', 'other_hardwood', species),
                species = dplyr::if_else(species == 'blackgum_sweetgum', 'other_hardwood', species),
                species = dplyr::if_else(species == 'walnut', 'other_hardwood', species))

# Make a second frequency table
freq2 <- table(taxon_matched$species)
freq2 <- as.data.frame(freq2)

freq2 |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = reorder(Var1, -Freq),
                                 y = Freq),
                    stat = 'identity') +
  ggplot2::xlab('') + ggplot2::ylab('Frequency') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

# This looks much better. We will apply this to each state

# Save
save(lowmichigan_matched, lowmichigan_ecosystem_matched, file = 'data/processed/PLS/lowmichigan_matched.RData')

load('data/processed/PLS/upmichigan_matched.RData')

# Make frequency table of each taxon
freq <- table(upmichigan_matched$species)
freq <- as.data.frame(freq)

# Plot with natural cut-off for combining species
freq |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = reorder(Var1, -Freq), 
                                 y = Freq), 
                    stat = 'identity') +
  ggplot2::xlab('') + ggplot2::ylab('Frequency') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

# Group
upmichigan_matched <- upmichigan_matched |>
  # Remove two taxa that are only present in the northeast and not in the Midwest
  dplyr::filter(species != 'chestnut',
                species != 'cypress') |>
  # Group all rare taxa into other hardwood group
  dplyr::mutate(species = dplyr::if_else(species == 'alder', 'other_hardwood', species),
                species = dplyr::if_else(species == 'locust', 'other_hardwood', species),
                species = dplyr::if_else(species == 'mulberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'unknown_tree', 'other_hardwood', species),
                species = dplyr::if_else(species == 'sycamore', 'other_hardwood', species),
                species = dplyr::if_else(species == 'hackberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'cherry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'buckeye', 'other_hardwood', species),
                species = dplyr::if_else(species == 'willow', 'other_hardwood', species),
                species = dplyr::if_else(species == 'dogwood', 'other_hardwood', species),
                species = dplyr::if_else(species == 'blackgum_sweetgum', 'other_hardwood', species),
                species = dplyr::if_else(species == 'walnut', 'other_hardwood', species))

# Make a second frequency table
freq2 <- table(upmichigan_matched$species)
freq2 <- as.data.frame(freq2)

freq2 |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = reorder(Var1, -Freq),
                                 y = Freq),
                    stat = 'identity') +
  ggplot2::xlab('') + ggplot2::ylab('Frequency') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

# This looks much better. We will apply this to each state

# Save
save(upmichigan_matched, upmichigan_ecosystem_matched, file = 'data/processed/PLS/upmichigan_matched.RData')

# Minnesota
load('data/processed/PLS/minnesota_matched.RData')

# Make frequency table of each taxon
freq <- table(minnesota_matched$species)
freq <- as.data.frame(freq)

# Plot with natural cut-off for combining species
freq |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = reorder(Var1, -Freq), 
                                 y = Freq), 
                    stat = 'identity') +
  ggplot2::xlab('') + ggplot2::ylab('Frequency') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

# Group
minnesota_matched <- minnesota_matched |>
  # Remove two taxa that are only present in the northeast and not in the Midwest
  dplyr::filter(species != 'chestnut',
                species != 'cypress') |>
  # Group all rare taxa into other hardwood group
  dplyr::mutate(species = dplyr::if_else(species == 'alder', 'other_hardwood', species),
                species = dplyr::if_else(species == 'locust', 'other_hardwood', species),
                species = dplyr::if_else(species == 'mulberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'unknown_tree', 'other_hardwood', species),
                species = dplyr::if_else(species == 'sycamore', 'other_hardwood', species),
                species = dplyr::if_else(species == 'hackberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'cherry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'buckeye', 'other_hardwood', species),
                species = dplyr::if_else(species == 'willow', 'other_hardwood', species),
                species = dplyr::if_else(species == 'dogwood', 'other_hardwood', species),
                species = dplyr::if_else(species == 'blackgum_sweetgum', 'other_hardwood', species),
                species = dplyr::if_else(species == 'walnut', 'other_hardwood', species))

# Make a second frequency table
freq2 <- table(minnesota_matched$species)
freq2 <- as.data.frame(freq2)

freq2 |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = reorder(Var1, -Freq),
                                 y = Freq),
                    stat = 'identity') +
  ggplot2::xlab('') + ggplot2::ylab('Frequency') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

# This looks much better. We will apply this to each state

# Save
save(minnesota_matched, minnesota_ecosystem_matched, file = 'data/processed/PLS/minnesota_matched.RData')

# Wisconsin
load('data/processed/PLS/wisconsin_matched.RData')

# Make frequency table of each taxon
freq <- table(wisconsin_matched$species)
freq <- as.data.frame(freq)

# Plot with natural cut-off for combining species
freq |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = reorder(Var1, -Freq), 
                                 y = Freq), 
                    stat = 'identity') +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 10000)) +
  ggplot2::xlab('') + ggplot2::ylab('Frequency') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

# Group
wisconsin_matched <- wisconsin_matched |>
  # Remove two taxa that are only present in the northeast and not in the Midwest
  dplyr::filter(species != 'chestnut',
                species != 'cypress') |>
  # Group all rare taxa into other hardwood group
  dplyr::mutate(species = dplyr::if_else(species == 'alder', 'other_hardwood', species),
                species = dplyr::if_else(species == 'locust', 'other_hardwood', species),
                species = dplyr::if_else(species == 'mulberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'unknown_tree', 'other_hardwood', species),
                species = dplyr::if_else(species == 'sycamore', 'other_hardwood', species),
                species = dplyr::if_else(species == 'hackberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'cherry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'buckeye', 'other_hardwood', species),
                species = dplyr::if_else(species == 'willow', 'other_hardwood', species),
                species = dplyr::if_else(species == 'dogwood', 'other_hardwood', species),
                species = dplyr::if_else(species == 'blackgum_sweetgum', 'other_hardwood', species),
                species = dplyr::if_else(species == 'walnut', 'other_hardwood', species))

# Make a second frequency table
freq2 <- table(wisconsin_matched$species)
freq2 <- as.data.frame(freq2)

freq2 |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = reorder(Var1, -Freq),
                                 y = Freq),
                    stat = 'identity') +
  ggplot2::xlab('') + ggplot2::ylab('Frequency') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

# This looks much better. We will apply this to each state

# Save
save(wisconsin_matched, wisconsin_ecosystem_matched, file = 'data/processed/PLS/wisconsin_matched.RData')
