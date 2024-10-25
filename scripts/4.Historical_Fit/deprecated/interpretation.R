rm(list = ls())

taxon <- readRDS('/save/taxon_point_rf_default.RDS')

taxon_rf <- taxon$finalModel
confusion <- taxon_rf$confusion
class_error <- confusion[,33]
confusion <- confusion[,-33]

# diagonals = correct prediciton
# rows = actual
# columns = predicted

#e.g. column 1, row 2: predicted alder 19 times when actually ash

# Plot classification error rates
class_error <- as.data.frame(class_error)
class_error <- tibble::rownames_to_column(class_error, var = 'taxon')

class_error |>
  dplyr::mutate(taxon = dplyr::if_else(taxon == 'mulberry', 'Mulberry', taxon),
                taxon = dplyr::if_else(taxon == 'locust', 'Locust', taxon),
                taxon = dplyr::if_else(taxon == 'cherry', 'Cherry', taxon),
                taxon = dplyr::if_else(taxon == 'walnut', 'Walnut', taxon),
                taxon = dplyr::if_else(taxon == 'other_hardwood', 'Other hardwood', taxon),
                taxon = dplyr::if_else(taxon == 'basswood', 'Basswood', taxon),
                taxon = dplyr::if_else(taxon == 'ironwood', 'Ironwood', taxon),
                taxon = dplyr::if_else(taxon == 'chestnut', 'Chestnut', taxon),
                taxon = dplyr::if_else(taxon == 'hackberry', 'Hackberry', taxon),
                taxon = dplyr::if_else(taxon == 'dogwood', 'Dogwood', taxon),
                taxon = dplyr::if_else(taxon == 'blackgum_sweetgum', 'Black gum/\nsweet gum', taxon),
                taxon = dplyr::if_else(taxon == 'buckeye', 'Buckeye', taxon),
                taxon = dplyr::if_else(taxon == 'sycamore', 'Sycamore', taxon),
                taxon = dplyr::if_else(taxon == 'elm', 'Elm', taxon),
                taxon = dplyr::if_else(taxon == 'hickory', 'Hickory', taxon),
                taxon = dplyr::if_else(taxon == 'ash', 'Ash', taxon),
                taxon = dplyr::if_else(taxon == 'fir', 'Fir', taxon),
                taxon = dplyr::if_else(taxon == ''))
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = reorder(taxon, X = -class_error), 
                                 y = class_error), 
                    stat = 'identity') +
  ggplot2::theme_minimal() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Classification error rate') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

# Find correct prediction rates
diags <- diag(confusion)

