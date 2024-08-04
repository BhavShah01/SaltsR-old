#' @title SaltsR: Function for returning a milequivalents
#' @description
#' @param salt_ppm Salt ion data from analysis (ppm)
#' @param dry_weight Dry weight of the sample (g)
#' @param water_added Amount of weater added (ml)
#' @param mol_wts_mileq Table for molecular weights of ions (mol)
#' @return A numeric of the salt milequivelents
#' @import dplyr
#' @export
#' @examples
#' salt_mileq(salt_ppm, dry_weight, water_added, mol_wts_mileq)

salt_mileq <- function(salt_ppm, dry_weight, water_added, mol_wts_mileq) {
  salt_mileq <- (salt_ppm * water_added) / (1000 * dry_weight * mol_wts_mileq)
  return(salt_mileq)
}
