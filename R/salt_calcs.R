#' @title SaltsR: Computes parameters from salt input
#' @description
#' @param salt_input Table for ppm values of ions
#' @param sample_name Column contain sampple names
#' @param dry_g Column containing dry weight of the sample (g)
#' @param water_ml Column containing amount of weater added (ml)
#' @param chloride_ppm Column containing chloride (ppm)
#' @param nitrate_ppm Column containing nitrate (ppm)
#' @param sulfate_ppm Column containing sulfate (ppm)
#' @param sodium_ppm Column containing sodium (ppm)
#' @param potassium_ppm Column containing potassium (ppm)
#' @param calcium_ppm Column containing calcium (ppm)
#' @param magnesium_ppm Column containing magnesium (ppm)
#' @return A tibble of salt ion outputs
#' @import dplyr magrittr tibble
#' @export
#' @examples
#' salt_calcs(salt_input, sample_name = "X225", dry_g = 1.128, water_ml = 100, chloride_ppm = 9.982, nitrate_ppm = 23.438, sulfate_ppm = 628.65, sodium_ppm = 31.25, potassium_ppm = 105.222, calcium_ppm = 75.594, magnesium_ppm = 7.582)

salt_calcs <- function(
  salt_input, sample_name, dry_g, water_ml,
  chloride_ppm, nitrate_ppm, sulfate_ppm,
  sodium_ppm, potassium_ppm, calcium_ppm, magnesium_ppm) {
  #
  mol_wts <- tibble::tibble(
    chloride  = 35.453,
    nitrate   = 62.0037,
    sulfate   = 48.0305,
    sodium    = 22.989769,
    potassium = 39.0983,
    calcium   = 20.039,
    magnesium = 12.1525
  )
  #
  salt_input %>%
    mutate(
      sample_name   = sample_name,
      dry_g         = dry_g,
      water_ml      = water_ml,
      chloride_ppm  = chloride_ppm,
      nitrate_ppm   = nitrate_ppm,
      sulfate_ppm   = sulfate_ppm,
      sodium_ppm    = sodium_ppm,
      potassium_ppm = potassium_ppm,
      calcium_ppm   = calcium_ppm,
      magnesium_ppm = magnesium_ppm,
      #
      chloride_wt = fun_salt_wt(chloride_ppm, dry_g, water_ml),
      nitrate_wt = fun_salt_wt(nitrate_ppm, dry_g, water_ml),
      sulfate_wt = fun_salt_wt(sulfate_ppm, dry_g, water_ml),
      sodium_wt = fun_salt_wt(sodium_ppm, dry_g, water_ml),
      potassium_wt = fun_salt_wt(potassium_ppm, dry_g, water_ml),
      calcium_wt = fun_salt_wt(calcium_ppm, dry_g, water_ml),
      magnesium_wt = fun_salt_wt(magnesium_ppm, dry_g, water_ml),
      total_salt =
        chloride_wt + nitrate_wt + sulfate_wt +
        sodium_wt + potassium_wt + calcium_wt + magnesium_wt,
      #
      chloride_mEq =
        fun_salt_mileq(chloride_ppm, dry_g,
                       water_ml, mol_wts$chloride),
      nitrate_mEq =
        fun_salt_mileq(nitrate_ppm, dry_g,
                       water_ml, mol_wts$nitrate),
      sulfate_mEq =
        fun_salt_mileq(sulfate_ppm, dry_g,
                       water_ml, mol_wts$sulfate),
      sodium_mEq =
        fun_salt_mileq(sodium_ppm, dry_g,
                       water_ml, mol_wts$sodium),
      potassium_mEq =
        fun_salt_mileq(potassium_ppm, dry_g,
                       water_ml, mol_wts$potassium),
      calcium_mEq =
        fun_salt_mileq(calcium_ppm, dry_g,
                       water_ml, mol_wts$calcium),
      magnesium_mEq =
        fun_salt_mileq(magnesium_ppm, dry_g,
                       water_ml, mol_wts$magnesium),
      total_anions = chloride_mEq + nitrate_mEq + sulfate_mEq,
      total_cations =
        sodium_mEq + potassium_mEq + calcium_mEq + magnesium_mEq,
      xs_cations = total_cations - total_anions,
      ave_ions = (total_anions + total_cations) / 2,
      abs_xs_ions = abs(total_anions - total_cations),
      xs_CaSO4 =
        ifelse(calcium_mEq ==
                 max(sodium_mEq, potassium_mEq, calcium_mEq, magnesium_mEq) &
                 (calcium_mEq >= abs_xs_ions) &
                 (total_anions < total_cations),
               TRUE, FALSE),
      analytical_uncert = ifelse(
        abs_xs_ions > 0.02 * (max(total_anions, total_cations)), TRUE, FALSE),
      #
      chloride_mEq_cor =
        ifelse(analytical_uncert == TRUE, chloride_mEq,
               chloride_mEq * abs_xs_ions / total_anions),
      nitrate_mEq_cor =
        ifelse(analytical_uncert == TRUE, nitrate_mEq,
               nitrate_mEq * abs_xs_ions / total_anions),
      sulfate_mEq_cor =
        ifelse(analytical_uncert == TRUE, sulfate_mEq,
               sulfate_mEq * abs_xs_ions / total_anions),
      sodium_mEq_cor =
        ifelse(analytical_uncert == TRUE, sodium_mEq,
               sodium_mEq * abs_xs_ions / total_anions),
      potassium_mEq_cor =
        ifelse(analytical_uncert == TRUE, potassium_mEq,
               potassium_mEq * abs_xs_ions / total_anions),
      calcium_mEq_cor =
        ifelse(analytical_uncert == TRUE, calcium_mEq - abs_xs_ions,
               calcium_mEq * ave_ions / total_cations),
      magnesium_mEq_cor =
        ifelse(analytical_uncert == TRUE, magnesium_mEq,
               magnesium_mEq * abs_xs_ions / total_anions),
      #
      ion_check_chloride =
        ifelse(total_anions > total_cations &
                 chloride_mEq_cor > nitrate_mEq_cor &
                 chloride_mEq_cor > sulfate_mEq_cor, TRUE, FALSE),
      ion_check_nitrate =
        ifelse(total_anions > total_cations &
                 nitrate_mEq_cor > chloride_mEq_cor &
                 nitrate_mEq_cor > sulfate_mEq_cor, TRUE, FALSE),
      ion_check_sulfate =
        ifelse(total_anions > total_cations &
                 sulfate_mEq_cor > chloride_mEq_cor &
                 sulfate_mEq_cor > nitrate_mEq_cor, TRUE, FALSE),
      ion_check_sodium =
        ifelse(total_cations > total_anions &
                 sodium_mEq_cor > potassium_mEq_cor &
                 sodium_mEq_cor > calcium_mEq_cor &
                 sodium_mEq_cor > magnesium_mEq_cor, TRUE, FALSE),
      ion_check_potassium =
        ifelse(total_cations > total_anions &
                 potassium_mEq_cor > sodium_mEq_cor &
                 potassium_mEq_cor > calcium_mEq_cor &
                 potassium_mEq_cor > magnesium_mEq_cor, TRUE, FALSE),
      ion_check_calcium =
        ifelse(total_cations > total_anions &
                 calcium_mEq_cor > sodium_mEq_cor &
                 calcium_mEq_cor > magnesium_mEq_cor &
                 calcium_mEq_cor > potassium_mEq_cor, TRUE, FALSE),
      ion_check_magnesium =
        ifelse(total_cations > total_anions &
                 magnesium_mEq_cor > sodium_mEq_cor &
                 magnesium_mEq_cor > calcium_mEq_cor &
                 magnesium_mEq_cor > potassium_mEq_cor, TRUE, FALSE),
      #
      chloride_mEq_SO4 =
        ifelse(total_anions > total_cations &
                 ave_ions > 0 & ion_check_chloride == TRUE,
               chloride_mEq_cor - ave_ions, chloride_mEq_cor),
      nitrate_mEq_SO4 =
        ifelse(total_anions > total_cations &
                 ave_ions > 0 & ion_check_nitrate == TRUE,
               nitrate_mEq_cor - ave_ions, nitrate_mEq_cor),
      sulfate_mEq_SO4 =
        ifelse(total_anions > total_cations &
                 ave_ions > 0 & ion_check_sulfate == TRUE,
               sulfate_mEq_cor - ave_ions, sulfate_mEq_cor),
      sodium_mEq_SO4 =
        ifelse(total_anions > total_cations &
                 ave_ions > 0 & ion_check_sodium == TRUE,
               sodium_mEq_cor - ave_ions, sodium_mEq_cor),
      potassium_mEq_SO4 =
        ifelse(total_anions > total_cations &
                 ave_ions > 0 & ion_check_potassium == TRUE,
               potassium_mEq_cor - ave_ions, potassium_mEq_cor),
      calcium_mEq_SO4 =
        ifelse(total_anions > total_cations &
                 ave_ions > 0 & ion_check_calcium == TRUE,
               calcium_mEq_cor - ave_ions, calcium_mEq_cor),
      magnesium_mEq_SO4 =
        ifelse(total_anions > total_cations &
                 ave_ions > 0 & ion_check_magnesium == TRUE,
               magnesium_mEq_cor - ave_ions, magnesium_mEq_cor),
      #
      total_anions_cor = chloride_mEq_SO4 + nitrate_mEq_SO4 + sulfate_mEq_SO4,
      total_cations_cor =
        sodium_mEq_SO4 + potassium_mEq_SO4 + calcium_mEq_SO4 + magnesium_mEq_SO4,
      abs_xs_ions_cor = abs(round(total_anions_cor - total_cations_cor, 6)),
      sulfate_mEq_SO4cor =
        sulfate_mEq_SO4 /
        (water_ml /  (dry_g * (48.305))), # (mol_wts$sulfate))), #
      # mol_wts$sulfate # in spreadsheet: 48.305
      sulfate_wt_cor =
        (sulfate_mEq_SO4cor * water_ml) / (10000 * dry_g),
      #
      chloride_mEq_corSO4 = chloride_mEq_SO4,
      nitrate_mEq_corSO4 = nitrate_mEq_SO4,
      sulfate_mEq_corSO4 = sulfate_mEq_SO4cor, # Corrected
      sodium_mEq_corSO4 = sodium_mEq_SO4,
      potassium_mEq_corSO4 = potassium_mEq_SO4,
      calcium_mEq_corSO4 = calcium_mEq_SO4,
      magnesium_mEq_corSO4 = magnesium_mEq_SO4,
      #
      sulfate_diff = ifelse(
        calcium_mEq_SO4 > sulfate_mEq_SO4, 0, calcium_mEq_SO4 - sulfate_mEq_SO4),
      calcium_diff = ifelse(
        calcium_mEq_SO4 > sulfate_mEq_SO4, calcium_mEq_SO4 - sulfate_mEq_SO4, 0),
      theoretical_CaSO4 =
        ifelse(sulfate_mEq_SO4 == min(calcium_mEq_SO4, sulfate_mEq_SO4),
               sulfate_mEq_SO4 * 2, calcium_mEq_SO4 * 2),
      #
      sulfate_ppm_cor =
        sulfate_diff / (water_ml / (dry_g * mol_wts$sulfate)),
      calcium_ppm_cor =
        calcium_diff / (water_ml / (dry_g * mol_wts$calcium)),
      sulfate_mEq_corCaSO4 = sulfate_mEq_SO4 - sulfate_diff,
      calcium_mEq_corCaSO4 = calcium_mEq_SO4 - calcium_diff,
      sulfate_ppm_corCaSO4 =
        sulfate_mEq_corCaSO4 / (water_ml / (dry_g * mol_wts$sulfate)),
      calcium_ppm_corCaSO4 =
        calcium_mEq_corCaSO4 / (water_ml / (dry_g * mol_wts$calcium)),
      Gypsum_wt =
        (sulfate_ppm_corCaSO4 * water_ml) / (10000 * dry_g) +
        (calcium_ppm_corCaSO4 * water_ml) / (10000 * dry_g),
      Total_anions =
        round(chloride_mEq_corSO4 + nitrate_mEq_corSO4 + sulfate_diff, 6),
      Total_cations =
        round(sodium_mEq_corSO4 + potassium_mEq_corSO4 +
                calcium_diff + magnesium_mEq_corSO4, 6),
      XS_ions = abs(Total_anions - Total_cations),
      #
      Chloride_ppm =
        chloride_mEq_corSO4 / (water_ml / (dry_g * mol_wts$chloride)),
      Nitrate_ppm =
        nitrate_mEq_corSO4 / (water_ml / (dry_g * mol_wts$nitrate)),
      Sulfate_ppm = sulfate_ppm_cor,
      # sulfate_mEq_corSO4 / (water_ml / (dry_g * mol_wts$sulfate)),
      Sodium_ppm =
        sodium_mEq_corSO4 / (water_ml / (dry_g * mol_wts$sodium)),
      Potassium_ppm =
        potassium_mEq_corSO4 / (water_ml / (dry_g * mol_wts$potassium)),
      Calcium_ppm =
        calcium_mEq_corSO4 / (water_ml / (dry_g * mol_wts$calcium)),
      Magnesium_ppm =
        magnesium_mEq_corSO4 / (water_ml / (dry_g * mol_wts$magnesium)),
      #
      chloride_eqkg_SO4 = chloride_mEq_SO4 / total_anions_cor * 100,
      nitrate_eqkg_SO4 = nitrate_mEq_SO4 / total_anions_cor * 100,
      sulfate_eqkg_SO4 = sulfate_mEq_SO4 / total_anions_cor * 100,
      sodium_eqkg_SO4 = sodium_mEq_SO4 / total_cations_cor * 100,
      potassium_eqkg_SO4 = potassium_mEq_SO4 / total_cations_cor * 100,
      calcium_eqkg_SO4 = calcium_mEq_SO4 / total_cations_cor * 100,
      magnesium_eqkg_SO4 = magnesium_mEq_SO4 / total_cations_cor * 100,
      #
      chloride_eqkg_CaSO4 = chloride_mEq_corSO4 / Total_anions * 100,
      nitrate_eqkg_CaSO4 = nitrate_mEq_corSO4 / Total_anions * 100,
      sulfate_eqkg_CaSO4 = sulfate_diff / Total_anions * 100, # Corrected
      sodium_eqkg_CaSO4 = sodium_mEq_corSO4 / Total_cations * 100,
      potassium_eqkg_CaSO4 = potassium_mEq_corSO4 / Total_cations * 100,
      calcium_eqkg_CaSO4 = calcium_diff / Total_cations * 100, # Corrected
      magnesium_eqkg_CaSO4 = magnesium_mEq_corSO4 / Total_cations * 100,
      #
      total_eqkg = sum(
        chloride_eqkg_CaSO4, nitrate_eqkg_CaSO4, sulfate_eqkg_CaSO4,
        sodium_eqkg_CaSO4, potassium_eqkg_CaSO4, calcium_eqkg_CaSO4,
        magnesium_eqkg_CaSO4),
      #
      Chloride_MMOLkg = chloride_mEq_corSO4,
      Nitrate_MMOLkg = nitrate_mEq_corSO4,
      Sulfate_MMOLkg = sulfate_diff / 2,
      Sodium_MMOLkg = sodium_mEq_corSO4,
      Potassium_MMOLkg = potassium_mEq_corSO4,
      Calcium_MMOLkg = calcium_diff / 2,
      Magnesium_MMOLkg = magnesium_mEq_corSO4 / 2,
      #
      total_anions_MMOLkg =
        Chloride_MMOLkg + Nitrate_MMOLkg + (Sulfate_MMOLkg * 2),
      total_cations_MMOLkg =
        Sodium_MMOLkg + Potassium_MMOLkg +
        (Calcium_MMOLkg * 2) + (Magnesium_MMOLkg * 2),
      diff_MMOLkg = round(total_anions_MMOLkg - total_cations_MMOLkg, 4),
      #
      Chloride_MOLkg =
        ifelse(total_anions_MMOLkg > total_cations_MMOLkg &
                 Chloride_MMOLkg ==
                 max(Chloride_MMOLkg, Nitrate_MMOLkg, Sulfate_MMOLkg),
               Chloride_MMOLkg - diff_MMOLkg, Chloride_MMOLkg) / 1000,
      Nitrate_MOLkg =
        ifelse(total_anions_MMOLkg > total_cations_MMOLkg &
                 Nitrate_MMOLkg ==
                 max(Chloride_MMOLkg, Nitrate_MMOLkg, Sulfate_MMOLkg),
               Nitrate_MMOLkg - diff_MMOLkg, Nitrate_MMOLkg) / 1000,
      Sulfate_MOLkg =
        ifelse(total_anions_MMOLkg > total_cations_MMOLkg &
                 Sulfate_MMOLkg ==
                 max(Chloride_MMOLkg, Nitrate_MMOLkg, Sulfate_MMOLkg),
               Sulfate_MMOLkg - diff_MMOLkg, Sulfate_MMOLkg) / 1000,
      Sodium_MOLkg =
        ifelse(total_cations_MMOLkg > total_anions_MMOLkg &
                 Sodium_MMOLkg ==
                 max(Sodium_MMOLkg, Potassium_MMOLkg,
                     Calcium_MMOLkg, Magnesium_MMOLkg),
               Sodium_MMOLkg - diff_MMOLkg, Sodium_MMOLkg) / 1000,
      Potassium_MOLkg =
        ifelse(total_cations_MMOLkg > total_anions_MMOLkg &
                 Sodium_MMOLkg ==
                 max(Sodium_MMOLkg, Potassium_MMOLkg,
                     Calcium_MMOLkg, Magnesium_MMOLkg),
               Potassium_MMOLkg - diff_MMOLkg, Potassium_MMOLkg) / 1000,
      Calcium_MOLkg =
        ifelse(total_cations_MMOLkg > total_anions_MMOLkg &
                 Calcium_MMOLkg ==
                 max(Sodium_MMOLkg, Potassium_MMOLkg,
                     Calcium_MMOLkg, Magnesium_MMOLkg),
               Calcium_MMOLkg - diff_MMOLkg, Calcium_MMOLkg) / 1000,
      Magnesium_MOLkg =
        ifelse(total_cations_MMOLkg > total_anions_MMOLkg &
                 Magnesium_MMOLkg ==
                 max(Sodium_MMOLkg, Potassium_MMOLkg,
                     Calcium_MMOLkg, Magnesium_MMOLkg),
               Magnesium_MMOLkg - diff_MMOLkg, Magnesium_MMOLkg) / 1000,
      #
      total_anions_MOLkg =
        Chloride_MOLkg + Nitrate_MOLkg + (Sulfate_MOLkg * 2),
      total_cations_MOLkg =
        Sodium_MOLkg + Potassium_MOLkg +
        (Calcium_MOLkg * 2) + (Magnesium_MOLkg * 2),
      diff_MOLkg = round(total_anions_MOLkg - total_cations_MOLkg, 4),
      #
      Chloride_PPM = Chloride_ppm,
      Nitrate_PPM = Nitrate_ppm,
      Sulfate_PPM = sulfate_ppm_cor, # Corrected
      Sodium_PPM = Sodium_ppm,
      Potassium_PPM = Potassium_ppm,
      Calcium_PPM = calcium_ppm_cor, # Corrected
      Magnesium_PPM = Magnesium_ppm,
      #
      Chloride_Wt = ((Chloride_PPM * water_ml) / (10000 * dry_g)),
      Nitrate_Wt = ((Nitrate_PPM * water_ml) / (10000 * dry_g)),
      Sulfate_Wt = ((Sulfate_PPM * water_ml) / (10000 * dry_g)),
      Sodium_Wt = ((Sodium_PPM * water_ml) / (10000 * dry_g)),
      Potassium_Wt = ((Potassium_PPM * water_ml) / (10000 * dry_g)),
      Calcium_Wt = ((Calcium_PPM * water_ml) / (10000 * dry_g)),
      Magnesium_Wt = ((Magnesium_PPM * water_ml) / (10000 * dry_g)),
      #
      total_Wt = Chloride_Wt + Nitrate_Wt + Sulfate_Wt +
        Sodium_Wt + Potassium_Wt + Calcium_Wt + Magnesium_Wt,
      total_gypsum = total_salt - Gypsum_wt,
      total_diff = total_salt - total_gypsum,
      #
      Chloride_ECOS = Chloride_MOLkg / total_anions_MOLkg * 100,
      Nitrate_ECOS = Nitrate_MOLkg / total_anions_MOLkg * 100,
      Sulfate_ECOS = Sulfate_MOLkg / total_anions_MOLkg * 100,
      Sodium_ECOS = Sodium_MOLkg / total_cations_MOLkg * 100,
      Potassium_ECOS = Potassium_MOLkg / total_cations_MOLkg * 100,
      Calcium_ECOS = Calcium_MOLkg / total_cations_MOLkg * 100,
      Magnesium_ECOS = Magnesium_MOLkg / total_cations_MOLkg * 100,
      #
      ECOS_total_anions =
        Chloride_ECOS + Nitrate_ECOS + Sulfate_ECOS * 2,
      ECOS_total_cations =
        Sodium_ECOS + Potassium_ECOS + Calcium_ECOS * 2 + Magnesium_ECOS * 2,
      ECOS_diff = abs(ECOS_total_anions - ECOS_total_cations),
      ECOS_sample_name = sample_name
    )
}
