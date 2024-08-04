# BENV0117 Dissertation 
# 
library(tidyverse); library(DT); library(lubridate); library(viridis)
library(leaflet); library(jsonlite); library(ggrepel); library(ggforce)
# library(janitor); # library(gt)
library(openair); library(worldmet)
library(plotly)

library(readr); library(readxl); library(vroom); library(writexl)
# Salt_data_input <- read_csv("~/ALL SENSORS/UCL DaSCH/BENV0117 Dissertation/BENV0117 Dissertation/input/Salt_data_input.csv")
# salt_input_template <- readxl::read_excel("~/ALL SENSORS/UCL DaSCH/BENV0117 Dissertation/BENV0117 Dissertation/input/Salt_input_template.xlsx", sheet = "test")

# Constants ----
#
# Ion molecular weights ----
# mol_wts <- tibble::tibble(
#   chloride_wt  = 35.45,
#   nitrate_wt   = 62.01,
#   sulfate_wt   = 96.06, # 48.03, # for mileq
#   sodium_wt    = 22.99,
#   potassium_wt = 39.098, 
#   calcium_wt   = 40.08, # 20.04, # for mileq
#   magnesium_wt = 24.31
# )
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

# Analysis ----
#
salt_test <- tibble(
  sample_name   = "test",   # "test2",
  dry_g         = 1.128,    # Dry sample weight for IC (g)
  water_ml      = 100,      # Amount of water added to sample for IC (ml)
  chloride_ppm  = 66.824,   # CHLORIDE (Cl-) (PPM)
  nitrate_ppm   = 332.956,  # NITRATE (NO3-) (PPM)
  sulfate_ppm   = 87.221,   # SULFATE (SO42-) (PPM)
  sodium_ppm    = 21.471,   # SODIUM (Na+) (PPM)
  potassium_ppm = 211.358,  # POTASSIUM (K+) (PPM)
  calcium_ppm   = 75.594,   # CALCIUM (Ca2+) (PPM)
  magnesium_ppm = 7.582,    # MAGNISIUM (Mg2+) (PPM)
  #
  # sample_name   = "test2",
  # dry_g = 1,
  # water_ml  = 100,
  # chloride_ppm  = 9.982,    # CHLORIDE (Cl-) (PPM)
  # nitrate_ppm   = 23.438,   # NITRATE (NO3-) (PPM)
  # sulfate_ppm   = 628.65,   # SULFATE (SO42-) (PPM)
  # sodium_ppm    = 31.25,    # SODIUM (Na+) (PPM)
  # potassium_ppm = 105.222,  # POTASSIUM (K+) (PPM)
  # calcium_ppm   = 200.272,  # CALCIUM (Ca2+) (PPM)
  # magnesium_ppm = 3.92      # MAGNISIUM (Mg2+) (PPM)
)
# 
# salt_test %>% select(ends_with("ppm")) %>%
#   fun_salt_wt(dry_g, water_ml) # %>% fun_total_salt() ; 8.88948581560284
# #
# salt_test %>% select(ends_with("ppm")) %>%
#   fun_salt_mol(water_ml, mol_wts)
# #
# salt_test %>% select(ends_with("ppm")) %>%
#   fun_salt_mileq(dry_g, water_ml, mol_wts)
# #
# salt_test %>% fun_salt_calcs() %>% glimpse()

# Functions ----

fun_salt_calcs <- function(
  salt_input, sample_name, dry_g, water_ml,
  chloride_ppm, nitrate_ppm, sulfate_ppm,
  sodium_ppm, potassium_ppm, calcium_ppm, magnesium_ppm) {
  #
  salt_input %>%
    mutate(
      sample_name   = sample_name,
      dry_g = dry_g,
      water_ml  = water_ml,
      chloride_ppm  = chloride_ppm,
      nitrate_ppm   = nitrate_ppm,
      sulfate_ppm   = sulfate_ppm,
      sodium_ppm    = sodium_ppm,
      potassium_ppm = potassium_ppm,
      calcium_ppm   = calcium_ppm,
      magnesium_ppm = magnesium_ppm,
      # chloride_wt, nitrate_wt, sulfate_wt, sodium_wt, potassium_wt, calcium_wt, magnesium_wt, total_salt_wt
      Chloride_wt = fun_salt_wt(chloride_ppm, dry_g, water_ml),
      Nitrate_wt = fun_salt_wt(nitrate_ppm, dry_g, water_ml),
      Sulfate_wt = fun_salt_wt(sulfate_ppm, dry_g, water_ml),
      Sodium_wt = fun_salt_wt(sodium_ppm, dry_g, water_ml),
      Potassium_wt = fun_salt_wt(potassium_ppm, dry_g, water_ml),
      Calcium_wt = fun_salt_wt(calcium_ppm, dry_g, water_ml),
      Magnesium_wt = fun_salt_wt(magnesium_ppm, dry_g, water_ml),
      total_salt_wt =
        Chloride_wt + Nitrate_wt + Sulfate_wt +
        Sodium_wt + Potassium_wt + Calcium_wt + Magnesium_wt,
      #
      chloride_mEq = fun_salt_mileq(chloride_ppm, dry_g, water_ml, mol_wts$chloride),
      nitrate_mEq = fun_salt_mileq(nitrate_ppm, dry_g, water_ml, mol_wts$nitrate),
      sulfate_mEq = fun_salt_mileq(sulfate_ppm, dry_g, water_ml, mol_wts$sulfate),
      sodium_mEq = fun_salt_mileq(sodium_ppm, dry_g, water_ml, mol_wts$sodium),
      potassium_mEq = fun_salt_mileq(potassium_ppm, dry_g, water_ml, mol_wts$potassium),
      calcium_mEq = fun_salt_mileq(calcium_ppm, dry_g,  water_ml, mol_wts$calcium),
      magnesium_mEq = fun_salt_mileq(magnesium_ppm, dry_g, water_ml, mol_wts$magnesium),
      total_anions_mEq = chloride_mEq + nitrate_mEq + sulfate_mEq,
      total_cations_mEq = sodium_mEq + potassium_mEq + calcium_mEq + magnesium_mEq,
      xs_cations_mEq = total_cations_mEq - total_anions_mEq,
      ave_ions_mEq = (total_anions_mEq + total_cations_mEq) / 2,
      abs_xs_ions_mEq = abs(total_anions_mEq - total_cations_mEq),
      xs_CaSO4_mEq =
        ifelse(calcium_mEq ==
                 max(sodium_mEq, potassium_mEq, calcium_mEq, magnesium_mEq) &
                 (calcium_mEq >= abs_xs_ions_mEq) &
                 (total_anions_mEq < total_cations_mEq),
               TRUE, FALSE),
      analytical_uncert = ifelse(
        abs_xs_ions_mEq > 0.02 * (max(total_anions_mEq, total_cations_mEq)), TRUE, FALSE),
      #
      chloride_mEq_cor =
        ifelse(analytical_uncert == TRUE, chloride_mEq,
               chloride_mEq * abs_xs_ions_mEq / total_anions_mEq),
      nitrate_mEq_cor =
        ifelse(analytical_uncert == TRUE, nitrate_mEq,
               nitrate_mEq * abs_xs_ions_mEq / total_anions_mEq),
      sulfate_mEq_cor =
        ifelse(analytical_uncert == TRUE, sulfate_mEq,
               sulfate_mEq * abs_xs_ions_mEq / total_anions_mEq),
      sodium_mEq_cor =
        ifelse(analytical_uncert == TRUE, sodium_mEq,
               sodium_mEq * abs_xs_ions_mEq / total_anions_mEq),
      potassium_mEq_cor =
        ifelse(analytical_uncert == TRUE, potassium_mEq,
               potassium_mEq * abs_xs_ions_mEq / total_anions_mEq),
      calcium_mEq_cor =
        ifelse(analytical_uncert == TRUE, calcium_mEq - abs_xs_ions_mEq,
               calcium_mEq * ave_ions_mEq / total_cations_mEq),
      magnesium_mEq_cor =
        ifelse(analytical_uncert == TRUE, magnesium_mEq,
               magnesium_mEq * abs_xs_ions_mEq / total_anions_mEq),
      #
      ion_check_chloride =
        ifelse(total_anions_mEq > total_cations_mEq &
                 chloride_mEq_cor > nitrate_mEq_cor &
                 chloride_mEq_cor > sulfate_mEq_cor, TRUE, FALSE),
      ion_check_nitrate =
        ifelse(total_anions_mEq > total_cations_mEq &
                 nitrate_mEq_cor > chloride_mEq_cor &
                 nitrate_mEq_cor > sulfate_mEq_cor, TRUE, FALSE),
      ion_check_sulfate =
        ifelse(total_anions_mEq > total_cations_mEq &
                 sulfate_mEq_cor > chloride_mEq_cor &
                 sulfate_mEq_cor > nitrate_mEq_cor, TRUE, FALSE),
      ion_check_sodium =
        ifelse(total_cations_mEq > total_anions_mEq &
                 sodium_mEq_cor > potassium_mEq_cor &
                 sodium_mEq_cor > calcium_mEq_cor &
                 sodium_mEq_cor > magnesium_mEq_cor, TRUE, FALSE),
      ion_check_potassium =
        ifelse(total_cations_mEq > total_anions_mEq &
                 potassium_mEq_cor > sodium_mEq_cor &
                 potassium_mEq_cor > calcium_mEq_cor &
                 potassium_mEq_cor > magnesium_mEq_cor, TRUE, FALSE),
      ion_check_calcium =
        ifelse(total_cations_mEq > total_anions_mEq &
                 calcium_mEq_cor > sodium_mEq_cor &
                 calcium_mEq_cor > magnesium_mEq_cor &
                 calcium_mEq_cor > potassium_mEq_cor, TRUE, FALSE),
      ion_check_magnesium =
        ifelse(total_cations_mEq > total_anions_mEq &
                 magnesium_mEq_cor > sodium_mEq_cor &
                 magnesium_mEq_cor > calcium_mEq_cor &
                 magnesium_mEq_cor > potassium_mEq_cor, TRUE, FALSE),
      #
      chloride_mEq_SO4 =
        ifelse(total_anions_mEq > total_cations_mEq &
                 ave_ions_mEq > 0 & ion_check_chloride == TRUE,
               chloride_mEq_cor - ave_ions_mEq, chloride_mEq_cor),
      nitrate_mEq_SO4 =
        ifelse(total_anions_mEq > total_cations_mEq &
                 ave_ions_mEq > 0 & ion_check_nitrate == TRUE,
               nitrate_mEq_cor - ave_ions_mEq, nitrate_mEq_cor),
      sulfate_mEq_SO4 =
        ifelse(total_anions_mEq > total_cations_mEq &
                 ave_ions_mEq > 0 & ion_check_sulfate == TRUE,
               sulfate_mEq_cor - ave_ions_mEq, sulfate_mEq_cor),
      sodium_mEq_SO4 =
        ifelse(total_anions_mEq > total_cations_mEq &
                 ave_ions_mEq > 0 & ion_check_sodium == TRUE,
               sodium_mEq_cor - ave_ions_mEq, sodium_mEq_cor),
      potassium_mEq_SO4 =
        ifelse(total_anions_mEq > total_cations_mEq &
                 ave_ions_mEq > 0 & ion_check_potassium == TRUE,
               potassium_mEq_cor - ave_ions_mEq, potassium_mEq_cor),
      calcium_mEq_SO4 =
        ifelse(total_anions_mEq > total_cations_mEq &
                 ave_ions_mEq > 0 & ion_check_calcium == TRUE,
               calcium_mEq_cor - ave_ions_mEq, calcium_mEq_cor),
      magnesium_mEq_SO4 =
        ifelse(total_anions_mEq > total_cations_mEq &
                 ave_ions_mEq > 0 & ion_check_magnesium == TRUE,
               magnesium_mEq_cor - ave_ions_mEq, magnesium_mEq_cor),
      #
      total_anions_cor = chloride_mEq_SO4 + nitrate_mEq_SO4 + sulfate_mEq_SO4,
      total_cations_cor = sodium_mEq_SO4 + potassium_mEq_SO4 + calcium_mEq_SO4 + magnesium_mEq_SO4,
      abs_xs_ions_cor = abs(round(total_anions_cor - total_cations_cor, 6)),
      sulfate_mEq_SO4cor = sulfate_mEq_SO4 / (water_ml / (dry_g * (48.305))),  
      sulfate_wt_cor = (sulfate_mEq_SO4cor * water_ml) / (10000 * dry_g),
      #
      chloride_mEq_corSO4 = chloride_mEq_SO4,
      nitrate_mEq_corSO4 = nitrate_mEq_SO4,
      sulfate_mEq_corSO4 = sulfate_mEq_SO4cor, 
      sodium_mEq_corSO4 = sodium_mEq_SO4,
      potassium_mEq_corSO4 = potassium_mEq_SO4,
      calcium_mEq_corSO4 = calcium_mEq_SO4,
      magnesium_mEq_corSO4 = magnesium_mEq_SO4,
      #
      sulfate_diff = ifelse(calcium_mEq_SO4 > sulfate_mEq_SO4, 0, calcium_mEq_SO4 - sulfate_mEq_SO4),
      calcium_diff = ifelse(calcium_mEq_SO4 > sulfate_mEq_SO4, calcium_mEq_SO4 - sulfate_mEq_SO4, 0),
      theoretical_CaSO4 = ifelse(sulfate_mEq_SO4 == min(calcium_mEq_SO4, sulfate_mEq_SO4), 
                                 sulfate_mEq_SO4 * 2, calcium_mEq_SO4 * 2),
      #
      sulfate_ppm_cor = sulfate_diff / (water_ml / (dry_g * mol_wts$sulfate)),
      calcium_ppm_cor = calcium_diff / (water_ml / (dry_g * mol_wts$calcium)),
      sulfate_mEq_corCaSO4 = sulfate_mEq_SO4 - sulfate_diff,
      calcium_mEq_corCaSO4 = calcium_mEq_SO4 - calcium_diff,
      sulfate_ppm_corCaSO4 = sulfate_mEq_corCaSO4 / (water_ml / (dry_g * mol_wts$sulfate)),
      calcium_ppm_corCaSO4 = calcium_mEq_corCaSO4 / (water_ml / (dry_g * mol_wts$calcium)),
      Gypsum_wt = 
        (sulfate_ppm_corCaSO4 * water_ml) / (10000 * dry_g) + 
        (calcium_ppm_corCaSO4 * water_ml) / (10000 * dry_g),
      Total_anions = round(chloride_mEq_corSO4 + nitrate_mEq_corSO4 + sulfate_diff, 6),
      Total_cations = round(sodium_mEq_corSO4 + potassium_mEq_corSO4 + 
                              calcium_diff + magnesium_mEq_corSO4, 6),
      XS_ions = abs(Total_anions - Total_cations),
      #
      Chloride_ppm = chloride_mEq_corSO4 / (water_ml / (dry_g * mol_wts$chloride)),
      Nitrate_ppm = nitrate_mEq_corSO4 / (water_ml / (dry_g * mol_wts$nitrate)),
      Sulfate_ppm = sulfate_ppm_cor,
      # sulfate_mEq_corSO4 / (water_ml / (dry_g * mol_wts$sulfate)),
      Sodium_ppm = sodium_mEq_corSO4 / (water_ml / (dry_g * mol_wts$sodium)),
      Potassium_ppm = potassium_mEq_corSO4 / (water_ml / (dry_g * mol_wts$potassium)),
      Calcium_ppm = calcium_mEq_corSO4 / (water_ml / (dry_g * mol_wts$calcium)),
      Magnesium_ppm = magnesium_mEq_corSO4 / (water_ml / (dry_g * mol_wts$magnesium)),
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
      total_anions_MMOLkg = Chloride_MMOLkg + Nitrate_MMOLkg + (Sulfate_MMOLkg * 2),
      total_cations_MMOLkg = Sodium_MMOLkg + Potassium_MMOLkg + 
        (Calcium_MMOLkg * 2) + (Magnesium_MMOLkg * 2),
      diff_MMOLkg = round(total_anions_MMOLkg - total_cations_MMOLkg, 4),
      #
      Chloride_MOLkg =
        ifelse(total_anions_MMOLkg > total_cations_MMOLkg &
                 Chloride_MMOLkg == max(Chloride_MMOLkg, Nitrate_MMOLkg, Sulfate_MMOLkg),
               Chloride_MMOLkg - diff_MMOLkg, Chloride_MMOLkg) / 1000,
      Nitrate_MOLkg =
        ifelse(total_anions_MMOLkg > total_cations_MMOLkg &
                 Nitrate_MMOLkg == max(Chloride_MMOLkg, Nitrate_MMOLkg, Sulfate_MMOLkg),
               Nitrate_MMOLkg - diff_MMOLkg, Nitrate_MMOLkg) / 1000,
      Sulfate_MOLkg =
        ifelse(total_anions_MMOLkg > total_cations_MMOLkg &
                 Sulfate_MMOLkg == max(Chloride_MMOLkg, Nitrate_MMOLkg, Sulfate_MMOLkg),
               Sulfate_MMOLkg - diff_MMOLkg, Sulfate_MMOLkg) / 1000,
      Sodium_MOLkg =
        ifelse(total_cations_MMOLkg > total_anions_MMOLkg &
                 Sodium_MMOLkg == max(Sodium_MMOLkg, Potassium_MMOLkg, 
                                      Calcium_MMOLkg, Magnesium_MMOLkg),
               Sodium_MMOLkg - diff_MMOLkg, Sodium_MMOLkg) / 1000,
      Potassium_MOLkg =
        ifelse(total_cations_MMOLkg > total_anions_MMOLkg &
                 Sodium_MMOLkg == max(Sodium_MMOLkg, Potassium_MMOLkg, 
                                      Calcium_MMOLkg, Magnesium_MMOLkg),
               Potassium_MMOLkg - diff_MMOLkg, Potassium_MMOLkg) / 1000,
      Calcium_MOLkg =
        ifelse(total_cations_MMOLkg > total_anions_MMOLkg &
                 Calcium_MMOLkg == max(Sodium_MMOLkg, Potassium_MMOLkg,
                                       Calcium_MMOLkg, Magnesium_MMOLkg),
               Calcium_MMOLkg - diff_MMOLkg, Calcium_MMOLkg) / 1000,
      Magnesium_MOLkg =
        ifelse(total_cations_MMOLkg > total_anions_MMOLkg &
                 Magnesium_MMOLkg == max(Sodium_MMOLkg, Potassium_MMOLkg,
                                         Calcium_MMOLkg, Magnesium_MMOLkg),
               Magnesium_MMOLkg - diff_MMOLkg, Magnesium_MMOLkg) / 1000,
      #
      total_anions_MOLkg_total = Chloride_MOLkg + Nitrate_MOLkg + (Sulfate_MOLkg * 2),
      total_cations_MOLkg_total = Sodium_MOLkg + Potassium_MOLkg + 
        (Calcium_MOLkg * 2) + (Magnesium_MOLkg * 2),
      diff_MOLkg_diff = round(total_anions_MOLkg_total - total_cations_MOLkg_total, 4),
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
      total_gypsum_Wt = total_salt_wt - Gypsum_wt,
      total_diff_Wt = total_salt_wt - total_gypsum_Wt,
      #
      Chloride_ECOS = Chloride_MOLkg / total_anions_MOLkg_total * 100 , # %>% abs()
      Nitrate_ECOS = Nitrate_MOLkg / total_anions_MOLkg_total * 100, # %>% abs()
      Sulfate_ECOS = Sulfate_MOLkg / total_anions_MOLkg_total * 100, # %>% abs()
      Sodium_ECOS = Sodium_MOLkg / total_cations_MOLkg_total * 100,
      Potassium_ECOS = Potassium_MOLkg / total_cations_MOLkg_total * 100,
      Calcium_ECOS = Calcium_MOLkg / total_cations_MOLkg_total * 100,
      Magnesium_ECOS = Magnesium_MOLkg / total_cations_MOLkg_total * 100,
      #
      ECOS_total_anions = Chloride_ECOS + Nitrate_ECOS + Sulfate_ECOS * 2,
      ECOS_total_cations = Sodium_ECOS + Potassium_ECOS + Calcium_ECOS * 2 + Magnesium_ECOS * 2,
      ECOS_diff = abs(ECOS_total_anions - ECOS_total_cations),
      ECOS_sample_name = sample_name
      # , 
      # ECOS_sample_xs = ifelse()
    )
}

# salt_ppm %>% fun_salt_calcs() %>% select(c(sample_name, ends_with("ppm"))) %>% pivot_longer(-sample_name, names_to = "var", values_to = "value") 

# ION CONTENT IN SAMPLE (w%) ----
fun_salt_wt <- function(salt_ppm, dry_g, water_ml) {
  salt_wt <- (salt_ppm * water_ml) / (10000 * dry_g)
  return(salt_wt)
}

# TOTAL SALT ION CONTENT IN SAMPLE (w%) ----
fun_total_salt <- function(salt_wts, ...) {
  total_salt <- sum(salt_wts)
  return(total_salt)
}

# MOL ----
fun_salt_mol <- function(salt_ppm, water_ml, mol_wts) {
  salt_mol <- (0.001 *  salt_ppm * water_ml) / mol_wts
  return(salt_mol)
}

# MILLIEQUIVALENTS PER KILOGRAM (mEq/kg) ----
fun_salt_mileq <- function(
  salt_ppm, dry_g, water_ml, mol_wts) {
  salt_mileq <- 
    (salt_ppm * water_ml) / (dry_g * mol_wts)
  return(salt_mileq)
}
# MILLIEQUIVALENTS PER GRAM (mEq/g) ----
fun_salt_mileq_g <- function(
  salt_ppm, dry_g, water_ml, mol_wts) {
  # Divide mol_wt by 2 if sulfate (48.03) and calcium (20.04) 
  salt_mileq_g <- 
    (salt_ppm * water_ml) / (1000 * dry_g * mol_wts)
  return(salt_mileq_g)
}

# TOTAL ANIONS (mEq/g) ----
fun_total_anions <- function(salt_mileq, ...) {
  total_anions <- chloride_mEq + nitrate_mEq + sulfate_mEq
  # total_anions <- sum(salt_mileq)
  return(total_anions)
}

# TOTAL CATIONS (mEq/g) ----
fun_total_cations <- function(salt_mEq, ...) {
  total_cations <- sodium_mEq + potassium_mEq + calcium_mEq + magnesium_mEq 
  # total_cations <- sum(salt_mEq)
  return(total_cations)
}

# EXCESS OF CATIONS (mEq/g) ----
fun_xs_cations <- function(total_anions, total_cations) {
  xs_cations <- total_cations - total_anions 
  return(xs_cations)
}

# EXCESS OF Ca or SO4 (Ca - SO4 or SO4 - Ca) (mEq/g) ----
fun_excess_CaSO4 <- function(sulfate_mEq, calcium_mEq) {
  excess_CaSO4 <- ifelse(sulfate_mEq < calcium_mEq, 
                         calcium_mEq - sulfate_mEq, 
                         sulfate_mEq - calcium_mEq)
  return(excess_CaSO4)
}

# FINAL AMOUNT OF CaSO4 IN SAMPLE (w%) ----
fun_total_CaSO4 <- function(
  sulfate_wt, calcium_wt, sulfate_mileq, calcium_mileq, excess_CaSO4) { 
  total_CaSO4 <- 
    ifelse(calcium_mileq > sulfate_mileq, 
           sulfate_wt + (calcium_wt - (calcium_wt / (calcium_mileq) * excess_CaSO4)),
           calcium_wt + (sulfate_wt - (sulfate_wt / (sulfate_mileq) * excess_CaSO4)))
  return(total_CaSO4)
}

# TOTAL SALT CONTENT EXCLUDING  CaSO4 (w%) ----
fun_total_salt_exCaSO4 <- function(total_salt, total_CaSO4) { 
  total_salt_exCaSO4 <- total_salt - total_CaSO4
  return(total_salt_exCaSO4)
} 

# Wt% CO3 ----
fun_total_CO3 <- function(xs_cations) {
  total_CO3 <- (xs_cations * 31.01) / 10
  return(total_CO3)
} 

# TOTAL SALT CONTENT EXCLUDING CaSO4, INCLUDING CO3 (w%) ----
fun_total_salt_exCaSO4incCO3 <- function(total_salt_exCaSO4, CO3_wt) { 
  total_salt_exCaSO4incCO3 <- total_salt_exCaSO4 + CO3_wt
  return(total_salt_exCaSO4incCO3)
} 

# TOTAL SALT CONTEN EXCLUDING CaSO4 and EXCESS Ca  (w%) ----
fun_total_salt_exCaSO4_Ca <- function(
  total_salt_exCaSO4, dry_g, water_ml, xs_cations, ...) { 
  # BH2(total_salt_exCaSO4)-(((($BP2/$T2)*(1000*$S2*20.04))*$T2)/(10000*$S2))
  total_salt_exCaSO4_Ca <- total_salt_exCaSO4 - 
    ((xs_cations / water_ml) * ((1000 * dry_g * 20.04) * 
                                      water_ml) / (10000 * dry_g))
  return(total_salt_exCaSO4_Ca)
} 



# Salt data ----
## Salt metadata ---- 
salt_names <- tribble(
  ~change_from, ~change_to, 
  # ---- | ----  
  "Institute", "Institute",
  "Sample", "sample_name", 
  "User", "user_name",
  "Email", "user_email", 
  "Location", "sample_location", 
  "Code", "sample_code", 
  "Date", "sample_date", 
  "Latitude", "sample_lat",
  "Longitude", "sample_lon", 
  "Altitude", "sample_alt", 
  "Material", "sample_material", 
  "Depth", "sample_depth", 
  "Height", "sample_height", 
  #
  "User name", "user_name", 
  "User email", "user_email", 
  "INSTITUTE", "Institute", # institute_name,
  "INSITUTE", "Institute", 
  "INSTITUTE DATA", "inst_data", 
  "INSITUTE DATA", "inst_data",
  "Institute abbreviation", "inst_abbr", 
  "Institute country", "inst_country",
  "Institutional document code", "inst_doc_code",
  #
  "Sample", "sample_data", 
  "sample_data", "sample_data",
  "Institutional sample code", "sample_code", 
  "GENERAL SAMPLING DATA", "Sample", 
  "Month", "sample_month", 
  "Year", "sample_year",
  "Sample date", "sample_date",
  "Location", "sample_location", 
  "Object Location Country", "sample_country", 
  "Object locatiion postcode", "sample_postcode", 
  "Object location postcode", "sample_postcode", 
  "Object location city", "sample_city", 
  "Object name", "sample_object", 
  "GPS coordinates", "sample_gps", 
  "Latitude", "sample_lat", # sample_lat
  "Longitude", "sample_lon", # sample_lon
  "Altitude", "sample_alt", # sample_alt
  "Exterior or Interior (Ex. or Int.)", "sample_ext_int", 
  "Sample material (see legend)", "sample_material", 
  "Sample material", "sample_material", # sample_material
  "Height", "sample_height",
  "Sample height  (cm)", "sample_height", # "sample_height"
  "Sample height (cm)", "sample_height", # "sample_height"
  "Sample height", "sample_height", # sample_height
  "Sample depth from surface of specific material (cm to cm)", "sample_depth", 
  "Sample depth (cm)","sample_depth", # sample_depth
  "Sample depth","sample_depth", # sample_depth
  "SPECIFIC SAMPLING DATA", "sample_specifics", 
  #
  "Dry sample weight for IC (g)", "dry_g", # dry_g
  "Label: Dry sample weight for IC (g)", "dry_g", # dry_g
  "Dry sample weight for IC (kg)", "dry_g_kg", 
  "Amount of water added to sample for IC (ml)", "water_ml",
  "Amount of water added to sample for IC (L)", "water_ml_l",
  "Water added for IC (ml)", "water_ml", # water_ml
  "Sample description", "sample_description", # "sample_description"
  #
  "IC DATA IONS (ppm)", "Ion (ppm)", 
  "CHLORIDE (Cl-) (PPM)", "chloride_ppm", 
  "NITRATE (NO3-) (PPM)", "nitrate_ppm", 
  "SULFATE (SO42-) (PPM)", "sulfate_ppm", 
  "SODIUM (Na+) (PPM)", "sodium_ppm", 
  "POTASSIUM (K+) (PPM)", "potassium_ppm", 
  "CALCIUM (Ca2+) (PPM)", "calcium_ppm", 
  "MAGNISIUM (Mg2+) (PPM)", "magnesium_ppm", 
  #
  "ION CONTENT IN SAMPLE (w%)", "Ion (w%)", 
  "CHLORIDE (Cl-) (w%)", "Chloride_wt",
  "NITRATE (NO3-) (w%)", "Nitrate_wt", 
  "SULFATE (SO42-) (w%)", "Sulfate_wt", 
  "SODIUM (Na+) (w%)", "Sodium_wt", 
  "POTASSIUM (K+) (w%)", "Potassium_wt",
  "CALCIUM (Ca2+) (w%)", "Calcium_wt", 
  "MAGNISIUM (Mg2+) (w%)", "Magnesium_wt", 
  "TOTAL SALT ION CONTEN IN SAMPLE (w%)", "total_wt",
  "TOTAL SALT ION CONTENT IN SAMPLE (w%)", "total_wt",
  #
  "MOL", "Ion (mol)", 
  "CHLORIDE (Cl-) (Mol)", "chloride_mol",
  "NITRATE (NO3-) (Mol)", "nitrate_mol", 
  "SULFATE (SO42-) (Mol)", "sulfate_mol", 
  "SODIUM (Na+) (Mol)", "sodium_mol",
  "POTASSIUM (K+) (Mol)", "potassium_mol", 
  "CALCIUM (Ca2+) (Mol)", "calcium_mol",
  "MAGNISIUM (Mg2+) (Mol)", "magnesium_mol"
)

# salt_data_raw <- read_excel("input/SALT-DATABASE_2004-2018_setupRILEM.xlsx", sheet = "DATABASE")
# 
# salt_data <- salt_data_raw %>% setNames(salt_meta$change_to[match(names(salt_data_raw), salt_meta$change_from)])


# Salt climatology ----

# Humidity ----

# Specific humidity (g/kg)
fun_SpecHum <- function(VapP, p = 101325) {
  # library(humidity)
  # myfun_SpecHum(myfun_VapP(myfun_SatP(Temp = 23), RH = 35)) # 6.147298
  Mw = 18.01528; Md = 28.9634
  k <- Mw / Md
  q <- k * VapP/(p - (1 - k) * VapP)
  return(q * 1000)
}

# Saturated vapour pressure Es (mb, hPa)
fun_SatP <- function(Temp) {
  # library(humidity)
  Es_Temp0 = 6.11 # hPa
  L = 2500000 # J/kg
  Rw = 461.52 # J/(kgK)
  T0 = 273.15 # K
  Es = Es_Temp0 * exp((L / Rw) * (1 / T0 - 1 / (Temp + T0)))
  return(Es)
}

# Vapour pressure E (mb, hPa)
fun_VapP <- function(SatP, RH) {
  RH * SatP
}

myfun_Abshum <- function(Temp, RH) {
  (2165*(RH*((101325*exp((((-0.1299*(1 - (373.15/(273.15 + Temp))) - 0.6445)*(1 - (373.15/(273.15 + Temp))) - 1.976)*(1 - (373.15/(273.15 + Temp))) + 13.3185)*(1 - (373.15/(273.15 + Temp)))))/1000)/100)/(Temp + 273.15) ) # / 1000
}
#
myfun_RHfromAbs <- function(Abs, Temp) {
  (1/(2165*(((101325*exp((((-0.1299*(1 - (373.15/(273.16 + Temp))) - 0.6445)*(1 - (373.15/(273.15 + Temp))) - 1.976)*(1 - (373.15/(273.15 + Temp))) + 13.3185)*(1 - (373.15/(273.15 + Temp)))))/1000)/100)/(Temp + 273.15)/Abs) ) # * 1000
}
#
myfun_RH <- function(Abs, Temp) {
  (Abs * (273.15 + Temp) * (10^(-(7.591386 * Temp)/(Temp + 240.7263)))) / (6.116441*2.16679)
}
#
myfun_DewPoint <- function(Temp, RH) {
  240.7263/(7.591386/(log10(220640*(exp((647.096/(Temp + 273.16))*((-7.85951783*(1 - ((Temp + 273.16)/647.096))) + (1.84408259*(1 - ((Temp + 273.16)/647.096))^1.5) + (-11.7866497*(1 - ((Temp + 273.16)/647.096))^3) + (22.6807411*(1 - ((Temp + 273.16)/647.096))^3.5) + (-15.9618719*(1 - ((Temp + 273.16)/647.096))^4) + (1.80122502*(1 - ((Temp + 273.16)/647.096))^7.5))))*(RH/100)/6.116441)) - 1)
}




# myfun_TRH_factors
myfun_TRH_factors <- function(
  df_tidy, Temp, RH, LowTspec, HighTspec, LowRHspec, HighRHspec, ...) {
  library(dplyr);
  #
  df <- df_tidy %>%
    dplyr::mutate(
      DewPointRisk = ifelse(Temp < DewPoint, 1, 0),
      FrostRisk = ifelse(Temp < 0, 1, 0),
      SaltRisk = ifelse(RH > 75, 1, 0),
      #
      TRH_within = ifelse(Temp >= LowTspec & Temp <= HighTspec &
                            RH >= LowRHspec & RH <= HighRHspec, 1, 0),
      T_within = ifelse(Temp >= LowTspec & Temp <= HighTspec, 1, 0),
      RH_within = ifelse(RH >= LowRHspec & RH <= HighRHspec, 1, 0),
      T_lower = ifelse(Temp < LowTspec, 1, 0),
      T_higher = ifelse(Temp > HighTspec, 1, 0),
      RH_lower = ifelse(RH < LowRHspec, 1, 0),
      RH_higher = ifelse(RH > HighRHspec, 1, 0),
      RH_lowcurve =
        myfun_RHfromAbs(myfun_Abshum(LowTspec, LowRHspec), Temp),
      RH_highcurve =
        myfun_RHfromAbs(myfun_Abshum(HighTspec, HighRHspec), Temp),
      RH_withincurve = ifelse(RH >= RH_lowcurve & RH <= RH_highcurve, 1, 0),
      RH_abovecurve = ifelse(RH > RH_highcurve, 1, 0),
      RH_belowcurve = ifelse(RH < RH_lowcurve, 1, 0)
    )
  return(df)
}

myfun_TRH_zones <- function(df_tidy, ...) {
  library(dplyr)#; library(magrittr)
  #
  # zone (x9): Within, Heating only, Dehum or heating, Dehum only, Cooling and dehum, Heating and hum, Hum only,  Hum or cooling, Cooling only, Cooling and hum
  df <- df_tidy %>%
    dplyr::mutate(
      zone = dplyr::case_when(
        TRH_within == 1 ~ "Within",
        T_lower == 1 & RH_withincurve == 1 ~ "Heating only",
        RH_higher == 1 & T_within == 1 & RH_withincurve == 1 ~
          "Dehum or heating",
        RH_higher == 1 & T_within == 1 & RH_abovecurve == 1 ~
          "Dehum only",
        T_higher == 1 & RH_abovecurve == 1 ~ "Cooling and dehum",
        T_lower == 1 & RH_belowcurve == 1 ~ "Heating and hum",
        T_lower == 1 & RH_abovecurve == 1 ~ "Heating and dehum",
        RH_lower == 1 & T_within == 1 & RH_withincurve == 1 ~
          "Hum or cooling",
        T_higher == 1 & RH_withincurve == 1 ~ "Cooling only",
        T_higher == 1 & RH_belowcurve == 1 ~ "Cooling and hum"
      )) %>%
    #
    # TRH_zone (x9): Within, Cold, Cold and humid, Cold and dry, Hot, Hot and humid, Hot and dry, Humid, Dry
    dplyr::mutate(
      TRH_zone = dplyr::case_when(
        TRH_within == 1 ~ "Within",
        T_lower == 1 & RH_within == 1 ~ "Cold",
        T_lower == 1 & RH_higher == 1 ~ "Cold and humid",
        T_lower == 1 & RH_lower == 1 ~ "Cold and dry",
        T_higher == 1 & RH_within == 1 ~ "Hot",
        T_higher == 1 & RH_higher == 1 ~ "Hot and humid",
        T_higher == 1 & RH_lower == 1 ~ "Hot and dry",
        RH_higher == 1 & T_within == 1 ~ "Humid",
        RH_lower == 1 & T_within == 1 ~ "Dry"
      )) %>%
    #
    # T_zone: Within, Cold, Hot
    dplyr::mutate(
      T_zone = dplyr::case_when(
        T_within == 1 ~ "Within",
        T_lower == 1 ~ "Cold",
        T_higher == 1 ~ "Hot"
      )) %>%
    #
    # RH_zone: Within, Dry, Humid
    dplyr::mutate(
      RH_zone = dplyr::case_when(
        RH_within == 1 ~ "Within",
        RH_lower == 1 ~ "Dry",
        RH_higher == 1 ~ "Humid"
      )) %>%
    #
    # dTemp
    dplyr::mutate(
      dTemp = dplyr::case_when(
        TRH_zone == "Within" ~ 0,
        TRH_zone == "Dry" ~ 0,
        TRH_zone == "Humid" ~ 0,
        TRH_zone == "Cold" ~ Temp - LowTspec,
        TRH_zone == "Cold and humid" ~ Temp - LowTspec,
        TRH_zone == "Cold and dry" ~ Temp - LowTspec,
        TRH_zone == "Hot" ~ Temp - HighTspec,
        TRH_zone == "Hot and humid" ~ Temp - HighTspec,
        TRH_zone == "Hot and dry" ~ Temp - HighTspec
      )) %>%
    #
    # dRH
    dplyr::mutate(
      dRH = dplyr::case_when(
        TRH_zone == "Within" ~ 0,
        TRH_zone == "Cold" ~ 0,
        TRH_zone == "Hot" ~ 0,
        TRH_zone == "Cold and humid" ~ RH - HighRHspec,
        TRH_zone == "Cold and dry" ~ RH - LowRHspec,
        TRH_zone == "Hot and humid" ~ RH - HighRHspec,
        TRH_zone == "Hot and dry" ~ RH - LowRHspec,
        TRH_zone == "Humid" ~ RH - HighRHspec,
        TRH_zone == "Dry" ~ RH - LowRHspec
      ))
  return(df)
}


myfun_TRH_recalcs <- function(
  df_tidy, Temp, RH, Abs, TempCorrection,
  LowTspec, HighTspec, LowRHspec, HighRHspec, ...) {
  library(dplyr); library(magrittr); library(purrr)
  #
  TempCorrection <- tibble(
    NewTemp =  c(-20,-19.9,-19.8,-19.7,-19.6,-19.5,-19.4,-19.3,-19.2,-19.1,-19,-18.9,-18.8,-18.7,-18.6,-18.5,-18.4,-18.3,-18.2,-18.1,-18,-17.9,-17.8,-17.7,-17.6,-17.5,-17.4,-17.3,-17.2,-17.1,-17,-16.9,-16.8,-16.7,-16.6,-16.5,-16.4,-16.3,-16.2,-16.1,-16,-15.9,-15.8,-15.7,-15.6,-15.5,-15.4,-15.3,-15.2,-15.1,-15,-14.9,-14.8,-14.7,-14.6,-14.5,-14.4,-14.3,-14.2,-14.1,-14,-13.9,-13.8,-13.7,-13.6,-13.5,-13.4,-13.3,-13.2,-13.1,-13,-12.9,-12.8,-12.7,-12.6,-12.5,-12.4,-12.3,-12.2,-12.1,-12,-11.9,-11.8,-11.7,-11.6,-11.5,-11.4,-11.3,-11.2,-11.1,-11,-10.9,-10.8,-10.7,-10.6,-10.5,-10.4,-10.3,-10.2,-10.1,-10,-9.9,-9.8,-9.7,-9.6,-9.5,-9.4,-9.3,-9.2,-9.1,-9,-8.9,-8.8,-8.7,-8.6,-8.5,-8.4,-8.3,-8.2,-8.1,-8,-7.9,-7.8,-7.7,-7.6,-7.5,-7.4,-7.3,-7.2,-7.1,-7,-6.9,-6.8,-6.7,-6.6,-6.5,-6.4,-6.3,-6.2,-6.1,-6,-5.9,-5.8,-5.7,-5.6,-5.5,-5.4,-5.3,-5.2,-5.1,-5,-4.9,-4.8,-4.7,-4.6,-4.5,-4.4,-4.3,-4.2,-4.1,-4,-3.9,-3.8,-3.7,-3.6,-3.5,-3.4,-3.3,-3.2,-3.1,-3,-2.9,-2.8,-2.7,-2.6,-2.5,-2.4,-2.3,-2.2,-2.1,-2,-1.9,-1.8,-1.7,-1.6,-1.5,-1.4,-1.3,-1.2,-1.1,-1,-0.9,-0.8,-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2,2.1,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,3,3.1,3.2,3.3,3.4,3.5,3.6,3.7,3.8,3.9,4,4.1,4.2,4.3,4.4,4.5,4.6,4.7,4.8,4.9,5,5.1,5.2,5.3,5.4,5.5,5.6,5.7,5.8,5.9,6,6.1,6.2,6.3,6.4,6.5,6.6,6.7,6.8,6.9,7,7.1,7.2,7.3,7.4,7.5,7.6,7.7,7.8,7.9,8,8.1,8.2,8.3,8.4,8.5,8.6,8.7,8.8,8.9,9,9.1,9.2,9.3,9.4,9.5,9.6,9.7,9.8,9.9,10,10.1,10.2,10.3,10.4,10.5,10.6,10.7,10.8,10.9,11,11.1,11.2,11.3,11.4,11.5,11.6,11.7,11.8,11.9,12,12.1,12.2,12.3,12.4,12.5,12.6,12.7,12.8,12.9,13,13.1,13.2,13.3,13.4,13.5,13.6,13.7,13.8,13.9,14,14.1,14.2,14.3,14.4,14.5,14.6,14.7,14.8,14.9,15,15.1,15.2,15.3,15.4,15.5,15.6,15.7,15.8,15.9,16,16.1,16.2,16.3,16.4,16.5,16.6,16.7,16.8,16.9,17,17.1,17.2,17.3,17.4,17.5,17.6,17.7,17.8,17.9,18,18.1,18.2,18.3,18.4,18.5,18.6,18.7,18.8,18.9,19,19.1,19.2,19.3,19.4,19.5,19.6,19.7,19.8,19.9,20,20.1,20.2,20.3,20.4,20.5,20.6,20.7,20.8,20.9,21,21.1,21.2,21.3,21.4,21.5,21.6,21.7,21.8,21.9,22,22.1,22.2,22.3,22.4,22.5,22.6,22.7,22.8,22.9,23,23.1,23.2,23.3,23.4,23.5,23.6,23.7,23.8,23.9,24,24.1,24.2,24.3,24.4,24.5,24.6,24.7,24.8,24.9,25,25.1,25.2,25.3,25.4,25.5,25.6,25.7,25.8,25.9,26,26.1,26.2,26.3,26.4,26.5,26.6,26.7,26.8,26.9,27,27.1,27.2,27.3,27.4,27.5,27.6,27.7,27.8,27.9,28,28.1,28.2,28.3,28.4,28.5,28.6,28.7,28.8,28.9,29,29.1,29.2,29.3,29.4,29.5,29.6,29.7,29.8,29.9,30,30.1,30.2,30.3,30.4,30.5,30.6,30.7,30.8,30.9,31,31.1,31.2,31.3,31.4,31.5,31.6,31.7,31.8,31.9,32,32.1,32.2,32.3,32.4,32.5,32.6,32.7,32.8,32.9,33,33.1,33.2,33.3,33.4,33.5,33.6,33.7,33.8,33.9,34,34.1,34.2,34.3,34.4,34.5,34.6,34.7,34.8,34.9,35,35.1,35.2,35.3,35.4,35.5,35.6,35.7,35.8,35.9,36,36.1,36.2,36.3,36.4,36.5,36.6,36.7,36.8,36.9,37,37.1,37.2,37.3,37.4,37.5,37.6,37.7,37.8,37.9,38,38.1,38.2,38.3,38.4,38.5,38.6,38.7,38.8,38.9,39,39.1,39.2,39.3,39.4,39.5,39.6,39.7,39.8,39.9,40,40.1,40.2,40.3,40.4,40.5,40.6,40.7,40.8,40.9,41,41.1,41.2,41.3,41.4,41.5,41.6,41.7,41.8,41.9,42,42.1,42.2,42.3,42.4,42.5,42.6,42.7,42.8,42.9,43,43.1,43.2,43.3,43.4,43.5,43.6,43.7,43.8,43.9,44,44.1,44.2,44.3,44.4,44.5,44.6,44.7,44.8,44.9,45,45.1,45.2,45.3,45.4,45.5,45.6,45.7,45.8,45.9,46,46.1,46.2,46.3,46.4,46.5,46.6,46.7,46.8,46.9,47,47.1,47.2,47.3,47.4,47.5,47.6,47.7,47.8,47.9,48,48.1,48.2,48.3,48.4,48.5,48.6,48.7,48.8,48.9,49,49.1,49.2,49.3,49.4,49.5,49.6,49.7,49.8,49.9,50),
    #
    Correction = c(-0.926591828,-0.922817972,-0.919038118,-0.915252265,-0.911460411,-0.907662555,-0.903858694,-0.900048827,-0.896232952,-0.892411068,-0.888583173,-0.884749265,-0.880909343,-0.877063405,-0.873211449,-0.869353473,-0.865489477,-0.861619457,-0.857743413,-0.853861343,-0.849973245,-0.846079118,-0.84217896,-0.838272769,-0.834360543,-0.830442281,-0.826517982,-0.822587643,-0.818651263,-0.81470884,-0.810760373,-0.80680586,-0.802845299,-0.798878689,-0.794906028,-0.790927314,-0.786942546,-0.782951722,-0.77895484,-0.774951899,-0.770942898,-0.766927834,-0.762906705,-0.758879511,-0.75484625,-0.750806919,-0.746761518,-0.742710045,-0.738652498,-0.734588875,-0.730519175,-0.726443396,-0.722361537,-0.718273596,-0.714179571,-0.71007946,-0.705973263,-0.701860978,-0.697742602,-0.693618134,-0.689487574,-0.685350918,-0.681208165,-0.677059315,-0.672904364,-0.668743313,-0.664576158,-0.660402899,-0.656223533,-0.65203806,-0.647846477,-0.643648783,-0.639444977,-0.635235056,-0.63101902,-0.626796866,-0.622568593,-0.6183342,-0.614093685,-0.609847046,-0.605594281,-0.60133539,-0.59707037,-0.59279922,-0.588521938,-0.584238523,-0.579948974,-0.575653288,-0.571351464,-0.5670435,-0.562729396,-0.558409148,-0.554082757,-0.54975022,-0.545411535,-0.541066701,-0.536715717,-0.532358581,-0.527995291,-0.523625846,-0.519250244,-0.514868484,-0.510480564,-0.506086482,-0.501686238,-0.497279829,-0.492867254,-0.488448511,-0.484023599,-0.479592517,-0.475155262,-0.470711833,-0.466262229,-0.461806448,-0.457344489,-0.452876349,-0.448402028,-0.443921524,-0.439434836,-0.434941961,-0.430442899,-0.425937647,-0.421426205,-0.41690857,-0.412384742,-0.407854718,-0.403318498,-0.398776079,-0.39422746,-0.38967264,-0.385111617,-0.380544389,-0.375970956,-0.371391315,-0.366805465,-0.362213405,-0.357615132,-0.353010646,-0.348399945,-0.343783027,-0.339159892,-0.334530537,-0.32989496,-0.325253161,-0.320605139,-0.31595089,-0.311290414,-0.30662371,-0.301950776,-0.29727161,-0.292586212,-0.287894578,-0.283196709,-0.278492602,-0.273782256,-0.269065669,-0.264342841,-0.259613769,-0.254878452,-0.250136888,-0.245389077,-0.240635016,-0.235874704,-0.23110814,-0.226335322,-0.221556249,-0.216770919,-0.211979331,-0.207181483,-0.202377373,-0.197567001,-0.192750365,-0.187927463,-0.183098294,-0.178262857,-0.17342115,-0.168573171,-0.163718919,-0.158858393,-0.15399159,-0.149118511,-0.144239153,-0.139353514,-0.134461594,-0.129563391,-0.124658903,-0.119748129,-0.114831068,-0.109907718,-0.104978077,-0.100042145,-0.095099919,-0.090151398,-0.085196582,-0.080235468,-0.075268055,-0.070294341,-0.065314325,-0.060328007,-0.055335383,-0.050336453,-0.045331216,-0.040319669,-0.035301812,-0.030277643,-0.025247161,-0.020210364,-0.015167251,-0.01011782,-0.00506207,0,0.005068392,0.010143108,0.015224148,0.020311514,0.025405208,0.030505231,0.035611584,0.040724269,0.045843287,0.050968639,0.056100328,0.061238354,0.066382719,0.071533424,0.076690471,0.08185386,0.087023594,0.092199674,0.097382101,0.102570877,0.107766003,0.11296748,0.11817531,0.123389494,0.128610033,0.13383693,0.139070185,0.144309799,0.149555775,0.154808113,0.160066815,0.165331883,0.170603317,0.175881119,0.18116529,0.186455832,0.191752747,0.197056035,0.202365697,0.207681736,0.213004153,0.218332949,0.223668125,0.229009682,0.234357623,0.239711949,0.24507266,0.250439758,0.255813245,0.261193122,0.266579391,0.271972051,0.277371106,0.282776556,0.288188403,0.293606648,0.299031293,0.304462338,0.309899785,0.315343636,0.320793891,0.326250553,0.331713622,0.3371831,0.342658988,0.348141288,0.353630001,0.359125128,0.36462667,0.370134629,0.375649007,0.381169804,0.386697022,0.392230662,0.397770726,0.403317214,0.408870129,0.414429471,0.419995243,0.425567444,0.431146077,0.436731143,0.442322642,0.447920578,0.45352495,0.45913576,0.46475301,0.470376701,0.476006833,0.481643409,0.48728643,0.492935896,0.49859181,0.504254173,0.509922985,0.515598249,0.521279965,0.526968135,0.532662761,0.538363842,0.544071382,0.54978538,0.555505839,0.56123276,0.566966143,0.572705991,0.578452304,0.584205085,0.589964333,0.595730051,0.601502239,0.607280899,0.613066033,0.618857641,0.624655725,0.630460287,0.636271326,0.642088846,0.647912846,0.653743328,0.659580295,0.665423746,0.671273683,0.677130107,0.68299302,0.688862423,0.694738317,0.700620704,0.706509584,0.71240496,0.718306832,0.724215201,0.730130069,0.736051437,0.741979306,0.747913678,0.753854554,0.759801935,0.765755822,0.771716217,0.777683121,0.783656535,0.78963646,0.795622898,0.80161585,0.807615317,0.813621301,0.819633802,0.825652822,0.831678362,0.837710423,0.843749008,0.849794116,0.855845749,0.861903908,0.867968595,0.874039811,0.880117557,0.886201834,0.892292644,0.898389988,0.904493867,0.910604281,0.916721234,0.922844725,0.928974756,0.935111328,0.941254442,0.947404101,0.953560303,0.959723052,0.965892349,0.972068194,0.978250588,0.984439534,0.990635031,0.996837082,1.003045688,1.009260849,1.015482568,1.021710844,1.027945681,1.034187077,1.040435036,1.046689558,1.052950644,1.059218295,1.065492513,1.071773299,1.078060655,1.08435458,1.090655077,1.096962147,1.10327579,1.109596009,1.115922804,1.122256177,1.128596128,1.134942659,1.141295771,1.147655465,1.154021743,1.160394605,1.166774054,1.173160089,1.179552713,1.185951925,1.192357729,1.198770124,1.205189112,1.211614694,1.218046872,1.224485646,1.230931018,1.237382988,1.243841558,1.25030673,1.256778504,1.263256882,1.269741864,1.276233452,1.282731647,1.28923645,1.295747863,1.302265886,1.308790521,1.315321769,1.32185963,1.328404107,1.334955201,1.341512911,1.348077241,1.35464819,1.36122576,1.367809953,1.374400768,1.380998208,1.387602274,1.394212966,1.400830286,1.407454236,1.414084815,1.420722026,1.427365869,1.434016346,1.440673458,1.447337206,1.45400759,1.460684613,1.467368275,1.474058578,1.480755522,1.487459109,1.49416934,1.500886216,1.507609738,1.514339907,1.521076725,1.527820192,1.53457031,1.54132708,1.548090502,1.554860579,1.561637311,1.568420699,1.575210745,1.582007449,1.588810813,1.595620837,1.602437524,1.609260873,1.616090887,1.622927566,1.629770911,1.636620924,1.643477605,1.650340957,1.657210978,1.664087672,1.670971039,1.67786108,1.684757797,1.691661189,1.698571259,1.705488008,1.712411437,1.719341546,1.726278337,1.733221811,1.740171969,1.747128812,1.754092342,1.761062559,1.768039464,1.775023059,1.782013344,1.789010322,1.796013992,1.803024356,1.810041415,1.81706517,1.824095623,1.831132773,1.838176623,1.845227174,1.852284425,1.85934838,1.866419038,1.873496401,1.88058047,1.887671246,1.894768729,1.901872922,1.908983825,1.916101439,1.923225766,1.930356805,1.93749456,1.944639029,1.951790216,1.95894812,1.966112742,1.973284085,1.980462148,1.987646933,1.994838441,2.002036674,2.009241631,2.016453314,2.023671725,2.030896864,2.038128732,2.045367331,2.052612661,2.059864723,2.067123519,2.07438905,2.081661316,2.088940319,2.09622606,2.103518539,2.110817759,2.118123719,2.125436421,2.132755866,2.140082055,2.147414989,2.15475467,2.162101097,2.169454273,2.176814198,2.184180873,2.191554299,2.198934478,2.20632141,2.213715096,2.221115538,2.228522737,2.235936693,2.243357407,2.250784881,2.258219115,2.265660111,2.27310787,2.280562393,2.28802368,2.295491732,2.302966552,2.310448139,2.317936495,2.325431621,2.332933518,2.340442186,2.347957627,2.355479842,2.363008832,2.370544598,2.378087141,2.385636462,2.393192561,2.400755441,2.408325101,2.415901544,2.423484769,2.431074778,2.438671572,2.446275152,2.45388552,2.461502675,2.469126619,2.476757353,2.484394878,2.492039195,2.499690305,2.507348209,2.515012907,2.522684402,2.530362694,2.538047784,2.545739672,2.553438361,2.56114385,2.568856141,2.576575236,2.584301134,2.592033837,2.599773345,2.607519661,2.615272784,2.623032716,2.630799458,2.638573011,2.646353375,2.654140553,2.661934543,2.669735349,2.67754297,2.685357408,2.693178663,2.701006737,2.70884163,2.716683344,2.724531879,2.732387237,2.740249418,2.748118424,2.755994255,2.763876912,2.771766397,2.77966271,2.787565851,2.795475824,2.803392627,2.811316262,2.819246731,2.827184033,2.83512817,2.843079143,2.851036954,2.859001601,2.866973088,2.874951415,2.882936582,2.89092859,2.898927442,2.906933136,2.914945676,2.922965061,2.930991292,2.93902437,2.947064297,2.955111073,2.963164699,2.971225176,2.979292505,2.987366687,2.995447723,3.003535614,3.01163036,3.019731964,3.027840425,3.035955744,3.044077923,3.052206963,3.060342864,3.068485627,3.076635254,3.084791744,3.0929551,3.101125322,3.109302411,3.117486367,3.125677193,3.133874888,3.142079454,3.150290891,3.158509201,3.166734384,3.174966442,3.183205374,3.191451183,3.19970387,3.207963434,3.216229877,3.224503199,3.232783403,3.241070488,3.249364456,3.257665307,3.265973043)
  )
  #
  df_tidy %>%
    dplyr::mutate(New_Absrhmin = myfun_Abshum(Temp, LowRHspec)) %>%
    dplyr::mutate(New_Absrhmax = myfun_Abshum(Temp, HighRHspec)) %>%
    dplyr::mutate(Abs_toCurvemin = myfun_Abshum(Temp, RH_lowcurve)) %>%
    dplyr::mutate(Abs_toCurvemax = myfun_Abshum(Temp, RH_highcurve)) %>%
    #
    dplyr::mutate(Temp_toRHmin = myfun_TfromAbsRH(Abs, LowRHspec)) %>%
    dplyr::mutate(Temp_toRHmin = round(Temp_toRHmin, 1)) %>%
    dplyr::left_join(TempCorrection, by = c("Temp_toRHmin" = "NewTemp")) %>%
    dplyr::mutate(Temp_toRHmin = Temp_toRHmin + Correction) %>%
    dplyr::select(-Correction) %>%
    #
    dplyr::mutate(Temp_toRHmax = myfun_TfromAbsRH(Abs, HighRHspec)) %>%
    dplyr::mutate(Temp_toRHmax = round(Temp_toRHmax, 1)) %>%
    dplyr::left_join(
      TempCorrection, by = c("Temp_toRHmax" = "NewTemp")) %>%
    dplyr::mutate(Temp_toRHmax = Temp_toRHmax + Correction) %>%
    dplyr::select(-Correction) %>%
    #
    dplyr::mutate(Temp_toCurvemin = myfun_TfromAbsRH(Abs_toCurvemin, RH)) %>%
    dplyr::mutate(Temp_toCurvemin = round(Temp_toCurvemin, 1)) %>%
    dplyr::left_join(
      TempCorrection, by = c("Temp_toCurvemin" = "NewTemp")) %>%
    dplyr::mutate(Temp_toCurvemin = Temp_toCurvemin + Correction) %>%
    dplyr::select(-Correction) %>%
    #
    dplyr::mutate(Temp_toCurvemax = myfun_TfromAbsRH(Abs_toCurvemax, RH)) %>%
    dplyr::mutate(Temp_toCurvemax = round(Temp_toCurvemax, 1)) %>%
    dplyr::left_join(
      TempCorrection, by = c("Temp_toCurvemax" = "NewTemp")) %>%
    dplyr::mutate(Temp_toCurvemax = Temp_toCurvemax + Correction) %>%
    dplyr::select(-Correction) %>%
    #
    dplyr::mutate(
      New_Temp = case_when(
        zone == "Heating only" &
          myfun_RHfromAbs(Abs, LowTspec) > HighRHspec ~ Temp_toRHmax,
        zone == "Heating only" &
          myfun_RHfromAbs(Abs, LowTspec) <= HighRHspec ~ LowTspec,
        zone == "Dehum or heating" ~ Temp_toRHmax,
        zone == "Dehum only" ~ Temp,
        zone == "Cooling and dehum" ~ HighTspec,
        zone == "Heating and dehum" ~ LowTspec,
        zone == "Heating and hum" ~ LowTspec,
        zone == "Hum only" ~ Temp,
        zone == "Hum or cooling" ~ Temp,
        zone == "Cooling only" &
          myfun_RHfromAbs(Abs, HighTspec) < LowRHspec ~ Temp_toRHmin,
        zone == "Cooling only" &
          myfun_RHfromAbs(Abs, HighTspec) >= LowRHspec ~ HighTspec,
        zone == "Cooling and hum" ~ LowTspec,
        zone == "Within" ~ Temp,
        is.na(zone) ~ Temp
      )) %>%
    #
    dplyr::mutate(
      New_Abs = case_when(
        zone == "Heating only" ~ Abs,
        zone == "Dehum or heating" ~ Abs,
        zone == "Dehum only" ~ New_Absrhmax,
        zone == "Cooling and dehum" ~ Abs_toCurvemax,
        zone == "Heating and dehum" ~ Abs_toCurvemax,
        zone == "Heating and hum" ~ Abs_toCurvemin,
        zone == "Hum only" ~ New_Absrhmin,
        zone == "Hum or cooling" ~ New_Absrhmin,
        zone == "Cooling only" ~ Abs,
        zone == "Cooling and hum" ~ Abs_toCurvemin,
        zone == "Within" ~ Abs
      )) %>%
    #
    # TRH in spec (heating/cooling then hum/dehum)
    dplyr::mutate(
      Temp_diff = New_Temp - Temp,
      Abs_diff = New_Abs - Abs,
      New_RH = myfun_RHfromAbs(New_Abs, New_Temp),
      RH_diff = New_RH - RH) %>%
    #
    #
    # RH only in spec (hum/dehum only)
    dplyr::mutate(
      New_Abs_RHonly = case_when(
        RH_zone == "Dry" ~ New_Absrhmin,
        RH_zone == "Humid" ~ New_Absrhmax,
        RH_zone == "Within" ~ Abs
      )) %>%
    #
    dplyr::mutate(
      Abs_diff_RHonly = New_Abs_RHonly - Abs,
      New_RH_AbsRHonly = myfun_RHfromAbs(New_Abs_RHonly, Temp),
      RH_diff_AbsRHonly = New_RH_AbsRHonly - RH) %>%
    #
    #
    # RH only in spec (heating/cooling only)
    dplyr::mutate(
      New_Temp_RHonly = case_when(
        RH_zone == "Dry" ~ Temp_toRHmin,
        RH_zone == "Humid" ~ Temp_toRHmax,
        RH_zone == "Within" ~ Temp
      )) %>%
    #
    dplyr::mutate(
      Temp_diff_RHonly = New_Temp_RHonly - Temp,
      New_RH_TempRHonly = myfun_RHfromAbs(Abs, New_Temp_RHonly),
      RH_diff_TempRHonly = New_RH_TempRHonly - RH)
}

myfun_TRH_MinsMaxs <- function(alldata, ...) {
  library(dplyr)
  #
  df <- alldata %>%
    dplyr::mutate(
      Tmin1  = ifelse(Temp <= LowTspec, Temp, NA) %>% as.numeric(),
      Tmin2  = ifelse(Temp <= LowTspec, LowTspec, NA) %>% as.numeric(),
      Tmax1  = ifelse(Temp >= HighTspec, Temp, NA) %>% as.numeric(),
      Tmax2  = ifelse(Temp >= HighTspec, HighTspec, NA) %>% as.numeric(),
      RHmin1 = ifelse(RH <= LowRHspec, RH, NA) %>% as.numeric(),
      RHmin2 = ifelse(RH <= LowRHspec, LowRHspec, NA) %>% as.numeric(),
      RHmax1 = ifelse(RH >= HighRHspec, RH, NA) %>% as.numeric(),
      RHmax2 = ifelse(RH >= HighRHspec, HighRHspec, NA) %>% as.numeric()
    )
  return(df)
}


