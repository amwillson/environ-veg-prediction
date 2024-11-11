# Overview

This repository contains code written by AM Willson for Willson et al. (in prep). This repository contains processed data, code, figures, and outputs for the paper. Inputs are available upon request because they are larger than the permitted file size on Github.

# License

This repository holds an MIT License, as described in the LICENSE file.

# Software versions

This repository was built in the R environment using R version 4.4.2.

# Package versions

* `cowplot` v. 1.1.3
* `dplyr` v. 1.1.4
* `fields` v. 16.2
* `ggplot2` v. 3.5.1
* `maps` v. 3.4.2
* `MASS` v. 7.3.61
* `mgcv` v. 1.9.1
* `ncdf4` v. 1.23
* `randomForestSRC` v. 3.3.1
* `readr` v. 2.1.5
* `remotes` v. 2.5.0
* `reshape2` v. 1.4.4
* `sf` v. 1.0.19
* `tibble` v. 3.2.1
* `tidyr` v. 1.3.1
* `tune` v. 1.2.1

All packages can be installed using the install_packages.R script. When possible, the specific version of the package will be installed.

# Directory structure

## data

**Raw** data, **intermediate** data products, and **processed** data products from processing data and combining data sources. The **raw** and **intermediate** subdirectories are empty because the contents are large. The **raw** will be provided upon request and the **intermediate** files are easily recreated without much memory using the provided code. All the created files in the **processed** script are provided, allowing for comparison between any products replicated and our original outputs. These can also be used to directly recreate any figures.

### raw: inputs to this workflow

- climate: folder containing climate drivers. These were originally produced in the repository https://github.com/amwillson/historic-modern-environment/
    - gridded_climate_modern.RData: dataframe with climate drivers from PRISM for the modern period
    - gridded_climate.RData: dataframe with climate drivers from PRISM for the historic period
- FIA_08082024: folder containing COND, PLOT, SUBPLOT, and TREE data tables for Illinois, Indiana, Michigan, Minnesota, and Wisconsin downloaded from the FIA DataMart on 08 August 2024
    - IL_COND.csv: COND table for Illinois
    - IL_PLOT.csv: PLOT table for Illinois
    - IL_SUBPLOT.csv: SUBPLOT table for Illinois
    - IL_TREE.csv: TREE table for Illinois
    - IN_COND.csv: COND table for Indiana
    - IN_PLOT.csv: PLOT table for Indiana
    - IN_SUBPLOT.csv: SUBPLOT table for Indiana
    - IN_TREE.csv: TREE table for Indiana
    - manual.pdf: Most recent version of FIA data manual as of August 2024
    - MI_COND.csv: COND table for Michigan
    - MI_PLOT.csv: PLOT table for Michigan
    - MI_SUBPLOT.csv: SUBPLOT table for Michigan
    - MI_TREE.csv: TREE table for Michigan
    - MN_COND.csv: COND table for Minnesota
    - MN_PLOT.csv: PLOT table for Minnesota
    - MN_SUBPLOT.csv: SUBPLOT table for Minnesota
    - MN_TREE.csv: TREE table for Minnesota
    - WI_COND.csv: COND table for Wisconsin
    - WI_PLOT.csv: PLOT table for Wisconsin
    - WI_SUBPLOT.csv: SUBPLOT table for Wisconsin
    - WI_TREE.csv: TREE table for Wisconsin
- gridded-composition: downloaded PLS relative abundance/fractional composition from the following DOI: https://doi.org/10.6073/pasta/8544e091b64db26fdbbbafd0699fa4f9
    - manifest.txt: contains information on the data product and its creation
    - msb-paleon.1.0.report.xml: metadata
    - msb-paleon.1.0.txt: metadata
    - msb-plaeon.1.0.xml: metadata
    - SetTreeComp_Level2_v1.0.nc: NetCDF file with gridded relative abundance/fractional composition data
- gridded_density: downloaded PLS stem density from the following DOI: https://doi.org/10.6073/pasta/1b2632d48fc79b370740a7c20a70b4b0
    - manifest.txt: contains information on the data product and its creation
    - msb-paleon.24.0.report.xml: metadata
    - msb-paleon.24.0.text: metadata
    - msb-paleon.24.0.xml: metadata
    - PLS_Density_Draws_Level2_v1.0.zip: posterior draws from model estimating stem density. This is not used currently
    - PLS_Density_Point_Level2_v1.0.nc: posterior mean from model estimating stem density. This is what is currently used
- soils: folder containing climate drivers. These were originally produced in the repository https://github.com/amwillson/historic-modern-environment/
    - gridded_soil.RData: dataframe with gridded soil products. They are the same for both time periods so there is only one dataframe for both time periods

### intermediate: intermediate outputs from data processing steps

- combined_COND_PLOT_TREE_SPECIES.RData: dataframe with all the data from the COND, PLOT, SUBPLOT, and TREE tables that is needed for estimation of total stem density and relative abundance for all five states: Illinois, Indiana, Michigan, Minnesota, Wisconsin

### processed: processed data for analysis

- FIA: processed FIA and related modern data products
    - gridded_all_plots.RData: gridded total stem density and relative abundance for all FIA plots, despite some having only swapped/fuzzed coordinates associated. This is what is currently used
    - gridded_only_known_plots.RData: gridded total stem density and relative abundance for only plots with known (unswapped/unfuzzed) coordinates. This is not currently used
    - xydata_in.RData: only in-sample grid cells containing vegetation, climate, and soil data in a dataframe
    - xydata_out.RData: only out-of-sample grid cells containing vegetation, climate, and soil data in a dataframe
    - xydata.RData: all grid cells (both in-sample and out-of-sample) containing vegetation, climate and soil data in a dataframe
- PLS: processed PLS and related historical data productd
    - gridded_fcomp_density.RData: gridded total stem density and relative abundance for all PLS grid cell. This is the same as the data products in the data/raw/ directory, but in a more R-friendly format and stored in a dataframe
    - xydata_in.RData: only in-sample grid cells containing vegetation, climate and soil data in a dataframe
    - xydata_out.RData: only out-of-sample grid cells containing vegetation, climate and soil data in a dataframe
    - xydata.RData: all grid cells (both in-sample and out-of-sample) containing vegetation, climate, and soil data in a dataframe

## figures

Figures saved from 4.7.density_figures.R, 4.14.abundance_figures.R, 5.7.density_figures.R, 5.14.abundance_figures.R, 6.7.density_figures.R, and 6.14.abundance_figures.R. Figures may additionally be added for processed data products but have not been added yet.

- gam: figures associated with the Generalized Additive Models
    - H: figures from the models fit to historic data
        - density: figures from the models fit to historic total stem density
            - fit: partial dependence plots
            - pred: figures associated with assessing model predictions
        - abundance: figures from the models fit to historic relative abundance
            - fit: partial dependence plots
            - pred: figures associated with assessing model predictions
    - M: figures from the models fit to modern data
        - density: figures from the models fit to modern total stem density
            - fit: partial dependence plots
            - pred: figures associated with assessing model predictions
        - abundance: figures from the models fit to modern relative abundance
            - fit: partial dependence plots
            - pred: figures associated with assessing model predictions
- rf: figures associated with the Random Forest models
    - H: figures from models fit to historic data
        - abundance: figures from the models fit to historic relative abundance
            - fit: partial dependence plots
            - pred: figures associated with assessing model predictions
        - density: figures from the models fit to historic total stem density
            - fit: partial dependence plots
            - pred: figures associated with assessing model predictions
    - M: figures from models fit to modern data
        - abundance: figures from the models fit to modern relative abundance
            - fit: partial dependence plots
            - pred: figures associated with assessing model predictions
        - density: figures from the models fit to modern total stem density
            - fit: partial dependence plots
            - pred: figures associated with assessing model predictions

## out

Predictions from random forests and GAMs

- rf: predictions from random forests
    - H: predictions from random forests fit to historic data
        - abundance: relative abundance predictions
            - predicted_historical_rf1.RData: predictions of historical relative abundance from the model fit to soil and climate covariates
            - predicted_historical_rf2.RData: predictions of historical relative abundance from the model fit to climate covariates
            - predicted_historical_rf3.RData: predictions of historical relative abundance from the model fit to the reduced number of covariates
            - predicted_historical_rf4.RData: predictions of historical relative abundance from the model fit to soil and climate covariates and the latitude and longitude of the grid cell
            - predicted_modern_rf1.RData: predictions of modern relative abundance from the model fit to soil and climate covariates
            - predicted_modern_rf2.RData: predictions of modern relative abundance from the model fit to climate covariates
            - predicted_modern_rf3.RData: predictions of modern relative abunance from the model fit to the reduced number of covariates
            - predicted_modern_rf4.RData: predictions of modern relative abundance from the model fit to soil and climate covariates and the latitude and longitude of the grid cell
        - density: total stem density predictions
            - predicted_historical_rf1.RData: predictions of historical total stem density from the model fit to soil and climate covariates
            - predicted_historical_rf2.RData: predictions of historical total stem density from the model fit to climate covariates
            - predicted_historical_rf3.RData: predictions of historical total stem density from the model fit to the reduced number of covariates
            - predicted_historical_rf4.RData: predictions of historical relative abundance from the model fit to soil and climate covariates and the latitude and longitude of the grid cell
            - predicted_modern_rf1.RData: predictions of modern total stem density from the model fit to soil and climate covariates
            - predicted_modern_rf2.RData: predictions of modern total stem density from the model fit to climate covariates
            - predicted_modern_rf3.RData: predictions of modern total stem density from the model fit to the reduced number of covariates
            - predicted_modern_rf4.RData: predictions of modern relative abundance from the model fit to soil and climate covariates and the latitude and longitude of the grid cell
    - M: predictions from random forests fit to modern data
        - abundance: relative abundance predictions
            - predicted_historical_rf1.RData: predictions of historical relative abundance from the model fit to soil and climate covariates
            - predicted_historical_rf2.RData: predictions of historical relative abundance from the model fit to climate covariates
            - predicted_historical_rf3.RData: predictions of historical relative abundance from the model fit to the reduced number of covariates
            - predicted_historical_rf4.RData: predictions of historical relative abundance from the model fit to soil and climate covariates and the latitude and longitude of the grid cell
            - predicted_modern_rf1.RData: predictions of modern relative abundance from the model fit to soil and climate covariates
            - predicted_modern_rf2.RData: predictions of modern relative abundance from the model fit to climate covariates
            - predicted_modern_rf3.RData: predictions of modern relative abunance from the model fit to the reduced number of covariates
            - predicted_modern_rf4.RData: predictions of modern relative abundance from the model fit to soil and climate covariates and the latitude and longitude of the grid cell
        - density: total stem density predictions
            - predicted_historical_rf1.RData: predictions of historical total stem density from the model fit to soil and climate covariates
            - predicted_historical_rf2.RData: predictions of historical total stem density from the model fit to climate covariates
            - predicted_historical_rf3.RData: predictions of historical total stem density from the model fit to the reduced number of covariates
            - predicted_historical_rf4.RData: predictions of historical relative abundance from the model fit to soil and climate covariates and the latitude and longitude of the grid cell
            - predicted_modern_rf1.RData: predictions of modern total stem density from the model fit to soil and climate covariates
            - predicted_modern_rf2.RData: predictions of modern total stem density from the model fit to climate covariates
            - predicted_modern_rf3.RData: predictions of modern total stem density from the model fit to the reduced number of covariates
            - predicted_modern_rf4.RData: predictions of modern relative abundance from the model fit to soil and climate covariates and the latitude and longitude of the grid cell
- gam: predictions from generalized additive models (GAMs)
    - H: predictions from GAMs fit to historic data
        - abundance: relative abundance predictions
            - predicted_historical_rf1.RData: predictions of historical relative abundance from the model fit to soil and climate covariates
            - predicted_historical_rf2.RData: predictions of historical relative abundance from the model fit to climate covariates
            - predicted_historical_rf3.RData: predictions of historical relative abundance from the model fit to the reduced number of covariates
            - predicted_historical_rf4.RData: predictions of historical relative abundance from the model fit to soil and climate covariates and the latitude and longitude of the grid cell
            - predicted_modern_rf1.RData: predictions of modern relative abundance from the model fit to soil and climate covariates
            - predicted_modern_rf2.RData: predictions of modern relative abundance from the model fit to climate covariates
            - predicted_modern_rf3.RData: predictions of modern relative abunance from the model fit to the reduced number of covariates
            - predicted_modern_rf4.RData: predictions of modern relative abundance from the model fit to soil and climate covariates and the latitude and longitude of the grid cell
        - density: total stem density predictions
            - predicted_historical_rf1.RData: predictions of historical total stem density from the model fit to soil and climate covariates
            - predicted_historical_rf2.RData: predictions of historical total stem density from the model fit to climate covariates
            - predicted_historical_rf3.RData: predictions of historical total stem density from the model fit to the reduced number of covariates
            - predicted_historical_rf4.RData: predictions of historical relative abundance from the model fit to soil and climate covariates and the latitude and longitude of the grid cell
            - predicted_modern_rf1.RData: predictions of modern total stem density from the model fit to soil and climate covariates
            - predicted_modern_rf2.RData: predictions of modern total stem density from the model fit to climate covariates
            - predicted_modern_rf3.RData: predictions of modern total stem density from the model fit to the reduced number of covariates
            - predicted_modern_rf4.RData: predictions of modern relative abundance from the model fit to soil and climate covariates and the latitude and longitude of the grid cell
    - M: predictions from GAMs fit to modern data
        - abundance: relative abundance predictions
            - predicted_historical_rf1.RData: predictions of historical relative abundance from the model fit to soil and climate covariates
            - predicted_historical_rf2.RData: predictions of historical relative abundance from the model fit to climate covariates
            - predicted_historical_rf3.RData: predictions of historical relative abundance from the model fit to the reduced number of covariates
            - predicted_historical_rf4.RData: predictions of historical relative abundance from the model fit to soil and climate covariates and the latitude and longitude of the grid cell
            - predicted_modern_rf1.RData: predictions of modern relative abundance from the model fit to soil and climate covariates
            - predicted_modern_rf2.RData: predictions of modern relative abundance from the model fit to climate covariates
            - predicted_modern_rf3.RData: predictions of modern relative abunance from the model fit to the reduced number of covariates
            - predicted_modern_rf4.RData: predictions of modern relative abundance from the model fit to soil and climate covariates and the latitude and longitude of the grid cell
        - density: total stem density predictions
            - predicted_historical_rf1.RData: predictions of historical total stem density from the model fit to soil and climate covariates
            - predicted_historical_rf2.RData: predictions of historical total stem density from the model fit to climate covariates
            - predicted_historical_rf3.RData: predictions of historical total stem density from the model fit to the reduced number of covariates
            - predicted_historical_rf4.RData: predictions of historical relative abundance from the model fit to soil and climate covariates and the latitude and longitude of the grid cell
            - predicted_modern_rf1.RData: predictions of modern total stem density from the model fit to soil and climate covariates
            - predicted_modern_rf2.RData: predictions of modern total stem density from the model fit to climate covariates
            - predicted_modern_rf3.RData: predictions of modern total stem density from the model fit to the reduced number of covariates
            - predicted_modern_rf4.RData: predictions of modern relative abundance from the model fit to soil and climate covariates and the latitude and longitude of the grid cell

## scripts

Code for processing data, fitting models, making out-of-sample predictions, and plotting results. The code is broken into seven sections. The first section (**1.PLS**) organizes and formats vegetation, climate, and soil data for the historical time period. The second section (**2.FIA**) organizes and formats vegetation, climate, and soil data for the modern time period. The third section (**3.NLCD**) produces maps of modern vegetation cover for the Upper Midwest region. The fourth section (**4.RF_H2M**) fits random forests to historical total stem density and tree taxon relative abundances, and then predicts out-of-sample historical and modern total stem density and relative abundances. The fifth section (**5.RF_M2H**) fits random forests to modern total stem density and tree taxon relative abundances, and again predicts out-of-sample historical and modern total stem density and relative abundances. The sixth section (**6.GAM_H2M**) fits generalized additive models (GAMs) to historical total stem density and tree taxon relative abundances, and predicts out-of-sample historical and modern total stem density and relative abundances. Finally, the seventh section (**7.GAM_M2H**) fits GAMs to modern total stem density and tree taxon relative abundances, and again predicts out-of-sample historical and modern total stem density and relative abundances. This directory additionally includes one helper file (**install_packages.R**) for installing required packages, including versions.

### 1.PLS

- **1.1.Process_gridded_products.R**: Formatting PLS gridded data products. These were previously published PalEON data products.
    - Inputs: 
        - data/raw/gridded_density/PLS_Density_Point_Level2_v1.0.nc: estimated stem density from PLS period from DOI https://doi.org/10.6073/pasta/1b2632d48fc79b370740a7c20a70b4b0
        - data/raw/gridded-composition/SetTreeComp_Level2_v1.0.nc: estimated fractional composition/relative abundance from PLS period from DOI https://doi.org/10.6073/pasta/8544e091b64db26fdbbbafd0699fa4f9
    - Outputs:
        - data/processed/PLS/gridded_fcomp_density.RData: Total stem density and relative abundances for each taxon in each 8 x 8 km grid cell (rows) for the PLS period in one dataframe. Used in 1.2.Collate_data.R
- **1.2.Collate_data.R**: Combining climate, soil, and vegetation data. Individual dataframes aggregated to the 8 x 8 km grid are simply combined for running the models. All variables are plotted
    - Inputs:
        - data/processed/PLS/gridded_fcomp_density.RData: PLS-era total stem density and fractional composition in each grid cell
        - data/raw/soils/gridded_soil.RData: Gridded soil variables from gSSURGO. Created in separate repository: https://github.com/amwillson/historic-modern-environment/
        - data/raw/climate/gridded_climate.RData: Gridded climate variables from PRISM. Created in separate repository: https://github.com/amwillson/historic-modern-environment/
    - Outputs:
        - data/processed/PLS/xydata.RData: Gridded soil, climate, and vegetation variables on 8 x 8 km grid where each row is a grid cell. Used in 1.3.Split_data.R
- **1.3.Split_data.R**: Subsetting in-sample and out-of-sample data in PLS. Out-of-sample grid cells should have corresponding grid cells in FIA, meaning that I had to check which grid cells are available in each time period.
    - Inputs:
        - data/processed/PLS/xydata.RData: PLS combined dataset with vegetation, climate, and soil data. This is the dataframe we are splitting between in-sample and oos 
        - data/processed/FIA/gridded_all_plots.RData: FIA vegetation datase. Contains the grid cells that are available in the modern (FIA) period. Used to select corresponding grid cells in both time periods
    - Outputs:
        - data/processed/PLS/xydata_in.RData: PLS-era vegetation, climate, and soil data for only in-sample grid cells. Used in 4.1.fit_density_allcovar.R, 4.2.fit_density_climcovar.R, 4.3.fit_density_redcovar.R, 4.4.fit_density_xycovar.R, 4.8.fit_abundance_allcovar.R, 4.9.fit_abundance_climcovar.R, 4.10.fit_abundance_redcovar.R, 4.11.fit_abundance_xycovar.R, 6.1.fit_density_allcovar.R, 6.2.fit_density_climcovar.R, 6.3.fit_density_redcovar.R, 6.4.fit_density_xycovar.R, 6.8.fit_abundance_allcovar.R, 6.9.fit_abundance_climcovar.R, 6.10.fit_abundance_redcovar.R., 6.11.fit_abundance_xycovar.R
        - data/processed/PLS/xydata_out.RData: PLS-era vegetation, climate, and soil data for only out-of-sample grid cells. Used in 4.5.density_historical_predictions.R, 4.6.density_modern_predictions.R, 4.12.abundance_historical_predictions.R, 4.13.abundance_modern_predictions.R, 5.5.density_historical_predictions.R, 5.6.density_modern_predictions.R, 5.12.abundance_historical_predictions.R, 5.13.abundance_modern_predictions.R, 6.5.density_historical_predictions.R, 6.6.density_modern_predictions.R, 6.12.abundance_historical_predictions.R, 6.13.abundance_modern_predictions.R, 7.5.density_historical_predictions.R, 7.6.density_modern_predictions.R, 7.12.abundance_historical_predictions.R, 7.13.abundance_modern_predictions.R

### 2.FIA

- **2.1.Join_FIA.R**: Joining FIA data. I downloaded COND, PLOT, SUBPLOT, and TREE tables on 08 August 2024. I joined all the tables using relation IDs following the user manual available at data/raw/FIA_08082024/manual.pdf and advice from FIA/USFS staff.
    - Inputs:
        - data/raw/FIA_08082024/IL_COND.csv: Condition table for Illinois. Contains information on the conditions present on each plot. Used to filter for the forested condition.
        - data/raw/FIA_08082024/IN_COND.csv: Condition table for Indiana. Contains information on the conditions present on each plot. Used to filter for the forested condition
        - data/raw/FIA_08082024/MI_COND.csv: Condition table for Michigan. Contains information on the conditions present on each plot. Used to filter for the forested condition
        - data/raw/FIA_08082024/MN_COND.csv: Condition table for Minnesota. Contains information on the conditions present on each plot. Used to filter for the forested condition
        - data/raw/FIA_08082024/WI_COND.csv: Condition table for Wisconsin. Contains information on the conditions present on each plot. Used to filter for the forested condition
        - data/raw/FIA_08082024/IL_PLOT.csv: Plot table for Illinois. Contains information about the whole plot. Used for coordinates and to filter for sampled plots.
        - data/raw/FIA_08082024/IN_PLOT.csv: Plot table for Indiana. Contains information about the whole plot. Used for coordinates and to filter for sampled plots.
        - data/raw/FIA_08082024/MI_PLOT.csv: Plot table for Michigan. Contains information about the whole plot. Used for coordinates and to filter for sampled plots.
        - data/raw/FIA_08082024/MN_PLOT.csv: Plot table for Minnesota. Contains information about the whole plot. Used for coordinates and to filter for sampled plots.
        - data/raw/FIA_08082024/WI_PLOT.csv: Plot table for Wisconsin. Contains information about the whole plot. Used for coordinates and to filter for sampled plots.
        - data/raw/FIA_08082024/IL_SUBPLOT.csv: Subplot table for Illinois. Contains which subplot each tree is on. Used to filter for only the main subplots that can be used to calculate stand-level variables.
        - data/raw/FIA_08082024/IN_SUBPLOT.csv: Subplot table for Indiana. Contains which subplot each tree is on. Used to filter for only the main subplots that can be used to calculate stand-level variables.
        - data/raw/FIA_08082024/MI_SUBPLOT.csv: Subplot table for Michigan. Contains which subplot each tree is on. Used to filter for only the main subplots that can be used to calculate stand-level variables.
        - data/raw/FIA_08082024/MN_SUBPLOT.csv: Subplot table for Minnesota. Contains which subplot each tree is on. Used to filter for only the main subplots that can be used to calculate stand-level variables.
        - data/raw/FIA_08082024/WI_SUBPLOT.csv: Subplot table for Wisconsin. Contains which subplot each tree is on. Used to filter for only the main subplots that can be used to calculate stand-level variables.
        - data/raw/FIA_08082024/IL_TREE.csv: Tree table for Illinois. Contains information on each tree in the plots/subplots. Used to find total stem density and relative abundance using TPA_UNADJ and species identity.
        - data/raw/FIA_08082024/IN_TREE.csv: Tree table for Indiana. Contains information on each tree in the plots/subplots. Used to find total stem density and relative abundance using TPA_UNADJ and species identity.
        - data/raw/FIA_08082024/MI_TREE.csv: Tree table for Michigan. Contains information on each tree in the plots/subplots. Used to find total stem density and relative abundance using TPA_UNADJ and species identity.
        - data/raw/FIA_08082024/MN_TREE.csv: Tree table for Minnesota. Contains information on each tree in the plots/subplots. Used to find total stem density and relative abundance using TPA_UNADJ and species identity
        - data/raw/FIA_08082024/WI_TREE.csv: Tree table for Wisconsin. Contains information on each tree in the plots/subplots. Used to find total stem density and relative abundance using TPA_UNADJ and species identity
    - Outputs:
        - data/intermediate/combined_COND_PLOT_TREE_SPECIES.RData: Contains all tree-level data from which total stem density and relative abundance are calculated. Used in 2.2.Estimate_density_composition.R
- **2.2.Estimate_density_composition.R**: Processing FIA data to produce gridded estimates of total stem density and fractional composition. This was done two ways: (1) with only the plots that I know the location of from previous PalEON work (so I kno wwhich grid cell each plot falls within), and (2) with all plots, estimating the grid cell they fall in where I don't know the exact coordinates.
    - Inputs:
        - data/intermediate/combined_COND_PLOT_TREE_SPECIES.RData: Contains all the tree and plot information necessary for calculating grid-level total stem density and relative abundance
        - data/conversions/fia_plaeongrid_albers.csv: Contains the standard PalEON 8 x 8 km grid that I aggregate FIA plots to
        - data/conversions/FIA_conversion_v03.csv: Contains the FIA species code-to-PLS taxon conversions previously used by the PalEON team
    - Outputs:
        - data/processed/FIA/gridded_only_known_plots.RData: Contains gridded FIA total stem density and relative abundance for only the plots that we know which grid cell they are in from unswapped/unfuzzed coordinates. This is NOT currently used because I favored using all the data even if their exact locations are more uncertain
        - data/processed/FIA/gridded_all_plots.RData: Contains gridded FIA total stem density and relative abundance for all plots, with the uncertain plots assigned to a grid cell based on swapped & fuzzed coordinates. Used in 2.3.Collate_modern_data.R
- **2.3.Collate_modern_data.R**: Collate modern data. Combining vegetation, climate, and soil data from PLS period.
    - Inputs:
        - data/processed/FIA/gridded_all_plots.RData: Gridded FIA-derived total stem density and relative abundance
        - data/raw/climate/gridded_climate_modern.RData: Gridded climate variables from PRISM. Created in a separate repository: https://github.com/amwillson/historic-modern-environment/
    - Outputs:
        - data/processed/FIA/xydata.RData: Dataframe with vegetation, soil, and climate data for each grid cell (rows). Used in 2.4.Split_data.R
- **2.4.Split_data.R**: Splitting FIA data into in-sample and OOS data. I follow the in-sample/OOS splits from the PLS data since I want the same OOS samples.
    - Inputs:
        - data/processed/FIA/xydata.RData: Formatted dataframe with FIA vegetation data, soil, and climate data for all grid cells.
        - data/processed/PLS/xydata_out.RData: Out-of-sample grid cells from historical period. Used to match up out-of-sample grid cells between time periods
    - Outputs:
        - data/processed/FIA/xydata_in.RData: In-sample grid cells of the modern period vegetation, soil, and climate data. Used in 5.1.fit_density_allcovar.R, 5.2.fit_density_climcovar.R, 5.3.fit_density_redcovar.R, 5.4.fit_density_xycovar.R, 5.8.fit_abundance_allcovar.R, 5.9.fit_abundance_climcovar.R, 5.10.fit_abundance_redcovar.R, 5.11.fit_abundance_xycovar.R, 7.1.fit_density_allcovar.R, 7.2.fit_density_climcovar.R, 7.3.fit_density_redcovar.R, 7.4.fit_density_xycovar.R, 7.8.fit_abundance_allcovar.R, 7.9.fit_abundance_climcovar.R, 7.10.fit_abundance_redcovar.R, 7.11.fit_abundance_xycovar.R
        - data/processed/FIA_xydata_out.RData: Out-of-sample grid cells of the modern period vegetation, soil, and climate data. Used in 4.6.density_modern_predictions.R, 4.13.abundance_modern_predictions.R, 5.6.density_modern_predictions.R, 5.13.abundance_modern_predictions.R, 6.6.density_modern_predictions.R, 6.13.abundance_modern_predictions.R, 7.6.density_modern_predictions.R, 7.13.abundance_modern_predictions.R

### 3.NLCD

**PLACEHOLDER**

### 4.RF_H2M

- **4.1.fit_density_allcovar.R**: Univariate random forest fit to historical total stem density and climate and soil covariates.
    - Inputs:
        - data/processed/PLS/xydata_in.RData: Dataframe of in-sample grid cells with historical (PLS) vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/density/allcovar.RData: Fitted random forest object saved to external hard drive. Used in 4.5.density_historical_predictions.R, 4.6.density_modern_predictions.R
- **4.2.fit_density_climcovar.R**: Univariate random forest fit to historical total stem density and climate covariates only.
    - Inputs: 
        - data/processed/PLS/xydata_in.RData: Dataframe of in-sample grid cells with historical (PLS) vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/density/climcovar.RData: Fitted random forest object saved to external hard drive. Used in 4.5.density_historical_predictions.R, 4.6.density_modern_predictions.R
- **4.3.fit_density_redcovar.R**: Univariate random forest fit to historical total stem density and reduced covariates. Reduced covariates refers to using a subset of all the covariates based on importance and minimum depth analysis performed in step 4-1: precipitation seasonality, maximum annual temperature, total annual precipitation, soil % clay.
    - Inputs:
        - data/processed/PLS/xydata_in.RData: Dataframe of in-sample grid cells with historical (PLS) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/density/redcovar.RData: Fitted random forest object saved to external hard drive. Used in 4.5.density_historical_predictions.R, 4.6.density_modern_predictions.R
- **4.4.fit_density_xycovar.R**: Univariate random forest fit to historical total stem density and climate and soil covariates plus coordinates.
    - Inputs:
        - data/processed/PLS/xydata_in.RData: Dataframe of in-sample grid cells with historical (PLS) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/density/xycovar.RData: Fitted random forest object saved to external hard drive. Used in 4.5.density_historical_predictions.R, 4.6.density_modern_predictions.R
- **4.5.density_historical_predictions.R**: Predict historical total stem density from historical fitted random forests
    - Inputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/density/allcovar.RData: Fitted random forest using all climate and soil covariates. Used tp make predictions from the main model with climate and soil covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/density/climcovar.RData: Fitted random forest using only climate covariates. Used to make predictions from the alternate model with only climate covariates
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/density/redcovar.RData: Fitted random forest using only the reduced set of covariates. Used to make predictions from the alternate model with a reduced set of covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/density/xycovar.RData: Fitted random forest using all the soil and climate covariates plus the latitude and longitude of the grid cell. Used to make predictions from the alternate model including grid cell coordinates and environmental covariates.
        - data/processed/PLS/xydata_out.RData: Dataframe containing the out-of-sample historical (PLS) vegetation, soil, and climate data. The covariates are used to make predictiosn of the out-of-sample historical vegetation data.
    - Outputs:
        - out/rf/H/density/predicted_historical_rf1.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical total stem density from the out-of-sample grid cells using the main model with climate and soil covariates. Used in 4.7.density_figures.R.
        - out/rf/H/density/predicted_historical_rf2.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical total stem density from the out-of-sample grid cells using the alternate model with only climate covariates. Used in 4.7.density_figures.R
        - out/rf/H/density/predicted_historical_rf3.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical total stem density from the out-of-sample grid cells using the alternate model with only the reduced set of covariates. Used in 4.7.density_figures.R
        - out/rf/H/density/predicted_historical_rf4.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical total stem density from the out-of-sample grid cells using the alternate model with the soil and climate covariates as well as the latitude and longitude of the grid cell. Used in 4.7.density_figures.R
- **4.6.density_modern_predictions.R**: Predicted modern total stem density from historical fitted random forests
    - Inputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/density/allcovar.RData: Fitted random forest using all climate and soil covariates. Used tp make predictions from the main model with climate and soil covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/density/climcovar.RData: Fitted random forest using only climate covariates. Used to make predictions from the alternate model with only climate covariates
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/density/redcovar.RData: Fitted random forest using only the reduced set of covariates. Used to make predictions from the alternate model with a reduced set of covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/density/xycovar.RData: Fitted random forest using all the soil and climate covariates plus the latitude and longitude of the grid cell. Used to make predictions from the alternate model including grid cell coordinates and environmental covariates.
        - data/processed/FIA/xydata_out.rData: Dataframe containing the out-of-sample modern (FIA) vegetation, soil, and climate data. The covariates are used to make predictions of the out-of-sample modern vegetation data
    - Outputs:
        - out/rf/H/density/predicted_modern_rf1.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern total stem density from the out-of-sample grid cells using the main model with climate and soil covariates. Used in 4.7.density_figures.R.
        - out/rf/H/density/predicted_modern_rf2.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern total stem density from the out-of-sample grid cells using the alternate model with only climate covariates. Used in 4.7.density_figures.R
        - out/rf/H/density/predicted_modern_rf3.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern total stem density from the out-of-sample grid cells using the alternate model with only the reduced set of covariates. Used in 4.7.density_figures.R
        - out/rf/H/density/predicted_modern_rf4.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern total stem density from the out-of-sample grid cells using the alternate model with the soil and climate covariates as well as the latitude and longitude of the grid cell. Used in 4.7.density_figures.R
- **4.7.density_figures.R**: Plot out-of-sample predictions from historical model predicting historical and modern total stem density
    - Inputs:
        - out/rf/H/density/predicted_historical_rf1.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical total stem density from main model with soil and climate covariates
        - out/rf/H/density/predicted_historical_rf2.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical total stem density from alternate model with only climate covariates
        - out/rf/H/density/predicted_historical_rf3.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical total stem density from alternate model with the reduced set of covariates
        - out/rf/H/density/predicted_historical_rf4.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical total stem density from alternate model with soil and climate covariates as well as latitude and longitude of the grid cell
        - out/rf/H/density/predicted_modern_rf1.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern total stem density from the main model with soil and climate covariates
        - out/rf/H/density/predicted_modern_rf2.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern total stem density from alternate model with only climate covariates
        - out/rf/H/density/predicted_modern_rf3.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern total stem density from alternate model with the reduced set of covariates
        - out/rf/H/density/predicted_modern_rf4.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern total stem density from alternate model with soil and climate covariates as well as latitude and longitude of the grid cell
    - Outputs: none
- **4.8.fit_abundance_allcovar.R**: Multivariate random forest fit to historical relative abundance and climate and soil covariates
    - Inputs:
        - data/processed/PLS/xydata_in.RData: Dataframe of in-sample grid cells with historical (PLS) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/allcovar.RData: Fitted random forest object saved to external hard drive. Used in 4.12.abundance_historical_predictions.R, 4.13.abundance_modern_predictions.R
- **4.9.fit_abundance_climcovar.R**: Multivariate random forest fit to historical relative abundance and climate covariates only
    - Inputs:
        - data/processed/PLS/xydata_in.RData: Dataframe of in-sample grid cells with historical (PLS) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/climcovar.RData: Fitted random forest object saved to external hard drive. Used in 4.12.abundance_historical_predictions.R, 4.13.abundance_modern_predictions.R
- **4.10.fit_abundance_redcovar.R**: Multivariate random forest fit to historical relative abundance and reduced covariates. Reduced covariates refers to using a subset of all the covariates based on importance and minimum depth analysis performed in step 4-8: maximum annual temperature, precipitation seasonality, total annual precipitation, and soil % clay
    - Inputs:
        - data/processed/PLS/xydata_in.RData: Dataframe of in-sample grid cells with historical (PLS) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/redcovar.RData: Fitted random forest object saved to external hard drive. Used in 4.12.abundance_historical_predictions.R, 4.13.abundance_modern_predictions.R
- **4.11.fit_abundance_xycovar.R**: Multivariate random forest fit to historical relative abundance and climate and soil covariates plus coordinates
    - Inputs:
        - data/processed/PLS/xydata_in.RData: Dataframe of in-sample grid cells with historical (PLS) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/xycovar.RData: Fitted random forest object saved to external hard drive. Used in 4.12.abundance_historical_predictions.R, 4.13.abundance_modern_predictions.R
- **4.12.abundance_historical_predictions.R**: Predict historical relative abundance from historical fitted random forests
    - Inputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/allcovar.RData: Fitted random forest using all climate and soil covariates. Used to make predictions from the main model with climate and soil covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/climcovar.RData: Fitted random forest using only climate covariates. Used to make predictions from the alternate model with only climate covariates
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/redcovar.RData: Fitted random forest using only the reduced set of covariates. Used to make predictions from the alternate model with a reduced set of covariates
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/xycovar.RData: Fitted random forest using all the soil and climate covariates plus the latitude and longitude of the grid cell. Used to make predictions from the alternate model including grid cell coordinates and environmental covariates
        - data/processed/PLS/xydata_out.RData: Dataframe containing the out-of-sample historical (PLS) vegetation, soil, and climate data. The covariates are used to make predictions of the out-of-sample historical vegetation
    - Outputs:
        - out/rf/H/abundance/predicted_historical_rf1.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical relative abundances from the out-of-sample grid cells using the main model with climate and soil covariates. Used in 4.14.abundance_figures.R
        - out/rf/H/abundance/predicted_historical_rf2.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical relative abundances from the out-of-sample grid cells using the alternate model with only climate covariates. Used in 4.14.abundance_figures.R
        - out/rf/H/abundance/predicted_historical_rf3.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical abundances from the out-of-sample grid cells using the alternate model with only the reduced set of covariates. Used in 4.14.abundance_figures.R
        - out/rf/H/abundance/predicted_historical_rf4.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical relative abundances from the out-of-sample grid cells using the alternate model with the soil and climate covariates as well as the latitude and longitude of the grid cell. Used in 4.14.abundance_figures.R
- **4.13.abundance_modern_predictions.R**: Predict modern relative abundance from historical fitted random forests
    - Inputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/allcovar.RData: Fitted random forest using all climate and soil covariates. Used to make predictions from the main model with climate and soil covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/climcovar.RData: Fitted random forest using only climate covariates. Used to make predictions from the alternate model with only climate covariates
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/redcovar.RData: Fitted random forest using only the reduced set of covariates. Used to make predictions from the alternate model with a reduced set of covariates
        - /Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/xycovar.RData: Fitted random forest using all the soil and climate covariates plus the latitude and longitude of the grid cell. Used to make predictions from the alternate model including grid cell coordinates and environmental covariates
        - data/processed/FIA/xydata_out.RData: Dataframe containing the out-of-sample modern (FIA) vegetation, soil, and climate data. The covariates are used to make predictions of the out-of-sample modern vegetation data
    - Outputs:
        - out/rf/H/abundance/predicted_modern_rf1.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern relative abundances from the out-of-sample grid cells using the main model with climate and soil covariates. Used in 4.14.abundance_figures.R
        - out/rf/H/abundance/predicted_modern_rf2.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern relative abundances from the out-of-sample grid cells using the alternate model with only climate covariates. Used in 4.14.abundance_figures.R
        - out/rf/H/abundance/predicted_modern_rf3.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern abundances from the out-of-sample grid cells using the alternate model with only the reduced set of covariates. Used in 4.14.abundance_figures.R
        - out/rf/H/abundance/predicted_modern_rf4.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern relative abundances from the out-of-sample grid cells using the alternate model with the soil and climate covariates as well as the latitude and longitude of the grid cell. Used in 4.14.abundance_figures.R
- **4.14.abundance_figures.R**: Plot out-of-sample predictions from historical model predicting historical and modern relative abundance
    - Inputs:
        - out/rf/H/abundance/predicted_historical_rf1.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical relative abundances from main model with soil and climate covariates
        - out/rf/H/abundance/predicted_historical_rf2.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical relative abundances from alternate model with only climate covariates
        - out/rf/H/abundance/predicted_historical_rf3.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical relative abundances from alternate model with the reduced set of covariates
        - out/rf/H/abundance/predicted_historical_rf4.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical relative abundances from alternate model with soil and climate covariates as well as latitude and longitude of the grid cell
        - out/rf/H/abundance/predicted_modern_rf1.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern relative abundances from main model with soil and climate covariates
        - out/rf/H/abundance/predicted_modern_rf2.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern relative abundances from alternate model with only climate covariates
        - out/rf/H/abundance/predicted_modern_rf3.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern relative abundances from alternate model with the reduced set of covariates
        - out/rf/H/abundance/predicted_modern_rf4.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern relative abundances from alternate model with soil and climate covariates as well as latitude and longitude of the grid cell
    - Outputs: none

### 5.RF_M2H

- **5.1.fit_density_allcovar.R**: Univariate random forest fit to modern total stem density and climate and soil covariates.
    - Inputs:
        - data/processed/FIA/xydata_in.RData: Dataframe of in-sample grid cells with modern (FIA) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/density/allcovar.RData: Fitted random forest object saved to external hard drive. Used in 5.5.density_historical_predictions.R, 5.6.density_modern_predictions.R
- **5.2.fit_density_climcovar.R**: Univariate random forest fit to modern total stem density and climate covariates only.
    - Inputs: 
        - data/processed/FIA/xydata_in.RData: Dataframe of in-sample grid cells with modern (FIA) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/density/climcovar.RData: Fitted random forest object saved to external hard drive. Used in 5.5.density_historical_predictions.R, 5.6.density_modern_predictions.R
- **5.3.fit_density_redcovar.R**: Univariate random forest fit to modern total stem density and reduced covariates. Reduced covariates refers to using a subset of all the covariates based on importance and minimum depth analysis performed in step 4-1: precipitation seasonality, maximum annual temperature, temperature seasonality, soil % clay.
    - Inputs:
        - data/processed/FIA/xydata_in.RData: Dataframe of in-sample grid cells with modern (FIA) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/density/redcovar.RData: Fitted random forest object saved to external hard drive. Used in 5.5.density_historical_predictions.R, 5.6.density_modern_predictions.R
- **5.4.fit_density_xycovar.R**: Univariate random forest fit to modern total stem density and climate and soil covariates plus coordinates.
    - Inputs:
        - data/processed/FIA/xydata_in.RData: Dataframe of in-sample grid cells with modern (FIA) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/density/xycovar.RData: Fitted random forest object saved to external hard drive. Used in 5.5.density_historical_predictions.R, 5.6.density_modern_predictions.R
- **5.5.density_historical_predictions.R**: Predict historical total stem density from modern fitted random forests
    - Inputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/density/allcovar.RData: Fitted random forest using all climate and soil covariates. Used to make predictions from the main model with climate and soil covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/density/climcovar.RData: Fitted random forest using only climate covariates. Used to make predictions from the alternate model with only climate covariates
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/density/redcovar.RData: Fitted random forest using only the reduced set of covariates. Used to make predictions from the alternate model with a reduced set of covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/density/xycovar.RData: Fitted random forest using all the soil and climate covariates plus the latitude and longitude of the grid cell. Used to make predictions from the alternate model including grid cell coordinates and environmental covariates.
        - data/processed/PLS/xydata_out.RData: Dataframe containing the out-of-sample historical (PLS) vegetation, soil, and climate data. The covariates are used to make predictiosn of the out-of-sample historical vegetation data.
    - Outputs:
        - out/rf/M/density/predicted_historical_rf1.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical total stem density from the out-of-sample grid cells using the main model with climate and soil covariates. Used in 5.7.density_figures.R.
        - out/rf/M/density/predicted_historical_rf2.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical total stem density from the out-of-sample grid cells using the alternate model with only climate covariates. Used in 5.7.density_figures.R
        - out/rf/M/density/predicted_historical_rf3.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical total stem density from the out-of-sample grid cells using the alternate model with only the reduced set of covariates. Used in 5.7.density_figures.R
        - out/rf/M/density/predicted_historical_rf4.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical total stem density from the out-of-sample grid cells using the alternate model with the soil and climate covariates as well as the latitude and longitude of the grid cell. Used in 5.7.density_figures.R
- **5.6.density_modern_predictions.R**: Predicted modern total stem density from modern fitted random forests
    - Inputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/density/allcovar.RData: Fitted random forest using all climate and soil covariates. Used to make predictions from the main model with climate and soil covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/density/climcovar.RData: Fitted random forest using only climate covariates. Used to make predictions from the alternate model with only climate covariates
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/density/redcovar.RData: Fitted random forest using only the reduced set of covariates. Used to make predictions from the alternate model with a reduced set of covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/density/xycovar.RData: Fitted random forest using all the soil and climate covariates plus the latitude and longitude of the grid cell. Used to make predictions from the alternate model including grid cell coordinates and environmental covariates.
        - data/processed/FIA/xydata_out.rData: Dataframe containing the out-of-sample modern (FIA) vegetation, soil, and climate data. The covariates are used to make predictions of the out-of-sample modern vegetation data
    - Outputs:
        - out/rf/M/density/predicted_modern_rf1.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern total stem density from the out-of-sample grid cells using the main model with climate and soil covariates. Used in 5.7.density_figures.R.
        - out/rf/M/density/predicted_modern_rf2.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern total stem density from the out-of-sample grid cells using the alternate model with only climate covariates. Used in 5.7.density_figures.R
        - out/rf/M/density/predicted_modern_rf3.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern total stem density from the out-of-sample grid cells using the alternate model with only the reduced set of covariates. Used in 5.7.density_figures.R
        - out/rf/M/density/predicted_modern_rf4.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern total stem density from the out-of-sample grid cells using the alternate model with the soil and climate covariates as well as the latitude and longitude of the grid cell. Used in 5.7.density_figures.R
- **5.7.density_figures.R**: Plot out-of-sample predictions from modern model predicting historical and modern total stem density
    - Inputs:
        - out/rf/M/density/predicted_historical_rf1.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical total stem density from main model with soil and climate covariates
        - out/rf/M/density/predicted_historical_rf2.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical total stem density from alternate model with only climate covariates
        - out/rf/M/density/predicted_historical_rf3.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical total stem density from alternate model with the reduced set of covariates
        - out/rf/M/density/predicted_historical_rf4.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical total stem density from alternate model with soil and climate covariates as well as latitude and longitude of the grid cell
        - out/rf/M/density/predicted_modern_rf1.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern total stem density from the main model with soil and climate covariates
        - out/rf/M/density/predicted_modern_rf2.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern total stem density from alternate model with only climate covariates
        - out/rf/M/density/predicted_modern_rf3.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern total stem density from alternate model with the reduced set of covariates
        - out/rf/M/density/predicted_modern_rf4.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern total stem density from alternate model with soil and climate covariates as well as latitude and longitude of the grid cell
    - Outputs: none
- **5.8.fit_abundance_allcovar.R**: Multivariate random forest fit to modern relative abundance and climate and soil covariates
    - Inputs:
        - data/processed/FIA/xydata_in.RData: Dataframe of in-sample grid cells with modern (FIA) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/abundance/allcovar.RData: Fitted random forest object saved to external hard drive. Used in 5.12.abundance_historical_predictions.R, 5.13.abundance_modern_predictions.R
- **5.9.fit_abundance_climcovar.R**: Multivariate random forest fit to modern relative abundance and climate covariates only
    - Inputs:
        - data/processed/FIA/xydata_in.RData: Dataframe of in-sample grid cells with modern (FIA) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/abundance/climcovar.RData: Fitted random forest object saved to external hard drive. Used in 5.12.abundance_historical_predictions.R, 5.13.abundance_modern_predictions.R
- **5.10.fit_abundance_redcovar.R**: Multivariate random forest fit to modern relative abundance and reduced covariates. Reduced covariates refers to using a subset of all the covariates based on importance and minimum depth analysis performed in step 4-8: maximum annual temperature, temperature seasonality, total annual precipitation, and soil % clay
    - Inputs:
        - data/processed/FIA/xydata_in.RData: Dataframe of in-sample grid cells with modern (FIA) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/abundance/redcovar.RData: Fitted random forest object saved to external hard drive. Used in 5.12.abundance_historical_predictions.R, 5.13.abundance_modern_predictions.R
- **5.11.fit_abundance_xycovar.R**: Multivariate random forest fit to modern relative abundance and climate and soil covariates plus coordinates
    - Inputs:
        - data/processed/FIA/xydata_in.RData: Dataframe of in-sample grid cells with modern (FIA) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/abundance/xycovar.RData: Fitted random forest object saved to external hard drive. Used in 5.12.abundance_historical_predictions.R, 5.13.abundance_modern_predictions.R
- **5.12.abundance_historical_predictions.R**: Predict historical relative abundance from modern fitted random forests
    - Inputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/abundance/allcovar.RData: Fitted random forest using all climate and soil covariates. Used to make predictions from the main model with climate and soil covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/abundance/climcovar.RData: Fitted random forest using only climate covariates. Used to make predictions from the alternate model with only climate covariates
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/abundance/redcovar.RData: Fitted random forest using only the reduced set of covariates. Used to make predictions from the alternate model with a reduced set of covariates
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/abundance/xycovar.RData: Fitted random forest using all the soil and climate covariates plus the latitude and longitude of the grid cell. Used to make predictions from the alternate model including grid cell coordinates and environmental covariates
        - data/processed/PLS/xydata_out.RData: Dataframe containing the out-of-sample historical (PLS) vegetation, soil, and climate data. The covariates are used to make predictions of the out-of-sample historical vegetation
    - Outputs:
        - out/rf/M/abundance/predicted_historical_rf1.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical relative abundances from the out-of-sample grid cells using the main model with climate and soil covariates. Used in 5.14.abundance_figures.R
        - out/rf/M/abundance/predicted_historical_rf2.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical relative abundances from the out-of-sample grid cells using the alternate model with only climate covariates. Used in 5.14.abundance_figures.R
        - out/rf/M/abundance/predicted_historical_rf3.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical abundances from the out-of-sample grid cells using the alternate model with only the reduced set of covariates. Used in 5.14.abundance_figures.R
        - out/rf/M/abundance/predicted_historical_rf4.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical relative abundances from the out-of-sample grid cells using the alternate model with the soil and climate covariates as well as the latitude and longitude of the grid cell. Used in 5.14.abundance_figures.R
- **5.13.abundance_modern_predictions.R**: Predict modern relative abundance from modern fitted random forests
    - Inputs:
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/abundance/allcovar.RData: Fitted random forest using all climate and soil covariates. Used to make predictions from the main model with climate and soil covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/abundance/climcovar.RData: Fitted random forest using only climate covariates. Used to make predictions from the alternate model with only climate covariates
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/abundance/redcovar.RData: Fitted random forest using only the reduced set of covariates. Used to make predictions from the alternate model with a reduced set of covariates
        - /Volumes/FileBackup/SDM_bigdata/out/rf/M/abundance/xycovar.RData: Fitted random forest using all the soil and climate covariates plus the latitude and longitude of the grid cell. Used to make predictions from the alternate model including grid cell coordinates and environmental covariates
        - data/processed/FIA/xydata_out.RData: Dataframe containing the out-of-sample modern (FIA) vegetation, soil, and climate data. The covariates are used to make predictions of the out-of-sample modern vegetation data
    - Outputs:
        - out/rf/M/abundance/predicted_modern_rf1.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern relative abundances from the out-of-sample grid cells using the main model with climate and soil covariates. Used in 5.14.abundance_figures.R
        - out/rf/M/abundance/predicted_modern_rf2.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern relative abundances from the out-of-sample grid cells using the alternate model with only climate covariates. Used in 5.14.abundance_figures.R
        - out/rf/M/abundance/predicted_modern_rf3.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern abundances from the out-of-sample grid cells using the alternate model with only the reduced set of covariates. Used in 5.14.abundance_figures.R
        - out/rf/M/abundance/predicted_modern_rf4.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern relative abundances from the out-of-sample grid cells using the alternate model with the soil and climate covariates as well as the latitude and longitude of the grid cell. Used in 5.14.abundance_figures.R
- **5.14.abundance_figures.R**: Plot out-of-sample predictions from modern model predicting historical and modern relative abundance
    - Inputs:
        - out/rf/M/abundance/predicted_historical_rf1.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical relative abundances from main model with soil and climate covariates
        - out/rf/M/abundance/predicted_historical_rf2.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical relative abundances from alternate model with only climate covariates
        - out/rf/M/abundance/predicted_historical_rf3.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical relative abundances from alternate model with the reduced set of covariates
        - out/rf/M/abundance/predicted_historical_rf4.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical relative abundances from alternate model with soil and climate covariates as well as latitude and longitude of the grid cell
        - out/rf/M/abundance/predicted_modern_rf1.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern relative abundances from main model with soil and climate covariates
        - out/rf/M/abundance/predicted_modern_rf2.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern relative abundances from alternate model with only climate covariates
        - out/rf/M/abundance/predicted_modern_rf3.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern relative abundances from alternate model with the reduced set of covariates
        - out/rf/M/abundance/predicted_modern_rf4.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern relative abundances from alternate model with soil and climate covariates as well as latitude and longitude of the grid cell
    - Outputs: none

### 6.GAM_H2M -- COME BACK

- **6.1.fit_density_allcovar.R**: Univariate GAM fit to historical total stem density and climate and soil covariates.
    - Inputs:
        - data/processed/PLS/xydata_in.RData: Dataframe of in-sample grid cells with historical (PLS) vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/density/allcovar.RData: Fitted GAM object saved to external hard drive. Used in 6.5.density_historical_predictions.R, 6.6.density_modern_predictions.R
- **6.2.fit_density_climcovar.R**: Univariate GAM fit to historical total stem density and climate covariates only.
    - Inputs: 
        - data/processed/PLS/xydata_in.RData: Dataframe of in-sample grid cells with historical (PLS) vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/density/climcovar.RData: Fitted GAM object saved to external hard drive. Used in 6.5.density_historical_predictions.R, 6.6.density_modern_predictions.R
- **6.3.fit_density_redcovar.R**: Univariate GAM fit to historical total stem density and reduced covariates. Reduced covariates refers to using a subset of all the covariates based on importance and minimum depth analysis performed in step 6-1: soil % sand, soil calcium carbonate concentration, soil available water content, fraction of grid cell in a floodplain, total annual precipitation, precipitation seasonality
    - Inputs:
        - data/processed/PLS/xydata_in.RData: Dataframe of in-sample grid cells with historical (PLS) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/density/redcovar.RData: Fitted random forest object saved to external hard drive. Used in 6.5.density_historical_predictions.R, 6.6.density_modern_predictions.R
- **6.4.fit_density_xycovar.R**: Univariate GAM fit to historical total stem density and climate and soil covariates plus coordinates.
    - Inputs:
        - data/processed/PLS/xydata_in.RData: Dataframe of in-sample grid cells with historical (PLS) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/density/xycovar.RData: Fitted GAM object saved to external hard drive. Used in 6.5.density_historical_predictions.R, 6.6.density_modern_predictions.R
- **6.5.density_historical_predictions.R**: Predict historical total stem density from historical fitted GAMs
    - Inputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/density/allcovar.RData: Fitted GAM using all climate and soil covariates. Used tp make predictions from the main model with climate and soil covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/density/climcovar.RData: Fitted GAM using only climate covariates. Used to make predictions from the alternate model with only climate covariates
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/density/redcovar.RData: Fitted GAM using only the reduced set of covariates. Used to make predictions from the alternate model with a reduced set of covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/density/xycovar.RData: Fitted GAM using all the soil and climate covariates plus the latitude and longitude of the grid cell. Used to make predictions from the alternate model including grid cell coordinates and environmental covariates.
        - data/processed/PLS/xydata_out.RData: Dataframe containing the out-of-sample historical (PLS) vegetation, soil, and climate data. The covariates are used to make predictiosn of the out-of-sample historical vegetation data.
    - Outputs:
        - out/gam/H/density/predicted_historical_gam1.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical total stem density from the out-of-sample grid cells using the main model with climate and soil covariates. Used in 6.7.density_figures.R.
        - out/gam/H/density/predicted_historical_gam2.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical total stem density from the out-of-sample grid cells using the alternate model with only climate covariates. Used in 6.7.density_figures.R
        - out/gam/H/density/predicted_historical_gam3.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical total stem density from the out-of-sample grid cells using the alternate model with only the reduced set of covariates. Used in 6.7.density_figures.R
        - out/gam/H/density/predicted_historical_gam4.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical total stem density from the out-of-sample grid cells using the alternate model with the soil and climate covariates as well as the latitude and longitude of the grid cell. Used in 6.7.density_figures.R
- **6.6.density_modern_predictions.R**: Predicted modern total stem density from historical fitted GAMs
    - Inputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/density/allcovar.RData: Fitted GAM using all climate and soil covariates. Used tp make predictions from the main model with climate and soil covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/density/climcovar.RData: Fitted GAM using only climate covariates. Used to make predictions from the alternate model with only climate covariates
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/density/redcovar.RData: Fitted GAM using only the reduced set of covariates. Used to make predictions from the alternate model with a reduced set of covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/density/xycovar.RData: Fitted GAM using all the soil and climate covariates plus the latitude and longitude of the grid cell. Used to make predictions from the alternate model including grid cell coordinates and environmental covariates.
        - data/processed/FIA/xydata_out.rData: Dataframe containing the out-of-sample modern (FIA) vegetation, soil, and climate data. The covariates are used to make predictions of the out-of-sample modern vegetation data
    - Outputs:
        - out/gam/H/density/predicted_modern_gam1.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern total stem density from the out-of-sample grid cells using the main model with climate and soil covariates. Used in 6.7.density_figures.R.
        - out/gam/H/density/predicted_modern_gam2.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern total stem density from the out-of-sample grid cells using the alternate model with only climate covariates. Used in 6.7.density_figures.R
        - out/gam/H/density/predicted_modern_gam3.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern total stem density from the out-of-sample grid cells using the alternate model with only the reduced set of covariates. Used in 6.7.density_figures.R
        - out/gam/H/density/predicted_modern_gam4.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern total stem density from the out-of-sample grid cells using the alternate model with the soil and climate covariates as well as the latitude and longitude of the grid cell. Used in 6.7.density_figures.R
- **6.7.density_figures.R**: Plot out-of-sample predictions from historical model predicting historical and modern total stem density
    - Inputs:
        - out/gam/H/density/predicted_historical_rf1.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical total stem density from main model with soil and climate covariates
        - out/gam/H/density/predicted_historical_rf2.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical total stem density from alternate model with only climate covariates
        - out/gam/H/density/predicted_historical_rf3.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical total stem density from alternate model with the reduced set of covariates
        - out/gam/H/density/predicted_historical_rf4.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical total stem density from alternate model with soil and climate covariates as well as latitude and longitude of the grid cell
        - out/gam/H/density/predicted_modern_rf1.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern total stem density from the main model with soil and climate covariates
        - out/gam/H/density/predicted_modern_rf2.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern total stem density from alternate model with only climate covariates
        - out/gam/H/density/predicted_modern_rf3.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern total stem density from alternate model with the reduced set of covariates
        - out/gam/H/density/predicted_modern_rf4.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern total stem density from alternate model with soil and climate covariates as well as latitude and longitude of the grid cell
    - Outputs: none
- **6.8.fit_abundance_allcovar.R**: Multivariate random forest fit to historical relative abundance and climate and soil covariates
    - Inputs:
        - data/processed/PLS/xydata_in.RData: Dataframe of in-sample grid cells with historical (PLS) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/abundance/allcovar.RData: Fitted GAM object saved to external hard drive. Used in 6.12.abundance_historical_predictions.R, 6.13.abundance_modern_predictions.R
- **6.9.fit_abundance_climcovar.R**: Multivariate GAM fit to historical relative abundance and climate covariates only
    - Inputs:
        - data/processed/PLS/xydata_in.RData: Dataframe of in-sample grid cells with historical (PLS) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/abundance/climcovar.RData: Fitted GAM object saved to external hard drive. Used in 6.12.abundance_historical_predictions.R, 6.13.abundance_modern_predictions.R
- **6.10.fit_abundance_redcovar.R**: Multivariate GAM fit to historical relative abundance and reduced covariates. Reduced covariates refers to using a subset of all the covariates based on importance and minimum depth analysis performed in step 6-8: XX, YY, ZZ
    - Inputs:
        - data/processed/PLS/xydata_in.RData: Dataframe of in-sample grid cells with historical (PLS) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/abundance/redcovar.RData: Fitted GAM object saved to external hard drive. Used in 6.12.abundance_historical_predictions.R, 6.13.abundance_modern_predictions.R
- **6.11.fit_abundance_xycovar.R**: Multivariate GAM fit to historical relative abundance and climate and soil covariates plus coordinates
    - Inputs:
        - data/processed/PLS/xydata_in.RData: Dataframe of in-sample grid cells with historical (PLS) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/abundance/xycovar.RData: Fitted GAM object saved to external hard drive. Used in 6.12.abundance_historical_predictions.R, 6.13.abundance_modern_predictions.R
- **6.12.abundance_historical_predictions.R**: Predict historical relative abundance from historical fitted GAMs
    - Inputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/abundance/allcovar.RData: Fitted GAM using all climate and soil covariates. Used to make predictions from the main model with climate and soil covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/abundance/climcovar.RData: Fitted GAM using only climate covariates. Used to make predictions from the alternate model with only climate covariates
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/abundance/redcovar.RData: Fitted GAM using only the reduced set of covariates. Used to make predictions from the alternate model with a reduced set of covariates
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/abundance/xycovar.RData: Fitted GAM using all the soil and climate covariates plus the latitude and longitude of the grid cell. Used to make predictions from the alternate model including grid cell coordinates and environmental covariates
        - data/processed/PLS/xydata_out.RData: Dataframe containing the out-of-sample historical (PLS) vegetation, soil, and climate data. The covariates are used to make predictions of the out-of-sample historical vegetation
    - Outputs:
        - out/gam/H/abundance/predicted_historical_gam1.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical relative abundances from the out-of-sample grid cells using the main model with climate and soil covariates. Used in 6.14.abundance_figures.R
        - out/gam/H/abundance/predicted_historical_gam2.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical relative abundances from the out-of-sample grid cells using the alternate model with only climate covariates. Used in 6.14.abundance_figures.R
        - out/gam/H/abundance/predicted_historical_gam3.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical abundances from the out-of-sample grid cells using the alternate model with only the reduced set of covariates. Used in 6.14.abundance_figures.R
        - out/gam/H/abundance/predicted_historical_gam4.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical relative abundances from the out-of-sample grid cells using the alternate model with the soil and climate covariates as well as the latitude and longitude of the grid cell. Used in 6.14.abundance_figures.R
- **6.13.abundance_modern_predictions.R**: Predict modern relative abundance from historical fitted GAMs
    - Inputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/abundance/allcovar.RData: Fitted GAM using all climate and soil covariates. Used to make predictions from the main model with climate and soil covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/abundance/climcovar.RData: Fitted GAM using only climate covariates. Used to make predictions from the alternate model with only climate covariates
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/abundance/redcovar.RData: Fitted GAM using only the reduced set of covariates. Used to make predictions from the alternate model with a reduced set of covariates
        - /Volumes/FileBackup/SDM_bigdata/out/gam/H/abundance/xycovar.RData: Fitted GAM using all the soil and climate covariates plus the latitude and longitude of the grid cell. Used to make predictions from the alternate model including grid cell coordinates and environmental covariates
        - data/processed/FIA/xydata_out.RData: Dataframe containing the out-of-sample modern (FIA) vegetation, soil, and climate data. The covariates are used to make predictions of the out-of-sample modern vegetation data
    - Outputs:
        - out/gam/H/abundance/predicted_modern_gam1.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern relative abundances from the out-of-sample grid cells using the main model with climate and soil covariates. Used in 6.14.abundance_figures.R
        - out/gam/H/abundance/predicted_modern_gam2.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern relative abundances from the out-of-sample grid cells using the alternate model with only climate covariates. Used in 6.14.abundance_figures.R
        - out/gam/H/abundance/predicted_modern_gam3.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern abundances from the out-of-sample grid cells using the alternate model with only the reduced set of covariates. Used in 6.14.abundance_figures.R
        - out/gam/H/abundance/predicted_modern_gam4.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern relative abundances from the out-of-sample grid cells using the alternate model with the soil and climate covariates as well as the latitude and longitude of the grid cell. Used in 6.14.abundance_figures.R
- **6.14.abundance_figures.R**: Plot out-of-sample predictions from historical model predicting historical and modern relative abundance
    - Inputs:
        - out/gam/H/abundance/predicted_historical_rf1.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical relative abundances from main model with soil and climate covariates
        - out/gam/H/abundance/predicted_historical_rf2.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical relative abundances from alternate model with only climate covariates
        - out/gam/H/abundance/predicted_historical_rf3.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical relative abundances from alternate model with the reduced set of covariates
        - out/gam/H/abundance/predicted_historical_rf4.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical relative abundances from alternate model with soil and climate covariates as well as latitude and longitude of the grid cell
        - out/gam/H/abundance/predicted_modern_rf1.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern relative abundances from main model with soil and climate covariates
        - out/gam/H/abundance/predicted_modern_rf2.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern relative abundances from alternate model with only climate covariates
        - out/gam/H/abundance/predicted_modern_rf3.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern relative abundances from alternate model with the reduced set of covariates
        - out/gam/H/abundance/predicted_modern_rf4.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern relative abundances from alternate model with soil and climate covariates as well as latitude and longitude of the grid cell
    - Outputs: none

### 7.GAM_M2H -- COME BACK

- **7.1.fit_density_allcovar.R**: Univariate GAM fit to modern total stem density and climate and soil covariates.
    - Inputs:
        - data/processed/FIA/xydata_in.RData: Dataframe of in-sample grid cells with modern (FIA) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/allcovar.RData: Fitted GAM object saved to external hard drive. Used in 7.5.density_historical_predictions.R, 7.6.density_modern_predictions.R
- **7.2.fit_density_climcovar.R**: Univariate GAM fit to modern total stem density and climate covariates only.
    - Inputs: 
        - data/processed/FIA/xydata_in.RData: Dataframe of in-sample grid cells with modern (FIA) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/climcovar.RData: Fitted GAM object saved to external hard drive. Used in 7.5.density_historical_predictions.R, 7.6.density_modern_predictions.R
- **7.3.fit_density_redcovar.R**: Univariate GAM fit to modern total stem density and reduced covariates. Reduced covariates refers to using a subset of all the covariates based on importance and minimum depth analysis performed in step 7-1: XX, YY, ZZ
    - Inputs:
        - data/processed/FIA/xydata_in.RData: Dataframe of in-sample grid cells with modern (FIA) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/redcovar.RData: Fitted GAM object saved to external hard drive. Used in 7.5.density_historical_predictions.R, 7.6.density_modern_predictions.R
- **7.4.fit_density_xycovar.R**: Univariate GAM fit to modern total stem density and climate and soil covariates plus coordinates.
    - Inputs:
        - data/processed/FIA/xydata_in.RData: Dataframe of in-sample grid cells with modern (FIA) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/xycovar.RData: Fitted GAM object saved to external hard drive. Used in 7.5.density_historical_predictions.R, 7.6.density_modern_predictions.R
- **7.5.density_historical_predictions.R**: Predict historical total stem density from modern fitted GAMs
    - Inputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/allcovar.RData: Fitted GAM using all climate and soil covariates. Used to make predictions from the main model with climate and soil covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/climcovar.RData: Fitted GAM using only climate covariates. Used to make predictions from the alternate model with only climate covariates
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/redcovar.RData: Fitted GAM using only the reduced set of covariates. Used to make predictions from the alternate model with a reduced set of covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/xycovar.RData: Fitted GAM using all the soil and climate covariates plus the latitude and longitude of the grid cell. Used to make predictions from the alternate model including grid cell coordinates and environmental covariates.
        - data/processed/PLS/xydata_out.RData: Dataframe containing the out-of-sample historical (PLS) vegetation, soil, and climate data. The covariates are used to make predictiosn of the out-of-sample historical vegetation data.
    - Outputs:
        - out/gam/M/density/predicted_historical_gam1.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical total stem density from the out-of-sample grid cells using the main model with climate and soil covariates. Used in 7.7.density_figures.R.
        - out/gam/M/density/predicted_historical_gam2.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical total stem density from the out-of-sample grid cells using the alternate model with only climate covariates. Used in 7.7.density_figures.R
        - out/gam/M/density/predicted_historical_gam3.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical total stem density from the out-of-sample grid cells using the alternate model with only the reduced set of covariates. Used in 7.7.density_figures.R
        - out/gam/M/density/predicted_historical_gam4.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical total stem density from the out-of-sample grid cells using the alternate model with the soil and climate covariates as well as the latitude and longitude of the grid cell. Used in 7.7.density_figures.R
- **7.6.density_modern_predictions.R**: Predicted modern total stem density from modern fitted GAMs
    - Inputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/allcovar.RData: Fitted GAM using all climate and soil covariates. Used to make predictions from the main model with climate and soil covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/climcovar.RData: Fitted GAM using only climate covariates. Used to make predictions from the alternate model with only climate covariates
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/redcovar.RData: Fitted GAM using only the reduced set of covariates. Used to make predictions from the alternate model with a reduced set of covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/xycovar.RData: Fitted GAM using all the soil and climate covariates plus the latitude and longitude of the grid cell. Used to make predictions from the alternate model including grid cell coordinates and environmental covariates.
        - data/processed/FIA/xydata_out.rData: Dataframe containing the out-of-sample modern (FIA) vegetation, soil, and climate data. The covariates are used to make predictions of the out-of-sample modern vegetation data
    - Outputs:
        - out/gam/M/density/predicted_modern_gam1.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern total stem density from the out-of-sample grid cells using the main model with climate and soil covariates. Used in 7.7.density_figures.R.
        - out/gam/M/density/predicted_modern_gam2.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern total stem density from the out-of-sample grid cells using the alternate model with only climate covariates. Used in 7.7.density_figures.R
        - out/gam/M/density/predicted_modern_gam3.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern total stem density from the out-of-sample grid cells using the alternate model with only the reduced set of covariates. Used in 7.7.density_figures.R
        - out/gam/M/density/predicted_modern_gam4.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern total stem density from the out-of-sample grid cells using the alternate model with the soil and climate covariates as well as the latitude and longitude of the grid cell. Used in 7.7.density_figures.R
- **7.7.density_figures.R**: Plot out-of-sample predictions from modern model predicting historical and modern total stem density
    - Inputs:
        - out/gam/M/density/predicted_historical_gam1.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical total stem density from main model with soil and climate covariates
        - out/gam/M/density/predicted_historical_gam2.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical total stem density from alternate model with only climate covariates
        - out/gam/M/density/predicted_historical_gam3.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical total stem density from alternate model with the reduced set of covariates
        - out/gam/M/density/predicted_historical_gam4.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical total stem density from alternate model with soil and climate covariates as well as latitude and longitude of the grid cell
        - out/gam/M/density/predicted_modern_gam1.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern total stem density from the main model with soil and climate covariates
        - out/gam/M/density/predicted_modern_gam2.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern total stem density from alternate model with only climate covariates
        - out/gam/M/density/predicted_modern_gam3.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern total stem density from alternate model with the reduced set of covariates
        - out/gam/M/density/predicted_modern_gam4.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern total stem density from alternate model with soil and climate covariates as well as latitude and longitude of the grid cell
    - Outputs: none
- **7.8.fit_abundance_allcovar.R**: Multivariate GAM fit to modern relative abundance and climate and soil covariates
    - Inputs:
        - data/processed/FIA/xydata_in.RData: Dataframe of in-sample grid cells with modern (FIA) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/abundance/allcovar.RData: Fitted random forest object saved to external hard drive. Used in 7.12.abundance_historical_predictions.R, 7.13.abundance_modern_predictions.R
- **7.9.fit_abundance_climcovar.R**: Multivariate GAM fit to modern relative abundance and climate covariates only
    - Inputs:
        - data/processed/FIA/xydata_in.RData: Dataframe of in-sample grid cells with modern (FIA) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/abundance/climcovar.RData: Fitted GAM object saved to external hard drive. Used in 7.12.abundance_historical_predictions.R, 7.13.abundance_modern_predictions.R
- **7.10.fit_abundance_redcovar.R**: Multivariate GAM fit to modern relative abundance and reduced covariates. Reduced covariates refers to using a subset of all the covariates based on importance and minimum depth analysis performed in step 7-8: XX, YY, ZZ
    - Inputs:
        - data/processed/FIA/xydata_in.RData: Dataframe of in-sample grid cells with modern (FIA) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/abundance/redcovar.RData: Fitted GAM object saved to external hard drive. Used in 7.12.abundance_historical_predictions.R, 7.13.abundance_modern_predictions.R
- **7.11.fit_abundance_xycovar.R**: Multivariate GAM fit to modern relative abundance and climate and soil covariates plus coordinates
    - Inputs:
        - data/processed/FIA/xydata_in.RData: Dataframe of in-sample grid cells with modern (FIA) era vegetation, soil, and climate data
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/abundance/xycovar.RData: Fitted GAM object saved to external hard drive. Used in 7.12.abundance_historical_predictions.R, 7.13.abundance_modern_predictions.R
- **7.12.abundance_historical_predictions.R**: Predict historical relative abundance from modern fitted GAMs
    - Inputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/abundance/allcovar.RData: Fitted GAM using all climate and soil covariates. Used to make predictions from the main model with climate and soil covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/abundance/climcovar.RData: Fitted GAM using only climate covariates. Used to make predictions from the alternate model with only climate covariates
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/abundance/redcovar.RData: Fitted GAM using only the reduced set of covariates. Used to make predictions from the alternate model with a reduced set of covariates
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/abundance/xycovar.RData: Fitted GAM using all the soil and climate covariates plus the latitude and longitude of the grid cell. Used to make predictions from the alternate model including grid cell coordinates and environmental covariates
        - data/processed/PLS/xydata_out.RData: Dataframe containing the out-of-sample historical (PLS) vegetation, soil, and climate data. The covariates are used to make predictions of the out-of-sample historical vegetation
    - Outputs:
        - out/gam/M/abundance/predicted_historical_gam1.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical relative abundances from the out-of-sample grid cells using the main model with climate and soil covariates. Used in 7.14.abundance_figures.R
        - out/gam/M/abundance/predicted_historical_gam2.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical relative abundances from the out-of-sample grid cells using the alternate model with only climate covariates. Used in 7.14.abundance_figures.R
        - out/gam/M/abundance/predicted_historical_gam3.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical abundances from the out-of-sample grid cells using the alternate model with only the reduced set of covariates. Used in 7.14.abundance_figures.R
        - out/gam/M/abundance/predicted_historical_gam4.RData: Dataframe of observed vegetation, soil, and climate data and predicted historical relative abundances from the out-of-sample grid cells using the alternate model with the soil and climate covariates as well as the latitude and longitude of the grid cell. Used in 7.14.abundance_figures.R
- **7.13.abundance_modern_predictions.R**: Predict modern relative abundance from modern fitted GAMs
    - Inputs:
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/abundance/allcovar.RData: Fitted GAM using all climate and soil covariates. Used to make predictions from the main model with climate and soil covariates.
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/abundance/climcovar.RData: Fitted GAM using only climate covariates. Used to make predictions from the alternate model with only climate covariates
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/abundance/redcovar.RData: Fitted GAM using only the reduced set of covariates. Used to make predictions from the alternate model with a reduced set of covariates
        - /Volumes/FileBackup/SDM_bigdata/out/gam/M/abundance/xycovar.RData: Fitted GAM using all the soil and climate covariates plus the latitude and longitude of the grid cell. Used to make predictions from the alternate model including grid cell coordinates and environmental covariates
        - data/processed/FIA/xydata_out.RData: Dataframe containing the out-of-sample modern (FIA) vegetation, soil, and climate data. The covariates are used to make predictions of the out-of-sample modern vegetation data
    - Outputs:
        - out/gam/M/abundance/predicted_modern_gam1.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern relative abundances from the out-of-sample grid cells using the main model with climate and soil covariates. Used in 7.14.abundance_figures.R
        - out/gam/M/abundance/predicted_modern_gam2.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern relative abundances from the out-of-sample grid cells using the alternate model with only climate covariates. Used in 7.14.abundance_figures.R
        - out/gam/M/abundance/predicted_modern_gam3.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern abundances from the out-of-sample grid cells using the alternate model with only the reduced set of covariates. Used in 7.14.abundance_figures.R
        - out/gam/M/abundance/predicted_modern_gam4.RData: Dataframe of observed vegetation, soil, and climate data and predicted modern relative abundances from the out-of-sample grid cells using the alternate model with the soil and climate covariates as well as the latitude and longitude of the grid cell. Used in 7.14.abundance_figures.R
- **7.14.abundance_figures.R**: Plot out-of-sample predictions from modern model predicting historical and modern relative abundance
    - Inputs:
        - out/gam/M/abundance/predicted_historical_gam1.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical relative abundances from main model with soil and climate covariates
        - out/gam/M/abundance/predicted_historical_gam2.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical relative abundances from alternate model with only climate covariates
        - out/gam/M/abundance/predicted_historical_gam3.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical relative abundances from alternate model with the reduced set of covariates
        - out/gam/M/abundance/predicted_historical_gam4.RData: Dataframe with observed historical out-of-sample vegetation, soil, and climate data as well as predicted historical relative abundances from alternate model with soil and climate covariates as well as latitude and longitude of the grid cell
        - out/gam/M/abundance/predicted_modern_gam1.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern relative abundances from main model with soil and climate covariates
        - out/gam/M/abundance/predicted_modern_gam2.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern relative abundances from alternate model with only climate covariates
        - out/gam/M/abundance/predicted_modern_gam3.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern relative abundances from alternate model with the reduced set of covariates
        - out/gam/M/abundance/predicted_modern_gam4.RData: Dataframe with observed modern out-of-sample vegetation, soil, and climate data as well as predicted modern relative abundances from alternate model with soil and climate covariates as well as latitude and longitude of the grid cell
    - Outputs: none

### Other code

- install_packages.R: helper file for installing packages, with package versions noted