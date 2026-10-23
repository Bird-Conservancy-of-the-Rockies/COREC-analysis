---
output:
  html_document: default
---
<!---
This README uses Markdown syntax and was originally written and saved as a .md file.
To view the file with its intended formatting, use the R package 'rmarkdown'
to render the file as html.
--->

Reference Information
=====================

Provenance for this README
--------------------------

* File name: Dryad_repo_metadata.md
* Author: Quresh S. Latif
* Co-authors for manuscript: Liza G. Rossi, Matthew F. McLaren, Jennifer M. Timmer, Melissa A. Dressen, Christopher C. Keefe, Heather N. Abernathy, Robert A. Sparks, Mark A. Ditmer
* Date created: 2025-07-09
* Date modified: 2026-10-23

Dataset Version and Release History
-----------------------------------

* Current Version:
  * Number: 1.0
  * Date: 2026-10-16
  * Persistent identifier: DOI: 10.5061/dryad.0rxwdbscz

Dataset Attribution and Usage
-----------------------------

* Dataset Title: Data for the article "Increased trail density negatively impacts breeding bird diversity on Colorado public lands due partly to human activity"

* Persistent Identifier: DOI: 10.5061/dryad.0rxwdbscz

* Dataset Contributors: Matthew F. McLaren, Jennifer M. Timmer, Heather N. Abernathy, Robert A. Sparks, Mark A. Ditmer

* Publisher: Bird Conservancy of the Rockies

* License: Use of these data is covered by the following license:

  - Title: CC0 1.0 Universal (CC0 1.0)

  - Specification: https://creativecommons.org/publicdomain/zero/1.0/; the authors respectfully request to be contacted by researchers interested in the re-use of these data so that the possibility of collaboration can be discussed.

* Suggested Citations:

  - Dataset citation: Latif, Q. S., L. G. Rossi, M. F. McLaren, J. M. Timmer, M. A. Dressen, C. C. Keefe, H. N. Abernathy, R. A. Sparks, and M. A. Ditmer. 2026. Data for the article "Increased trail density negatively impacts breeding bird diversity on Colorado public lands due partly to human activity", Dryad, Dataset. https://doi.org/10.5061/dryad.0rxwdbscz.

  - Corresponding publication: Latif, Q. S., L. G. Rossi, M. F. McLaren, J. M. Timmer, M. A. Dressen, C. C. Keefe, H. N. Abernathy, R. A. Sparks, and M. A. Ditmer. In press. Increased trail density negatively impacts breeding bird diversity on Colorado public lands due partly to human activity. Ecological Applications.

Contact Information
-------------------

  * Name: Quresh S. Latif
  * Affiliations: Bird Conservancy of the Rockies
  * Email: quresh.latif@birdconservancy.org
  * Alternate Email: qureshlatif@yahoo.com

- - -

Additional Dataset Metadata
===========================

Acknowledgements
----------------

* Funding sources: Colorado Parks and Wildlife (CPW), the U.S. Bureau of Land Management (BLM), the USDA Forest Service, and various other partners of the Integrated Monitoring in Bird Conservation Regions (IMBCR) funded the background monitoring effort that provided large portion of the data for this study. CPW and BLM provided additional project-specific funding for supplementary data collection and analysis. The USDA Forest Service Rocky Mountain Research Station funded acquisition and curation of the GPS smartphone data that formed the basis for human traffic metrics.


Dates and Locations
-------------------

* Dates of data collection: Field data collected between during bird breeding seasons of 2001-2003 following IMBCR protocols and sampling design.

* Geographic locations of data collection: Fieldwork conducted on public lands of western Colorado, U.S.A., managed by the U.S. Forest Service and BLM.

- - -

Methodological Information
==========================

* Methods of data collection/generation: see manuscript for details

- - -

File metadata
=============

`Species_detection_parameter_estimates.csv`
-------------------------------------------

* Description: A comma-delimited file listing detectability sub-model parameter estimates from the avian community abundance model. Species abundance estimates reported in the manuscript are corrected for detectability represented in these estimates. Species are ordered alphabetically by 4-letter code. All estimates are reported as posterior median (and 80% credible intervals), and asterices indicate covariate relationships supported with 90% confidence (i.e., where 80% CIs excluded zero).

* Format: .csv

* Dimensions: 150 rows X 8 columns

* Fields:
  * Species: 4-letter species codes. Complete species names are listed in `Species_list_and_groups.csv`.
  * a0: scale parameter for hazard function corresponding to the distance at which the species are detected at mean covariate values
  * zeta.Traffic_intensity: log-linear relationship of perceptibility (scale parameter for hazard function) with scaled (centered and divided by SD) unconditional traffic intensity
  * pa0: availability (i.e., the probability an individual of the species is available for detection if present within a 6-min survey period) at mean covariate values
  * theta.Traffic_intensity: logit linear relationship of availability with scaled unconditional traffic intensity
  * theta.Survey_date: logit linear relationship of availability with scaled survey day of year
  * theta.Survey_time_since_sunrise = logit linear relationship of availability with scaled time (minutes) since sunrise
  * theta.Survey_time_since_sunrise2 = logit linear relationship of availability with scaled time (minutes) since sunrise squared

`Species_list_and_groups.csv`
-----------------------------

* Description: A comma-delimited file listing species represented in the data analyzed for the manuscript and species trait group assignments.

* Format: .csv

* Dimensions: 150 rows X 12 columns

* Fields:
  * Common_name: Official species common name assigned by the American Ornithological Society
  * Taxonomic_name: Species taxonomic name
  * Code: 4-letter species code assigned by the American Ornithological Society and referenced in figures and tables in the manuscript
  * Total_count: the sum of counts across grid cell survey observations
  * HS: Binary indicator of whether the species was classified as a habitat specialist
  * M: Binary indicator of whether the species was classified as migratory
  * HC: Binary indicator of whether the species was classified as a human commensal
  * S: Binary indicator of whether the species was classified as small-bodied
  * DS: Binary indicator of whether the species was classified as a dietary specialist
  * I: Binary indicator of whether the species was classified as an insectivore
  * G: Binary indicator of whether the species was classified as one that nests or forages on or near the ground
  * SGCN: Binary indicator of inclusion on the 2025 Colorado Species of Greatest Conservation Need list

`Species_management_relations_and_human_traffic_contributions.csv`
------------------------------------------------------------------

* Description: A comma-delimited file listing total management relationship estimates and the percentage of these explained by human traffic relationships (i.e., mechanistic contributions of human traffic). Species are ordered alphabetically by 4-letter code. All estimates are reported as posterior median (and 80% credible intervals), and asterices indicate mechanistic contributions supported with 90% confidence (i.e., where 80% CIs excluded zero). Note that within our inferential framework, positive percent contributions are consistent with our *a priori* hypotheses and necessary (although not sufficient) to support strong inference of mechanism (see manuscript Methods for further detail).

* Format: .csv

* Dimensions: 150 rows X 7 columns

* Fields:
  * Species: 4-letter species codes. Complete species names are listed in `Species_list_and_groups.csv`.
  * Trail.total: Total relationship (direct + indirect) of species abundance with trail density
  * Trail.pctExplained: estimated percent of total trail density relationship explained by human traffic
  * OHV.total: Total relationship (direct + indirect) of species abundance with proportion of trails where OHVs were prohibited (i.e., proportion no OHVs)
  * OHV.pctExplained: estimated percent of total relationship with proportion no OHVs explained by human traffic
  * Road.total: Total relationship (direct + indirect) of species abundance with road density
  * Road.pctExplained: estimated percent of total road density relationship explained by human traffic

`Species_model_performance.csv`
------------------------------------------------------------------

* Description: A comma-delimited file listing species-specific model performance metrics. Species are ordered alphabetically by 4-letter code.

* Format: .csv

* Dimensions: 150 rows X 10 columns

* Fields:
  * Species: 4-letter species codes. Complete species names are listed in `Species_list_and_groups.csv`.
  * n: Number of detections of the species recorded across all surveys in the data
  * r.md, r.10, r.90: Posterior median, 10th, and 90th %-iles for Pearson's correlation coefficient relating observed counts with posterior predicted counts
  * r.pct.na: Percentage of posterior samples for with the correlation coefficient relating observed with predicted counts could not be calculated due to predicted counts being all zero
  * ChiSqr_p: Bayesian p-value indicating the proportion of posterior predicted datasets for which a Pearson's Chi-square goodness-of-fit test statistic was greater than the equivalent statistic for observed data
  * Bays_Rsqr.md, Bays_Rsqr.10, Bays_Rsqr.90: Posterior median, 10th, and 90th %-iles for R-squared values quantifying overall standardized effect sizes for each species (Gelman et al. 2019)

`mod_path`
----------

* Description: This file is an R object containing all model output as generated by the “01-Analysis.R” script file archived in the accompanying code archive (Latif 2026; see Code/Software below). This file can be loaded using the R function 'R.utils::loadObject()'.  Broadly, this file contains posterior samples of parameter model estimates and their summaries as generated by the data analysis described in the manuscript and encoded in archived scripts (Latif 2026). This file is needed to replicate results summaries generated by various scripts located in the accompanying code archive (Latif 2026). As such, we do not provide a detailed catalog of the contents of this file.

`mod_path_GOF`
----------

* Description: This file is an R object equivalent to `mod_path` but with additional parameters saved to calculate performance metrics and referenced by the “02-Tabulate_bird_GOF.R” script file archived in the accompanying code archive (Latif 2026). This file can be loaded using the R function 'R.utils::loadObject()'.

`Data_compiled.RData`
---------------------

* Description: This is an R workspace containing all data and R objects necessary to run R scripts implementing data analyses and results summaries associated with this manuscript.

* Format: .RDATA

* Access: R objects contained in this file can be loaded into an R workspace using the Base R function 'load'.

* Terminology and abbreviations used in this workspace:
  * Grid cell (`TransectNum`): The primary sampling unit, a 1-km^2 grid cell surveyed through the Integrated Monitoring in Bird Conservation Regions (IMBCR) program. Each grid cell contains a 4 x 4 array of survey points spaced 250 m apart. There are 285 grid cells, each identified by a code of the form "CO-BCR10-BL1".
  * Grid-year (also "grid x year" or "GrdYr"): A grid cell surveyed in a given year. There are 500 grid-years (2021-2023), each identified by "<TransectNum>-<Year>" (e.g., "CO-BCR10-BL1-2021"). Rows of `covariates`, `CovIndMat`, `samples`, and columns of `n.mat` correspond to grid-years.
  * Point survey: A 6-min bird count conducted at a survey point. `Effort` is the number of point surveys completed within a grid cell in a given year (maximum 16).
  * Species code (`BirdCode`, `Spp`): 4-letter species code assigned by the American Ornithological Society. Complete names are in `Species_list_and_groups.csv`.
  * Radial distance (`radialDistance`): Distance (m) from the survey point to a detected bird, measured with a laser range finder.
  * Time period (`TimePeriod`): The minute interval (1-6) of the 6-min point survey in which a bird was first detected (time-removal bin).
  * Distance bin (`dclass`): Integer 1-10 indexing the distance bin containing a detection. Each species has its own distance cutoff (`cutoff`), and the 10 bins are of equal width (cutoff / 10).
  * Cluster (`CL_ID`, `CL_Count`): Birds detected together are recorded as a cluster; `CL_ID` identifies the cluster (missing for single birds) and `CL_Count` is the number of birds in the detection record.
  * Index columns (`GrdYrInd`, `GridInd`, `YearInd`, `SppInd`, `GridYrInd`): Integer positions of the grid-year, grid cell, year, and species within `gridXyears.list`, `grid.list`, `years`, and `Spp`, respectively (e.g., `GrdYrInd` and `GridYrInd` both index `gridXyears.list`).
  * Suffixes and abbreviations: `tssr` = time since sunrise; `DOY` = day of year (1 January = 1); `_mn` or `_mean` = mean; `Prp` = proportion; `Totm` = total in meters; `OHV` = off-highway vehicle; `NA` = missing or undefined.
* Units: Distances (`radialDistance`, `breaks`, `cutoff`) are in meters (m); `area.band` is in m^2 and `area.circle` is in km^2; `TrailTotm` and `RoadTotm` are total lengths in m within a 1-km^2 grid cell (i.e., equivalent to m per km^2); `Survey_tssr` is in minutes; `Speed` is in miles per hour (mi/h; the manuscript reports traffic speed in km per hour, and 1 mi/h = 1.609 km/h); `TOD_mean` is in hours of the day (0-24); `Traffic_DOY_mn` and `Survey_DOY` are days of the year; proportions are unitless (0-1). `HumanTraffic` is in units of users x residency time per day [CONFIRM: units of residency time, e.g., minutes]; log-transformed versions are natural logarithms (log base e) of this quantity.
* R objects contained in this workspace (dimensions are listed in parentheses):
  * area.band (matrix, 150 species x 10 distance bins): The area (m^2) of each distance bin for each species
  * breaks (matrix, 150 species x 11): Break points (m) for distance bins for each species (bin edges from 0 to the species' distance cutoff)
  * covariates (data frame, 500 grid-years x 28): Raw (unscaled, unimputed) covariate values for all grid-years (includes several covariates not ultimately included in the analysis model). Columns are:
    * Grid_year: Grid-year ID ("<TransectNum>-<Year>")
    * TransectNum: Grid cell ID
    * Year: Survey year (2021-2023)
    * GrdYrInd, GridInd, YearInd: Index columns (see above)
    * Date: Date of the grid cell survey (format YYYY-MM-DD)
    * Survey_DOY: Day of year of the survey
    * Effort: Number of point surveys completed in the grid cell that year
    * Survey_tssr: Time since sunrise (minutes), averaged across the point surveys completed in the grid cell that year
    * HumanPresence: Binary indicator (1 or 0) of whether any human traffic was recorded within 100 m of the grid cell (i.e., `HumanTraffic` > 0)
    * LogTrafficNoZeros: Natural log of `HumanTraffic`; missing where `HumanTraffic` = 0
    * LogTraffic: Natural log of (`HumanTraffic` + 0.1)
    * Traffic_DOY_mn: Mean day of year of the smartphone movement records used to calculate `Speed`; missing where no movement records were available
    * Speed: Mean speed (mi/h) of smartphone user movement; missing where no movement records were available. Note that the manuscript reports traffic speed in km per hour (1 mi/h = 1.609 km/h).
    * TOD_mean: Mean time of day (hours) of smartphone location records [CONFIRM: time zone and 0-24 scale]; missing for some grid cells
    * TrailTotm: Total length (m) of trails within the grid cell
    * RoadTotm: Total length (m) of roads within the grid cell
    * Prp_MotRestricted: Proportion of trail length where motorized use (ATVs and OHVs) is prohibited; missing where `TrailTotm` = 0
    * Prp_HorseRestricted: Proportion of trail length where horseback riding is prohibited; missing where `TrailTotm` = 0
    * Shrubland, PinyonJuniper, ConiferForest, Aspen, OakWoodland, GrasslandMeadow, Mesic, Alpine: Proportion of point surveys in the grid cell (pooled across years) classified in the primary habitat class named (Shrubland = sage, desert, and semi-desert shrublands; PinyonJuniper = pinyon-juniper woodland; ConiferForest = spruce-fir, mixed conifer, lodgepole pine, and ponderosa pine forest; Aspen = aspen woodland; OakWoodland = oak woodland; GrasslandMeadow = grassland, montane meadow, and herbaceous meadow; Mesic = riparian, open water, and wetland; Alpine = alpine tundra)
    * Note: Human traffic, trail, road, and vegetation columns have one value per grid cell (identical across years), whereas `Date`, `Survey_DOY`, `Effort`, and `Survey_tssr` vary by year.
  * CovIndMat (numeric matrix, 500 grid-years x 24): Raw covariate values included in analysis. Rows are in the same order as `gridXyears.list`. Columns are the following subset of `covariates` columns, with identical definitions: GrdYrInd, GridInd, YearInd, Effort, Survey_DOY, Survey_tssr, HumanPresence, LogTrafficNoZeros, LogTraffic, Traffic_DOY_mn, Speed, TOD_mean, TrailTotm, RoadTotm, Prp_MotRestricted, Prp_HorseRestricted, Shrubland, PinyonJuniper, ConiferForest, Aspen, OakWoodland, GrasslandMeadow, Mesic, and Alpine
  * dat_human (data frame, 285 grid cells x 5): Grid cell values for human traffic metrics. Columns are `TransectNum` (grid cell ID), `Date_mn` (mean day of year of smartphone movement records; named `Traffic_DOY_mn` in `covariates`), `Speed` (mean speed in mi/h), `TOD_mean` (mean time of day in hours), and `HumanTraffic` (human traffic intensity: mean number of unique smartphone users per day multiplied by mean daily residency time within 100 m of the grid cell [CONFIRM units]; 0 where no smartphone locations were recorded). `Date_mn`, `Speed`, and `TOD_mean` are missing for grid cells lacking the relevant records.
  * dat_trails (data frame, 285 grid cells x 5): Grid cell values for management metrics. Columns are `TransectNum` (grid cell ID), `TrailTotm` and `RoadTotm` (total length of trails and roads in m within the grid cell), and `Prp_MotRestricted` and `Prp_HorseRestricted` (proportion of trail length where motorized use or horseback riding, respectively, is prohibited). For grid cells with no trails (`TrailTotm` = 0), the two proportion columns hold the mean across grid cells with trails (placeholders) and are set to missing in `covariates`.
  * detection.trim.summary (data frame, 150 species x 7): Summary of detection data trimmed for each species when filtering out the furthest 5% of detections (generated by `00-Data_wrangle.R` script in Latif 2026). Columns are `Spp` (species code), `total` (number of detection records), `n.keep` (number retained), `n.drop` (number dropped; missing if none), `prp.keep` and `prp.drop` (proportion of detection records retained and dropped), and `Dist_time_cor` (Spearman rank correlation between distance bin and time period among retained detections; missing where it could not be calculated)
  * detections (data frame, 54105 x 8): Bird detection dataset queried from the Bird Conservancy database. Columns are `TransectNum` (grid cell ID), `Point` (survey point number within the grid cell), `Year`, `BirdCode`, `radialDistance` (m), `TimePeriod` (minute interval of first detection), `CL_ID` (cluster ID; missing for single birds), and `CL_Count` (number of birds in the detection record)
  * detects.cutoff (data frame, 51163 x 6): Bird detection data retained after applying the distance cutoff (furthest 5% dropped for species with at least 75 detections). Columns are `Grid_year` (grid-year ID), `BirdCode`, `radialDistance` (m), `dclass` (distance bin), `TimePeriod`, and `CL_Count`
  * detects.drop (data frame, 2612 x 6): Bird detection data dropped prior to analysis (i.e., detections beyond the species-specific distance cutoff, which is the furthest 5% for species with at least 75 detections). Columns are the same as `detects.cutoff`
  * dtobs (numeric matrix, 51163 x 5): Detection data retained for analysis in index form. Columns are `SppInd` (species index), `GridYrInd` (grid-year index), `CL_Count` (cluster size), `dclass` (distance bin), and `TimePeriod` (time removal bin)
  * n.mat (matrix, 150 species x 500 grid-years): Count data matrix, i.e., the number of detection records retained for analysis (after distance cutoffs) for each species in each grid-year. Each detection record is a single bird or a cluster of birds.
  * samples (data frame, 500 grid-years x 6): Survey specific info. Columns are `TransectNum` (grid cell ID), `Year`, `Date` (survey date), `TZ` (time zone; "Mountain" for all surveys), `Effort` (number of point surveys completed), and `Survey_tssr` (time since sunrise in minutes when point surveys were conducted, averaged across point surveys in the grid cell)
  * Spp_list (data frame, 150 species x 3): Species list. Columns are `BirdCode` (species code), `Species` (common name), and `Detections` (sum of all counts, i.e., the number of birds detected summed across detection records, before applying distance cutoffs)
  * area.circle (numeric vector, length 150): Point-level area of estimation for each species (km^2; i.e., area of a circle with radius equal to the species' distance cutoff)
  * area.prop (numeric vector, length 10): Proportion of the area of estimation within each distance bin (same for all species)
  * cutoff (named numeric vector, length 150): Distance cutoff for each species (m; i.e., radius of point-level area of estimation)
  * grid.list (character vector, length 285): Grid cell IDs
  * gridXyears.list (character vector, length 500): Grid cell x year IDs
  * min.n.trunc (numeric, length 1): Minimum number of detections required to apply a 5% distance threshold cutoff to a species (75)
  * nG (numeric, length 1): Number of distance bins (10)
  * Spp (character vector, length 150): List of species codes labeling rows or elements in data objects with a species dimension (e.g., first dimension of n.mat)
  * trunc.pct (numeric, length 1): Proportion of detections to retain for species with at least 75 detections (0.95; determines distance cutoff)
  * years (integer vector, length 3): Years data were collected (2021, 2022, 2023)

* Metadata note: This file is provided primarily to allow replicability of study results using scripts provided in the accompanying code archive (Latif 2026). Every object and every column or field of each data frame and matrix is described above.

`code_reference_files.zip`
----------------------

* Description: This zipped directory contains three csv files referenced by name by R scripts archived by Latif (2026). Two of the files list species-specific estimates of management relationships (posterior median, 10th and 90th percentiles, and support category) that were generated by the analysis and are read by downstream scripts for tabulating and plotting results; the third lists species and their trait group assignments.

* Format: sub-directory containing .csv files

* Fields:
  * `Species_list_assigned.csv` (150 rows x 12 columns): Species list with trait group assignments, one row per species.
    * BirdCode: 4-letter species code assigned by the American Ornithological Society
    * Species: Common name
    * ScientificName: Scientific name
    * Sum_counts: Sum of all counts for the species (number of birds detected summed across detection records, before applying distance cutoffs; equivalent to `Detections` in `Spp_list` of `Data_compiled.RData`)
    * Hab_specialist, Migratory, HumanCommensal, Omnivore, Insectivore, Ground, SGCN: Logical (TRUE/FALSE) indicators of whether the species was classified as a habitat specialist, migratory, a human commensal, an omnivore, an insectivore, one that nests or forages on or near the ground, or included on the Colorado Species of Greatest Conservation Need list, respectively (see manuscript Table 1)
    * Mass: Species body mass (g), obtained from the AVONET database (see manuscript); used to classify small-bodied species (mass less than the median across the species in this study)
  * `Spp_total_management_effects.csv` (136 rows x 13 columns) and `Spp_unexplained_management_effects.csv` (136 rows x 14 columns): Species-specific total management relationships, and the portions of those relationships not explained by human traffic, respectively. Each relationship is the natural log of the ratio of species abundance predicted under a management scenario to abundance predicted under a reference scenario (log ratio of abundance), calculated from posterior samples of the path analysis model. Scenarios are defined in `Path_analysis_source.R` in the code archive (Latif 2026). Rows are the 136 species (of the 150 analyzed) with supported relationships, ordered alphabetically by species code (`Spp_unexplained_management_effects.csv` also has an unnamed first column holding the row number).
    * Spp: 4-letter species code
    * HighTrail.md, HighTrail.lo, HighTrail.hi: Posterior median, 10th percentile, and 90th percentile (i.e., 80% credible interval) of the log ratio of abundance at high trail density (with average OHV restriction) relative to baseline (no trails or roads)
    * HighTrail.supp: Support category for the trail density relationship: "pos" (80% credible interval entirely above zero), "neg" (80% credible interval entirely below zero), or "none" (80% credible interval includes zero)
    * NoOHV.md, NoOHV.lo, NoOHV.hi, NoOHV.supp: Same as above but for the log ratio of abundance at high trail density with OHVs prohibited on all trails relative to high trail density with OHVs prohibited on no trails
    * HighRoad.md, HighRoad.lo, HighRoad.hi, HighRoad.supp: Same as above but for the log ratio of abundance at high road density relative to baseline (no trails or roads)

Code/Software
=============

Linked with this data repository is a code archive (Latif 2026) containing all R scripts for compiling data, implementing all analyses, and generating all results reported in the manuscript. The code archive is a Zenodo record that preserves a snapshot (release v1.3) of a private GitHub repository Bird-Conservancy-of-the-Rockies/COREC-analysis. The Zenodo record is the citable link for the code and the only code link provided with this data repository. The README file within the code archive catalogs each script file contained therein. Files contained in this data repository are primarily intended to promote transparency and repeatability of all results in the manuscript by allowing interested persons to run scripts provided by Latif (2026).

References
==========

Gelman, A., B. Goodrich, J. Gabry, and A. Vehtari. 2019. R-squared for Bayesian regression models. The American Statistician 73:307–309.

Latif, Q. S. 2026. Bird-Conservancy-of-the-Rockies/COREC-analysis (v1.3). Zenodo. https://doi.org/10.5281/zenodo.23419954

- - -
END OF README
