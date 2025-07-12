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
* Co-authors for manuscript: Liza G. Rossi, Matthew F. McLaren, Jennifer M. Timmer, Melissa A. Dressen, Christopher C. Keefe, Heather N. Abernathy, Mark A. Ditmer
* Date created: 2023-01-25
* Date modified: 2023-02-01

Dataset Version and Release History
-----------------------------------

* Current Version:
  * Number: 0.0
  * Date: 2025-07-09
  * Persistent identifier: DOI: 10.5061/dryad.0rxwdbscz

Dataset Attribution and Usage
-----------------------------

* Dataset Title: Data for the article "Human traffic explains recreation impacts for Colorado breeding bird diversity"

* Persistent Identifier: TBD

* Dataset Contributors: Matthew F. McLaren, Jennifer M. Timmer, Heather N. Abernathy, Mark A. Ditmer

* Publisher: Bird Conservancy of the Rockies

* License: Use of these data is covered by the following license:

  - Title: CC0 1.0 Universal (CC0 1.0)

  - Specification: https://creativecommons.org/publicdomain/zero/1.0/; the authors respectfully request to be contacted by researchers interested in the re-use of these data so that the possibility of collaboration can be discussed.

* Suggested Citations:

  - Dataset citation: Latif, Q. S., M. F. McLaren, J. M. Timmer, H. N. Abernathy, and M. A. Ditmer. 2025. Data for the article "Human traffic explains recreation impacts for Colorado breeding bird diversity", Dryad, Dataset. https://doi.org/DOI:10.5061/dryad.0rxwdbscz.

  - Corresponding publication: Latif, Q. S., M. F. McLaren, J. M. Timmer, H. N. Abernathy, and M. A. Ditmer. In Review. Human traffic explains recreation impacts for Colorado breeding bird diversity. Ecological Applications.

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

`mod_path`
----------

* Description: This file is an R object containing all model output as generated by the “01-Analysis.R” script file archived in the accompanying git repository (Latif 2025). This file can be loaded using the R function 'R.utils::loadObject()'.  Broadly, this file contains posterior samples of parameter model estimates and their summaries as generated by the data analysis described in the manuscript and encoded in archived scripts (Latif 2023). This file is needed to replicate results summaries generated by various scripts located in the accompanying git repository (Latif 2025). As such, we do not provide a detailed catalog of the contents of this file.

`Data_compiled.RData`
---------------------

* Description: This is an R workspace containing all data and R objects necessary to run R scripts implementing data analyses and results summaries associated with this manuscript.

* Format: .RDATA

* Access: R objects contained in this file can be loaded into an R workspace using the Base R function 'load'.

* R objects contained in this workspace:
  * area.band: The area (m^2) of distance bins for each species
  * breaks: Break points for distance bins for each species
  * covariates: Data frame containing raw covariate values for all grid cell x year occasions (includes several covariates not ultimately included in the analysis model)
  * CovIndMat: Matrix with raw covariate values included in analysis
  * dat_human: Data frame with grid cell values for human traffic metrics
  * dat_trails: Data frame with grid cell values for management metrics
  * detection.trim.summary: Summary of detection data trimmed for each species when filtering out the furthest 5% of detections (generated by `01-Data_wrangle.R` script in Latif 2025)
  * detections: Bird detection dataset queried from the Bird Conservancy database
  * detects.cutoff: Bird detection data retained after applying the distance cutoff for species with at least 75 detections
  * detects.drop: Bird detection data dropped prior to analysis (i.e., furthest 5% for species with at least 75 detections)
  * dtobs: Species index, grid-year index, cluster size, distance bin, and time removal bin for all detections retained for analysis
  * n.mat: Count data matrix (species x grid survey occasion)
  * samples: Data frame with survey specific info (ID, Year, Date, time zone, number of point surveys completed, and time since sunrise when grid survey was initiated)
  * Spp_list: Species list dataframe (species code, common name, and sum of all counts)
  * area.circle: Point-level area of estimation for each species
  * area.prop: Proportion of the area of estimation within each distance bin (same for all species)
  * cutoff: Distance cutoff for each species (i.e., radius of point-level area of estimation)
  * grid.list: Grid cell IDs
  * gridXyears.list: Grid cell x year IDs
  * min.n.trunc: Minimum number of detections required to apply a 5% distance threshold cutoff to a species
  * nG: Number of distance bins
  * Spp: List of species codes labeling rows or elements in data objects with a species dimension (e.g., first dimension of n.mat)
  * trunc.pct: Proportion of detections to retain for species with at least 75 detections (determines distance cutoff)
  * years: Years data were collected

* Metadata note: Because this file is provided primarily to allow replicability of study results using scripts provided in the accompanying git repository (Latif 2025), we only provide a relatively high level description of the contents of this file, i.e., we briefly describe data objects but we do not necessarily describe every field appearing in each object.

`code_reference_files.zip`
----------------------

* Description: This zipped directory containing csv files referenced by R scripts archived by Latif (2025). Metadata is not provided for these files as they are simply provided for code reproducbility and are referenced by name by relevant scripts.

* Format: sub-directory

Code/Software
=============

Linked with this data repository is a git repository (Latif 2025) containing all R scripts for compiling data, implementing all analyses, and generating all results reported in the manuscript. The README file within the git repository catalogs each script file contained therein. Files contained in this data repository are primarily intended to promote transparency and repeatability of all results in the manuscript by allowing interested persons to run scripts provided by Latif (2025).

References
==========

Latif, Q. S. 2025. Bird-Conservancy-of-the-Rockies/COREC-analysis (v0). To be archived at Zenodo. currently available at https://github.com/Bird-Conservancy-of-the-Rockies/COREC-analysis

- - -
END OF README
