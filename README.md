# gems_ap_cog

# Long-term exposure to air pollution and cognitive aging: Findings from a U.S. cohort of older adults 
Authors: Sindana D. Ilango, Claire E. Adam, Xinmei Huang, Cindy S. Leary, Erin O. Semmens, Annette L. Fitzpatrick, Paul K. Crane, Joel D. Kaufman, Anjum Hajat

This repository contains code used to analyze the associations between air pollution, cognitive level, and cognitive decline in the Ginkgo Biloba of Memory Study (GEMS). 

# Data Sources
Data for this study is from GEMS (Snitz et al.) and from an analysis on air pollution and dementia (Semmens et al.). 
- Snitz BE, O’Meara ES, Carlson MC, Arnold AM, Ives DG, Rapp SR, Saxton J, Lopez OL, Dunn LO, Sink KM, DeKosky ST. Ginkgo biloba for preventing cognitive decline in older adults: a randomized trial. Jama. 2009 Dec 23;302(24):2663-70.
- Semmens EO, Leary CS, Fitzpatrick AL, Ilango SD, Park C, Adam CE, DeKosky ST, Lopez O, Hajat A, Kaufman JD. Air pollution and dementia in older adults in the Ginkgo Evaluation of Memory Study. Alzheimer's & Dementia. 2023 Feb;19(2):549-59.

# Code
The following SAS programs are included in this repo: 
## 01_prep_data.sas
Code to prepare dataset for main and supplementary analyses. This program combines neuropsych testing administered to gems particiants to cohort data	and assigns study times and air pollution exposure. Tests from the full neuropsych exam is grouped into the following cog domains: (1) memory: cvlt_frl and ro_dr; (2) construction: ro_copy and blocks; (3) attention: dspan_f and ta_time; (4) language: bnt_spon and wg_anim; (5) executive function: tb_time and strink
## 02_prep_sens.sas
Code to prepare dataset for supplementary analyses 
## 03_analysis.sas
Code for analyses


