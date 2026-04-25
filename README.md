# temporal_-indices

R code for calculating dengue temporal indices based on the framework of Wen et al. (2010): occurrence probability, epidemic duration, and transmission intensity.
Version 1.00
Repository: https://github.com/shiwenvet/temporal_-indices

## Purpose

This repository provides reproducible R code to calculate three temporal indices from daily dengue case-count data. The daily data are first aggregated into weekly case counts to match the original publication, in which the temporal unit was a 7-day week.

The three indices are:

1. **Occurrence probability (α)**  
   The proportion of epidemic weeks with one or more dengue cases.

2. **Duration index (β)**  
   The mean number of consecutive weeks per epidemic wave.

3. **Transmission intensity index (γ)**  
   The mean incidence of cumulative dengue cases per epidemic wave, using epidemic waves that persisted for more than two weeks.

## Required input data

The input dataset should be a `.csv` file containing daily case-count data.

Minimum required columns:

| Column | Description | Example |
|---|---|---|
| `area` | Spatial unit, such as district or Li | Sanmin |
| `date` | Date of observation or symptom onset | 2023-01-01 |
| `cases` | Number of dengue cases on that date in that area | 3 |

Optional column:

| Column | Description | Example |
|---|---|---|
| `population` | Population of the spatial unit | 100000 |

Example input:

```csv
area,date,cases,population
Sanmin,2023-01-01,0,100000
Sanmin,2023-01-02,2,100000
Sanmin,2023-01-10,3,100000
Fengshan,2023-01-01,1,120000
Fengshan,2023-01-08,0,120000
Fengshan,2023-01-15,4,120000

Accession and installation
1. create a local repository: 
Code:

install.packages("devtools")

devtools::source_url(
  "https://raw.githubusercontent.com/shiwenvet/temporal_-indices/main/R/calculate_temporal_indices.R"
)


Citation

This code was developed based on the temporal index framework described in:

Wen, T.-H., Lin, N. H., Chao, D.-Y., Hwang, K.-P., Kan, C.-C., Lin, K. C.-M., Wu, J. T.-S., Huang, S. Y.-J., Fan, I.-C., & King, C.-C. (2010). Spatial–temporal patterns of dengue in areas at risk of dengue hemorrhagic fever in Kaohsiung, Taiwan, 2002. International Journal of Infectious Diseases, 14(4), e334–e343.