---
title: 'findSVI: an R package to calculate the Social Vulnerability Index at multiple geographical levels'
tags:
  - R
  - social vulnerability
  - census data
  - community resilience
  - spatial inequity
authors:
  - name: Heli Xu
    orcid: 0000-0002-9792-2727
    corresponding: true
    affiliation: 1
  - name: Ran Li
    orcid: 0000-0002-4699-4755
    affiliation: 1
  - name: Usama Bilal
    orcid: 0000-0002-9868-7773
    affiliation: "1, 2"
affiliations:
 - name: Urban Health Collaborative, Drexel Dornsife School of Public Health, Philadelphia, PA, United States of America
   index: 1
 - name: Department of Epidemiology and Biostatistics, Drexel Dornsife School of Public Health, Philadelphia, PA, United States of America
   index: 2
date: 3 March 2024 
bibliography: paper.bib
---

# Summary

The [Social Vulnerability Index (SVI)](https://svi.cdc.gov/Documents/Publications/CDC_ATSDR_SVI_Materials/JEH2018.pdf) was created by the Centers for Disease Control and Prevention and Agency for Toxic Substances and Disease Registry (CDC/ATSDR) as a tool to assess the resilience of communities in preparation for public health crisis [@flanagan2011social]. In the event of public health emergencies, communities with higher SVI are considered more vulnerable, i.e., more likely to suffer serious consequences such as mortality/physical injuries, displacement and economic loss. The [Geospatial Research, Analysis, and Services Program (GRASP)](https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html) maintains the SVI database and updates the SVI every two years.

The SVI uses 16 variables from the American Community Survey (ACS) [@flanagan2011social].These 16 variables are grouped into 4 themes/domains: Socioeconomic Status, Household Characteristics, Racial & Ethnic Minority Status and Housing Type & Transportation. For each variable, percentile ranks are calculated and aggregated (summed) for each theme and all themes for a geographic level of interest, which are used to generate another set of percentile ranks for a geographic unit as the theme-specific and overall SVIs. The CDC/ATSDR provides the SVI at the census tract and county levels, with percentiles calculated with respect to each state and the entire United States of America.    

The main aim of the findSVI R package is to extend the application of CDC/ATSDR SVI methodology to more geographic levels and to utilize more up-to-date ACS data. Leveraging the tidycensus R package [@walker2022tidycensus], findSVI allows data retrieval from American Community Survey for any year from 2012 to 2021, with more options for geographic levels (e.g., zip code tabulation areas or ZCTAs, places) and regions of reference for percentile calculations (e.g., county-specific, multiple states). In addition, findSVI provides an efficient SVI analysis workflow for requests involving multiple year-state pairs that need to be ranked separately. Apart from returning the result as an SVI data frame, findSVI also supports output as an SVI table with simple feature geometry for spatial analysis in R and other Geographic Information Systems tools. 

# Statement of Need

The SVI is a widely used indicator to measure the differential impact of public health crisis on communities with different socioeconomic and demographic characteristics. In addition to facilitating effective planning of social services and public assistance for disasters [@schmidt2021equitable], the SVI has also provided a valuable metric for characterizing the relationships between social vulnerability and various health outcomes, such as surgical outcomes [@hyer2021high; @paro2021profiles], environmental exposure-related illness [@do2023spatiotemporal; @lehnert2020spatial], cardiovascular diseases [@ibrahim2023impact; @bevan2022neighborhood; @khan2021social], as well as COVID-19 in recent years [@tsai2022association; @hughes2021county; @karaye2020impact; @khazanchi2020county]. In particular, CDC/ATSDR SVI has been used to examine spatial and racial/ethnic inequities in COVID-19 resources and the disproportionate impact of COVID-19 on vulnerable communities [@gaynor2020social; @bilal2021spatial; @islam2021temporal; @bilal2022racial; @bilal2022heterogeneity; @mullachery2022inequities].

However, many of the applications above had to recalculate the census-tract level SVI to match their units of analysis [@bilal2021spatial; @bilal2022racial; @bilal2022heterogeneity], including ZCTAs, a unit of analysis for which public health data tends to be available quicker in public health emergencies [@bilal2021spatial]. With findSVI, the workflow involved in obtaining SVI can be streamlined and standardized for a specific research question. Provided with year(s), region(s) of reference and a geographic level of interest, findSVI retrieves the required variables from the US Census Bureau data and calculates SVI for communities in the specified area. Furthermore, with its features to retrieve ACS data at more flexible geographic levels and for more recent years, findSVI offers researchers more options for using SVI to accurately reflect the region/year of focus for both association studies and spatial analyses.

# Functionality

Basic usage of retrieving Census data and computing SVI for a region at a geographic level can be found in the vignette [Introduction to findSVI](https://heli-xu.github.io/findSVI/articles/findSVI.html). In addition, [Validation of SVI Results](https://heli-xu.github.io/findSVI/articles/svi-validation.html) contains a reference table of the SVI variables and ACS variables across available years and a comparison between SVI database and findSVI calculations. Examples involving spatial analyses with SVI are included in [Mapping SVI for Spatial Analysis](https://heli-xu.github.io/findSVI/articles/SVI-mapping.html) and [Geographic Context of SVI](https://heli-xu.github.io/findSVI/articles/geo_context_of_svi.html). An example of using findSVI to study social vulnerabilities and health outcomes is also included in [SVI and health outcome](https://heli-xu.github.io/findSVI/articles/svi-covid.html), where the association between SVI and COVID-19 hospitalizations in Philadelphia is explored with maps and a scatter plot.

FindSVI makes use of the tidycensus package [@walker2022tidycensus] for Census data retrieval, dplyr [@dplyr], purrr [@purrr], stringr [@stringr], tidyr [@tidyr], tidyselect [@tidyselect], rlang [@rlang] packages for data manipulation and cli [@cli] package for user-friendly error messages.

# References

