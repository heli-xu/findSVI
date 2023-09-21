---
title: 'findSVI: an R package to calculate Social Vulnerability Index'
tags:
  - R
  - social vulnerability
  - Census data
  - community resilience
  - spatial inequity
authors:
  - name: Heli Xu
    orcid: 0000-0002-9792-2727
    corresponding: true
    affiliation: 1
  - name: Ran Li
    orcid: 0000-0002-4699-4755
    affiliation: 2
  - name: Usama Bilal
    orcid: 
    affiliation: "2, 3"
affiliations:
 - name: Independent Researcher
   index: 1
 - name: Urban Health Collaborative, Drexel Dornsife School of Public Health, Philadelphia, PA
   index: 2
 - name: Department of Epidemiology and Biostatistics, Drexel Dornsife School of Public Health, Philadelphia, PA
   index: 3
date: 21 September 2023 
bibliography: paper.bib
---

# Summary

As a tool to assess the resilience of communities in preparation for
public health crisis, Centers for Disease Control and Prevention and
Agency for Toxic Substances and Disease Registry Social Vulnerability
Index (CDC/ATSDR SVI, or SVI) uses percentile ranks of US Census
variables to indicate the relative vulnerability of a community within a
region[@flanagan2011social]. With minor modifications over time, CDC/ATSDR SVI
currently includes 16 Census variables in 4 themes/domains based
(Socioeconomic Status, Household Characteristics, Racial & Ethnic
Minority Status and Housing Type & Transportation), and percentile ranks
are performed for each variable, each theme, and all themes. In the
event of public health emergencies, communities with higher SVI are
considered more vulnerable, i.e., more likely to suffer serious
consequences such as mortality/physical injuries, displacement and
economic loss. Geospatial Research, Analysis, and Services Program
(GRASP) maintains the CDC/ATSDR SVI database[@centers2021cdc] and updates SVI
every two years for every county or census tract, with percentile ranks
performed at the state (and District of Columbia, Puerto Rico) or U.S.
level.

Using the methodology of CDC/ATSDR SVI, findSVI aims to support SVI
calculations using more up-to-date Census data at more flexible
geographic levels. Leveraging tidycensus package [ref], findSVI allows
data retrieval from American Community Survey for 2012-2021, with more
options for geographic levels (e.g., ZCTA, places) and region (e.g.,
county-specific, multiple states). In addition, findSVI provides an
efficient SVI analysis workflow for requests involving multiple
year-state pairs that need to be ranked separately. Apart from returning
the result as an SVI data frame, findSVI also supports output as an SVI
table with simple feature geometry for spatial analysis (e.g., with sf
package [ref]).

FindSVI makes use of the tidycensus package [ref] for Census data
retrieval, dplyr [ref], purrr, stringr, tidyr, tidyselect, rlang
packages for data manipulation and cli package for user-friendly error
messages.

# Statement of Need

CDC/ATSDR SVI is a widely used indicator to measure the differential
impact of public health crisis on communities with different
socioeconomic and demographic characteristics. In addition to
facilitating effective planning of social services and public assistance
for disasters, CDC/ATSDR SVI has also provided a valuable metric for
studying the relationships between social vulnerability and various
health-related outcomes, such as surgical outcomes[@paro2021profiles; @hyer2021high],
cardiovascular diseases[@khan2021social; @bevan2022neighborhood; @ibrahim2023impact], as well as COVID-19 in recent years. In
particular, CDC/ATSDR SVI has been used to shed light on spatial
inequity in COVID-19 resources and the disproportionate impact of
COVID-19 on vulnerable communities[reffff].

With findSVI, the workflow involved in obtaining CDC/ATSDR SVI can be
streamlined and standardized for a research project among all team
members. Provided with year(s), region(s) and a geographic level of
interest, ‘findSVI’ retrieves the required variables from US Census data
and calculates SVI for communities in the specified area. Furthermore,
with its features to retrieve Census data at more flexible geographic
levels and for more recent years, findSVI offers researchers more
options for using CDC/ATSDR SVI to accurately reflect the region/year of
focus for both association studies and spatial analyses.

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
