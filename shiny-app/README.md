# Welcome to TADAShiny: Data Discovery and Cleaning

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

[![](https://github.com/USEPA/TADAShiny/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/USEPA/TADAShiny/actions/workflows/R-CMD-check.yaml)

TADAShiny provides a user interface (<https://owshiny-dev.app.cloud.gov/tada-dev/>) on top of the TADAPackage (<https://github.com/USEPA/TADA>). This application can be used to compile and evaluate Water Quality Portal (WQP) data for samples collected from surface water monitoring sites on streams and lakes.

In 2012, the WQP was deployed by the U.S. Geological Survey (USGS), the U.S. Environmental Protection Agency (USEPA), and the National Water Quality Monitoring Council to combine and serve water-quality data from numerous sources in a standardized format. The WQP holds over 420 million water quality sample results from over 1000 federal, state, tribal and other partners, and is the nation's largest source for single point of access for water-quality data. Participating organizations submit their data to the WQP using the EPA's Water Quality Exchange (WQX), a framework designed to map their data holdings to a common data structure.

TADAShiny (Module 1: Data Discovery and Cleaning) retrieves data from the WQP, and runs it through a series of data discovery, wrangling, and cleaning steps. Features include WQP data retrieval, flagging invalid results and metadata using validation reference tables, harmonization of synonyms, result and depth unit conversions, censored data substitutions, dataset filtering, and data visualizations. Users will be able to review and download summary information about their dataset, along with a data file and that is ready for additional manual review and use in subsequent analyses. Users make all decisions using the app to flag data for removal or keep data depending on its quality and relevance for their analysis.

[More about the EPA TADA Project](https://www.epa.gov/waterdata/TADA)

## Installation

You can install the development version of TADAShiny from [GitHub](https://github.com/USEPA/TADAShiny) by running:

``` r
library (remotes)
remotes::install_github("USEPA/TADAShiny", ref = "develop", dependencies = TRUE)
```

## Open-Source Code Policy

Effective August 8, 2016, the [OMB Mandate: M-16-21; Federal Source Code Policy: Achieving Efficiency, Transparency, and Innovation through Reusable and Open Source Software](https://obamawhitehouse.archives.gov/sites/default/files/omb/memoranda/2016/m_16_21.pdf) applies to new custom-developed code created or procured by EPA consistent with the scope and applicability requirements of Office of Management and Budget's (OMB's) Federal Source Code Policy. In general, it states that all new custom-developed code by Federal Agencies should be made available and reusable as open-source code.

The EPA specific implementation of OMB Mandate M-16-21 is addressed in the [System Life Cycle Management Procedure](https://www.epa.gov/irmpoli8/policy-procedures-and-guidance-system-life-cycle-management-slcm). EPA has chosen to use GitHub as its version control system as well as its inventory of open-source code projects. EPA uses GitHub to inventory its custom-developed, open-source code and generate the necessary metadata file that is then posted to code.gov for broad reuse in compliance with OMB Mandate M-16-21.

If you have any questions or want to read more, check out the [EPA Open Source Project Repo](https://github.com/USEPA/open-source-projects) and [EPA's Interim Open Source Code Guidance](https://www.epa.gov/developers/open-source-software-and-epa-code-repository-requirements).

## License

All contributions to this project will be released under the CCO-1.0 license file dedication. By submitting a pull request or issue, you are agreeing to comply with this waiver of copyright interest.

## Disclaimer

This United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.

## Contact

If you have any questions, please reach out to Cristina Mullin (mullin.cristina\@epa.gov).
