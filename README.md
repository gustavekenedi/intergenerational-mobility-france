# Replication files for *Intergenerational Income Mobility in France: A Comparative and Geographic Analysis*

*Authors:* Gustave Kenedi and Louis Sirugue

*Journal:* [Journal of Public Economics](https://www.sciencedirect.com/science/article/pii/S0047272723001561)

*Year:* 2023

**Should you notice any error(s) in our code or issues with our analysis, please reach out at `gustavekenedi@gmail.com` or `louis.sirugue@psemail.eu`, we'll be happy to discuss it with you and make any corrections if necessary.**

## General notes

This repository contains all the code files necessary to replicate our paper as well as the paper's figures. We provide `R` markdown or quarto notebooks, as well as the data outputs and code used to construct the figures in the paper.

#### Software requirements

You can download `R` [here](https://cloud.r-project.org/) and `RStudio` [here](https://posit.co/downloads/). Once all set up, the only package you need to manually install is [`librarian`](https://cran.r-project.org/web/packages/librarian/vignettes/intro-to-librarian.html).

#### Folders

Three sets of codes and/or outputs are provided:

1.  `intergen_fr/`: the analysis to compute estimates of intergenerational mobility in France using the *Permanent Demographic Sample (EDP)*.

2.  `psid_validation/`: the analysis to validate the two-sample two-stage least squares (TSTSLS) methodology using the *Panel Study of Income Dynamics (PSID)*.

3.  `figures/`:

    1.  `code.rmd`: code to re-create (almost) all the figures in the paper (main + appendix). The figures in Appendix B PSID Validation Exercise are created directly within the code of the PSID validation exercise (see below).

    2.  `figures_paper/`: pdf figures from the paper (main text + appendices).
    
    3.  `other_data/`: some other data files necessary to create the figures (shapefiles, data from Chetty et al. (2020), etc.).
    
    4. `out/`: folder to store the figures when you run `code.rmd`. This avoids overwriting the original figures.

## Intergenerational mobility in France

*Data used:* for this analysis we use the *2017 version* of the *Permanent Demographic Sample (EDP)*. This dataset can be accessed from the French [*Secure Data Access Center (CASD)*](https://www.casd.eu/en/). Additional information on the EDP can be found [here](https://www.casd.eu/en/source/permanent-demographic-sample), and all the documentation **in French** is available [here](https://utiledp.site.ined.fr/fr/variables/variables-de-l-edp/). Note that new versions of the EDP have become available since we started working on this project and we have not checked whether our code is compatible with newer versions.

The folder `intergen_fr/` contains two subfolders:

-   `code/`: all cleaning and analysis codes.

    - `0_convert_src_data.sas`: convert raw SAS data files to csv.
    
    - `1_data_cleaning.Rmd`: data cleaning code.
    
    - `2_article.Rmd`: all results from the paper.
    
    - `3_bootstrap.Rmd` and `4_master_bootstrap.R`: for bootstrap standard errors.

-   `figures_data/`: underlying data for the figures in the paper.

## PSID Validation Exercise

*Data used:* for this analysis we use the *Panel Study of Income Dynamics (PSID)*. This dataset can be accessed from the University of Michigan's [*Institute for Social Research*](https://psidonline.isr.umich.edu/). We use the **raw** version of the PSID which can be downloaded [here](https://simba.isr.umich.edu/Zips/ZipMain.aspx?). We downloaded:

-   every family file (`Main Study/Family Files`) from 1968 to 2019 (without the wealth supplements which we do not use), and
-   the cross-year individual file (`Main Study/Cross-year Individual`) 1968-2019 (now 2021).

We also make use of an Excel version of the PSID cross-year index avaiable in `/psid_validation/data/psid.xlsx`. Note that the a potentially updated version of this file can be found [here](https://psidonline.isr.umich.edu/help/xyr/psid.xlsx). To replicate our analysis we recommend using our file since we have not checked whether the code runs with the (potentially) newer version. Note that you'll need to change the paths in `qmd` files manually.

### Converting PSID .sas to .csv

In order to use the PSID data in `R`, we ran the code in `code/sas_to_csv.R`. This code converts the `sas` files to `csv`. It unfortunately takes quite a while to run (a couple of hours if I recall correctly), but it should work. If you know of a simpler way of doing this please let us know. Once all this is done, you should be able to open the PSID data files in `R`.

### Code files

The folder `psid_validation/` contains three subfolder:

-   `code/`: cleaning and analysis codes
-   `code_lib/`: any functions created for the analysis
-   `data/`: only contains the very useful `psid.xlsx` excel file which contains a cross-year index of variables which we make extensive use of.

Within `code/` you need to run the files in the following order:

1.  `income_data.qmd`: generates a dataset containing the various income definitions we use.

2.  `psid_sample.qmd`: generates datasets containing the parent and chil samples.

3.  `psid_variables.qmd`: creates the parent characteristics necessary for the TSTSLS method.

After that, you should be able to run both `psid_results.qmd` which contains the code for the results of the TSTSLS validation exercise, and `psid_desc_stats.qmd` which contains the code for the descriptive statistics.

## Figures

- **Figures themselves:** all the figures from the paper (main text and appendices) can be found in `figures/figures_paper/`.

- **Figure codes:** the code to create the figures can be found in `code.rmd`. The best way to use this code while avoiding issues with paths is to double-click on `replication_files.Rproj` to open this project in `RStudio`, and then simply running `code.rmd` and all the figures will be saved in `figures/out/`.
