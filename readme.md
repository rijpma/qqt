## QQT

Replications files for the paper [Quantity versus Quality: Household structure, number of siblings, and educational attainment in the long nineteenth century](http://www.cgeh.nl/quantity-versus-quality-household-structure-number-siblings-and-educational-attainment-long-nineteen).

### Data files

The data are harmonised in the [qqtdata.r script](qqtdata.r). Most raw datafiles are available online:
* [North Atlantic Population Project](https://www.nappdata.org/napp/).
* [Mosaic project](http://censusmosaic.org).
* [Canadian Families](http://web.uvic.ca/hrd/cfp/data/index.html).
* [National Archives of Ireland](http://www.census.nationalarchives.ie).

Change the working directory to match the location of the files. The harmonised files are left in a subdirectory called cleandata. A table of overall summary statistics is created as well.

### Analyses

After processing the data, the analyses can be carried out in the [qqtanalyses.r script](qqtanalyses.r). It first creates a number of plots, tables with summary statistics, and harmonised regressions, followed by all the regressions for the appendix.

### Helper functions

The [script qqtheader.r]( qqtheader.r) contains a number of functions used in the other two scripts.

### local files

There are a number of references to local files that are not directly accessible on the internet:
* ~/dropbox/cliodata/allcliodata_raw.csv // should be in new composite folder
* ~/dropbox/viennapaper/data/comped.csv // ask Lotte
* ~/dropbox/viennapaper/data/europeancities.csv // xls file at


