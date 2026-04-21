## -----------------------------------------------------------------------
#| include: FALSE
library(BiocStyle)

#### load tidyverse packages that include: dplyr, ggplot2, stringr, purrr, tidyr etc...
library(tidyverse)

install.packages(tidyverse)
library(tidyverse)

install(PeacoQC)
library(PeacoQC)

install.packages("BiocManager")
library(BiocManager)

install.packages("remotes")
library("remotes")

remotes::install_github("hally166/flowSpectrum")
library("flowSpectrum")

# remotes::install_version("ggplot2", version = "3.5.2", repos = "https://cloud.r-project.org")

remotes::install_github("DavidRach/Luciernaga", ref = "v0.99.7")
library("Luciernaga")

install.packages("purrr")
library("purrr")

# # Location: Bioconductor
# # Website: https://www.bioconductor.org/packages/release/bioc/html/flowCore.html#
install("flowCore")
library("flowCore")

# # Location: Bioconductor
# # Website: https://www.bioconductor.org/packages/release/bioc/html/flowWorkspace.html
install("flowWorkspace")
library("flowWorkspace")

# # Location: CRAN
# # Website: https://stringr.tidyverse.org/
#
install.packages("stringr")
library("stringr")


# # Location: Bioconductor
# # Website: https://www.bioconductor.org/packages/release/bioc/html/ggcyto.html#
install("ggcyto")

# # Location: Bioconductor
# # Website: https://www.bioconductor.org/packages/release/bioc/html/openCyto.html
#
install("openCyto")
library("openCyto")

# # Location: CRAN
# # Website: https://xml2.r-lib.org/
#
install.packages("xml2")
library("xml2")


# # Location: GitHub
# # Website: https://github.com/saeyslab/CytoNorm

install_github("saeyslab/CytoNorm")

# # Location: CRAN
# # Website: https://lubridate.tidyverse.org/
#
install.packages("lubridate")
library("lubridate")

# # Location: CRAN
# # Website: https://httr2.r-lib.org/
#
install.packages("httr2")
library("httr2")

# # Location: Bioconductor
# # Website: https://www.bioconductor.org/packages/release/bioc/html/flowGate.html
#
install("flowGate")
library("flowGate")

# # Location: CRAN
# # Website: https://devtools.r-lib.org/
#
install.packages("devtools")
library("devtools")

# # Location: CRAN
# # Website: https://plotly.com/r/getting-started/
#
install.packages("plotly")
library("plotly")

# # Location: GitHub
# # Website: https://github.com/biosurf/cyCombine
#
install_github("biosurf/cyCombine")
library(cyCombine)

# # Location: Bioconductor
# # Website: https://www.bioconductor.org/packages/release/bioc/html/CytoML.html
#
install("CytoML")
library(CytoML)

# # Location: CRAN
# # Website: https://github.com/jkrijthe/Rtsne
#
install.packages("Rtsne")
library(Rtsne)

# # Location: CRAN
# # Website:https://jlmelville.github.io/uwot/
#
install.packages("uwot")
library("uwot")

##### Problem#1 -----

PeacoQC::PeacoQC		#Peak-based detection of high quality cytometry data
PeacoQC::PeacoQCHeatmap		#Make overview heatmap of quality control analysis
PeacoQC::PlotPeacoQC    #Visualise deleted cells of PeacoQC

#### problem#2 -----

# * How many cytometry packages? A=/ 69 packages
# * the author/maintainer with the most contributed cytometry R packages A=/ Greg Finak and Mike Jiang
# * couple packages that you would be interested in exploring more in-depth later in the course: ggcyto, CATALYST, COMPASS and diffcyt

### Problem #3 -----

# I am still working in R studio (and I feel very comfortable on it). I will try to migrate to Positron, but not yet...

