# Turning In Optional Take-Home Problems

This folder is for the use of submitting your completed Take-Home Problems for evaluation by course instructors. Please see [Getting Help](/course/00_Homeworks/index.qmd) walkthrough for more detailed instructions. 

Within your branch, inside this "homeworks" folder, create a new folder (name it with your GitHub username). Then copy all files you will be submitting within your folder.  Then commit the change to git, and push to GitHub. See [Getting Help](/course/00_Homeworks/index.qmd)for details on submitting the pull request to the UMGCCCFCSR/CytometryInR homework branch. 

# Take-home Problems

Below you will find the optional take-home problems for Week #1. Learning how to code well requires continous practice, and that involves cycles of trying something, failing, and troubleshooting to get it working. The goal of these problems is to help you explore the topic in greater depth than the currated boundaries of this one lecture. 

You are more than welcome to open a [Discussion](https://github.com/UMGCCCFCSR/CytometryInR/discussions) to engage with others in the course to discuss these questions. Once you are done tinkering, and want to get final instructor feedback on your work, place your files within a folder, and follow the instructions to submit it as a [Pull Request](/course/00_Homeworks/index.qmd#submitting-take-home-problems) to the Cytometry In R repositories homework branch. 

:::{.callout-tip title="Problem 1"}
We installed PeacoQC during this session, but we didn't have time to explore what functions are present within the package. Using what you have learned about accessing documentation, figure out and list what functions it contains
:::

```{r}
?PeacoQC
```

:::{.callout-tip title="Problem 2"}
Take a closer look at the list of Bioconductor [cytometry](https://www.bioconductor.org/packages/release/BiocViews.html#___FlowCytometry) packages. Report back on how many there are currently in Bioconductor, the author/maintainer with the most contributed cytometry R packages, and a couple packages that you would be interested in exploring more in-depth later in the course. 
:::

Answer: Of todays date (3/3-2026) there are 69 packages for Flow Cytometry in Bioconductor. Mike Jiang is the most featured author of packages, and I would like to look closer into the high-parameter visualisation packages.
 
:::{.callout-tip title="Problem 3"}
There is another way to install R packages, using the newer [pak](https://pak.r-lib.org/) package. Positron uses this when installing suggested dependencies. 

After learning more about it via the documentation and it's pkgdown website, I would like you to attempt to install the following three R packages using this newer method: "broom", "cytoMEM", "DillonHammill/CytoExploreR". 

Take screenshots, and in a new [quarto markdown document](/course/00_Quarto/index.qmd), describe how the installation process differed from what you saw for `install.packages()`, `install()` and `install_github()`.
:::

### Installing pak from CRAN

First we will need to install the "pak" package from CRAN. The homepage suggest using this code block for the installation taking into account package type, operating system, and CPU architecture:

install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))

# Differences from install.packages(), install() and install_github().

We are tasked with installing "broom", "cytoMEM", and "DillonHammill/CytoExploreR" using the new "pak" feature.
The code is universal regardless of the package-origin, which makes it easier to use.
We only need the correct name of the package, and if it is from GitHub, we also need to correct formatting telling "pak" who is the provider of the package (in the case below it is "DillonHammill").

```{r}
pak::pkg_install("broom")
pak::pkg_install("cytoMEM")
pak::pkg_install("DillonHammill/CytoExploreR")
```

I got an error on some dependencies when trying to install the CytoExploreR:
Error:
! ! error in pak subprocess
Caused by error: 
! Could not solve package dependencies:
* DillonHammill/CytoExploreR:
  * Can't install dependency EmbedSOM (>= 1.0.0)
  * Can't install dependency superheat (>= 1.0.0)
* EmbedSOM: Can't find package called EmbedSOM.
Hide Traceback
    ▆
 1. └─pak::pkg_install("DillonHammill/CytoExploreR")
 2.   └─pak:::remote(...)
 3.     └─err$throw(res$error)

 I tried to install these dependencies with this code block.
 
 ```{r}
 pak::pkg_install("EmbedSOM")
 pak::pkg_install("superheat")
 ```

This gave the following error:
Error:
! ! error in pak subprocess
Caused by error: 
! Could not solve package dependencies:
* EmbedSOM: Can't find package called EmbedSOM.
Hide Traceback
    ▆
 1. └─pak::pkg_install("EmbedSOM")
 2.   └─pak:::remote(...)
 3.     └─err$throw(res$error)

 I Googled the package name and found it to be present on GitHub "exaexa/EmbedSOM"

 ```{r}
 pak::pkg_install("exaexa/EmbedSOM")
 ```

That seemed to work, and I tried the same approach for the "superheat" package. This was also on GitHub "rlbarter/superheat".

```{r}
 pak::pkg_install("rlbarter/superheat")
```

Then I tried the "CytoExploreR" installation again.

```{r}
pak::pkg_install("DillonHammill/CytoExploreR")
```

That seemed to work without any errors :)

