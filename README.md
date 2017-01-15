# SeriesNetDynMatch
==============================
Dynamic community matching for TV series conversational networks

* Copyright 2016-17 Vincent Labatut 

SeriesNetDynMatch is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. For source availability and license information see `licence.txt`

* Lab site: http://lia.univ-avignon.fr/
* GitHub repo: https://github.com/CompNet/SeriesNetDynMatch
* Contact: Vincent Labatut <vincent.labatut@univ-avignon.fr>

-----------------------------------------------------------------------

# Description
This set of R scripts was designed to perform community detection on dynamic networks corresponding to conversational interactions between characters of TV series. It also can generate alluvial diagrams representing the evolution of these interactions, as well as plots of the evolution of the character strength and the link weights. 

These scripts were used in article [BLGL'17], which deals with TV series [Breaking Bad](https://en.wikipedia.org/wiki/Breaking_Bad), [Game of Thrones](https://en.wikipedia.org/wiki/Game_of_Thrones), and [House of Cards](https://en.wikipedia.org/wiki/House_of_Cards_(U.S._TV_series)). 


# Data
The scripts have been written to be applied on the data previously extracted from the listed TV series. These data take the form of Graphml networks, which are available on [FigShare](https://figshare.com), at the address [https://dx.doi.org/10.6084/m9.figshare.2199646](https://dx.doi.org/10.6084/m9.figshare.2199646). 

Download the data and unzip them in the `data` folder of this project, in order to match the existing folder structure. 


# Organization
Here are the folders composing the project:
* Folder `src`: contains the source code (R scripts).
* Folder `data`: contains the files used by our scripts, i.e. the inputs, as well as the folder created by our scripts, i.e. the outputs.
  * Folder `xxx_dyn_ns`: input files for series xxx, corresponding to graphs obtained through narrative smoothing. 
  * Folder `xxx_dyn_ns_alluv`: alluvial diagrams generated for series xxx (only for the graphs obtained through narrative smoothing).
  * Folder `xxx_dyn_ns_clstr`: community detected for series xxx (only for narrative smoothing).
  * Folder `xxx_dyn_ns_match`: community matches obtained for series xxx (only for narrative smoothing).
  * Folder `xxx_dyn_ts10`: input files for series xxx, corresponding to graphs obtained through temporal integration using 10-scene windows. 
  * Folder `xxx_dyn_ts40`: input files for series xxx, corresponding to graphs obtained through temporal integration using 40-scene windows.
  * Folder `BB_dyn_strength`: strength and weight plots generated for all three types of graphs (narrative smoothing, 10- and 40-scene windows).


# Installation
1. Install the [`R` language](https://www.r-project.org/)
2. Install the following R packages:
   * [`igraph`](http://igraph.org/r/) (tested with version 1.0.1).
   * [`alluvial`](https://cran.r-project.org/web/packages/alluvial/index.html) (tested with version 0.1-2)
3. Download this project from GitHub and unzip the archive.
4. Download the data from FigShare and unzip in the data folder, so as to match the existing folder structure.


# Use
In order to replicate the experiments from the article, perform the following operations:

1. Open the `R` console.
2. Set the project root directory as the working directory, using `setwd("<my directory>")`.
3. Using the command `source("<script.R>")`, run:
   * The script `src/custom-match.R` to perform community detection and matching on the narrative smoothing graphs;
   * Or the script  `src/strength-evol.R` to generate comparison plots from the three types of graphs.

The generated files will be placed in the data folder, consistently with the description given in the Organization section.


# Dependencies
* [`igraph`](http://igraph.org/r/) package: used to build and handle graphs.
* [`alluvial`](https://cran.r-project.org/web/packages/alluvial/index.html) package: used to generate the alluvial diagrams.

Note: the `src/_archive.zip` file contains various programs and scripts which were tried in order to perform community detection and matching, but were not finally kept. There are provided here just for information.


# References
* **[BLGL'17]** X. Bost, V. Labatut, S. Gueye and G. Linarès. Extraction and Analysis of Dynamic Conversational Networks from TV Series, Submitted, 2017.
<URL goes here>
