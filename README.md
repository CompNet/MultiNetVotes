MultiNetVotes
=======
*Multiple Partitioning of Multiplex Signed Networks: Application to European Parliament Votes*

* Copyright 2018-19 Nejat Arinik & Vincent Labatut

MultiNetVotes is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. For source availability and license information see `licence.txt`

* Lab site: http://lia.univ-avignon.fr/
* GitHub repo: https://github.com/CompNet/MultiNetVotes
* Contact: Nejat Arinik <arinik9@gmail.com>

-----------------------------------------------------------------------

## Description
This set of `R` scripts was designed to analyze the European Parliament votes through a *multiplex network*-based approach. See reference [[AFL'20](#references)] for more details.

This software is based on our previous project [NetVotes](https://github.com/CompNet/NetVotes). If you use this source code, or the associated data, please cite reference [[AFL'20](#references)]:
```bibtex
@Article{Arinik2020,
  author    = {Arınık, Nejat and Figueiredo, Rosa and Labatut, Vincent},
  title     = {Multiple Partitioning of Multiplex Signed Networks: Application to {E}uropean {P}arliament Votes},
  journal   = {Social Networks},
  year      = {2020},
  volume    = {60},
  pages     = {83-102},
  doi       = {10.1016/j.socnet.2019.02.001},
}
```


## Data
Our tool was applied to data representing the activity of the members of the European Parliament (MEPs) during the 7th term (from June 2009 to June 2014), as described in [[AFL'20](#references)]. The raw data describing this activity were retrieved from the [It's Your Parliament](http://www.itsyourparliament.eu/) website. There were some minor issues with these data, which we had to correct: some MEPs were represented twice, some profiles were incomplete, the policy domains were not defined for all vote texts, etc. These cleaned data, as well as our figures and results, are available on [Zenodo](https://doi.org/10.5281/zenodo.6816121).


## Organization
Here are the folders composing the project:
* Folder `src`: contains the source code (R scripts).
* Folder `in`: contains the files used by our scripts, i.e. the inputs.
  * Folder `itsyourparliament`: this folder will be contain raw input files. The raw data can be downloaded from [Zenodo](https://doi.org/10.5281/zenodo.6816121).
  * Folder `_overall`: the files in this folder will be generated from raw data.
    * `all-votes.csv`: individual vote data, i.e. how each MEP voted.
    * `dom-details.csv`: Committee and polcy details for each domain.
    * `mep-details.csv`: description of each MEP.
    * `rollcall-details.csv`: description of each voted roll-call.
    * `rollcall-domains.csv`: the domains associated to the roll-calls.
    * (optional) `rollcall-theme-details.csv`: theme details associated to roll-calls, depending on domains. Extra efforts needed. An example file is included. If it will be used, a new column `Theme Id` should be added into `rollcall-details.csv`, and fill in it for the considered roll-calls. See 'src/main.R' for a bit more details.
  * Folder `score`: score files describing agreement index. It is used when extracting networks.
* Folder `lib`: contains executable files related to the used external partitioning methods.
  * Folder `ExCC`: Executable file of the method `ExCC` whose the name will be `cplex-partition.jar`. See the *Installation* section for more details.
* Folder `out`: contains the folders and files produced by our scripts. See the *Use* section for more details.


## Installation
1. Install the [`R` language](https://www.r-project.org/)
2. Install the following R packages:
   * [`igraph`](http://igraph.org/r/) Tested with the version 1.2.2.
   * [`XML`](https://cran.r-project.org/web/packages/XML/index.html)
   * [`msm`](https://cran.r-project.org/web/packages/msm/index.html)
   * [`alluvial`](https://cran.r-project.org/web/packages/alluvial/)
   * [`cluster`](https://cran.r-project.org/web/packages/cluster/)
   * [`stringr`](https://cran.r-project.org/web/packages/stringr/)
   * [`plotrix`](https://cran.r-project.org/web/packages/plotrix/)
   * [`ggplot2`](https://cran.r-project.org/web/packages/ggplot2/)
   * [`scales`](https://cran.r-project.org/web/packages/scales/)
3. Install [`Circos`](http://circos.ca/). Tested with the version 0.69.6. Set correctly the variable `CIRCOS_CMD` in `circos.R` (e.g. `/opt/circos-0.69-6/bin/circos`).
3. Install [`IBM CPlex`](https://www.ibm.com/developerworks/community/blogs/jfp/entry/CPLEX_Is_Free_For_Students?lang=en). Tested with the version 12.8 and 20.1. Set correctly the variable `CPLEX.BIN.PATH` in `define-algos.R` (e.g. `/opt/ibm/ILOG/CPLEX_Studio128/cplex/bin/x86-64_linux/`).
   * For ubuntu, type the following command:
     * `sudo ./cplex_studio12.8.8.linux-x86-64.bin` 
       * The default installation location for education version is: `/opt/ibm/ILOG/CPLEX_Studio128`.
       * The default installation location for trial version is: `/opt/ibm/ILOG/CPLEX_Studio_Community128/cplex/bin/x86-64_linux/`.
4. Download the project of `ExCC` from [GitHub](https://github.com/CompNet/ExCC). First, configure and then compile it. To test it, you can run the file `run.sh`.If everything works (i.e. if a file `ExCC-result.txt` created in the output folder), move the exectuable file `cplex-partition.jar`, which is in `exe`, into the folder `lib/ExCC` in this project.
4. Download the raw data from from [Zenodo](https://doi.org/10.5281/zenodo.6816121). The only needed data is the folder named `itsyourparliament.zip`. Unzip it, then place it under `in/` (overwrite the existing one).
5. Download this project from GitHub.


## Use
1. Set correctly the variables `CIRCOS_CMD` and `CPLEX.BIN.PATH`.
2. Open the `R` console.
3. Set the current directory as the working directory, using `setwd("<my directory>")`.
4. Run the main script `src/main.R`.

The script will produce the following subfolders in the folder `out`:
* `rollcall-networks`: Single roll-call networks representing the votes voted by MEPs for an indivudal roll-call. Subfolders are structured by political groups, countries, domains and periods. At the end, period folders contain as many roll-calls as folders.
* `rollcall-partitions`: Partitions of the single roll-call networks. It depends on the variable `cons.vote.types.list`. Subfolders are structured by political groups, countries, domains and periods. At the end, period folders contain as many roll-calls as folders.
* `rollcall-clustering`: Clustering of the considered roll-calls via `k-medoids` in order to regroup similarly voted roll-calls. Subfolders are structured by political groups, countries, domains and periods.
  * `votetypes=<VOTE_TYPE>`: A subfolder for each considered vote types (e.g. `FAA` which stands for `FOR`, `AGAINST`, `ABSTAIN`)
    * `<MEASURE_NAME>-k=<K_VALUE>-sil=<SILHOUETTE_SCORE>`: A subfolder for each considered `k` value (i.e. desired number of clusters) in `k-medoids`, depending on similarty measures.
      * `clu<CLUSTER_ID>`: A subfolder for each cluster of the resulting `k-medoids` result (e.g. `clu1` and `clu2` for `2-medoids` result)
        * `networks-with-meps-absence-thresh=<THRESHOLD>`: A subfolder containing aggregated networks (after `k-medoids`) for treshold values of the MEPs absence. Networks might be signed, unsigned, or positive part of the signed version.
        * `signed-partitions-with-meps-absence-thresh=<THRESHOLD>`: A subfolder containing partitions of the aggregated signed networks obtained through signed graph partitioning methods (might be correlation clustering methods,  or community detection ones designed for signed networks).
        * `unsigned-partitions-with-meps-absence-thresh=<THRESHOLD>`: A subfolder containing partitions of the aggregated unsigned networks obtained through unsigned graph partitioning methods (i.e. community detection methods).


## References
* **[AFL'20]** N. Arinik, R. Figueiredo & V. Labatut. *Multiple Partitioning of Multiplex Signed Networks: Application to European Parliament Votes*. Social Networks 60:83-102, 2020. [doi: 10.1016/j.socnet.2019.02.001](https://doi.org/10.1016/j.socnet.2019.02.001) - [⟨hal-02082574⟩](https://hal.archives-ouvertes.fr/hal-02082574)

