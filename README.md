# lbd-contrast
An experimental constrast approach for Literature-Based Discovery

This is the companion code for the paper "A new paradigm for literature-based discovery: a contrastive approach".

The approach relies on contrasting different “views” of the data, the data being the biomedical literature (Medline and PubMed Central). The experiments use 8 relations which correspond to past discoveries in the domain of Neurodegenerative Diseases (NDs).

- The paper can be found at **TODO link**
- The code is available at https://github.com/erwanm/lbd-contrast

This repository contains a library of R functions to manipulate the data, apply the new contrast methods described in the paper and evaluate the results. It requires some preprocessed data which can be obtained either by downloading the dataset used in the paper **TODO link** or by running the full preprocessing. 

A step by step documentation is provided in the form of two [R Markdown](https://rmarkdown.rstudio.com/) documents:

- [Part 1: full replication](part1-full-replication.html): follow these instructions to run the full preprocessing, then go to step 2 below.
- [Part 2: reproducibility study](part2-short-reproducibility-study.html): follow these instructions in order to reproduce the experiment with the preprocessed data.

