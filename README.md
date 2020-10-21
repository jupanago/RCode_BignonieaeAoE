# Exploring the Areas of Endemism of Bignonieae

RStudio project to explore the results of an anaysis of endemicity of the plant tribe Bignonieae implemented in NDM/VNDM v 3.1.

PhD Dissertation Chapter 2. Unraveling distribution patterns of Neotropical lianas: An analysis of endemicity of the tribe Bignonieae (Bignoniaceae).

## Table of contents

* [General settings](#general-settings)
* [NDM/VNDM outputs nomenclature](#ndmvndm-outputs-nomenclature)
* [Usage](#usage)
* [System specifications](#system-specifications)
* [Credits](#credits)

System specifications

## General settings

This R project is organized according to the following tasks:

1) Producing NDM/VNDM input files
2) Importing NDM/VNDM outputs into R
3) Exploration of consensus areas
4) Drawing maps

Scripts are organized in separate files as follows:

1) Functions: Source files with the functions design for different tasks.
2) Workflow: General pipeline of the analysis.
3) Additional scripts detailing the exploration of the consensus areas.

This R project is organized in the following system of directories:

      Working_directory/
      data/           -> Data files
      docs/           -> Working manuscripts and markdown files
      output/         -> Outputs of the analysis
      figs/           -> Figures
      R/              -> R Scripts

The content from ./figs and ./output can be completely reproduced from scripts in ./R.

## NDM/VNDM outputs nomenclature

Output files, figures, and consensus areas in were saved using the following nomenclature.This nomenclature were designed to trace the NDM/VDNM outputs to their respective analyses, spatial scale, and the geographical location of consensus areas. The nomenclature must be strictly followed to reproduce the analysis.

### Area name code: 
It considers the spatial scale of analysis or grid size, the NDM code of the consensus area, relative geographical location, and type of GIS file (i.e grids or points) produced using VNDM-NDM save options. 

Example: __1dgD_CA0_AF-grid__ stands for:

    1dg = grid size; 
    D = Default analysis; 
    CA0 = Consensus area number 0;
    AF = A name for the geographical region where the area is located.
    grid/point = ndm grid output file type.

General regular expression = "^.*grid.txt" 

## Usage

The pipeline of the analysis is described and can be executed from the `NDM_workflow.R` script. A [Wiki](https://github.com/jupanago/RCode_BignonieaeAoE/wiki) page is available in this repository to look easily at the results of this script.

The NDM/VNDM output files are saved in `./data`.

The following libraries are necessary to run the R project.

`install.packages(c("readxl", "tidyverse", "ggpubr", "gtable", "ggthemes", "ggalt", "viridis",  "raster", "rgdal", "sp", "sf", rmapshaper", "UpSetR", "VennDiagram", "grid", "gridExtra","patchwork", "ggrepel", "lwgeom"))`

The Bignonieae Database (Dr. Lohmann's unpublished data) has the columns `NAME1`, `XCOOR`, and `YCOOR` for the species name, longitude, and latitude, repectively. All the functions used in this project assumed that the data frame used has these columns.

## System specifications

The Scripts in .R/ were prepeared under the following system specifications:

R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
Copyright (C) 2020 The R Foundation for Statistical Computing
Platform: x86_64-pc-linux-gnu (64-bit)

RStudio
Version 1.3.1093
© 2009-2020 RStudio, PBC
"Apricot Nasturtium" (aee44535, 2020-09-17) for Ubuntu Bionic Mozilla/5.0 (X11; Linux x86_64) 

Packages versions uptadated until September 2020.

OS Name: Ubuntu 20.04.1 LTS
OS Type: 64-bit
GNOME Version: 3.36.3
Windowing System: X11

Hardware specifications:

Memory: 11.6 GiB
Processor: Intel® Core™ i7-4720HQ CPU @ 2.60GHz × 8 
Graphics: NV117 / Intel® HD Graphics 4600 (HSW GT2)
Disk Capacity: 1.0 TB

## Credits

[Andrea Sanchez-Tapia](https://github.com/AndreaSanchezTapia/) provided guidance on how to develop an R project.


