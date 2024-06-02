# Relational Reprojection Platform

While quantitative geography has long acknowledged that non-Cartesian spaces and distances are often more appropriate for analyzing and visualizing real-world data and complex spatial phenomena, commonly available GIS software solutions make working with non-linear distances extremely difficult. Current prevailing GIS logics force specific kinds of data standards not relevant to many exploratory spatial analyses. 

Our **Relational Reprojection Platform (RRP)** fills this gap with a simple stereographic projection engine centering any given data point to the rest of the set, and transforming great circle distances from this point to the other locations using a variety of non-linear functions as options with particular use cases in mind. This method of re-projecting data allows users to quickly and easily explore spatial relationships within complex spatial data sets. 

Our initial release is a [Shiny app](https://github.com/willbpayne/relational_reprojection_platform/blob/master/app/RRP_app_v3.R) that allows users to upload simple CSV files with geographic coordinates and data columns and minimal cleaning and explore a variety of spatial transformations of their data. We hope this heuristic tool will enhance the exploratory stages of social research using spatial data.

![RRP](docs/relationalreprojection.jpeg)
