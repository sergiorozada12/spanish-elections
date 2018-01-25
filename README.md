# spanish-elections

**Objective:** The analysis intends to find what factors geographically explain the 2016 Spanish elections.

**Data:** The datasets were obtained from Kaggle and the National Spanish Institut of Statistics:
  - https://www.kaggle.com/mlprojectbth/spanish-region-and-election-results/data
  - http://www.ine.es/jaxiT3/Tabla.htm?t=3994
  - http://www.ine.es/dynt3/inebase/en/index.html?padre=1691&dh=1
  - http://www.arcgis.com/home/item.html?id=83d81d9336c745fd839465beab885ab7
  
**Approach:** The pipeline used to perform the analysis is described as follows:
  1) Wrangling: Data is cleaned, tidied and merged in order to get everything prepared for the analysis.
  2) Basic statistics: An initial approach to the data is performed by looking at histograms, distributions and visualizations of the data.
  3) Linear analysis: The first analytical step performed is a linear analysis to check whether some variables can linearly explain different geographical trends. If not, looking at the geographical distribution of residuals can show some regions that differ from the rest.
  4) Geographically weighted statistics: Local behaviour of correlations has been studied across the country. The interesting point here is to see in which parts of the country, political trends can be explained by socio-economic factors.
  5) Clustering: Regions are clustered together in order to look for similar structures in regions from different perspectives.
