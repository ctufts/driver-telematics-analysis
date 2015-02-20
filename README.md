# driver-telematics-analysis
This repository contains the files used in the analysis of the driver telematics analysis challenge.  

Files used for clustering:
kaggleSubmission1.R - kmeans trip/stop data - score: 0.51133
kaggleSubmission2.R - hclust trip/stop data - score: 0.52460
prediction\_histogram_heirarchal.R - hclust hist data - score:0.50924
prediction\_histogram_kmeans.R - kmeans hist data - score:0.48321


Files used for summarising and transforming:
distanceCharacteristics.R
distanceHistogram.R
stopCharacteristics.R

Summarised data:
driverHistogramDistance20150219.csv - histograms of distance between samples
driverStopStats20150219.csv - summary of number of stops, trip length, unique stops
driverTripStats20150219.csv - summary statistics of distances between samples

