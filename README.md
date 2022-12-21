# movielens
Harvardx Data Science Project

This project contains three main files:

1). 01_movilens.R = This code can be run from start to end without need of any additional information. All the code is commented and if needed, all objects can be found on the rda folder or in the GitHub repository. On step "VI.III.II PCA" we will perform PCA in the matrix with our residual ratings for the best movies and users, the code takes several minutes for this process (at least 60 minutes), so if your computer crashes at some point with this code, you can download manually the "pca_edx_residuals.rda" object from the folder "rda" in this repository.

2). movielens_final.Rmd = This rmd file needs the project donwloaded from GitHub in order to run properly. The instructions are explained in the first chunk "setup".

3). movielens_final.pdf = This is the pdf knitted with R markdown (.Rmd file). 

Additionally, there is a R script "02_pca_svd_explained.R" where PCA and SVD are illustrated with an example. If you are a beginner as I am, maybe this could help you to better understand these tools.

First, you must run the 01_movielens.R script. While running it, you can read the corresponding sections in the PDF file (these sections are indicated in the PDF).

Then you can try the .Rmd file. The Rmd file needs some objects to be previously created with the R script.
