# PCA and SVD

## 1. Load Image ---------------------------------------------------------------------

if(!require(png)) install.packages("png", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")

library(png)
library(tidyverse)
library(ggrepel)

# Read image
img <- readPNG("figs/tiger.png")

# The object is an array of 3 dimensions (note that the 2 first elements are the pixels)
class(img)
dim(img)

## 2. Transform Image ---------------------------------------------------------------------

# We transform the image into a 2 dimensional matrix (pixels x * pixels y)
img <- matrix(img,dim(img)[1],dim(img)[2])
class(img)
dim(img)

# Here is a representation of the matrix we created of the image
image(img)

# To see as the original image, first we traspose the matrix to flip the image to the original orientation
img <- t(img)[,nrow(img):1]

?image
# Then We define black and white color to display the original colors 
pal <- colorRampPalette(c("black","white"))

# Let's try the first 10 colors in the palette
pal(10)

# Now we get our original image
image(img, col = pal(10))

# Note that the values in the matrix represent the intensity of the color
# Where rows = observations or samples and columns = variables or features
colnames(img) <- paste("var",1:dim(img)[2],sep="")
rownames(img) <- paste("obs",1:dim(img)[1],sep="")

# Here is an extract of our matrix
eye <- round(img,4)[75:100,75:94]
eye

# Which represents the left eye of the tiger
image(eye, col = pal(10))

## 3. PCA with prcomp() ---------------------------------------------------------------------

# Always center the data for PCA (the argument center by dafault is TRUE) and scale only if ranges of values are different for each variable.
pca <- prcomp(img, center = TRUE, scale = FALSE)

# The main elements of the prcomp are:

##... sdev --------------------------------------------------------------------

# sdev = standard deviation of each axis of our orthogonal linear transformation, ordered by importance (decreasing order).
# sdev^2 = eigenvalues of each axis or component, 
#        = sum of the squares distances between the projected points (to the axis) and the origin = ss(distances).
# The PC or axis is the line that best fit the points: the line with the largest ss(distances).

# The axes are the reference from where you take a "picture" of your data.
# The axes are either the number of variables or the number of observations, whichever is smaller.
pca$sdev^2

# So, our diagonal matrix of the variances is:
diag(pca$sdev^2)

# Which is the same as the diagonal of the covariance matrix of pca$x:
cov(pca$x)

##... x --------------------------------------------------------------------

# x = The orthogonal linear transformation of our original data into the axes or Principal Components.
# These are the coordinates of the observations on each PC.
# PCs are ordered by importance (as shown before: pca$sdev^2).
dim(pca$x) 
top_n <- 10
pca$x[1:top_n,1:top_n]

# Here is a graph of our observations from PC1 and PC2 point of view (the most important components).
# "Picture" of our observations viewed from PC1 and PC2.
pca_x_df <- data.frame(Observation=rownames(pca$x),
                     PC1 = pca$x[,1],
                     PC2 = pca$x[,2])

top_n(pca_x_df,10)

pca_x_df %>% ggplot(aes(x=PC1, y=PC2, label=Observation)) +
  geom_point(alpha=0.6,fill="#0072B2",colour="#0072B2") +
  geom_text_repel(box.padding = .05,size=3,min.segment.length = 0) +
  xlab(str_c("PC1 (", pca.var.per[1], "%)", sep="")) +
  ylab(str_c("PC2 (", pca.var.per[2], "%)", sep="")) +
  theme_bw() +
  ggtitle("Our Observations view from PC1 and PC2 (PCA)")

##... rotation --------------------------------------------------------------------

# rotation = matrix of eigenvectors, matrix of loading scores ("recepe" with the mix of the variables to prepare "PCs").
dim(pca$rotation)
top_n <- 10
pca$rotation[1:top_n,1:top_n]

# So the "top_n" variables that contribute the most to PC "n" are:
n <- 1
top_n <- 10
sort(abs(pca$rotation[,n]),decreasing = TRUE)[1:top_n]

# Here is a graph of our variables from PC1 and PC2 point of view (the most important components).
# "Picture" of our variables view from PC1 and PC2.
pca_rotation_df <- data.frame(Variable=rownames(pca$rotation),
                     PC1 = pca$rotation[,1],
                     PC2 = pca$rotation[,2])

top_n(pca_rotation_df,10)

pca_rotation_df %>% ggplot(aes(x=PC1, y=PC2, label=Variable)) +
  geom_point(alpha=0.6,fill="#0072B2",colour="#0072B2") +
  geom_text_repel(box.padding = .05,size=3,min.segment.length = 0) +
  xlab(str_c("PC1 (", pca.var.per[1], "%)", sep="")) +
  ylab(str_c("PC2 (", pca.var.per[2], "%)", sep="")) +
  theme_bw() +
  ggtitle("Our Variables view from PC1 and PC2 (PCA)")

##... Scree Plot -----------------------------------------------------------------------

# A scree plot is a graph of the variance accounted for each PC.

# Variance in %.
pca_var_per <- round(pca$sdev^2/sum(pca$sdev^2)*100, 2) 
pca_var_per

variance_df <- data.frame(pc = 1:length(pca_var_per), variation_perc = pca_var_per) %>%
  mutate(variation_accum = cumsum(variation_perc))

pareto <- 80
pc_pareto <- max(variance_df$pc[which(variance_df$variation_accum<=pareto)]) + 1
variance_accum_pareto <- variance_df$variation_accum[pc_pareto]

variance_df %>%
  ggplot(aes(x=pc)) +
  geom_bar(aes(y=variation_perc), stat="identity", alpha=0.5, fill="#0072B2", colour="#0072B2") +
  geom_point(aes(y=variation_accum), colour="#0072B2", size=.8) +
  ggtitle("Variance accounted for each PC (PCA)") +
  xlab("Principal Component") +
  ylab("Variance (%)") +
  theme_bw() +
  annotate(geom="text",
           x= pc_pareto + 1,
           y= pareto,
           hjust= 0,
           label= str_c(pc_pareto," PCs = ",round(variance_accum_pareto,2)," % of variance)",sep=""),
           size= 3)

##... Reduced Matrix with PCA -----------------------------------------------------------------------

# Our data reduced to "n" dimensions (in this case pc_pareto):
n <- pc_pareto

approx_img_pca <-  sweep(pca$x[,1:n,drop=FALSE],2,pca$sdev[1:n],"*") %*%
  t(pca$rotation[,1:n,drop=FALSE])

image(approx_img_pca, col = pal(20))

## 4. SVD ---------------------------------------------------------------------

# To compare with PCA, we center the data:
svd <- svd(scale(img,scale=FALSE,center=TRUE))

# The main elements of the SVD are:

##... d --------------------------------------------------------------------

# d = diagonal VECTOR with standard deviations of each axis or component, sorted decreasingly.
# NOTE that the diagonal matrix = diag(d).

# The elements of d are formed by taking the sum of the squares of the principal components but not dividing by the sample size.
# d^2 / (n - 1) = eigenvalues of each axis or component, 
#               = sum of the squares distances between the projected points (to the axis) and the origin = ss(distances).
# The PC or axis is the line that best fit the points: the line with the largest ss(distances).

# The axes are the reference from where you take a "picture" of your data.
# The axes are either the number of variables or the number of observations, whichever is smaller.
svd$d^2/(nrow(img) - 1) #equals to pca$sdev^2

# So, our diagonal matrix of the variances is:
diag(svd$d^2/(nrow(img) - 1))

# Which is the same as the diagonal of the covariance matrix of pca$x = t(t(svd$u) * svd$d):
cov(t(t(svd$u) * svd$d))

##... u --------------------------------------------------------------------

# u = matrix of eigenvalues compressed to the unit vector.
dim(svd$u)
top_n <- 10
svd$u[1:top_n,1:top_n]

# To decompress u we might do the following:
# These are the coordinates of the observations on each PC.
# PCs are ordered by importance (as shown before: svd$d^2/(nrow(img) - 1)).
t(t(svd$u) * svd$d) #equals to pca$x

(svd$u[,1:n,drop=FALSE] * svd$d[1:n])

t(t(svd$u) * svd$d)[1:top_n,1:top_n] # equals to pca$x[1:top_n,1:top_n]

identical(t(t(svd$u) * svd$d)[1:top_n,1:top_n], pca$x[1:top_n,1:top_n])

svd_x <- t(t(svd$u) * svd$d)
svd_x

# Here is a graph of our observations from PC1 and PC2 point of view (the most important components).
# "Picture" of our observations viewed from PC1 and PC2.
svd_x_df <- data.frame(Observation=seq.int(nrow(svd_x)),
                       PC1 = svd_x[,1],
                       PC2 = svd_x[,2])

top_n(svd_x_df,10) # equals to top_n(pca_x_df,10)

svd_x_df %>% ggplot(aes(x=PC1, y=PC2, label=Observation)) +
  geom_point(alpha=0.6,fill="#0072B2",colour="#0072B2") +
  geom_text_repel(box.padding = .05,size=3,min.segment.length = 0) +
  xlab(str_c("PC1 (", pca.var.per[1], "%)", sep="")) +
  ylab(str_c("PC2 (", pca.var.per[2], "%)", sep="")) +
  theme_bw() +
  ggtitle("Our Observations view from PC1 and PC2 (SVD)")

##... v --------------------------------------------------------------------

# v = matrix of eigenvectors, matrix of loading scores ("recepe" with the mix of the variables to prepare "PCs").
dim(svd$v)
top_n <- 10
svd$v[1:top_n,1:top_n]

# So the "top_n" variables that contribute the most to PC "n" are:
n <- 1
top_n <- 10
sort(abs(svd$v[,n]),decreasing = TRUE)[1:top_n]

# Here is a graph of our variables from PC1 and PC2 point of view (the most important components).
# "Picture" of our variables view from PC1 and PC2.
svd_v_df <- data.frame(Variable=seq.int(nrow(svd$v)),
                              PC1 = svd$v[,1],
                              PC2 = svd$v[,2])

top_n(svd_v_df,10) #equals to top_n(pca_rotation_df,10)

svd_v_df %>% ggplot(aes(x=PC1, y=PC2, label=Variable)) +
  geom_point(alpha=0.6,fill="#0072B2",colour="#0072B2") +
  geom_text_repel(box.padding = .05,size=3,min.segment.length = 0) +
  xlab(str_c("PC1 (", pca.var.per[1], "%)", sep="")) +
  ylab(str_c("PC2 (", pca.var.per[2], "%)", sep="")) +
  theme_bw() +
  ggtitle("Our Variables view from PC1 and PC2 (SVD)")

##... Scree Plot -----------------------------------------------------------------------

# A scree plot is a graph of the variance accounted for each PC.

# Variance in %.
variance <- svd$d^2/(nrow(img) - 1)
svd_var_per <- round(variance/sum(variance)*100, 2) 
svd_var_per #equals to pca_var_per

variance_df <- data.frame(pc = 1:length(svd_var_per), variation_perc = svd_var_per) %>%
  mutate(variation_accum = cumsum(variation_perc))

pareto <- 80
pc_pareto <- max(variance_df$pc[which(variance_df$variation_accum<=pareto)]) + 1
variance_accum_pareto <- variance_df$variation_accum[pc_pareto]

variance_df %>%
  ggplot(aes(x=pc)) +
  geom_bar(aes(y=variation_perc), stat="identity", alpha=0.5, fill="#0072B2", colour="#0072B2") +
  geom_point(aes(y=variation_accum), colour="#0072B2", size=.8) +
  ggtitle("Variance accounted for each PC (SVD)") +
  xlab("Principal Component") +
  ylab("Variance (%)") +
  theme_bw() +
  annotate(geom="text",
           x= pc_pareto + 1,
           y= pareto,
           hjust= 0,
           label= str_c(pc_pareto," PCs = ",round(variance_accum_pareto,2)," % of variance)",sep=""),
           size= 3)

##... Reduced Matrix with SVD -----------------------------------------------------------------------

# Our data reduced to "n" dimensions (in this case pc_pareto):
n <- pc_pareto
approx_img_svd <- t(t(svd$u[,1:n,drop=FALSE]) * svd$d[1:n]) %*% t(svd$v[,1:n,drop=FALSE])

# Or can be computed like this:
approx_img_svd_2 <- svd_x[,1:n,drop=FALSE] %*% t(svd$v[,1:n,drop=FALSE])

# Here are the 3 matrices to compare results:
# Note that with only 13 
image(approx_img_svd, col = pal(20))
image(approx_img_svd_2, col = pal(20))
image(approx_img_pca, col = pal(20))
image(img, col = pal(20))

# Eigen -------------------------------------------------------------------

eigen(cov(img))$values
eigen(cov(img))$vectors

