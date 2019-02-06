# To install VisCello

## On the local system, once

dnf install libcurl libcurl-devel openssl-libs openssl-devel
sudo dnf install udunits2-devel
sudo dnf install libxml2-devel

To convert sample data:

pip install --user umap umap-learn # if you want to convert sample data
R -f data-raw/preprocess_GSE72857.R

## In R on the local system, from the directory inst/app:

install.packages("devtools")
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("monocle", update = F, ask=F)
BiocManager::install("clusterProfiler", update = F, ask=F)
BiocManager::install("org.Mm.eg.db", update = F, ask=F)
BiocManager::install("org.Hs.eg.db", update = F, ask=F)
options(repos=BiocManager::repositories())
BiocManager::install("limma")
devtools::install_github("https://github.com/qinzhu/VisCello.base.git", auth_token=<token>, ref=<github branc?h>)

## To deploy on shinyapps.io:

### Do this once:

Set up your rsconnect parameters:
rsconnect::setAccountInfo(name="<ACCOUNT>", token="<TOKEN>", secret="<SECRET>")


If VisCello is still in a private repo, first connect to shinyapps.io, go to the Account/Profile tab, and authorize shinyapps.io to github
devtools::install_github("rstudio/rsconnect", ref='737cd48')

### Any time you want to redeploy (must specify appName=<something> arg to deployApp the first time)
options(repos=BiocManager::repositories())
rsconnect::deployApp()