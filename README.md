# Plugin RStudio
  
Refacto is in progress. Not working yet   

## Installation

* Current version (this repo):
From **R**, with a VPN to the Saagie datacenter:

SSH :

```R
# install.packages("devtools")
# install.packages("git2r")
myssh <- "ssh://git@gitlab.saagie.tech:42/service/rstudio-saagie-addin.git"
devtools::install_git(
  myssh,
  credentials = git2r::cred_ssh_key(
    publickey = "C:/Users/Charline/.ssh/id_rsa.pub",
    privatekey = "C:/Users/Charline/.ssh/id_rsa"
  ))
```

HTTPS :
```R
# install.packages("devtools")
# install.packages("git2r")
myhttps <- "https://gitlab.saagie.tech/service/rstudio-saagie-addin.git"
email <- "charline@creativedata.fr"
devtools::install_git(
  url = myhttps,
  credentials = git2r::cred_user_pass(email, .rs.askForPassword("Gitlab password"))
)
```

* Previous version (`packrat` archive available on [Google Drive](https://drive.google.com/drive/u/0/folders/0Bwhav1A1_fxXRGRuNU00S0ZJSEU)):  
Install **R** (version >= 3.2.5)  
Install **RStudio** (version RStudio-0.99.902)  
Add to packrat the file `"Saagie/Saagie.Rproj"`  
Run the source in `main.R` (in the folder `Saagie/R/main.R`)  

```R
source("R/library.R") 
source("R/view.R")
source("R/validator.R")
source("R/model.R")
```
Load the package with: `Ctrl`+`Shift`+`b` (within **RStudio**)  

## Usage

Within **RStudio**:  
Click on "Addins"  
Click "Add to platform Saagie"  

## Informations

The previous version of this package (`packrat` archive) has been tested on Linux and Windows.
