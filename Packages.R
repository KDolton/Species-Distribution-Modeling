
#list of packages
packages<-c("tidyverse", 
            "rgbif", 
            "usethis", 
            "CoordinateCleaner", 
            "leaflet", 
            "mapview", 
            "webshot2",
            "sf",
            "rnaturalearth",
            "sp",
            "raster",
            "dismo",
            "terra",
            "ENMeval",
            "geodata", 
            "rJava", 
            "maps")  

# install packages not yet installed
installed_packages<-packages %in% rownames(installed.packages())
if(any(installed_packages==FALSE)){
  install.packages(packages[!installed_packages])
}

# Packages loading, with library function
invisible(lapply(packages, library, character.only=TRUE))
