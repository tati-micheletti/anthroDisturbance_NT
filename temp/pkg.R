remove.packages("SpaDES.project")
remove.packages("SpaDES.project", lib = "/home/tmichele/.local/share/R/anthroDisturbance_NT/packages/x86_64-pc-linux-gnu/4.3/")
devtools::install_github(repo = "PredictiveEcology/SpaDES.project", 
                         ref = "transition", 
                         type = "source")
devtools::install_github(repo = "PredictiveEcology/SpaDES.project", 
                         ref = "transition", 
                         lib = "/home/tmichele/.local/share/R/anthroDisturbance_NT/packages/x86_64-pc-linux-gnu/4.3/", 
                         type = "source", force = TRUE)


remove.packages("reproducible")
remove.packages("reproducible", lib = "/home/tmichele/.local/share/R/anthroDisturbance_NT/packages/x86_64-pc-linux-gnu/4.3/")
devtools::install_github(repo = "PredictiveEcology/reproducible", 
                         ref = "b6b6490",
                         lib = "/home/tmichele/.local/share/R/anthroDisturbance_NT/packages/x86_64-pc-linux-gnu/4.3/", 
                         type = "source", force = TRUE)


remove.packages("SpaDES.core")
remove.packages("SpaDES.core", lib = "/home/tmichele/.local/share/R/anthroDisturbance_NT/packages/x86_64-pc-linux-gnu/4.3/")
devtools::install_github(repo = "PredictiveEcology/SpaDES.core", 
                         ref = "development", 
                         lib = "/home/tmichele/.local/share/R/anthroDisturbance_NT/packages/x86_64-pc-linux-gnu/4.3/", 
                         type = "source", force = TRUE)

devtools::install_github("LandSciTech/caribouMetrics", 
                         lib = "/home/tmichele/.local/share/R/anthroDisturbance_NT/packages/x86_64-pc-linux-gnu/4.3/", 
                         type = "source", force = TRUE)

devtools::install_github("pik-piam/rmndt", 
                         lib = "/home/tmichele/.local/share/R/anthroDisturbance_NT/packages/x86_64-pc-linux-gnu/4.3/", 
                         type = "source", force = TRUE)
# install.packages("terra", type = "source", lib = "/home/tmichele/.local/share/R/anthroDisturbance_NT/packages/x86_64-pc-linux-gnu/4.3/")
# install.packages("cpp11", type = "source", lib = "/home/tmichele/.local/share/R/anthroDisturbance_NT/packages/x86_64-pc-linux-gnu/4.3/")
# install.packages("igraph", type = "source", lib = "/home/tmichele/.local/share/R/anthroDisturbance_NT/packages/x86_64-pc-linux-gnu/4.3/")
# install.packages("s2", type = "source", lib = "/home/tmichele/.local/share/R/anthroDisturbance_NT/packages/x86_64-pc-linux-gnu/4.3/")
# install.packages("sp", type = "source", lib = "/home/tmichele/.local/share/R/anthroDisturbance_NT/packages/x86_64-pc-linux-gnu/4.3/")
# install.packages("unitsare", type = "source", lib = "/home/tmichele/.local/share/R/anthroDisturbance_NT/packages/x86_64-pc-linux-gnu/4.3/")
# install.packages("sf", type = "source", lib = "/home/tmichele/.local/share/R/anthroDisturbance_NT/packages/x86_64-pc-linux-gnu/4.3/")
install.packages("devtools", type = "source", dependencies = TRUE)
install.packages("devtools", type = "source", lib = "/home/tmichele/.local/share/R/anthroDisturbance_NT/packages/x86_64-pc-linux-gnu/4.3/", dependencies = TRUE)


# DEVTOOLS
install.packages("miniUI", type = "source", lib = "/home/tmichele/.local/share/R/anthroDisturbance_NT/packages/x86_64-pc-linux-gnu/4.3/")
install.packages("shiny", type = "source", lib = "/home/tmichele/.local/share/R/anthroDisturbance_NT/packages/x86_64-pc-linux-gnu/4.3/")
install.packages("profvis", type = "source", lib = "/home/tmichele/.local/share/R/anthroDisturbance_NT/packages/x86_64-pc-linux-gnu/4.3/")
install.packages("sessioninfo", type = "source", lib = "/home/tmichele/.local/share/R/anthroDisturbance_NT/packages/x86_64-pc-linux-gnu/4.3/")
install.packages("urlchecker", type = "source", lib = "/home/tmichele/.local/share/R/anthroDisturbance_NT/packages/x86_64-pc-linux-gnu/4.3/")
install.packages("usethis", type = "source", lib = "/home/tmichele/.local/share/R/anthroDisturbance_NT/packages/x86_64-pc-linux-gnu/4.3/")
