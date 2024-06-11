# TODO

# NEXT RELEASE
# [ ] NEWS - have diffs ready
# [ ] Versioning Rmd and Module
# [ ] Date on Rmd
# [ ] Render PDF
 
# [ ] Implement usage of roads' map: stage set
# [ ] Description of the DEM input
# [ ] Run replicates (up to 27 would be generally ok!)
# 0.2 Scenarios
    # [ ] BAU 1 
    # [ ] BAU 2 
    # [ ] BAU 3
    # [ ] BAU 4
    # [ ] BAU 5
# 0.4 Scenario
    # [ ] 04 1 
    # [ ] 04 2 
    # [ ] 04 3
    # [ ] 04 4 
    # [ ] 04 5
# 0.6 Scenario
    # [ ] 06 1 
    # [ ] 06 2
    # [ ] 06 3
    # [ ] 06 4
    # [ ] 06 5

# [ ] Put all to run. 
# [ ] Cleanup scripts
# [ ] Push all
# [ ] Versioning + Log of changes 
#     - More realistic grid for seismic lines (usage with iSSA and RSF)
#     - implementation of quantiles for caribou
#     - implementation of beta distribution
#     - start of implementation of roads package
# [ ] Release



# ADD TO DOCUMENTATION!
 
# OPTION 1: The user passes nothing, just use ECCC data to "create DisturbanceRate" and update the table.
# OPTION 2: The user modifies the big table alone.
# OPTION 3: Pass DisturbanceRate to fix the big table --> Here the user specifies the percentage that they want, we just update the table. Advantage to quickly test different scenarios.
# OPTION 4: Pass only totalDisturbanceRate, then it uses ECCC data to calculate as DisturbanceRate is NULL. (if passing totalDisturbanceRate, doesn't know what they want!)
# OPTION 5: If the user passes both totalDisturbanceRate and DisturbanceRate, it stops.


# Debate using the layer created by James Hodson GNWT:
#       ECCC_2015_anthro_dist_corrected_to_NT1_2016_final.zip: 
#       https://drive.google.com/file/d/1sxAa0wwwt7iwiHD7zB0DDnjfqyIQjKI2/view?usp=drive_link)
# The problem is that according to James, the ECCC layer of 2015 is NOT correct. It misses several 
# disturbances. To get what they are using for policy, we need to use the same layer 


# Implemented an option of following "the current trend" in terms of proportion of 
# increasing disturbance. Check out ECCC 2010-2015 and get the proportion from there. Except 
# for seismic lines, which are declining (at least in the study area I have now). In that case,
# the disturbance can stay stable and shouldn't have a growth rate; DisturbanceRate = 0)
# This can be done using the default of DisturbanceRate.
# 
# When buffering individual disturbances, it is fine to use ESRI:102001
# However, when buffering all disturbances (i.e., much larger area), it might give a huge difference in the 
# number of pixels. For example:
# chosenDistrib = how many pixels together (each value is one iteration) until, when buffered (), they 
# achieve expectedDistPixels
# Browse[1]> chosenDistrib
# [1]  7  5  3  7  3  7  6  6  7  5  5  8 10  7  1  4  3  7  5  6  5  8  6  5  5  6  7  5  6  9  6  5  2  4  7  5  7  6  4  4  8  5
# Browse[1]> round(expectedDistPixels, 0)
# [1] 1287
# Browse[1]> round(nPixChosenTotal, 0)
# [1] 1299
# Here we see that the nPixChosenTotal is very close to expectedDistPixels. The buffered was done INDIVIDUALLY, on each cell cluster (i.e., 7 cells, then 5 cells, then 3 cells, then 7 cells, etc)
# Once we get to actually buffering all cells, is where we see the effects of the projection.
# First, buffering all chosen cells (i.e., after all have been placed in best potential areas), leads 
# to a warning in RGDAL when using `terra::buffer(newDistLayBuff, width = 500, background = -1)`
# Warning message:
# In x@cpp$buffer(width, background, opt) :
#   GDAL Message 1: Pixels not square, distances will be inaccurate.
# We can then see that this represents about 33% (smaller) difference than encountered when each individual 
# disturbance was buffered
### 0: Chosen total pixels
### 1: Chosen buffer pixels
# Browse[1]> table(newDistLayBuff2[]) # Layer in ESRI:102001 projection
# -1             0        1 
# 15753393      248     2137 
# Browse[1]> table(newDistLayBuff3[]) # Layer in lon/lat WGS 84 (EPSG:4326)
#  -1            1 
# 16961711      789 
# Moreover, reprojecting also makes the 1's (pixels to be buffered) disappear once the buffering 
# is done by GDAL.
# 