###############################
# Author: John Brandon [jbrandon@greeneridge.com | ..@lgl.com | ..gmail.com]
# Date:   17 Nov. 2012
# Machine/OS/Rversion: MacBook Pro Late 2011 / Mac OS 10.6.8 / R64 bit ver. 2.14.1
# Purpose : Calculate perpendicular distance from sightings in aerial images
# Notes: 
# 1) Working in units pixels
# 2) Adopting Bill Koski's model and naming conventions, i.e. altitude denoted "AC", outer edge to centerline "FC" etc. (see his drawing)
# 3) Drawing here: https://dl.dropbox.com/u/31861309/PhotoSizeOnWater.jpg
# 4) Tannis Thomas has been sending JB *.xls files, and these need to be saved as *.csv
# 5) 
# 6) 
# TO DO: 
#   i) Add some initial checking of the data to be sure column names and certain values (i.e. none missing) meet expectations 
#  ii) Make main double loop towards the end into a function call (need to list arguments), which returns 'dist.tmp'
###############################
# Notes on merging branch with master through Git
# Step 1. From your project repository, bring in changes (branch) and test
# $ git fetch origin
# $ git checkout -b Calculator-Edits origin/Calculator-Edits
# $ git merge master
# Step 2. Merge the changes (merge branch with master) and update on GitHub
# $ git checkout master
# $ git merge --no-ff Calculator-Edits
# $ git push origin master
###############################
# Set working directory -- NOTE: other users will need to modify this path according to their machine's directory  
###############################
rm(list=ls())
#file.dir = "/Users/johnbrandon/Documents/2012 Work/Aerial Camera Systems/Image Perpendicular Distance Calculator"
file.dir = "/Users/johnbrandon/DropBox/Aerial 2012 Shared Field DropBox/John Brandon/Image Perpendicular Distance Calculator"
file.dir = setwd(dir=file.dir); file.dir; dir()
getwd(); dir()
# Calculator-Edit Branch
# Test a change in the branch
# Test another change in branch
###############################
# read in image data file
###############################
#dat.file.name = "Burger_Sighting_Review_Database_08Nov2012.csv"
#dat.file.name = "Lateral.Distances.csv"
#dat.file.name = "Lateral Distances_2.csv"
#dat.file.name = "Lateral Distances_3.csv"
#dat.file.name = "Lateral Distances_4.csv"
dat.file.name = "Lateral Distances_5.csv"

perp.dist.dat.file = read.csv(file=dat.file.name, header=TRUE, na.strings = c("NA","X"))

###############################
# It would be a good idea to develop some code here which checks data file for expected column names, missing data etc.
#  This code should print out warnings when column names in perp.dist.dat.file do not match expectations
#  Also check for missing data, e.g. "Camera.Angle" expected to have a value in every record where there is a value for "x.coor"
#   Same for "Alt"itude
###############################

names(perp.dist.dat.file)
Camera.Angle
Alt
x.coor


###############################
# Initialize Variables
###############################
cam.horizontal.fov = 81
cam.vertical.fov = 54
cam.horizontal.pixels = 4912
cam.horizontal.pixels = 7360
pixels.per.degree.horizontal = cam.horizontal.pixels / cam.horizontal.fov
radians = function(deg){  # function to convert from degrees to radians
  rad = deg * pi / 180
  return(rad)
}
ft.to.m = 0.3048
###############################
# Work with data 
###############################
detach(perp.dist.dat.file)
attach(perp.dist.dat.file) ; names(perp.dist.dat.file)
nrow(perp.dist.dat.file)

pixels = x.coor; pixels  # need to have check above that there is a column named "pixels"
Altitude = Alt  # same as above re: "Alt"

angle.to.near.edge = 0.5 * cam.horizontal.fov - Camera.Angle # e.g. if Cam Angle = 25 deg, this wil be 15.5 deg (angle made by CAD in drawing)
angle.to.far.edge = cam.horizontal.fov - angle.to.near.edge # e.g. if Cam Angle = 25 deg, this wil be 65.5 deg (angle made by FAC in drawing)

calculate.BC = function(ac, cam.angle){
  return(ac * tan(radians(cam.angle)))
}  

calculate.CD = function(ac, cam.angle.near){
  return(ac * tan(radians(cam.angle.near)))
}
AC = Altitude * ft.to.m ; AC # convert altitude in ft to m
# AB = AC / cos(radians(Camera.Angle)) ; AB # refer to drawing
# BC = AC * tan(radians(Camera.Angle)) ; BC # refer to drawing
# AF = AC / cos(radians(angle.to.far.edge)) ; AF # refer to drawing
# FC = AC * tan(radians(angle.to.far.edge)) ; FC # refer to drawing
# AD = AC / cos(radians(angle.to.near.edge)) ; AD # refer to drawing
# CD = AC * tan(radians(angle.to.near.edge)) ; CD # refer to drawing

angle.to.near.edge[846] # was NA originally, debugging file 5. Should be 15.5 after inserting missing camera angle

# how many pixels between near / far edge and center line
near.pixel.count = pixels.per.degree.horizontal * angle.to.near.edge ; near.pixel.count
far.pixel.count = cam.horizontal.pixels - near.pixel.count ; far.pixel.count
  
# is the number of pixels greater than or less than number of pixels corresponding with angle to near edge
which(x.coor == 0)
which(is.na(x.coor)); which(is.na(pixels)) # should match
is.sighting.outside.center = rep(FALSE, length(pixels))
ii = 0
for(ii in 1:length(pixels)){
  if(is.na(pixels[ii])){
    print(paste(ii, " : NA")) # debugging
    is.sighting.outside.center[ii] = NA
  }else{
    print(ii) # debugging
    print(paste("near.pixel.count:", near.pixel.count[ii])) # debugging
    print(paste("pixels:", pixels[ii])) # debugging
    if(near.pixel.count[ii] > pixels[ii]) is.sighting.outside.center[ii] = FALSE else is.sighting.outside.center[ii] = TRUE  
  }
}

# if number of pixels is less than, then sighting is under the aircraft on near side of center line
# if number of pixels is greater than, then sighting is on far side of center line
pixels

# make this into a function call (need to list arguments), which returns 'dist.tmp'
x1 = rep(0, length(pixels))
deg.tmp = rep(0, length(pixels))
dist.tmp = rep(0, length(pixels))
for(ii in 1:length(pixels)){
  if(is.na(pixels[ii])){
    dist.tmp[ii] = NA
    next # skip records with no number for x.coor of sighting from edge
  } 
  deg.tmp[ii] = pixels[ii] / pixels.per.degree.horizontal # total 'angle' from near edge
  if(is.sighting.outside.center[ii] == FALSE){ # sighting inside center line
      x1[ii] = angle.to.near.edge[ii] - deg.tmp[ii]  # given number of pixels per degree, convert number of pixels into degrees from center line
      dist.tmp[ii] = -1* calculate.CD(AC[ii],x1[ii]) # given degrees from center line, calculate distance from center line given altitude (AC) - either BC or DC
     }else{                                      # sighting outside center line
      x1[ii] = deg.tmp[ii] - angle.to.near.edge[ii]  # given number of pixels per degree, convert number of pixels into degrees from center line
      dist.tmp[ii] = calculate.BC(AC[ii], x1[ii])   # given degrees from center line, calculate distance from center line given altitude (AC) - either BC or DC
    }
}
x1; deg.tmp; dist.tmp
which(is.na(dist.tmp))
###############################
# Output distance measurements by writing to .CSV data file
###############################
detach(perp.dist.dat.file)
perp.dist.dat.file$distance = dist.tmp
write.csv(perp.dist.dat.file, file = dat.file.name, na="X")
system(paste("open ", dat.file.name)) # MacOS specific system command / can change to a system command for DOS / Windows
?write.csv
getwd()

###############################
# Scratch code checking intermediate calculations
###############################
# 304.8 / cos(radians(25))
# AC = Altitude * ft.to.m ; AC # convert altitude in ft to m
# AB = AC / cos(radians(Camera.Angle)) ; AB # refer to drawing
# BC = AC * tan(radians(Camera.Angle)) ; BC # refer to drawing
# AF = AC / cos(radians(angle.to.far.edge)) ; AF # refer to drawing
# FC = AC * tan(radians(angle.to.far.edge)) ; FC # refer to drawing
# AD = AC / cos(radians(angle.to.near.edge)) ; AD # refer to drawing
# CD = AC * tan(radians(angle.to.near.edge)) ; CD # refer to drawing
# 
# ii = 0; xx = 0; 
# 
# for(ii in 1:length(AC)){
#   cam.angle = seq(0, angle.to.far.edge[ii])
#   xx = calculate.BC(AC[ii], cam.angle)
#   
# }
# xx
# length(xx)
# 
# seq(from = 0, to = 65.5, by = 0.5)
# 
# ang = seq(from = 0, to = 15.5, by = 0.5)
# dis = calculate.CD(AC[1], ang)
# dis
# 
# # Summing up distances for each pixel should equal CD + FC = total swadth width in meters
# pix.dist = rep(-99, cam.horizontal.pixels); pix.dist # vector with distance across each pixel from near edge to far edge of image
# pixels.per.degree.horizontal # 7360
# a = cam.horizontal.pixels * 15.5 / 81
# b = cam.horizontal.pixels * 65.5 / 81
# a + b
# 
# 15.5 / 81
# 
# CD + FC
# 
# length(cam.angle)
# length(AC)
# 
# ac
# angle.to.near.edge
# 
# angle.to.near.edge + angle.to.far.edge
# 
# detach(dat.file)
