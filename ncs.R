#####################################################
# code to simulate emissions factors, GHG flux data #
# and/or peatland extent for NCS estimates          #
#####################################################
# Notes from MN Peatland meeting
# error accuracy on a pixel by pixel basis
# GHG inventory
# uncertainty
# IPCC emissions factors estimates and uncertainty
# geospatial error- errors of comittance and ommission
# geospatial error is the hardest to handle...

# Started 3/7/2024 by Ailene

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory. Add in your own path in an if statement for your file structure
if (length(grep("ailene", getwd()))>0) 
{setwd("C:/Users/ailene.ettinger/Documents/GitHub/grephon/analyses")
}# else if

# load packages

# simulate emissions factors

# mitigation=flux*extent (flux=emission factor/emission reduction)

# simulate flux data

# simulate extent data (pixels)

# code for standard (non-Bayesian) approach to quantifying mitigation (could incorporate error for emissions factor, using IPCC approach

# code for Bayesian approach to quantifying mitigation (incorporate error from extent and emissions factor)




