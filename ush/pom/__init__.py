##@namespace pom
# Runs the MPI Princeton Ocean Model for Tropical Cyclones (MPIPOMTC) ocean model initialization.
# 
# \anchor pom_overview
# This package contains classes and functions that know how to run the
# MPIPOMTC ocean model initialization.  It contains a test job in the
# pom.kick module that runs a simple test of the POM initialization.  
# The hwrf.mpipomtc module is a wrapper around this package that knows
# how to plug the POM initialization output into a coupled WRF+POM
# forecast.
#
#\note Please report bugs/questions/comments to bijuthomas(at)mail(dot)uri(dot)edu.
#\author Biju Thomas, GSO, University of Rhode Island.
#\date June 13, 2014
