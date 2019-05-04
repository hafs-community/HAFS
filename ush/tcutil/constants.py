"""!Constants used throughout the tcutil package

This module contains various constants, including the Earth
"radius" and unit conversions.  Unit conversion constants are given as
fractions.Fraction objects to allow the conversion to be exact."""

##@var __all__
# ensures that "from tcutil.constants import *" does nothing
__all__=['']

import fractions

# Metric/SI/nautical unit conversions: these conversions are exact.
# knots, nmi: http://www.nist.gov/pml/wmd/metric/length.cfm
# ft/inches:  http://physics.nist.gov/cuu/Units/outside.html

##@var ft2m
# """US foot to meters conversion (exact)."""
ft2m = fractions.Fraction(12*254,10000)
"""US foot to meters conversion (exact)."""

##@var m2ft
# Meters to US foot conversion (exact).
m2ft = 1/ft2m
"""Meters to US foot conversion (exact)."""

##@var nmi2km
# Nautical miles to kilometers conversion (exact).
nmi2km = fractions.Fraction(1852,1000)
"Nautical miles to kilometers conversion (exact)."""

##@var km2nmi
# Kilometers to nautical miles conversion (exact).
km2nmi = 1/nmi2km
"""Kilometers to nautical miles conversion (exact)."""

##@var kts2mps
# Knots to meters per second conversion (exact).
kts2mps = fractions.Fraction(1852,3600)
"""Knots to meters per second conversion (exact)."""

##@var mps2kts
# Meters per second to knots conversion (exact).
mps2kts = 1/kts2mps
"""Meters per second to knots conversion (exact)."""

# Various earth radii from the tcutilutil library constants_module:
##@var Rpole
# EGM2008 Earth radius at the pole.
Rpole      = 6356752.3142
"""EGM2008 Earth radius at the pole."""

##@var Requator
# EGM2008 Earth radius at the equator.
Requator   = 6378137.0000
"""EGM2008 Earth radius at the equator."""

##@var flattening
# EGM2008 Earth flattening ratio.
flattening = 1/298.257223563
"""EGM2008 Earth flattening ratio."""

##@var REmean
# Earth mean ellipsoid radius from IUGG 1980
REmean       = 6371009.0
"""Earth mean ellipsoid radius from IUGG 1980"""

##@var REauthalic
# Earth authalic (equal surface area) radius from IUGG 1980
REauthalic   = 6371007.2
"""Earth authalic (equal surface area) radius from IUGG 1980"""

##@var REvolume
# Earth equal volume radius from IUGG 1980
REvolume     = 6371000.8
"""Earth equal volume radius from IUGG 1980"""

##@var RErectifying
# Earth rectivying (equal circumference) radius from IUGG 1980
RErectifying = 6367449.1
"""Earth rectivying (equal circumference) radius from IUGG 1980"""

##@var Rearth
# A compromise: the average of the mean ellipsoid radius, authalic radius and equal volume radius
Rearth = (REmean+REauthalic+REvolume)/3
"""Average of the mean ellipsoid radius, authalic radius and equal
volume radius."""
