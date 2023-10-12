#! /usr/bin/env python3

import sys
from math import pi, sin, tan, cos, pow, atan

def cot(angle): return 1/tan(angle)
def acot(tanv): return atan(1/tanv)

cen_lon = float(sys.argv[2]) * pi/180.
cen_lat = float(sys.argv[3]) * pi/180.
nx = int(sys.argv[4])
ny = int(sys.argv[5])
dx = float(sys.argv[6])
dy = float(sys.argv[7])

Re = 6.3712e+6

x_corner = -dx*nx/2.
y_corner = -dy*ny/2.

n = sin(cen_lat)
a = pi/4 + cen_lat/2
F = cos(cen_lat)*pow(tan(a), n)/n

# print("r_earth ",Re)
# print("1/4 c_er", 2*Re*pi/4)
# print("1/8 c_er", 2*Re*pi/8)
# print("x_corner",x_corner)
# print("y_corner",y_corner)
# print("n",n)
# print("a",a)
# print("F",F)

rho_center = Re*F*pow(cot(a), n)
# print("rho_center",rho_center)

theta_corner = acot((rho_center-y_corner)/x_corner)
lon_corner = theta_corner/n + cen_lon
# print("lon_corner","radians",lon_corner,"degrees",lon_corner*180/pi)

rho_corner = x_corner/sin(theta_corner)
# print("rho_corner",rho_corner)

a_corner = acot(pow(rho_corner/(Re*F),1.0/n))
lat_corner = a_corner*2 - pi/2
# print("lat_corner","radians",lat_corner,"degrees",lat_corner*180/pi)

if sys.argv[1] == "lon_corner":
    print(lon_corner*180/pi)
elif sys.argv[1] == "lat_corner":
    print(lat_corner*180/pi)
else:
    print('INVALID: EXPECTED lon_corner OR lat_corner')
