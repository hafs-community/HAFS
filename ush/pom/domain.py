#!/usr/bin/env python

"""
Written By Biju Thomas, GSO, University of Rhode Island on 26/3/15. This 
function selects sMPIPOM domain based on TCVital file(tmpvit) and storm Basin.

"""
__all__ = [ 'choose_domain' ]
from os.path import exists, getsize
import hwrf.launcher
import logging

domains = ["transatl", "eastpac", "westpac", "northind",
           "southind", "swpac", "sepac"]
domain_bd = {"transatl":{"lon_beg":261.5,"lon_end":344.7,"lat_beg":10.,"lat_end":47.5},
             "eastpac":{"lon_beg":192.5,"lon_end":275.7,"lat_beg":5.,"lat_end":42.5},
             "westpac":{"lon_beg":96.6,"lon_end":179.8,"lat_beg":5.,"lat_end":42.5}, 
             "northind":{"lon_beg":32.3,"lon_end":115.5,"lat_beg":2.5,"lat_end":40.}, 
             "southind":{"lon_beg":32.3,"lon_end":115.5,"lat_beg":-40.,"lat_end":-2.5}, 
             "swpac":{"lon_beg":96.6,"lon_end":179.8,"lat_beg":-40.,"lat_end":-2.5}, 
             "sepac":{"lon_beg":180.2,"lon_end":263.4,"lat_beg":-40.,"lat_end":-2.5}}
domain_d = {"L":"transatl", "E":"eastpac", "W":"westpac",
            "A":"northind", "S":"southind", "P":"swpac",
            "X":"sepac", "C":"eastpac", "O":"westpac",
            "B":"northind", "U":"swpac", "T":"westpac",
            "Q":"southatl"}
shbasid=["P","S"]
nhbasid=["L","E","C","W","A","B"]
def choose_domain(basid, conf, usefcst = True, logger=None):
    if logger is None: logger=logging.getLogger('pom')
    storminfo = conf.syndat
    lon = storminfo.lon if storminfo.lon > 0.0 else storminfo.lon + 360.0
    lat = storminfo.lat
    if storminfo.havefcstloc:
        fhr = storminfo.fhr
    else:
        fhr = -999
    if fhr > 0:
        flon = storminfo.flon if storminfo.flon > 0.0 else storminfo.flon + 360.0
        flat = storminfo.flat

    domain_id = "none"
    if fhr <= 0 or not usefcst :
        dlon = 0.0
        dlat = 0.0
#        if belongs(lon, lat, domain_bd["transatl"]["lon_beg"], 
#                                 domain_bd["transatl"]["lon_end"],
#                                 domain_bd["transatl"]["lat_beg"], 
#                                 domain_bd["transatl"]["lat_end"],dlon,dlat):
#            
#            domain_id = "transatl"
        if basid == "L" :
            domain_id = "transatl"
        elif belongs(lon, lat, 195.0, 
                                 275.0,
                                 domain_bd["eastpac"]["lat_beg"], 
                                 domain_bd["eastpac"]["lat_end"],dlon,dlat): 
            domain_id = "eastpac"
        elif belongs(lon, lat, 99.5,
                                 178.0,
                                 domain_bd["westpac"]["lat_beg"],
                                 domain_bd["westpac"]["lat_end"],dlon,dlat):
            domain_id = "westpac"
        elif belongs(lon, lat, domain_bd["northind"]["lon_beg"],
                                 99.5,
                                 domain_bd["northind"]["lat_beg"],
                                 domain_bd["northind"]["lat_end"],dlon,dlat):
            domain_id = "northind"
        elif belongs(lon, lat, domain_bd["southind"]["lon_beg"],
                                 99.5,
                                 domain_bd["southind"]["lat_beg"],
                                 domain_bd["southind"]["lat_end"],dlon,dlat):
            domain_id = "southind"
        elif belongs(lon, lat, domain_bd["swpac"]["lon_beg"],
                                 domain_bd["swpac"]["lon_end"],
                                 domain_bd["swpac"]["lat_beg"],
                                 domain_bd["swpac"]["lat_end"],dlon,dlat):
            domain_id = "swpac"
        elif belongs(lon, lat, domain_bd["sepac"]["lon_beg"],
                                 domain_bd["sepac"]["lon_end"],
                                 domain_bd["sepac"]["lat_beg"],
                                 domain_bd["sepac"]["lat_end"],dlon,dlat):
            domain_id = "sepac"
        else:
            msg='Domain Selection based on StormID'
            logger.warning(msg)
            domain_id = domain_d[basid]
    else:
        dlon = 0.0
        dlat = 0.0
#       if belongs(lon, lat, domain_bd["transatl"]["lon_beg"],
#                                domain_bd["transatl"]["lon_end"],
#                                domain_bd["transatl"]["lat_beg"],
#                                domain_bd["transatl"]["lat_end"],dlon,dlat) and  \
#                  belongs(flon, flat, domain_bd["transatl"]["lon_beg"],
#                                domain_bd["transatl"]["lon_end"],
#                                domain_bd["transatl"]["lat_beg"],
#                                domain_bd["transatl"]["lat_end"],dlon,dlat):
#           domain_id = "transatl"
        if basid == "L" :
            domain_id = "transatl"
        elif belongs(lon, lat, 195.0, 
                                 275.0,
                                 domain_bd["eastpac"]["lat_beg"], 
                                 domain_bd["eastpac"]["lat_end"],dlon,dlat) and  \
                   belongs(flon, flat, 195.0,
                                 275.0,
                                 domain_bd["eastpac"]["lat_beg"],
                                 domain_bd["eastpac"]["lat_end"],dlon,dlat):
            domain_id = "eastpac"
        elif belongs(lon, lat, 99.5,
                                 178.0,
                                 domain_bd["westpac"]["lat_beg"],
                                 domain_bd["westpac"]["lat_end"],dlon,dlat) and  \
                   belongs(flon, flat, 99.5,
                                 178.0,
                                 domain_bd["westpac"]["lat_beg"],
                                 domain_bd["westpac"]["lat_end"],dlon,dlat):
            domain_id = "westpac"
        elif belongs(lon, lat, domain_bd["northind"]["lon_beg"],
                                 99.5,
                                 domain_bd["northind"]["lat_beg"],
                                 domain_bd["northind"]["lat_end"],dlon,dlat) and \
                   belongs(flon, flat, domain_bd["northind"]["lon_beg"],
                                 99.5,
                                 domain_bd["northind"]["lat_beg"],
                                 domain_bd["northind"]["lat_end"],dlon,dlat):
            domain_id = "northind"
        elif belongs(lon, lat, domain_bd["southind"]["lon_beg"],
                                 99.5,
                                 domain_bd["southind"]["lat_beg"],
                                 domain_bd["southind"]["lat_end"],dlon,dlat) and \
                   belongs(flon, flat, domain_bd["southind"]["lon_beg"],
                                 99.5,
                                 domain_bd["southind"]["lat_beg"],
                                 domain_bd["southind"]["lat_end"],dlon,dlat):
            domain_id = "southind"
        elif belongs(lon, lat, domain_bd["swpac"]["lon_beg"],
                                 domain_bd["swpac"]["lon_end"],
                                 domain_bd["swpac"]["lat_beg"],
                                 domain_bd["swpac"]["lat_end"],dlon,dlat) and  \
                   belongs(flon, flat, domain_bd["swpac"]["lon_beg"],
                                 domain_bd["swpac"]["lon_end"],
                                 domain_bd["swpac"]["lat_beg"],
                                 domain_bd["swpac"]["lat_end"],dlon,dlat):
            domain_id = "swpac"
        elif belongs(lon, lat, domain_bd["sepac"]["lon_beg"],
                                 domain_bd["sepac"]["lon_end"],
                                 domain_bd["sepac"]["lat_beg"],
                                 domain_bd["sepac"]["lat_end"],dlon,dlat) and  \
                   belongs(flon, flat, domain_bd["sepac"]["lon_beg"],
                                 domain_bd["sepac"]["lon_end"],
                                 domain_bd["sepac"]["lat_beg"],
                                 domain_bd["sepac"]["lat_end"],dlon,dlat):
            domain_id = "sepac"
        else:
            msg='Domain Selection based on StormID'
            logger.warning(msg)
            domain_id = domain_d[basid] 
    if domain_id in domains:
        return domain_id
    else:
        return None 

def belongs(lon, lat, lon_beg, lon_end, lat_beg, lat_end, dlon, dlat, logger=None):
    if logger is None: logger=logging.getLogger('pom')
    if lon_beg >= lon_end or lat_beg >= lat_end:
        logger.error(" Incorrect domain boundaries  %s %s %s %s" %(lon_beg, lon_end, lat_beg, lat_end))
        return None
    if lon > lon_beg + dlon and lon < lon_end - dlon:
        if lat > lat_beg + dlat and lat < lat_end - dlat:
            return True
    return False   
