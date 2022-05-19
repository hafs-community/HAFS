#!/usr/bin/env python3

import sys, getopt
from datetime import datetime, date, time, timedelta
import datetime

##cycle='2019082900'

cycle = sys.argv[1]

cycle_yr=int(cycle[0:4])
cycle_mo=int(cycle[4:6])
cycle_dy=int(cycle[6:8])
cycle_hr=int(cycle[8:10])

d1 = date(cycle_yr, cycle_mo, cycle_dy)

d_1900=date(1900, 12, 31)
delta2 = d1-d_1900

results=[]
results.append(delta2.days+cycle_hr/24.0)
results.append(delta2.days+cycle_hr/24.0+5.25)
results.append("false")
results.append("false")

print(results)

with open('limits', 'w') as f:
    for row in results:
        f.write("%s " % str(row))
