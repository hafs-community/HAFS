#! /usr/bin/env python3

import logging, os, sys, re, functools
import produtil.setup
import tcutil.numerics, tcutil.revital

def main(args):
    """!Set up logging, reads vitals, outputs storm list."""

    # Set up logging, disable dbn alerts:
    produtil.setup.setup(send_dbn=False,ologlevel=99,jloglevel=99,
                         eloglevel=logging.INFO)
    logger=logging.getLogger('tcutil_interpolate.py')
    file_pre=args[1]
    file_aft=args[2]
    option=int(args[3]) # 0 for interpolate, 1 for extrapolate

    lines_pre=list()
    caseid_pre=list()
    lines_aft=list()
    caseid_aft=list()
    with open(file_pre,'rt') as f:
      for line in f.readlines():
        temp = line.split()
        lines_pre.append(temp)
    with open(file_aft,'rt') as f:
      for line in f.readlines():
        temp = line.split()
        lines_aft.append(temp)

    for storms in lines_pre:
      caseid_pre.append(storms[1])
    for storms in lines_aft:
      caseid_aft.append(storms[1])
    count=0
    for sid_pre in caseid_pre:
      for sid_aft in caseid_aft:
        if sid_pre == sid_aft:
          count=count+1
    if count < 1:
      print("Error! No storm is not consistently existing with 6 hours")
      exit
    for sid_pre in range(len(caseid_pre)):
      for sid_aft in range(len(caseid_aft)):
        if caseid_pre[sid_pre] == caseid_aft[sid_aft]:
          item_pre=lines_pre[sid_pre][:]
          item_aft=lines_aft[sid_aft][:]
          fout=list()
          for sid in range(len(item_pre)):
            if item_pre[sid] == item_aft[sid]:
              fout.append(item_pre[sid])
            else:
              if sid == 5 or sid == 6:
                value_pre=item_pre[sid]
                value_aft=item_aft[sid]
                if sid == 5:
                  if option == 0:
                    value=(int(value_pre[:3])+int(value_aft[:3]))/2
                  else:
                    value=(int(value_aft[:3])-int(value_pre[:3]))/2+int(value_aft[:3])
                  fout.append(str('%3.3d'%int(value+0.5))+value_pre[3])
                else:
                  if option == 0:
                    value=(int(value_pre[:4])+int(value_aft[:4]))/2
                  else:
                    value=(int(value_aft[:4])-int(value_pre[:4]))/2+int(value_aft[:4])
                  fout.append(str('%4.4d'%int(value+0.5))+value_pre[4])
              else:
                if sid == 18:
                  if option == 0:
                    fout.append(item_pre[sid])
                  else:
                    fout.append(item_aft[sid])
                else:
                  if option == 0:
                    value=(int(item_pre[sid])+int(item_aft[sid]))/2
                  else:
                    value=(int(item_aft[sid])-int(item_pre[sid]))/2+int(item_aft[sid])
                  fout.append(str(int(value+0.5)))
          print("%-4s %3s %-9s %8s %4s %4s %5s %3.3d %3.3d %4.4d %4.4d %4.4d %2.2d %3.3d %4d %4d %4d %4d %s"%(fout[0],fout[1],fout[2],fout[3],fout[4],fout[5],fout[6],int(fout[7]),int(fout[8]),int(fout[9]),int(fout[10]),int(fout[11]),int(fout[12]),int(fout[13]),int(fout[14]),int(fout[15]),int(fout[16]),int(fout[17]),fout[18]))
    return fout

if __name__=='__main__': main(sys.argv)
