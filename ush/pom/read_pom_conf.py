
__all__ = [ 'read_pom_conf' ]

import string
import logging
from os.path import exists, getsize 

def read_pom_conf(fin, logger=None):
    if logger is None: logger=logging.getLogger('pom')
    if not exists(fin) or getsize(fin) == 0:
        logger.error("%s does not exists " %(fin))
        print("%s does not exists " %(fin))
        return {}
    with open(fin, 'rt') as fid:
        txt = fid.read()
    confd = {}
    plist = list(string.punctuation)
    excpt = ['[', ']','=','_','.']
    plist=list(filter(lambda x: x not in excpt, plist))
    for rline in txt.split("\n"):
        if rline:
           cline=''
           for x in rline:
               if x in plist:     break
               cline = ''.join([cline , x])
           if cline: 
               if '[' in cline and ']' in cline: 
                   sec = cline[cline.index('[')+1 : cline.index(']')].strip()
               elif '=' in cline: 
                   l=cline.split('=')
                   confd[''.join([ sec,'.',l[0].strip()])] = l[1].strip()
    return confd    
