__all__ = [ 'read_rtofs_infile', 'add_afile_infile'  ]

import string, re
import logging, datetime
from os.path import exists, getsize
import pom.exceptions

hycom_epoch=datetime.datetime(1900,12,31,0,0,0)

def read_rtofs_infile(fin, logger=None):
    if logger is None: logger=logging.getLogger('pom')
    logger.info('read_rtofs_infile(%s)'%(repr(fin),))
    if not exists(fin) or getsize(fin) == 0:
        logger.error("%s does not exists " %(fin))
        print("%s does not exists " %(fin))
        return {}
    dims = {}       
    with open(fin, 'rt') as fid:
        txt = fid.read()
    excpt = ['=']
    plist = list(string.punctuation)
    plist=list(filter(lambda x: x not in excpt, plist))
    for rline in txt.split("\n"):
        if rline:
            cline=''
            for x in rline:
                if x == '=':    break
                if x not in plist: 
                    cline = ''.join([cline , x])
            if 'idm' in cline.split() and not 'idmp' in cline.split():
                dims['idm'] = cline.split()[0]
            if 'jdm' in cline.split() and not 'jdmp' in cline.split():
                dims['jdm'] = cline.split()[0]
            if 'kdm' in cline.split():
                dims['kdm'] = cline.split()[0]
            if 'kz' in cline.split():
                dims['kz'] = cline.split()[0]
    return dims 

def date_hycom2normal(hycom):
    if isinstance(hycom,basestring):
        hycom=float(hycom)
    if isinstance(hycom,int):
        hycom=datetime.timedelta(hours=hycom*24)
    elif isinstance(hycom,float):
        hycom=datetime.timedelta(hours=hycom*24)
    elif isinstance(hycom,fractions.Fraction):
        hycom=datetime.timedelta(hours=float(hycom*24))
    return hycom_epoch+hycom

def add_afile_infile(fin, afile, bfile, fout, logger=None):
    if logger is None: logger=logging.getLogger('pom')
    logger.info('add_afile_to_infile(%s, %s, %s)'%(
                repr(fin),repr(afile),repr(fout)))
    if not exists(fin) or getsize(fin) == 0:
        logger.error("%s does not exists " %(fin))
        return None
    with open(fin, 'rt') as fid:
        line1=fid.readline()
        txt=fid.read()

    with open(fout, 'wt') as fid:
        fid.write('%s\n%s'%(afile,line1))
        fid.write(txt)

    if not exists(fout) or getsize(fout) == 0:
        logger.error("%s does not exists " %(fout))

def get_rtofs_kdm(bfile, logger=None):
    if logger is None: logger=logging.getLogger('pom')
    logger.info('get_rtofs_kdm(%s)'%(repr(bfile)))
    if not exists(bfile) or getsize(bfile) == 0:
        logger.error("%s does not exists " %(bfile))
        return None
    with open(bfile, 'rt') as fid:
        btxt=fid.read()
    blines=btxt.split("\n")
    kdm=0
    for rline in blines:
        if rline:
            if 'temp  ' in rline and '=' in rline:
                kdm=kdm+1
    return kdm

def prep_rtofs_infile(fin, afile, bfile, fout, logger=None):
    if logger is None: logger=logging.getLogger('pom')
    logger.info('prep_rtofs_infile(%s, %s, %s)'%(
                repr(fin),repr(afile),repr(fout)))
    if not exists(fin) or getsize(fin) == 0:
        logger.error("%s does not exists " %(fin))
        return None
    with open(fin, 'rt') as fid:
        txt=fid.read()

    with open(bfile, 'rt') as fid:
        btxt=fid.read()
    blines=btxt.split("\n")
    kdm=0
    for rline in blines:
        if rline:
            if 'iexpt' in rline and '=' in rline:
                liexpt = rline
            if 'yrflag' in rline and '=' in rline:
                lyrflag = rline
            if 'idm   ' in rline and '=' in rline:
                lidm = rline
            if 'jdm   ' in rline and '=' in rline:
                ljdm = rline
            if 'temp  ' in rline and '=' in rline:
                kdm=kdm+1
    lkdm="%4d    'kdm   ' = number of layers"%(kdm)

    with open(fout, 'wt') as fid:
        fid.write('%s\n'%(afile))
        fid.write('%s\n'%('BIN3D'))
        fid.write('%s\n'%(liexpt))
        fid.write('%s\n'%(lyrflag))
        fid.write('%s\n'%(lidm))
        fid.write('%s\n'%(ljdm))
        fid.write('%s\n'%(lkdm))
        fid.write(txt)

    if not exists(fout) or getsize(fout) == 0:
        logger.error("%s does not exists " %(fout))
