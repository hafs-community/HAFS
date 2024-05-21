#! /usr/bin/env python
################################################################################
# Script Name: exhafs_msg_check.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script checks storm messages and sends an alert email if encountering
#   an issue.
################################################################################
import os, sys, re, logging, glob

if 'USHhafs' in os.environ:
    sys.path.append(os.environ['USHhafs'])
elif 'HOMEhafs' in os.environ:
    sys.path.append(os.path.join(os.environ['HOMEhafs'],'ush'))
else:
    guess_HOMEhafs=os.path.dirname(os.path.dirname(
            os.path.realpath(__file__)))
    guess_USHhafs=os.path.join(guess_HOMEhafs,'ush')
    sys.path.append(guess_USHhafs)

import produtil.setup, produtil.log, produtil.dbnalert
from produtil.run import batchexe, run

def send_email(email_list,subject,email_from=None,
               content_text=None,content_file=None,
               logger=None):
    """!Send email function"""

    if content_file:
        cmd='mail.py -s '+ '"' + subject + '"' + ' -v "' + email_list + '" < content_file '
    else:
        assert(content_text is not None)
        with open('tempfile','wt') as f:
            f.write(content_text)
        cmd='mail.py -s '+ '"' + subject + '"' + ' -v "' + email_list + '" < tempfile '
    print('cmd')
    print(repr(cmd))
    status=os.system(cmd)
    return status

def read_alert_message(alert_file):
    """!Get email content from the alert file"""
    with open(alert_file,'rt') as f:
        content=f.read()
    return content

def main():
    logger=logging.getLogger('Sending_Alert_Email')
    email_from=os.environ.get('MAILFROM', 'nco.spa@noaa.gov')
    alert_email_list=os.environ.get('MAILTO','sdm@noaa.gov')
    messagedir=os.environ.get('COMINmsg')
    nhcdir=os.environ.get('COMINnhc')
    jtwcdir=os.environ.get('COMINjtwc')
    patrn=str(sys.argv[1])+" "+str(sys.argv[2])
    message=0
    for ff in glob.glob(os.path.join(messagedir,'message?')):
        content=read_alert_message(ff)
        print(ff + " is: " + content)
        if re.search(patrn, content):
            message=message+1
    ymd="20"+str(sys.argv[1])
    hh=str(sys.argv[2])+"Z"
    print("number of message files for " + patrn + "Z =", message)

    #NHC message
    nhc_message=0
    for ff in glob.glob(os.path.join(nhcdir,'storm?')):
        content=read_alert_message(ff)
        if re.search(patrn, content):
            nhc_message=nhc_message+1
    print("number of NHC requested storms=",nhc_message)

    #JTWC message
    jtwc_message=0
    for ff in glob.glob(os.path.join(jtwcdir,'storm?')):
        content=read_alert_message(ff)
        if re.search(patrn, content):
            jtwc_message=jtwc_message+1
    print("number of JTWC requested storms from message=",jtwc_message)

    if message == 0 and nhc_message+jtwc_message > 0:
        subject='FATAL ERROR: No message created for active storm for cycle: '+ymd+hh
        alert_message='Dear SDM: There is no message created from active storms yet on WCOSS2 in ' + messagedir + ' . Please Check if setup_hurricane has been properly executed'
        status=send_email(alert_email_list,subject,email_from=email_from,
                      content_text=alert_message,logger=logger)
        logger.error('FATAL ERROR: No message created for active storm in ',messagedir)
        sys.exit(2)

if __name__ == '__main__':
    main()

