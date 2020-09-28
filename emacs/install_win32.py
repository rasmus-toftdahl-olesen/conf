#!/usr/bin/env python

import os
from os import path
import sys
#import shutil

home = path.expanduser ( '~' )
appdata = os.getenv('APPDATA')
dotemacs = path.abspath(path.dirname(__file__))
filename = '.emacs'

print ( 'Home:', home )
print ( 'Application Data:', appdata )
print ( 'dotemacs:', dotemacs )

link = path.join(appdata, filename)
target = path.join(dotemacs, filename)

ret = os.system ( 'NET SESSION >nul 2>&1' )
if ret != 0:
    print ( 'This script needs to run as admin' )
    sys.exit(-1)

if os.system ( f'mklink {link} {target}' ) != 0:
    print ( f' - File probably already exists, please delete {link} and try again.' )

#shutil.copyfile ( path.join(dotemacs, filename), path.join(appdata, filename) )
