#!/usr/bin/env python

import os
from os import path
import shutil

home = path.expanduser ( '~' )
appdata = os.getenv('APPDATA')
dotemacs = path.dirname(__file__)
filename = '.emacs'

print 'Home:', home
print 'Application Data:', appdata
print 'dotemacs:', dotemacs

shutil.copyfile ( path.join(dotemacs, filename), path.join(appdata, filename) )
