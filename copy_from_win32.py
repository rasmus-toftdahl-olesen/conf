#!/usr/bin/env python

import os
from os import path
import shutil

appdata = os.getenv('APPDATA')
dotemacs = path.dirname(__file__)
filename = '.emacs'

print 'Application Data:', appdata
print 'dotemacs:', dotemacs

shutil.copyfile ( path.join(appdata, filename),  path.join(dotemacs, filename) )
