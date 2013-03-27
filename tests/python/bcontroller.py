#!/usr/bin/env python
#
# ***** BEGIN LICENSE BLOCK *****
# Version: MPL 1.1/GPL 2.0/LGPL 2.1
#
# The contents of this file are subject to the Mozilla Public License Version
# 1.1 (the "License"); you may not use this file except in compliance with
# the License. You may obtain a copy of the License at
# http://www.mozilla.org/MPL/
#
# Software distributed under the License is distributed on an "AS IS" basis,
# WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
# for the specific language governing rights and limitations under the
# License.
#
# The Original Code is standalone Firefox Windows performance test.
#
# The Initial Developer of the Original Code is Google Inc.
# Portions created by the Initial Developer are Copyright (C) 2006
# the Initial Developer. All Rights Reserved.
#
# Contributor(s):
#   Alice Nodelman <anodelman@mozilla.com> (original author)
#
# Alternatively, the contents of this file may be used under the terms of
# either the GNU General Public License Version 2 or later (the "GPL"), or
# the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
# in which case the provisions of the GPL or the LGPL are applicable instead
# of those above. If you wish to allow use of your version of this file only
# under the terms of either the GPL or the LGPL, and not to allow others to
# use your version of this file under the terms of the MPL, indicate your
# decision by deleting the provisions above and replace them with the notice
# and other provisions required by the GPL or the LGPL. If you do not delete
# the provisions above, a recipient may use your version of this file under
# the terms of any one of the MPL, the GPL or the LGPL.
#
# ***** END LICENSE BLOCK *****

__author__ = 'anodelman@mozilla.com (Alice Nodelman)'

import os
import time
import subprocess
import threading
import platform
from ffprocess_linux import LinuxProcess
from ffprocess_mac import MacProcess
from ffprocess_win32 import Win32Process
from utils import talosError
import sys
import getopt

import stat


if platform.system() == "Linux":
    platform_type = 'linux_'
    ffprocess = LinuxProcess()
elif platform.system() in ("Windows", "Microsoft"):
    import win32pdh
    import win32api
    import win32event
    import win32con
    platform_type = 'win_'
    ffprocess = Win32Process()
elif platform.system() == "Darwin":
    platform_type = 'mac_'
    ffprocess = MacProcess()

class BrowserWaiter(threading.Thread):

  def __init__(self, command, log, mod, deviceManager = None):
     self.command = command
     self.log = log
     self.mod = mod
     self.endTime = -1
     self.returncode = -1
     self.deviceManager = deviceManager
     threading.Thread.__init__(self)
     self.start()

  def run(self):
    if self.mod:
      if (self.deviceManager.__class__.__name__ == "WinmoProcess"):
        if (self.mod == "str(int(time.time()*1000))"):
          self.command += self.deviceManager.getCurrentTime()
      else:
        self.command = self.command + eval(self.mod)

    if (self.deviceManager.__class__.__name__ == "WinmoProcess"):
      retVal = self.deviceManager.launchProcess(self.command, timeout=600)
      if retVal <> None:
        self.deviceManager.getFile(retVal, self.log)
        self.returncode = 0
      else:
        self.returncode = 1
    else:    #blocking call to system
      self.returncode = os.system(self.command + " > " + self.log) 

    self.endTime = int(time.time()*1000)

  def hasTime(self):
    return self.endTime > -1

  def getTime(self):
    return self.endTime

  def getReturn(self):
    def innerMethod(self):
      pass
    return self.returncode

  def testMethod2(self):
    class InnerClass:
      def innerInnerMethod(self):
        pass
    return

  class Test:
    def testMethod(self):
      pass

class BrowserController:

  def __init__(self, command, mod, name, child_process, 
               timeout, log, host='', port=20701, root=''):
    global ffprocess
    self.command = command
    self.mod = mod
    self.process_name = name
    self.child_process = child_process
    self.browser_wait = timeout
    self.log = log
    self.timeout = 1200 #no output from the browser in 20 minutes = failure
    self.host = host
    self.port = port
    self.root = root
    if (host <> ''):
      from ffprocess_winmo import WinmoProcess
      platform_type = 'win_'
      ffprocess = WinmoProcess(host, port, root)

    self.ffprocess = ffprocess

  def run(self):
    self.bwaiter = BrowserWaiter(self.command, self.log, self.mod, self.ffprocess)
    noise = 0
    prev_size = 0
    while not self.bwaiter.hasTime():
      if noise > self.timeout: # check for frozen browser
        try:
          ffprocess.cleanupProcesses(self.process_name, self.child_process, self.browser_wait)
        except talosError, te:
          os.abort() #kill myself off because something horrible has happened
        os.chmod(self.log, 0777)
        results_file = open(self.log, "a")
        results_file.write("\n__FAILbrowser frozen__FAIL\n")
        results_file.close()
        return
      time.sleep(1)
      try:
        open(self.log, "r").close() #HACK FOR WINDOWS: refresh the file information
        size = os.path.getsize(self.log)
      except:
        size = 0

      if size > prev_size:
        prev_size = size
        noise = 0
      else:
        noise += 1

    results_file = open(self.log, "a")
    if self.bwaiter.getReturn() != 0:  #the browser shutdown, but not cleanly
      results_file.write("\n__FAILbrowser non-zero return code (%d)__FAIL\n" % self.bwaiter.getReturn())
      return
    results_file.write("__startSecondTimestamp%d__endSecondTimestamp\n" % self.bwaiter.getTime())
    results_file.close()
    return


def main(argv=None):

   command = ""
   name = "firefox" #default
   child_process = "plugin-container" #default
   timeout = ""
   log = ""
   mod = ""
   host = ""
   deviceRoot = ""
   port = 20701

   if argv is None:
        argv = sys.argv
   opts, args = getopt.getopt(argv[1:], "c:t:n:p:l:m:h:r:o", ["command=", "timeout=", "name=", "child_process=", "log=", "mod=", "host=", "deviceRoot=", "port="])

   # option processing
   for option, value in opts:
     if option in ("-c", "--command"):
       command = value
     if option in ("-t", "--timeout"):
       timeout = int(value)
     if option in ("-n", "--name"):
       name = value
     if option in ("-p", "--child_process"):
       child_process = value
     if option in ("-l", "--log"):
       log = value
     if option in ("-m", "--mod"):
       mod = value
     if option in ("-h", "--host"):
       host = value
     if option in ("-r", "--deviceRoot"):
       deviceRoot = value
     if option in ("-o", "--port"):
       port = value

   if command and timeout and log:
     bcontroller = BrowserController(command, mod, name, child_process, timeout, log, host, port, deviceRoot)
     bcontroller.run()
   else:
     print "\nFAIL: no command\n"
     sys.stdout.flush()

   class mainClass:
     def mainClassMethod(self):
       pass
     pass

   def mainMethod(self):
     class mainMethodClass:
       pass
     pass


if __name__ == "__main__":
    sys.exit(main())
