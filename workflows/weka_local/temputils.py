__author__ = 'vid'

from tempfile import gettempdir, mkdtemp
import os
import sys


# This class offers a temporary file tries to overcome the problems with NamedTemporary files
# on Windows. It *should* work both on Linux and Windows.
# When an instance of TempFile is deleted, underlying temporary file and directory are automatically
# removed, too.
# The only case when the del method needs to be called is when we manually raise an Exception.
class TemporaryFile(object):
    TF_PREFIX = 'tempfile'

    def __init__(self, flags='w+', suffix='.tmp', dir=None):
        pd = dir  if  (dir!=None and os.path.exists(dir))  else  gettempdir()
        self.directory = mkdtemp(dir=pd)
        self.name = os.path.normpath('%s/%s%s' % (self.directory, TemporaryFile.TF_PREFIX, suffix))
        try:
            self.fp = open(self.name, flags)
        except IOError as e:
            raise IOError('Temporary file %s cannot be opened (flags: "%s")\n' % (self.name, flags))
    # end

    def writeString(self, dataString, endWithNewline=False):
        self.fp.write(dataString)
        if endWithNewline:
            self.fp.write('\n')
        self.fp.flush()

    # clean temporary file and directory
    def __del__(self):
        self.fp.close()
        os.remove(self.name)
        remFiles = os.listdir(self.directory)
        if remFiles:
            sys.stderr.write('Removing bogus files in private temporary directory %s\n' % self.directory)
        for fn in remFiles:
            os.remove(os.path.join(self.directory, fn))
        os.rmdir(self.directory)
# end


class TemporaryDirectory(object):
    '''This class offers a temporary directory which is emptied and deleted automatically
    after use'''

    def __init__(self):
        self.name = mkdtemp()

    def clearContents(self):
        remFiles = os.listdir(self.name)
        for fn in remFiles:
            os.remove(os.path.join(self.name, fn))
    # end

    # clean all files and directory
    def __del__(self):
        try:
            self.clearContents()
        except OSError:
            sys.stderr.write('Directory does not exist anymore: %s\n' % self.name)
        else:
            os.rmdir(self.name)
    # end
# end


# This function creates a new directory
# If necessary, it creates all non-existing parent directories
#
def makeDir(dname):
    if os.path.exists(dname):
        return

    dlist = []
    currDname = dname
    while True:
        head, tail = os.path.split(currDname)
        if tail:
            dlist.append(tail)
            currDname = head
        else:
            if head: # if path was absolute
                dlist.append(head)
            break
    # end
    dlist.reverse()

    pth = ''
    for subDir in dlist:
        pth = os.path.join(pth, subDir)
        if not os.path.exists(pth):
            os.mkdir(pth)
# end
