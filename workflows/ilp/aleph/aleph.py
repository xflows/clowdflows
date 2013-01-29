#
# Python interface to Aleph.
# 
# author: Anze Vavpetic <anze.vavpetic@ijs.si>, 2011
#
import os.path
import shutil
import logging
import re
import tempfile
from stat import S_IREAD, S_IEXEC
from subprocess import Popen, PIPE

DEBUG = True

# Setup a logger
logger = logging.getLogger("Aleph [Python]")
logger.setLevel(logging.DEBUG if DEBUG else logging.INFO)
ch = logging.StreamHandler()
formatter = logging.Formatter("%(name)s %(levelname)s: %(message)s")
ch.setFormatter(formatter)
logger.addHandler(ch)

class Aleph(object):
    # The aleph source file is presumed to be in the same dir as this file.
    THIS_DIR = os.path.dirname(__file__) if os.path.dirname(__file__) else '.'
    ALEPH_FN = 'aleph.pl'
    YAP = '/usr/local/bin/yap'
    RULES_SUFFIX = 'Rules'
    SCRIPT = 'run_aleph.pl'

    ESSENTIAL_PARAMS = {
        'depth' : 10,
        'evalfn' : 'coverage',
        'i' : 2,
        'language' : 'inf',
        'm' : 0.0,
        'max_features' : 'inf',
        'minpos' : 1,
        'noise' : 0
    }
    
    def __init__(self, verbosity=logging.NOTSET):
        """
        Creates an Aleph object.
        
        @param logging can be DEBUG, INFO or NOTSET (default). This controls the verbosity of the output.
        """
        self.tmpdir = tempfile.mkdtemp()
        self.aleph_script = '%s/%s' % (self.tmpdir, Aleph.ALEPH_FN)
        self.postGoal = None
        self.postScript = None
        # Dictionary of non-default settings
        self.settings = dict()
        logger.setLevel(verbosity)
        
        shutil.copy("%s/%s" % (Aleph.THIS_DIR, Aleph.ALEPH_FN), self.aleph_script)
        
    def set(self, name, value):
        """
        Sets the value of setting 'name' to 'value'.
        """
        self.settings[name] = value

    def settingsAsFacts(self, settings):
        """
        Parses a string of settings in the form set(name1, val1), set(name2, val2)...
        """
        pattern = re.compile('set\(([a-zA-Z0-9_]+),(\[a-zA-Z0-9_]+)\)')
        pairs = pattern.findall(settings)
        for name, val in pairs:
            self.set(name, val)

    def setPostScript(self, goal, script):
        """
        After learning call the given script using 'goal'.
        """
        self.postGoal = goal
        self.postScript = script
            
    def induce(self, mode, pos, neg, b, filestem='default'):
        """
        Induce a theory in 'mode'.
        
        @param filestem The base name of this experiment.
        @param mode In which mode to induce rules.
        @param pos String of positive examples.
        @param neg String of negative examples.
        @param b String with background knowledge.
        """
        # Write the inputs to appropriate files.
        self.__prepare(filestem, pos, neg, b)

        # Make a script to run aleph (with appropriate settings, stack/heap sizes, ...).
        self.__script(mode, filestem)

        logger.info("Running aleph...")

        # Run the aleph script.
        p = Popen(['./' + Aleph.SCRIPT], cwd=self.tmpdir, stdout=PIPE)
        stdout_str, stderr_str = p.communicate()
        
        logger.debug(stdout_str)
        logger.debug(stderr_str)
        
        logger.info("Done.")
        
        # Return the rules written in the output file.
        rules = open('%s/%s' % (self.tmpdir, filestem + Aleph.RULES_SUFFIX)).read()

        #shutil.copy('%s/%s.py' % (self.tmpdir, filestem), '/home/anzev/programiranje/sdm/results/')
        
        # Cleanup.
        self.__cleanup()
        
        return rules

    def __prepare(self, filestem, pos, neg, b):
        """
        Prepares the needed files.
        """
        posFile = open('%s/%s.f' % (self.tmpdir, filestem), 'w')
        negFile = open('%s/%s.n' % (self.tmpdir, filestem), 'w')
        bFile = open('%s/%s.b' % (self.tmpdir, filestem), 'w')

        posFile.write(pos)
        negFile.write(neg)
        bFile.write(b)
        
        posFile.close()
        negFile.close()
        bFile.close()
        
    def __cleanup(self):
        """
        Cleans up all the temporary files.
        """
        try:
            shutil.rmtree(self.tmpdir)
        except:
            logger.info('Problem removing temporary files. The files are probably in use.')

    def __script(self, mode, filestem):
        """
        Makes the script file to be run by yap.
        """
        scriptPath = '%s/%s' % (self.tmpdir, Aleph.SCRIPT)
        script = open(scriptPath, 'w')
        
        #print scriptPath
        
        # Permit the owner to execute and read this script
        os.chmod(scriptPath, S_IREAD | S_IEXEC)
        
        cat = lambda x: script.write(x + '\n')
        cat("#!%s -L -s50000 -h200000" % Aleph.YAP)
        cat(":- initialization(run_aleph).")
        cat("run_aleph :- ")
        cat("consult(aleph),")
        cat("read_all('%s')," % filestem)
        # Cat all the non-default settings
        for setting, value in self.settings.items():
            cat("set(%s, %s)," % (setting, value))
        cat("%s," % mode)
        cat("write_rules('%s')%s" % (filestem + Aleph.RULES_SUFFIX, ',' if self.postScript else '.'))
        if self.postScript:
            cat(self.postGoal + ".")
            cat(self.postScript)
        script.close()