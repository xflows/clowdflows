# Python interface to RSD.
# 
# author: Anze Vavpetic <anze.vavpetic@ijs.si>, 2012
#
import os.path
import shutil
import logging
import re
import tempfile
from stat import S_IREAD, S_IEXEC
from subprocess import PIPE

try:
    from ..security import SafePopen
except:
    import os
    parent_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    os.sys.path.append(parent_dir)
    from security import SafePopen

DEBUG = True

# Setup a logger
logger = logging.getLogger("RSD [Python]")
logger.setLevel(logging.DEBUG if DEBUG else logging.INFO)
ch = logging.StreamHandler()
formatter = logging.Formatter("%(name)s %(levelname)s: %(message)s")
ch.setFormatter(formatter)
logger.addHandler(ch)

class RSD(object):
    THIS_DIR = os.path.dirname(__file__) if os.path.dirname(__file__) else '.'
    RSD_FILES = ['featurize.pl', 'process.pl', 'rules.pl']

    # Generated scripts filenames
    CONSTRUCT = '_construct.pl'
    SAVE = '_save.pl'
    SUBGROUPS = '_subgroups.pl'

    SCRIPTS = [CONSTRUCT, SAVE, SUBGROUPS]

    ESSENTIAL_PARAMS = {
        'clauselength' : 8,
        'depth' : 4,
        'negation' : 'none',
        'min_coverage' : 1,
        'filtering' : 'true'
    }

    def __init__(self, verbosity=logging.NOTSET):
        """
        Creates an RSD object.
        """
        self.tmpdir = tempfile.mkdtemp()
        self.settings = dict()
        logger.setLevel(verbosity)

        # Copy needed files to tmp dir
        for fn in RSD.RSD_FILES:
            shutil.copy("%s/%s" % (RSD.THIS_DIR, fn), self.tmpdir)

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

    def induce(self, b, filestem='default', examples=None, pos=None, neg=None, cn2sd=True):
        """
        Generate features and find subgroups.
        
        @param filestem The base name of this experiment.
        @param examples Classified examples; can be used instead of separate pos / neg files below.
        @param pos String of positive examples.
        @param neg String of negative examples.
        @param b String with background knowledge.
        @param cn2sd Find subgroups after feature construction?

        Returns a tuple (features, weka, rules), where:
            - features is a set of prolog clauses of generated features,
            - weka is the propositional form of the input data,
            - rules is a set of generated cn2sd subgroup descriptions; 
              this will be an empty string if cn2sd is set to False.
        """
        # Write the inputs
        self.__prepare(filestem, b, examples=examples, pos=pos, neg=neg)

        # Write scripts
        self.__scripts(filestem)

        # Run the script
        logger.info("Running RSD...")
        try:
            for script in RSD.SCRIPTS:
                # Skip subgroup discovery part?
                if script == RSD.SUBGROUPS and not cn2sd:
                    continue
                p = SafePopen(['yap', '-s50000', '-h200000', '-L', script], cwd=self.tmpdir, stdout=PIPE).safe_run()
                stdout_str, stderr_str = p.communicate()
                logger.debug(stdout_str)
                logger.debug(stderr_str)
            logger.info("Done.")

            # Return the rules written in the output file.
            features = open('%s/%s' % (self.tmpdir, filestem + '_frs.pl')).read()
            weka = open('%s/%s' % (self.tmpdir, filestem + '.arff')).read()
            rules = open('%s/%s' % (self.tmpdir, filestem + '.rules')).read() if cn2sd else ''

            self.__cleanup()
            return (features, weka, rules)
        except OSError:
            raise RuntimeError("Yap compiler could not be loaded! (see http://www.dcc.fc.up.pt/~vsc/Yap/).")

    def __prepare(self, filestem, b, examples=None, pos=None, neg=None):
        """
        Prepares the needed files.
        """
        if examples:
            examplesFile = open('%s/%s.pl' % (self.tmpdir, filestem), 'w')
            examplesFile.write(examples)
            examplesFile.close()
        elif pos and neg:
            posFile = open('%s/%s.f' % (self.tmpdir, filestem), 'w')
            negFile = open('%s/%s.n' % (self.tmpdir, filestem), 'w')
            posFile.write(pos)
            negFile.write(neg)
            posFile.close()
            negFile.close()
        else:
            raise Exception('You need to provide either a single file of classified examples or \
                two files, positive and negative examples.')
        bFile = open('%s/%s.b' % (self.tmpdir, filestem), 'w')
        # Write settings.
        for setting, val in self.settings.items():
            bFile.write(':- set(%s,%s).\n' % (setting, val))
        bFile.write(b)
        bFile.close()

    def __cleanup(self):
        """
        Cleans up all the temporary files.
        """
        try:
            shutil.rmtree(self.tmpdir)
        except:
            logger.info('Problem removing temporary files. The files are probably in use.')

    def __scripts(self, filestem):
        """
        Generates the required scripts.
        """
        script_construct = open('%s/%s' % (self.tmpdir, RSD.CONSTRUCT), 'w')
        script_save = open('%s/%s' % (self.tmpdir, RSD.SAVE), 'w')
        script_subgroups = open('%s/%s' % (self.tmpdir, RSD.SUBGROUPS), 'w')

        # Permit the owner to execute and read this script
        for fn in RSD.SCRIPTS:
            os.chmod('%s/%s' % (self.tmpdir, fn), S_IREAD | S_IEXEC)

        # Writes one line of script
        new_script = lambda script: lambda x: script.write(x + '\n')

        #
        # 'Construction' script
        #
        w = new_script(script_construct)
        w(':- initialization(main).')
        w('main :-')
        w('[featurize],')
        w('r(%s),' % filestem)
        w('w.')
        script_construct.close()

        #
        # 'Saving' script
        #
        w = new_script(script_save)
        w(':- initialization(main).')
        w('main :-')
        w('[process],')
        w('r(%s),' % filestem)
        w('w,')
        w('w(weka, %s),' % filestem)
        w('w(rsd, %s).' % filestem)
        script_save.close()

        #
        # 'Subgroups' script
        #
        w = new_script(script_subgroups)
        w(':- initialization(main).')
        w('main :-')
        w('[rules],')
        w('r(%s),' % filestem)
        w('i,')
        w('w.')
        script_subgroups.close()

if __name__ == '__main__':
    examples = open('example/trains.pl').read()
    b = open('example/trains.b').read()
    rsd = RSD()
    features, weka, rules = rsd.induce(b, examples=examples)
    print features
    print weka
    print rules