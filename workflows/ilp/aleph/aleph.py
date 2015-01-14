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
import json
from StringIO import StringIO
from stat import S_IREAD, S_IEXEC
from subprocess import PIPE

if __name__ != '__main__':
    from ..security import SafePopen
else:
    import os
    parent_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    os.sys.path.append(parent_dir)
    from security import SafePopen

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
    FEATURES_FN = 'features.pl'
    RULES_SUFFIX = 'Rules'
    FEATURES_SUFFIX = 'Features'
    PROP_DATASET_SUFFIX = 'Propositional'
    SCRIPT = 'run_aleph.pl'

    ESSENTIAL_PARAMS = {
        'depth': 10,
        'evalfn': 'coverage',
        'i': 2,
        'language': 'inf',
        'm': 0.0,
        'max_features': 'inf',
        'minpos': 1,
        'noise': 0
    }

    def __init__(self, verbosity=logging.NOTSET):
        """
        Creates an Aleph object.

        @param logging Can be DEBUG, INFO or NOTSET (default).
                       This controls the verbosity of the output.
        """
        self.tmpdir = tempfile.mkdtemp()
        self.aleph_script = '%s/%s' % (self.tmpdir, Aleph.ALEPH_FN)
        self.postGoal = None
        self.postScript = None
        # Dictionary of non-default settings
        self.settings = dict()
        logger.setLevel(verbosity)

        shutil.copy("%s/%s" % (Aleph.THIS_DIR, Aleph.ALEPH_FN), self.tmpdir)
        shutil.copy("%s/%s" % (Aleph.THIS_DIR, Aleph.FEATURES_FN), self.tmpdir)

    def set(self, name, value):
        """
        Sets the value of setting 'name' to 'value'.
        """
        self.settings[name] = value

    def settingsAsFacts(self, settings):
        """
        Parses a string of settings in the form:
            set(name1, val1), set(name2, val2)...
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
        Induce a theory or features in 'mode'.

        @param filestem The base name of this experiment.
        @param mode In which mode to induce rules/features.
        @param pos String of positive examples.
        @param neg String of negative examples.
        @param b String with background knowledge.

        @return The theory as a string or an arff dataset in induce_features mode.
        """
        # Write the inputs to appropriate files.
        self.__prepare(filestem, pos, neg, b)

        # Make a script to run aleph (with appropriate settings).
        self.__script(mode, filestem)

        logger.info("Running aleph...")

        # Run the aleph script.
        p = SafePopen(['yap', '-s50000', '-h200000', '-L', Aleph.SCRIPT], cwd=self.tmpdir).safe_run()
        stdout_str, stderr_str = p.communicate()
        logger.debug(stdout_str)
        logger.debug(stderr_str)

        logger.info("Done.")

        result = None
        if mode != 'induce_features':
            # Return the rules written in the output file.
            rules_fn = filestem + Aleph.RULES_SUFFIX
            result = open('%s/%s' % (self.tmpdir, rules_fn)).read()
        else:
            features_fn = filestem + Aleph.FEATURES_SUFFIX
            features = open('%s/%s' % (self.tmpdir, features_fn)).read()
            dataset_fn = filestem + Aleph.PROP_DATASET_SUFFIX
            pl_dataset = open('%s/%s' % (self.tmpdir, dataset_fn)).read()
            result = self.__to_arff(features, pl_dataset, filestem)


        # Cleanup.
        self.__cleanup()
        return result

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
            logger.info('Problem removing temporary files. \
                         The files are probably in use.')

    def __script(self, mode, filestem):
        """
        Makes the script file to be run by yap.
        """
        scriptPath = '%s/%s' % (self.tmpdir, Aleph.SCRIPT)
        script = open(scriptPath, 'w')

        # Permit the owner to execute and read this script
        os.chmod(scriptPath, S_IREAD | S_IEXEC)

        cat = lambda x: script.write(x + '\n')
        cat(":- initialization(run_aleph).")
        cat("run_aleph :- ")
        cat("consult(aleph),")
        cat("read_all('%s')," % filestem)
        # Cat all the non-default settings
        for setting, value in self.settings.items():
            cat("set(%s, %s)," % (setting, value))
        cat("%s," % mode)

        eof = ',' if self.postScript else '.'
        if mode == 'induce_features':
            cat("consult(features),")
            features_fn = filestem + Aleph.FEATURES_SUFFIX
            dataset_fn = filestem + Aleph.PROP_DATASET_SUFFIX
            cat('save_features(%s),' % features_fn)
            cat('save_dataset(%s)%s' % (dataset_fn, eof))
        else:
            rules_fn = filestem + Aleph.RULES_SUFFIX
            cat("write_rules('%s')%s" % (rules_fn, eof))
        if self.postScript:
            cat(self.postGoal + ".")
            cat(self.postScript)
        script.close()

    def __to_arff(self, features, pl_dataset, filestem):
        arff = StringIO()
        cat = lambda x: arff.write(x + '\n')

        cat('@RELATION "%s"' % filestem)
        features = re.findall(r"feature\((\d+),\((.*)\)\).", features)
        for fid, feature in sorted(features, key=lambda e: e[0]):
            cat('%% f%s: %s' % (fid, feature))
            cat('@ATTRIBUTE f%s {0,1}' % fid)

        # Class attribute
        class_id = len(features)
        cat('@ATTRIBUTE class {negative,positive}')

        cat('@DATA')
        examples = re.findall(r"example\((\w+),(\[[\d,]*\]),(\w+)\)\.", pl_dataset)
        for _, features, cls in examples:
            vals = []
            for i in range(0, class_id):
                vals.append('1' if i in json.loads(features) else '0')
	    vals.append(cls)
            cat('%s' % ','.join(vals))
        return arff.getvalue()


if __name__ == '__main__':
    aleph = Aleph()
    print aleph.induce('induce_features',
                        open('test/train.f').read(), 
                        open('test/train.n').read(), 
                        open('test/train.b').read(), 
                        filestem='trains_test')
