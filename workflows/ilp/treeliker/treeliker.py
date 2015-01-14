import shutil
import tempfile
import os.path
from subprocess import Popen, PIPE


class TreeLiker:
    def __init__(self, dataset, template):
        self.basename = 'default'
        self.dataset = dataset
        self.template = template
        self.tmpdir = tempfile.mkdtemp()

        with open('%s/%s.txt' % (self.tmpdir, self.basename), 'w') as f:
            f.write(dataset)

        # Copy binaries to tmp folder
        cdir = os.path.dirname(os.path.abspath(__file__))
        shutil.copytree('%s/bin/' % cdir, '%s/bin/' % self.tmpdir)

    def run(self, settings={}):
        '''
        Runs TreeLiker with the given settings.
        '''
        self._batch(settings)

        p = Popen(['java', '-Xmx1G', '-cp', 'bin/TreeLiker.jar', 
                   'ida.ilp.treeLiker.TreeLikerMain', '-batch', self.batch], 
                   cwd=self.tmpdir)
        stdout_str, stderr_str = p.communicate()
        arff = open('%s/%s.arff' % (self.tmpdir, self.basename)).read()

        self._cleanup()

        return arff

    def _batch(self, settings):
        '''
        Creates the batch file to run the experiment.
        '''
        self.batch = '%s/%s.treeliker' % (self.tmpdir, self.basename)

        commands = []
        commands.append('set(output_type, single)')
        commands.append("set(examples, '%s.txt')" % self.basename)
        commands.append('set(template, %s)' % self.template)
        commands.append('set(output, %s.arff)' % self.basename)

        # Optional settings
        for key, val in settings.items():
            if val not in [None, '']:
                commands.append('set(%s, %s)' % (key, str(val)))

        commands.append('work(yes)')

        with open(self.batch, 'w') as f:
            f.write('\n'.join(commands))

    def _cleanup(self):
        """
        Cleans up all the temporary files.
        """
        try:
            shutil.rmtree(self.tmpdir)
        except:
            logger.info('Problem removing temporary files. \
                         The files are probably in use.')
