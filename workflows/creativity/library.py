import tempfile
import os
from subprocess import CalledProcessError, check_output, Popen


def creativity_mapper(input_dict):
    domain1 = input_dict['domain1']
    domain2 = input_dict['domain2']
    file1 = tempfile.NamedTemporaryFile(suffix='.dt')
    file2 = tempfile.NamedTemporaryFile(suffix='.dt')
    file1.write(domain1)
    file2.write(domain2)
    file1.flush()
    file2.flush()
    dirname = os.path.join(os.path.dirname(os.path.abspath(__file__)),'mapper/')
    prolog_script = open(dirname+'run.pl').read()
    new_script = prolog_script.replace("{{domain1}}",file1.name).replace("{{domain2}}",file2.name)
    run_script_file = tempfile.NamedTemporaryFile(suffix='.pl',dir=dirname)
    run_script_file.write(new_script)
    run_script_file.flush()
    output_dict = {}

    print run_script_file.name
    #try:
    mapped = check_output(["yap","-L",run_script_file.name])
    #except CalledProcessError:
    #    raise Exception("Couldn't execute prolog script.")

    #p = SafePopen(['yap', '-s50000', '-h200000', '-L', Aleph.SCRIPT], cwd=self.tmpdir).safe_run()
    #stdout_str, stderr_str = p.communicate()
    #print stdout_str 
    #print stderr_str


    print mapped
    #print mapped

    output_dict['relations']=mapped

    file1.close()
    file2.close()
    run_script_file.close()
    return output_dict
