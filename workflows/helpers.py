import os

def ensure_dir(f):
    d = os.path.dirname(f)
    if not os.path.exists(d):
        os.makedirs(d)
        
class UnpicklableObject:
    def __init__(self, init_string):
        self.init_string = init_string
        self.imports = []
        
    def addimport(self,import_string):
        self.imports.append(import_string)
    
    def generate(self):
        for i in self.imports:
            exec(i)
        return eval(self.init_string)

    def __unicode__(self):
        return self.init_string
    
    def __str__(self):
        return self.init_string
        
    def __repr__(self):
        return self.init_string