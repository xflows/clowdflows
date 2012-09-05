from mothra.local_settings import FILES_FOLDER

def safeOpen(filename):
    if filename.startswith(FILES_FOLDER):
        if filename.find("..")==-1:
            return open(filename,'r')
        else: 
            raise Exception("Invalid filename")
    else:
        raise Exception("Invalid filename.")