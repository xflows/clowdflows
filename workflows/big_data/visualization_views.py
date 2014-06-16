from django.shortcuts import render

def big_data_display_summation(request,input_dict,output_dict,widget):
    if sum(input_dict['intList']) == input_dict['sum']:
        check = 'The calculation appears correct.'
    else:
        check = 'The calculation appears incorrect!'
    return render(request, 'visualizations/big_data_display_integers.html',{'widget':widget,'input_dict':input_dict, 'output_dict':output_dict, 'check':check})

def results_to_file(request,input_dict,output_dict,widget):
    from disco.core import Disco, result_iterator
    import os.path
    from mothra.settings import MEDIA_ROOT
    from workflows.helpers import ensure_dir

    ddfs = Disco().ddfs
    tag = input_dict["string"]
    destination = MEDIA_ROOT+'/'+str(request.user.id)+'/'+str(widget.id)+'.txt'
    ensure_dir(destination)
    
    if tag == None:
        return {"string":"Tag does not exist."}
    
    elif not os.path.isfile(destination): #file doesnt exists
        f = open(destination,'w')
        for line in result_iterator(ddfs.urls(tag)):
            f.writelines(str(line[0]) + " ".join(map(str,line[1])) + "\n")
    f.close()
    filename = str(request.user.id)+'/'+str(widget.id)+'.txt'
    output_dict['filename'] = filename
    return render(request, 'visualizations/string_to_file.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})
