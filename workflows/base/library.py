def base_concatenate_lists(input_dict):
    lists = input_dict['lists']
    new_list = []
    for every_list in lists:
        new_list = new_list+every_list
    output_dict = {}
    output_dict['list']=new_list
    return output_dict

def stopwatch(input_dict):
    import time
    inputTime = input_dict['time_in']
    now = time.time()
    if (isinstance(inputTime, float)):
        elapsedTime = now - inputTime
    else:
        elapsedTime = None
    output_dict = {}
    output_dict['signal_out'] = input_dict['signal']
    output_dict['time_out'] = now
    output_dict['time_span'] = elapsedTime

    return output_dict