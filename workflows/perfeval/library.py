def benchmark(input_dict):
    import time
    in_att = input_dict.get('in_att', None)
    start_time= input_dict.get('start_time', None)
    time_diff=(time.time()-start_time) if start_time else time.time()
    return {'out_att': in_att, 'time_diff': time_diff}
