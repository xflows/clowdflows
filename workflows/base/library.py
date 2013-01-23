def base_concatenate_lists(input_dict):
    lists = input_dict['lists']
    new_list = []
    for every_list in lists:
        new_list = new_list+every_list
    output_dict = {}
    output_dict['list']=new_list
    return output_dict