from django.shortcuts import render


def measure_distribution(request, input_dict, output_dict, widget):
    from disco.core import result_iterator
    from discomll.utils import distribution

    results = distribution.measure(input_dict["dataset"])

    dist = ""
    for k, v in result_iterator(results):
        dist += str(k.split("/")[-1][:-2]) + " " + str(v) + "\n"

    input_dict["string"] = dist
    return render(request, 'visualizations/display_string.html',
                  {'widget': widget, 'input_dict': input_dict, 'output_dict': output_dict})


def bigdata_ca(request, input_dict, output_dict, widget):
    from discomll.utils import accuracy
    import os.path
    from mothra.settings import MEDIA_ROOT
    from workflows.helpers import ensure_dir

    folder = 'discomll_measures'
    tag = input_dict["predictions"]
    destination = MEDIA_ROOT + '/' + folder + "/" + tag[0][6:] + '.txt'
    ensure_dir(destination)

    if input_dict["dataset"].params["id_index"] == -1:
        input_dict["string"] = "ID index should be defined."
    elif not os.path.isfile(destination):  # file doesnt exists
        measure, acc = accuracy.measure(test_data=input_dict["dataset"],
                                   predictions=input_dict["predictions"],
                                   measure="ca")
        string = "Classification Accuracy \n"
        score = str(measure) + " " + str(acc) + "\n"
        string += score
        input_dict["string"] = string

        f = open(destination, 'w')
        f.write(score)
        f.close()

    else:
        #ca results are cached
        string = "Classification Accuracy \n"
        f = open(destination, 'r')
        input_dict["string"] = string + str(f.readlines()[0])
        f.close()

    return render(request, 'visualizations/display_string.html',
                  {'widget': widget, 'input_dict': input_dict, 'output_dict': output_dict})


def bigdata_mse(request, input_dict, output_dict, widget):
    from discomll.utils import accuracy
    import os.path
    from mothra.settings import MEDIA_ROOT
    from workflows.helpers import ensure_dir

    folder = 'discomll_measures'
    tag = input_dict["predictions"]
    destination = MEDIA_ROOT + '/' + folder + "/" + tag[0][6:] + '.txt'
    ensure_dir(destination)

    if input_dict["dataset"].params["id_index"] == -1:
        input_dict["string"] = "ID index should be defined."
    elif not os.path.isfile(destination):  # file doesnt exists
        measure, acc = accuracy.measure(test_data=input_dict["dataset"],
                                   predictions=input_dict["predictions"],
                                   measure="mse")
        string = "Mean squared error\n"
        score = str(measure) + " " + str(acc) + "\n"
        string += score
        input_dict["string"] = string

        f = open(destination, 'w')
        f.write(score)
        f.close()

    else:
        string = "Mean squared error\n"
        f = open(destination, 'r')
        input_dict["string"] = string + str(f.readlines()[0])
        f.close()

    return render(request, 'visualizations/display_string.html',
                  {'widget': widget, 'input_dict': input_dict, 'output_dict': output_dict})


def results_to_file(request, input_dict, output_dict, widget):
    from disco.core import result_iterator
    import os.path
    from mothra.settings import MEDIA_ROOT
    from workflows.helpers import ensure_dir

    tag = input_dict["string"]
    folder = 'discomll_results'
    add = "add" if input_dict["add_params"] == "true" else ""

    destination = MEDIA_ROOT + '/' + folder + "/" + tag[0][6:] + add + '.txt'

    ensure_dir(destination)

    if not os.path.isfile(destination):  # file doesnt exists

        f = open(destination, 'w')
        if input_dict["add_params"] == "true":
            for k, v in result_iterator(tag):
                f.writelines(str(k) + " " + str(v) + "\n")
        else:
            for k, v in result_iterator(tag):
                f.writelines(str(k) + " " + str(v[0]) + "\n")
        f.close()
    filename = folder + "/" + tag[0][6:] + add + '.txt'

    output_dict['filename'] = filename
    return render(request, 'visualizations/string_to_file.html',
                  {'widget': widget, 'input_dict': input_dict, 'output_dict': output_dict})


def model_view(request, input_dict, output_dict, widget):
    from discomll.utils import model_view
    import os.path
    from mothra.settings import MEDIA_ROOT
    from workflows.helpers import ensure_dir

    folder = 'discomll_models'
    tag_name = input_dict["fitmodel_url"]
    tag = input_dict["fitmodel_url"].values()[0]

    destination = MEDIA_ROOT + '/' + folder + "/" + tag[0][6:] + '.txt'
    ensure_dir(destination)

    if not os.path.isfile(destination):  # file doesnt exists
        model = model_view.output_model(tag_name)
        f = open(destination, 'w')
        f.write(model)
        f.close()

    filename = folder + "/" + tag[0][6:] + '.txt'

    output_dict['filename'] = filename
    return render(request, 'visualizations/string_to_file.html',
                  {'widget': widget, 'input_dict': input_dict, 'output_dict': output_dict})
