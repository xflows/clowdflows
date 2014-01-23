
def file_url(input_dict):
    from discomll import dataset

    X_indices_splited = input_dict["X_indices"].replace(" ","").split("-")
    if len(X_indices_splited) == 2:
        a, b = X_indices_splited
        if not a.isdigit() or not b.isdigit():
            raise Exception("Feature indices should be integers. Example: 1-10")
        X_indices = range(int(a), int(b))
    else:
        X_indices = [int(v) for v in input_dict["X_indices"].replace(" ","").split(",") if v != ""]
    del(input_dict["X_indices"])

    input_dict["data_type"] = "gzip" if input_dict["data_type"] == "true" else ""
    urls = [url.strip() for url in input_dict["url"].split("\n") if url != ""]

    data = dataset.Data(data_tag = urls,
                            X_indices = X_indices,
                            **input_dict)

    return {"dataset" : data}

def log_reg_fit(input_dict):
    from discomll.classification import logistic_regression

    fit_model_url = logistic_regression.fit(input_dict["dataset"],
                                            alpha = input_dict["alpha"],
                                            max_iterations = input_dict["itr"])
    return {"fitmodel_url" : fit_model_url}

def log_reg_predict(input_dict):
    from discomll.classification import logistic_regression
    predictions_url = logistic_regression.predict(input_dict["dataset"],
                                                thetas_url = input_dict["fitmodel_url"])

    from disco.core import result_iterator    
    pred = "ID__Pred__Real__Probs\n"
    for X_id, (y_predicted, y_real, probs) in result_iterator(predictions_url):
        probs = [round(p, 4) for p in probs]
        pred +=  str(X_id) + " " + str(y_predicted) + " " + str(y_real) + " " + str(probs) + "\n"
    
    return {"string" : pred}




def gaussian_naive_bayes_fit(input_dict):
    from discomll.classification import naivebayes_gaussian

    fit_model_url = naivebayes_gaussian.fit(input_dict["dataset"],
                                                save_results = True)
    return {"fitmodel_url" : fit_model_url}

def gaussian_naive_bayes_predict(input_dict):
    from discomll.classification import naivebayes_gaussian
 


    predictions_url = naivebayes_gaussian.predict(input = input_dict["dataset"], 
                                fit_model_url = input_dict["fitmodel_url"],
                                log_probs = True if input_dict["log_probs"] == "true" else False,
                                save_results = True )

    #results widget
    from disco.core import result_iterator    
    pred = "ID__Pred__Real__Probs\n"
    for X_id, (y_predicted, y_real, probs) in result_iterator(predictions_url):
        probs = [round(p, 4) for p in probs]
        pred +=  str(X_id) + " " + str(y_predicted) + " " + str(y_real) + " " + str(probs) + "\n"
    
    return {"string" : pred}



def multinomail_naive_bayes_fit(input_dict):
    from discomll.classification import naivebayes_multinomial

    fit_model_url = naivebayes_multinomial.fit(input_dict["dataset"],
                                                save_results = True)
    return {"fitmodel_url" : fit_model_url}


def multinomial_naive_bayes_predict(input_dict):
    from discomll.classification import naivebayes_multinomial

    
    m = 1 if input_dict["m"] == "" else input_dict["m"]
    predictions_url = naivebayes_multinomial.predict(input = input_dict["dataset"], 
                                fit_model_url = input_dict["fitmodel_url"],
                                m = m,
                                save_results = True)

    
    #ta del gre v results
    from disco.core import result_iterator
    pred = "ID__Pred__Real__Probs\n"
    for X_id, (y_predicted, y_real, probs) in result_iterator(predictions_url):
        probs = [round(p, 4) for p in probs]
        pred +=  str(X_id) + " " + str(y_predicted) + " " + str(y_real) + " " + str(probs) + "\n"
   
    return {"string" : pred}
