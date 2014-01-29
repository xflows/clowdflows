
def file_url(input_dict):
    from discomll import dataset
    import itertools

    urls = [url.strip() for url in input_dict["url"].split("\n") if url != ""]
    for url in urls:
        if url.split("/")[0].lower().startswith("https"):
            raise Exception("URLs should be accessible via HTTP and not HTTPS.")

    if input_dict["range"] == "true":
        if len(urls) != 2:
            raise Exception("A first and last URL should be specified if the Range parameter is checked.")
        
        url_start = urls[0].split("/")
        url_end = urls[1].split("/")
            
        url_base = "/".join(url_start[:-1])
        split_index = url_start[-1].index("a")
        file_name = url_start[-1][0:split_index]
        url_base += "/" + file_name
        repeat = url_start[-1][split_index:]
        finish = url_end[-1][split_index:]
        
        alphabet = "abcdefghijklmnopqrstuvwxyz"
        product = itertools.product(alphabet, repeat=len(repeat))
        urls = []
        for p in product:
            urls.append(url_base + "".join(p))
            if "".join(p) == finish:
                break

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
    
    data = dataset.Data(data_tag = urls,
                            X_indices = X_indices,
                            **input_dict)

    return {"dataset" : data}

def lpsvm_fit(input_dict):
    from discomll.classification import linear_proximal_svm

    fitmodel_url = linear_proximal_svm.fit(input_dict["dataset"],
                                            nu = input_dict["nu"],
                                            save_results = True)
    
    return {"fitmodel_url" : fitmodel_url}    

def lpsvm_predict(input_dict):
    from discomll.classification import linear_proximal_svm

    predictions_url = linear_proximal_svm.predict(input_dict["dataset"],
                                    fitmodel_url = input_dict["fitmodel_url"],
                                    save_results = True)

    from disco.core import result_iterator    
    pred = "ID__predicted__real__\n"
    for X_id, (predicted, real) in result_iterator(predictions_url):
        pred +=  str(X_id) + " " + str(predicted) + " " + str(real) + "\n"
    
    return {"string" : pred}


def lin_reg_fit(input_dict):
    from discomll.regression import linear_regression
    fitmodel_url = linear_regression.fit(input_dict["dataset"],
                    save_results = True)

    return {"fitmodel_url" : fitmodel_url}

def lin_reg_predict(input_dict):
    from discomll.regression import linear_regression

    predictions_url = linear_regression.predict(input_dict["dataset"],
                                    fitmodel_url = input_dict["fitmodel_url"],
                                    save_results = True)

    from disco.core import result_iterator    
    pred = "ID__predicted__real__\n"
    for X_id, (predicted, real) in result_iterator(predictions_url):
        pred +=  str(X_id) + " " + str(predicted) + " " + str(real) + "\n"
    
    return {"string" : pred}





def kmeans_fit(input_dict):
    from discomll.clustering import kmeans

    fitmodel_url = kmeans.fit(input_dict["dataset"],
                                n_clusters = input_dict["clusters"],
                                 max_iterations = input_dict["itr"],
                                 save_results = True)

    return {"fitmodel_url" : fitmodel_url}

def kmeans_predict(input_dict):
    from discomll.clustering import kmeans

    predictions_url = kmeans.predict(input_dict["dataset"],
                                        fitmodel_url = input_dict["fitmodel_url"],
                                        save_results = True)

  
    from disco.core import result_iterator    
    pred = "ID__cluster__real__distance\n"
    for X_id, (cluster, real, distance) in result_iterator(predictions_url):
        pred +=  str(X_id) + " " + str(cluster) + " " + str(real) + " " + str(distance) + "\n"
    
    return {"string" : pred}


def log_reg_fit(input_dict):
    from discomll.classification import logistic_regression

    fitmodel_url = logistic_regression.fit(input_dict["dataset"],
                                            alpha = input_dict["alpha"],
                                            max_iterations = input_dict["itr"],
                                            save_results = True)
    return {"fitmodel_url" : fitmodel_url}

def log_reg_predict(input_dict):
    from discomll.classification import logistic_regression
    predictions_url = logistic_regression.predict(input_dict["dataset"],
                                                fitmodel_url = input_dict["fitmodel_url"],
                                                save_results =True)

    from disco.core import result_iterator    
    pred = "ID__Pred__Real__Probs\n"
    for X_id, (y_predicted, y_real, probs) in result_iterator(predictions_url):
        probs = [round(p, 4) for p in probs]
        pred +=  str(X_id) + " " + str(y_predicted) + " " + str(y_real) + " " + str(probs) + "\n"
    
    return {"string" : pred}




def gaussian_naive_bayes_fit(input_dict):
    from discomll.classification import naivebayes_gaussian

    fitmodel_url = naivebayes_gaussian.fit(input_dict["dataset"],
                                                save_results = True)
    return {"fitmodel_url" : fitmodel_url}

def gaussian_naive_bayes_predict(input_dict):
    from discomll.classification import naivebayes_gaussian

    predictions_url = naivebayes_gaussian.predict(input = input_dict["dataset"], 
                                fitmodel_url = input_dict["fitmodel_url"],
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

    fitmodel_url = naivebayes_multinomial.fit(input_dict["dataset"],
                                                save_results = True)
    return {"fitmodel_url" : fitmodel_url}


def multinomial_naive_bayes_predict(input_dict):
    from discomll.classification import naivebayes_multinomial
    
    m = 1 if input_dict["m"] == "" else input_dict["m"]
    predictions_url = naivebayes_multinomial.predict(input = input_dict["dataset"], 
                                fitmodel_url = input_dict["fitmodel_url"],
                                m = m,
                                save_results = True)

    
    #ta del gre v results
    from disco.core import result_iterator
    pred = "ID__Pred__Real__Probs\n"
    for X_id, (y_predicted, y_real, probs) in result_iterator(predictions_url):
        probs = [round(p, 4) for p in probs]
        pred +=  str(X_id) + " " + str(y_predicted) + " " + str(y_real) + " " + str(probs) + "\n"
   
    return {"string" : pred}
