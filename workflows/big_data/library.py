def file_url(input_dict):
    from discomll import dataset

    if input_dict["range"] == "true":
        urls = [url.strip() for url in input_dict["url"].split("\n") if url != ""]
    else:
        urls = [[url.strip()] for url in input_dict["url"].split("\n") if url != ""]
        for url in urls:
            if url[0].split("://")[0] == "https":
                raise Exception("Dataset should be accessible over HTTP.")
    del (input_dict["url"])

    X_indices_splited = input_dict["X_indices"].replace(" ", "").split("-")
    if len(X_indices_splited) == 2:
        a, b = X_indices_splited
        if not a.isdigit() or not b.isdigit():
            raise Exception("Feature indices should be integers. Example: 1-10")
        X_indices = range(int(a), int(b))
    else:
        X_indices = [int(v) for v in input_dict["X_indices"].replace(" ", "").split(",") if v != ""]
    del (input_dict["X_indices"])

    input_dict["data_type"] = "gzip" if input_dict["data_type"] == "true" else ""

    if input_dict["atr_meta"] == "numeric":
        X_meta = ["c" for i in range(len(X_indices))]
    elif input_dict["atr_meta"] == "discrete":
        X_meta = ["d" for i in range(len(X_indices))]
    else:
        X_meta = input_dict["custom"]

    data = dataset.Data(data_tag=urls,
                        X_indices=X_indices,
                        X_meta=X_meta,
                        generate_urls=True if input_dict["range"] == "true" else False,
                        **input_dict)

    print data.params

    return {"dataset": data}


def big_data_apply_classifier(input_dict):
    if "naivebayes_fitmodel" in input_dict["fitmodel_url"]:
        return naivebayes_predict(input_dict)
    elif "logreg_fitmodel" in input_dict["fitmodel_url"]:
        return logreg_predict(input_dict)
    elif "linsvm_fitmodel" in input_dict["fitmodel_url"]:
        return linsvm_predict(input_dict)
    elif "kmeans_fitmodel" in input_dict["fitmodel_url"]:
        return kmeans_predict(input_dict)
    elif "fddt_fitmodel" in input_dict["fitmodel_url"]:
        return dt_predict(input_dict)
    elif "drf_fitmodel" in input_dict["fitmodel_url"]:
        return rf_predict(input_dict)
    elif "dwfr_fitmodel" in input_dict["fitmodel_url"]:
        return wrf_predict(input_dict)
    elif "linreg_fitmodel" in input_dict["fitmodel_url"]:
        return linreg_predict(input_dict)


def lwlr_fit_predict(input_dict):
    from discomll.regression import locally_weighted_linear_regression

    predictions_url = locally_weighted_linear_regression.fit_predict(
        fitting_data=input_dict["fitting_dataset"],
        training_data=input_dict["training_dataset"],
        tau=input_dict["tau"],
        save_results=True)
    return {"string": predictions_url}


def dt_fit(input_dict):
    from discomll.ensemble import forest_distributed_decision_trees

    random_state = None if input_dict["seed"] == "None" else int(input_dict["seed"])

    fitmodel_url = forest_distributed_decision_trees.fit(input_dict["dataset"],
                                                         trees_per_chunk=input_dict["trees_per_subset"],
                                                         max_tree_nodes=input_dict["tree_nodes"],
                                                         min_samples_leaf=input_dict["min_samples_leaf"],
                                                         min_samples_split=input_dict["min_samples_split"],
                                                         class_majority=input_dict["majority"],
                                                         bootstrap=input_dict["bootstrap"] == "true",
                                                         measure=input_dict["measure"],
                                                         accuracy=input_dict["accuracy"],
                                                         separate_max=input_dict["separate_max"] == "true",
                                                         random_state=random_state,
                                                         save_results=True)
    return {"fitmodel_url": fitmodel_url}


def dt_predict(input_dict):
    from discomll.ensemble import forest_distributed_decision_trees

    predictions_url = forest_distributed_decision_trees.predict(input_dict["dataset"],
                                                                fitmodel_url=input_dict["fitmodel_url"],
                                                                save_results=True)
    return {"string": predictions_url}


def rf_fit(input_dict):
    from discomll.ensemble import distributed_random_forest

    random_state = None if input_dict["seed"] == "None" else int(input_dict["seed"])

    fitmodel_url = distributed_random_forest.fit(input_dict["dataset"],
                                                 trees_per_chunk=input_dict["trees_per_subset"],
                                                 max_tree_nodes=input_dict["tree_nodes"],
                                                 min_samples_leaf=input_dict["min_samples_leaf"],
                                                 min_samples_split=input_dict["min_samples_split"],
                                                 class_majority=input_dict["majority"],
                                                 measure=input_dict["measure"],
                                                 accuracy=input_dict["accuracy"],
                                                 separate_max=input_dict["separate_max"] == "true",
                                                 random_state=random_state,
                                                 save_results=True)

    return {"fitmodel_url": fitmodel_url}


def rf_predict(input_dict):
    from discomll.ensemble import distributed_random_forest

    predictions_url = distributed_random_forest.predict(input_dict["dataset"],
                                                        fitmodel_url=input_dict["fitmodel_url"],
                                                        save_results=True)
    return {"string": predictions_url}


def wrf_fit(input_dict):
    from discomll.ensemble import distributed_weighted_forest_rand

    random_state = None if input_dict["seed"] == "None" else int(input_dict["seed"])

    fitmodel_url = distributed_weighted_forest_rand.fit(input_dict["dataset"],
                                                        trees_per_chunk=input_dict["trees_per_subset"],
                                                        max_tree_nodes=input_dict["tree_nodes"],
                                                        num_medoids=input_dict["num_medoids"],
                                                        min_samples_leaf=input_dict["min_samples_leaf"],
                                                        min_samples_split=input_dict["min_samples_split"],
                                                        class_majority=input_dict["majority"],
                                                        measure=input_dict["measure"],
                                                        accuracy=input_dict["accuracy"],
                                                        separate_max=input_dict["separate_max"] == "true",
                                                        random_state=random_state,
                                                        save_results=True)
    return {"fitmodel_url": fitmodel_url}


def wrf_predict(input_dict):
    from discomll.ensemble import distributed_weighted_forest_rand

    predictions_url = distributed_weighted_forest_rand.predict(input_dict["dataset"],
                                                               fitmodel_url=input_dict["fitmodel_url"],
                                                               coeff=input_dict["coeff"],
                                                               save_results=True)
    return {"string": predictions_url}


def linsvm_fit(input_dict):
    from discomll.classification import linear_svm
    fitmodel_url = linear_svm.fit(input_dict["dataset"],
                                  nu=input_dict["nu"],
                                  save_results=True)
    return {"fitmodel_url": fitmodel_url}


def linsvm_predict(input_dict):
    from discomll.classification import linear_svm

    predictions_url = linear_svm.predict(input_dict["dataset"],
                                         fitmodel_url=input_dict["fitmodel_url"],
                                         save_results=True)
    return {"string": predictions_url}


def linreg_fit(input_dict):
    from discomll.regression import linear_regression

    fitmodel_url = linear_regression.fit(input_dict["dataset"],
                                         save_results=True)

    return {"fitmodel_url": fitmodel_url}


def linreg_predict(input_dict):
    from discomll.regression import linear_regression

    predictions_url = linear_regression.predict(input_dict["dataset"],
                                                fitmodel_url=input_dict["fitmodel_url"],
                                                save_results=True)

    return {"string": predictions_url}


def kmeans_fit(input_dict):
    from discomll.clustering import kmeans

    random_state = None if input_dict["seed"] == "None" else int(input_dict["seed"])

    fitmodel_url = kmeans.fit(input_dict["dataset"],
                              n_clusters=input_dict["clusters"],
                              max_iterations=input_dict["itr"],
                              random_state=random_state,
                              save_results=True)

    return {"fitmodel_url": fitmodel_url}


def kmeans_predict(input_dict):
    from discomll.clustering import kmeans

    predictions_url = kmeans.predict(input_dict["dataset"],
                                     fitmodel_url=input_dict["fitmodel_url"],
                                     save_results=True)
    return {"string": predictions_url}


def logreg_fit(input_dict):
    from discomll.classification import logistic_regression

    fitmodel_url = logistic_regression.fit(input_dict["dataset"],
                                           alpha=input_dict["alpha"],
                                           max_iterations=input_dict["itr"],
                                           save_results=True)
    return {"fitmodel_url": fitmodel_url}


def logreg_predict(input_dict):
    from discomll.classification import logistic_regression

    predictions_url = logistic_regression.predict(input_dict["dataset"],
                                                  fitmodel_url=input_dict["fitmodel_url"],
                                                  save_results=True)
    return {"string": predictions_url}


def naivebayes_fit(input_dict):
    from discomll.classification import naivebayes

    fitmodel_url = naivebayes.fit(input_dict["dataset"], save_results=True)

    return {"fitmodel_url": fitmodel_url}


def naivebayes_predict(input_dict):
    from discomll.classification import naivebayes
    m = 1 if input_dict["m"] == "" else input_dict["m"]

    predictions_url = naivebayes.predict(input_dict["dataset"],
                                         fitmodel_url=input_dict["fitmodel_url"],
                                         m=input_dict["m"],
                                         save_results=True)

    return {"string": predictions_url}


def results_to_file(input_dict):
    # implementation is in visualization_views.py
    return {}


def measure_distribution(input_dict):
    # implementation is in visualization_views.py
    return {}


def model_view(input_dict):
    # implementation is in visualization_views.py
    return {}


def bigdata_ca(input_dict):
    # implementation is in visualization_views.py
    return {}


def bigdata_mse(input_dict):
    # implementation is in visualization_views.py
    return {}
