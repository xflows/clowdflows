def benchmark(input_dict):
    import time
    in_att = input_dict.get('in_att', None)
    start_time= input_dict.get('start_time', None)
    time_diff=(time.time()-start_time) if start_time else time.time()
    return {'out_att': in_att, 'time_diff': time_diff}


def perfeval_classification_statistics(input_dict):
    from sklearn import metrics
    labels = input_dict['true_and_predicted_labels']
    pos_label = input_dict.get('pos_label', None)

    # Check if we have true and predicted labels for each fold
    if labels and type(labels[0][0]) == list:
        try:
            # Flatten
            y_true, y_pred = [], []
            for fold_labels in labels:
                y_true.extend(fold_labels[0])
                y_pred.extend(fold_labels[1])
            labels = [y_true, y_pred]
        except:
            raise Exception('Expected true and predicted labels for each fold, but failed.' + 
                            'If you wish to provide labels for each fold separately it should look like: ' + 
                            '[[y_true_1, y_predicted_1], [y_true_2, y_predicted_2], ...]')
    if len(labels) != 2:
        raise Exception('Wrong input structure, this widget accepts labels in the form: [y_true, y_pred]')
    
    y_true, y_pred = labels
    
    classes = set()
    classes.update(y_true + y_pred)
    classes = sorted(list(classes))

    # Assign integers to classes
    class_to_int = {}
    for i, cls_label in enumerate(classes):
        class_to_int[cls_label] = i

    y_true = [class_to_int[lbl] for lbl in y_true]
    y_pred = [class_to_int[lbl] for lbl in y_pred]

    accuracy = metrics.accuracy_score(y_true, y_pred)
    precision = metrics.precision_score(y_true, y_pred)
    recall = metrics.recall_score(y_true, y_pred)
    f1 = metrics.f1_score(y_true, y_pred)
    confusion_matrix = metrics.confusion_matrix(y_true, y_pred)

    # AUC is defined only for binary classes
    if len(classes) == 2:
        auc = metrics.auc_score(y_true, y_pred)
    else:
        auc = 'undefined for multiple classes'
    return {'accuracy': accuracy, 'precision': precision, 'recall': recall, 
            'f1': f1, 'auc': auc, 'confusion_matrix': confusion_matrix}
