import datetime
import json
import re

import numpy as np
import pandas as pd
import requests
from django.conf import settings



def auth():
    us,pa=(settings.PD_MANAGER_DB_USERNAME, settings.PD_MANAGER_DB_PASSWORD)
    PD_MANAGER_DB_URL = settings.PD_MANAGER_DB_URL
    url = "%s/oauth/token" % PD_MANAGER_DB_URL

    response = requests.post(url,
                             data={'username':us, 'password':pa, 'grant_type':'password'})

    acc_token = json.loads(str(response.text))['access_token']
    acc_token = str(acc_token)

    headers_acc_token = {"Authorization": "bearer %s"%(acc_token)}
    return headers_acc_token


def pandas_dataframe_to_arff(df):
    import arff

    d = {'relation': 'aaaa', 'attributes':[]}
    data_tr = []

    # attributes and data_tr
    for j,c in enumerate(df.columns):
        if df[c].dtype in [np.dtype('int64'), np.dtype('float64')]:
            d['attributes'].append( (c, 'NUMERIC') )

        else:
            att_values0 = list(df[c].unique())
            att_values = [v for v in att_values0 if not(pd.isnull(v))]

            d['attributes'].append( (c, att_values) )

        # handle missing values in column
        col_data = ['?' if pd.isnull(el) else el for el in list(df[c])]
        data_tr.append( col_data )

    # transpose data (and np.transpose() does not work well with strings)
    d['data'] = []
    for i in range(len(df)):
        row = []
        for j in range(df.shape[1]):
            row.append(data_tr[j][i])
        d['data'].append(row)

    arff_str = arff.dumps(d)

    return arff_str


def download_dataset(ds_id, h):
    url_1 = 'https://pdmanager.3dnetmedical.com/api/datasets/%s'%ds_id

    response_1 = requests.get(url_1, headers=h)
    print str(response_1)

    j = json.loads(response_1.text)

    return j

def parse_observations(l_observations, column_names=None, pat_id=None, obs_type=None):
    """ Parse observations

    :param pat_id:
    :param obs_type: eg. finger_tapping, or gait, ...
    :param l_observations: list of strings of format "1498941988645, 59.0, 168.47, 304.0, 0.0;"
    :param column_names: names of rest of the columns (besides time)
    :return:
    """

    df = []
    if len(l_observations)==0:
        return []
    for l in l_observations:
        if len(l.strip()) == 0:
            break
        l = l.strip()
        if l.find(";")>=0:
            l = l.split(";")[0]
        df.append([float(v) for v in l.split(",")])

    if column_names is None:
        columns = ["time"] + ["var_{:02d}".format(j) for j in range(len(l.split(","))-1)]
    else:
        if len(l.split(",")) - 1 == len(column_names):
            columns = ["time"] + column_names
        else:
            raise Exception("Observation parsing problem: Number of column names needed is {} while provided is {}!".format(len(l.split(",")) - 1, len(column_names)))

    df = pd.DataFrame(df, columns=columns)
    df['time'] = df.apply(lambda r: datetime.datetime.fromtimestamp(r["time"] / 1000.0), axis=1)
    df['pat_id'] = pat_id
    df['obs_type'] = obs_type

    # print len(df), "obs in ", df.time.min().date(), df.time.min().date(), "\n\n"

    # transform to long format
    df = pd.melt(df, id_vars="pat_id time obs_type".split(), var_name="variable", value_name="value")

    return df

def convert(name):
    s1 = re.sub('(.)([A-Z][a-z]+)', r'\1_\2', name)
    return re.sub('([a-z0-9])([A-Z])', r'\1_\2', s1).lower()



def pdmanager_import_dataset(input_dict):
    """Import dataset from the main database"""

    # --- dataset id, target variable and predictor variables ---------------
    d_id = settings.PD_MANAGER_DATASET_ID
    target, target_val_prefix = ('gait_var_00', 'Gait')

    # non_motor
    l_nm = "fingerTappingSimple fingerTappingAlternate".split()

    # motor
    l_m = ["gait"]
    # ------------------------------------------------------------------------

    dict_json = download_dataset(d_id, auth())


    num_p = len(dict_json["Values"])
    print("Parsing data for {} patients".format(num_p))

    df = []
    if num_p >= 2:
        for j, v in enumerate(dict_json["Values"]):  # for each patient
            pat_id = "pat_{:02d}_{}_{}".format(j, v["PatientInfo"]["gender"].replace(" ", ""),
                                               v["PatientInfo"]["age"].replace(" ", ""))

            # non_motor
            if len(v["NonMotor"].keys()) > 0:
                for t in l_nm:
                    df_ = parse_observations(v["NonMotor"][t], pat_id=pat_id, obs_type=convert(t))
                    df = pd.concat([df, df_]) if len(df) > 0 else df_

            # motor
            if len(v["Motor"].keys()) > 0:
                for t in l_m:
                    df_ = parse_observations(v["Motor"][t], pat_id=pat_id, obs_type=convert(t))
                    df = pd.concat([df, df_]) if len(df) > 0 else df_

    df = df.sort("pat_id time obs_type variable".split()).reset_index(drop=1)

    # non_m and m data on same day
    df['date'] = df['time'].dt.date

    df_ = df.groupby("pat_id date obs_type variable".split()).value.max().reset_index()
    df_['obs_type_var'] = df_['obs_type'] + "_" + df_['variable']
    df_['pat_date'] = df_.apply(lambda r: r['pat_id'] + "_" + str(r['date']), axis=1)

    df_ = df_.pivot(index="pat_date", columns="obs_type_var", values="value")

    # target must not have nulls - drop all such rows
    df_ = df_.dropna(subset=[target])
    df_.ix[df_[target] == 0.0, target] = '{}_0'.format(target_val_prefix)
    df_.ix[df_[target] == 1.0, target] = '{}_1'.format(target_val_prefix)
    df_.ix[df_[target] == 2.0, target] = '{}_2'.format(target_val_prefix)
    df_.ix[df_[target] == 3.0, target] = '{}_3'.format(target_val_prefix)
    df_.ix[df_[target] == 4.0, target] = '{}_4'.format(target_val_prefix)

    num_predictors = df_.shape[1] - 1

    # drop rows for which we have no non_m data
    df_['num_nulls'] = df_.transpose().isnull().sum()
    df_ = df_[df_.num_nulls < num_predictors]
    df_ = df_.drop(['num_nulls'], axis=1)


    arff_str = pandas_dataframe_to_arff(df_)

    with open("pdm_sample_dataset_%3d_rows.arff"%len(df), "w") as f:
        f.write(arff_str)

    return {'arff_data': str(arff_str)}
