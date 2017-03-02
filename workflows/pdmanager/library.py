import json
import requests, urllib
from django.conf import settings
import pandas as pd, numpy as np

def pdmanager_import_data(input_dict):

    dataset_id = input_dict['dataset_id']
    url = settings.PD_MANAGER_DB_URL + '/oauth/token'
    response = requests.post(url, data={'username': settings.PD_MANAGER_DB_USERNAME,
                                        'password': settings.PD_MANAGER_DB_PASSWORD,
                                        'grant_type': 'password'})

    acc_token = json.loads(str(response.text))['access_token']
    acc_token = str(acc_token)

    return {'data': [acc_token]}


def convert_date_to_unix(y_m_d_tuple):
    # dti = pd.DatetimeIndex( [ pd.datetime(2016,5,5) ])   #.astype(np.int64)

    y,m,d = (y_m_d_tuple[0], y_m_d_tuple[1], y_m_d_tuple[2])

    dti = pd.DatetimeIndex( [ pd.datetime(y,m,d) ])
    a = dti.astype(np.int64) // 10**6
    # print a
    return a[0]

def convert_from_unix_to_datetime(a):
    b = pd.to_datetime(a / 1000.0, unit='s')
    print b[0]
    return b[0]




