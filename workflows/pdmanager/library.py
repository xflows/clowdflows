import json
import requests
from django.conf import settings

def pdmanager_import_data(input_dict):

    dataset_id = input_dict['dataset_id']
    url = settings.PD_MANAGER_DB_URL + '/oauth/token'
    response = requests.post(url, data={'username': settings.PD_MANAGER_DB_USERNAME,
                                        'password': settings.PD_MANAGER_DB_PASSWORD,
                                        'grant_type': 'password'})

    acc_token = json.loads(str(response.text))['access_token']
    acc_token = str(acc_token)

    return {'data': [acc_token]}
