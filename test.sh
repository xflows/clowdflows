coverage run --source='.' manage.py test
coverage html --omit=workflows/segmine/data/mappings.py
