# Local installation of the ClowdFlows project #
## Prerequisites ##

- python >= 2.5
- pip
- virtualenv/wrapper (optional)
- python headers if you're compiling Pillow from source: you need the `python-dev` package on debian systems

## Installation ##
### Creating the environment ###
Create a virtual python environment for the project.
If you're not using virtualenv or virtualenvwrapper you may skip this step.

#### For virtualenvwrapper ####
```bash
mkvirtualenv --no-site-packages cf-env
```

#### For virtualenv ####
```bash
virtualenv --no-site-packages cf-env
cd cf-env
source bin/activate
```

### Clone the code ###
Obtain the url to your git repository.

```bash
git clone -b 2.0 git@github.com:xflows/clowdflows.git clowdflows_2_0
```

### Install requirements ###
```bash
cd clowdflows_2_0
pip install -r requirements.txt
```

### Install external package cf_core ###
```bash
cd ..
git clone git@github.com:xflows/cf_core.git
cd cf_core
pip install -r requirements.txt
python setup.py develop
cd ..
```

### Install external package cf_data_mining ###
```bash
git clone git@github.com:xflows/cf_data_mining.git
cd cf_data_mining
pip install -r requirements.txt
python setup.py develop
cd ..
```

### Configure ClowdFlows project ###
```bash
cd clowdflows_2_0
cp mothra/__local_settings.py mothra/local_settings.py
vi mothra/local_settings.py
```

### Enable workflow packages ###
Uncomment the packages `cf_core`, `cf_data_mining` and other packages needed, in the `INSTALLED_APPS_EXTERNAL_PACKAGES` tuple.

### Sync database ###
Say "no" to creating a super-user when prompted. You'll create the user after migrations.

```bash
python manage.py syncdb --noinput
```

### Migrate database ###
```bash
python manage.py migrate
```

### Create super-user ###
```bash
python manage.py createsuperuser
```

### Import packages ###
```bash
python manage.py import_all
```

## Running ##
```bash
python manage.py runserver
```

## Running with debugger ##
```bash
python manage.py runserver_plus
```

Open browser to http://127.0.0.1:8000
