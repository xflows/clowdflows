# Local installation of the Mothra project #
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
mkvirtualenv --no-site-packages mothra-env
```

#### For virtualenv ####
```bash
virtualenv --no-site-packages mothra-env
cd mothra-env
source bin/activate
```

### Clone the code ###
Obtain the url to your git repository.

```bash
git clone git@source.ijs.si:kt/mothra.git
```

### Install requirements ###
```bash
cd mothra
pip install -r requirements.txt
```

### Configure project ###
```bash
cp mothra/__local_settings.py mothra/local_settings.py
vi mothra/local_settings.py
```

### Enable workflow packages ###
Uncomment the packages that you need in `mothra/local_settings.py` in the `INSTALLED_APPS_WORKFLOWS_SUB` tuple.

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