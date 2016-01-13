from django.conf import settings
from django.core.management.base import BaseCommand

from .import_package import import_package


class Command(BaseCommand):
    args = 'package_name'
    help = 'Imports all packages.'

    def handle(self, *args, **options):
        packages = []
        extern_packages = []
        for app in settings.INSTALLED_APPS:
            if 'workflows.' in app:
                package_name = app.split('workflows.')[1]
                packages.append(package_name)
            elif app in settings.INSTALLED_APPS_EXTERNAL_PACKAGES:
                extern_packages.append(app)

        for package in packages:
            self.stdout.write("Importing package "+package+"\n")
            import_package(package,self.stdout)

        for package in extern_packages:
            self.stdout.write("Importing external package "+package+"\n")
            import_package(package,self.stdout, external=True)
