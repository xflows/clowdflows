#!/usr/bin/env python

# Copyright (C) <2014> Janez Kranjc

import os, sys

if __name__ == '__main__':
    os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'mothra.settings')

    from django.core.management import execute_from_command_line
    execute_from_command_line(sys.argv)
