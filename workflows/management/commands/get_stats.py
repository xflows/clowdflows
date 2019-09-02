from django.contrib.auth.models import User
from django.core.management.base import BaseCommand, CommandError
from collections import defaultdict
from datetime import datetime


class Command(BaseCommand):
    help = 'Get some user usage stats'
    args = 'cut date'

    def diff_month(self, d1, d2):
    	year1 = int(d1.split('_')[0])
    	year2 = int(d2.split('_')[0])
    	month1 = int(d1.split('_')[1])
    	month2 = int(d2.split('_')[1])
    	print(month1, month2, year1, year2)
        return (year1 - year2) * 12 + month1 - month2

    def handle(self, *args, **options):
    	cut_date = args[0]
        users = User.objects.all()
        monthly_stats_joined = defaultdict(int)
        daily_stats_joined = defaultdict(int)
        monthly_stats_active = defaultdict(int)
        daily_stats_active = defaultdict(int)
        counter = 0
        today = str(datetime.today().year) + '_' + str(datetime.today().month) + '_' + str(datetime.today().day)
        all_months = self.diff_month(today, cut_date)


        for user in users:
        	
        	joined_month = user.date_joined.month
        	if joined_month < 10:
        		joined_month = str(0) + str(joined_month)
        	else:
        		joined_month = str(joined_month)

        	login_month = user.last_login.month
        	if login_month < 10:
        		login_month = str(0) + str(login_month)
        	else:
        		login_month = str(login_month)


        	joined_day = user.date_joined.day
        	if joined_day < 10:
        		joined_day = str(0) + str(joined_day)
        	else:
        		joined_day = str(joined_day)

        	login_day = user.last_login.day
        	if login_day < 10:
        		login_day = str(0) + str(login_day)
        	else:
        		login_day = str(login_day)



        	ts_joined_d = str(user.date_joined.year) + '_' + joined_month + '_' + joined_day
        	ts_active_d = str(user.last_login.year) + '_' + login_month + '_' + login_day
        	ts_joined_m = str(user.date_joined.year) + '_' + joined_month
        	ts_active_m = str(user.last_login.year) + '_' + login_day



        	if ts_joined_m > cut_date:
        		counter += 1

        	monthly_stats_joined[ts_joined_m] += 1
        	monthly_stats_active[ts_active_m] += 1
        	daily_stats_joined[ts_joined_d] += 1
        	daily_stats_active[ts_active_d] += 1
        
        print 'New users per month: '
        
        for month, num in sorted(monthly_stats_joined.items(), key=lambda x:x[0]):
        	print month + ': ' + str(num)
        
        
        print 'New users since ', cut_date, ': ', counter
        print 'Average new users per month: ', counter/float(all_months)

   





        
