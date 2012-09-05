from django.contrib.auth.models import User
from django.contrib.auth import authenticate, login, logout
from django.shortcuts import render, get_object_or_404, redirect

def signuplogin(request):
    if request.method == 'POST':
        if request.POST.get('login'):
            uname = request.POST['username']
            passw = request.POST['password']

            user = authenticate(username=uname, password=passw)

            if user is not None:
                login(request, user)

                if not request.POST.get('remember', None):
                    request.session.set_expiry(0)

                return ajaxresponse(request, 'OK')
            else:
                return ajaxresponse(request, 'ERR')

        elif request.POST.get('register'):
            uname = request.POST['rusername']
            passw = request.POST['rpassword']
            rpass = request.POST['repeat_password']
            mail = request.POST['email']

            try:
                tuser = User.objects.get(username__exact=uname)
            except User.DoesNotExist:
                tuser = None
            
            if User.objects.filter(email=mail).count()>0:
                return ajaxresponse(request, 'MAILTAKEN')
            
            if tuser is not None:
                return ajaxresponse(request, 'TAKEN')
            else:
                new_user = User.objects.create_user(uname, mail, passw)

                user = authenticate(username=uname, password=passw)
                login(request, user)
                return ajaxresponse(request, 'OK')
    else:
        return render(request, 'signuplogin/signuplogin.html')

def ajaxresponse(request, txt):
    return render(request, 'signuplogin/ajaxresponse.html', {'response':txt})