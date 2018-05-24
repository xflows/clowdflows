# Create your views here.
from django.contrib.auth.models import User
from django.shortcuts import render


def gdpr_form(request):
    posted = False
    error = ""
    if request.method == "POST":
        posted = True
        try:
            user = User.objects.get(email=request.POST['email'])
            success = True
        except User.DoesNotExist:
            success = False
            error = "User with this email does not exist."
        if success:
            gdpr_profile = user.gdprprofile
            gdpr_profile.allow_correspondence = request.POST.get("correspondence", False) is not False
            gdpr_profile.accepted_terms = request.POST.get("privacy", False) is not False
            gdpr_profile.save()
    return render(request, "gdpr.html", {
        'posted': posted,
        'error': error,
        'email': request.GET.get('email', '')
    })
