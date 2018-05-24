from django.db import models
from django.contrib.auth.models import User
from django.db.models.signals import post_save


class GDPRProfile(models.Model):
    user = models.OneToOneField(User, related_name="gdprprofile")
    accepted_terms = models.BooleanField(default=False)
    allow_correspondence = models.BooleanField(default=False)

    updated = models.DateTimeField(auto_now=True)

    def __unicode__(self):
        return unicode(self.user)


def create_user_gdpr_profile(sender, instance, created, **kwargs):
    profile_set = GDPRProfile.objects.filter(user__id=instance.id)
    if created and not profile_set.exists():
        GDPRProfile.objects.create(user=instance)


post_save.connect(create_user_gdpr_profile, sender=User)
