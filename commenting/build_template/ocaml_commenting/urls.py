from django.conf.urls import patterns, url
from django.contrib import admin
from django.views import generic
from django.conf import settings


admin.autodiscover()


urlpatterns = patterns("",
    
    # There's no favicon here!
    url(r"^favicon.ico$", generic.RedirectView.as_view()),
    
    # Serve all static files.
    url(r"^(.*)", "django.views.static.serve", kwargs={
        "document_root": settings.WWW_ROOT,
    }),
    
)