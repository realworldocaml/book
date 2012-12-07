from django.conf.urls import patterns, url
from django.contrib import admin
from django.views import generic
from django.conf import settings
from django.views.static import serve


admin.autodiscover()


def serve_with_index(request, path):
    if path.endswith("/"):
        path += "index.html"
    return serve(request, path, document_root=settings.WWW_ROOT)


urlpatterns = patterns("",
    
    # There's no favicon here!
    url(r"^favicon.ico$", generic.RedirectView.as_view()),
    
    # Serve all static files.
    url(r"^(.*)", serve_with_index),
    
)