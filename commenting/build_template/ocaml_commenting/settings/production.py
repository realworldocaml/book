"""
Production settings for ocaml_commenting project.

For an explanation of these settings, please see the Django documentation at:

<http://docs.djangoproject.com/en/dev/>

While many of these settings assume sensible defaults, you must provide values
for the site, database, media and email sections below.
"""

import os


# Domain settings.

PREPEND_WWW = False


# GitHub settings.

GITHUB_CLIENT_ID = "0d3f42d377adb03650e0"

GITHUB_CLIENT_SECRET = "cfbc90cd86b76161859372f72fb80d9f9fc80957"


# Database settings.

DATABASES = {
    "default": {}
}


# Error reporting settings.  Use these to set up automatic error notifications.

ADMINS = (
    ("Onespacemedia Error Reporting", "errors@onespacemedia.com"),
)

MANAGERS = ()

SEND_BROKEN_LINK_EMAILS = False


# Locale settings.

TIME_ZONE = "Europe/London"

LANGUAGE_CODE = "en-gb"

USE_I18N = False

USE_L10N = False

USE_TZ = False


# Auto-discovery of project location.

SITE_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))

WWW_ROOT = os.path.join(SITE_ROOT, "www")


# A list of additional installed applications.

INSTALLED_APPS = ()


# Additional static file locations.

STATICFILES_DIRS = ()


# Dispatch settings.

MIDDLEWARE_CLASSES = (
    "django.middleware.gzip.GZipMiddleware",
    "django.middleware.common.CommonMiddleware",
    "django.contrib.sessions.middleware.SessionMiddleware",
    "ocaml_commenting.middleware.OAuth2Middleware",
)

ROOT_URLCONF = "ocaml_commenting.urls"

WSGI_APPLICATION = "ocaml_commenting.wsgi.application"

SESSION_ENGINE = "django.contrib.sessions.backends.signed_cookies"


# Absolute path to the directory where templates are stored.

TEMPLATE_DIRS = (
    os.path.join(SITE_ROOT, "templates"),
)


# A secret key used for cryptographic algorithms.

SECRET_KEY = "&amp;=dti_ic_yb5m(_#1@cgx13=r4j7$4_y^_e44734v$=x2lgod0"


# Logging configuration.

LOGGING = {
    "version": 1,
    "disable_existing_loggers": False,
    "filters": {
        "require_debug_false": {
            "()": "django.utils.log.RequireDebugFalse"
        }
    },
    "handlers": {
        "mail_admins": {
            "level": "ERROR",
            "filters": ["require_debug_false"],
            "class": "django.utils.log.AdminEmailHandler"
        }
    },
    "loggers": {
        "django.request": {
            "handlers": ["mail_admins"],
            "level": "ERROR",
            "propagate": True,
        },
    }
}