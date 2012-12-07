import urllib2, json

from django.conf import settings
from django.utils.http import urlencode


class OAuth2Middleware(object):
    
    def process_response(self, request, response):
        # Set cookies for client id.
        if not "github_client_id" in request.COOKIES:
            response.set_cookie("github_client_id", settings.GITHUB_CLIENT_ID)
        # Check for a temporary access code.
        if "code" in request.GET:
            # Get an access token.
            data = urlencode({
                "client_id": settings.GITHUB_CLIENT_ID,
                "code": request.GET["code"],
                "state": "",
            })
            request = urllib2.Request("https://github.com/login/oauth/access_token", data, headers = {
                "Accept": "application/json",
            })
            try:
                response = urllib2.urlopen(request)
                body = response.read()
            except urllib2.URLError:
                pass
            data = json.loads(body)
            access_token = data["access_token"]
            # Save access token to a cookie.
            response.set_cookie("github_access_token", access_token)
        # All done!
        return response