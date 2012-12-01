/**
 * HTTP helpers.
 */
define(function() {
    
    /**
     * Creates a new XMLHttpRequest object with the given success and error handlers.
     * The request will have been opened, but not yet sent, so headers can
     * still be applied.
     * 
     * If successful, the onSuccess handler will be called with the response text.
     * If an error occurs, then the onError handler will be called with any response
     * text that could be obtained.
     */
    function createRequest(method, url, onSuccess, onError) {
        var req = new XMLHttpRequest();
        req.addEventListener("readystatechange", function(event) {
            if (req.readyState == 4) {
                if (req.status >= 200 && req.status < 300) {
                    if (onSuccess) {
                        onSuccess(req.responseText);
                    }
                } else {
                    if (onError) {
                        onError(req.responseText);
                    }
                }
            }
        });
        req.open("POST", url, true);
        return req;
    }
    
    /**
     * Performs a GET request against the given URL.
     */
    function get(url, onSuccess, onError) {
        var req = createRequest("GET", url, onSuccess, onError);
        req.send()
    }
    
    /**
     * Performs a GET request against the given JSON resource.
     */
    function getJSON(url, onSuccess, onError) {
        get(url, function(data) {
            onSuccess(JSON.parse(data))
        }, onError);
    }
    
    // Export the public API.
    
    return {
        get: get,
        getJSON: getJSON
    };
    
});