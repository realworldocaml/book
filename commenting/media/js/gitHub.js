/**
 * GitHub API helpers.
 */
define([
    "module",
    "http"
], function(
    module,
    http
) {
    
    // Configure the module.
    var config = module.config();
    var gitHubUser = config.user;
    var gitHubRepo = config.repo;
    
    /**
     * Returns the list of issues with a given tag.
     */
    function getIssuesByTag(tag, onSuccess) {
        http.getJSON("https://api.github.com/" + encodeURIComponent(gitHubUser) + "/" + encodeURIComponent(gitHubRepo), onSuccess);
    }
    
    // Export the public API.
    
    return {
        getIssuesByTag: getIssuesByTag
    };
    
});