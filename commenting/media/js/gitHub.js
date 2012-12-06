/**
 * GitHub API helpers.
 */
define([
    "module",
    "jquery"
], function(
    module,
    $
) {
    
    // Configure the module.
    var config = module.config();
    var gitHubUser = config.user;
    var gitHubRepo = config.repo;
    var gitHubMilestone = config.milestone;
    var gitHubClientId = config.clientId;
    var gitHubPageLabel = "page-" + config.page.split(".")[0];
    
    /**
     * Looks up a list of milestones for this repository.
     */
    function getMilestones(onSuccess) {
        $.getJSON("https://api.github.com/repos/" + encodeURIComponent(gitHubUser) + "/" + encodeURIComponent(gitHubRepo) + "/milestones?callback=?", onSuccess);
    }
    
    /**
     * Santizes a milestone title for lookup.
     */
    function santizeMilestoneTitle(title) {
        return title.replace(" ", "").toLowerCase();
    }
    
    /**
     * Looks up a milestone by the supplied config title.
     */
    function getMilestoneByTitle(onSuccess) {
        getMilestones(function(milestoneData) {
            var milestone;
            for (var n = 0; n < milestoneData.data.length; n++) {
                milestone = milestoneData.data[n];
                if (santizeMilestoneTitle(milestone.title) == santizeMilestoneTitle(gitHubMilestone)) {
                    onSuccess(milestone);
                    return;
                }
            }
            throw new Error("No milestone with title " + gitHubMilestone + " was found");
        });
    }
    
    /**
     * Returns the list of issues with a given tag.
     */
    function getIssues(onSuccess) {
        getMilestoneByTitle(function(milestone) {
            $.getJSON("https://api.github.com/repos/" + encodeURIComponent(gitHubUser) + "/" + encodeURIComponent(gitHubRepo) + "/issues?callback=?", {
                milestone: milestone.number,
                labels: gitHubPageLabel
            }, onSuccess);
        });
    }
    
    /**
     * Looks up the auth code query parameter.
     */
    function getAuthCode() {
        var match = String(document.location).match(/code=(\w+)/);
        return !!match && match[1];
    }
    
    /**
     * Activates authentication.
     */
    function authenticate(onSuccess) {
        var authCode = getAuthCode();
        var loginStatusContainer = $("#login-status");
        if (authCode) {
            $.getJSON("https://api.github.com", function(data) {
                console.log(data)
            })
            
            // Authenticate the code.
            $.post("https://github.com/login/oauth/access_token", {
                code: authCode
            }, function(data) {
                console.log(data);
            });
        } else if (false) {
            // See if we are already logged in.
            console.log("Already logged in.")
        } else {
            // We are not logged in.
            loginStatusContainer.append($("<a/>", {
                text: "Login to GitHub view comments",
                href: "https://github.com/login/oauth/authorize?response_type=token&client_id=" + gitHubClientId
            }));
            onSuccess();
        }
    }
    
    // Export the public API.
    
    return {
        getIssues: getIssues,
        authenticate: authenticate
    };
    
});