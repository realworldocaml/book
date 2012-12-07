/**
 * GitHub API helpers.
 */
define([
    "module",
    "jquery",
    "jquery.cookie"
], function(
    module,
    $
) {
    
    // Configure the module.
    var config = module.config();
    var gitHubUser = config.user;
    var gitHubRepo = config.repo;
    var gitHubMilestone = config.milestone;
    var gitHubPageLabel = "page-" + config.page.split(".")[0];
    var gitHubClientId = $.cookie("github_client_id");
    var gitHubAccessToken = $.cookie("github_access_token");
    
    /**
     * Tests if the user is currently authenticated.
     */
    function isAuthenticated() {
        return !!gitHubAccessToken;
    }
    
    function getOAuth2RedirectURL() {
        return "https://github.com/login/oauth/authorize?client_id=" + encodeURIComponent(gitHubClientId) + "&redirect_uri=" + encodeURIComponent(String(window.location));
    }
    
    /**
     * Looks up a list of milestones for this repository.
     */
    function getMilestones(onSuccess) {
        $.getJSON("https://api.github.com/repos/" + encodeURIComponent(gitHubUser) + "/" + encodeURIComponent(gitHubRepo) + "/milestones?callback=?", {
            access_token: gitHubAccessToken
        }, onSuccess);
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
            // Get all open and closed issues.
            var receiveCount = 0;
            var fullData = [];
            function receiveIssues(data) {
                fullData = fullData.concat(data.data);
                console.log(fullData)
                receiveCount += 1;
                if (receiveCount == 2) {
                    fullData.sort(function(a, b) {
                        if (a.created_at > b.created_at) {
                            return 1;
                        }
                        return -1;
                    });
                    onSuccess(fullData);
                }
            }
            $.each(["open", "closed"], function(_, state) {
                $.getJSON("https://api.github.com/repos/" + encodeURIComponent(gitHubUser) + "/" + encodeURIComponent(gitHubRepo) + "/issues?callback=?", {
                    milestone: milestone.number,
                    labels: gitHubPageLabel,
                    access_token: gitHubAccessToken,
                    state: state
                }, receiveIssues);
            });
        });
    }
    
    // Export the public API.
    
    return {
        isAuthenticated: isAuthenticated,
        getOAuth2RedirectURL: getOAuth2RedirectURL,
        getIssues: getIssues
    };
    
});