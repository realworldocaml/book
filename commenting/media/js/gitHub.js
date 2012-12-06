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
    
    // Export the public API.
    
    return {
        getIssues: getIssues
    };
    
});