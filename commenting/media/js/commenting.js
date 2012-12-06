/**
 * The Real World OCaml commenting system.
 */
define([
    "gitHub"
], function(
    gitHub
) {
    
    /**
     * Initializes the commenting system.
     */
    function init() {
        // Load the list of GitHub issues.
        gitHub.getIssues(function(data) {
            // Find all commentable elements.
            var commentableElements = $(".page p[id]");
            // Add in the comment action.
            commentableElements.each(function() {
                var element = $(this);
                var elementLabel = "block-" + element.attr("id");
                // Count issues.
                var issues = [];
                $.each(data.data, function(_, issue) {
                    $.each(issue.labels, function(_, label) {
                        if (label.name == elementLabel) {
                            issues.push(issue)
                        }
                    });
                });
                // Add in a button to initialize comments.
                var button = $("<span/>", {
                    "class": "comment-action",
                    text: issues.length + " comment" + (issues.length == 1 ? "" : "s")
                }).appendTo(element);
            });
        });
    }
    
    // Export the public API.
    
    return {
        init: init
    }
    
});