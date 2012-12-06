/**
 * The Real World OCaml commenting system.
 */
define([
    "gitHub"
], function(
    gitHub
) {
    
    var body = $("body");
    
    /**
     * Creates an overlay to show the given issues.
     */
    function createOverlay(issues) {
        var outer = $("<div/>", {
            "class": "comment-overlay-outer"
        }).appendTo(body);
        var container = $("<div/>", {
            "class": "comment-overlay"
        }).appendTo(outer);
        $("<h1/>", {
            text: issues.length + " comment" + (issues.length == 1 ? "" : "s")
        }).appendTo(container);
        // Add a close button.
        $("<span/>", {
            "class": "comment-overlay-close",
            text: "close"
        }).appendTo(container).click(function() {
            outer.fadeOut(function() {
                outer.remove();
            });
        })
        // Show the overlay.
        outer.fadeIn();
    }
    
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
                }).appendTo(element).click(function() {
                    createOverlay(issues);
                });
            });
        });
    }
    
    // Export the public API.
    
    return {
        init: init
    }
    
});