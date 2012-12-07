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
     * Formates the date into a human-readable string.
     */
    function formatDate(date) {
        var delta = Math.floor((Date.now() - date.getTime()) / 1000);
        if (delta < 60) {
            return delta + " seconds ago";
        }
        if (delta < 60 * 60) {
            return Math.floor(delta / 60) + " minutes ago";
        }
        if (delta < 60 * 60 * 24) {
            return Math.floor(delta / 60 / 60) + " hours ago";
        }
        return Math.floor(delta / 60 / 60 / 24) + " days ago";
    }
    
    /**
     * Creates an overlay to show the given issues.
     */
    function createOverlay(issues) {
        var outer = $("<div/>", {
            "class": "comment-overlay-outer"
        }).appendTo(body).hide();
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
            outer.fadeOut("fast", function() {
                outer.remove();
            });
        })
        // Add in all issues.
        $.each(issues, function(_, issue) {
            var article = $("<article/>").appendTo(container);
            var userURL = "https://github.com/" + issue.user.login;
            $("<h1/>").append(
                $("<a/>", {
                    href: userURL,
                    text: issue.user.login
                }),
                " commented " + formatDate(new Date(issue.created_at))
            ).appendTo(article);
            var commentText = $("<p/>", {
                text: issue.title
            }).appendTo(article);
            if (issue.closed_at != null) {
                commentText.addClass("closed");
            }
            $("<img/>", {
                src: issue.user.avatar_url
            }).appendTo($("<a/>", {
                href: userURL,
            }).appendTo(article));
        });
        // Show the overlay.
        outer.fadeIn("fast");
    }
    
    /**
     * Initializes the commenting system.
     */
    function init() {
        // Check if we are authenticated.
        if (gitHub.isAuthenticated()) {
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
                    $.each(data, function(_, issue) {
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
        } else {
            // Start the authenticate process.
            var header = $(".header");
            header.append($("<p/>").append($("<a/>", {
                href: gitHub.getOAuth2RedirectURL(),
                text: "Click here to login with GitHub and view the comments!"
            })).hide().fadeIn("fast"));
        }
    }
    
    // Export the public API.
    
    return {
        init: init
    }
    
});