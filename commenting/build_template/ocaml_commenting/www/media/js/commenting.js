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
            return delta + " second" + (delta == 1 ? "": "s") +  " ago";
        }
        if (delta < 60 * 60) {
            delta = Math.floor(delta / 60);
            return delta + " minute" + (delta == 1 ? "": "s") +  " ago";
        }
        if (delta < 60 * 60 * 24) {
            delta = Math.floor(delta / 60 / 60);
            return Math.floor(delta / 60 / 60) + " hour" + (delta == 1 ? "": "s") +  " ago";
        }
        delta = Math.floor(delta / 60 / 60 / 24);
        return delta + " day" + (delta == 1 ? "": "s") +  " ago";
    }
    
    /**
     * Creates an overlay to show the given issues.
     */
    function createOverlay(milestone, elementContent, elementTag, issues, onIssueCreate) {
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
                html: issue.body.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/\n/g, "<br>")
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
        // Add in comment box.
        $("<h2/>", {
            text: "Write a comment"
        }).appendTo(container)
        var area = $("<textarea/>").appendTo(container);
        var submit = $("<input/>", {
            value: "Submit",
            type: "submit",
            click: function() {
                var content = $.trim(area.val());
                if (content) {
                    gitHub.createIssue("New comment on block" + elementTag, content, milestone, onIssueCreate);
                    outer.fadeOut("fast");
                } else {
                    alert("Please enter a comment before submitting!");
                }
            }
        }).appendTo(container);
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
            gitHub.getIssues(function(milestone, data) {
                // Find all commentable elements.
                var commentableElements = $(".page p[id]");
                // Add in the comment action.
                commentableElements.each(function() {
                    var element = $(this);
                    var elementContent = $.trim(element.text()).replace(/\s+/g, " ");
                    var elementLabel = "block-" + element.attr("id");
                    var elementTag = " [" + elementLabel + "]";
                    // Count issues.
                    var issues = [];
                    $.each(data, function(_, issue) {
                        if (issue.title.indexOf(elementTag) != -1) {
                            issues.push(issue);
                        }
                    });
                    // Add in a button to initialize comments.
                    function getButtonText() {
                        return issues.length + " comment" + (issues.length == 1 ? "" : "s");
                    }
                    var button = $("<span/>", {
                        "class": "comment-action",
                        text: getButtonText()
                    }).appendTo(element).click(function() {
                        createOverlay(milestone, elementContent, elementTag, issues, function(newIssue) {
                            issues.push(newIssue);
                            button.text(getButtonText());
                        });
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