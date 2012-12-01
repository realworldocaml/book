require([
    "domReady",
    "commenting"
], function(
    domReady,
    commenting
) {
    
    domReady(function() {
        
        commenting.init();
        
    });
    
});