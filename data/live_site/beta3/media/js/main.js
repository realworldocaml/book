require.config({
    shim: {
        "jquery.cookie": ["jquery"]
    }
});

require([
    "jquery",
    "commenting"
], function(
    $,
    commenting
) {
    
    $(function() {
        
        commenting.init();
        
    });
    
});