$(function(){
    // Mark anchored
    var hash = window.location.hash.substring(1);
    if(hash !== ""){
        $("tr:has(a[name="+hash+"]) td")
            .wrapInner("<mark></mark>");
    }
    
    // Coloured nicks
    function hashCode(str) {
        var hash = 0;
        for (var i = 0; i < str.length; i++) {
            hash = str.charCodeAt(i) + ((hash << 5) - hash);
        }
        return hash;
    }

    var colors = ["red", "green", "blue",
                  "darkred", "darkgreen", "darkblue",
                  "lightred", "lightgreen",
                  "cyan", "magenta", "black",
                  "crimson", "#333", "maroon",
                  "midnightblue", "navy", "steelblue",
                  "sienna", "purple", "indigo",
                  "forestgreen", "darkslateblue"];
    $("tr td:nth-child(2)").each(function(){
        $(this).css("color", colors[hashCode($(this).text()) % colors.length]);
    });

    // Clickable URLs
    jQuery.fn.appendText = function(text){
        $(this).each(function(){
            $(this).append(document.createTextNode(text));
        });
        return this;
    };
    
    $("tr td:nth-child(3)").each(function(){
        var text = $(this).text();
        var urlregex = /([a-zA-Z]+):\/\/([A-Za-z0-9\-\.\_\~\:\/\?\#\[\]\@\!\$\&\'\(\)\*\+\,\;\=\%]*)/g;
        var previndex = 0;
        $(this).empty();
        while((match = urlregex.exec(text)) != null){
            var before = text.substring(previndex, match.index);
            var url = $("<a>")
                .text(match[0])
                .attr("href", match[0]);
            $(this).appendText(before)
                .append(url);
            previndex = match.index+match[0].length;
        }
        $(this).appendText(text.substring(previndex));
    });
});
