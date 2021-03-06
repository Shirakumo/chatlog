$(function(){    
    // Coloured nicks
    function hashCode(str) {
        var hash = 0;
        for (var i = 0; i < str.length; i++) {
            hash = str.charCodeAt(i) + ((hash << 5) - hash);
        }
        return Math.abs(hash);
    }

    var colors = ["red", "green", "blue",
                  "darkred", "darkgreen", "darkblue",
                  "lightred", "lightgreen",
                  "cyan", "magenta", "black",
                  "crimson", "#333", "maroon",
                  "midnightblue", "navy", "steelblue",
                  "sienna", "purple", "indigo",
                  "forestgreen", "darkslateblue"];
    $(".nick").each(function(){
        $(this).css("color", colors[hashCode($(this).text()) % colors.length]);
    });

    // Clickable URLs
    jQuery.fn.appendText = function(text){
        $(this).each(function(){
            $(this).append(document.createTextNode(text));
        });
        return this;
    };
    
    $(".text").each(function(){
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

    // Shortcuts
    $(window).keyup(function(e){
        switch(e.which){
        case 65:
            window.location.href = $(".backward").attr("href");
            break;
        case 68:
            window.location.href = $(".forward").attr("href");
            break;
        }
    });

    // Mark anchored
    var hash = window.location.hash.substring(1);
    if(hash !== ""){
        $(".message[id="+hash+"] .text").wrapInner("<mark>");
    }
});
