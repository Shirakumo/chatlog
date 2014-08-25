$(function(){
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
    var hash = window.location.hash.substring(1);
    if(hash !== ""){
        $("tr:has(a[name="+hash+"]) td")
            .wrapInner("<mark></mark");
    }

    $("tr td:nth-child(2)").each(function(){
        $(this).css("color", colors[hashCode($(this).text()) % colors.length]);
    });
});
