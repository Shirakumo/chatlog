$(function(){
    var hash = window.location.hash.substring(1);
    if(hash !== ""){
        $("tr:has(a[name="+hash+"]) td")
            .wrapInner("<mark></mark");
    }
});
