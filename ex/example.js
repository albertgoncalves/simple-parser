(function() {
    var counter = function(x) {
        return function() {
            x = x + 1;
            return x;
        };
    };
    var x = 1;
    var c = counter(x);
    c();
    c();
    console.log(c());
    console.log(x);
})();
