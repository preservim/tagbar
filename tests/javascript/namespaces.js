/* Object Literal Notation */
var objectLiteral = {
    str: '1',
    func: function() { return 1; }
};

/* Module Pattern 1 */
var module = (function(){
    var private = 1;
    return {
        method: function() { private++; }
    };
})();

/* Module Pattern 2 */
var module2 = {};
(function(context){
    var private = 1;
    context.method = function() { private++; }
})(module2);

/* Module Pattern 3 */
var module3 = {};
(function(){
    var private = 1;
    this.method = function() { private++; }
}).apply(module3);
