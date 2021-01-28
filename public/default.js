var msg = "";

msg = "Changing inline-css for language-haskell"
console.log("BEGIN: ".concat(msg));
var all = document.getElementsByClassName('language-haskell');
for (var i = 0; i < all.length; i++) {
  all[i].style.color = '#fff';
}
console.log("END: ".concat(msg));
