$('.post h3').replaceWith(function () {
  return "<h5>" + $(this).html() + "</h5>";
});

$('.post h2').replaceWith(function () {
  return "<h4>" + $(this).html() + "</h4>";
});

$('.post h1').replaceWith(function () {
  return "<h3>" + $(this).html() + "</h3>";
});
