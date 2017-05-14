// jQuery script to show/hide elements depending on a click event


$(document).ready(function() {

  // Default language FR
  $('[lang="en"]').hide();
  $('[lang="es"]').hide();

  // SELECTION OF FR
  $("#lang-fr").click(function() {
    $('[lang="fr"]').show(); $('[lang="en"]').hide(); $('[lang="es"]').hide();
  });

  // SELECTION OF EN
  $("#lang-en").click(function() {
    $('[lang="fr"]').hide(); $('[lang="en"]').show(); $('[lang="es"]').hide();
  });

  // SELECTION OF ES
  $("#lang-es").click(function() {
    $('[lang="fr"]').hide(); $('[lang="en"]').hide(); $('[lang="es"]').show();
  });
});
