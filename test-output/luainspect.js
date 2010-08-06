// LuaInspect (c) 2010 David Manura, MIT License.

function highlightSameClass(obj, enable) {
  var classes = obj.attr('class').split(' ');
  for (var i in classes) {
    var aclass = classes[i];
    if (aclass.match(/^id\w*\d+/)) {
      if (enable) {
        $("." + aclass).addClass("highlight");
      }
      else {
        $("." + aclass).removeClass("highlight");
      }
    }
  }
}

$(document).ready(function() {
  $(".id").hover(
    function() {
      var tip = $(this).next('span');
      tip.stop(true, true).animate({opacity: "show"}, "slow");

      highlightSameClass($(this), true);
    },
    function() {
      var tip = $(this).next('span');
      tip.animate({opacity: "hide"}, "fast");
      highlightSameClass($(this), false);
    }
  );
  $(".keyword").hover(
    function() {
      highlightSameClass($(this), true);
    },
    function() {
      highlightSameClass($(this), false);
    }
  );
});

//.mousemove(function(kmouse) {
//        $tip.css({left:kmouse.pageX+15, top:kmouse.pageY+100});
//      })
