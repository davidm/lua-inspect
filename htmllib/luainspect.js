// LuaInspect (c) 2010 David Manura, MIT License.

function get_line_of_domobject(obj) {
  var line = $(obj).text().match(/used-line:(\d+)/);
  if (line) { line = line[1]; }
  return line;
}

function get_linerange_of_objects(jobject) {
  var maxlinenum; var minlinenum;
  jobject.next().each(function() {
    var linenum = get_line_of_domobject(this);
    if (linenum) {
      minlinenum = (minlinenum==null) ? linenum : Math.min(minlinenum, linenum);
      maxlinenum = (maxlinenum==null) ? linenum : Math.max(maxlinenum, linenum);
    }
  });
  return [minlinenum, maxlinenum];
}

function highlight_id(aclass, enable) {
  var methname = enable ? "addClass" : "removeClass";
  $("." + aclass)[methname]("highlight");
  var linenums = get_linerange_of_objects($("." + aclass));
  if (linenums) { for (var i=linenums[0]; i <= linenums[1]; i++) {
    $('#L'+i)[methname]("highlight");
  }}
}

function highlightSameClass(obj, enable) {
  var classes = obj.attr('class').split(' ');
  for (var i in classes) {
    var aclass = classes[i];
    if (aclass.match(/^id\w*\d+/)) {
      highlight_id(aclass, enable);
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
