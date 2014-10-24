/**
 * @fileOverview
 * Embedded SWISH
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

(function($) {
  var pluginName = 'LPN';
  var currentSWISHElem = null;

  var SWISH = "http://swish.swi-prolog.org/";
//var SWISH = "http://localhost:3050/";

  /** @lends $.fn.LPN */
  var methods = {
    _init: function(options) {
      var currentSource = null;

      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */

	if ( elem.hasClass("source") ) {
	  data.queries = [];
	  currentSource = data;
	  elem.wrap("<div class='source'></div>");
	  elem.parent()
	    .append("<div class='load'></div>")
	    .on("click", "div.load", function() {
	      toggleSWISH(elem);
	    });
	} else if ( elem.hasClass("query") ) {
	  if ( currentSource )
	    currentSource.queries.push("?- ", elem.text(), "\n");
	}

	elem.data(pluginName, data);	/* store with element */
      });
    }
  }; // methods

  // <private functions>

  function toggleSWISH(elem) {
    function attr(name, value) {
      content.push(" ", name, "='", value, "'");
    }

    var data    = elem.data(pluginName);

    if ( data.swish ) {
      var swish = data.swish;

      delete data.swish;
      currentSWISHElem = null;
      swish.hide(400, function() { swish.remove(); });
      elem.show(400, function() { elem.parent().removeClass("swish"); });
      elem.parent()
	.resizable('destroy')
        .css("width", "auto")
        .css("height", "auto");
    } else
    { var source  = elem.text();
      var query   = SWISH+"?code="+encodeURIComponent(source);
      var content = [ "<iframe " ];

      if ( currentSWISHElem )
	toggleSWISH(currentSWISHElem);

      if ( data.queries.length > 0 ) {
	query += "&examples=" + encodeURIComponent(data.queries.join(""));
      }

      attr("class", "swish");
      attr("src", query);
      attr("width", "100%");
      attr("height", "100%");

      content.push("></iframe>");

      data.swish = $(content.join(""))
	.hide()
	.insertAfter(elem);
      elem.parent()
	.css("height", "300px")
	.resizable();
      elem.hide(400);
      data.swish.show(400, function() { elem.parent().addClass("swish"); });

      currentSWISHElem = elem;
    }
  }

  /**
   * <Class description>
   *
   * @class LPN
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.LPN = function(method) {
    if ( methods[method] ) {
      return methods[method]
	.apply(this, Array.prototype.slice.call(arguments, 1));
    } else if ( typeof method === 'object' || !method ) {
      return methods._init.apply(this, arguments);
    } else {
      $.error('Method ' + method + ' does not exist on jQuery.' + pluginName);
    }
  };
}(jQuery));


		 /*******************************
		 *	    CHEAP MODAL		*
		 *******************************/

var modal = (function() {
  var method = {},
  $overlay,
  $modal,
  $content,
  $close;

				// Center the modal in the viewport
  method.center = function () {
    var top, left;

    top = Math.max(window.innerHeight - $modal.outerHeight(), 0) / 2;
    left = Math.max(window.innerWidth - $modal.outerWidth(), 0) / 2;

    $modal.css({ top:top + $(window).scrollTop(),
                 left:left + $(window).scrollLeft()
               });
  };

				// Open the modal
  method.open = function (settings) {
    $content.empty().append(settings.content);

    $modal.css({ width: settings.width || 'auto',
                 height: settings.height || 'auto'
	       });

    method.center();
    $(window).bind('resize.modal', method.center);
    $modal.show();
    $overlay.show();
  };

				// Close the modal
  method.close = function () {
    $modal.hide();
    $overlay.hide();
    $content.empty();
    $(window).unbind('resize.modal');
  };

				// Generate the HTML and add it to the document
  $overlay = $('<div id="overlay"></div>');
  $modal = $('<div id="modal"></div>');
  $content = $('<div id="content"></div>');
  $close = $('<a id="close" href="#">close</a>');

  $modal.hide();
  $overlay.hide();
  $modal.append($content, $close);

  $(document).ready(function() {
    $('body').append($overlay, $modal);
  });

  $close.click(function(e) {
    e.preventDefault();
    method.close();
  });

  return method;

}());
