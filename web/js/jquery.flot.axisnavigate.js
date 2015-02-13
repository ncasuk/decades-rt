/*
  Allows zooming/panning on individual axis using the mouse

  Replaces "contextmenu" with "rightclick"

  Mousewheel or events (zoom:dblclick, zoomout:rightclick) to zoom in and out, or drag axis.

  D.Tiddeman 13/2/2015

Requires jquery.flot.navigate.js - 
     also seems to need jquery.event.drag.js even though it is included in the navigate plugin ?!?

MIT licences for both.


*/

// Make the right click an event by taking over context menu functionality.

$.event.special.rightclick = {
     bindType: "contextmenu",
     delegateType: "contextmenu"
};

(function ($) {

    function axisnavigation(plot,eventHolder){
                if(plot.getOptions().zoom.zoomout){
                  eventHolder.bind(plot.getOptions().zoom.zoomout,function(e){
                     var c = plot.offset();
                     c.left = e.pageX - c.left;
                     c.top = e.pageY - c.top;
                     plot.zoomOut({center:c});
                     return false;
                  });
                }
		$.each(plot.getAxes(), function (i, axis) {
			if (!axis.show)return;
                        var offset=plot.offset();
			var box = axis.box;

			$("<div class='axisTarget' style='position:absolute; left:" + box.left + "px; top:" + box.top + "px; width:" + box.width +  "px; height:" + box.height + "px'></div>")
				.data("axis.direction", axis.direction)
				.data("axis.n", axis.n)
				.css({ backgroundColor: "#0f0", opacity: 0, cursor: "pointer" })
				.appendTo(plot.getPlaceholder())
				.hover(
					function () { $(this).css({ opacity: 0.10 }) },
					function () { $(this).css({ opacity: 0 }) }
				)
                                .bind("dragend",function (event,dd) { 
                                        if(axis.direction=="x"){
                                          var x1=axis.c2p(dd.startX - offset.left);
                                          var x2=axis.c2p(event.pageX - offset.left);
                                          axis.options.max=axis.max+x1-x2;
                                          axis.options.min=axis.min+x1-x2;
                                          plot.setupGrid();
                                          plot.draw();
                                        }
                                        if(axis.direction=="y"){
                                          var y1=axis.c2p(dd.startY - offset.top);
                                          var y2=axis.c2p(event.pageY - offset.top);
                                          axis.options.max=axis.max+y1-y2;
                                          axis.options.min=axis.min+y1-y2;
                                          plot.setupGrid();
                                          plot.draw();
                                        }
                                })
                                .bind("mousewheel",function(event,dd){
                                        scale=(3.0-dd)/3.0
                                        if(axis.direction=="x"){
                                          var x=axis.c2p(event.pageX - offset.left);
                                          axis.options.max=x+(axis.max-x)*scale
                                          axis.options.min=x-(x-axis.min)*scale
                                          //plot.setupGrid();
                                          plot.draw();
                                        }
                                        if(axis.direction=="y"){
                                          var y=axis.c2p(event.pageY - offset.top);
                                          axis.options.max=y+(axis.max-y)*scale
                                          axis.options.min=y-(y-axis.min)*scale
                                          //plot.setupGrid();
                                          plot.draw();
                                        }
                                        return false;
                                })
                                .bind(plot.getOptions().axisnavigate.zoomout,function(event){
                                        mw=$.Event("mousewheel");
                                        mw.pageX=event.pageX;
                                        mw.pageY=event.pageY;
                                        $(this).triggerHandler(mw,-1);
                                        return false;
                                })
				.bind(plot.getOptions().axisnavigate.zoom,function (event) {
                                        mw=$.Event("mousewheel");
                                        mw.pageX=event.pageX;
                                        mw.pageY=event.pageY;
                                        $(this).triggerHandler(mw,+1);
                                        return false;
				})
                                ;
                });
    }

    var options = {
        axisnavigate: {
            zoom:"dblclick",
            zoomout:"rightclick"
            },
        zoom:{zoomout:"rightclick"}
    };


    function init(plot) {
        plot.hooks.bindEvents.push(axisnavigation);
    }

    $.plot.plugins.push({
        init: init,
        options: options,
        name: "axisnavigate",
        version: "1.0"
    });

})(jQuery);
