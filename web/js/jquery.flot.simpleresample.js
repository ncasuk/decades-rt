/*
  Simple resampling of bigger datasets so as flot stays usable.

  Samples down to about 'maxpoints' just by stepping through data number_of_points/maxpoints

  D.Tiddeman 6/2/2015
*/

(function ($) {
    function pointsindisplayarea(series){
        var mxy=series.yaxis.options.max;
        var mxx=series.xaxis.options.max;
        var mny=series.yaxis.options.min;
        var mnx=series.xaxis.options.min;
        var d=series.data;
        return d.reduce(function(previousValue, v, i) {
                  if(((v[0]>=mnx)||(mnx==null))&&
                     ((v[1]>=mny)||(mny==null))&&
                     ((v[0]<=mxx)||(mxx==null))&&
                     ((v[1]<=mxy)||(mxy==null))){
                    return previousValue + 1;
                  }else{return previousValue};},0);
    };

    function calcstep(series){
        var step=Math.floor(pointsindisplayarea(series)/(series.simpleresample.maxpoints));
        if(step<2)step=1;
        return step;
    }

    function resample(series){
        var points=series.datapoints.points;
        var d=series.data;
        var ps=series.datapoints.pointsize;
        points.length=0;
        for(var i=0;i<d.length;i++){
           if((i % series.step)==0){
              for(var p=0;p<ps;p++){
                 if(d[i][p]!=null){
                    points.push(d[i][p]);
                 }else{  // remove point
                    points.length=points.length-p;
                    if(series.simpleresample.hidenull){
                       break;
                    }else{  // Fill with null
                       for(var ni=0;ni<ps;ni++){
                          points.push(null)
                       }
                       break;
                    }
                 }
              }
           }
        }
    }

    function drawSeries(plot,canvascontext,series){
      if((series.lastranges[0]!=series.xaxis.options.min)||
         (series.lastranges[0]!=series.xaxis.options.max)||
         (series.lastranges[0]!=series.yaxis.options.min)||
         (series.lastranges[0]!=series.yaxis.options.max)){
          var step=calcstep(series);
          if(step!=series.step){
             series.lastranges=[series.xaxis.options.min,series.xaxis.options.max,
                                series.yaxis.options.min,series.yaxis.options.max];
             series.step=step;
             resample(series);
          }
       }
    };
    function  processDatapoints(plot,series,datapoints){
      resample(series);
    };
    function processRawData ( plot, series ) {
      series.step=calcstep(series);
      series.lastranges=[series.xaxis.options.min,series.xaxis.options.max,
                         series.yaxis.options.min,series.yaxis.options.max];
      
    }


    var options = {
        series: {
            simpleresample: {
                maxpoints: 1000,
                hidenull:true
            }
        }
    };


    function init(plot) {
        plot.hooks.processRawData.push(processRawData);
        plot.hooks.processDatapoints.push(processDatapoints);
        plot.hooks.drawSeries.push(drawSeries);
    }

    $.plot.plugins.push({
        init: init,
        options: options,
        name: "simpleresample",
        version: "1.0"
    });

})(jQuery);
