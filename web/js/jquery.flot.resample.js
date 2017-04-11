/*
  Simple resampling of bigger datasets so as flot stays usable.

  Samples down to about 'maxpoints' just by stepping through data number_of_points/maxpoints

  Doesn't resample 'recent' number of points

  D.Tiddeman 6/2/2015
*/
(function ($) {

    function processRawData ( plot, series ) {
            var l=series.data.length;
            var mxy=series.yaxis.options.max;
            var mxx=series.xaxis.options.max;
            var mny=series.yaxis.options.min;
            var mnx=series.xaxis.options.min;
            var first=0;
            var last=l-series.resample.recent-2;
            if(last>0){
            var recent;
            recent=series.data.slice(-series.resample.recent)
            if(mnx&&mny&&mxx&&mxy){
              //if((mnx!=-1)||(mny!=-1)||(mxx!=1)||(mxy!=1)){
                l=series.data.slice(0,-series.resample.recent).reduce(function(previousValue, v, i) {
                  if((v[0]>=mnx)&&(v[1]>=mny)&&(v[0]<=mxx)&&(v[1]<=mxy)){
                    if(first==0){first=i;}
                    last=i;
                    return previousValue + 1;
                  }else{return previousValue};},0);
                if(first>0){first--;}
                if(last<l){last++;}
              //}
            }
            var step=Math.floor(l/series.resample.maxpoints);
            if((last>first)&&(step>1)){
                series.data=(series.data.slice(first,last)).filter(function(v,i){return ((i % step)==0)}).concat(recent);
            }
            }
    }


    var options = {
        series: {
            resample: {
                maxpoints: 1000,
                recent:100
            }
        }
    };

    function init(plot) {
        plot.hooks.processRawData.push(processRawData);
    }

    $.plot.plugins.push({
        init: init,
        options: options,
        name: "resample",
        version: "1.0"
    });

})(jQuery);
