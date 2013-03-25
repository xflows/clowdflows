$(document).ready(setTimeout(function(){
      $('#canvas2').find('div[class*=output]').tipsy({
        gravity: $.fn.tipsy.autoWE,
        html: true,
        fade: true
      });
      $('#canvas2').find('div[class*=input]').tipsy({
        gravity: $.fn.tipsy.autoWE,
        html: true,
        fade: true
      });
    }, 5000)
);